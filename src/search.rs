// Copyright 2015 Jerome Rasky <jerome@rasky.co>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
// or http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.
use std::collections::{HashMap, BinaryHeap};
use std::cmp::Ordering;
use std::iter::FromIterator;
use std::sync::Arc;
use std::cell::RefCell;
use std::borrow::BorrowMut;

use unicode_normalization::UnicodeNormalization;

use std::mem;

use constants::*;

// thread-local scratch area
thread_local!(static SCRATCH: RefCell<Option<SearchScratch>> = RefCell::new(None));

/// Contains the searchable database
#[derive(Debug)]
pub struct SearchBase {
    lines: Vec<LineInfo>,
}

/// Parsed information about a line, ready to be searched by a SearchBase.
#[derive(Debug)]
pub struct LineInfo {
    line: Arc<String>,
    char_map: HashMap<char, Vec<usize>>,
    heat_map: Vec<f32>,
    factor: f32,
}

#[derive(PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Numeric,
    Alphabetic,
    First,
    Other,
}

#[derive(Debug)]
struct LineMatch {
    score: f32,
    factor: f32,
    line: Arc<String>,
}

#[derive(Debug)]
struct SearchScratch {
    position: Vec<usize>,
    state: Vec<usize>,
    lists: Vec<&'static Vec<usize>>,
}

impl Ord for LineMatch {
    fn cmp(&self, other: &LineMatch) -> Ordering {
        match self.score.partial_cmp(&other.score) {
            Some(Ordering::Equal) | None => {
                self.factor
                    .partial_cmp(&other.factor)
                    .unwrap_or(Ordering::Equal)
            }
            Some(order) => order,
        }
    }
}

impl PartialOrd for LineMatch {
    fn partial_cmp(&self, other: &LineMatch) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for LineMatch {
    fn eq(&self, other: &LineMatch) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for LineMatch {}

/// Creates a LineInfo object with a factor of zero
impl<T: Into<String>> From<T> for LineInfo {
    fn from(item: T) -> LineInfo {
        LineInfo::new(item, 0.0)
    }
}

impl<V: Into<LineInfo>> FromIterator<V> for SearchBase {
    fn from_iter<T: IntoIterator<Item = V>>(iterator: T) -> SearchBase {
        SearchBase::new(iterator.into_iter().map(|item| item.into()).collect())
    }
}

impl SearchScratch {
    fn new(size: usize) -> SearchScratch {
        SearchScratch {
            position: vec![0; size],
            state: vec![0; size],
            lists: Vec::with_capacity(size),
        }
    }
}

impl SearchBase {
    /// Construct a new SearchBase from a Vec of LineInfos.
    pub fn new(lines: Vec<LineInfo>) -> SearchBase {
        SearchBase { lines: lines }
    }

    /// Perform a query of the SearchBase.
    ///
    /// number limits the number of matches returned.
    ///
    /// Matches any supersequence of the given query, with heuristics to order
    /// matches based on how close they are to the given query.
    pub fn query<T: AsRef<str>>(&self, query: T, number: usize) -> Vec<Arc<String>> {
        let query = query.as_ref();
        if query.is_empty() {
            // non-matching query
            return vec![];
        }

        let mut matches: BinaryHeap<LineMatch> = BinaryHeap::with_capacity(number);

        let composed: Vec<char> = query.nfkc().collect();

        SCRATCH.with(|scratch| {
            *scratch.borrow_mut() = Some(SearchScratch::new(composed.len()));
        });

        for item in self.lines.iter() {
            let score = match item.score(composed.iter()) {
                None => {
                    // non-matching line
                    continue;
                }
                Some(score) => score,
            };

            let match_item = LineMatch {
                score: -score,
                factor: -item.factor,
                line: item.line.clone(),
            };

            if matches.len() < number {
                matches.push(match_item);
            } else if &match_item < matches.peek().unwrap() {
                matches.push_pop(match_item);
            }
        }

        matches.into_sorted_vec().into_iter().map(|x| x.line).collect()
    }
}

impl LineInfo {
    /// Constructs a new LineInfo objects from the given item.
    ///
    /// Factor is a "tie-breaker," or something to weight the matches in a way
    /// beyond the matching already done in flx. The greater the factor, the
    /// more greatly matching favors the item.
    pub fn new<T: Into<String>>(item: T, factor: f32) -> LineInfo {
        let mut map: HashMap<char, Vec<usize>> = HashMap::new();
        let mut heat = vec![];
        let line = Arc::new(item.into());

        let mut ws_score: f32 = 0.0;
        let mut cs_score: f32 = 0.0;
        let mut cur_class = CharClass::First;
        let mut cs_change = false;

        for (idx, c) in line.as_ref().nfkc().enumerate() {
            if idx > MAX_LEN {
                break;
            }

            if !c.is_whitespace() {
                if cur_class == CharClass::First {
                    cs_score += FIRST_FACTOR;
                }
            }

            if c.is_whitespace() {
                cur_class = CharClass::Whitespace;
                ws_score = WHITESPACE_FACTOR;
            } else if c.is_numeric() {
                if cur_class != CharClass::Numeric {
                    cur_class = CharClass::Numeric;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            } else if c.is_alphabetic() {
                if cur_class != CharClass::Alphabetic {
                    cur_class = CharClass::Alphabetic;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            } else {
                if cur_class != CharClass::Other {
                    cur_class = CharClass::Other;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            }

            if cur_class != CharClass::Whitespace {
                map.entry(c).or_insert(Vec::default()).push(idx);
                if c.is_uppercase() {
                    for lc in c.to_lowercase() {
                        map.entry(lc).or_insert(Vec::default()).push(idx);
                    }
                }
            }

            heat.push(ws_score + cs_score);

            ws_score *= WHITESPACE_REDUCE;
            if !cs_change {
                cs_score *= CLASS_REDUCE;
            }
        }

        LineInfo {
            line: line,
            char_map: map,
            heat_map: heat,
            factor: factor,
        }
    }

    fn score_position(&self, position: &Vec<usize>) -> f32 {
        let avg_dist: f32;

        if position.len() < 2 {
            avg_dist = 0.0;
        } else {
            avg_dist = position.windows(2)
                               .map(|pair| {
                                   pair[1] as f32 - pair[0] as f32
                               })
                               .sum::<f32>() / position.len() as f32;
        }

        let heat_sum: f32 = position.iter()
                                    .map(|idx| self.heat_map[*idx])
                                    .sum();

        avg_dist * DIST_WEIGHT + heat_sum * HEAT_WEIGHT + self.factor * FACTOR_REDUCE
    }

    fn score<'a, T: Iterator<Item = &'a char>>(&self, query: T) -> Option<f32> {
        SCRATCH.with(|scratch| self.score_inner(query, scratch.borrow_mut().as_mut().unwrap()))
    }

    fn score_inner<'a, T: Iterator<Item = &'a char>>(&self,
                                                     query: T,
                                                     scratch: &mut SearchScratch)
                                                     -> Option<f32> {
        let &mut SearchScratch {ref mut position, ref mut state, ref mut lists} = scratch;

        let mut idx: usize = 0;
        let mut after: usize = 0;
        let mut score: Option<f32> = None;

        lists.clear();
        for ref ch in query {
            // don't match whitespace
            if !ch.is_whitespace() {
                match self.char_map.get(ch) {
                    Some(list) => {
                        // transmute lifetime, because of thread-local storage
                        // we clear the list before use, so we're good anyways
                        lists.push(unsafe { mem::transmute(list) });
                    }
                    None => {
                        return None;
                    }
                }
            }
        }

        // initialize the first element of the position
        position[0] = 0;

        // create the first position
        loop {
            if lists[idx][position[idx]] > after || idx == 0 {
                after = lists[idx][position[idx]];
                idx += 1;
                // clear next position entry
                if idx >= position.len() {
                    break;
                } else {
                    position[idx] = 0;
                }
            } else {
                position[idx] += 1;
                if position[idx] >= lists[idx].len() {
                    return None;
                }
            }
        }

        // try to find a score
        'outer: loop {
            if idx >= position.len() {
                // read position
                for idx in 0..position.len() {
                    state[idx] = lists[idx][position[idx]];
                }

                // score postion
                let new_score = self.score_position(&state);
                score = score.map(|score| {
                                 if new_score > score {
                                     new_score
                                 } else {
                                     score
                                 }
                             })
                             .or(Some(new_score));
                // after should still be correct at this point
                idx -= 1;
            } else {
                // try to increment this position
                loop {
                    position[idx] += 1;
                    if position[idx] >= lists[idx].len() {
                        if idx == 0 {
                            // no more steps possible
                            break 'outer;
                        } else {
                            // bump back
                            position[idx] = 0;
                            idx -= 1;
                            after = lists[idx][position[idx]];
                        }
                    } else if lists[idx][position[idx]] > after {
                        after = lists[idx][position[idx]];
                        idx += 1;
                        break;
                    }
                }
            }
        }

        score
    }
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;
    use std::sync::Arc;

    use rand::Rng;

    use rand;
    use test;

    use super::*;

    #[test]
    fn test_matches() {
        // create a simple search set
        let test_strings = vec!["test1", "test2", "test3"];
        let base = SearchBase::from_iter(test_strings);

        // search for something deinitely not in it
        let result = base.query("abc", 1);

        assert!(result.is_empty());
    }

    #[test]
    fn test_one_long() {
        let test_strings = vec!["a", "b", "ab"];
        let base = SearchBase::from_iter(test_strings);

        let result = base.query("a", 1);

        assert!(result.contains(&Arc::new("a".into())));
    }

    #[test]
    fn test_simple_matches() {
        // create a simple search set
        let test_strings = vec!["test", "hello", "hello2"];
        let base = SearchBase::from_iter(test_strings);

        // search
        let result = base.query("hello", 3);

        assert!(result.contains(&Arc::new("hello".into())));
        assert!(result.contains(&Arc::new("hello2".into())));
        assert!(!result.contains(&Arc::new("test".into())));
    }

    #[test]
    fn test_truncate() {
        let test_strings = vec!["test", "toast"];
        let base = SearchBase::from_iter(test_strings);

        // tt matches test more closely than toast
        let result = base.query("tt", 1);

        assert_eq!(result.len(), 1);
        assert!(result.contains(&Arc::new("test".into())));
    }

    #[bench]
    fn bench_search(b: &mut test::Bencher) {
        let mut rng = rand::thread_rng();

        let test_strings = vec!["touaoeuaoeeaoeuaoeuaoeusaoeuaoeuaoeuoeautaoeuaoeuaoeu",
                                "aoeuaoeuhaoeuaoeuaoeueaoeuaoeuaoeulaoeuaoeuaoeuloaeuoaeuoeauooea\
                                 ua",
                                "aoeuaoeuahoeuaouaoeuoaeeuaoeuoaeuaoeulaoeuoaeuaoeulaoeuaoeuaoeuo\
                                 aoeuoaeuaoeu2aoeuoae"];
        let mut test_set = Vec::with_capacity(1000);

        for _ in 0..1000 {
            let num = rng.gen::<usize>() % test_strings.len();
            test_set.push(test_strings[num].clone());
        }

        let base = SearchBase::from_iter(test_set);

        b.iter(|| base.query("hello", 10));
    }
}
