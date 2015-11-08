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

use constants::*;

/// Contains the searchable database
#[derive(Debug)]
pub struct SearchBase {
    lines: Vec<LineInfo>
}

/// Parsed information about a line, ready to be searched by a SearchBase.
#[derive(Debug)]
pub struct LineInfo {
    line: Arc<String>,
    char_map: HashMap<char, Vec<usize>>,
    heat_map: Vec<f32>,
    factor: f32
}

#[derive(PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Numeric,
    Alphabetic,
    First,
    Other
}

#[derive(Debug)]
struct LineMatch {
    score: f32,
    factor: f32,
    line: Arc<String>
}

impl Ord for LineMatch {
    fn cmp(&self, other: &LineMatch) -> Ordering {
        match self.score.partial_cmp(&other.score) {
            Some(Ordering::Equal) | None =>
                self.factor.partial_cmp(&other.factor)
                .unwrap_or(Ordering::Equal),
            Some(order) => order
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

impl<T: Into<String>> From<T> for LineInfo {
    fn from(item: T) -> LineInfo {
        LineInfo::new(item, 0.0)
    }
}

impl<V: Into<LineInfo>> FromIterator<V> for SearchBase {
    fn from_iter<T: IntoIterator<Item=V>>(iterator: T) -> SearchBase {
        SearchBase::new(iterator.into_iter().map(|item| item.into()).collect())
    }
}

impl SearchBase {
    /// Construct a new SearchBase from a Vec of LineInfos.
    pub fn new(lines: Vec<LineInfo>) -> SearchBase {
        SearchBase{
            lines: lines
        }
    }

    /// Perform a query of the SearchBase.
    ///
    /// number limits the number of matches returned.
    ///
    /// Matches any supersequence of the given query, with heuristics to order
    /// matches based on how close they are to the given query.
    pub fn query<T: AsRef<str>>(&self, query: T, number: usize)
                                -> Vec<Arc<String>> {
        if query.as_ref().is_empty() {
            // an empty query means don't match anything
            return vec![];
        }

        let mut matches: BinaryHeap<LineMatch> =
            BinaryHeap::with_capacity(number);

        for item in self.lines.iter() {
            let score = match item.score(&query) {
                None => {
                    // non-matching line
                    continue;
                },
                Some(score) => {
                    score
                }
            };

            let match_item = LineMatch {
                score: -score,
                factor: -item.factor,
                line: item.line.clone()
            };

            if matches.len() < number {
                matches.push(match_item);
            } else if &match_item < matches.peek().unwrap() {
                matches.push_pop(match_item);
            }
        }

        matches.into_sorted_vec().into_iter().map(|x| {x.line}).collect()
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

        for (idx, c) in line.chars().enumerate() {
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
                    cs_change =  false;
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
            factor: factor
        }
    }

    fn get_positions(&self, item: char, after: Option<usize>)
                     -> Option<Vec<usize>> {
        match after {
            None => self.char_map.get(&item).map(|list| {list.to_vec()}),
            Some(after) => {
                self.char_map.get(&item).and_then(|list| {
                    match list.binary_search(&after) {
                        Ok(idx) if idx + 1 < list.len() =>
                            Some(list[idx + 1..].to_vec()),
                        Err(idx) if idx < list.len() =>
                            Some(list[idx..].to_vec()),
                        _ => None
                    }
                })
            }
        }
    }

    fn position_list<T: AsRef<str>>(&self, item: T) -> Option<Vec<Vec<usize>>> {
        let mut positions = vec![];
        let mut last = None;

        for c in item.as_ref().chars() {
            if c.is_whitespace() {
                // don't match whitespace
                continue;
            }

            match self.get_positions(c, last) {
                None => return None,
                Some(list) => {
                    last = match list.get(0) {
                        None => return None,
                        Some(idx) => Some(*idx)
                    };
                    positions.push(list);
                }
            }
        }

        Some(positions)
    }

    fn permute_positions(mut list: Vec<Vec<usize>>) -> Option<Vec<Vec<usize>>> {
        let mut result = vec![];

        trace!("Position list: {:?}", list);

        match list.pop() {
            None => return None,
            Some(item) => {
                for idx in item {
                    result.push(vec![idx]);
                }
            }
        }

        for base_list in list.into_iter().rev() {
            for list in result.iter_mut() {
                let compare = *list.last().unwrap();
                for item in base_list.iter().rev() {
                    if *item < compare {
                        list.push(*item);
                    }
                }
            }
        }

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    fn score_position(&self, position: Vec<usize>) -> f32 {
        let avg_dist: f32;

        trace!("Scoring position: {:?}", position);

        if position.len() < 2 {
            avg_dist = 0.0;
        } else {
            avg_dist = position.windows(2).map(|pair| {
                trace!("Window: {:?}", pair);
                pair[0] as f32 - pair[1] as f32
            }).sum::<f32>() / position.len() as f32;
        }

        let heat_sum: f32 = position.iter()
            .map(|idx| {self.heat_map[*idx]}).sum();

        avg_dist  * DIST_WEIGHT + heat_sum
            * HEAT_WEIGHT + self.factor * FACTOR_REDUCE
    }

    fn best_position(&self, positions: Vec<Vec<usize>>) -> Option<f32> {
        let mut scores = positions.into_iter()
            .map(|position| {self.score_position(position)});

        scores.next()
            .map(|score| scores.fold(score, |acc, item| item.max(acc)))
    }

    fn score<T: AsRef<str>>(&self, query: T) -> Option<f32> {
        self.position_list(query)
            .and_then(|positions| {LineInfo::permute_positions(positions)})
            .and_then(|positions| {self.best_position(positions)})
    }
}

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
