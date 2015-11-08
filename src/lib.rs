// Copyright 2015 Jerome Rasky <jerome@rasky.co>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
// or http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.
#![feature(binary_heap_extras)]
#![feature(iter_arith)]
#[macro_use]
extern crate log;
extern crate env_logger;

mod constants;
mod search;

pub use search::{SearchBase, LineInfo};
