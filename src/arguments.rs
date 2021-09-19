use std::any::Any;
use std::io::Write;
use std::ops::{Bound, RangeBounds, RangeFull};

use crate::parser::{ArgumentParser, ParserProperties};

#[derive(PartialEq, Debug)]
pub struct DoubleArgument(pub DoubleProperties);

impl Default for DoubleArgument {
    fn default() -> Self {
        DoubleArgument::new(RangeFull)
    }
}

impl<'a> DoubleArgument {
    pub fn new<R>(range: R) -> DoubleArgument
    where
        R: RangeBounds<f64>,
    {
        DoubleArgument(DoubleProperties {
            min: range.start_bound().cloned(),
            max: range.end_bound().cloned(),
        })
    }
}

impl ArgumentParser for DoubleArgument {
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        let mut i = 0;
        for char in input.chars() {
            match char {
                '0'..='9' | '.' => i += 1,
                _ => break,
            }
        }
        let value: f64 = input[..i].parse().ok()?;
        if match self.0.min {
            Bound::Included(x) => value >= x,
            Bound::Excluded(x) => value > x,
            Bound::Unbounded => true,
        } && match self.0.max {
            Bound::Included(x) => value <= x,
            Bound::Excluded(x) => value < x,
            Bound::Unbounded => true,
        } {
            Some((i, Box::new(value)))
        } else {
            None
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &self.0
    }

    fn get_identifier(&self) -> Option<&'static str> {
        Some("brigadier:double")
    }
}

#[derive(PartialEq, Debug)]
pub struct DoubleProperties {
    min: Bound<f64>,
    max: Bound<f64>,
}

impl ParserProperties for DoubleProperties {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        let mut flags = 0;
        let mut data = Vec::new();
        let mut read = 0;
        match self.min {
            Bound::Unbounded => (),
            Bound::Included(x) | Bound::Excluded(x) => {
                flags |= 1 << 0;
                data.extend(x.to_be_bytes());
            }
        }
        match self.max {
            Bound::Unbounded => (),
            Bound::Included(x) | Bound::Excluded(x) => {
                flags |= 1 << 0;
                data.extend(x.to_be_bytes());
            }
        }
        read += buf.write(&[flags])?;
        read += buf.write(&data)?;
        Ok(read)
    }
}

/*

    Float {
        min: Option<f32>,
        max: Option<f32>
    },
    Integer {
        min: Option<i32>,
        max: Option<i32>
    },
    Long {
        min: Option<i64>,
        max: Option<i64>
    },
    String(StringType),
    Entity {
        only_single: bool,
        only_players: bool,
    },
    ScoreHolder {
        allow_multiple: bool
    },
    Range {
        decimals: bool
    }
}

pub enum StringType {
    SingleWord,
    QuotablePhrase,
    GreedyPhrase,
}
*/
