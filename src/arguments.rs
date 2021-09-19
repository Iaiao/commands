use std::any::Any;
use std::io::Write;
use std::ops::{Bound, RangeBounds, RangeFull};

use crate::parser::{ArgumentParser, ParserProperties};

macro_rules! integer_arguments {
    ($argument: ident, $properties: ident, $typ: ty: $chars: pat, $identifier: literal) => {
        #[derive(PartialEq, Debug)]
        pub struct $argument(pub $properties);

        impl Default for $argument {
            fn default() -> Self {
                $argument::new(RangeFull)
            }
        }

        impl $argument {
            pub fn new<R>(range: R) -> $argument
            where
                R: RangeBounds<$typ>,
            {
                $argument($properties {
                    min: range.start_bound().cloned(),
                    max: range.end_bound().cloned(),
                })
            }
        }

        impl ArgumentParser for $argument {
            fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
                let mut i = 0;
                for char in input.chars() {
                    match char {
                        $chars => i += 1,
                        _ => break,
                    }
                }
                let value: $typ = input[..i].parse().ok()?;
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
                Some($identifier)
            }
        }

        #[derive(PartialEq, Debug)]
        pub struct $properties {
            min: Bound<$typ>,
            max: Bound<$typ>,
        }

        impl ParserProperties for $properties {
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
    };
}
integer_arguments!(DoubleArgument, DoubleProperties, f64: '0'..='9' | '.', "brigadier:double");
integer_arguments!(FloatArgument, FloatProperties, f32: '0'..='9' | '.', "brigadier:float");
integer_arguments!(IntegerArgument, IntegerProperties, i32: '0'..='9', "brigadier:integer");
integer_arguments!(LongArgument, LongProperties, i64: '0'..='9', "brigadier:long");

#[derive(PartialEq, Debug)]
pub struct StringArgument(pub StringProperties);

impl Default for StringArgument {
    fn default() -> Self {
        StringArgument::new(StringProperties::SingleWord)
    }
}

impl StringArgument {
    pub fn new(properties: StringProperties) -> StringArgument {
        StringArgument(properties)
    }
}

impl ArgumentParser for StringArgument {
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        match self.0 {
            StringProperties::SingleWord => {
                if let Some(i) = input.find(' ') {
                    Some((i, Box::new(input[..i].to_string())))
                } else {
                    None
                }
            }
            StringProperties::QuotablePhrase => {
                if input.starts_with('"') {
                    if let (Some(i), _) =
                        input
                            .chars()
                            .enumerate()
                            .fold((None, None), |(found, prev), (i, char)| {
                                if found.is_some() {
                                    (found, prev)
                                } else if char == '"' && (prev.is_some() && prev.unwrap() != '\\') {
                                    (Some(i), None)
                                } else {
                                    (None, Some(char))
                                }
                            })
                    {
                        return Some((i + 1, Box::new(input[1..i].to_string())));
                    }
                    None
                } else {
                    // behave like SingleWord
                    if let Some(i) = input.find(' ') {
                        Some((i, Box::new(input[..i].to_string())))
                    } else {
                        None
                    }
                }
            }
            StringProperties::GreedyPhrase => Some((input.len(), Box::new(input.to_string()))),
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &self.0
    }

    fn get_identifier(&self) -> Option<&'static str> {
        Some("brigadier:string")
    }
}

#[derive(PartialEq, Debug)]
pub enum StringProperties {
    SingleWord,
    QuotablePhrase,
    GreedyPhrase,
}

impl ParserProperties for StringProperties {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        buf.write(&[match self {
            StringProperties::SingleWord => 0,
            StringProperties::QuotablePhrase => 1,
            StringProperties::GreedyPhrase => 2,
        }])
    }
}

/*
    TODO
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
