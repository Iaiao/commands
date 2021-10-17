use std::any::Any;
use std::fmt::{Debug, Formatter};
use std::io::Write;

pub trait Argument {
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)>;

    fn get_properties(&self) -> &dyn ParserProperties;

    fn get_identifier(&self) -> &'static str;
}

impl<T, U: 'static> Argument for T
where
    T: ArgumentParser<Output = U>,
{
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        ArgumentParser::parse(self, input).map(|(i, output)| (i, Box::new(output) as Box<dyn Any>))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        ArgumentParser::get_properties(self)
    }

    fn get_identifier(&self) -> &'static str {
        ArgumentParser::get_identifier(self)
    }
}

impl Debug for dyn Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.get_identifier())
    }
}

/// Argument parser
pub trait ArgumentParser {
    type Output;

    /// Parse input string
    /// Return Some(amount of chars to skip, output) if found, None otherwise
    fn parse(&self, input: &str) -> Option<(usize, Self::Output)>;

    fn get_properties(&self) -> &dyn ParserProperties;

    fn get_identifier(&self) -> &'static str;
}

/// Used for Declare Commands packet (https://wiki.vg/Command_Data)
/// If you're creating your own argument, you don't need to implement ParserProperties
pub trait ParserProperties {
    fn write(&self, buf: &mut dyn Write) -> std::io::Result<usize>;
}

impl ParserProperties for () {
    fn write(&self, _buf: &mut dyn Write) -> std::io::Result<usize> {
        Ok(0)
    }
}
