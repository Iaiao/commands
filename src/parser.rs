use std::any::Any;
use std::io::Write;
use std::fmt::Debug;

/// Argument parser
pub trait ArgumentParser: Debug {
    /// Parse input string to [`Self::Output`]
    /// Return Some(amount of chars to skip, output) if found, None otherwise
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)>;

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
