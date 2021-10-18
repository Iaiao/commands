use std::any::Any;
use std::fmt::Debug;
use std::io::Write;

pub trait Argument: Debug + Send {
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)>;

    fn get_properties(&self) -> &dyn ParserProperties;

    fn get_identifier(&self) -> &'static str;

    fn clone_boxed(&self) -> Box<dyn Argument>;
}

impl<T: 'static, U: 'static> Argument for T
where
    T: ArgumentParser<Output = U> + Debug + Clone + Send,
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

    fn clone_boxed(&self) -> Box<dyn Argument> {
        Box::new(self.clone())
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
