use std::any::Any;

/// Argument parser
pub trait ArgumentParser {
    /// Parse input string to [`Self::Output`]
    /// Return Some(amount of chars to skip, output) if found, None otherwise
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)>;

    fn get_properties(&self) -> &dyn ParserProperties;

    fn get_identifier(&self) -> Option<&'static str>;
}

/// Used for Declare Commands packet (https://wiki.vg/Command_Data)
/// If you're creating your own argument, you don't need to implement ParserProperties
pub trait ParserProperties {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
}

impl ArgumentParser for () {
    fn parse(&self, _input: &str) -> Option<(usize, Box<dyn Any>)> {
        unreachable!()
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        unreachable!()
    }

    fn get_identifier(&self) -> Option<&'static str> {
        unreachable!()
    }
}

impl<T> ArgumentParser for Box<T>
where
    T: ArgumentParser + ?Sized,
{
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        T::parse(self, input)
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        T::get_properties(self)
    }

    fn get_identifier(&self) -> Option<&'static str> {
        T::get_identifier(self)
    }
}
