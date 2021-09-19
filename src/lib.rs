pub mod arguments;
pub mod command;
pub mod dispatcher;
mod node;
pub mod parser;

#[cfg(test)]
mod tests {
    use crate::arguments::DoubleArgument;
    use crate::dispatcher::CommandDispatcher;

    #[test]
    fn simple_command() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test 1")
            .unwrap()
            .executes(|_args, _context| true);

        let result = dispatcher.find_command("test 1");
        assert_eq!(result, Some(vec![1, 2]));
        assert_eq!(dispatcher.execute_command("test 1", ()), true);
    }

    #[test]
    fn command_with_argument() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .with_argument("arg", DoubleArgument::default())
            .executes(|args, _context| {
                *args.into_iter().next().unwrap().downcast::<f64>().unwrap() == 1.2
            });

        let result = dispatcher.find_command("test 1.2");
        assert_eq!(result, Some(vec![1, 2]));
        assert!(dispatcher.execute_command("test 1.2", ()));
    }

    #[test]
    fn command_with_subcommands() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .with(|cmd| {
                cmd.with_argument("arg", DoubleArgument::default())
                    .executes(|args, _context| {
                        *args.into_iter().next().unwrap().downcast::<f64>().unwrap() == 1.2
                    })
            })
            .with(|cmd| {
                cmd.with_subcommand("subcommand")
                    .executes(|_args, _context| true)
            });

        let result = dispatcher.find_command("test 1.2");
        assert_eq!(result, Some(vec![1, 2]));
        assert!(dispatcher.execute_command("test 1.2", ()));

        let result = dispatcher.find_command("test subcommand");
        assert_eq!(result, Some(vec![1, 3]));
        assert!(dispatcher.execute_command("test subcommand", ()));
    }

    #[test]
    fn test_macro() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        crate::command!(dispatcher,
                "test",
                "x": DoubleArgument::new(0.0..=5.0) => _x: f64,
                "y": DoubleArgument::new(0.0..3.0)  => _y: f64,
                _context {
            Ok(())
        });

        assert!(dispatcher.execute_command("test 3.5 2.5", ()));
    }
}
