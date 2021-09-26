pub mod arguments;
pub mod command;
pub mod dispatcher;
pub mod node;
pub mod parser;
mod varint;

#[cfg(test)]
mod tests {
    use crate::arguments::*;
    use crate::dispatcher::CommandDispatcher;
    use crate::node::CompletionType;
    use crate::parser::{ArgumentParser, ParserProperties};
    use std::any::Any;
    use std::io::Write;

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
            .with_argument(
                "arg",
                Box::new(DoubleArgument::default()),
                CompletionType::Custom("none".to_string()),
            )
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
                cmd.with_argument(
                    "arg",
                    Box::new(DoubleArgument::default()),
                    CompletionType::Custom("none".to_string()),
                )
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
        let dispatcher = &mut CommandDispatcher::<()>::new();
        crate::command!(dispatcher,
                "test",
                "x": IntegerArgument::new(0..=5), "none" => _x: i32,
                "y": DoubleArgument::new(0.0..3.0), "none" => _y: f64,
                "entities": EntityArgument::default(), "entity" => _entities: EntitySelector,
                "string": StringArgument::new(StringProperties::GreedyPhrase), "none" => _s: String,
                _context {
            Ok(())
        });

        assert!(dispatcher.execute_command(
            r#"test 3 2.5 @a[name="!a b c",type=hoglin,distance=..10] abc def"#,
            ()
        ));
    }

    #[test]
    fn test_packet() {
        let dispatcher = &mut CommandDispatcher::<()>::new();
        crate::command!(dispatcher,
                "test",
                "x": IntegerArgument::new(0..=5), "none" => _x: i32,
                "y": DoubleArgument::new(0.0..3.0), "none" => _y: f64,
                "entities": EntityArgument::default(), "entity" => _entities: EntitySelector,
                "string": StringArgument::new(StringProperties::GreedyPhrase), "none" => _s: String,
                _context {
            Ok(())
        });
        assert_eq!(
            dispatcher.packet().unwrap(),
            vec![
                6, 0, 1, 1, 1, 1, 2, 4, 116, 101, 115, 116, 18, 1, 3, 1, 120, 17, 98, 114, 105,
                103, 97, 100, 105, 101, 114, 58, 105, 110, 116, 101, 103, 101, 114, 1, 0, 0, 0, 0,
                0, 0, 0, 5, 20, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 97, 115, 107, 95,
                115, 101, 114, 118, 101, 114, 18, 1, 4, 1, 121, 16, 98, 114, 105, 103, 97, 100,
                105, 101, 114, 58, 100, 111, 117, 98, 108, 101, 1, 0, 0, 0, 0, 0, 0, 0, 0, 64, 8,
                0, 0, 0, 0, 0, 0, 20, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 97, 115, 107,
                95, 115, 101, 114, 118, 101, 114, 18, 1, 5, 8, 101, 110, 116, 105, 116, 105, 101,
                115, 16, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 101, 110, 116, 105, 116,
                121, 0, 20, 109, 105, 110, 101, 99, 114, 97, 102, 116, 58, 97, 115, 107, 95, 115,
                101, 114, 118, 101, 114, 22, 0, 6, 115, 116, 114, 105, 110, 103, 16, 98, 114, 105,
                103, 97, 100, 105, 101, 114, 58, 115, 116, 114, 105, 110, 103, 2, 20, 109, 105,
                110, 101, 99, 114, 97, 102, 116, 58, 97, 115, 107, 95, 115, 101, 114, 118, 101,
                114, 0
            ]
        )
    }

    #[test]
    fn test_completions() {
        pub struct GamemodeArgument;

        impl ArgumentParser for GamemodeArgument {
            fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
                match input.split(' ').next().unwrap() {
                    "survival" => Some(("survival".len(), Box::new(Gamemode::Survival))),
                    "creative" => Some(("creative".len(), Box::new(Gamemode::Creative))),
                    "adventure" => Some(("adventure".len(), Box::new(Gamemode::Adventure))),
                    "spectator" => Some(("spectator".len(), Box::new(Gamemode::Spectator))),
                    _ => None,
                }
            }

            fn get_properties(&self) -> &dyn ParserProperties {
                &()
            }

            fn get_identifier(&self) -> &'static str {
                "brigadier:string"
            }
        }

        impl ParserProperties for () {
            fn write(&self, _buf: &mut dyn Write) -> std::io::Result<usize> {
                Ok(0)
            }
        }

        let dispatcher = &mut CommandDispatcher::<()>::new();
        crate::command!(dispatcher,
            "test",
            _context {
                Ok(())
        });
        crate::command!(dispatcher,
            "test2",
            "gamemode": GamemodeArgument, "gamemode" => _gamemode: Gamemode,
            _context {
                Ok(())
        });
        crate::command!(dispatcher,
            "3test2",
            _context {
                Ok(())
        });

        assert_eq!(
            dispatcher.tab_complete(r#"te"#, ()),
            Some(vec![
                ("test".to_string(), None),
                ("test2".to_string(), None)
            ])
        );

        dispatcher.register_tab_completion(
            "gamemode",
            Box::new(|_, _| {
                vec![
                    (
                        "survival".to_string(),
                        Some("Survival gamemode".to_string()),
                    ),
                    (
                        "creative".to_string(),
                        Some("Creative gamemode".to_string()),
                    ),
                    (
                        "adventure".to_string(),
                        Some("Adventure gamemode".to_string()),
                    ),
                    (
                        "spectator".to_string(),
                        Some("Spectator gamemode".to_string()),
                    ),
                ]
            }),
        );
        assert_eq!(
            dispatcher.tab_complete(r#"test2 "#, ()),
            Some(vec![
                (
                    "survival".to_string(),
                    Some("Survival gamemode".to_string()),
                ),
                (
                    "creative".to_string(),
                    Some("Creative gamemode".to_string()),
                ),
                (
                    "adventure".to_string(),
                    Some("Adventure gamemode".to_string()),
                ),
                (
                    "spectator".to_string(),
                    Some("Spectator gamemode".to_string()),
                ),
            ])
        );

        crate::command!(dispatcher,
            "testspaces",
            "s": StringArgument::new(StringProperties::GreedyPhrase), "none" => _s: String,
            _context {
                Ok(())
        });

        assert_eq!(dispatcher.tab_complete(r#"testspaces a "#, ()), None);
        assert_eq!(dispatcher.tab_complete(r#"testspaces a b"#, ()), None);
    }
}
