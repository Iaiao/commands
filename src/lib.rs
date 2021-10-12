mod args;
pub mod arguments;
pub mod create_command;
pub mod dispatcher;
pub mod node;
pub mod parser;
mod varint;

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::arguments::*;
    use crate::dispatcher::CommandDispatcher;
    use crate::node::CompletionType;
    use crate::parser::{ArgumentParser, ParserProperties};

    #[test]
    fn simple_command() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test 1")
            .unwrap()
            .executes(|_context| true);

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
            .argument(
                "arg",
                DoubleArgument::default(),
                CompletionType::custom("none"),
            )
            .executes(|_context, num: f64| num == 1.2);

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
                cmd.argument(
                    "arg",
                    DoubleArgument::default(),
                    CompletionType::custom("none"),
                )
                .executes(|_context, num: f64| num == 1.2);
            })
            .with(|cmd| {
                cmd.subcommand("subcommand").executes(|_context| true);
            });

        let result = dispatcher.find_command("test 1.2");
        assert_eq!(result, Some(vec![1, 2]));
        assert!(dispatcher.execute_command("test 1.2", ()));

        let result = dispatcher.find_command("test subcommand");
        assert_eq!(result, Some(vec![1, 3]));
        assert!(dispatcher.execute_command("test subcommand", ()));
    }

    #[test]
    fn test_packet() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .with(|cmd| {
                cmd.argument::<f64, _>(
                    "arg",
                    DoubleArgument::default(),
                    CompletionType::custom("none"),
                );
            })
            .with(|cmd| {
                cmd.subcommand("subcommand").executes(|_context| true);
            });
        assert_eq!(
            dispatcher.packet().unwrap(),
            vec![
                4, 0, 1, 1, 1, 2, 2, 3, 4, 116, 101, 115, 116, 18, 0, 3, 97, 114, 103, 16, 98, 114,
                105, 103, 97, 100, 105, 101, 114, 58, 100, 111, 117, 98, 108, 101, 0, 20, 109, 105,
                110, 101, 99, 114, 97, 102, 116, 58, 97, 115, 107, 95, 115, 101, 114, 118, 101,
                114, 5, 0, 10, 115, 117, 98, 99, 111, 109, 109, 97, 110, 100, 0
            ]
        )
    }

    #[test]
    fn test_completions() {
        #[derive(Debug)]
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
                &StringProperties::SingleWord
            }

            fn get_identifier(&self) -> &'static str {
                "brigadier:string"
            }
        }

        let dispatcher = &mut CommandDispatcher::<()>::new();
        dispatcher.create_command("test").unwrap();
        dispatcher
            .create_command("test2")
            .unwrap()
            .argument::<Gamemode, _>(
                "gamemode",
                GamemodeArgument,
                CompletionType::custom("gamemode"),
            );
        dispatcher.create_command("3test2").unwrap();

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

        dispatcher
            .create_command("testspaces")
            .unwrap()
            .argument::<String, _>(
                "s",
                StringArgument::new(StringProperties::GreedyPhrase),
                CompletionType::custom("none"),
            );

        assert_eq!(dispatcher.tab_complete(r#"testspaces a "#, ()), None);
        assert_eq!(dispatcher.tab_complete(r#"testspaces a b"#, ()), None);
    }
}
