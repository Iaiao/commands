pub mod args;
pub mod arguments;
pub mod create_command;
pub mod dispatcher;
pub mod node;
pub mod parser;
mod varint;

#[cfg(test)]
mod tests {
    use crate::arguments::*;
    use crate::dispatcher::CommandDispatcher;
    use crate::parser::{ArgumentParser, ParserProperties};

    #[test]
    fn simple_command() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test 1")
            .unwrap()
            .executes(|_context| Ok(0));

        let result = dispatcher.find_command("test 1");
        assert_eq!(result, Some(vec![1, 2]));
        assert_eq!(
            dispatcher.execute_command("test 1", ()).unwrap().unwrap(),
            0
        );
    }

    #[test]
    fn command_with_argument() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .argument("arg", DoubleArgument::default(), "none")
            .executes(|_context, num| {
                if num == 1.2 {
                    Ok(0)
                } else {
                    Err(anyhow::anyhow!("Not 1.2"))
                }
            });

        let result = dispatcher.find_command("test 1.2");
        assert_eq!(result, Some(vec![1, 2]));
        assert!(dispatcher.execute_command("test 1.2", ()).is_some());
    }

    #[test]
    fn command_with_subcommands() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .with(|cmd| {
                cmd.argument("arg", DoubleArgument::default(), "none")
                    .executes(|_context, num| {
                        if num == 1.2 {
                            Ok(0)
                        } else {
                            Err(anyhow::anyhow!("Not 1.2"))
                        }
                    });
            })
            .with(|cmd| {
                cmd.subcommand("subcommand").executes(|_context| Ok(0));
            });

        let result = dispatcher.find_command("test 1.2");
        assert_eq!(result, Some(vec![1, 2]));
        assert!(dispatcher.execute_command("test 1.2", ()).is_some());

        let result = dispatcher.find_command("test subcommand");
        assert_eq!(result, Some(vec![1, 3]));
        assert!(dispatcher.execute_command("test subcommand", ()).is_some());
    }

    #[test]
    fn test_packet() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        dispatcher
            .create_command("test")
            .unwrap()
            .with(|cmd| {
                cmd.argument("arg", DoubleArgument::default(), "none");
            })
            .with(|cmd| {
                cmd.subcommand("subcommand").executes(|_context| Ok(0));
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
        #[derive(Debug, Clone)]
        pub struct GamemodeArgument;

        impl ArgumentParser for GamemodeArgument {
            type Output = Gamemode;

            fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
                match input.split(' ').next().unwrap() {
                    "survival" => Some(("survival".len(), Gamemode::Survival)),
                    "creative" => Some(("creative".len(), Gamemode::Creative)),
                    "adventure" => Some(("adventure".len(), Gamemode::Adventure)),
                    "spectator" => Some(("spectator".len(), Gamemode::Spectator)),
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
        dispatcher.create_command("test2").unwrap().argument(
            "gamemode",
            GamemodeArgument,
            "gamemode",
        );
        dispatcher.create_command("3test2").unwrap();

        assert_eq!(
            dispatcher.tab_complete(r#"te"#, ()),
            Some((
                0,
                2,
                vec![("test".to_string(), None), ("test2".to_string(), None)]
            ))
        );

        dispatcher.register_tab_completion("gamemode", |prompt, _| {
            (
                0,
                prompt.len(),
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
                ],
            )
        });
        assert_eq!(
            dispatcher.tab_complete(r#"test2 "#, ()),
            Some((
                6,
                0,
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
            ))
        );

        dispatcher.create_command("testspaces").unwrap().argument(
            "s",
            StringArgument::GREEDY_PHRASE,
            "none",
        );

        assert_eq!(dispatcher.tab_complete(r#"testspaces a "#, ()), None);
        assert_eq!(dispatcher.tab_complete(r#"testspaces a b"#, ()), None);
    }

    #[test]
    fn test_redirect() {
        let mut dispatcher = CommandDispatcher::<()>::new();
        let root = 0;
        dispatcher
            .create_command("test")
            .unwrap()
            .executes(|_| Ok(1));
        let command = dispatcher.create_command("execute").unwrap();
        let execute = command.current_node_id();
        command
            .with(|command| {
                command
                    .subcommand("as")
                    .argument("entity", EntityArgument::ENTITIES, "none")
                    .redirect(execute);
            })
            .with(|command| {
                command.subcommand("run").redirect(root);
            });

        assert_eq!(
            dispatcher.find_command("execute run test"),
            Some(vec![2, 5, 1])
        );
        assert_eq!(
            dispatcher
                .execute_command("execute as @a run test", ())
                .unwrap()
                .unwrap(),
            1
        );
    }
}
