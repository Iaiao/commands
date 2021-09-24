pub use anyhow;
pub use log;

#[macro_export]
macro_rules! command {
    ($dispatcher: ident, $command: literal, $($name: literal: $parser: expr, $completion: literal => $arg: ident: $typ: ty),*, $context: ident $executor: block) => {
        {
            let mut command = $crate::dispatcher::CommandDispatcher::create_command($dispatcher, $command).unwrap();
            $(
                command.with_argument($name, $parser, $crate::node::CompletionType::Custom($completion.to_owned()));
            )*
            command.executes(move |args, $context| {
                let mut args = args.into_iter();
                $(
                    let $arg: $typ = *args.next().unwrap().downcast::<$typ>().unwrap();
                )*
                drop(args);
                let run = || -> $crate::command::anyhow::Result<()> {
                    $executor
                };
                match run() {
                    Ok(()) => true,
                    Err(err) => {
                        $crate::command::log::warn!("Error in command {}: {}", $command, err);
                        false
                    }
                }
            })
        }
    };
    ($dispatcher: ident, $command: literal, $context: ident $executor: block) => {
        {
            let mut command = $crate::dispatcher::CommandDispatcher::create_command($dispatcher, $command).unwrap();
            command.executes(|args, $context| {
                drop(args);
                let run = || -> $crate::command::anyhow::Result<()> {
                    $executor
                };
                match run() {
                    Ok(()) => true,
                    Err(err) => {
                        $crate::command::log::warn!("Error in command {}: {}", $command, err);
                        false
                    }
                }
            })
        }
    };
}
