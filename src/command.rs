pub use anyhow;
pub use log;

#[macro_export]
macro_rules! command {
    ($dispatcher: ident, $command: literal, $($name: literal: $parser: expr, $completion: literal => $arg: ident: $typ: ty),*, $context: ident $executor: block) => {
        {
            let mut command = $crate::dispatcher::CommandDispatcher::create_command($dispatcher, $command).unwrap();
            $(
                command.argument($name, $parser, $crate::node::CompletionType::custom($completion));
            )*
            command.executes(move |args, mut $context| {
                let mut args = args.into_iter();
                $(
                    let $arg: $typ = *args.next().unwrap().downcast::<$typ>().unwrap();
                )*
                drop(args);
                #[allow(unused_mut)]
                let mut run = || -> $crate::command::anyhow::Result<()> {
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
            command.executes(|args, mut $context| {
                drop(args);
                #[allow(unused_mut)]
                let mut run = || -> $crate::command::anyhow::Result<()> {
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
