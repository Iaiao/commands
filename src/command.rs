#[macro_export]
macro_rules! command {
    ($dispatcher: ident, $command: literal, $($name: literal: $parser: expr => $arg: ident: $typ: ty),*, $context: ident $executor: block) => {
        {
            let mut command = $crate::dispatcher::CommandDispatcher::create_command(&mut $dispatcher, $command).unwrap();
            $(
                command.with_argument($name, $parser);
            )*
            command.executes(|args, $context| {
                let mut args = args.into_iter();
                $(
                    let $arg: $typ = *args.next().unwrap().downcast::<$typ>().unwrap();
                )*
                drop(args);
                $executor
            })
        }
    };
}
