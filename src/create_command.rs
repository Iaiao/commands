use std::marker::PhantomData;

use crate::args::{Combine, CombinedTuples, Func, Product, Tuple};
use crate::dispatcher::CommandDispatcher;
use crate::node::{CommandNode, CompletionType};
use crate::parser::ArgumentParser;

pub struct CreateCommand<'a, T, A: 'a + Tuple> {
    current_node: usize,
    dispatcher: &'a mut CommandDispatcher<T>,
    args_type: PhantomData<A>,
}

impl<'a, T, A: 'a + Tuple> CreateCommand<'a, T, A> {
    pub(crate) fn new(
        node: usize,
        dispatcher: &'a mut CommandDispatcher<T>,
    ) -> CreateCommand<'a, T, A> {
        CreateCommand {
            current_node: node,
            dispatcher,
            args_type: PhantomData,
        }
    }

    pub fn with(self, f: impl FnOnce(Self)) -> Self {
        let node = self.current_node;

        // TODO fix this
        unsafe {
            let ptr = self.dispatcher as *mut CommandDispatcher<T>;
            f(CreateCommand::new(node, &mut *ptr));
        }

        self
    }

    pub fn subcommand(self, name: &str) -> Self {
        let i = self
            .dispatcher
            .insert_child(CommandNode::Literal {
                execute: None,
                name: name.to_owned(),
                children: vec![],
                parent: self.current_node,
            })
            .unwrap();
        CreateCommand::new(i, self.dispatcher)
    }

    pub fn argument<Arg, Parser: ArgumentParser + 'static, Completion: Into<CompletionType>>(
        self,
        name: &str,
        parser: Parser,
        completion_type: Completion,
    ) -> CreateCommand<'a, T, CombinedTuples<A, (Arg,)>>
    where
        <A as Tuple>::HList: Combine<Product<Arg, ()>>,
    {
        let i = self
            .dispatcher
            .insert_child(CommandNode::Argument {
                execute: None,
                name: name.to_owned(),
                suggestions_type: completion_type.into(),
                parser: Box::new(parser),
                children: vec![],
                parent: self.current_node,
            })
            .unwrap();
        CreateCommand::new(i, self.dispatcher)
    }

    pub fn executes(&'a mut self, f: impl Func<T, A> + 'static) -> Self {
        let f = self.dispatcher.executors.insert(f.to_fn());
        self.dispatcher
            .nodes
            .get_mut(self.current_node)
            .unwrap()
            .executes(f);
        CreateCommand::new(self.current_node, self.dispatcher)
    }
}
