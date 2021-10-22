use std::fmt::Debug;
use std::marker::PhantomData;

use crate::args::{Combine, CombinedTuples, Func, Product, Tuple};
use crate::dispatcher::{Args, CommandDispatcher, CommandOutput};
use crate::node::{CommandNode, CompletionType};
use crate::parser::ArgumentParser;

pub struct CreateCommand<'a, T, Text, A: 'a + Tuple> {
    current_node: usize,
    dispatcher: &'a mut CommandDispatcher<T, Text>,
    args_type: PhantomData<A>,
}

impl<'a, T, Text, A: 'a + Tuple> CreateCommand<'a, T, Text, A> {
    pub(crate) fn new(
        node: usize,
        dispatcher: &'a mut CommandDispatcher<T, Text>,
    ) -> CreateCommand<'a, T, Text, A> {
        CreateCommand {
            current_node: node,
            dispatcher,
            args_type: PhantomData,
        }
    }

    pub fn current_node_id(&self) -> usize {
        self.current_node
    }

    pub fn redirect(self, redirect_node: usize) -> Self {
        match self.dispatcher.nodes.get_mut(self.current_node).unwrap() {
            CommandNode::Root { .. } => unreachable!(),
            CommandNode::Literal { redirect, .. } | CommandNode::Argument { redirect, .. } => {
                *redirect = Some(redirect_node)
            }
        }
        self
    }

    // TODO change `Args` to `A`
    /// Execute multiple commands with different contexts
    pub fn fork(
        self,
        f: impl FnMut(&mut Args, T, Box<&mut dyn FnMut(&mut Args, T) -> CommandOutput>) -> CommandOutput
            + 'static,
    ) -> Self {
        match self.dispatcher.nodes.get_mut(self.current_node).unwrap() {
            CommandNode::Root { .. } => unreachable!(),
            CommandNode::Literal { fork, .. } | CommandNode::Argument { fork, .. } => {
                *fork = Some(self.dispatcher.forks.insert(Box::new(f)))
            }
        }
        self
    }

    pub fn with(self, f: impl FnOnce(Self)) -> Self {
        let node = self.current_node;

        // TODO fix this
        unsafe {
            let ptr = self.dispatcher as *mut CommandDispatcher<T, Text>;
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
                redirect: None,
                fork: None,
            });
        CreateCommand::new(i, self.dispatcher)
    }

    pub fn argument<
        Parser: ArgumentParser + Debug + Clone + Send + 'static,
        Completion: Into<CompletionType>,
    >(
        self,
        name: &str,
        parser: Parser,
        completion_type: Completion,
    ) -> CreateCommand<'a, T, Text, CombinedTuples<A, (Parser::Output,)>>
    where
        <A as Tuple>::HList: Combine<Product<Parser::Output, ()>>,
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
                redirect: None,
                fork: None,
            });
        CreateCommand::new(i, self.dispatcher)
    }

    pub fn executes(self, f: impl Func<T, A> + 'static) -> Self {
        let f = self.dispatcher.executors.insert(f.to_fn());
        self.dispatcher
            .nodes
            .get_mut(self.current_node)
            .unwrap()
            .executes(f);
        CreateCommand::new(self.current_node, self.dispatcher)
    }
}
