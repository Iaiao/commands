use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use anyhow::{anyhow, bail};
use slab::Slab;

use crate::node::{CommandNode, CompletionType};
use crate::parser::ArgumentParser;
use crate::varint::write_varint;

pub type Args = Vec<Box<dyn Any>>;
pub type Completer<T> = Box<dyn Fn(&str, &mut T) -> Vec<(String, Option<String>)>>;

pub struct CommandDispatcher<T> {
    // 0 is always root node
    pub(crate) nodes: Slab<CommandNode>,
    pub(crate) executors: Slab<Box<dyn Fn(Args, T) -> bool>>,
    pub(crate) tab_completers: HashMap<String, Completer<T>>,
}

impl<T> Debug for CommandDispatcher<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CommandDispatcher")
            .field(
                "nodes",
                &self.nodes.iter().map(|(_, n)| n).collect::<Vec<_>>(),
            )
            .field(
                "executors",
                &format!("Executors ({})", self.executors.len()),
            )
            .field("tab_completers", &self.tab_completers.keys())
            .finish()
    }
}

impl<T> Default for CommandDispatcher<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> CommandDispatcher<T> {
    pub fn new() -> CommandDispatcher<T> {
        let mut nodes = Slab::new();
        nodes.insert(CommandNode::default());
        CommandDispatcher {
            nodes,
            executors: Slab::new(),
            tab_completers: HashMap::default(),
        }
    }

    pub fn find_command(&self, command: &str) -> Option<Vec<usize>> {
        self.nodes
            .get(0)
            .expect("Couldn't find root node")
            .matches(command, self)
    }

    pub fn tab_complete(
        &self,
        prompt: &str,
        mut context: T,
    ) -> Option<Vec<(String, Option<String>)>> {
        self.nodes
            .get(0)
            .expect("Couldn't find root node")
            .find_suggestions(prompt, &mut context, self)
    }

    pub fn create_command(&mut self, name: &str) -> anyhow::Result<CreateCommand<T>> {
        if name.is_empty() {
            bail!("Command name is empty")
        } else {
            let names = name.split(' ');
            let mut node = 0;
            for name in names {
                node = self.insert_child(
                    CommandNode::Literal {
                        execute: None,
                        name: name.to_owned(),
                        children: vec![],
                        parent: node,
                    },
                )?;
            }
            Ok(CreateCommand {
                current_node: node,
                dispatcher: self,
            })
        }
    }

    pub fn execute_command(&self, mut command: &str, context: T) -> bool {
        let cmd = self.find_command(command);
        if let Some(cmd) = cmd {
            let mut args = Vec::new();
            for node_id in cmd {
                if let Some(node) = self.nodes.get(node_id) {
                    match node {
                        CommandNode::Root { .. } => panic!("Found root node in find_command"),
                        CommandNode::Literal { name, execute, .. } => {
                            if let Some(execute) = execute {
                                return self.executors.get(*execute).unwrap()(args, context);
                            } else {
                                command = &command[name.len() + 1..];
                            }
                        }
                        CommandNode::Argument {
                            parser, execute, ..
                        } => {
                            if let Some(parse_result) = parser.parse(command) {
                                let (i, arg): (usize, Box<dyn Any>) = parse_result;
                                args.push(arg);
                                if execute.is_some() && command.len() == i {
                                    return self.executors.get(execute.unwrap()).unwrap()(
                                        args, context,
                                    );
                                } else if command.len() == i {
                                    return false;
                                } else {
                                    command = &command[i + 1..];
                                }
                            } else {
                                return false;
                            }
                        }
                    }
                } else {
                    return false;
                }
            }
            false
        } else {
            false
        }
    }

    pub fn packet(&self) -> std::io::Result<Vec<u8>> {
        let mut bytes = Vec::new();
        write_varint(self.nodes.len() as i32, &mut bytes)?;
        for (_, node) in &self.nodes {
            node.write_to(&mut bytes)?;
        }
        bytes.push(0);
        Ok(bytes)
    }

    pub fn get_completions(
        &self,
        completion_type: &CompletionType,
        context: &mut T,
        prompt: &str,
    ) -> Option<Vec<(String, Option<String>)>> {
        match completion_type {
            CompletionType::Custom(s) => self
                .tab_completers
                .get(s)
                .map(|completer| completer(prompt, context)),
            _ => Some(vec![]),
        }
    }

    pub fn register_tab_completion(&mut self, completion_type: &str, completer: Completer<T>) {
        self.tab_completers
            .insert(completion_type.to_owned(), completer);
    }

    #[allow(clippy::type_complexity)]
    pub fn into_parts(
        self,
    ) -> (
        Slab<CommandNode>,
        Slab<Box<dyn Fn(Args, T) -> bool>>,
        HashMap<String, Completer<T>>,
    ) {
        (self.nodes, self.executors, self.tab_completers)
    }

    pub fn add_nodes(&mut self, nodes: Vec<CommandNode>) {
        let nodes_len = self.nodes.len();
        let executors_len = self.executors.len();
        for node in nodes {
            match node {
                CommandNode::Root { .. } => (),
                CommandNode::Literal {
                    execute,
                    name,
                    children,
                    parent,
                } => {
                    let i = self.nodes.insert(CommandNode::Literal {
                        execute: execute.map(|e| e + executors_len),
                        name,
                        children: children.iter().map(|c| c + nodes_len - 1).collect(),
                        parent: if parent == 0 {
                            0
                        } else {
                            parent + nodes_len - 1
                        },
                    });
                    if parent == 0 {
                        self.nodes.get_mut(0).unwrap().add_child(i);
                    }
                }
                CommandNode::Argument {
                    execute,
                    name,
                    suggestions_type,
                    parser,
                    children,
                    parent,
                } => {
                    self.nodes.insert(CommandNode::Argument {
                        execute: execute.map(|e| e + executors_len),
                        name,
                        suggestions_type,
                        parser,
                        children: children.iter().map(|c| c + nodes_len - 1).collect(),
                        parent: if parent == 0 {
                            0
                        } else {
                            parent + nodes_len - 1
                        },
                    });
                }
            }
        }
    }

    pub fn add_executor(&mut self, executor: Box<dyn Fn(Args, T) -> bool>) -> usize {
        self.executors.insert(executor)
    }

    fn insert_child(&mut self, child: CommandNode) -> anyhow::Result<usize> {
        let parent = child.parent().unwrap();
        let i = self.nodes.insert(child);
        let parent = self
            .nodes
            .iter_mut()
            .find(|(i, _)| *i == parent)
            .map(|(_, node)| node)
            .ok_or_else(|| anyhow!("Couldn't find child node"))?;
        parent.add_child(i);
        Ok(i)
    }

    pub(crate) fn matches(&self, command: &str, node: usize) -> Option<Vec<usize>> {
        self.nodes.get(node)?.matches(command, self)
    }

    pub(crate) fn find_node_suggestions(
        &self,
        command: &str,
        context: &mut T,
        node: usize,
    ) -> Option<Vec<(String, Option<String>)>> {
        self.nodes
            .get(node)?
            .find_suggestions(command, context, self)
    }
}

pub struct CreateCommand<'a, T> {
    current_node: usize,
    dispatcher: &'a mut CommandDispatcher<T>,
}

impl<'a, T> CreateCommand<'a, T> {
    pub fn with(mut self, f: impl FnOnce(&mut Self)) -> Self {
        let node = self.current_node;
        f(&mut self);
        self.current_node = node;
        self
    }

    pub fn with_subcommand(&mut self, name: &str) -> &mut Self {
        let i = self
            .dispatcher
            .insert_child(
                CommandNode::Literal {
                    execute: None,
                    name: name.to_owned(),
                    children: vec![],
                    parent: self.current_node,
                },
            )
            .unwrap();
        self.current_node = i;
        self
    }

    pub fn with_argument(
        &mut self,
        name: &str,
        parser: Box<dyn ArgumentParser>,
        completion_type: CompletionType,
    ) -> &mut Self {
        let i = self
            .dispatcher
            .insert_child(
                CommandNode::Argument {
                    execute: None,
                    name: name.to_owned(),
                    suggestions_type: completion_type,
                    parser,
                    children: vec![],
                    parent: self.current_node,
                },
            )
            .unwrap();
        self.current_node = i;
        self
    }

    pub fn executes(&mut self, f: impl Fn(Args, T) -> bool + 'static) {
        let f = self.dispatcher.executors.insert(Box::new(f));
        self.dispatcher
            .nodes
            .get_mut(self.current_node)
            .unwrap()
            .executes(f)
    }
}
