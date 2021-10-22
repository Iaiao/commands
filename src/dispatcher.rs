use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use slab::Slab;

use crate::create_command::CreateCommand;
use crate::node::{CommandNode, CompletionType};
use crate::varint::write_varint;

pub type Args = Vec<Box<dyn Any>>;
pub type Completer<T, Text> = Box<dyn Fn(&str, &mut T) -> TabCompletion<Text>>;
// (replacement start, replacement end, Vec<(replacement, Option<tooltip>)>)
pub type TabCompletion<Text> = (usize, usize, Vec<(String, Option<Text>)>);
pub type CommandOutput = anyhow::Result<i32>;
pub type Fork<T> = dyn for<'a> FnMut(
    &mut Args,
    T,
    Box<&'a mut dyn FnMut(&mut Args, T) -> CommandOutput>,
) -> CommandOutput;

pub struct CommandDispatcher<T, Text> {
    // 0 is always root node
    pub(crate) nodes: Slab<CommandNode>,
    pub(crate) executors: Slab<Box<dyn Fn(&mut Args, T) -> CommandOutput>>,
    pub(crate) tab_completers: HashMap<String, Completer<T, Text>>,
    pub(crate) forks: Slab<Box<Fork<T>>>,
}

impl<T, Text> Debug for CommandDispatcher<T, Text> {
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
            .field(
                "forks",
                &self.forks.iter().map(|(i, _)| i).collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl<T, Text> Default for CommandDispatcher<T, Text> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, Text> CommandDispatcher<T, Text> {
    pub fn new() -> CommandDispatcher<T, Text> {
        let mut nodes = Slab::new();
        nodes.insert(CommandNode::default());
        CommandDispatcher {
            nodes,
            executors: Slab::new(),
            tab_completers: HashMap::default(),
            forks: Slab::new(),
        }
    }

    pub fn find_command(&self, command: &str) -> Option<Vec<usize>> {
        self.nodes
            .get(0)
            .expect("Couldn't find root node")
            .matches(command, self)
    }

    pub fn tab_complete(&self, prompt: &str, mut context: T) -> Option<TabCompletion<Text>> {
        self.nodes
            .get(0)
            .expect("Couldn't find root node")
            .find_suggestions(prompt, &mut context, self)
    }

    pub fn create_command(&mut self, name: &str) -> CreateCommand<T, Text, ()> {
        if name.is_empty() {
            panic!("Command name is empty")
        } else {
            let names = name.split(' ');
            let mut node = 0;
            for name in names {
                node = self.insert_child(CommandNode::Literal {
                    execute: None,
                    name: name.to_owned(),
                    children: vec![],
                    parent: node,
                    redirect: None,
                    fork: None,
                });
            }
            CreateCommand::new(node, self)
        }
    }

    pub fn execute_command(&mut self, mut command: &str, context: T) -> Option<CommandOutput>
    where
        T: 'static,
    {
        let cmd = self.find_command(command);
        let mut executor = None;
        if let Some(cmd) = cmd {
            let mut forks = Vec::new();
            let mut args = Vec::new();
            for node_id in cmd {
                if let Some(node) = self.nodes.get_mut(node_id) {
                    match node {
                        CommandNode::Root { .. } => panic!("Found root node in find_command"),
                        CommandNode::Literal {
                            name,
                            execute,
                            fork,
                            ..
                        } => {
                            if let Some(fork) = fork {
                                forks.push(*fork);
                            }
                            if execute.is_some() && command == name {
                                executor = Some(execute.unwrap());
                                break;
                            } else {
                                command = &command[name.len() + 1..];
                            }
                        }
                        CommandNode::Argument {
                            parser,
                            execute,
                            fork,
                            ..
                        } => {
                            if let Some(parse_result) = parser.parse(command) {
                                let (i, arg): (usize, Box<dyn Any>) = parse_result;
                                args.push(arg);
                                if let Some(fork) = fork {
                                    forks.push(*fork);
                                }
                                if execute.is_some() && command.len() == i {
                                    executor = Some(execute.unwrap());
                                    break;
                                } else if command.len() == i {
                                    return None;
                                } else {
                                    command = &command[i + 1..];
                                }
                            } else {
                                return None;
                            }
                        }
                    }
                } else {
                    return None;
                }
            }
            executor.map(|execute| {
                let mut successful_forks = 0;
                let res = next(
                    0,
                    execute,
                    &mut forks,
                    &mut args,
                    context,
                    &mut successful_forks,
                    self,
                );

                fn next<T: 'static, Text>(
                    i: usize,
                    execute: usize,
                    forks: &mut Vec<usize>,
                    args: &mut Args,
                    context: T,
                    successful_forks: &mut i32,
                    dispatcher: &mut CommandDispatcher<T, Text>,
                ) -> CommandOutput {
                    if forks.len() <= i {
                        match dispatcher.executors.get_mut(execute).unwrap()(args, context) {
                            Ok(res) => {
                                *successful_forks += 1;
                                Ok(res)
                            }
                            Err(err) => Err(err),
                        }
                    } else {
                        let fork = forks.get_mut(i).unwrap();
                        // SAFETY: `forks` is pub(crate), so the fork can't invalidate itself
                        let f = unsafe {
                            &mut *(dispatcher.forks.get_mut(*fork).unwrap() as *mut Fork<T>)
                        };
                        f(
                            args,
                            context,
                            Box::new(&mut |args, context| {
                                next(
                                    i + 1,
                                    execute,
                                    forks,
                                    args,
                                    context,
                                    successful_forks,
                                    dispatcher,
                                )
                            }),
                        )
                    }
                }

                if successful_forks == 1 {
                    res
                } else {
                    Ok(successful_forks)
                }
            })
        } else {
            None
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
    ) -> Option<TabCompletion<Text>> {
        match completion_type {
            CompletionType::Custom(s) => self
                .tab_completers
                .get(s)
                .map(|completer| completer(prompt, context)),
            _ => Some((prompt.len(), 0, vec![])),
        }
    }

    pub fn register_tab_completion(
        &mut self,
        completion_type: &str,
        completer: impl Fn(&str, &mut T) -> TabCompletion<Text> + 'static,
    ) {
        self.tab_completers
            .insert(completion_type.to_owned(), Box::new(completer));
    }

    #[allow(clippy::type_complexity)]
    pub fn into_parts(
        self,
    ) -> (
        Slab<CommandNode>,
        Slab<Box<dyn Fn(&mut Args, T) -> CommandOutput>>,
        HashMap<String, Completer<T, Text>>,
        Slab<Box<Fork<T>>>,
    ) {
        (self.nodes, self.executors, self.tab_completers, self.forks)
    }

    pub fn add_nodes(&mut self, nodes: Vec<CommandNode>) {
        let nodes_len = self.nodes.len();
        let executors_len = self.executors.len();
        let forks_len = self.forks.len();
        for node in nodes {
            match node {
                CommandNode::Root { .. } => (),
                CommandNode::Literal {
                    execute,
                    name,
                    children,
                    parent,
                    redirect,
                    fork,
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
                        redirect: redirect.map(|r| if r == 0 { 0 } else { nodes_len + r - 1 }),
                        fork: fork.map(|f| f + forks_len),
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
                    redirect,
                    fork,
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
                        redirect: redirect.map(|r| if r == 0 { 0 } else { nodes_len + r - 1 }),
                        fork: fork.map(|f| f + forks_len),
                    });
                }
            }
        }
    }

    pub fn add_executor(
        &mut self,
        executor: impl Fn(&mut Args, T) -> CommandOutput + 'static,
    ) -> usize {
        self.executors.insert(Box::new(executor))
    }

    pub fn add_fork(&mut self, fork: Box<Fork<T>>) -> usize {
        self.forks.insert(fork)
    }

    pub fn nodes(&self) -> impl Iterator<Item = (usize, &CommandNode)> {
        self.nodes.iter()
    }

    pub fn forks(&self) -> impl Iterator<Item = (usize, &Box<Fork<T>>)> {
        self.forks.iter()
    }

    pub fn fork(&mut self, fork: usize) -> Option<&mut Box<Fork<T>>> {
        self.forks.get_mut(fork)
    }

    pub(crate) fn insert_child(&mut self, child: CommandNode) -> usize {
        let parent = child.parent().unwrap();
        let i = self.nodes.insert(child);
        let parent = self
            .nodes
            .iter_mut()
            .find(|(i, _)| *i == parent)
            .map(|(_, node)| node)
            .unwrap_or_else(|| panic!("Couldn't find parent node"));
        parent.add_child(i);
        i
    }

    pub fn matches(&self, command: &str, node: usize) -> Option<Vec<usize>> {
        self.nodes.get(node)?.matches(command, self)
    }

    pub(crate) fn find_node_suggestions(
        &self,
        command: &str,
        context: &mut T,
        node: usize,
    ) -> Option<TabCompletion<Text>> {
        self.nodes
            .get(node)?
            .find_suggestions(command, context, self)
    }
}
