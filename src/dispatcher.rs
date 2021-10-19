use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use anyhow::{anyhow, bail};
use slab::Slab;

use crate::create_command::CreateCommand;
use crate::node::{CommandNode, CompletionType, Fork};
use crate::varint::write_varint;

pub type Args = Vec<Box<dyn Any>>;
pub type Completer<T> = Box<dyn Fn(&str, &mut T) -> TabCompletion>;
// (replacement start, replacement end, Vec<(replacement, Option<tooltip>)>)
pub type TabCompletion = (usize, usize, Vec<(String, Option<String>)>);
pub type CommandOutput = anyhow::Result<i32>;

pub struct CommandDispatcher<T> {
    // 0 is always root node
    pub(crate) nodes: Slab<CommandNode<T>>,
    pub(crate) executors: Slab<Box<dyn Fn(Args, T) -> CommandOutput>>,
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

    pub fn tab_complete(&self, prompt: &str, mut context: T) -> Option<TabCompletion> {
        self.nodes
            .get(0)
            .expect("Couldn't find root node")
            .find_suggestions(prompt, &mut context, self)
    }

    pub fn create_command(&mut self, name: &str) -> anyhow::Result<CreateCommand<T, ()>> {
        if name.is_empty() {
            bail!("Command name is empty")
        } else {
            let names = name.split(' ');
            let mut node = 0;
            for name in names {
                node = self.insert_child(CommandNode::<T>::Literal {
                    execute: None,
                    name: name.to_owned(),
                    children: vec![],
                    parent: node,
                    redirect: None,
                    fork: None,
                })?;
            }
            Ok(CreateCommand::new(node, self))
        }
    }

    pub fn execute_command(&mut self, mut command: &str, context: T) -> Option<CommandOutput> {
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
                            if let Some(fork) = fork.as_mut() {
                                // TODO replace with safe code
                                forks.push((None, unsafe { &mut *(fork as *mut _) }));
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
                                if let Some(fork) = fork.as_mut() {
                                    // TODO replace with safe code
                                    forks.push((Some(args.len() - 1), unsafe {
                                        &mut *(fork as *mut _)
                                    }));
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
                let execute = self.executors.get(execute).unwrap();
                let mut successful_forks = 0;
                let res = next(0, execute, &mut forks, args, context, &mut successful_forks);

                fn next<T>(
                    i: usize,
                    execute: &dyn Fn(Args, T) -> CommandOutput,
                    forks: &mut Vec<(Option<usize>, &mut Box<Fork<T>>)>,
                    args: Args,
                    context: T,
                    successful_forks: &mut i32,
                ) -> CommandOutput {
                    if forks.len() <= i {
                        match execute(args, context) {
                            Ok(res) => {
                                *successful_forks += 1;
                                Ok(res)
                            }
                            Err(err) => Err(err),
                        }
                    } else {
                        // TODO rewrite this
                        let (arg, fork) = forks.get_mut(i).unwrap();
                        let fork = unsafe {
                            &mut *(fork as *mut &mut Box<
                                dyn FnMut(
                                    Args,
                                    T,
                                    Option<usize>,
                                    Box<&mut dyn FnMut(Args, T) -> CommandOutput>,
                                ) -> CommandOutput,
                            >)
                        };
                        fork(
                            args,
                            context,
                            *arg,
                            Box::new(&mut |args, context| {
                                next(i + 1, execute, forks, args, context, successful_forks)
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
    ) -> Option<TabCompletion> {
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
        completer: impl Fn(&str, &mut T) -> TabCompletion + 'static,
    ) {
        self.tab_completers
            .insert(completion_type.to_owned(), Box::new(completer));
    }

    #[allow(clippy::type_complexity)]
    pub fn into_parts(
        self,
    ) -> (
        Slab<CommandNode<T>>,
        Slab<Box<dyn Fn(Args, T) -> CommandOutput>>,
        HashMap<String, Completer<T>>,
    ) {
        (self.nodes, self.executors, self.tab_completers)
    }

    pub fn add_nodes(&mut self, nodes: Vec<CommandNode<T>>) {
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
                        redirect: redirect.map(|r| if r == 0 { 0 } else { nodes_len + r }),
                        fork,
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
                        redirect: redirect.map(|r| if r == 0 { 0 } else { nodes_len + r }),
                        fork,
                    });
                }
            }
        }
    }

    pub fn add_executor(&mut self, executor: impl Fn(Args, T) -> CommandOutput + 'static) -> usize {
        self.executors.insert(Box::new(executor))
    }

    pub fn nodes(&self) -> impl Iterator<Item = (usize, &CommandNode<T>)> {
        self.nodes.iter()
    }

    pub(crate) fn insert_child(&mut self, child: CommandNode<T>) -> anyhow::Result<usize> {
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
    ) -> Option<TabCompletion> {
        self.nodes
            .get(node)?
            .find_suggestions(command, context, self)
    }
}
