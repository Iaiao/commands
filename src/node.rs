use crate::dispatcher::CommandDispatcher;
use crate::parser::ArgumentParser;

#[derive(Debug)]
pub enum CommandNode<T>
where
    T: ArgumentParser,
{
    Root {
        children: Vec<usize>,
    },
    Literal {
        execute: Option<usize>,
        name: String,
        children: Vec<usize>,
        parent: usize,
    },
    Argument {
        execute: Option<usize>,
        name: String,
        suggestions_type: Option<String>,
        parser: T,
        children: Vec<usize>,
        parent: usize,
    },
}

impl<T, U> PartialEq<CommandNode<U>> for CommandNode<T>
where
    T: ArgumentParser,
    U: ArgumentParser,
{
    fn eq(&self, other: &CommandNode<U>) -> bool {
        match (&self, &other) {
            (
                &CommandNode::Root { children },
                &CommandNode::Root {
                    children: children_1,
                },
            ) => children == children_1,
            (
                &CommandNode::Literal {
                    execute,
                    name,
                    children,
                    parent,
                },
                &CommandNode::Literal {
                    execute: execute_1,
                    name: name_1,
                    children: children_1,
                    parent: parent_1,
                },
            ) => {
                execute == execute_1
                    && name == name_1
                    && children == children_1
                    && parent == parent_1
            }
            (&CommandNode::Argument { .. }, &CommandNode::Argument { .. }) => {
                unimplemented!()
            }
            _ => false,
        }
    }
}

impl<T> CommandNode<T>
where
    T: ArgumentParser,
{
    pub fn matches<U>(
        &self,
        command: &str,
        dispatcher: &CommandDispatcher<U>,
    ) -> Option<Vec<usize>> {
        match self {
            CommandNode::Root { children } => {
                let mut result = None;
                for child in children {
                    if let Some(nodes) = dispatcher.matches(command, *child) {
                        let mut res = vec![*child];
                        res.extend(nodes);
                        result = Some(res);
                        break;
                    }
                }
                result
            }
            CommandNode::Literal {
                name,
                children,
                execute,
                ..
            } => {
                if execute.is_some() && command == name {
                    Some(Vec::new())
                } else if command.starts_with(&format!("{} ", name)) {
                    let command = &command[name.len() + 1..];
                    let mut result = None;
                    for child in children {
                        if let Some(nodes) = dispatcher.matches(command, *child) {
                            let mut res = vec![*child];
                            res.extend(nodes);
                            result = Some(res);
                            break;
                        }
                    }
                    result
                } else {
                    None
                }
            }
            CommandNode::Argument {
                execute,
                parser,
                children,
                ..
            } => {
                if let Some((size, _)) = parser.parse(command) {
                    if execute.is_some() {
                        Some(Vec::new())
                    } else {
                        let command = &command[size + 1..];
                        let mut result = None;
                        for child in children {
                            if let Some(nodes) = dispatcher.matches(command, *child) {
                                let mut res = vec![*child];
                                res.extend(nodes);
                                result = Some(res);
                                break;
                            }
                        }
                        result
                    }
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn add_child(&mut self, child: usize) {
        match self {
            CommandNode::Root { children }
            | CommandNode::Literal { children, .. }
            | CommandNode::Argument { children, .. } => children.push(child),
        }
    }

    pub(crate) fn executes(&mut self, f: usize) {
        match self {
            CommandNode::Root { .. } => panic!("executes() on root node"),
            CommandNode::Literal { execute, .. } => *execute = Some(f),
            CommandNode::Argument { execute, .. } => *execute = Some(f),
        }
    }
}
