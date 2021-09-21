use crate::dispatcher::CommandDispatcher;
use crate::parser::ArgumentParser;
use std::io::Write;
use crate::varint::write_varint;

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
                    } else if command.len() == size {
                        None
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
    
    pub fn write_to(&self, buf: &mut dyn Write) -> std::io::Result<usize> {
        let mut wrote = 0;
        match self {
            CommandNode::Root { children } => {
                let flags = 0;
                wrote += buf.write(&[flags])?;
                wrote += write_varint(children.len() as i32, buf)?;
                for child in children {
                    wrote += write_varint(*child as i32, buf)?;
                }
            }
            CommandNode::Literal { execute, name, children, .. } => {
                let mut flags = 1;
                if execute.is_some() {
                    flags |= 1 << 2;
                }
                wrote += buf.write(&[flags])?;
                wrote += write_varint(children.len() as i32, buf)?;
                for child in children {
                    wrote += write_varint(*child as i32, buf)?;
                }
                wrote += write_varint(name.as_bytes().len() as i32, buf)?;
                wrote += buf.write(name.as_bytes())?;
            }
            CommandNode::Argument { execute, name, suggestions_type, parser, children, .. } => {
                let mut flags = 2;
                if execute.is_some() {
                    flags |= 1 << 2;
                }
                if suggestions_type.is_some() {
                    flags |= 1 << 4;
                }
                wrote += buf.write(&[flags])?;
                wrote += write_varint(children.len() as i32, buf)?;
                for child in children {
                    wrote += write_varint(*child as i32, buf)?;
                }
                wrote += write_varint(name.as_bytes().len() as i32, buf)?;
                wrote += buf.write(name.as_bytes())?;
                let identifier = parser.get_identifier();
                wrote += write_varint(identifier.as_bytes().len() as i32, buf)?;
                wrote += buf.write(identifier.as_bytes())?;
                wrote += parser.get_properties().write(buf)?;
                if let Some(suggestions) = suggestions_type {
                    wrote += write_varint(suggestions.as_bytes().len() as i32, buf)?;
                    wrote += buf.write(suggestions.as_bytes())?;
                }
            }
        }
        Ok(wrote)
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
