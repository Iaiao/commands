use std::io::Write;

use crate::dispatcher::CommandDispatcher;
use crate::parser::ArgumentParser;
use crate::varint::write_varint;

#[repr(C)]
pub enum CommandNode {
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
        suggestions_type: CompletionType,
        parser: Box<dyn ArgumentParser>,
        children: Vec<usize>,
        parent: usize,
    },
}

#[allow(clippy::derivable_impls)]
impl Default for CommandNode {
    fn default() -> Self {
        CommandNode::Root {
            children: Default::default(),
        }
    }
}

impl CommandNode {
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

    pub fn find_suggestions<U>(
        &self,
        mut prompt: &str,
        context: &mut U,
        dispatcher: &CommandDispatcher<U>,
    ) -> Option<Vec<(String, Option<String>)>> {
        if !prompt.contains(' ') {
            match self {
                CommandNode::Root { children } => {
                    let mut result: Option<Vec<(String, Option<String>)>> = None;
                    for child in children {
                        if let Some(suggestions) =
                            dispatcher.find_node_suggestions(prompt, context, *child)
                        {
                            if let Some(result) = result.as_mut() {
                                result.extend(suggestions);
                            } else {
                                result = Some(suggestions);
                            }
                        }
                    }
                    result
                }
                CommandNode::Literal { name, .. } => Some(vec![(name.to_owned(), None)]),
                CommandNode::Argument {
                    suggestions_type, ..
                } => dispatcher.get_completions(suggestions_type, context, prompt),
            }
        } else {
            match self {
                CommandNode::Root { children } => {
                    let mut result: Option<Vec<(String, Option<String>)>> = None;
                    for child in children {
                        if let Some(suggestions) =
                            dispatcher.find_node_suggestions(prompt, context, *child)
                        {
                            if let Some(result) = result.as_mut() {
                                result.extend(suggestions);
                            } else {
                                result = Some(suggestions);
                            }
                        }
                    }
                    result
                }
                CommandNode::Literal { children, name, .. } => {
                    let mut result: Option<Vec<(String, Option<String>)>> = None;
                    if prompt.starts_with(&format!("{} ", name)) {
                        prompt = &prompt[name.len() + 1..];
                        for child in children {
                            if let Some(suggestions) =
                                dispatcher.find_node_suggestions(prompt, context, *child)
                            {
                                if let Some(result) = result.as_mut() {
                                    result.extend(suggestions);
                                } else {
                                    result = Some(suggestions);
                                }
                            }
                        }
                    }
                    result
                }
                CommandNode::Argument {
                    parser, children, ..
                } => {
                    let mut result: Option<Vec<(String, Option<String>)>> = None;
                    if let Some((size, _)) = parser.parse(prompt) {
                        prompt = &prompt[size + 1..];
                        for child in children {
                            if let Some(suggestions) =
                                dispatcher.find_node_suggestions(prompt, context, *child)
                            {
                                if let Some(result) = result.as_mut() {
                                    result.extend(suggestions);
                                } else {
                                    result = Some(suggestions);
                                }
                            }
                        }
                    }
                    result
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
            CommandNode::Literal {
                execute,
                name,
                children,
                ..
            } => {
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
            CommandNode::Argument {
                execute,
                name,
                suggestions_type,
                parser,
                children,
                ..
            } => {
                let mut flags = 2;
                if execute.is_some() {
                    flags |= 1 << 2;
                }
                flags |= 1 << 4;
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
                wrote += write_varint(suggestions_type.as_send_str().as_bytes().len() as i32, buf)?;
                wrote += buf.write(suggestions_type.as_send_str().as_bytes())?;
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

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub enum CompletionType {
    Custom(String),
    AllRecipes,
    AvailableSounds,
    AvailableBiomes,
    SummonableEntities,
}

impl CompletionType {
    fn as_send_str(&self) -> &'static str {
        match self {
            CompletionType::Custom(_) => "minecraft:ask_server",
            CompletionType::AllRecipes => "minecraft:all_recipes",
            CompletionType::AvailableSounds => "minecraft:available_sounds",
            CompletionType::AvailableBiomes => "minecraft:available_biomes",
            CompletionType::SummonableEntities => "minecraft:summonable_entities",
        }
    }
}
