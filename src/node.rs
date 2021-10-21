use std::fmt::Debug;
use std::io::Write;

use crate::dispatcher::{CommandDispatcher, TabCompletion};
use crate::parser::Argument;
use crate::varint::write_varint;

#[derive(Debug)]
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
        redirect: Option<usize>,
        fork: Option<usize>,
    },
    Argument {
        execute: Option<usize>,
        name: String,
        suggestions_type: CompletionType,
        parser: Box<dyn Argument>,
        children: Vec<usize>,
        parent: usize,
        redirect: Option<usize>,
        fork: Option<usize>,
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
    pub fn matches<T>(
        &self,
        command: &str,
        dispatcher: &CommandDispatcher<T>,
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
                redirect,
                ..
            } => {
                if execute.is_some() && command == name {
                    Some(Vec::new())
                } else if command.starts_with(&format!("{} ", name)) {
                    let command = &command[name.len() + 1..];
                    let mut result = None;
                    let mut children = children.clone();
                    if let Some(redirect) = redirect {
                        children.extend(dispatcher.nodes.get(*redirect).unwrap().children());
                    }
                    for child in children {
                        if let Some(nodes) = dispatcher.matches(command, child) {
                            let mut res = vec![child];
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
                redirect,
                ..
            } => {
                if let Some((size, _)) = parser.parse(command) {
                    if execute.is_some() && command.len() == size {
                        Some(Vec::new())
                    } else if command.len() == size {
                        None
                    } else {
                        let command = &command[size + 1..];
                        let mut result = None;
                        let mut children = children.clone();
                        if let Some(redirect) = redirect {
                            children.extend(dispatcher.nodes.get(*redirect).unwrap().children());
                        }
                        for child in children {
                            if let Some(nodes) = dispatcher.matches(command, child) {
                                let mut res = vec![child];
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

    pub fn find_suggestions<T>(
        &self,
        mut prompt: &str,
        context: &mut T,
        dispatcher: &CommandDispatcher<T>,
    ) -> Option<TabCompletion> {
        match self {
            CommandNode::Root { children } => {
                let mut result: Option<TabCompletion> = None;
                for child in children {
                    if let Some(suggestions) =
                        dispatcher.find_node_suggestions(prompt, context, *child)
                    {
                        if let Some(result) = result.as_mut() {
                            result.2.extend(suggestions.2);
                        } else {
                            result = Some(suggestions);
                        }
                    }
                }
                result
            }
            CommandNode::Literal {
                children,
                name,
                redirect,
                ..
            } => {
                if name.starts_with(prompt) {
                    Some((0, prompt.len(), vec![(name.to_string(), None)]))
                } else if prompt.starts_with(&format!("{} ", name)) {
                    prompt = &prompt[name.len() + 1..];
                    let mut result: Option<TabCompletion> = None;
                    let mut children = children.clone();
                    if let Some(redirect) = redirect {
                        children.extend(dispatcher.nodes.get(*redirect).unwrap().children());
                    }
                    for child in children {
                        if let Some(suggestions) =
                            dispatcher.find_node_suggestions(prompt, context, child)
                        {
                            if let Some(result) = result.as_mut() {
                                if result.0 == suggestions.0 && result.1 == suggestions.1 {
                                    result.2.extend(suggestions.2);
                                } else {
                                    log::warn!("Tab completion ambiguity: different replacement beginning/end")
                                }
                            } else {
                                result = Some(suggestions);
                            }
                        }
                    }
                    if let Some(suggestions) = result.as_mut() {
                        suggestions.0 += name.len() + 1
                    }
                    result
                } else {
                    None
                }
            }
            CommandNode::Argument {
                parser,
                children,
                suggestions_type,
                redirect,
                ..
            } => {
                if let Some((size, _)) = parser.parse(prompt) {
                    if prompt.len() == size {
                        dispatcher.get_completions(suggestions_type, context, prompt)
                    } else {
                        prompt = &prompt[size + 1..];
                        let mut result: Option<TabCompletion> = None;
                        let mut children = children.clone();
                        if let Some(redirect) = redirect {
                            children.extend(dispatcher.nodes.get(*redirect).unwrap().children());
                        }
                        for child in children {
                            if let Some(suggestions) =
                                dispatcher.find_node_suggestions(prompt, context, child)
                            {
                                if let Some(result) = result.as_mut() {
                                    if result.0 == suggestions.0 && result.1 == suggestions.1 {
                                        result.2.extend(suggestions.2);
                                    } else {
                                        log::warn!("Tab completion ambiguity: different replacement beginning/end")
                                    }
                                } else {
                                    result = Some(suggestions);
                                }
                            }
                        }
                        if let Some(suggestions) = result.as_mut() {
                            suggestions.0 += size + 1
                        }
                        result
                    }
                } else {
                    dispatcher.get_completions(suggestions_type, context, prompt)
                }
            }
        }
    }

    pub fn parent(&self) -> Option<usize> {
        match self {
            CommandNode::Root { .. } => None,
            CommandNode::Literal { parent, .. } | CommandNode::Argument { parent, .. } => {
                Some(*parent)
            }
        }
    }

    pub fn children(&self) -> &Vec<usize> {
        match self {
            CommandNode::Root { children }
            | CommandNode::Literal { children, .. }
            | CommandNode::Argument { children, .. } => children,
        }
    }

    pub fn children_mut(&mut self) -> &mut Vec<usize> {
        match self {
            CommandNode::Root { children }
            | CommandNode::Literal { children, .. }
            | CommandNode::Argument { children, .. } => children,
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
                redirect,
                ..
            } => {
                let mut flags = 1;
                if execute.is_some() {
                    flags |= 1 << 2;
                }
                if redirect.is_some() {
                    flags |= 1 << 3;
                }
                wrote += buf.write(&[flags])?;
                wrote += write_varint(children.len() as i32, buf)?;
                for child in children {
                    wrote += write_varint(*child as i32, buf)?;
                }
                if let Some(redirect) = redirect {
                    wrote = write_varint(*redirect as i32, buf)?;
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
                redirect,
                ..
            } => {
                let mut flags = 2;
                if execute.is_some() {
                    flags |= 1 << 2;
                }
                if redirect.is_some() {
                    flags |= 1 << 3;
                }
                flags |= 1 << 4;
                wrote += buf.write(&[flags])?;
                wrote += write_varint(children.len() as i32, buf)?;
                for child in children {
                    wrote += write_varint(*child as i32, buf)?;
                }
                if let Some(redirect) = redirect {
                    wrote = write_varint(*redirect as i32, buf)?;
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
        self.children_mut().push(child)
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
    pub fn custom(s: impl Into<String>) -> CompletionType {
        CompletionType::Custom(s.into())
    }

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

impl<T> From<T> for CompletionType
where
    T: Into<String>,
{
    fn from(s: T) -> Self {
        CompletionType::custom(s)
    }
}
