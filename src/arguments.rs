use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ops::{Bound, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeToInclusive};
use std::str::FromStr;

use anyhow::{anyhow, bail};
use quartz_nbt::NbtCompound;
use serde::de::{Error, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use uuid::Uuid;

use crate::arguments::entity_selector_serde::EntitySelectorDeserializer;
use crate::parser::{ArgumentParser, ParserProperties};

// Unstable, should be removed when we increment our MSRV
fn cloned_bound<T: Clone>(bound: Bound<&T>) -> Bound<T> {
    match bound {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(x) => Bound::Included(x.clone()),
        Bound::Excluded(x) => Bound::Excluded(x.clone()),
    }
}

fn split_two_dots_once(s: &str) -> Option<(&str, &str)> {
    let start = s.find("..")?;
    let end = start + "..".len();
    Some((&s[..start], &s[end..]))
}

macro_rules! impl_integer_argument {
    ($argument: ident, $properties: ident, $typ: ty, $is_float: literal, $identifier: literal) => {
        #[derive(PartialEq, Debug, Clone)]
        pub struct $argument(pub $properties);

        impl Default for $argument {
            fn default() -> Self {
                $argument::new(RangeFull)
            }
        }

        impl $argument {
            pub fn new<R>(range: R) -> $argument
            where
                R: RangeBounds<$typ>,
            {
                $argument($properties {
                    min: cloned_bound(range.start_bound()),
                    max: cloned_bound(range.end_bound()),
                })
            }
        }

        impl ArgumentParser for $argument {
            type Output = $typ;

            fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
                let mut i = 0;
                for char in input.chars() {
                    match char {
                        '-' if i == 0 => i += 1,
                        '0'..='9' => i += 1,
                        '.' if $is_float => i += 1,
                        _ => break,
                    }
                }
                let value: $typ = input[..i].parse().ok()?;
                if match self.0.min {
                    Bound::Included(x) => value >= x,
                    Bound::Excluded(x) => value > x,
                    Bound::Unbounded => true,
                } && match self.0.max {
                    Bound::Included(x) => value <= x,
                    Bound::Excluded(x) => value < x,
                    Bound::Unbounded => true,
                } {
                    Some((i, value))
                } else {
                    None
                }
            }

            fn get_properties(&self) -> &dyn ParserProperties {
                &self.0
            }

            fn get_identifier(&self) -> &'static str {
                $identifier
            }
        }

        #[derive(PartialEq, Debug, Clone)]
        pub struct $properties {
            min: Bound<$typ>,
            max: Bound<$typ>,
        }

        impl ParserProperties for $properties {
            fn write(&self, buf: &mut dyn Write) -> std::io::Result<usize> {
                let mut flags = 0;
                let mut data = Vec::new();
                let mut read = 0;
                match self.min {
                    Bound::Unbounded => (),
                    Bound::Included(x) | Bound::Excluded(x) => {
                        flags |= 1 << 0;
                        data.extend(&x.to_be_bytes());
                    }
                }
                match self.max {
                    Bound::Unbounded => (),
                    Bound::Included(x) | Bound::Excluded(x) => {
                        flags |= 1 << 1;
                        data.extend(&x.to_be_bytes());
                    }
                }
                read += buf.write(&[flags])?;
                read += buf.write(&data)?;
                Ok(read)
            }
        }
    };
}

impl_integer_argument!(
    DoubleArgument,
    DoubleProperties,
    f64,
    true,
    "brigadier:double"
);
impl_integer_argument!(FloatArgument, FloatProperties, f32, true, "brigadier:float");
impl_integer_argument!(
    IntegerArgument,
    IntegerProperties,
    i32,
    false,
    "brigadier:integer"
);
impl_integer_argument!(LongArgument, LongProperties, i64, false, "brigadier:long");

#[derive(PartialEq, Debug, Clone)]
pub struct StringArgument(pub StringProperties);

impl Default for StringArgument {
    fn default() -> Self {
        StringArgument::new(StringProperties::SingleWord)
    }
}

impl StringArgument {
    pub const SINGLE_WORD: StringArgument = StringArgument::new(StringProperties::SingleWord);
    pub const QUOTABLE_PHRASE: StringArgument =
        StringArgument::new(StringProperties::QuotablePhrase);
    pub const GREEDY_PHRASE: StringArgument = StringArgument::new(StringProperties::GreedyPhrase);

    pub const fn new(properties: StringProperties) -> StringArgument {
        StringArgument(properties)
    }
}

impl ArgumentParser for StringArgument {
    type Output = String;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        match self.0 {
            StringProperties::SingleWord => input
                .find(' ')
                .or_else(|| Some(input.len()))
                .map(|i| (i, input[..i].to_string())),
            StringProperties::QuotablePhrase => {
                Self::find_quoted_phrase_or_read_until(input, &[' '])
            }
            StringProperties::GreedyPhrase => {
                let i = if input.ends_with(' ') {
                    input.len() - 1
                } else {
                    input.len()
                };
                Some((i, input[..i].to_string()))
            }
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &self.0
    }

    fn get_identifier(&self) -> &'static str {
        "brigadier:string"
    }
}

impl StringArgument {
    pub fn find_quoted_phrase_or_read_until(
        input: &str,
        chars: &[char],
    ) -> Option<(usize, String)> {
        if input.starts_with('"') {
            if let (Some(i), _) =
                input
                    .chars()
                    .enumerate()
                    .fold((None, None), |(found, prev), (i, char)| {
                        if found.is_some() {
                            (found, prev)
                        } else if char == '"' && (prev.is_some() && prev.unwrap() != '\\') {
                            (Some(i), None)
                        } else {
                            (None, Some(char))
                        }
                    })
            {
                Some((i + 1, input[1..i].to_string()))
            } else {
                None
            }
        } else {
            // behave like SingleWord
            let s = input.split(chars).next().unwrap();
            Some((s.len(), s.to_string()))
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum StringProperties {
    SingleWord,
    QuotablePhrase,
    GreedyPhrase,
}

impl ParserProperties for StringProperties {
    fn write(&self, buf: &mut dyn Write) -> std::io::Result<usize> {
        buf.write(&[match self {
            StringProperties::SingleWord => 0,
            StringProperties::QuotablePhrase => 1,
            StringProperties::GreedyPhrase => 2,
        }])
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct EntityArgument(pub EntityProperties);

impl EntityArgument {
    pub const ENTITY: EntityArgument = EntityArgument(EntityProperties {
        single: true,
        only_players: false,
    });
    pub const PLAYER: EntityArgument = EntityArgument(EntityProperties {
        single: true,
        only_players: true,
    });
    pub const ENTITIES: EntityArgument = EntityArgument(EntityProperties {
        single: false,
        only_players: false,
    });
    pub const PLAYERS: EntityArgument = EntityArgument(EntityProperties {
        single: false,
        only_players: true,
    });
}

impl Default for EntityArgument {
    fn default() -> Self {
        EntityArgument::new(EntityProperties {
            single: false,
            only_players: false,
        })
    }
}

impl EntityArgument {
    pub fn new(properties: EntityProperties) -> EntityArgument {
        EntityArgument(properties)
    }
}

impl ArgumentParser for EntityArgument {
    type Output = EntitySelector;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        self.parse(input, false).map(|(i, selector)| (i, selector))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &self.0
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:entity"
    }
}

impl EntityArgument {
    pub fn parse(&self, mut input: &str, allow_trailing: bool) -> Option<(usize, EntitySelector)> {
        let mut requirements = Vec::new();
        match input.split('[').next().unwrap() {
            s if s.len() < 2 => {
                return None;
            }
            s if &s[..2] == "@p" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Nearest,
                ));
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            s if &s[..2] == "@r" => {
                requirements.push(EntitySelectorPredicate::Sort(EntitySelectorSorting::Random));
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            s if &s[..2] == "@a" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Arbitrary,
                ));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            s if &s[..2] == "@e" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Arbitrary,
                ));
            }
            s if &s[..2] == "@s" => {
                requirements.push(EntitySelectorPredicate::Sender);
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            something => {
                let something = something.split_whitespace().next()?;
                let selector = Uuid::from_str(something)
                    .map(EntitySelector::Uuid)
                    .unwrap_or_else(|_| EntitySelector::Name(something.to_owned()));
                return if (self.0.only_players && matches!(selector, EntitySelector::Uuid(_)))
                    || (matches!(&selector, EntitySelector::Name(name) if name.len() > 16
                        || name.len() < 2
                        || !name.chars().all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))))
                {
                    None
                } else {
                    Some((something.len(), selector))
                };
            }
        };
        input = &input[2..];
        let i = if input.starts_with('[') {
            let (predicates, left) =
                entity_selector_serde::from_str::<Vec<EntitySelectorPredicate>>(input).ok()?;
            requirements.extend(predicates);
            input.len() - left.len()
        } else {
            0
        };
        let mut only_allows_players = false;
        let mut single = false;
        for requirement in &requirements {
            match requirement {
                EntitySelectorPredicate::Type(EntityType::Player) => only_allows_players = true,
                EntitySelectorPredicate::Advancements(_) => only_allows_players = true,
                EntitySelectorPredicate::Gamemode(_) => only_allows_players = true,
                EntitySelectorPredicate::Level(_) => only_allows_players = true,
                EntitySelectorPredicate::Sender => only_allows_players = true, // TODO console shouldn't do this
                EntitySelectorPredicate::Limit(1) => single = true,
                _ => (),
            }
        }
        // TODO change return type to Result to report parsing errors
        #[allow(clippy::if_same_then_else)]
        if self.0.single && !single {
            None
        } else if self.0.only_players && !only_allows_players {
            None
        } else if !allow_trailing && input.len() != i && input.chars().nth(i).unwrap() != ' ' {
            None
        } else {
            Some((i + 2, EntitySelector::Selector(requirements)))
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct EntityProperties {
    single: bool,
    only_players: bool,
}

impl ParserProperties for EntityProperties {
    fn write(&self, buf: &mut dyn Write) -> std::io::Result<usize> {
        let mut flags = 0;
        if self.single {
            flags |= 1 << 0;
        }
        if self.only_players {
            flags |= 1 << 1;
        }
        buf.write(&[flags])
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntitySelector {
    Selector(Vec<EntitySelectorPredicate>),
    Name(String),
    Uuid(Uuid),
}

/// A struct for data in @e\[data\]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum EntitySelectorPredicate {
    /// Filter target selection based on the entity's identifier.
    /// The given entity type must be a valid entity ID or entity type tag
    /// used to identify different types of entities internally.
    /// The namespace can be left out if the ID is within the minecraft namespace.
    /// This is always "player" for @a
    Type(EntityType),
    /// Filter target selection based on the entity's advancements.
    /// This naturally filters out all non-player targets.
    /// All advancements are in a single object), with a list of individual
    /// advancement IDs between the braces afterward.
    /// The values are true or false.
    /// For advancements with one criterion, testing for that
    /// criterion always gives the same results as testing for the advancement
    Advancements(HashMap<String, AdvancementPredicate>),
    /// Specifies the range of distance.
    /// Float ranges are supported to select a specific region.
    /// Only unsigned values are allowed.
    Distance(WrappedRange<f64>),
    /// Signed difference from `x`
    Dx(f64),
    /// Signed difference from `y`
    Dy(f64),
    /// Signed difference from `z`
    Dz(f64),
    /// Filter target selection by game mode.
    /// This naturally filters out all non-player targets.
    /// Arguments testing for equality cannot be duplicated,
    /// while arguments testing for inequality can.
    Gamemode(BoolPredicate<Gamemode>),
    /// Filter target selection based on the entity's experience levels.
    /// This naturally filters out all non-player targets.
    Level(WrappedRange<u32>),
    /// Limit the number of selectable targets for a target selector.
    /// When using the variables @p and @r, this argument defaults to one.
    /// Applying the limiting argument to them may increase the number of
    /// nearest or random targets selected. When applying this argument to @a or @e,
    /// this argument returns only a limited number of targets
    Limit(u32),
    /// Filter target selection by name. Values are strings, so spaces are allowed only
    /// if quotes are applied. This cannot be a JSON text compound.
    /// Arguments testing for equality cannot be duplicated,
    /// while arguments testing for inequality can.
    Name(BoolPredicate<String>),
    /// Filter target selection based on the entity's NBT data
    //Nbt(BoolPredicate<NbtPredicate>),
    /// Filter target selection by predicates.
    /// The given values must be a valid predicate represented by a resource location
    Predicate(BoolPredicate<String>),
    /// Filter target selection based on their scores in the specified objectives.
    /// All tested objectives are in a single object,
    /// with a list of individual score arguments between braces afterward
    Scores(HashMap<String, WrappedRange<i32>>),
    /// Specify selection priority
    Sort(EntitySelectorSorting),
    /// Filter target selection based on the entity's scoreboard tags (not implemented yet)
    Tag(BoolPredicate<Tag>),
    /// Filter target selection based on teams.
    /// Arguments testing for equality cannot be duplicated,
    /// while arguments testing for inequality can.
    Team(BoolPredicate<String>),
    /// Define a position in the world the selector starts at,
    /// for use with the `distance` argument, the volume (`dx`, `dy`, `dz`)
    /// arguments, or the `limit` argument.
    /// Using these arguments alone will not restrict the entities found,
    /// and will only affect the sorting of targets
    X(f64),
    /// Define a position in the world the selector starts at,
    /// for use with the `distance` argument, the volume (`dx`, `dy`, `dz`)
    /// arguments, or the `limit` argument.
    /// Using these arguments alone will not restrict the entities found,
    /// and will only affect the sorting of targets
    Y(f64),
    /// Define a position in the world the selector starts at,
    /// for use with the `distance` argument, the volume (`dx`, `dy`, `dz`)
    /// arguments, or the `limit` argument.
    /// Using these arguments alone will not restrict the entities found,
    /// and will only affect the sorting of targets
    Z(f64),
    /// Filter target selection based on the entity's rotation along the pitch axis,
    /// measured in degrees. Values range from -90 (straight up)
    /// to 0 (at the horizon) to +90 (straight down)
    XRotation(WrappedRange<f32>),
    /// Filter target selection based on the entity's rotation along the yaw axis,
    /// measured clockwise in degrees from due south (or the positive Z direction).
    /// Values vary from -180 (facing due north) to -90 (facing due east)
    /// to 0 (facing due south) to +90 (facing due west) to +180 (facing due north again)
    YRotation(WrappedRange<f32>),
    /// Only match command sender. Only used when deserializing @s
    #[serde(skip)]
    Sender,
}

#[derive(Serialize, Deserialize)]
pub struct NbtPredicate; // TODO

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum EntitySelectorSorting {
    /// Sort by increasing distance. (Default for @p)
    Nearest,
    /// Sort by decreasing distance.
    Furthest,
    /// Sort randomly. (Default for @r)
    Random,
    /// Sort by time created. (Default for @e, @a)
    Arbitrary,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum AdvancementPredicate {
    /// Include only players with the specified advancements and values.
    Value(bool),
    /// Include only players with the specified advancement's criteria.
    Criteria(HashMap<String, bool>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum WrappedRange<T> {
    RangeFrom(RangeFrom<T>),
    RangeTo(RangeToInclusive<T>),
    Range(RangeInclusive<T>),
}

impl<T> WrappedRange<T> {
    fn start_bound(&self) -> Bound<&T> {
        match self {
            WrappedRange::RangeFrom(range) => range.start_bound(),
            WrappedRange::RangeTo(range) => range.start_bound(),
            WrappedRange::Range(range) => range.start_bound(),
        }
    }
    fn end_bound(&self) -> Bound<&T> {
        match self {
            WrappedRange::RangeFrom(range) => range.end_bound(),
            WrappedRange::RangeTo(range) => range.end_bound(),
            WrappedRange::Range(range) => range.end_bound(),
        }
    }

    fn bound_value(bound: Bound<&T>) -> Option<&T> {
        match bound {
            Bound::Included(x) => Some(x),
            Bound::Excluded(x) => Some(x),
            Bound::Unbounded => None,
        }
    }

    pub fn contains<U>(&self, item: &U) -> bool
    where
        T: PartialOrd<U>,
        U: ?Sized + PartialOrd<T>,
    {
        (match self.start_bound() {
            Bound::Included(start) => start <= item,
            Bound::Excluded(start) => start < item,
            Bound::Unbounded => true,
        }) && (match self.end_bound() {
            Bound::Included(end) => item <= end,
            Bound::Excluded(end) => item < end,
            Bound::Unbounded => true,
        })
    }
}

impl<T> From<RangeInclusive<T>> for WrappedRange<T> {
    fn from(range: RangeInclusive<T>) -> Self {
        WrappedRange::Range(range)
    }
}

impl<T> From<RangeToInclusive<T>> for WrappedRange<T> {
    fn from(range: RangeToInclusive<T>) -> Self {
        WrappedRange::RangeTo(range)
    }
}

impl<T> From<RangeFrom<T>> for WrappedRange<T> {
    fn from(range: RangeFrom<T>) -> Self {
        WrappedRange::RangeFrom(range)
    }
}

impl<T> Serialize for WrappedRange<T>
where
    T: ToString,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!(
            "{}..{}",
            Self::bound_value(self.start_bound())
                .map(ToString::to_string)
                .unwrap_or_default(),
            Self::bound_value(self.end_bound())
                .map(ToString::to_string)
                .unwrap_or_default()
        ))
    }
}

impl<'de, T> Deserialize<'de> for WrappedRange<T>
where
    T: Deserialize<'de> + FromStr + Debug + PartialOrd,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(RangeVisitor(PhantomData))
    }
}

pub struct RangeVisitor<T>(PhantomData<T>);

impl<'de, T> Visitor<'de> for RangeVisitor<T>
where
    T: FromStr + Debug + PartialOrd,
{
    type Value = WrappedRange<T>;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("a range")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_string(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Self::parse(v).map_err(|err| E::custom(format!("Failed to parse a range: {}", err)))
    }
}

impl<T> RangeVisitor<T>
where
    T: FromStr + Debug + PartialOrd,
{
    pub fn parse(s: String) -> anyhow::Result<WrappedRange<T>> {
        let (start, end) = split_two_dots_once(&s).unwrap_or((&s, &s));
        let (start, end) = (
            if start.is_empty() {
                None
            } else {
                Some(
                    start
                        .parse()
                        .map_err(|_| anyhow!("Invalid range (start): {}", s))?,
                )
            },
            if end.is_empty() {
                None
            } else {
                Some(
                    end.parse()
                        .map_err(|_| anyhow!("Invalid range (end): {}", s))?,
                )
            },
        );
        match (start, end) {
            (None, None) => bail!("Double-open ranges (\"..\") are not allowed"),
            (Some(start), None) => Ok(WrappedRange::RangeFrom(start..)),
            (None, Some(end)) => Ok(WrappedRange::RangeTo(..=end)),
            (Some(start), Some(end)) => {
                if start > end {
                    bail!(
                        "{:?} is greater than {:?}, so {0:?}..{1:?} is invalid",
                        start,
                        end
                    )
                } else {
                    Ok(WrappedRange::Range(start..=end))
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tag(pub NbtCompound);

#[derive(Debug, PartialEq, Clone)]
pub struct BoolPredicate<T>(pub bool, pub T);

impl<T> Serialize for BoolPredicate<T>
where
    T: ToString,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.0 {
            serializer.serialize_str(&self.1.to_string())
        } else {
            serializer.serialize_str(&format!("!{}", self.1.to_string()))
        }
    }
}

impl<'de, T> Deserialize<'de> for BoolPredicate<T>
where
    T: Deserialize<'de> + FromStr,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(BoolPredicateVisitor(PhantomData))
    }
}

impl Serialize for BoolPredicate<Tag> {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        unimplemented!()
    }
}

impl<'de> Deserialize<'de> for BoolPredicate<Tag> {
    fn deserialize<D>(mut deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // SAFETY
        // Can't do a check because D is not 'static.
        // But anyway the only usage of BoolPredicate<Tag> is entity selector
        let d =
            unsafe { &mut *(&mut deserializer as *mut _ as *mut &mut EntitySelectorDeserializer) };
        let mut b = true;
        if d.peek_char() == Ok('!') {
            b = false;
            d.next_char().unwrap();
        }
        let (tag, chars) = quartz_nbt::snbt::parse_and_size(d.input)
            .map_err(|e| D::Error::custom(e.to_string()))?;
        d.input = &d.input[chars..];
        Ok(BoolPredicate(b, Tag(tag)))
    }
}

pub struct BoolPredicateVisitor<T>(PhantomData<T>);

impl<'de, T> Visitor<'de> for BoolPredicateVisitor<T>
where
    T: FromStr,
{
    type Value = BoolPredicate<T>;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("a bool predicate")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_string(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Self::parse(v).map_err(|_| E::custom("Failed to parse value"))
    }
}

impl<T> BoolPredicateVisitor<T>
where
    T: FromStr,
{
    pub fn parse(s: String) -> Result<BoolPredicate<T>, <T as FromStr>::Err> {
        Ok(if let Some(stripped) = s.strip_prefix('!') {
            BoolPredicate(false, stripped.parse()?)
        } else {
            BoolPredicate(true, s.parse()?)
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Gamemode {
    Survival,
    Creative,
    Adventure,
    Spectator,
}

impl Display for Gamemode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Gamemode::Survival => "survival",
            Gamemode::Creative => "creative",
            Gamemode::Adventure => "adventure",
            Gamemode::Spectator => "spectator",
        })
    }
}

impl FromStr for Gamemode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "survival" => Ok(Gamemode::Survival),
            "creative" => Ok(Gamemode::Creative),
            "adventure" => Ok(Gamemode::Adventure),
            "spectator" => Ok(Gamemode::Spectator),
            _ => Err(()),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum EntityType {
    Player,
    AreaEffectCloud,
    ArmorStand,
    Arrow,
    Axolotl,
    Bat,
    Bee,
    Blaze,
    Boat,
    Cat,
    CaveSpider,
    ChestMinecart,
    Chicken,
    Cod,
    CommandBlockMinecart,
    Cow,
    Creeper,
    Dolphin,
    Donkey,
    DragonFireball,
    Drowned,
    Egg,
    ElderGuardian,
    EndCrystal,
    EnderDragon,
    EnderPearl,
    Enderman,
    Endermite,
    Evoker,
    EvokerFangs,
    ExperienceBottle,
    ExperienceOrb,
    EyeOfEnder,
    FallingBlock,
    Fireball,
    FireworkRocket,
    Fox,
    FurnaceMinecart,
    Ghast,
    Giant,
    GlowItemFrame,
    GlowSquid,
    Goat,
    Guardian,
    Hoglin,
    HopperMinecart,
    Horse,
    Husk,
    Illusioner,
    IronGolem,
    Item,
    ItemFrame,
    LeashKnot,
    LightningBolt,
    Llama,
    LlamaSpit,
    MagmaCube,
    Marker,
    Minecart,
    Mooshroom,
    Mule,
    Ocelot,
    Painting,
    Panda,
    Parrot,
    Phantom,
    Pig,
    Piglin,
    PiglinBrute,
    Pillager,
    PolarBear,
    Potion,
    Pufferfish,
    Rabbit,
    Ravager,
    Salmon,
    Sheep,
    Shulker,
    ShulkerBullet,
    Silverfish,
    Skeleton,
    SkeletonHorse,
    Slime,
    SmallFireball,
    SnowGolem,
    Snowball,
    SpawnerMinecart,
    SpectralArrow,
    Spider,
    Squid,
    Stray,
    Strider,
    Tnt,
    TntMinecart,
    TraderLlama,
    Trident,
    TropicalFish,
    Turtle,
    Vex,
    Villager,
    Vindicator,
    WanderingTrader,
    Witch,
    Wither,
    WitherSkeleton,
    WitherSkull,
    Wolf,
    Zoglin,
    Zombie,
    ZombieHorse,
    ZombieVillager,
    ZombifiedPiglin,
}

impl EntityType {
    pub fn name(&self) -> &'static str {
        match self {
            EntityType::AreaEffectCloud => "area_effect_cloud",
            EntityType::ArmorStand => "armor_stand",
            EntityType::Arrow => "arrow",
            EntityType::Axolotl => "axolotl",
            EntityType::Bat => "bat",
            EntityType::Bee => "bee",
            EntityType::Blaze => "blaze",
            EntityType::Boat => "boat",
            EntityType::Cat => "cat",
            EntityType::CaveSpider => "cave_spider",
            EntityType::Chicken => "chicken",
            EntityType::Cod => "cod",
            EntityType::Cow => "cow",
            EntityType::Creeper => "creeper",
            EntityType::Dolphin => "dolphin",
            EntityType::Donkey => "donkey",
            EntityType::DragonFireball => "dragon_fireball",
            EntityType::Drowned => "drowned",
            EntityType::ElderGuardian => "elder_guardian",
            EntityType::EndCrystal => "end_crystal",
            EntityType::EnderDragon => "ender_dragon",
            EntityType::Enderman => "enderman",
            EntityType::Endermite => "endermite",
            EntityType::Evoker => "evoker",
            EntityType::EvokerFangs => "evoker_fangs",
            EntityType::ExperienceOrb => "experience_orb",
            EntityType::EyeOfEnder => "eye_of_ender",
            EntityType::FallingBlock => "falling_block",
            EntityType::FireworkRocket => "firework_rocket",
            EntityType::Fox => "fox",
            EntityType::Ghast => "ghast",
            EntityType::Giant => "giant",
            EntityType::GlowItemFrame => "glow_item_frame",
            EntityType::GlowSquid => "glow_squid",
            EntityType::Goat => "goat",
            EntityType::Guardian => "guardian",
            EntityType::Hoglin => "hoglin",
            EntityType::Horse => "horse",
            EntityType::Husk => "husk",
            EntityType::Illusioner => "illusioner",
            EntityType::IronGolem => "iron_golem",
            EntityType::Item => "item",
            EntityType::ItemFrame => "item_frame",
            EntityType::Fireball => "fireball",
            EntityType::LeashKnot => "leash_knot",
            EntityType::LightningBolt => "lightning_bolt",
            EntityType::Llama => "llama",
            EntityType::LlamaSpit => "llama_spit",
            EntityType::MagmaCube => "magma_cube",
            EntityType::Minecart => "minecart",
            EntityType::ChestMinecart => "chest_minecart",
            EntityType::CommandBlockMinecart => "command_block_minecart",
            EntityType::FurnaceMinecart => "furnace_minecart",
            EntityType::HopperMinecart => "hopper_minecart",
            EntityType::SpawnerMinecart => "spawner_minecart",
            EntityType::TntMinecart => "tnt_minecart",
            EntityType::Marker => "marker",
            EntityType::Mule => "mule",
            EntityType::Mooshroom => "mooshroom",
            EntityType::Ocelot => "ocelot",
            EntityType::Painting => "painting",
            EntityType::Panda => "panda",
            EntityType::Parrot => "parrot",
            EntityType::Phantom => "phantom",
            EntityType::Pig => "pig",
            EntityType::Piglin => "piglin",
            EntityType::PiglinBrute => "piglin_brute",
            EntityType::Pillager => "pillager",
            EntityType::PolarBear => "polar_bear",
            EntityType::Tnt => "tnt",
            EntityType::Pufferfish => "pufferfish",
            EntityType::Rabbit => "rabbit",
            EntityType::Ravager => "ravager",
            EntityType::Salmon => "salmon",
            EntityType::Sheep => "sheep",
            EntityType::Shulker => "shulker",
            EntityType::ShulkerBullet => "shulker_bullet",
            EntityType::Silverfish => "silverfish",
            EntityType::Skeleton => "skeleton",
            EntityType::SkeletonHorse => "skeleton_horse",
            EntityType::Slime => "slime",
            EntityType::SmallFireball => "small_fireball",
            EntityType::SnowGolem => "snow_golem",
            EntityType::Snowball => "snowball",
            EntityType::SpectralArrow => "spectral_arrow",
            EntityType::Spider => "spider",
            EntityType::Squid => "squid",
            EntityType::Stray => "stray",
            EntityType::Strider => "strider",
            EntityType::Egg => "egg",
            EntityType::EnderPearl => "ender_pearl",
            EntityType::ExperienceBottle => "experience_bottle",
            EntityType::Potion => "potion",
            EntityType::Trident => "trident",
            EntityType::TraderLlama => "trader_llama",
            EntityType::TropicalFish => "tropical_fish",
            EntityType::Turtle => "turtle",
            EntityType::Vex => "vex",
            EntityType::Villager => "villager",
            EntityType::Vindicator => "vindicator",
            EntityType::WanderingTrader => "wandering_trader",
            EntityType::Witch => "witch",
            EntityType::Wither => "wither",
            EntityType::WitherSkeleton => "wither_skeleton",
            EntityType::WitherSkull => "wither_skull",
            EntityType::Wolf => "wolf",
            EntityType::Zoglin => "zoglin",
            EntityType::Zombie => "zombie",
            EntityType::ZombieHorse => "zombie_horse",
            EntityType::ZombieVillager => "zombie_villager",
            EntityType::ZombifiedPiglin => "zombified_piglin",
            EntityType::Player => "player",
        }
    }
}

mod entity_selector_serde {
    use std::fmt;
    use std::fmt::Display;
    use std::ops::{AddAssign, MulAssign, Neg};

    use serde::de::{DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};
    use serde::{de, ser, Deserialize, Serialize};

    pub type Result<T> = std::result::Result<T, SelectorError>;

    #[derive(Clone, Debug, PartialEq)]
    pub struct SelectorError(pub String);

    impl ser::Error for SelectorError {
        fn custom<T: Display>(msg: T) -> Self {
            Self(msg.to_string())
        }
    }

    impl de::Error for SelectorError {
        fn custom<T: Display>(msg: T) -> Self {
            Self(msg.to_string())
        }
    }

    impl Display for SelectorError {
        fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str(&self.0)
        }
    }

    impl std::error::Error for SelectorError {}

    pub struct EntitySelectorSerializer {
        output: String,
    }

    #[allow(dead_code)]
    pub fn to_string<T>(value: &T) -> Result<String>
    where
        T: Serialize,
    {
        let mut serializer = EntitySelectorSerializer {
            output: String::new(),
        };
        value.serialize(&mut serializer)?;
        Ok(serializer.output)
    }

    impl<'a> ser::Serializer for &'a mut EntitySelectorSerializer {
        type Ok = ();

        type Error = SelectorError;

        type SerializeSeq = Self;
        type SerializeTuple = Self;
        type SerializeTupleStruct = Self;
        type SerializeTupleVariant = Self;
        type SerializeMap = Self;
        type SerializeStruct = Self;
        type SerializeStructVariant = Self;

        fn serialize_bool(self, v: bool) -> Result<()> {
            self.serialize_str(match v {
                true => "true",
                false => "false",
            })
        }

        fn serialize_i8(self, v: i8) -> Result<()> {
            self.serialize_i64(i64::from(v))
        }

        fn serialize_i16(self, v: i16) -> Result<()> {
            self.serialize_i64(i64::from(v))
        }

        fn serialize_i32(self, v: i32) -> Result<()> {
            self.serialize_i64(i64::from(v))
        }

        fn serialize_i64(self, v: i64) -> Result<()> {
            self.output += &v.to_string();
            Ok(())
        }

        fn serialize_u8(self, v: u8) -> Result<()> {
            self.serialize_u64(u64::from(v))
        }

        fn serialize_u16(self, v: u16) -> Result<()> {
            self.serialize_u64(u64::from(v))
        }

        fn serialize_u32(self, v: u32) -> Result<()> {
            self.serialize_u64(u64::from(v))
        }

        fn serialize_u64(self, v: u64) -> Result<()> {
            self.output += &v.to_string();
            Ok(())
        }

        fn serialize_f32(self, v: f32) -> Result<()> {
            self.serialize_f64(f64::from(v))
        }

        fn serialize_f64(self, v: f64) -> Result<()> {
            self.output += &v.to_string();
            Ok(())
        }

        fn serialize_char(self, v: char) -> Result<()> {
            self.serialize_str(&v.to_string())
        }

        fn serialize_str(self, v: &str) -> Result<()> {
            let quote = v.contains(&[' ', '"', '\\'][..]);
            if quote {
                self.output += "\"";
            }
            self.output += &v.replace('"', "\\\"");
            if quote {
                self.output += "\"";
            }
            Ok(())
        }

        fn serialize_bytes(self, _: &[u8]) -> Result<()> {
            unimplemented!()
        }

        fn serialize_none(self) -> Result<()> {
            self.serialize_unit()
        }

        fn serialize_some<T>(self, _value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            unimplemented!()
        }

        fn serialize_unit(self) -> Result<()> {
            unimplemented!()
        }

        fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
            self.serialize_unit()
        }

        fn serialize_unit_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
        ) -> Result<()> {
            self.serialize_str(variant)
        }

        fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            value.serialize(self)
        }

        fn serialize_newtype_variant<T>(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            value: &T,
        ) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            variant.serialize(&mut *self)?;
            self.output += "=";
            value.serialize(&mut *self)?;
            Ok(())
        }

        fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
            self.output += "[";
            Ok(self)
        }

        fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
            self.serialize_seq(Some(len))
        }

        fn serialize_tuple_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeTupleStruct> {
            self.serialize_seq(Some(len))
        }

        fn serialize_tuple_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeTupleVariant> {
            self.output += "{";
            variant.serialize(&mut *self)?;
            self.output += "=[";
            Ok(self)
        }

        fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
            self.output += "{";
            Ok(self)
        }

        fn serialize_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeStruct> {
            self.serialize_map(Some(len))
        }

        fn serialize_struct_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeStructVariant> {
            self.output += "{";
            variant.serialize(&mut *self)?;
            self.output += "={";
            Ok(self)
        }
    }

    impl<'a> ser::SerializeSeq for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('[') {
                self.output += ",";
            }
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "]";
            Ok(())
        }
    }

    impl<'a> ser::SerializeTuple for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('[') {
                self.output += ",";
            }
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "]";
            Ok(())
        }
    }

    impl<'a> ser::SerializeTupleStruct for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('[') {
                self.output += ",";
            }
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "]";
            Ok(())
        }
    }

    impl<'a> ser::SerializeTupleVariant for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('[') {
                self.output += ",";
            }
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "]}";
            Ok(())
        }
    }

    impl<'a> ser::SerializeMap for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('{') {
                self.output += ",";
            }
            key.serialize(&mut **self)
        }

        fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            self.output += "=";
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "}";
            Ok(())
        }
    }

    impl<'a> ser::SerializeStruct for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('{') {
                self.output += ",";
            }
            key.serialize(&mut **self)?;
            self.output += "=";
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "}";
            Ok(())
        }
    }

    impl<'a> ser::SerializeStructVariant for &'a mut EntitySelectorSerializer {
        type Ok = ();
        type Error = SelectorError;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            if !self.output.ends_with('{') {
                self.output += ",";
            }
            key.serialize(&mut **self)?;
            self.output += "=";
            value.serialize(&mut **self)
        }

        fn end(self) -> Result<()> {
            self.output += "}}";
            Ok(())
        }
    }

    pub struct EntitySelectorDeserializer<'de> {
        pub input: &'de str,
    }

    impl<'de> EntitySelectorDeserializer<'de> {
        pub fn from_str(input: &'de str) -> Self {
            EntitySelectorDeserializer { input }
        }
    }

    pub fn from_str<'de, T>(s: &'de str) -> Result<(T, &'de str)>
    where
        T: Deserialize<'de>,
    {
        let mut deserializer = EntitySelectorDeserializer::from_str(s);
        let t = T::deserialize(&mut deserializer)?;
        Ok((t, deserializer.input))
    }

    impl<'de> EntitySelectorDeserializer<'de> {
        pub fn peek_char(&self) -> Result<char> {
            self.input
                .chars()
                .next()
                .ok_or_else(|| SelectorError("Unexpected EOF".to_string()))
        }

        pub fn next_char(&mut self) -> Result<char> {
            let ch = self.peek_char()?;
            self.input = &self.input[ch.len_utf8()..];
            Ok(ch)
        }

        fn parse_unsigned<T>(&mut self) -> Result<T>
        where
            T: AddAssign<T> + MulAssign<T> + From<u8>,
        {
            let mut int = match self.next_char()? {
                ch @ '0'..='9' => T::from(ch as u8 - b'0'),
                _ => {
                    return Err(SelectorError("Expected integer".to_string()));
                }
            };
            loop {
                match self.input.chars().next() {
                    Some(ch @ '0'..='9') => {
                        self.input = &self.input[1..];
                        int *= T::from(10);
                        int += T::from(ch as u8 - b'0');
                    }
                    _ => {
                        return Ok(int);
                    }
                }
            }
        }

        fn parse_signed<T>(&mut self) -> Result<T>
        where
            T: Neg<Output = T> + AddAssign<T> + MulAssign<T> + From<i8>,
        {
            let neg = self.peek_char() == Ok('-');
            if neg {
                self.next_char()?;
            }
            let mut int = match self.next_char()? {
                ch @ '0'..='9' => T::from((ch as u8 - b'0') as i8),
                _ => {
                    return Err(SelectorError("Expected integer".to_string()));
                }
            };
            loop {
                match self.input.chars().next() {
                    Some(ch @ '0'..='9') => {
                        self.input = &self.input[1..];
                        int *= T::from(10);
                        int += T::from((ch as u8 - b'0') as i8);
                    }
                    _ => {
                        return Ok(if neg { -int } else { int });
                    }
                }
            }
        }

        fn parse_string(&mut self) -> Result<&'de str> {
            if self.peek_char()? != '"' {
                if let Some(len) = self.input.find(&[',', ' ', ']', '}', '='][..]) {
                    let s = &self.input[..len];
                    self.input = &self.input[len..];
                    return Ok(s);
                }
            } else {
                self.next_char()?;
                let mut escaping = false;
                for (i, ch) in self.input.chars().enumerate() {
                    match ch {
                        '\\' => escaping = true,
                        '"' => {
                            if !escaping {
                                let res = Ok(&self.input[..i]);
                                self.input = &self.input[i + 1..];
                                return res;
                            }
                        }
                        _ => continue,
                    }
                }
            }
            Err(SelectorError("Unexpected EOF".to_string()))
        }
    }

    impl<'de, 'a> de::Deserializer<'de> for &'a mut EntitySelectorDeserializer<'de> {
        type Error = SelectorError;

        fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            match self.peek_char()? {
                'n' => self.deserialize_unit(visitor),
                '0'..='9' => self.deserialize_u64(visitor),
                '-' => self.deserialize_i64(visitor),
                '[' => self.deserialize_seq(visitor),
                '{' => self.deserialize_map(visitor),
                't' | 'f' => self.deserialize_bool(visitor),
                _ => self.deserialize_str(visitor),
            }
        }

        fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            match self.parse_string()? {
                "true" | "\"true\"" => visitor.visit_bool(true),
                "false" | "\"false\"" => visitor.visit_bool(false),
                something_else => visitor.visit_str(something_else),
            }
        }

        fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_i8(self.parse_signed()?)
        }

        fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_i16(self.parse_signed()?)
        }

        fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_i32(self.parse_signed()?)
        }

        fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_i64(self.parse_signed()?)
        }

        fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_u8(self.parse_unsigned()?)
        }

        fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_u16(self.parse_unsigned()?)
        }

        fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_u32(self.parse_unsigned()?)
        }

        fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_u64(self.parse_unsigned()?)
        }

        fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            for (i, ch) in self.input.chars().enumerate() {
                match ch {
                    '-' if i == 0 => continue,
                    '0'..='9' | '.' => continue,
                    _ => {
                        let res = visitor.visit_f32(
                            self.input[..i]
                                .parse()
                                .map_err(|_| SelectorError("Invalid number".to_string()))?,
                        );
                        self.input = &self.input[i..];
                        return res;
                    }
                }
            }
            Err(SelectorError("Unexpected EOF".to_string()))
        }

        fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            for (i, ch) in self.input.chars().enumerate() {
                match ch {
                    '-' if i == 0 => continue,
                    '0'..='9' | '.' => continue,
                    _ => {
                        let res = visitor.visit_f64(
                            self.input[..i]
                                .parse()
                                .map_err(|_| SelectorError("Invalid number".to_string()))?,
                        );
                        self.input = &self.input[i..];
                        return res;
                    }
                }
            }
            Err(SelectorError("Unexpected EOF".to_string()))
        }

        fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            unimplemented!()
        }

        fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_borrowed_str(self.parse_string()?)
        }

        fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_str(visitor)
        }

        fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            unimplemented!()
        }

        fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            unimplemented!()
        }

        fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            unimplemented!()
        }

        fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_unit()
        }

        fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_unit(visitor)
        }

        fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_newtype_struct(self)
        }

        fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.next_char()? == '[' {
                let value = visitor.visit_seq(CommaSeparated::new(self))?;
                if self.next_char()? == ']' {
                    Ok(value)
                } else {
                    Err(SelectorError("Expected array end".to_string()))
                }
            } else {
                Err(SelectorError("Expected array".to_string()))
            }
        }

        fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_seq(visitor)
        }

        fn deserialize_tuple_struct<V>(
            self,
            _name: &'static str,
            _len: usize,
            visitor: V,
        ) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_seq(visitor)
        }

        fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.next_char()? == '{' {
                let value = visitor.visit_map(CommaSeparated::new(self))?;
                if self.next_char()? == '}' {
                    Ok(value)
                } else {
                    Err(SelectorError("Expected map end".to_string()))
                }
            } else {
                Err(SelectorError("Expected map".to_string()))
            }
        }

        fn deserialize_struct<V>(
            self,
            _name: &'static str,
            _fields: &'static [&'static str],
            visitor: V,
        ) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_map(visitor)
        }

        fn deserialize_enum<V>(
            self,
            _name: &'static str,
            _variants: &'static [&'static str],
            visitor: V,
        ) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            visitor.visit_enum(Enum::new(self))
        }

        fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_str(visitor)
        }

        fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.deserialize_any(visitor)
        }
    }

    struct CommaSeparated<'a, 'de: 'a> {
        de: &'a mut EntitySelectorDeserializer<'de>,
        first: bool,
    }

    impl<'a, 'de> CommaSeparated<'a, 'de> {
        fn new(de: &'a mut EntitySelectorDeserializer<'de>) -> Self {
            CommaSeparated { de, first: true }
        }
    }

    impl<'de, 'a> SeqAccess<'de> for CommaSeparated<'a, 'de> {
        type Error = SelectorError;

        fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
        where
            T: DeserializeSeed<'de>,
        {
            if self.de.peek_char()? == ']' {
                return Ok(None);
            }
            if !self.first && self.de.next_char()? != ',' {
                return Err(SelectorError("Expected comma".to_string()));
            }
            self.first = false;
            seed.deserialize(&mut *self.de).map(Some)
        }
    }

    impl<'de, 'a> MapAccess<'de> for CommaSeparated<'a, 'de> {
        type Error = SelectorError;

        fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
        where
            K: DeserializeSeed<'de>,
        {
            if self.de.peek_char()? == '}' {
                return Ok(None);
            }
            if !self.first && self.de.next_char()? != ',' {
                return Err(SelectorError("Expected comma".to_string()));
            }
            self.first = false;
            seed.deserialize(&mut *self.de).map(Some)
        }

        fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
        where
            V: DeserializeSeed<'de>,
        {
            if self.de.next_char()? != '=' {
                return Err(SelectorError("Expected =".to_string()));
            }
            seed.deserialize(&mut *self.de)
        }
    }

    struct Enum<'a, 'de: 'a> {
        de: &'a mut EntitySelectorDeserializer<'de>,
    }

    impl<'a, 'de> Enum<'a, 'de> {
        fn new(de: &'a mut EntitySelectorDeserializer<'de>) -> Self {
            Enum { de }
        }
    }

    impl<'de, 'a> EnumAccess<'de> for Enum<'a, 'de> {
        type Error = SelectorError;
        type Variant = Self;

        fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
        where
            V: DeserializeSeed<'de>,
        {
            let val = seed.deserialize(&mut *self.de)?;
            Ok((val, self))
        }
    }

    impl<'de, 'a> VariantAccess<'de> for Enum<'a, 'de> {
        type Error = SelectorError;

        fn unit_variant(self) -> Result<()> {
            Ok(())
        }

        fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
        where
            T: DeserializeSeed<'de>,
        {
            if self.de.next_char()? != '=' {
                Err(SelectorError("Expected =".to_string()))
            } else {
                seed.deserialize(self.de)
            }
        }

        fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.de.next_char()? != '=' {
                Err(SelectorError("Expected =".to_string()))
            } else {
                de::Deserializer::deserialize_seq(self.de, visitor)
            }
        }

        fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.de.next_char()? != '=' {
                Err(SelectorError("Expected =".to_string()))
            } else {
                de::Deserializer::deserialize_map(self.de, visitor)
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use std::collections::HashMap;
        use std::iter::FromIterator;

        use crate::arguments::{
            AdvancementPredicate, BoolPredicate, EntitySelectorPredicate, EntitySelectorSorting,
            EntityType,
        };

        use super::*;

        #[test]
        fn test_serialize_struct() {
            #[derive(Serialize)]
            struct Test {
                x: f64,
                dx: f64,
                sort: EntitySelectorSorting,
            }

            let test = Test {
                x: 5.0,
                dx: 2.5,
                sort: EntitySelectorSorting::Nearest,
            };
            let expected = r#"{x=5,dx=2.5,sort=nearest}"#;
            assert_eq!(to_string(&test).unwrap(), expected);
        }

        #[test]
        fn test_serialize_enum() {
            #[derive(Serialize)]
            enum E {
                Newtype(u32),
                Tuple(u32, u32),
                Struct { a: u32 },
            }

            let n = E::Newtype(1);
            let expected = r#"Newtype=1"#;
            assert_eq!(to_string(&n).unwrap(), expected);

            let t = E::Tuple(1, 2);
            let expected = r#"{Tuple=[1,2]}"#;
            assert_eq!(to_string(&t).unwrap(), expected);

            let s = E::Struct { a: 1 };
            let expected = r#"{Struct={a=1}}"#;
            assert_eq!(to_string(&s).unwrap(), expected);

            let args = vec![
                EntitySelectorPredicate::Dx(1.5),
                EntitySelectorPredicate::Distance((1.5..=5.9).into()),
                EntitySelectorPredicate::Predicate(BoolPredicate(
                    false,
                    "my_predicate".to_string(),
                )),
                EntitySelectorPredicate::Level((0..=6).into()),
                EntitySelectorPredicate::Type(EntityType::Player),
                EntitySelectorPredicate::Advancements(HashMap::from_iter(vec![
                    ("test/1".to_string(), AdvancementPredicate::Value(true)),
                    (
                        "test/2".to_string(),
                        AdvancementPredicate::Criteria(HashMap::from_iter(vec![(
                            "criteria".to_string(),
                            true,
                        )])),
                    ),
                ])),
            ];

            let expected = r#"[dx=1.5,distance=1.5..5.9,predicate=!my_predicate,level=0..6,type=player,advancements={test/1=true,test/2={criteria=true}}]"#;
            let expected2 = r#"[dx=1.5,distance=1.5..5.9,predicate=!my_predicate,level=0..6,type=player,advancements={test/2={criteria=true},test/1=true}]"#;
            assert!(
                to_string(&args).unwrap() == expected || to_string(&args).unwrap() == expected2
            );
        }

        #[test]
        fn test_deserialize_struct() {
            #[derive(Deserialize, PartialEq, Debug)]
            struct Test {
                x: f64,
                dx: f64,
                sort: EntitySelectorSorting,
            }

            let j = r#"{x=5,dx=2.5,sort=nearest}"#;
            let expected = Test {
                x: 5.0,
                dx: 2.5,
                sort: EntitySelectorSorting::Nearest,
            };

            assert_eq!(expected, from_str(j).unwrap().0);
        }

        #[test]
        fn test_deserialize_enum() {
            #[derive(Deserialize, PartialEq, Debug)]
            enum E {
                Newtype(u32),
                Tuple(u32, u32),
                Struct { a: u32 },
            }

            let j = r#"Newtype=1"#;
            let expected = E::Newtype(1);
            assert_eq!(expected, from_str(j).unwrap().0);

            let j = r#"Tuple=[1,2]"#;
            let expected = E::Tuple(1, 2);
            assert_eq!(expected, from_str(j).unwrap().0);

            let j = r#""Struct"={a=1}"#;
            let expected = E::Struct { a: 1 };
            assert_eq!(expected, from_str(j).unwrap().0);

            let j = r#"[dx=1.5,distance=1.5..5.9,predicate=!my_predicate,level=0..6,advancements={test/1=true,test/2={criteria=true}}]"#;
            let expected: Vec<EntitySelectorPredicate> = vec![
                EntitySelectorPredicate::Dx(1.5),
                EntitySelectorPredicate::Distance((1.5..=5.9).into()),
                EntitySelectorPredicate::Predicate(BoolPredicate(
                    false,
                    "my_predicate".to_string(),
                )),
                EntitySelectorPredicate::Level((0..=6).into()),
                EntitySelectorPredicate::Advancements(HashMap::from_iter(vec![
                    ("test/1".to_string(), AdvancementPredicate::Value(true)),
                    (
                        "test/2".to_string(),
                        AdvancementPredicate::Criteria(HashMap::from_iter(vec![(
                            "criteria".to_string(),
                            true,
                        )])),
                    ),
                ])),
            ];

            assert_eq!(
                expected,
                from_str::<Vec<EntitySelectorPredicate>>(j).unwrap().0
            );
        }
    }
}

#[derive(Debug, Clone)]
pub struct ItemPredicateArgument;

impl ArgumentParser for ItemPredicateArgument {
    type Output = ItemPredicate;

    fn parse(&self, mut input: &str) -> Option<(usize, Self::Output)> {
        let is_tag = input.starts_with('#');
        if is_tag {
            input = &input[1..];
        }
        let space = input.find(' ').unwrap_or(input.len());
        let item;
        let mut tag = None;
        let mut tag_len = 0;
        if let Some(brace) = input.find('{') {
            if brace < space {
                item = &input[..brace];
                let (t, len) = quartz_nbt::snbt::parse_and_size(&input[brace..]).ok()?;
                tag_len = len;
                tag = Some(t);
            } else {
                item = &input[..space]
            }
        } else {
            item = &input[..space]
        }
        Some((
            is_tag as usize + item.len() + tag_len,
            ItemPredicate {
                predicate_type: match is_tag {
                    true => ItemPredicateType::Tag(item.parse().ok()?),
                    false => ItemPredicateType::Item(item.parse().ok()?),
                },
                tag: Tag(tag.unwrap_or_default()),
            },
        ))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:item_predicate"
    }
}

#[derive(Debug)]
pub struct ItemPredicate {
    pub predicate_type: ItemPredicateType,
    pub tag: Tag,
}

#[derive(Debug)]
pub enum ItemPredicateType {
    /// item tag like #minecraft:boats
    Tag(ResourceLocation),
    /// item id like minecraft:stone
    Item(ResourceLocation),
}

#[derive(Debug, Clone)]
pub struct ResourceLocation(String, String);

impl ResourceLocation {
    pub fn new(namespace: String, value: String) -> ResourceLocation {
        ResourceLocation(namespace, value)
    }

    pub fn namespace(&self) -> &str {
        &self.0
    }

    pub fn value(&self) -> &str {
        &self.1
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.0.len() + 1 + self.1.len()
    }
}

impl FromStr for ResourceLocation {
    type Err = anyhow::Error;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        macro_rules! only_these_chars {
            ($var: expr, $($p: pat)|+) => {
                {
                    for c in $var.chars() {
                        match c {
                            $($p)|+ => continue,
                            _ => bail!("Found illegal characters")
                        }
                    }
                    $var
                }
            }
        }

        if value.is_empty() {
            bail!("The string cannot be empty")
        }

        let s = value.split(':').collect::<Vec<_>>();
        match s.len() {
            1 => Ok(ResourceLocation::new(
                "minecraft".to_string(),
                only_these_chars!(s[0].to_string(), '0'..='9' | 'a'..='z' | '_' | '-' | '.' | '/'),
            )),
            2 => Ok(ResourceLocation::new(
                only_these_chars!(s[0].to_string(), '0'..='9' | 'a'..='z' | '_' | '-'),
                only_these_chars!(s[1].to_string(), '0'..='9' | 'a'..='z' | '_' | '-' | '.' | '/'),
            )),
            _ => bail!("Resource location must not contain additional `:` characters"),
        }
    }
}

impl Display for ResourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.0, self.1))
    }
}

#[derive(Debug, Clone)]
pub struct MessageArgument;

impl ArgumentParser for MessageArgument {
    type Output = Message;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        macro_rules! find {
            ($haystack: expr, $( $pattern: literal )|+) => {
                {
                    let haystack = $haystack;
                    None
                    $(
                        .or_else(|| haystack.find($pattern))
                    )+
                }
            };
        }

        let mut strings = Vec::new();
        let mut selectors = Vec::new();
        let mut i = 0;
        loop {
            if let Some(n) = find!(&input[i..], "@a" | "@p" | "@r" | "@s" | "@e") {
                strings.push(input[i..i + n].to_owned());
                i += n;
                let (len, selector) = EntityArgument::ENTITIES.parse(&input[i..], true)?;
                i += len;
                match selector {
                    EntitySelector::Selector(s) => selectors.push(s),
                    _ => unreachable!(),
                }
            } else {
                strings.push(input[i..].to_owned());
                break;
            }
        }
        Some((input.len(), Message(strings, selectors)))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:message"
    }
}

#[derive(Debug)]
pub struct Message(Vec<String>, Vec<Vec<EntitySelectorPredicate>>);

impl Message {
    pub fn to_string(
        &self,
        to_string: impl Fn(Vec<EntitySelectorPredicate>) -> Vec<String>,
    ) -> String {
        let mut s = String::new();
        let mut i = 0;
        loop {
            if self.0.len() > i {
                s += &self.0[i];
            }
            if self.1.len() > i {
                // TODO make comma gray
                s += &to_string(self.1[i].clone()).join(", ");
                i += 1;
            } else {
                break;
            }
        }
        s
    }
}

#[derive(Debug, Clone)]
pub struct SwizzleArgument;

impl ArgumentParser for SwizzleArgument {
    type Output = Swizzle;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        let mut x = false;
        let mut y = false;
        let mut z = false;
        for char in input.chars() {
            match char {
                'x' => {
                    if !x {
                        x = true;
                    } else {
                        return None;
                    }
                }
                'y' => {
                    if !y {
                        y = true;
                    } else {
                        return None;
                    }
                }
                'z' => {
                    if !z {
                        z = true;
                    } else {
                        return None;
                    }
                }
                ' ' => break,
                _ => return None,
            }
        }
        Some((x as usize + y as usize + z as usize, Swizzle { x, y, z }))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:swizzle"
    }
}

#[derive(Debug, Clone)]
pub struct Swizzle {
    pub x: bool,
    pub y: bool,
    pub z: bool,
}

#[derive(Debug, Clone)]
pub struct EntityAnchorArgument;

impl ArgumentParser for EntityAnchorArgument {
    type Output = EntityAnchor;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        match input.split(' ').next().unwrap() {
            "feet" => Some(("feet".len(), EntityAnchor::Feet)),
            "eyes" => Some(("eyes".len(), EntityAnchor::Eyes)),
            _ => None,
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:entity_anchor"
    }
}

#[derive(Debug, Clone)]
pub enum EntityAnchor {
    Feet,
    Eyes,
}

impl Default for EntityAnchor {
    fn default() -> Self {
        EntityAnchor::Feet
    }
}

#[derive(Debug, Clone)]
pub struct TimeArgument;

impl ArgumentParser for TimeArgument {
    type Output = (TimeUnit, f32);

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        let mut s = String::new();
        let mut suffix = None;
        for char in input.chars() {
            match char {
                c @ ('0'..='9' | '.' | '-') => s.push(c),
                c @ ('d' | 's' | 't') => {
                    if suffix.is_some() {
                        return None;
                    } else {
                        suffix = Some(c.try_into().ok()?)
                    }
                }
                ' ' => break,
                _ => return None,
            }
        }
        Some((
            s.len() + suffix.is_some() as usize,
            (suffix.unwrap_or_default(), s.parse().ok()?),
        ))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:time"
    }
}

#[derive(Debug, Clone)]
pub enum TimeUnit {
    Days,
    Seconds,
    Ticks,
}

impl TryFrom<char> for TimeUnit {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'd' => Ok(TimeUnit::Days),
            's' => Ok(TimeUnit::Seconds),
            't' => Ok(TimeUnit::Ticks),
            c => Err(format!(
                "Invalid time unit `{}`. Expected: `d`, `s` or `t`",
                c
            )),
        }
    }
}

impl Default for TimeUnit {
    fn default() -> Self {
        TimeUnit::Ticks
    }
}

#[derive(Debug, Clone)]
pub struct BlockPosArgument;

impl ArgumentParser for BlockPosArgument {
    type Output = BlockPos;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        let mut now = 0;
        let mut relative_now = false;
        let mut local = false;
        let mut s = String::new();
        let mut pos = MaybeUninit::uninit();
        let mut i = 0;
        for char in input.chars() {
            match char {
                '^' => {
                    if now == 0 && s.is_empty() {
                        local = true;
                    } else if !local || !s.is_empty() {
                        return None;
                    }
                }
                '~' => {
                    if !s.is_empty() {
                        return None;
                    }
                    relative_now = true;
                    if local {
                        return None;
                    }
                }
                c @ ('0'..='9' | '-') => {
                    if s.is_empty() && local {
                        return None;
                    }
                    s.push(c);
                }
                '.' => {
                    if !local {
                        return None;
                    }
                    s.push('.');
                }
                ' ' => {
                    if now == 0 {
                        // X
                        if local {
                            pos.write(BlockPos::Local(
                                if s.is_empty() { 0.0 } else { s.parse().ok()? },
                                0.0,
                                0.0,
                            ));
                        } else {
                            pos.write(BlockPos::Relative {
                                x: (relative_now, if s.is_empty() { 0 } else { s.parse().ok()? }),
                                y: (false, 0),
                                z: (false, 0),
                            });
                        }
                    } else if now == 1 {
                        // Y
                        // SAFETY: `now` can is set to 1 only after passing previous block, where `pos` is initialized
                        match unsafe { pos.assume_init_mut() } {
                            BlockPos::Local(_, y, _) => {
                                *y = if s.is_empty() { 0.0 } else { s.parse().ok()? }
                            }
                            BlockPos::Relative { y, .. } => {
                                *y = (relative_now, if s.is_empty() { 0 } else { s.parse().ok()? })
                            }
                        }
                    } else {
                        break;
                    }
                    now += 1;
                    s = String::new();
                    relative_now = false;
                }
                _ => return None,
            }
            i += 1;
        }
        if now != 2 {
            return None;
        }
        // Z
        // SAFETY: `pos` is initialized when increasing `now` from 0 to 1
        unsafe {
            match pos.assume_init_mut() {
                BlockPos::Local(_, _, z) => *z = if s.is_empty() { 0.0 } else { s.parse().ok()? },
                BlockPos::Relative { z, .. } => {
                    *z = (relative_now, if s.is_empty() { 0 } else { s.parse().ok()? })
                }
            }
            Some((i, pos.assume_init()))
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:block_pos"
    }
}

#[derive(Debug, Clone)]
pub enum BlockPos {
    /// Relative position (~1, ~-1, 1). If not prefixed with ~, the first bool is set false
    Relative {
        x: (bool, i32),
        y: (bool, i32),
        z: (bool, i32),
    },
    Local(f64, f64, f64),
}

#[derive(Debug, Clone)]
pub struct BlockStateArgument;

impl ArgumentParser for BlockStateArgument {
    type Output = BlockState;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        let res_loc = input.split(&[' ', '[', '{'][..]).next().unwrap();
        let has_namespace = res_loc.contains(':');
        let block: ResourceLocation = res_loc.parse().ok()?;
        let block_len = if has_namespace {
            block.len()
        } else {
            block.len() - "minecraft:".len()
        };

        let (properties_len, properties) = if input.len() > block_len
            && input.chars().nth(block_len).unwrap() == '['
        {
            let props = &input[block_len..];
            let mut s = props;
            let mut properties = HashMap::new();
            loop {
                s = s[1..].trim_start_matches(' ');
                let (len, key) = StringArgument::find_quoted_phrase_or_read_until(s, &['=', ' '])?;

                s = s[len..].trim_start_matches(' ');
                if !s.starts_with('=') {
                    return None;
                }

                s = s[1..].trim_start_matches(' ');
                let (len, value) =
                    StringArgument::find_quoted_phrase_or_read_until(s, &[',', ' ', ']'])?;

                s = s[len..].trim_start_matches(' ');
                properties.insert(key, value);

                match s.chars().next() {
                    Some(',') => continue,
                    Some(']') => break,
                    _ => return None,
                }
            }
            (props.len() - s.len() + 1, properties)
        } else {
            (0, HashMap::default())
        };

        let (nbt, nbt_len) = if input.len() > block_len + properties_len
            && input.chars().nth(block_len + properties_len).unwrap() == '{'
        {
            quartz_nbt::snbt::parse_and_size(&input[block_len + properties_len..]).ok()?
        } else {
            (NbtCompound::new(), 0)
        };

        Some((
            block_len + properties_len + nbt_len,
            BlockState {
                block,
                properties,
                nbt,
            },
        ))
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:block_state"
    }
}

#[derive(Debug, Clone)]
pub struct BlockState {
    pub block: ResourceLocation,
    pub properties: HashMap<String, String>,
    pub nbt: NbtCompound,
}

#[derive(Debug, Clone)]
pub struct ResourceLocationArgument;

impl ArgumentParser for ResourceLocationArgument {
    type Output = ResourceLocation;

    fn parse(&self, input: &str) -> Option<(usize, Self::Output)> {
        let value = input.split(' ').next().unwrap();
        let has_namespace = value.contains(':');
        let value: ResourceLocation = value.parse().ok()?;
        if has_namespace {
            Some((value.len(), value))
        } else {
            Some((value.len() - "minecraft:".len(), value))
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &()
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:resource_location"
    }
}
