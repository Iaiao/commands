use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::marker::PhantomData;
use std::ops::{Bound, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeToInclusive};
use std::str::FromStr;

use anyhow::{anyhow, bail};
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use uuid::Uuid;

use crate::arguments::EntitySelectorPredicate::Limit;
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
        #[derive(PartialEq, Debug)]
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
            fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
                let mut i = 0;
                for char in input.chars() {
                    match char {
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
                    Some((i, Box::new(value)))
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

        #[derive(PartialEq, Debug)]
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
                        flags |= 1 << 0;
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
    pub fn new(properties: StringProperties) -> StringArgument {
        StringArgument(properties)
    }
}

impl ArgumentParser for StringArgument {
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        match self.0 {
            StringProperties::SingleWord => input
                .find(' ')
                .map(|i| (i, Box::new(input[..i].to_string()) as Box<dyn Any>)),
            StringProperties::QuotablePhrase => {
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
                        return Some((i + 1, Box::new(input[1..i].to_string())));
                    }
                    None
                } else {
                    // behave like SingleWord
                    input
                        .find(' ')
                        .map(|i| (i, Box::new(input[..i].to_string()) as Box<dyn Any>))
                }
            }
            StringProperties::GreedyPhrase => {
                let i = if input.ends_with(' ') {
                    input.len() - 1
                } else {
                    input.len()
                };
                Some((i, Box::new(input[..i].to_string())))
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
    fn parse(&self, input: &str) -> Option<(usize, Box<dyn Any>)> {
        let mut requirements = Vec::new();
        match input.split('[').next().unwrap() {
            "@p" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Nearest,
                ));
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            "@r" => {
                requirements.push(EntitySelectorPredicate::Sort(EntitySelectorSorting::Random));
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            "@a" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Arbitrary,
                ));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            "@e" => {
                requirements.push(EntitySelectorPredicate::Sort(
                    EntitySelectorSorting::Arbitrary,
                ));
            }
            "@s" => {
                requirements.push(EntitySelectorPredicate::Sender);
                requirements.push(EntitySelectorPredicate::Limit(1));
                requirements.push(EntitySelectorPredicate::Type(EntityType::Player));
            }
            something => {
                let something = something.split_whitespace().next()?;
                let selector = Uuid::from_str(something)
                    .map(|uuid| EntitySelector::Uuid(uuid))
                    .unwrap_or(EntitySelector::Name(something.to_owned()));
                return Some((something.len(), Box::new(selector)));
            }
        };
        let i = if input.contains('[') {
            let start = input.find('[').unwrap(); // always 2 for now
            let (predicates, left) =
                entity_selector_serde::from_str::<Vec<EntitySelectorPredicate>>(&input[start..])
                    .ok()?;
            requirements.extend(predicates);
            input.len() - left.len()
        } else {
            2
        };
        if self.0.single && !requirements.contains(&EntitySelectorPredicate::Limit(1)) {
            // TODO change return type to Result to report parsing errors
            None
        } else if self.0.only_players
            && !requirements.contains(&EntitySelectorPredicate::Type(EntityType::Player))
        {
            None
        } else {
            Some((i, Box::new(EntitySelector::Selector(requirements))))
        }
    }

    fn get_properties(&self) -> &dyn ParserProperties {
        &self.0
    }

    fn get_identifier(&self) -> &'static str {
        "minecraft:entity"
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
    Tag(BoolPredicate<String>),
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
    ZRotation(WrappedRange<f32>),
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
        input: &'de str,
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
        fn peek_char(&mut self) -> Result<char> {
            self.input
                .chars()
                .next()
                .ok_or_else(|| SelectorError("Unexpected EOF".to_string()))
        }

        fn next_char(&mut self) -> Result<char> {
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

        fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.next_char()? == '[' {
                let value = visitor.visit_seq(CommaSeparated::new(&mut self))?;
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

        fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.next_char()? == '{' {
                let value = visitor.visit_map(CommaSeparated::new(&mut self))?;
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
