//! A library for the handling and analysis of Japanese text, particularly
//! Kanji. It can be used to find the density of Kanji in given texts according
//! to their *Level* classification, as defined by the Japan Kanji Aptitude
//! Testing Foundation (日本漢字能力検定協会).
//!
//! The Kanji data presented here matches the Foundation's official 2020
//! February charts. Note that [some Kanji had their levels changed][changed]
//! (pdf) as of 2020.
//!
//! # Usage
//!
//! Two main useful types are [`Character`] and [`Kanji`].
//!
//! ### Reading Japanese Text
//!
//! To reinterpret every `char` of the input into a [`Character`] we can reason
//! about:
//!
//! ```
//! use std::fs;
//! use kanji::Character;
//!
//! let cs: Option<Vec<Character>> = fs::read_to_string("your-text.txt")
//!   .map(|content| content.chars().map(Character::new).collect())
//!   .ok();
//! ```
//!
//! But maybe we're just interested in the [`Kanji`]:
//!
//! ```
//! use std::fs;
//! use kanji::Kanji;
//!
//! let ks: Option<Vec<Kanji>> = fs::read_to_string("your-text.txt")
//!   .map(|content| content.chars().filter_map(Kanji::new).collect())
//!   .ok();
//! ```
//!
//! Alongside normal pattern matching, the [`Character::kanji`] method can also
//! help us extract [`Kanji`] values.
//!
//! ### Filtering
//!
//! In general, when we want to reduce a text to a single [`Character`] subtype,
//! we can `filter`:
//!
//! ```
//! let orig = "そこで犬が寝ている";
//!
//! let ks: String = orig.chars().filter(|c| kanji::is_kanji(*c)).collect();
//! assert_eq!("犬寝", ks);
//!
//! let hs: String = orig.chars().filter(|c| kanji::is_hiragana(*c)).collect();
//! assert_eq!("そこでがている", hs);
//! ```
//!
//! ### Level Analysis
//!
//! To find out how many Kanji of each exam level belong to some text:
//!
//! ```
//! let level_table = kanji::level_table();
//! let texts = vec![
//!     "非常に面白い文章",
//!     "誰でも読んだ事のある名作",
//!     "飛行機で空を飛ぶ",
//! ];
//!
//! for t in texts {
//!     let counts = kanji::kanji_counts(t, &level_table);
//!     println!("{:#?}", counts);
//! }
//! ```
//!
//! And if you want to know *what* the Kanji were from a particular level:
//!
//! ```
//! let level_table = kanji::level_table();
//! let text = "日常生活では、鮫に遭う事は基本的にない。";
//!
//! let ks: String = text
//!     .chars()
//!     // Filter out all chars that aren't Kanji.
//!     .filter_map(kanji::Kanji::new)
//!     // Preserve only those that appear in Level 10.
//!     .filter_map(|k| match level_table.get(&k) {
//!         Some(kanji::Level::Ten) => Some(k.get()),
//!         _ => None,
//!     })
//!     // Fold them all back into a String.
//!     .collect();
//!
//! assert_eq!("日生本", ks);
//! ```
//!
//! # Notes on Unicode
//!
//! All Japanese characters, Kanji or otherwise, are a single Unicode Scalar
//! Value, and thus can be safely represented by a single internal `char`.
//!
//! Further, the ordering of Kanji in the official Foundation lists is in no way
//! related to their ordering in Unicode, since in Unicode, Kanji are grouped by
//! radical. So:
//!
//! ```
//! use kanji::exam_lists;
//!
//! let same_as_uni = exam_lists::LEVEL_10.chars().max() < exam_lists::LEVEL_09.chars().min();
//! assert!(!same_as_uni);
//! ```
//!
//! # Features
//! - `serde`: Enable `serde` trait implementations.
//!
//! # Resources
//! - [CJK Unicode Chart](https://www.unicode.org/charts/PDF/U4E00.pdf) (pdf)
//! - [StackOverflow: Unicode Ranges for Japanese](https://stackoverflow.com/q/19899554/643684)
//! - [級別漢字表](https://www.kanken.or.jp/kanken/outline/data/outline_degree_national_list20200217.pdf) (pdf)
//!
//! [changed]: https://www.kanken.or.jp/kanken/topics/data/alterclassofkanji2020.pdf

#![doc(html_root_url = "https://docs.rs/kanji/1.1.0")]

use std::char;
use std::collections::HashMap;
use std::fmt;

#[cfg(feature = "serde")]
use serde::de::{Error, Unexpected, Visitor};
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// A complete list of all Kanji in every level of the exam.
pub mod exam_lists;

/// A single symbol of Kanji, also known as a [CJK Unified Ideograph][cjk].
///
/// Japanese Kanji were borrowed from China over several waves during the last
/// 1,500 years. Japan declares 2,136 of these as their standard set, with rarer
/// characters being the domain of place names, academia and writers.
///
/// Japanese has many Japan-only Kanji. Common ones include:
///
/// - 畑 (a type of rice field)
/// - 峠 (a narrow mountain pass)
/// - 働 (to do physical labour)
///
/// [cjk]: https://en.wikipedia.org/wiki/Han_unification
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Kanji(char);

impl Kanji {
    /// Attempt to form a `Kanji`. Will fail if the given `char` is out of the
    /// Unicode range that Japanese text inhabits.
    ///
    /// ```
    /// use kanji::Kanji;
    ///
    /// assert_eq!(Some('志'), Kanji::new('志').map(|k| k.get()));
    /// assert_eq!(None, Kanji::new('a'));
    /// ```
    pub fn new(c: char) -> Option<Kanji> {
        if is_kanji(c) {
            Some(Kanji(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for Kanji {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Kanji {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Kanji {
    fn deserialize<D>(deserializer: D) -> Result<Kanji, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(KanjiVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct KanjiVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for KanjiVisitor {
    type Value = Kanji;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<Kanji, E> {
        Kanji::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Kanji, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// A Hiragana character, from あ to ん.
///
/// These are learned first by Japanese school children and foreign learners,
/// and are used most often for grammatical word endings and prepositions. Some
/// women's first names are written purely in Hiragana, as the characters
/// themselves have a soft, flowing feel to them (very much unlike the blocky,
/// angular [`Katakana`]).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Hiragana(char);

impl Hiragana {
    /// Attempt to form a `Hiragana`. Will fail if the given `char` is out of
    /// the expected Unicode range.
    ///
    /// ```
    /// use kanji::Hiragana;
    ///
    /// assert_eq!(Some('あ'), Hiragana::new('あ').map(|k| k.get()));
    /// assert_eq!(None, Hiragana::new('鼠'));
    /// assert_eq!(None, Hiragana::new('a'));
    /// ```
    pub fn new(c: char) -> Option<Hiragana> {
        if is_hiragana(c) {
            Some(Hiragana(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for Hiragana {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Hiragana {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Hiragana {
    fn deserialize<D>(deserializer: D) -> Result<Hiragana, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(HiraganaVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct HiraganaVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for HiraganaVisitor {
    type Value = Hiragana;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<Hiragana, E> {
        Hiragana::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Hiragana, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// A Katakana character, from ア to ン.
///
/// These are typically learned after [`Hiragana`], and are used to represent
/// foreign names, sound effects, and occasionally words whose Kanji are
/// "difficult". Two such examples are ネズミ (鼠) and アリ (蟻).
///
/// It used to be common to use Katakana as Hiragana are used today, so the
/// phrase君と街を歩きたい would have been written 君ト街ヲ歩キタイ. Admittedly
/// strange to modern eyes!
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Katakana(char);

impl Katakana {
    /// Attempt to form a `Katakana`. Will fail if the given `char` is out of
    /// the expected Unicode range.
    ///
    /// ```
    /// use kanji::Katakana;
    ///
    /// assert_eq!(Some('ア'), Katakana::new('ア').map(|k| k.get()));
    /// assert_eq!(None, Katakana::new('匙'));
    /// assert_eq!(None, Katakana::new('a'));
    /// ```
    pub fn new(c: char) -> Option<Katakana> {
        if is_katakana(c) {
            Some(Katakana(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for Katakana {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Katakana {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Katakana {
    fn deserialize<D>(deserializer: D) -> Result<Katakana, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(KatakanaVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct KatakanaVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for KatakanaVisitor {
    type Value = Katakana;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<Katakana, E> {
        Katakana::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Katakana, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// Japanese symbols and punctuation.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Punctuation(char);

impl Punctuation {
    /// Attempt to form a `Punctuation`. Will fail if the given `char` is out of
    /// the expected Unicode range.
    ///
    /// ```
    /// use kanji::Punctuation;
    ///
    /// assert_eq!(Some('。'), Punctuation::new('。').map(|k| k.get()));
    /// assert_eq!(None, Punctuation::new('a'));
    /// ```
    pub fn new(c: char) -> Option<Punctuation> {
        if is_japanese_punct(c) {
            Some(Punctuation(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for Punctuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Punctuation {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Punctuation {
    fn deserialize<D>(deserializer: D) -> Result<Punctuation, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(PunctuationVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct PunctuationVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for PunctuationVisitor {
    type Value = Punctuation;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<Punctuation, E> {
        Punctuation::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Punctuation, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// Japanese full-width alphanumeric characters and a few punctuation symbols.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct AlphaNum(char);

impl AlphaNum {
    /// Attempt to form a `AlphaNum`. Will fail if the given `char` is out of
    /// the expected Unicode range.
    ///
    /// ```
    /// use kanji::AlphaNum;
    ///
    /// assert_eq!(Some('Ａ'), AlphaNum::new('Ａ').map(|k| k.get()));
    /// assert_eq!(Some('＊'), AlphaNum::new('＊').map(|k| k.get()));
    /// assert_eq!(None, AlphaNum::new('a'));
    /// ```
    pub fn new(c: char) -> Option<AlphaNum> {
        if is_alphanum(c) {
            Some(AlphaNum(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for AlphaNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for AlphaNum {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for AlphaNum {
    fn deserialize<D>(deserializer: D) -> Result<AlphaNum, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(AlphaNumVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct AlphaNumVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for AlphaNumVisitor {
    type Value = AlphaNum;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<AlphaNum, E> {
        AlphaNum::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<AlphaNum, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// A standard ASCII character.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ASCII(char);

impl ASCII {
    /// Attempt to form a `ASCII`. Will fail if the given `char` is out of
    /// the expected Unicode range.
    ///
    /// ```
    /// use kanji::ASCII;
    ///
    /// assert_eq!(Some('a'), ASCII::new('a').map(|k| k.get()));
    /// assert_eq!(None, ASCII::new('あ'));
    /// ```
    pub fn new(c: char) -> Option<ASCII> {
        if char::is_ascii(&c) {
            Some(ASCII(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }
}

impl fmt::Display for ASCII {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for ASCII {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.0)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for ASCII {
    fn deserialize<D>(deserializer: D) -> Result<ASCII, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(ASCIIVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct ASCIIVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for ASCIIVisitor {
    type Value = ASCII;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<ASCII, E> {
        ASCII::new(v).ok_or(Error::invalid_value(Unexpected::Char(v), &self))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<ASCII, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// General categories for characters, at least as is useful for thinking about
/// Japanese.
///
/// Japanese "full-width" numbers and letters are counted as `AlphaNum`, whereas
/// "normal" ASCII characters have the `ASCII` variant.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub enum Character {
    Kanji(Kanji),
    Hiragana(Hiragana),
    Katakana(Katakana),
    Punctuation(Punctuation),
    AlphaNum(AlphaNum),
    ASCII(ASCII),
    Other(char),
}

impl Character {
    /// Form a new `Character` from some `char`.
    pub fn new(c: char) -> Character {
        Kanji::new(c)
            .map(Character::Kanji)
            .or_else(|| Hiragana::new(c).map(Character::Hiragana))
            .or_else(|| Katakana::new(c).map(Character::Katakana))
            .or_else(|| Punctuation::new(c).map(Character::Punctuation))
            .or_else(|| AlphaNum::new(c).map(Character::AlphaNum))
            .or_else(|| ASCII::new(c).map(Character::ASCII))
            .unwrap_or_else(|| Character::Other(c))
    }

    /// A convenience method for attempting to extract a possible
    /// [`Kanji`].
    pub fn kanji(&self) -> Option<Kanji> {
        match self {
            Character::Kanji(k) => Some(*k),
            _ => None,
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        match self {
            Character::Kanji(c) => c.get(),
            Character::Hiragana(c) => c.get(),
            Character::Katakana(c) => c.get(),
            Character::Punctuation(c) => c.get(),
            Character::AlphaNum(c) => c.get(),
            Character::ASCII(c) => c.get(),
            Character::Other(c) => *c,
        }
    }
}

impl fmt::Display for Character {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Character {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_char(self.get())
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Character {
    fn deserialize<D>(deserializer: D) -> Result<Character, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(deserializer.deserialize_char(CharacterVisitor)?)
    }
}

#[cfg(feature = "serde")]
struct CharacterVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for CharacterVisitor {
    type Value = Character;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a character in the legal UTF8 range")
    }

    fn visit_char<E: Error>(self, v: char) -> Result<Character, E> {
        Ok(Character::new(v))
    }

    fn visit_str<E: Error>(self, v: &str) -> Result<Character, E> {
        let mut iter = v.chars();
        match (iter.next(), iter.next()) {
            (Some(c), None) => self.visit_char(c),
            _ => Err(Error::invalid_value(Unexpected::Str(v), &self)),
        }
    }
}

/// A level or "kyuu" (級) of Japanese Kanji ranking.
///
/// There are 12 of these,
/// from 10 to 1, including two "pre" levels between 3 and 2, and 2 and 1.
///
/// Japanese students will typically have Level-5 ability by the time they
/// finish elementary school. Level-5 accounts for 1,026 characters. By the end
/// of middle school, they would have covered up to Level-3 (1,623 Kanji) in
/// their Japanese class curriculum.
///
/// While Level-2 (2,136 Kanji) is considered "standard adult" ability, many
/// adults would not pass the Level-2, or even the Level-Pre2 exam without
/// considerable study. It is not only the reading and writing of the characters
/// themselves that is tested, but also their associated vocabularly, usage in
/// real text, and appearance in classic Chinese idioms (四字熟語).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub enum Level {
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    PreTwo,
    Two,
    PreOne,
    One,
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Level::Ten => write!(f, "十"),
            Level::Nine => write!(f, "九"),
            Level::Eight => write!(f, "八"),
            Level::Seven => write!(f, "七"),
            Level::Six => write!(f, "六"),
            Level::Five => write!(f, "五"),
            Level::Four => write!(f, "四"),
            Level::Three => write!(f, "三"),
            Level::PreTwo => write!(f, "準二"),
            Level::Two => write!(f, "二"),
            Level::PreOne => write!(f, "準一"),
            Level::One => write!(f, "一"),
        }
    }
}

/// Kanji appear in the Unicode range 4e00 to 9ffc.
/// The final Japanese Kanji is 9fef (鿯).
///
/// For a chart of the full official range, see [this pdf] from the Unicode
/// organization.
///
/// A number of Level Pre-One Kanji appear in the [CJK Compatibility
/// Ideographs][compat] list, so there is an extra check here for those.
///
/// ```
/// assert!(kanji::is_kanji('澄'));  // Obviously a legal Kanji.
/// assert!(!kanji::is_kanji('a'));  // Obviously not.
/// ```
///
/// [compat]: https://www.unicode.org/charts/PDF/UF900.pdf
/// [this pdf]: https://www.unicode.org/charts/PDF/U4E00.pdf
pub fn is_kanji(c: char) -> bool {
    (c >= '\u{4e00}' && c <= '\u{9ffc}') // Standard set.
        || (c >= '\u{f900}' && c <= '\u{faff}') // CJK Compatibility Ideographs.
        || (c >= '\u{3400}' && c <= '\u{4dbf}') // Extension A
        || (c >= '\u{20000}' && c <= '\u{2a6dd}') // Extension B
        || (c >= '\u{2a700}' && c <= '\u{2b734}') // Extension C
        || (c >= '\u{2b740}' && c <= '\u{2b81d}') // Extension D
        || (c >= '\u{2b820}' && c <= '\u{2cea1}') // Extension E
        || (c >= '\u{2ceb0}' && c <= '\u{2ebe0}') // Extension F
        || (c >= '\u{30000}' && c <= '\u{3134a}') // Extension G
}

/// Is a given `char` betwen あ and ゖ?
///
/// ```
/// assert!(kanji::is_hiragana('あ'));
/// assert!(!kanji::is_hiragana('ゟ'));
/// assert!(!kanji::is_hiragana('a'));
/// ```
pub fn is_hiragana(c: char) -> bool {
    c >= '\u{3041}' && c <= '\u{3096}'
}

/// Is a given `char` betwen あ and ゟ?
/// Strictly compliant with the [unicode definition of hiragana](https://www.unicode.org/charts/PDF/U3040.pdf),
/// including ponctuation, marks and a digraph
///
/// ```
/// assert!(kanji::is_hiragana_extended('あ'));
/// assert!(kanji::is_hiragana_extended('ゟ'));
/// assert!(!kanji::is_hiragana_extended('a'));
/// ```
pub fn is_hiragana_extended(c: char) -> bool {
    c >= '\u{3041}' && c <= '\u{309f}'
}

/// Is a given `char` between ァ and ヺ?
///
/// ```
/// assert!(kanji::is_katakana('ン'));
/// assert!(!kanji::is_katakana('ヿ'));
/// assert!(!kanji::is_katakana('a'));
/// ```
pub fn is_katakana(c: char) -> bool {
    c >= '\u{30a1}' && c <= '\u{30fa}' || c == '\u{30fc}'
}

/// Is a given `char` between ゠ and ヿ?
/// Strictly compliant with the [unicode definition of katakana](https://www.unicode.org/charts/PDF/U30A0.pdf),
/// including ponctuation, marks and a digraph
///
/// ```
/// assert!(kanji::is_katakana_extended('ン'));
/// assert!(kanji::is_katakana_extended('ヿ'));
/// assert!(!kanji::is_katakana_extended('a'));
/// ```
pub fn is_katakana_extended(c: char) -> bool {
    c >= '\u{30a0}' && c <= '\u{30ff}'
}

/// Does a given `char` belong to the set of Japanese symbols and punctuation?
pub fn is_japanese_punct(c: char) -> bool {
    c >= '\u{3000}' && c <= '\u{303f}'
}

/// Does a given `char` belong to the set of Japanese alphanumeric characters
/// and western punctuation?
pub fn is_alphanum(c: char) -> bool {
    c >= '\u{ff01}' && c <= '\u{ff5e}'
}

/// All possible Kanji characters, as well as non-character radicals, in a
/// heap-allocated UTF-8 `String`.
///
/// ```
/// let ks = kanji::all_kanji();
///
/// assert_eq!(Some('一'), ks.chars().next());
/// assert_eq!(Some('\u{9ffc}'), ks.chars().last());
/// ```
pub fn all_kanji() -> String {
    let mut s = String::with_capacity(62967); // Capacity in bytes.
    (0x4e00..=0x9ffc)
        .filter_map(char::from_u32)
        .for_each(|c| s.push(c));
    s
}

/// Using the data stored in the `LEVEL_*` constants, generate a lookup table
/// for Kanji levels.
pub fn level_table() -> HashMap<Kanji, Level> {
    let pairs = vec![
        (exam_lists::LEVEL_10, Level::Ten),
        (exam_lists::LEVEL_09, Level::Nine),
        (exam_lists::LEVEL_08, Level::Eight),
        (exam_lists::LEVEL_07, Level::Seven),
        (exam_lists::LEVEL_06, Level::Six),
        (exam_lists::LEVEL_05, Level::Five),
        (exam_lists::LEVEL_04, Level::Four),
        (exam_lists::LEVEL_03, Level::Three),
        (exam_lists::LEVEL_02_PRE, Level::PreTwo),
        (exam_lists::LEVEL_02, Level::Two),
        (exam_lists::LEVEL_01, Level::One),
        // PreOne is added afterward to account for the 396 characters that
        // appear in both level One and PreOne.
        (exam_lists::LEVEL_01_PRE, Level::PreOne),
    ];
    let mut hm = HashMap::new();

    pairs.iter().for_each(|(c, l)| {
        c.chars().filter_map(Kanji::new).for_each(|k| {
            hm.insert(k, *l);
        });
    });

    hm
}

/// Determine how many Kanji of each exam level appear in some text,
/// given a lookup table.
///
/// The lookup table can be created via [`level_table`].
pub fn kanji_counts(s: &str, levels: &HashMap<Kanji, Level>) -> HashMap<Level, u32> {
    let mut counts: HashMap<Level, u32> = HashMap::new();

    s.chars()
        .filter_map(Kanji::new)
        .filter_map(|k| levels.get(&k))
        .for_each(|&l| {
            let counter = counts.entry(l).or_insert(0);
            *counter += 1;
        });

    counts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unicode_ranges() {
        let ks = all_kanji();

        assert_eq!(20989, ks.chars().count());
        assert_eq!(62967, ks.len()); // Bytes.
    }

    #[test]
    fn all_kanjiable() {
        assert!(exam_lists::LEVEL_10.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_09.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_08.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_07.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_06.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_05.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_04.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_03.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_02_PRE.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_02.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_01_PRE.chars().all(|c| is_kanji(c)));
        assert!(exam_lists::LEVEL_01.chars().all(|c| is_kanji(c)));
    }

    #[test]
    fn sane_overwrite() {
        let k = Kanji::new('氣').unwrap();
        let m = level_table();
        assert_eq!(Some(&Level::PreOne), m.get(&k))
    }

    #[test]
    fn lookup_map_length() {
        let m = level_table();
        assert_eq!(5906, m.len());
    }
}
