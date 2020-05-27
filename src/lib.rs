//! A library for the handling and analysis of Japanese text, particularly
//! Kanji. It can be used to find the density of Kanji in given texts according
//! to their *Level* classification, as defined by the Japan Kanji Aptitude
//! Testing Foundation (日本漢字能力検定協会).
//!
//! # Usage
//! Foobar
//!
//! # Resources
//! - [CJK Unicode Chart](https://www.unicode.org/charts/PDF/U4E00.pdf) (pdf)
//! - [StackOverflow: Unicode Ranges for Japanese](https://stackoverflow.com/q/19899554/643684)

use std::char;

/// A single symbol of Kanji, also known as a [CJK Unified Ideograph].
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
/// With regards to Rust's string-handling nuances: all Japanese characters,
/// Kanji or otherwise, are a single Unicode Scalar Value, and thus can be
/// represented by a single internal `char`.
///
/// [CJK Unified Ideograph]: https://en.wikipedia.org/wiki/Han_unification
#[derive(Debug, PartialEq, Copy, Clone)]
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
        if is_kanji(&c) {
            Some(Kanji(c))
        } else {
            None
        }
    }

    /// Pull out the inner `char`.
    pub fn get(&self) -> char {
        self.0
    }

    /// The level or "kyuu" (級) of the Kanji, according to the official
    /// ordering from the Testing Foundation.
    pub fn level(&self) -> Level {
        Level::Unknown
    }
}

/// A Hiragana character, from あ to ん.
///
/// These are learned first by Japanese school children and foreign learners,
/// and are used most often for grammatical word endings and prepositions. Some
/// women's first names are written purely in Hiragana, as the characters
/// themselves have a soft, flowing feel to them (very much unlike the blocky,
/// angular Katakana).
#[derive(Debug, PartialEq)]
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
        if is_hiragana(&c) {
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

/// A Katakana character, from ア to ン.
///
/// These are typically learned after Hiragana, and are used to represent
/// foreign names, sound effects, and occasionally words whose Kanji are
/// "difficult". Two such examples are ネズミ (鼠) and アリ (蟻).
///
/// It used to be common to use Katakana as Hiragana are used today, so the
/// phrase君と街を歩きたい would have been written 君ト街ヲ歩キタイ. Admittedly
/// strange to modern eyes!
#[derive(Debug, PartialEq)]
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
        if is_katakana(&c) {
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

/// Japanese symbols and punctuation.
#[derive(Debug, PartialEq)]
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
        if is_japanese_punct(&c) {
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

/// Japanese full-width alphanumeric characters and a few punctuation symbols.
#[derive(Debug, PartialEq)]
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
        if is_alphanum(&c) {
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

/// A standard ASCII character.
#[derive(Debug, PartialEq)]
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

/// General categories for characters, at least as is useful for thinking about
/// Japanese.
///
/// Japanese "full-width" numbers and letters will be counted as `Number` and
/// `Letter` respectively, alongside their usual ASCII forms.
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
            .or(Hiragana::new(c).map(Character::Hiragana))
            .or(Katakana::new(c).map(Character::Katakana))
            .or(Punctuation::new(c).map(Character::Punctuation))
            .or(AlphaNum::new(c).map(Character::AlphaNum))
            .or(ASCII::new(c).map(Character::ASCII))
            .unwrap_or(Character::Other(c))
    }

    /// A convenience method for attempting to extract a possible `Kanji`.
    pub fn kanji(&self) -> Option<Kanji> {
        match self {
            Character::Kanji(k) => Some(*k),
            _ => None,
        }
    }
}

/// A level or "kyuu" (級) of Japanese Kanji ranking.
///
/// There are 12 of these,
/// from 10 to 1, including two "pre" levels between 3 and 2, and 2 and 1.
///
/// Japanese students will typically have Level-5 ability by the time they
/// finish elementary school. Level-5 accounts for 1,006 characters. By the end
/// of middle school, they would have covered up to Level-3 (1,607 Kanji) in
/// their Japanese class curriculum.
///
/// While Level-2 (2,136 Kanji) is considered "standard adult" ability, many
/// adults would not pass the Level-2, or even the Level-Pre2 (1,940 Kanji) exam
/// without considerable study. It is not only the reading and writing of the
/// characters themselves that is tested, but also their associated vocabularly,
/// usage in real text, and appearance in classic Chinese idioms (四字熟語).
///
/// Level data for Kanji above Level-2 is currently not provided by this
/// library.
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
    Unknown,
}

/// Kanji appear in the Unicode range 4e00 to 9ffc.
/// The final Japanese Kanji is 9fef (鿯).
///
/// For a chart of the full official range, see [this pdf] from the Unicode
/// organization.
///
/// ```
/// assert!(kanji::is_kanji(&'澄'));  // Obviously a legal Kanji.
/// assert!(!kanji::is_kanji(&'a'));  // Obviously not.
/// ```
///
/// [this pdf]: https://www.unicode.org/charts/PDF/U4E00.pdf
pub fn is_kanji(c: &char) -> bool {
    *c >= '\u{4e00}' && *c <= '\u{9ffc}'
}

/// Is a given `char` betwen あ and ん?
///
/// ```
/// assert!(kanji::is_hiragana(&'あ'));
/// assert!(!kanji::is_hiragana(&'a'));
/// ```
pub fn is_hiragana(c: &char) -> bool {
    *c >= '\u{3041}' && *c <= '\u{309f}'
}

/// Is a given `char` between ア and ン?
///
/// ```
/// assert!(kanji::is_katakana(&'ン'));
/// assert!(!kanji::is_katakana(&'a'));
/// ```
pub fn is_katakana(c: &char) -> bool {
    *c >= '\u{30a0}' && *c <= '\u{30ff}'
}

/// Does a given `char` belong to the set of Japanese symbols and punctuation?
pub fn is_japanese_punct(c: &char) -> bool {
    *c >= '\u{3000}' && *c <= '\u{303f}'
}

/// Does a given `char` belong to the set of Japanese alphanumeric characters
/// and western punctuation?
pub fn is_alphanum(c: &char) -> bool {
    *c >= '\u{ff01}' && *c <= '\u{ff5e}'
}

/// All possible Kanji characters, as well as non-character radicals, in a
/// heap-allocated UTF-8 `String`.
///
/// ```
/// let ks = kanji::all_kanji();
///
/// assert_eq!(Some('一'), ks.chars().next());
/// assert_eq!(Some('\u{9ffc}'), ks.chars().last());
/// assert_eq!(20989, ks.chars().count());
/// assert_eq!(62967, ks.len()); // Bytes.
/// ```
pub fn all_kanji() -> String {
    let mut s = String::with_capacity(62967); // Capacity in bytes.
    (0x4e00..=0x9ffc)
        .filter_map(char::from_u32)
        .for_each(|c| s.push(c));
    s
}

#[cfg(test)]
mod tests {}
