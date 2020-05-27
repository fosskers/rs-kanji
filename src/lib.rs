//! A library for the handling and analysis of Japanese text, particularly
//! Kanji. It can be used to find the density of Kanji in given texts according
//! to their *Level* classification, as defined by the Japan Kanji Aptitude
//! Testing Foundation (日本漢字能力検定協会).

use std::char;

/// A single symbol of Kanji. Japanese Kanji were borrowed from China over
/// several waves during the last 1,500 years. Japan declares 2,136 of these as
/// their standard set, with rarer characters being the domain of place names,
/// academia and writers.
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

    /// The level or "kyuu" (級) of the Kanji, according to the official
    /// ordering from the Testing Foundation.
    pub fn level(&self) -> Level {
        Level::Unknown
    }
}

/// A Hiragana character, from あ to ん.
///
/// These are learned first by Japanese school children, and are used most often
/// for grammatical word endings and prepositions. Some women's first names are
/// written purely in Hiragana, as the characters themselves have a soft,
/// flowing feel to them (very much unlike the blocky, angular Katakana).
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

/// General categories for characters, at least as is useful for thinking about
/// Japanese.
///
/// Japanese "full-width" numbers and letters will be counted as `Number` and
/// `Letter` respectively, alongside their usual ASCII forms.
pub enum Character {
    Kanji(Kanji),
    Hiragana(Hiragana),
    Katakana,
    Number,
    Letter,
    Punctuation,
    Other,
}

impl Character {
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

/// [CJK Unified Ideographs] (aka "Kanji") appear in the Unicode range 4e00 (一)
/// to 9ffc. The final Japanese Kanji is 9fef (鿯).
///
/// For a chart of the full official range, see [this pdf] from the Unicode
/// organization.
///
/// ```
/// assert!(kanji::is_kanji('澄'));  // Obviously a legal Kanji.
/// assert!(!kanji::is_kanji('a'));  // Obviously not.
/// ```
///
/// [CJK Unified Ideographs]: https://en.wikipedia.org/wiki/Han_unification
/// [this pdf]: https://www.unicode.org/charts/PDF/U4E00.pdf
pub fn is_kanji(c: char) -> bool {
    c >= '\u{4e00}' && c <= '\u{9ffc}'
}

/// Is a given `char` betwen あ and ん?
///
/// ```
/// assert!(kanji::is_hiragana('あ'));
/// assert!(!kanji::is_hiragana('a'));
/// ```
pub fn is_hiragana(c: char) -> bool {
    c >= '\u{3040}' && c <= '\u{309f}'
}

/// Is a given `char` between ア and ン?
///
/// ```
/// assert!(kanji::is_katakana('ン'));
/// assert!(!kanji::is_katakana('a'));
/// ```
pub fn is_katakana(c: char) -> bool {
    c >= '\u{30a0}' && c <= '\u{30ff}'
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
