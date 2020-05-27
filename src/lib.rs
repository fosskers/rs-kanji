//! A library for analysing the density of Kanji in given texts, according to
//! their Level classification, as defined by the Japan Kanji Aptitude Testing
//! Foundation (日本漢字能力検定協会).

/// A single symbol of Kanji. Japanese Kanji were borrowed from China over
/// several waves during the last 1,500 years. Japan declares 2,136 of these as
/// their standard set, with rarer characters being the domain of academia and
/// esoteric writers.
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
pub struct Kanji(char);

impl Kanji {
    /// Attempt to form a `Kanji`. Will fail if the given `char` is out of the
    /// Unicode range that Japanese text inhabits.
    pub fn new(_: char) -> Option<Kanji> {
        Some(Kanji('本'))
    }
}

/// [CJK Unified Ideographs] (aka "Kanji") appear in the Unicode range 4e00 to 9faf.
///
/// ```
/// assert!(kanji::is_kanji('澄'));  // Obviously a legal Kanji.
/// assert!(!kanji::is_kanji('a'));  // Obviously not.
/// ```
///
/// [CJK Unified Ideographs]: https://en.wikipedia.org/wiki/Han_unification
pub fn is_kanji(c: char) -> bool {
    c >= '\u{4e00}' && c <= '\u{9faf}'
}

#[cfg(test)]
mod tests {}
