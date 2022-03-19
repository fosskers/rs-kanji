# `kanji` Changelog

## 2.0.0 (2022-03-19)

#### Changed

- `is_kanji` now does everything that `is_kanji_extended` used to.
- The argument to `is_kanji`, etc., is now `char` instead of `&char`.
- `is_hiragana` and `is_katakana` have had their ranges extended to better match
  the Unicode standard.

#### Removed

- `is_kanji_extended` has been removed in favour of `is_kanji`.

## 1.1.0 (2020-08-24)

#### Added

- `serde` trait implementations via an optional `serde` feature.

## 1.0.1 (2020-05-15)

#### Fixed

- Performance improvement when parsing `Character`s.

## 1.0.0 (2020-05-31)

This is the initial release of the library.

#### Added

- Kanji data from every level of the exam.
- Types and functions for basic Kanji analysis of Japanese text.
