# Kanji

[![Tests](https://github.com/fosskers/rs-kanji/workflows/Tests/badge.svg)](https://github.com/fosskers/rs-kanji/actions)
[![](https://img.shields.io/crates/v/kanji.svg)](https://crates.io/crates/kanji)

A library for the handling and analysis of Japanese text, particularly Kanji. It
can be used to find the density of Kanji in given texts according to their
*Level* classification, as defined by the Japan Kanji Aptitude Testing
Foundation (日本漢字能力検定協会).

The Kanji data presented here matches the Foundation's official 2020 February
charts. Note that [some Kanji had their levels changed][changed] (pdf) as of
2020.

See the documentation for further explanation and usage examples.

For the Haskell version of this library, [see here][haskell].

[changed]: https://www.kanken.or.jp/kanken/topics/data/alterclassofkanji2020.pdf
[haskell]: http://hackage.haskell.org/package/kanji

### Example

To find out how many Kanji of each exam level belong to some text:

```rust
let level_table = kanji::level_table();
let texts = vec![
    "非常に面白い文章",
    "誰でも読んだ事のある名作",
    "飛行機で空を飛ぶ",
];

for t in texts {
    let counts = kanji::kanji_counts(t, &level_table);
    println!("{:#?}", counts);
}
```
