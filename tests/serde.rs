use kanji::*;
use serde::{Deserialize, Serialize};

#[test]
fn serde_instances() {
    round_trip::<Kanji>("\"合\"");
    round_trip::<Hiragana>("\"あ\"");
    round_trip::<Katakana>("\"ア\"");
    round_trip::<Punctuation>("\"。\"");
    round_trip::<AlphaNum>("\"Ａ\"");
    round_trip::<ASCII>("\"a\"");
    round_trip::<Character>("\"魚\"");
}

fn round_trip<'a, T>(s: &'a str)
where
    T: Serialize + Deserialize<'a>,
{
    let from = serde_json::from_str::<T>(s).unwrap();
    let to = serde_json::to_string(&from).unwrap();
    assert_eq!(s, to);
}
