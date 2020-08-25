use kanji::*;
use serde_json;

#[test]
fn serde_instances() {
    assert!(serde_json::from_str::<Kanji>("\"合\"").is_ok());
    assert!(serde_json::from_str::<Hiragana>("\"あ\"").is_ok());
    assert!(serde_json::from_str::<Katakana>("\"ア\"").is_ok());
}
