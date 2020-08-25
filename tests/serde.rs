use kanji::Kanji;
use serde_json;

#[test]
fn serde_instances() {
    assert!(serde_json::from_str::<Kanji>("\"合\"").is_ok());
}
