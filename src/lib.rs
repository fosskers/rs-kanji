//! A library for the handling and analysis of Japanese text, particularly
//! Kanji. It can be used to find the density of Kanji in given texts according
//! to their *Level* classification, as defined by the Japan Kanji Aptitude
//! Testing Foundation (日本漢字能力検定協会).
//!
//! The Kanji data presented here matches the Foundation's official 2020
//! February charts.
//!
//! # Usage
//!
//! Two main useful types are `Character` and `Kanji`.
//!
//! ### Reading Japanese Text
//!
//! To reinterpret every `char` of the input into a `Character` we can reason about:
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
//! But maybe we're just interested in the Kanji:
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
//! Alongside normal pattern matching, the `Character::kanji` method can also
//! help us extract `Kanji` values.
//!
//! ### Filtering
//!
//! In general, when we want to reduce a text to a single `Character` subtype,
//! we can `filter`:
//!
//! ```
//! let orig = "そこで犬が寝ている";
//!
//! let ks: String = orig.chars().filter(kanji::is_kanji).collect();
//! assert_eq!("犬寝", ks);
//!
//! let hs: String = orig.chars().filter(kanji::is_hiragana).collect();
//! assert_eq!("そこでがている", hs);
//! ```
//!
//! ### Level Analysis
//!
//! The `Kanji::level` method tells us what testing `Level` a given Kanji
//! belongs to.
//!
//! # Resources
//! - [CJK Unicode Chart](https://www.unicode.org/charts/PDF/U4E00.pdf) (pdf)
//! - [StackOverflow: Unicode Ranges for Japanese](https://stackoverflow.com/q/19899554/643684)
//! - [級別漢字表](https://www.kanken.or.jp/kanken/outline/data/outline_degree_national_list20200217.pdf) (pdf)

use std::char;

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
/// With regards to Rust's string-handling nuances: all Japanese characters,
/// Kanji or otherwise, are a single Unicode Scalar Value, and thus can be
/// represented by a single internal `char`.
///
/// [cjk]: https://en.wikipedia.org/wiki/Han_unification
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

/// The lowest level of the Kanji exam, learned in the 1st year of Japanese
/// elementary school.
///
/// ```
/// assert_eq!(80, kanji::LEVEL_10.chars().count());
/// ```
pub const LEVEL_10: &str = "\
一七三上下中九二五人休先入八六円出力十千\
口右名四土夕大天女子字学小山川左年手文日\
早月木本村林校森正気水火犬玉王生田男町白\
百目石空立竹糸耳花草虫見貝赤足車金雨青音";

/// These are learned in the 2nd year of Japanese elementary school.
///
/// ```
/// assert_eq!(160, kanji::LEVEL_09.chars().count());
/// ```
pub const LEVEL_09: &str = "\
万丸交京今会体何作元兄光公内冬刀分切前北\
午半南原友古台合同回図国園地場声売夏外多\
夜太妹姉室家寺少岩工市帰広店弓引弟弱強当\
形後心思戸才教数新方明星春昼時晴曜書朝来\
東楽歌止歩母毎毛池汽活海点父牛理用画番直\
矢知社秋科答算米紙細組絵線羽考聞肉自船色\
茶行西親角言計記話語読谷買走近通週道遠里\
野長門間雪雲電頭顔風食首馬高魚鳥鳴麦黄黒";

/// These are learned in the 3rd year of Japanese elementary school.
///
/// ```
/// assert_eq!(200, kanji::LEVEL_08.chars().count());
/// ```
pub const LEVEL_08: &str = "\
丁世両主乗予事仕他代住使係倍全具写列助勉\
動勝化区医去反取受号向君味命和品員商問坂\
央始委守安定実客宮宿寒対局屋岸島州帳平幸\
度庫庭式役待急息悪悲想意感所打投拾持指放\
整旅族昔昭暑暗曲有服期板柱根植業様横橋次\
歯死氷決油波注泳洋流消深温港湖湯漢炭物球\
由申界畑病発登皮皿相県真着短研礼神祭福秒\
究章童笛第筆等箱級終緑練羊美習者育苦荷落\
葉薬血表詩調談豆負起路身転軽農返追送速進\
遊運部都配酒重鉄銀開院陽階集面題飲館駅鼻";

/// These are learned in the 4th year of Japanese elementary school.
///
/// ```
/// assert_eq!(202, kanji::LEVEL_07.chars().count());
/// ```
pub const LEVEL_07: &str = "\
不争付令以仲伝位低例便信倉候借停健側働億\
兆児共兵典冷初別利刷副功加努労勇包卒協単\
博印参史司各告周唱喜器囲固型堂塩士変夫失\
好季孫完官害察巣差希席帯底府康建径徒得必\
念愛成戦折挙改救敗散料旗昨景最望未末札材\
束松果栄案梅械極標機欠歴残殺毒氏民求治法\
泣浅浴清満漁灯無然焼照熱牧特産的省祝票種\
積競笑管節粉紀約結給続置老胃脈腸臣航良芸\
芽英菜街衣要覚観訓試説課議象貨貯費賞軍輪\
辞辺連達選郡量録鏡関陸隊静順願類飛飯養験";

/// These are learned in the 5th year of Japanese elementary school.
///
/// ```
/// assert_eq!(193, kanji::LEVEL_06.chars().count());
/// ```
pub const LEVEL_06: &str = "\
久仏仮件任似余価保修俵個備像再刊判制券則\
効務勢厚句可営因団圧在均基報境墓増夢妻婦\
容寄富導居属布師常幹序弁張往復徳志応快性\
恩情態慣承技招授採接提損支政故敵断旧易暴\
条枝査格桜検構武比永河液混減測準演潔災燃\
版犯状独率現留略益眼破確示祖禁移程税築精\
素経統絶綿総編績織罪群義耕職肥能興舌舎術\
衛製複規解設許証評講謝識護豊財貧責貸貿賀\
資賛質輸述迷退逆造過適酸鉱銅銭防限険際雑\
非預領額飼";

/// These are learned in the 6th year of Japanese elementary school, the final
/// year before middle school.
///
/// 1,026 Kanji are learned by this point.
///
/// ```
/// assert_eq!(191, kanji::LEVEL_05.chars().count());
/// ```
pub const LEVEL_05: &str = "\
並乱乳亡仁供俳値傷優党冊処刻割創劇勤危卵\
厳収后否吸呼善困垂城域奏奮姿存孝宅宇宗宙\
宝宣密寸専射将尊就尺届展層己巻幕干幼庁座\
延律従忘忠憲我批担拝拡捨探推揮操敬映晩暖\
暮朗机枚染株棒模権樹欲段沿泉洗派済源潮激\
灰熟片班異疑痛皇盛盟看砂磁私秘穀穴窓筋策\
簡糖系紅納純絹縦縮署翌聖肺背胸脳腹臓臨至\
若著蒸蔵蚕衆裁装裏補視覧討訪訳詞誌認誕誠\
誤論諸警貴賃遺郵郷針鋼閉閣降陛除障難革頂\
骨";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elementary() {
        let ks = vec![LEVEL_10, LEVEL_09, LEVEL_08, LEVEL_07, LEVEL_06, LEVEL_05];

        assert_eq!(1026, ks.concat().chars().count());
    }
}
