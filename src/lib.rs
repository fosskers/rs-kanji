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
//! TODO
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
//! let same_as_uni = kanji::LEVEL_10.chars().max() < kanji::LEVEL_09.chars().min();
//! assert!(!same_as_uni);
//! ```
//!
//! # Resources
//! - [CJK Unicode Chart](https://www.unicode.org/charts/PDF/U4E00.pdf) (pdf)
//! - [StackOverflow: Unicode Ranges for Japanese](https://stackoverflow.com/q/19899554/643684)
//! - [級別漢字表](https://www.kanken.or.jp/kanken/outline/data/outline_degree_national_list20200217.pdf) (pdf)
//!
//! [changed]: https://www.kanken.or.jp/kanken/topics/data/alterclassofkanji2020.pdf

use std::char;
use std::collections::HashMap;

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
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
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
#[derive(Debug, PartialEq, Copy, Clone)]
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
/// assert!(kanji::is_kanji(&'澄'));  // Obviously a legal Kanji.
/// assert!(!kanji::is_kanji(&'a'));  // Obviously not.
/// ```
///
/// [compat]: https://www.unicode.org/charts/PDF/UF900.pdf
/// [this pdf]: https://www.unicode.org/charts/PDF/U4E00.pdf
pub fn is_kanji(c: &char) -> bool {
    (*c >= '\u{4e00}' && *c <= '\u{9ffc}') // Standard set.
        || (*c >= '\u{f900}' && *c <= '\u{faff}') // CJK Compatibility Ideographs.
}

/// Detect if a `char` is Kanji while accounting for all of the Unicode CJK
/// extensions.
///
/// `is_kanji` should be enough for normal use.
pub fn is_kanji_extended(c: &char) -> bool {
    (*c >= '\u{4e00}' && *c <= '\u{9ffc}') // Standard set.
        || (*c >= '\u{f900}' && *c <= '\u{faff}') // CJK Compatibility Ideographs.
        || (*c >= '\u{3400}' && *c <= '\u{4dbf}') // Extension A
        || (*c >= '\u{20000}' && *c <= '\u{2a6dd}') // Extension B
        || (*c >= '\u{2a700}' && *c <= '\u{2b734}') // Extension C
        || (*c >= '\u{2b740}' && *c <= '\u{2b81d}') // Extension D
        || (*c >= '\u{2b820}' && *c <= '\u{2cea1}') // Extension E
        || (*c >= '\u{2ceb0}' && *c <= '\u{2ebe0}') // Extension F
        || (*c >= '\u{30000}' && *c <= '\u{3134a}') // Extension G
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
pub fn by_level() -> HashMap<Kanji, Level> {
    let pairs = vec![
        (LEVEL_10, Level::Ten),
        (LEVEL_09, Level::Nine),
        (LEVEL_08, Level::Eight),
        (LEVEL_07, Level::Seven),
        (LEVEL_06, Level::Six),
        (LEVEL_05, Level::Five),
        (LEVEL_04, Level::Four),
        (LEVEL_03, Level::Three),
        (LEVEL_02_PRE, Level::PreTwo),
        (LEVEL_02, Level::Two),
    ];
    let mut hm = HashMap::new();

    pairs.iter().for_each(|(c, l)| {
        c.chars().filter_map(Kanji::new).for_each(|k| {
            hm.insert(k, *l);
        });
    });

    hm
}

/// The lowest level of the Kanji exam, learned in the 1st year of Japanese
/// elementary school.
pub const LEVEL_10: &str = "\
一七三上下中九二五人休先入八六円出力十千口右名四土夕大天女子字学小山川左年手文日\
早月木本村林校森正気水火犬玉王生田男町白百目石空立竹糸耳花草虫見貝赤足車金雨青音";

/// These are learned in the 2nd year of Japanese elementary school.
pub const LEVEL_09: &str = "\
万丸交京今会体何作元兄光公内冬刀分切前北午半南原友古台合同回図国園地場声売夏外多\
夜太妹姉室家寺少岩工市帰広店弓引弟弱強当形後心思戸才教数新方明星春昼時晴曜書朝来\
東楽歌止歩母毎毛池汽活海点父牛理用画番直矢知社秋科答算米紙細組絵線羽考聞肉自船色\
茶行西親角言計記話語読谷買走近通週道遠里野長門間雪雲電頭顔風食首馬高魚鳥鳴麦黄黒";

/// These are learned in the 3rd year of Japanese elementary school.
pub const LEVEL_08: &str = "\
丁世両主乗予事仕他代住使係倍全具写列助勉動勝化区医去反取受号向君味命和品員商問坂\
央始委守安定実客宮宿寒対局屋岸島州帳平幸度庫庭式役待急息悪悲想意感所打投拾持指放\
整旅族昔昭暑暗曲有服期板柱根植業様横橋次歯死氷決油波注泳洋流消深温港湖湯漢炭物球\
由申界畑病発登皮皿相県真着短研礼神祭福秒究章童笛第筆等箱級終緑練羊美習者育苦荷落\
葉薬血表詩調談豆負起路身転軽農返追送速進遊運部都配酒重鉄銀開院陽階集面題飲館駅鼻";

/// These are learned in the 4th year of Japanese elementary school.
pub const LEVEL_07: &str = "\
不争井付令以仲伝位低佐例便信倉候借健側働億兆児共兵典冷初別利刷副功加努労勇包卒協\
単博印参司各周唱器固城埼塩変夫失奈好媛季孫完官害富察岐岡崎巣差希席帯底府康建径徒\
徳必念愛成戦折挙改敗散料旗昨景最望未末札材束松果栃栄案梅梨械極標機欠残氏民求沖治\
法泣浅浴清満滋漁潟灯無然焼照熊熱牧特産的省祝票種積競笑管節約結給続縄置群老臣良芸\
芽英茨菜街衣要覚観訓試説課議貨賀軍輪辞辺連達選郡量録鏡関阜阪陸隊静順願類飛飯養香\
験鹿";

/// These are learned in the 5th year of Japanese elementary school.
pub const LEVEL_06: &str = "\
久仏仮件任似余価保修個停備像再刊判制則効務勢厚句可史告喜営因団囲圧在均型基堂報境\
墓増士夢妻婦容寄導居属布師常幹序弁張往得復志応快性情態慣技招授採接提損支政故救断\
旧易暴条枝査格桜検構武歴殺毒比永河液混減測準演潔災燃版犯状独率現留略益眼破確示祖\
禁移程税築粉精紀素経統絶綿総編績織罪義耕職肥能脈興舎航術衛製複規解設許証評講謝識\
護豊象財貧責貯貸費貿資賛賞質輸述迷逆造過適酸鉱銅防限険際雑非領額飼";

/// These are learned in the 6th year of Japanese elementary school, the final
/// year before middle school.
///
/// 1,026 Kanji are learned by this point.
pub const LEVEL_05: &str = "\
並乱乳亡仁供俳俵値傷優党冊処券刻割創劇勤危卵厳収后否吸呼善困垂域奏奮姿存孝宅宇宗\
宙宝宣密寸専射将尊就尺届展層己巻幕干幼庁座延律従忘忠恩憲我批承担拝拡捨探推揮操敬\
敵映晩暖暮朗机枚染株棒模権樹欲段沿泉洗派済源潮激灰熟片班異疑痛皇盛盟看砂磁私秘穀\
穴窓筋策簡糖系紅納純絹縦縮署翌聖肺胃背胸脳腸腹臓臨至舌若著蒸蔵蚕衆裁装裏補視覧討\
訪訳詞誌認誕誠誤論諸警貴賃退遺郵郷針銭鋼閉閣降陛除障難革頂預骨";

/// These are learned in middle school.
pub const LEVEL_04: &str = "\
丈与丘丹乾互介仰伺依侵俗倒偉傍傾僧儀兼冒凡凶刈到刺剣剤劣勧匹占即却及叫召吐含吹咲\
唐嘆噴圏坊執堅堤塔壁壊壱奇奥奴妙姓威娘婚寂寝尋尽尾屈峠峰巡巨帽幅幾床弐弾彩影彼征\
御微徴忙怒怖恋恐恒恥恵悩惑惨慎慢慮憶戒戯扇払扱抗抜抱抵押拍拓拠振捕掘描握援搬摘撃\
攻敏敷斜旨旬是普暇暦曇更替朱朽杯枯柄柔桃欄歓歳殖殿汗汚沈沢沼況泊浜浮浸涙淡添渡溶\
滴漫澄濁濃為烈煙煮燥爆狂狩狭猛獣獲玄珍環甘畳疲療皆盆盗監盤盾眠瞬矛砲祈秀称稲稿突\
端箇範粒紋紫紹絡継維網緯縁繁繰罰翼耐肩肪胴脂脚脱腐腕腰膚致舗舞舟般芋芝茂荒菓蓄薄\
薪被襲触訴詰詳誇誉謡豪販賦贈越趣距跡跳踊踏躍軒較載輝輩込迎迫逃透途遅違遣避郎釈鈍\
鉛鋭鎖鑑闘陣陰隠隣隷雄雅雌離雷需震霧露響項頼飾駆騒驚髪鬼鮮麗黙鼓齢";

/// These are learned by the end of middle school.
pub const LEVEL_03: &str = "\
乏乙了企伏伐伴伸佳侍促倣倹偶催債克免冗冠凍凝刑削励勘募匠匿卑卓卸厘又双吉吏哀哲啓\
喚喫嘱坑埋塊塗墜墨墳墾壇奉契奪如妨姫娯婆婿嫁嬢孔孤宴審寿封尿岳峡崩巧帆帝幻幽廉廊\
弧彫徐忌怠怪恨悔悟悦惜愚慈慌慕慨慰憂憎憩房抑択抽拘掃掌排掛控措掲揚換揺携搾摂撮擁\
擦敢斗斤斥施既昇晶暫架某桑棄棋楼概欧欺殊殴没泌浪湾湿滅滑滝滞漂漏潜潤濫瀬炉炊炎焦\
牲犠猟獄甲畔畜疾痘癖硬碑礎祉稚穂穏穫窒符篤簿籍粋粗粘糧紛紺絞綱緊締緩縛縫繕翻聴肝\
胆胎胞脅膜膨芳苗菊華葬藩虐虚蛮衝衰袋裂裸覆訂託詠該誘請諮諾謀譲豚貫賊賢赦赴超軌軸\
辛辱逮遂遇遭遵邦邪郊郭酔酵鋳錠錬錯鍛鎮鐘閲阻陪陳陵陶隆随隔隻雇零霊顧飽餓駐騎髄魂\
魅魔鯨鶏";

/// These are learned by the end of high school.
pub const LEVEL_02_PRE: &str = "\
且丙亜享亭仙伯但併侮侯俊俸倫偏偵偽傑傘僕僚儒償充准凸凹刃剖剛剰劾勅勲升厄叔叙吟呈\
呉唆唇唯喝喪嗣嚇囚坪垣培堀堕堪塀塁塑塚塾壌壮奔奨妃妄妊妥姻娠媒嫌嫡宜宰宵寛寡寧寮\
尉尚尼履屯岬崇帥幣庶庸廃廷弊弔弦彰循徹忍恭悠患悼惰愁愉慶憤憾懇懐懲懸戻扉扶抄把披\
抹拐拒拙括拷挑挟挿捜据搭摩撤撲擬斉斎旋昆暁曹朕朴杉析枠枢柳栓核栽桟棚棟棺槽款殉殻\
汁江沸泡泥泰洞津洪浄浦涯涼淑渇渉渋渓渦溝漆漠漬漸濯煩爵猫献猶猿珠琴璽瓶甚畝疎疫症\
痢痴癒盲眺睡督矯砕硝硫碁磨礁祥禅禍租秩稼窃窮窯竜筒粛粧糾紡索累紳緒繊繭缶罷羅翁耗\
肌肖肢肯臭舶艇艦茎荘菌薦薫藻虜虞蚊蛇蛍融衡衷裕褐褒襟覇訟診詐詔誓諭謁謄謙謹譜貞貢\
賄賓賜賠購践軟轄迅迭逐逓逝逸遍遮遷還邸酌酢酪酬酷醜醸釣鈴鉢銃銘閑閥附陥隅雰霜靴韻\
頑頒頻顕飢駄騰麻";

/// These are considered "standard adult level".
pub const LEVEL_02: &str = "\
串丼乞亀伎侶俺傲僅冥冶凄刹剥勃勾匂叱呂呪咽哺唄唾喉喩嗅嘲堆塞填妖妬嫉宛尻崖嵐巾弄\
弥彙怨恣惧慄憧憬戚戴拉拭拳拶挨挫捉捗捻摯斑斬旦旺昧曖曽枕柵柿桁梗椅椎楷毀氾汎汰沃\
沙淫湧溺潰煎爪爽牙狙玩瑠璃璧瓦畏畿痕痩瘍眉睦瞭瞳稽窟箋箸籠綻緻罵羞羨肘股脇脊腎腫\
腺膝膳臆臼舷艶芯苛萎葛蓋蔑蔽藍藤虎虹蜂蜜袖裾訃詣詮誰諦諧謎貌貪貼賂賭踪蹴辣遜遡那\
酎醒采釜錦錮鍋鍵鎌闇隙韓頃須頓頬顎餅餌駒骸鬱鶴麓麺";

/// The second-highest level, which adds an additional 1,238 characters on top
/// of the standard 常用 set.
pub const LEVEL_01_PRE: &str = "\
丑丞乃之乍乎乘也亂云亘亙些亞亥亦亨亮什仇仔伊伍伶伽佃佑佛佼侃來俄俠俣俱倂倖倦倭假\
偓偲傭傳僑僞價僻儉儘儲允兇兎兒兜兩其册冴凌凧凪凰凱函剃剩劃劉劍劑劫勞勳勵勸勺勿匁\
匙匝匡匪區卜卦卯卷卽卿厨厩厭參叉叛叡叢叩只叶吃吊吋吞吠吻吾呆咳哉哨哩啄啐啞喋喧喬\
單喰嘉嘗嘩噂噌噓噸噺嚙嚴囊囑圃圈國圍圓圖團圭坐坤坦垢埠埴堯堰堵堺塙塵增墮壓壕壘壞\
壤壬壯壹壺壻壽夙夷奄套奧奬妓妾姐姑姥姦姪姶娃娩娼婁嬉嬬嬰孃孜孟學宋宍宏宕宥寅寓寢\
實寫寬寵寶將專對尖尤尭屆屍屑屢屬岨岱峨峯峻峽嵩嵯嶋嶺嶽巌巖巢巳巴巷巽帖帶幌幡庄庇\
庖庚庵廓廟廠廢廣廳廻廿弗弘弛弼彈彊彌彦彪彬徑從徵德徽忽怜怯恆恕恢恰悅悉悌悶惇惚惟\
惠惡惣惱惹愈愼慘慧慾憐應懷戀戊戎或戟戰戲戾托扮拂拔拜按挺挽挾捌捧捲捷捺掠掩掬揃插\
揖揭搔搖搜摑摸摺撒撚撞撫播撰擇擊擔據擢擧擴擾攝攪收效敍敎敕敦數斌斐斡斧斯斷於旭昂\
昌昏晃晉晋晒晚晝晦智暢曆曉曙曝曳曾會朋朔李杏杓杖杜杢杭杵杷枇柁柊柏柑柘柚柴柾栂栖\
栗栴桂桐桓桔桶梁梓條梢梧梯梱梶棉棧棲椀椋椙椛椴椿楊楓楕楚楠楢楯楳榊榎榛榮槇槌槍槙\
槪槻樂樋樓樗樞樟樣樫樵樺樽橘橡橫橿檀檎檜檢檮櫓櫛櫻權欣欽歎歐歡此步歪歷歸殆殘殼毅\
毆每毘氣汀汐汝汲沌沒沓沫洛洩洲浩浬涉涌淀淋淘淚淨淳淵淸淺渚渠渥渴湊湘湛溜溢溪溫滯\
滿漉漑漕漣潑潛澁澗澤澱濕濟濠濡濤濱瀆瀕瀞瀦瀧瀨灌灘灣灸灼烏烹焚焰煉煤煽熔燈燐燒燕\
營燦燭爐爭爲爺爾牌牒牝牟牡牢牽犀犧狀狐狗狛狸狹狼狽猪猷獅獨獵獸獻玖玲珂珊珪琉琢琳\
琵琶瑚瑛瑞瑤瑳瓜瓢甁甑甜甥甫畠畢畦畫當畷疊疋疏疹痔瘦癌癡發皐盃盈盜盡眞瞥矧矩砥砦\
砧硏硯硲碇碍碎碓碗碧碩磐磯礦礪祁祇祐祕祿禄禎禦禪禮禰禱禽禾禿秤秦稀稔稗稜稻穆穎穗\
穣穩穰穿窄窗窪窺竈竊竝竣竪竺竿笈笠笥笹筈筏筑箔箕箭篇篠篦簞簸簾籾粁粂粍粕粟粥粹糊\
糎糞糟糠紐紗紘紬絃絢絲經綜綠綬綴綾緋緖緣緬縞縣縱總繡繩繪繫繼纂續纏纖缺罐罫翠翫翰\
耀而耶耽聡聯聰聲聽聾肅肇肋肱肴胡胤脆脹腔腦腿膏膽膿臟臥臺與舊舍舖舘舛舜舵艮艷芙芥\
芭芹苅苑苓苔苧苫茄茅茜茸荊荏荻莊莖莞莫菅菖菩菰菱萄萊萌萩萬萱葎葡董葦葱葵葺蒐蒔蒙\
蒜蒲蒼蓉蓑蓬蓮蔀蔓蔚蔣蔦蔭蕃蕉蕊蕎蕗蕨蕩蕪薗薙薩薯薰藁藏藝藥藪藷蘇蘭處虛號虻蚤蛋\
蛙蛛蛤蛭蛸蛾蜎蜘蝕蝦蝶螢螺蟬蟲蟹蟻蠅蠣蠶蠻衞衿袈袴袷裝裟裡裳襃襖覗覺覽觀觸訊訣註\
詑詫誹誼諏諒諜諫諺謂謠謬證譯譽讀讃變讓豐豫豹貰貳賑賣賤賴贊贋赫趨跨踐蹄蹟軀輔輕輯\
輿轉轍轟轡辨辭辯辰辻辿迂迄迦迺逗這逢逼遁遙遞遥遲遼邊邑郁郞鄕鄭鄰酉酋醇醉醍醐醫醬\
醱釀釋釘釦釧鈷鉤鉦鉾銑銚鋒鋤鋪鋲鋸錄錆錐錘錢錨錫鍊鍍鍔鍬鍾鎗鎚鎧鎭鏑鐙鐵鐸鑄鑓鑛\
閃閏閤關阿陀陷隈隨險隱隸隼雀雁雙雛雜雫霞靈靑靖靜靱鞄鞍鞘鞠鞭韃韮頁頗頸顏顚顯飮飴\
餐餘餠饗馨馳馴駁駈駕駿騷驅驗驛髓體髭髮鬪魁魯鮎鮒鮪鮫鮭鯉鯖鯛鰍鰐鰭鰯鰹鰺鰻鱒鱗鳩\
鳳鳶鴇鴛鴦鴨鴫鴻鵜鵠鵡鵬鶯鷄鷗鷲鷹鷺鸚鹼鹽麒麟麥麴麵麿黃黍黑默黛點黨鼎鼠齊齋齒齡\
龍龜龝欄廊朗虜殺類隆塚晴猪益神祥福靖精羽諸都飯飼館侮僧免勉勤卑喝嘆器塀墨層悔慨憎\
懲敏既暑梅海渚漢煮琢碑社祉祈祐祖祝禍禎穀突節練繁署者臭著褐視謁謹賓贈逸難響頻";

/// The highest examination level, which nearly doubles the existing set with an
/// additional 2,928 characters.
pub const LEVEL_01: &str = "\
乂几匕匚丫于兀孑孒尸已幺弋丐亢仂仄仆仍卅夬夭尹弌弖戈扎曰毋丕丱仗仞仟册刋匆卉卮叨\
叭叮叺圦夲孕屶弍戉朮艾辷价伉冱凩刎刔匈卍吁夸奸屹幵忖戌戍扛扞扠扣扨收朶朸朿汕犲聿\
艸芒阡羽估佇佗佚佛佝佞兌冏刪劬劭匣吝吩听吭吮吶吼吽呎呏圻址坎坏壯夾妝妣孚孛尨屁岌\
岑岔巫彷忤忱忸忻扼找抃抉抒抓抔抖旰旱杆杙杞杠杣步每汞汨汪沁沂沍沐沒沚沛狃狄狆瓧甸\
疔皁矣竍糺罕肚肛芟芫芬苆豕邨阨阮阯免乖亞亟佩佯佰佶佻來侈侏侑侖侘兒兩冽凭刮刳劼卷\
呟呰呵呶呷呻咀咄咆咎咏咐囹坡坩坿妲姆孥屆岫帑帙帚帛幷弩彽彿徂忝忿怏怕怙怛怡怦怩怫\
怺戔戾抻拂拆拇拈拊拌拑拔拗旻昃昊杪杲杳杼枉枋枌枡歿殀毟氓氛沮沱沽沾泄泅泓泗泛泝泪\
泯炒炙爬爭牀狀狎狒玫瓩疚疝盂矻矼祀秉穹竏籵罔羌肬肭舍苙苜苞苟苡苳苴苹苺苻范茆茉衫\
軋迚迪邯邱邵陂靑卑社俎俐俑俔俘俚俛俟俤俥兪冑剄剋剌勁匍卽厖叟咢咤咥咨咫咬咸哂哄哇\
哈囿垓垠垤奎奐奕姙姚姜姨姮姸孩宦屎峙庠弭徇很徊怎怱恂恃恆恊恍恟恤恪恫恬扁拜拮拯拱\
拵挂挌挘斫昜昴昵昶曷朏枳枴枷枸枹柎柝柞柢柤柩柬柮柯殃殄洌洒洟洫洵洶洸洽炬炮炯炷炸\
爰牴狠狡狢玻珀珈瓮瓰瓱瓲畋疣疥癸盻眄眇眈矜砌砒禹禺秕穽竓竕竽紂紆紉缸罘胄胖胙胚胛\
胝胥臾茗茘茫茯茱茲茴茵茹荀荅荐衍衲衵袂迢迥酊閂陌侮勉祉祈突者乘俯俶俾倂倅倆倏們倔\
倚倡倥倨倩倪倬冢冤凅凊剔剞勍哢哥哦哭哮哳哽唏唔圄埃埆埒奘奚娉娑娟娥宸峪峭峽徑恁恙\
恚悁悃悄悅悋悍悒悖悚悛悧拿挈挾捍捏捐效旁旃旄旆晁晉晏晟栞栩栫栲桄框桎桙桛殷氣浙浚\
浣浹涅涉涎涓涕烋烙烝狷狹珞珥珮畚畛疱疳疵疸疼疽痂痃皰盍眛眞眥眩砠祓祕祗祚祟祠秣秧\
秬窈站竚竝笆笊笏粏紊紕紜缺罟罠羔翅耄耆耘耙耿胯胱舐舫芻茣荳荼莅莉莎莖莚莟莠莢莨莪\
虒虔蚌蚓蚣蚩蚪衄衾袍袒袗袙袢訌訐訖豈豺赳躬迴迹逅郛郞郢郤陜陝陞陟韋鬯益神悔海祐祖\
祝臭偃假偈偐偕偖做偬偸冕剪剳勒勖匏匐區參售唸啀啅啖啜啣圈圉國堊娵娶婀婉婢婪婬孰寇\
將專屛屠崑崔崙崚崛崟崢巢帶帷彗徘徙從悴悵悸悽悾惆惓惘戛捫捶掀掉掎掏掖掟掫敍敎敕敖\
斛旌晚晝晞晢晤晧晨曼桴桷梃梏梔條梟梠梭梲梵梺梻欷欸殍毫毬涵涸淅淆淌淒淙淚淨淪淬淸\
淹淺烽焉牾猊猖猗猜猝琅瓷瓸畤痊痍痒皎盒眷眸硅硏窕竟竡笘笙笞笨笳笵紮紲紵紿絅絆羚羝\
翊聆聊脛脣脩脯舂舳舸莊菁菎菘菠菫菲菴菽萁萃萍處蚫蚯蚰蚶蛄蛆蛉衒袤袰袱袵袿裃裄覓訛\
訥趺趾跂躮軛逋逍逑逕逖逞逡逧酖釵閊陬陲陷馗鹵麥朗殺祥敏梅傀傅傚剩剴勞厥啻啼啾喀喃\
喇喊喑喘喙喞喟喨單圍堝堡堯壹壻奠奢媚孱寐寔嵋嵌嵎嵒幀幃幄廁廂弑彭徨惠惡惱惴惶惺惻\
愀愃愎愒愕掣掾揀揄揆揉插揣揩揭揶搜敝敞晰曾朞棍棕棗棘棠棣棧棹椁椄椈椏椒椚欹殘殼毯\
毳渙渝渟渣渫渭渴游渺渾湃湍湎湫湲溂焙焜焠爲牋犂犇猋猥猩猯猴琥琲琺甦畫畬痙痞痣發皓\
皖皴盜睇稈稍窖窗窘竢竦筅筌筍筐粢粤粨絎絏絖絣絨絮絲絳翔翕耋聒脾腆腋腑腓舒萬萵萸萼\
葆葩葭葷虛蛔蛞蛟蛬蛯裎裙覃覘觜觝訝訶詁詆詈詒詛貂貲貳貶貽赧趁跋跌跎跏跖跚跛軫軻軼\
辜逵鄂酣酥釉釿鈑鈔鈕鈞閔隋隍靫颪馭馮黃黑黹虜隆晴猪都喝既渚琢著視逸亂亶傳傴僂僉剽\
剿勠勣勦匯嗄嗇嗔嗚嗜嗟嗤圓塋塒壼奧媼媽嫋寞尠嵬幎廈徭惷愆愍愧愴愼愾慊慍戡搏搓搖搗\
搤搦搨搶斟暄暈暉暘會椰椹椽楔楝楡楫楮楸楹楾榀榁榔歃歇溏溘溟溥溪溫溲溷溽滂滄滉滓滔\
煌煖煥煦煬猾獏瑁瑇瑕瑙瑜瑟瑶甁當畸痰痲痳痹痺痼痾痿瘁皙盞睚睛睥睨睫矮碌碎碚祺祿稟\
稠筥筧筮筰筱筲筴筵粱粲粳糀絛絽綉綏經罧罨罩耡聘肄肅肆腠腥腦腮腱腴舅艀蒟蒡蒹蒻蒿蓁\
蓆蓍蓐蓖蓙號蛹蛻蜀蜃蜆蜈蜉蜊蜍蜑蜒蜓蜹衙裔裘裝裨裲裼褂褄觥詢詬詭詼誂誄誅豢貉貊賈\
跟跣跪躱軾輀輅輊辟逾遉遏遐遑遒遖鄒鄕酩鈿鉅鉈鉉鉋鉗鉞閘隕隗隘雉雋雍雎雹頌頏飩飫飭\
飮骭髢魞鳧鳰黽廊塚靖飯勤暑煮碑僖僞僥僭僮兢匱厲嗷嗽嗾嘔嘖嘛圖團塹塿墅壽夥奬嫖嫗嫣\
嫩孵寢寤寥實寨寬對嶂嶄嶇幔幗廖愨愬愿慂慇慓慘慝慟慠慥慳慴慵慷截搴摎摏摧摶敲暝榑榕\
榜榧榮榱榴榻榾槁槃槇槊槎槐槓歉殞滌滯滲滷滸滾滿漓漱漲漾熄熕熙犒犖瑣瑤瑪瑰甃甅瘉瘋\
瘧皹盡睹睾碣碪禊窩竭竰箍箏箒箘箚箜箝粹粽綠綢綣綦綫綯綰綵綸綺綽緇翡聚聢膀膂膃膈膊\
臧臺與蓼蓿蔔蔕蔗蔘蔟蔡蜚蜥蜩蜴蜷蜻蜿裴裹褌褓覡誑誚誡誣誥誦誨趙跼跿踉輒輓輕遙遞鄙\
酲酳銓銕銖銛銜閧閨靤靺靼鞁鞅韶颯颱骰髣髦麼齊福精飼僧嘆塀慨漢禍禎署褐賓僵價儁儂儉\
儚冪凜劈劍匳嘴嘶嘸噁噎增墟墮嫺嫻嬋嬌寫嶝嶢幟幢廛廝廡廢廣彈徵德慙慫憔憚憫憮戮撈撓\
撕撥撩擒數槧槨槭槲槿樂樅樊樒樓樔樛樞樣歐殤毆漿潘潛潦潯潸潺澁澆澎熨熬牖獗瑩瑾璇璋\
甍瘟瘠瘡瘢瘤瘦皚皺瞋瞎瞑碼碾磅磊磋磑禝稷稻箴篁篆篋篌糂糅緖緘緝緞緡緣緤緲縅羯翦耦\
膠膣膵舖艘蕀蕁蕕蕘蕣蝌蝎蝓蝗蝘蝟蝠蝣蝨蝮蝲蝴蝸褞褥褪褫諂諄諍諚豌貎賚賣踐踑踝踞踟\
輛輜輟輦遨遯鄰鄲醂醉醋銷銹鋏錵錺閭霄霆霈靠鞋鞐頡頤餃餉駑駘駛駝駟髫髮髱髴鬧魃魄魬\
魳魴魸鴃鴆麩麾黎齒墨層憎穀節練儔儕冀劑勳勵嘯噤噦噪噫噬噯嚆圜壅嬖學寰嶬嶮廨廩徼憊\
憑憖憺懆懈懊懌懍戰撻撼擂擅擇擔據暹暾曆曉槪樸橄橇橈橙橦橫檠歔歙歷殪殫澡澣澤澪澳澹\
濛熹熾燈燉燎燒燔燗獨獪璞甌甎瘰瘴瘻盥盧瞞瞠磚磧磬窶篝篥篩篪簑糒縉縊縋縒縕縟縡縣罹\
羲耨膩膰臻艙蕭蕷蕾薀薇薈薊薏薐薑薔薛薜薤薨螂螈螗螟螢蟇衞褶褸襁覦覩諠諡諢諤諳諷謔\
豫賴赬赭踰踴踵蹂躾輳輹輻辨遲遶邁鋺錄錏錙錚錢錣錻閹閻閾隧隨險雕霍霎霏霑霓霖霙靜靦\
頷頽餒餔餘駭駮駱骼髷髻鬨鮃鮇鮑鮓鮖鴉鴒鴕鴟鴣麈麭黔默龍龜﨟諸器謁頻儡嚀嚊壎壑壓嬪\
嬲嬶孺嶷嶼嶽幫彌懃應懋懦戲擊擘擠擡擣擧擯擱斂曚朦檄檋檐檔檗檢檣檬歛氈濔濕濘濟濬濱\
營燠燧燬燮燵牆獮獰癆癇癈癘盪瞰磴磽禧禪穗窿篷簀簇簍簓簗糜糝縱縲縵縷縹縺縻總繆罅翳\
聰聲聳膺膽膾臀臂臉舊艚艝艱薰薹薺藉藏藐虧螫螯螳螻螾蟀蟄蟋蟎褻襃襄襌覬諱謇謐謖謗謠\
謨谿貘賺賻賽蹇蹈蹉蹊蹌蹐輾轂轅遽邀邂醢鍊鍠鍮鍼闃闊闌闍隰隱隸雖鞜顆颶餞餠餡餤馘駸\
駻騁鮟鮠鮨鮱鮲鮴鴾鴿鵁鵄鵆鵇麋黜黝點黻鼾齋龠館繁嚔嚠壘壙彝懣戳擲擴擺擽斃斷旛檳檸\
檻櫂櫃歟歸殯濺濾瀁瀉瀋瀏瀑燹燻燼燿獵甓甕癜瞻瞼瞽瞿矇礑礒禮穡穢竄竅簣簧繖繙繚繝繞\
繧羂羃翹聶臍臑艟藕藜藝藥蟠蟪蟯蟲蠆襠覲觴謦謫謳謾豐贄贅蹙蹠蹣蹤轆轉轌邇醪醫釐鎬鎭\
鎰鎹鎺闔闕闖雙雜霤鞣鞦鞨鞫鞳顋顏餬餮馥騅騏髀鬆鬩魍魎魏鮸鮹鯀鯊鯎鯏鯐鯑鯒鵑鵝鵤鵥\
麌黠鼕鼬謹儳勸嚥嚬嚮壜壞壟嬾孼廬懶懷攀曠櫚櫟歠瀚瀝瀟瀧瀨爍牘犢獸獺疆疇癡礙穩簫簷\
簽繩繪繳繹羆羶羸羹臘艢艤艨藹藺藾蘆蘊蘋蘢蟶蟷蟾蠊蠍蠖襞襤襦覈證譌譎譏譖譚贇贊蹲蹶\
蹻蹼躇轎辭邊鏃鏈鏐鏖鏗鏘鏝鏤鏨關隴霪靡鞴韞餽餾饂騙鯔鯡鯢鯣鯤鯰鯱鰙鵯鵲鵺鶄鶇鶉鶍\
鶎麑麕黼龐類懲贈難嚴嚶壤孀孃孅寶巉懺懽攘曦朧櫨櫪櫱瀰瀲瀾爐犧獻瓏癢矍礫礬籃籌糯糲\
繻繼繽纃罌臙臚蘚蠑蠕襭覺觸譟譫譬譯贍躁躄躅轗辮醴醵釋鐃鐐鐓鐚闡霰飄饅饉騫騭騷驀鬪\
鰄鰆鰈鰉鰊鰌鰒鰓鰔鰕鰘鰚鶚鶤鶩鶪鶫鹹麵黥黨鼯齟齠齡齣儷儺嚼囀囁囂囃囈屬巍廱懼懾攝\
曩櫺櫻欅權歡殲爛瓔癧癨癩癪籒籔纈纊續纐罍臟艪蠡蠢襯覽譴譽贐贓贔躊躋辯醺鐫鐵鐶鐺闢\
闥霹飆饋饌饐饑饒騾驂驃驅髏鬘魑鰤鰥鰧鰮鰰鶲鶸鶺鶻鷁鷂鷄鷆麝黯齎齦齧欄儻儼巒巓彎懿\
攢攤灑疊癬癭禳穰竊籙籟籡糴罎羇聽艫蘿襴襷覿讀贖躑躓躔轢鑄霽霾顫饕驍驎驕鬚鬻鰾鱆鱇\
鷓鷙黐鼴齪齬龕響巖戀攣攫曬欒癰籤籥纓纔纖罐蠱變讌讐轤邏鑛鑞鑠鑢靨顬顯驗驛髑髓體鬟\
鱏鱓鱚鱛鷦鷭鷯鷸黴齏囑屭攬癲纛羈艷蠹衢觀讒讓讖讙軈釀鑪靂靄靆靈顰驟鬢魘鱟鱠鱧鱩鷽\
鹽鼇齲齶齷廳欖灣籬糶纘臠蠻躡鑰靉顱鬣黌鼈矚蠶躪鑷顴驢驥鬮鱲鱶黶纜鑼鑽鑾顳驤驩鱸黷\
癴鑿钁鸛爨驪鸞麤";

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
        assert!(LEVEL_10.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_09.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_08.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_07.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_06.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_05.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_04.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_03.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_02_PRE.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_02.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_01_PRE.chars().all(|c| is_kanji(&c)));
        assert!(LEVEL_01.chars().all(|c| is_kanji(&c)));
    }

    #[test]
    fn kanji_list_lengths() {
        assert_eq!(80, LEVEL_10.chars().count());
        assert_eq!(160, LEVEL_09.chars().count());
        assert_eq!(200, LEVEL_08.chars().count());
        assert_eq!(202, LEVEL_07.chars().count());
        assert_eq!(193, LEVEL_06.chars().count());
        assert_eq!(191, LEVEL_05.chars().count());
        assert_eq!(313, LEVEL_04.chars().count());
        assert_eq!(284, LEVEL_03.chars().count());
        assert_eq!(328, LEVEL_02_PRE.chars().count());
        assert_eq!(185, LEVEL_02.chars().count());
        assert_eq!(1238, LEVEL_01_PRE.chars().count());
        assert_eq!(2928, LEVEL_01.chars().count());
    }

    #[test]
    fn elementary() {
        let ks = vec![LEVEL_10, LEVEL_09, LEVEL_08, LEVEL_07, LEVEL_06, LEVEL_05];

        assert_eq!(1026, ks.concat().chars().count());
    }

    #[test]
    fn jouyou() {
        let ks = vec![
            LEVEL_10,
            LEVEL_09,
            LEVEL_08,
            LEVEL_07,
            LEVEL_06,
            LEVEL_05,
            LEVEL_04,
            LEVEL_03,
            LEVEL_02_PRE,
            LEVEL_02,
        ];

        assert_eq!(2136, ks.concat().chars().count());
    }

    #[test]
    fn test_by_level() {
        assert_eq!(2136, by_level().len());
    }
}
