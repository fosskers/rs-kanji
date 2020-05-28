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
不争井付令以仲伝位低佐例便信倉候借健側働\
億兆児共兵典冷初別利刷副功加努労勇包卒協\
単博印参司各周唱器固城埼塩変夫失奈好媛季\
孫完官害富察岐岡崎巣差希席帯底府康建径徒\
徳必念愛成戦折挙改敗散料旗昨景最望未末札\
材束松果栃栄案梅梨械極標機欠残氏民求沖治\
法泣浅浴清満滋漁潟灯無然焼照熊熱牧特産的\
省祝票種積競笑管節約結給続縄置群老臣良芸\
芽英茨菜街衣要覚観訓試説課議貨賀軍輪辞辺\
連達選郡量録鏡関阜阪陸隊静順願類飛飯養香\
験鹿";

/// These are learned in the 5th year of Japanese elementary school.
///
/// ```
/// assert_eq!(193, kanji::LEVEL_06.chars().count());
/// ```
pub const LEVEL_06: &str = "\
久仏仮件任似余価保修個停備像再刊判制則効\
務勢厚句可史告喜営因団囲圧在均型基堂報境\
墓増士夢妻婦容寄導居属布師常幹序弁張往得\
復志応快性情態慣技招授採接提損支政故救断\
旧易暴条枝査格桜検構武歴殺毒比永河液混減\
測準演潔災燃版犯状独率現留略益眼破確示祖\
禁移程税築粉精紀素経統絶綿総編績織罪義耕\
職肥能脈興舎航術衛製複規解設許証評講謝識\
護豊象財貧責貯貸費貿資賛賞質輸述迷逆造過\
適酸鉱銅防限険際雑非領額飼";

/// These are learned in the 6th year of Japanese elementary school, the final
/// year before middle school.
///
/// 1,026 Kanji are learned by this point.
///
/// ```
/// assert_eq!(191, kanji::LEVEL_05.chars().count());
/// ```
pub const LEVEL_05: &str = "\
並乱乳亡仁供俳俵値傷優党冊処券刻割創劇勤\
危卵厳収后否吸呼善困垂域奏奮姿存孝宅宇宗\
宙宝宣密寸専射将尊就尺届展層己巻幕干幼庁\
座延律従忘忠恩憲我批承担拝拡捨探推揮操敬\
敵映晩暖暮朗机枚染株棒模権樹欲段沿泉洗派\
済源潮激灰熟片班異疑痛皇盛盟看砂磁私秘穀\
穴窓筋策簡糖系紅納純絹縦縮署翌聖肺胃背胸\
脳腸腹臓臨至舌若著蒸蔵蚕衆裁装裏補視覧討\
訪訳詞誌認誕誠誤論諸警貴賃退遺郵郷針銭鋼\
閉閣降陛除障難革頂預骨";

/// These are learned in middle school.
///
/// ```
/// assert_eq!(313, kanji::LEVEL_04.chars().count());
/// ```
pub const LEVEL_04: &str = "\
丈与丘丹乾互介仰伺依侵俗倒偉傍傾僧儀兼冒\
凡凶刈到刺剣剤劣勧匹占即却及叫召吐含吹咲\
唐嘆噴圏坊執堅堤塔壁壊壱奇奥奴妙姓威娘婚\
寂寝尋尽尾屈峠峰巡巨帽幅幾床弐弾彩影彼征\
御微徴忙怒怖恋恐恒恥恵悩惑惨慎慢慮憶戒戯\
扇払扱抗抜抱抵押拍拓拠振捕掘描握援搬摘撃\
攻敏敷斜旨旬是普暇暦曇更替朱朽杯枯柄柔桃\
欄歓歳殖殿汗汚沈沢沼況泊浜浮浸涙淡添渡溶\
滴漫澄濁濃為烈煙煮燥爆狂狩狭猛獣獲玄珍環\
甘畳疲療皆盆盗監盤盾眠瞬矛砲祈秀称稲稿突\
端箇範粒紋紫紹絡継維網緯縁繁繰罰翼耐肩肪\
胴脂脚脱腐腕腰膚致舗舞舟般芋芝茂荒菓蓄薄\
薪被襲触訴詰詳誇誉謡豪販賦贈越趣距跡跳踊\
踏躍軒較載輝輩込迎迫逃透途遅違遣避郎釈鈍\
鉛鋭鎖鑑闘陣陰隠隣隷雄雅雌離雷需震霧露響\
項頼飾駆騒驚髪鬼鮮麗黙鼓齢";

/// These are learned by the end of middle school.
///
/// ```
/// assert_eq!(284, kanji::LEVEL_03.chars().count());
/// ```
pub const LEVEL_03: &str = "\
乏乙了企伏伐伴伸佳侍促倣倹偶催債克免冗冠\
凍凝刑削励勘募匠匿卑卓卸厘又双吉吏哀哲啓\
喚喫嘱坑埋塊塗墜墨墳墾壇奉契奪如妨姫娯婆\
婿嫁嬢孔孤宴審寿封尿岳峡崩巧帆帝幻幽廉廊\
弧彫徐忌怠怪恨悔悟悦惜愚慈慌慕慨慰憂憎憩\
房抑択抽拘掃掌排掛控措掲揚換揺携搾摂撮擁\
擦敢斗斤斥施既昇晶暫架某桑棄棋楼概欧欺殊\
殴没泌浪湾湿滅滑滝滞漂漏潜潤濫瀬炉炊炎焦\
牲犠猟獄甲畔畜疾痘癖硬碑礎祉稚穂穏穫窒符\
篤簿籍粋粗粘糧紛紺絞綱緊締緩縛縫繕翻聴肝\
胆胎胞脅膜膨芳苗菊華葬藩虐虚蛮衝衰袋裂裸\
覆訂託詠該誘請諮諾謀譲豚貫賊賢赦赴超軌軸\
辛辱逮遂遇遭遵邦邪郊郭酔酵鋳錠錬錯鍛鎮鐘\
閲阻陪陳陵陶隆随隔隻雇零霊顧飽餓駐騎髄魂\
魅魔鯨鶏";

/// These are learned by the end of high school.
///
/// ```
/// assert_eq!(328, kanji::LEVEL_02P.chars().count());
/// ```
pub const LEVEL_02P: &str = "\
且丙亜享亭仙伯但併侮侯俊俸倫偏偵偽傑傘僕\
僚儒償充准凸凹刃剖剛剰劾勅勲升厄叔叙吟呈\
呉唆唇唯喝喪嗣嚇囚坪垣培堀堕堪塀塁塑塚塾\
壌壮奔奨妃妄妊妥姻娠媒嫌嫡宜宰宵寛寡寧寮\
尉尚尼履屯岬崇帥幣庶庸廃廷弊弔弦彰循徹忍\
恭悠患悼惰愁愉慶憤憾懇懐懲懸戻扉扶抄把披\
抹拐拒拙括拷挑挟挿捜据搭摩撤撲擬斉斎旋昆\
暁曹朕朴杉析枠枢柳栓核栽桟棚棟棺槽款殉殻\
汁江沸泡泥泰洞津洪浄浦涯涼淑渇渉渋渓渦溝\
漆漠漬漸濯煩爵猫献猶猿珠琴璽瓶甚畝疎疫症\
痢痴癒盲眺睡督矯砕硝硫碁磨礁祥禅禍租秩稼\
窃窮窯竜筒粛粧糾紡索累紳緒繊繭缶罷羅翁耗\
肌肖肢肯臭舶艇艦茎荘菌薦薫藻虜虞蚊蛇蛍融\
衡衷裕褐褒襟覇訟診詐詔誓諭謁謄謙謹譜貞貢\
賄賓賜賠購践軟轄迅迭逐逓逝逸遍遮遷還邸酌\
酢酪酬酷醜醸釣鈴鉢銃銘閑閥附陥隅雰霜靴韻\
頑頒頻顕飢駄騰麻";

/// These are considered "standard adult level".
///
/// ```
/// assert_eq!(185, kanji::LEVEL_02.chars().count());
/// ```
pub const LEVEL_02: &str = "\
串丼乞亀伎侶俺傲僅冥冶凄刹剥勃勾匂叱呂呪\
咽哺唄唾喉喩嗅嘲堆塞填妖妬嫉宛尻崖嵐巾弄\
弥彙怨恣惧慄憧憬戚戴拉拭拳拶挨挫捉捗捻摯\
斑斬旦旺昧曖曽枕柵柿桁梗椅椎楷毀氾汎汰沃\
沙淫湧溺潰煎爪爽牙狙玩瑠璃璧瓦畏畿痕痩瘍\
眉睦瞭瞳稽窟箋箸籠綻緻罵羞羨肘股脇脊腎腫\
腺膝膳臆臼舷艶芯苛萎葛蓋蔑蔽藍藤虎虹蜂蜜\
袖裾訃詣詮誰諦諧謎貌貪貼賂賭踪蹴辣遜遡那\
酎醒采釜錦錮鍋鍵鎌闇隙韓頃須頓頬顎餅餌駒\
骸鬱鶴麓麺";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elementary() {
        let ks = vec![LEVEL_10, LEVEL_09, LEVEL_08, LEVEL_07, LEVEL_06, LEVEL_05];

        assert_eq!(1026, ks.concat().chars().count());
    }

    #[test]
    fn jouyou() {
        let ks = vec![
            LEVEL_10, LEVEL_09, LEVEL_08, LEVEL_07, LEVEL_06, LEVEL_05, LEVEL_04, LEVEL_03,
            LEVEL_02P, LEVEL_02,
        ];

        assert_eq!(2136, ks.concat().chars().count());
    }
}
