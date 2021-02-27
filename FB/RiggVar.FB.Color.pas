unit RiggVar.FB.Color;

interface

{ I wanted a custom set of unique color values,
    which includes web colors (all) and a few custom colors.
  All colors shall support 'picking by name' in the IDE code editor.
  https://en.delphipraxis.net/topic/4412-about-medgray-and-windowwhite
  Special color listbox components serve as 'proof of concept'.
  }

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes;

const
  WebColorCount = 141;
  UniqueWebColorCount = WebColorCount - 2;
  CustomColorCount = 16;
  ColorMapCount = UniqueWebColorCount + CustomColorCount;

type
  TRggColor = TAlphaColor;

  TRggColorKind = (
    WebColor,
    CustomColor,
    Unknown
  );

  TRggColorGroup = (
    CombinedGroup,
    PinkGroup,
    PurpleGroup,
    RedGroup,
    OrangeGroup,
    YellowGroup,
    BrownGroup,
    GreenGroup,
    CyanGroup,
    BlueGroup,
    WhiteGroup,
    GrayGroup,
    CustomGroup
  );

  TWebColorGroupSet = set of TRggColorGroup;

  TRggWebColors = class
  public
    const
    Aliceblue = claAliceblue;
    Antiquewhite = claAntiquewhite;
//    Aqua = claAqua; // duplicate of Cyan
    Aquamarine = claAquamarine;
    Azure = claAzure;
    Beige = claBeige;
    Bisque = claBisque;
    Black = claBlack;
    Blanchedalmond = claBlanchedalmond;
    Blue = claBlue;
    Blueviolet = claBlueviolet;
    Brown = claBrown;
    Burlywood = claBurlywood;
    Cadetblue = claCadetblue;
    Chartreuse = claChartreuse;
    Chocolate = claChocolate;
    Coral = claCoral;
    Cornflowerblue = claCornflowerBlue;
    Cornsilk = claCornsilk;
    Crimson = claCrimson;
    Cyan = claCyan;
    Darkblue = claDarkblue;
    Darkcyan = claDarkcyan;
    Darkgoldenrod = claDarkgoldenrod;
    Darkgray = claDarkgray;
    Darkgreen = claDarkgreen;
    Darkkhaki = claDarkkhaki;
    Darkmagenta = claDarkmagenta;
    Darkolivegreen = claDarkolivegreen;
    Darkorange = claDarkorange;
    Darkorchid = claDarkorchid;
    Darkred = claDarkred;
    Darksalmon = claDarksalmon;
    Darkseagreen = claDarkseagreen;
    Darkslateblue = claDarkslateblue;
    Darkslategray = claDarkslategray;
    Darkturquoise = claDarkturquoise;
    Darkviolet = claDarkviolet;
    Deeppink = claDeeppink;
    Deepskyblue = claDeepskyblue;
    Dimgray = claDimgray;
    Dodgerblue = claDodgerblue;
    Firebrick = claFirebrick;
    Floralwhite = claFloralwhite;
    Forestgreen = claForestgreen;
//    Fuchsia = claFuchsia; // duplicate of Magenta
    Gainsboro = claGainsboro;
    Ghostwhite = claGhostwhite;
    Gold = claGold;
    Goldenrod = claGoldenrod;
    Gray = claGray;
    Green = claGreen;
    Greenyellow = claGreenyellow;
    Honeydew = claHoneydew;
    Hotpink = claHotpink;
    Indianred = claIndianred;
    Indigo = claIndigo;
    Ivory = claIvory;
    Khaki = claKhaki;
    Lavender = claLavender;
    Lavenderblush = claLavenderblush;
    Lawngreen = claLawngreen;
    Lemonchiffon = claLemonchiffon;
    Lightblue = claLightblue;
    Lightcoral = claLightcoral;
    Lightcyan = claLightcyan;
    Lightgoldenrodyellow = claLightgoldenrodyellow;
    Lightgray = claLightgray;
    Lightgreen = claLightgreen;
    Lightpink = claLightpink;
    Lightsalmon = claLightsalmon;
    Lightseagreen = claLightseagreen;
    Lightskyblue = claLightskyblue;
    Lightslategray = claLightslategray;
    Lightsteelblue = claLightsteelblue;
    Lightyellow = claLightyellow;
    Lime = claLime;
    Limegreen = claLimegreen;
    Linen = claLinen;
    Magenta = claMagenta;
    Maroon = claMaroon;
    Mediumaquamarine = claMediumaquamarine;
    Mediumblue = claMediumblue;
    Mediumorchid = claMediumorchid;
    Mediumpurple = claMediumpurple;
    Mediumseagreen = claMediumseagreen;
    Mediumslateblue = claMediumslateblue;
    Mediumspringgreen = claMediumspringgreen;
    Mediumturquoise = claMediumturquoise;
    Mediumvioletred = claMediumvioletred;
    Midnightblue = claMidnightblue;
    Mintcream = claMintcream;
    Mistyrose = claMistyrose;
    Moccasin = claMoccasin;
    Navajowhite = claNavajowhite;
    Navy = claNavy;
    Oldlace = claOldlace;
    Olive = claOlive;
    Olivedrab = claOlivedrab;
    Orange = claOrange;
    Orangered = claOrangered;
    Orchid = claOrchid;
    Palegoldenrod = claPalegoldenrod;
    Palegreen = claPalegreen;
    Paleturquoise = claPaleturquoise;
    Palevioletred = claPalevioletred;
    Papayawhip = claPapayawhip;
    Peachpuff = claPeachpuff;
    Peru = claPeru;
    Pink = claPink;
    Plum = claPlum;
    Powderblue = claPowderblue;
    Purple = claPurple;
    Rebeccapurple = TRggColor($FF663399);
    Red = claRed;
    Rosybrown = claRosybrown;
    Royalblue = claRoyalblue;
    Saddlebrown = claSaddlebrown;
    Salmon = claSalmon;
    Sandybrown = claSandybrown;
    Seagreen = claSeagreen;
    Seashell = claSeashell;
    Sienna = claSienna;
    Silver = claSilver;
    Skyblue = claSkyblue;
    Slateblue = claSlateblue;
    Slategray = claSlategray;
    Snow = claSnow;
    Springgreen = claSpringgreen;
    Steelblue = claSteelblue;
    Tan = claTan;
    Teal = claTeal;
    Thistle = claThistle;
    Tomato = claTomato;
    Turquoise = claTurquoise;
    Violet = claViolet;
    Wheat = claWheat;
    White = claWhite;
    Whitesmoke = claWhitesmoke;
    Yellow = claYellow;
    Yellowgreen = claYellowgreen;
  end;

  TRggCustomColors = class(TRggWebColors)
  public
    const
    Alpha = claBlack;
    Null = claNull;

    Windowgray = TRggColor($FFF0F0F0);
    Porcelain = TRggColor($FFE0E0E0);
    Mercury = TRggColor($FFA0A0A0);

    BackgroundWhite = TRggColor($FFF9F9F9);
    BackgroundBlue = TRggColor($FF372E69);
    BackgroundGray = TRggColor($FF333333);

    MoneyGreen = TRggColor($FFC0DCC0);
    LegacySkyBlue = TRggColor($FFA6CAF0);
    Cream = TRggColor($FFFFFBF0);

    Gray35 = TRggColor($FFA5A5A5);
    Gray15 = TRggColor($FFD9D9D9);
    Gray05 = TRggColor($FFF2F2F2);

    Darkbrown = TRggColor($FF333300);
    Lightorange = TRggColor($FFFF9900);
    Sea = TRggColor($FF339966);
    Paleblue = TRggColor($FF99CCFF);

    { Alternative names may be used when assigning a color in code. }
    WindowWhite = Windowgray;
    BtnFace = Windowgray;
    ButtonFace = Windowgray;
    RectangleGray = Porcelain;
    QuickSilver = Mercury;
    MedGray = Mercury;
    MediumGray = Mercury;
    DarkSilver = TRggWebColors.Darkgray;
    Dovegray = TRggWebColors.Dimgray;

    Aqua = TRggWebColors.Cyan;
    Fuchsia = TRggWebColors.Magenta;

    Gray80 = BackgroundGray;
    Gray50 = TRggWebColors.Gray;
    Gray25 = TRggWebColors.Silver;
  end;

  TRggColorMapEntry = record
    Kind: TRggColorKind;
    IndexA: Integer;
    IndexN: Integer;
    Group: TRggColorGroup;
    Value: Cardinal;
    Name: string;
    class function Create(AKind: TRggColorKind; AValue: Cardinal; const AName: string): TRggColorMapEntry; static;
    class function ColorKindToString(Value: TRggColorKind): string; static;
    class function ColorKindToChar(Value: TRggColorKind): char; static;
    class function GetEmtpyMapEntry: TRggColorMapEntry; static;
  end;

const
  CustomColors: array [0..CustomColorCount-1] of TRggColorMapEntry = (
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Windowgray; Name: 'Windowgray'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Porcelain; Name: 'Porcelain'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Mercury; Name: 'Mercury'),

    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.BackgroundWhite; Name: 'BackgroundWhite'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.BackgroundBlue; Name: 'BackgroundBlue'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.BackgroundGray; Name: 'BackgroundGray'),

    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.MoneyGreen; Name: 'MoneyGreen'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.LegacySkyBlue; Name: 'LegacySkyBlue'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Cream; Name: 'Cream'),

    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Gray35; Name: 'Gray35'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Gray15; Name: 'Gray15'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Gray05; Name: 'Gray05'),

    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Darkbrown; Name: 'Darkbrown'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Lightorange; Name: 'Lightorange'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Sea; Name: 'Sea'),
    (Kind: CustomColor; Group: CustomGroup; Value: TRggCustomColors.Paleblue; Name: 'Paleblue')
  );

  WebColors: array [0..UniqueWebColorCount-1] of TRggColorMapEntry = (
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Aliceblue; Name: 'Aliceblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Antiquewhite; Name: 'Antiquewhite'),
//    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Aqua; Name: 'Aqua'), // see Cyan
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Aquamarine; Name: 'Aquamarine'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Azure; Name: 'Azure'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Beige; Name: 'Beige'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Bisque; Name: 'Bisque'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Black; Name: 'Black'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Blanchedalmond; Name: 'Blanchedalmond'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Blue; Name: 'Blue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Blueviolet; Name: 'Blueviolet'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Brown; Name: 'Brown'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Burlywood; Name: 'Burlywood'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Cadetblue; Name: 'Cadetblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Chartreuse; Name: 'Chartreuse'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Chocolate; Name: 'Chocolate'),
    (Kind: WebColor; Group: OrangeGroup; Value: TRggWebColors.Coral; Name: 'Coral'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Cornflowerblue; Name: 'Cornflowerblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Cornsilk; Name: 'Cornsilk'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Crimson; Name: 'Crimson'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Cyan; Name: 'Cyan'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Darkblue; Name: 'Darkblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Darkcyan; Name: 'Darkcyan'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Darkgoldenrod; Name: 'Darkgoldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Darkgray; Name: 'Darkgray'), // DarkSilver
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Darkgreen; Name: 'Darkgreen'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Darkkhaki; Name: 'Darkkhaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Darkmagenta; Name: 'Darkmagenta'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Darkolivegreen; Name: 'Darkolivegreen'),
    (Kind: WebColor; Group: OrangeGroup; Value: TRggWebColors.Darkorange; Name: 'Darkorange'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Darkorchid; Name: 'Darkorchid'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Darkred; Name: 'Darkred'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Darksalmon; Name: 'Darksalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Darkseagreen; Name: 'Darkseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Darkslateblue; Name: 'Darkslateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Darkslategray; Name: 'Darkslategray'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Darkturquoise; Name: 'Darkturquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Darkviolet; Name: 'Darkviolet'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Deeppink; Name: 'Deeppink'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Deepskyblue; Name: 'Deepskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Dimgray; Name: 'Dimgray'), // DoveGray
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Dodgerblue; Name: 'Dodgerblue'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Firebrick; Name: 'Firebrick'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Floralwhite; Name: 'Floralwhite'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Forestgreen; Name: 'Forestgreen'),
//    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Fuchsia; Name: 'Fuchsia'), // see Magenta
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Gainsboro; Name: 'Gainsboro'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Ghostwhite; Name: 'Ghostwhite'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Gold; Name: 'Gold'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Goldenrod; Name: 'Goldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Gray; Name: 'Gray'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Green; Name: 'Green'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Greenyellow; Name: 'Greenyellow'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Honeydew; Name: 'Honeydew'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Hotpink; Name: 'Hotpink'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Indianred; Name: 'Indianred'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Indigo; Name: 'Indigo'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Ivory; Name: 'Ivory'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Khaki; Name: 'Khaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Lavender; Name: 'Lavender'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Lavenderblush; Name: 'Lavenderblush'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Lawngreen; Name: 'Lawngreen'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Lemonchiffon; Name: 'Lemonchiffon'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Lightblue; Name: 'Lightblue'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Lightcoral; Name: 'Lightcoral'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Lightcyan; Name: 'Lightcyan'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Lightgoldenrodyellow; Name: 'Lightgoldenrodyellow'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Lightgray; Name: 'Lightgray'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Lightgreen; Name: 'Lightgreen'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Lightpink; Name: 'Lightpink'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Lightsalmon; Name: 'Lightsalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Lightseagreen; Name: 'Lightseagreen'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Lightskyblue; Name: 'Lightskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Lightslategray; Name: 'Lightslategray'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Lightsteelblue; Name: 'Lightsteelblue'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Lightyellow; Name: 'Lightyellow'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Lime; Name: 'Lime'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Limegreen; Name: 'Limegreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Linen; Name: 'Linen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Magenta; Name: 'Magenta'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Maroon; Name: 'Maroon'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Mediumaquamarine; Name: 'Mediumaquamarine'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Mediumblue; Name: 'Mediumblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Mediumorchid; Name: 'Mediumorchid'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Mediumpurple; Name: 'Mediumpurple'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Mediumseagreen; Name: 'Mediumseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Mediumslateblue; Name: 'Mediumslateblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Mediumspringgreen; Name: 'Mediumspringgreen'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Mediumturquoise; Name: 'Mediumturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Mediumvioletred; Name: 'Mediumvioletred'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Midnightblue; Name: 'Midnightblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Mintcream; Name: 'Mintcream'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Mistyrose; Name: 'Mistyrose'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Moccasin; Name: 'Moccasin'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Navajowhite; Name: 'Navajowhite'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Navy; Name: 'Navy'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Oldlace; Name: 'Oldlace'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Olive; Name: 'Olive'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Olivedrab; Name: 'Olivedrab'),
    (Kind: WebColor; Group: OrangeGroup; Value: TRggWebColors.Orange; Name: 'Orange'),
    (Kind: WebColor; Group: OrangeGroup; Value: TRggWebColors.Orangered; Name: 'Orangered'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Orchid; Name: 'Orchid'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Palegoldenrod; Name: 'Palegoldenrod'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Palegreen; Name: 'Palegreen'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Paleturquoise; Name: 'Paleturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Palevioletred; Name: 'Palevioletred'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Papayawhip; Name: 'Papayawhip'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Peachpuff; Name: 'Peachpuff'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Peru; Name: 'Peru'),
    (Kind: WebColor; Group: PinkGroup; Value: TRggWebColors.Pink; Name: 'Pink'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Plum; Name: 'Plum'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Powderblue; Name: 'Powderblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Purple; Name: 'Purple'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Red; Name: 'Red'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Rebeccapurple; Name: 'Rebeccapurple'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Rosybrown; Name: 'Rosybrown'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Royalblue; Name: 'Royalblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Saddlebrown; Name: 'Saddlebrown'),
    (Kind: WebColor; Group: RedGroup; Value: TRggWebColors.Salmon; Name: 'Salmon'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Sandybrown; Name: 'Sandybrown'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Seagreen; Name: 'Seagreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Seashell; Name: 'Seashell'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Sienna; Name: 'Sienna'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Silver; Name: 'Silver'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Skyblue; Name: 'Skyblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Slateblue; Name: 'Slateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TRggWebColors.Slategray; Name: 'Slategray'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Snow; Name: 'Snow'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Springgreen; Name: 'Springgreen'),
    (Kind: WebColor; Group: BlueGroup; Value: TRggWebColors.Steelblue; Name: 'Steelblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Tan; Name: 'Tan'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Teal; Name: 'Teal'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Thistle; Name: 'Thistle'),
    (Kind: WebColor; Group: OrangeGroup; Value: TRggWebColors.Tomato; Name: 'Tomato'),
    (Kind: WebColor; Group: CyanGroup; Value: TRggWebColors.Turquoise; Name: 'Turquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: TRggWebColors.Violet; Name: 'Violet'),
    (Kind: WebColor; Group: BrownGroup; Value: TRggWebColors.Wheat; Name: 'Wheat'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.White; Name: 'White'),
    (Kind: WebColor; Group: WhiteGroup; Value: TRggWebColors.Whitesmoke; Name: 'Whitesmoke'),
    (Kind: WebColor; Group: YellowGroup; Value: TRggWebColors.Yellow; Name: 'Yellow'),
    (Kind: WebColor; Group: GreenGroup; Value: TRggWebColors.Yellowgreen; Name: 'Yellowgreen')
  );

type
  TRggGetColorInfoProc = procedure(
    i: Integer;
    k: TRggColorKind;
    c: TRggColor;
    const s: string) of object;

  TRggColorIndexCallback = procedure(Value: TRggColor);

  TRggColorPool = class(TRggCustomColors)
  strict private
    class var TempIndexN: Integer;
    class procedure InitColorMap;
    class function CardinalToIdent(Int: Cardinal; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
    class function IdentToCardinal(const Ident: string; var Int: Cardinal; const Map: array of TRggColorMapEntry): Boolean;
    class function ColorToIdent(Color: Cardinal; var Ident: string): Boolean;
  private
    class function GetCount: Integer; static;
    class procedure ColorIndexCallback(Value: TRggColor); static;
    class procedure EnumeratePartition(Proc: TRggColorIndexCallback); static;
  protected
    class function ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
    class function GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
  public
    class var ColorMap: array of TRggColorMapEntry;
    class constructor Create;

    class function ColorFromRGB(R, G, B: Byte): TRggColor; static;
    class function ColorFromName(const s: string): TRggColor; static;

    class procedure EnumerateColors(Proc: TRggGetColorInfoProc); static;
    class procedure UpdateColorName(c: TRggColor; s: string); static;

    class function ColorToString(Value: TRggColor): string; static;
    class function ColorToKind(Value: TRggColor): TRggColorKind; static;
    class function ColorToGroup(Value: TRggColor): TRggColorGroup; static;
    class function ColorGroupToGroupName(g: TRggColorGroup): string; static;
    class function ColorToGroupName(Value: TRggColor): string; static;
    class function ColorToIndexA(Value: TRggColor): Integer; static;
    class function ColorToIndexN(Value: TRggColor): Integer; static;
    class function GetColorKindString(Value: TRggColor): string; static;

    class function GetColorIndex(Value: TRggColor): Integer; static;
    class function LookupRowForIndexN(IdxN: Integer): Integer; static;

    class procedure UpdateIndexN; static;
    class procedure UpdateColorNames; static;
    class procedure RevertColorNames; static;
    class property Count: Integer read GetCount;
  end;

  TRggColors = class(TRggCustomColors);

  TRggGrayColors = record
  const
    Whitesmoke = TRggWebColors.Whitesmoke;
    Windowgray = TRggCustomColors.Windowgray;
    Porcelain = TRggCustomColors.Porcelain;
    Gainsboro = TRggWebColors.Gainsboro;
    Lightgray = TRggWebColors.Lightgray;
    Silver = TRggWebColors.Silver;
    DarkSilver = TRggCustomColors.DarkSilver;
    Mercury = TRggCustomColors.Mercury;
    Gray = TRggWebColors.Gray;
    Dovegray = TRggWebColors.Dimgray;
  end;

  TRggPercentGrayColors = record
  const
    Gray80 = TRggCustomColors.BackgroundGray; // 33
    Gray50 = TRggWebColors.Gray; // 80
    Gray35 = TRggCustomColors.Gray35; // A5
    Gray25 = TRggWebColors.Silver; // C0
    Gray15 = TRggCustomColors.Gray15; // D9
    Gray05 = TRggCustomColors.Gray05; // F2
  end;

implementation

uses
  RiggVar.FB.ColorGroup;

{ TRggColorMapEntry }

class function TRggColorMapEntry.Create(
  AKind: TRggColorKind;
  AValue: Cardinal;
  const AName: string): TRggColorMapEntry;
begin
  result.Kind := AKind;
  result.Value := AValue;
  result.Name := aName;
end;

class function TRggColorMapEntry.GetEmtpyMapEntry: TRggColorMapEntry;
begin
  result.Name := '-';
  result.Kind := TRggColorKind.Unknown;
  result.Value := 0;
end;

class function TRggColorMapEntry.ColorKindToChar(Value: TRggColorKind): char;
begin
  case Value of
    WebColor: result := 'W';
    CustomColor: result := 'C';
    Unknown: result := 'U';
    else result := '-';
  end;
end;

class function TRggColorMapEntry.ColorKindToString(Value: TRggColorKind): string;
begin
  case Value of
    WebColor: result := 'Web';
    CustomColor: result := 'Custom';
    Unknown: result := 'Unknown';
    else result := '-';
  end;
end;

{ TRggColorPool }

class constructor TRggColorPool.Create;
begin
  InitColorMap;
end;

class function TRggColorPool.ColorFromName(const s: string): TRggColor;
var
  i: Cardinal;
begin
  if IdentToCardinal(s, i, ColorMap) then
    result := TRggColor(i)
  else
    result := TRggWebColors.White;
end;

class function TRggColorPool.ColorFromRGB(R, G, B: Byte): TRggColor;
var
  acr: TAlphaColorRec;
begin
  acr.R := R;
  acr.G := G;
  acr.B := B;
  acr.A := 255;
  result := acr.Color;
end;

class function TRggColorPool.ColorToIdent(Color: Cardinal; var Ident: string): Boolean;
begin
  result := CardinalToIdent(Color, Ident, ColorMap);
  if not result then
  begin
    AlphaColorToIdent(Color, Ident); // return value ignored
  end;
end;

class function TRggColorPool.ColorToString(Value: TRggColor): string;
begin
  ColorToIdent(Value, Result);
end;

class function TRggColorPool.ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Value then
    begin
      MapEntry := ColorMap[i];
      Result := I;
      Exit;
    end;
end;

class function TRggColorPool.GetColorKindString(Value: TRggColor): string;
begin
  result := TRggColorMapEntry.ColorKindToString(GetColorMapEntry(Value).Kind);
end;

class function TRggColorPool.GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
var
  I: Integer;
begin
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Value then
    begin
      result := ColorMap[i];
      Exit;
    end;
  result := TRggColorMapEntry.GetEmtpyMapEntry;
end;

class function TRggColorPool.ColorToIndexA(Value: TRggColor): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Value then
    begin
      result := I;
      Assert(I = ColorMap[I].IndexA);
      Exit;
    end;
  result := -1;
end;

class function TRggColorPool.ColorToIndexN(Value: TRggColor): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Value then
    begin
      result := ColorMap[I].IndexN;
      Exit;
    end;
  result := -1;
end;

class function TRggColorPool.LookupRowForIndexN(IdxN: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ColorMap)-1 do
    if IdxN = ColorMap[I].IndexN then
    begin
      result := ColorMap[i].IndexA;
      Exit;
    end;
  result := -1;
end;

class function TRggColorPool.GetColorIndex(Value: TRggColor): Integer;
begin
  result := LookupRowForIndexN(ColorToIndexA(Value));
end;

class function TRggColorPool.GetCount: Integer;
begin
  result := Length(ColorMap);
end;

class function TRggColorPool.ColorToKind(Value: TRggColor): TRggColorKind;
var
  cme: TRggColorMapEntry;
begin
  if ColorToMapEntry(Value, cme) > -1 then
    result := cme.Kind
  else
    result := TRggColorKind.Unknown;
end;

class procedure TRggColorPool.EnumerateColors(Proc: TRggGetColorInfoProc);
var
  i: Integer;
begin
  for i := 0 to Length(ColorMap)-1 do
  begin
    Proc(i, ColorMap[i].Kind, ColorMap[i].Value, ColorMap[i].Name);
  end;
end;

class function TRggColorPool.IdentToCardinal(const Ident: string; var Int: Cardinal; const Map: array of TRggColorMapEntry): Boolean;
var
  I: Integer;
begin
  for I := Low(Map) to High(Map) do
    if SameText(Map[I].Name, Ident) then
    begin
      Result := True;
      Int := Map[I].Value;
      Exit;
    end;
  Result := False;
end;

class function TRggColorPool.CardinalToIdent(Int: Cardinal; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
var
  I: Integer;
begin
  for I := Low(Map) to High(Map) do
    if Map[I].Value = Int then
    begin
      Result := True;
      Ident := Map[I].Name;
      Exit;
    end;
  Result := False;
end;

class procedure TRggColorPool.InitColorMap;
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  SetLength(ColorMap, High(WebColors) + High(CustomColors) + 2);

  Assert(Low(WebColors) = 0);
  Assert(High(WebColors) = UniqueWebColorCount-1);
  Assert(ColorMapCount >= UniqueWebColorCount);

  { web colors }
  for i := 0 to High(WebColors) do
  begin
    ColorMap[i] := WebColors[i];
    ColorMap[i].IndexA := i;
  end;

  { custom colors }
  k := High(WebColors);
  for i := 0 to High(CustomColors) do
  begin
    j := k + 1 + i;
    ColorMap[j] := CustomColors[i];
    ColorMap[j].IndexA := j;
  end;

  UpdateIndexN;
end;

class procedure TRggColorPool.UpdateColorName(c: TRggColor; s: string);
var
  i: Integer;
begin
  i := ColorToIndexA(c);
  if i > -1 then
    ColorMap[i].Name := s;
end;

class procedure TRggColorPool.UpdateColorNames;
begin
  { update ColorMap with custom names }
  UpdateColorName(Darkgray, 'DarkSilver');
  UpdateColorName(Dimgray, 'Dovegray');

  UpdateColorName(Windowgray, 'WindowWhite');
  UpdateColorName(Porcelain, 'RectangleGray');
  UpdateColorName(Mercury, 'QuickSilver');
end;

class procedure TRggColorPool.RevertColorNames;
begin
  { revert to original web color names }
  UpdateColorName(Darkgray, 'Darkgray');
  UpdateColorName(Dimgray, 'Dimgray');

  { revert to self chosen 'default' names for custom colors }
  UpdateColorName(Windowgray, 'Windowgray');
  UpdateColorName(Porcelain, 'Porcelain');
  UpdateColorName(Mercury, 'Mercury');
end;

class function TRggColorPool.ColorToGroup(Value: TRggColor): TRggColorGroup;
var
  I: Integer;
begin
  result := CombinedGroup;
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Value then
    begin
      Result := ColorMap[I].Group;
      Exit;
    end;
end;

class function TRggColorPool.ColorGroupToGroupName(g: TRggColorGroup): string;
begin
  result := '';
  case g of
    CombinedGroup: result := 'Combined';
    PinkGroup: result := 'Pink';
    PurpleGroup: result := 'Purple';
    RedGroup: result := 'Red';
    OrangeGroup: result := 'Orange';
    YellowGroup: result := 'Yellow';
    BrownGroup: result := 'Brown';
    GreenGroup: result := 'Green';
    CyanGroup: result := 'Cyan';
    BlueGroup: result := 'Blue';
    WhiteGroup: result := 'White';
    GrayGroup: result := 'Gray';
    CustomGroup: result := 'Custom';
  end;
  result := result + 'Group';
end;

class function TRggColorPool.ColorToGroupName(Value: TRggColor): string;
var
  g: TRggColorGroup;
begin
  g := ColorToGroup(Value);
  result := ColorGroupToGroupName(g);
end;

class procedure TRggColorPool.UpdateIndexN;
begin
  TempIndexN := 0;
  EnumeratePartition(ColorIndexCallback);
end;

class procedure TRggColorPool.EnumeratePartition(Proc: TRggColorIndexCallback);
var
  pinkE: TRggPinkWebColorEnum;
  purpleE: TRggPurpleWebColorEnum;
  redE: TRggRedWebColorEnum;
  orangeE: TRggOrangeWebColorEnum;
  yellowE: TRggYellowWebColorEnum;
  brownE: TRggBrownWebColorEnum;
  greenE: TRggGreenWebColorEnum;
  cyanE: TRggCyanWebColorEnum;
  blueE: TRggBlueWebColorEnum;
  whiteE: TRggWhiteWebColorEnum;
  grayE: TRggGrayWebColorEnum;
  customE: TRggCustomColorEnum;
begin
  for pinkE := Low(pinkE) to High(pinkE) do
    Proc(PinkWebColorArray[pinkE]);

  for purpleE := Low(purpleE) to High(purpleE) do
    Proc(PurpleWebColorArray[purpleE]);

  for redE := Low(redE) to High(redE) do
    Proc(RedWebColorArray[redE]);

  for orangeE := Low(orangeE) to High(orangeE) do
    Proc(OrangeWebColorArray[orangeE]);

  for yellowE := Low(yellowE) to High(yellowE) do
    Proc(YellowWebColorArray[yellowE]);

  for brownE := Low(brownE) to High(brownE) do
    Proc(BrownWebColorArray[brownE]);

  for greenE := Low(greenE) to High(greenE) do
    Proc(GreenWebColorArray[greenE]);

  for cyanE := Low(cyanE) to High(cyanE) do
    Proc(CyanWebColorArray[cyanE]);

  for blueE := Low(blueE) to High(blueE) do
    Proc(BlueWebColorArray[blueE]);

  for whiteE := Low(whiteE) to High(whiteE) do
    Proc(WhiteWebColorArray[whiteE]);

  for grayE := Low(grayE) to High(grayE) do
    Proc(GrayWebColorArray[grayE]);

  for customE := Low(customE) to High(customE) do
    Proc(CustomColorArray[customE]);
end;

class procedure TRggColorPool.ColorIndexCallback(Value: TRggColor);
var
  cme: TRggColorMapEntry;
begin
  if TRggColorPool.ColorToMapEntry(Value, cme) > -1 then
  begin
    ColorMap[TempIndexN].IndexN := cme.IndexA;
  end;
  Inc(TempIndexN);
end;

end.
