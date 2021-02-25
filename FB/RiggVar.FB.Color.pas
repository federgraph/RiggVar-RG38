unit RiggVar.FB.Color;

interface

{ I wanted a unique set of color values,
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
    Value: Integer;
    Name: string;
    class function Create(AKind: TRggColorKind; AValue: Integer; const AName: string): TRggColorMapEntry; static;
    class function ColorKindToString(Value: TRggColorKind): string; static;
    class function ColorKindToChar(Value: TRggColorKind): char; static;
    class function GetEmtpyMapEntry: TRggColorMapEntry; static;
  end;

const
  CustomColors: array [0..CustomColorCount-1] of TRggColorMapEntry = (
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Windowgray); Name: 'Windowgray'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Porcelain); Name: 'Porcelain'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Mercury); Name: 'Mercury'),

    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.BackgroundWhite); Name: 'BackgroundWhite'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.BackgroundBlue); Name: 'BackgroundBlue'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.BackgroundGray); Name: 'BackgroundGray'),

    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.MoneyGreen); Name: 'MoneyGreen'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.LegacySkyBlue); Name: 'LegacySkyBlue'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Cream); Name: 'Cream'),

    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Gray35); Name: 'Gray35'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Gray15); Name: 'Gray15'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Gray05); Name: 'Gray05'),

    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Darkbrown); Name: 'Darkbrown'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Lightorange); Name: 'Lightorange'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Sea); Name: 'Sea'),
    (Kind: CustomColor; Group: CustomGroup; Value: Integer(TRggCustomColors.Paleblue); Name: 'Paleblue')
  );

  WebColors: array [0..UniqueWebColorCount-1] of TRggColorMapEntry = (
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Aliceblue); Name: 'Aliceblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Antiquewhite); Name: 'Antiquewhite'),
//    (Kind: WebColor; Group: TRggWebColorGroup.CyanGroup; Value: Integer(TRggWebColors.Aqua); Name: 'Aqua'), // see Cyan
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Aquamarine); Name: 'Aquamarine'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Azure); Name: 'Azure'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Beige); Name: 'Beige'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Bisque); Name: 'Bisque'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Black); Name: 'Black'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Blanchedalmond); Name: 'Blanchedalmond'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Blue); Name: 'Blue'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Blueviolet); Name: 'Blueviolet'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Brown); Name: 'Brown'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Burlywood); Name: 'Burlywood'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Cadetblue); Name: 'Cadetblue'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Chartreuse); Name: 'Chartreuse'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Chocolate); Name: 'Chocolate'),
    (Kind: WebColor; Group: OrangeGroup; Value: Integer(TRggWebColors.Coral); Name: 'Coral'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Cornflowerblue); Name: 'Cornflowerblue'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Cornsilk); Name: 'Cornsilk'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Crimson); Name: 'Crimson'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Cyan); Name: 'Cyan'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Darkblue); Name: 'Darkblue'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Darkcyan); Name: 'Darkcyan'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Darkgoldenrod); Name: 'Darkgoldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Darkgray); Name: 'Darkgray'), // DarkSilver
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Darkgreen); Name: 'Darkgreen'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Darkkhaki); Name: 'Darkkhaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Darkmagenta); Name: 'Darkmagenta'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Darkolivegreen); Name: 'Darkolivegreen'),
    (Kind: WebColor; Group: OrangeGroup; Value: Integer(TRggWebColors.Darkorange); Name: 'Darkorange'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Darkorchid); Name: 'Darkorchid'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Darkred); Name: 'Darkred'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Darksalmon); Name: 'Darksalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Darkseagreen); Name: 'Darkseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Darkslateblue); Name: 'Darkslateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Darkslategray); Name: 'Darkslategray'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Darkturquoise); Name: 'Darkturquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Darkviolet); Name: 'Darkviolet'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Deeppink); Name: 'Deeppink'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Deepskyblue); Name: 'Deepskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Dimgray); Name: 'Dimgray'), // DoveGray
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Dodgerblue); Name: 'Dodgerblue'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Firebrick); Name: 'Firebrick'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Floralwhite); Name: 'Floralwhite'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Forestgreen); Name: 'Forestgreen'),
//    (Kind: WebColor; Group: TRggWebColorGroup.PurpleGroup; Value: Integer(TRggWebColors.Fuchsia); Name: 'Fuchsia'), // see Magenta
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Gainsboro); Name: 'Gainsboro'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Ghostwhite); Name: 'Ghostwhite'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Gold); Name: 'Gold'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Goldenrod); Name: 'Goldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Gray); Name: 'Gray'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Green); Name: 'Green'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Greenyellow); Name: 'Greenyellow'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Honeydew); Name: 'Honeydew'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Hotpink); Name: 'Hotpink'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Indianred); Name: 'Indianred'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Indigo); Name: 'Indigo'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Ivory); Name: 'Ivory'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Khaki); Name: 'Khaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Lavender); Name: 'Lavender'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Lavenderblush); Name: 'Lavenderblush'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Lawngreen); Name: 'Lawngreen'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Lemonchiffon); Name: 'Lemonchiffon'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Lightblue); Name: 'Lightblue'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Lightcoral); Name: 'Lightcoral'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Lightcyan); Name: 'Lightcyan'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Lightgoldenrodyellow); Name: 'Lightgoldenrodyellow'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Lightgray); Name: 'Lightgray'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Lightgreen); Name: 'Lightgreen'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Lightpink); Name: 'Lightpink'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Lightsalmon); Name: 'Lightsalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Lightseagreen); Name: 'Lightseagreen'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Lightskyblue); Name: 'Lightskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Lightslategray); Name: 'Lightslategray'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Lightsteelblue); Name: 'Lightsteelblue'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Lightyellow); Name: 'Lightyellow'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Lime); Name: 'Lime'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Limegreen); Name: 'Limegreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Linen); Name: 'Linen'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Magenta); Name: 'Magenta'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Maroon); Name: 'Maroon'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Mediumaquamarine); Name: 'Mediumaquamarine'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Mediumblue); Name: 'Mediumblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Mediumorchid); Name: 'Mediumorchid'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Mediumpurple); Name: 'Mediumpurple'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Mediumseagreen); Name: 'Mediumseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Mediumslateblue); Name: 'Mediumslateblue'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Mediumspringgreen); Name: 'Mediumspringgreen'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Mediumturquoise); Name: 'Mediumturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Mediumvioletred); Name: 'Mediumvioletred'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Midnightblue); Name: 'Midnightblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Mintcream); Name: 'Mintcream'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Mistyrose); Name: 'Mistyrose'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Moccasin); Name: 'Moccasin'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Navajowhite); Name: 'Navajowhite'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Navy); Name: 'Navy'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Oldlace); Name: 'Oldlace'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Olive); Name: 'Olive'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Olivedrab); Name: 'Olivedrab'),
    (Kind: WebColor; Group: OrangeGroup; Value: Integer(TRggWebColors.Orange); Name: 'Orange'),
    (Kind: WebColor; Group: OrangeGroup; Value: Integer(TRggWebColors.Orangered); Name: 'Orangered'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Orchid); Name: 'Orchid'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Palegoldenrod); Name: 'Palegoldenrod'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Palegreen); Name: 'Palegreen'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Paleturquoise); Name: 'Paleturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Palevioletred); Name: 'Palevioletred'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Papayawhip); Name: 'Papayawhip'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Peachpuff); Name: 'Peachpuff'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Peru); Name: 'Peru'),
    (Kind: WebColor; Group: PinkGroup; Value: Integer(TRggWebColors.Pink); Name: 'Pink'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Plum); Name: 'Plum'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Powderblue); Name: 'Powderblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Purple); Name: 'Purple'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Red); Name: 'Red'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Rebeccapurple); Name: 'Rebeccapurple'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Rosybrown); Name: 'Rosybrown'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Royalblue); Name: 'Royalblue'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Saddlebrown); Name: 'Saddlebrown'),
    (Kind: WebColor; Group: RedGroup; Value: Integer(TRggWebColors.Salmon); Name: 'Salmon'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Sandybrown); Name: 'Sandybrown'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Seagreen); Name: 'Seagreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Seashell); Name: 'Seashell'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Sienna); Name: 'Sienna'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Silver); Name: 'Silver'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Skyblue); Name: 'Skyblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Slateblue); Name: 'Slateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: Integer(TRggWebColors.Slategray); Name: 'Slategray'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Snow); Name: 'Snow'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Springgreen); Name: 'Springgreen'),
    (Kind: WebColor; Group: BlueGroup; Value: Integer(TRggWebColors.Steelblue); Name: 'Steelblue'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Tan); Name: 'Tan'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Teal); Name: 'Teal'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Thistle); Name: 'Thistle'),
    (Kind: WebColor; Group: OrangeGroup; Value: Integer(TRggWebColors.Tomato); Name: 'Tomato'),
    (Kind: WebColor; Group: CyanGroup; Value: Integer(TRggWebColors.Turquoise); Name: 'Turquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: Integer(TRggWebColors.Violet); Name: 'Violet'),
    (Kind: WebColor; Group: BrownGroup; Value: Integer(TRggWebColors.Wheat); Name: 'Wheat'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.White); Name: 'White'),
    (Kind: WebColor; Group: WhiteGroup; Value: Integer(TRggWebColors.Whitesmoke); Name: 'Whitesmoke'),
    (Kind: WebColor; Group: YellowGroup; Value: Integer(TRggWebColors.Yellow); Name: 'Yellow'),
    (Kind: WebColor; Group: GreenGroup; Value: Integer(TRggWebColors.Yellowgreen); Name: 'Yellowgreen')
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
    class var
      TempIndexN: Integer;
    class procedure InitColorMap;
    class function IntToIdent(Int: Integer; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
    class function IdentToInt(const Ident: string; var Int: Integer; const Map: array of TRggColorMapEntry): Boolean;
    class function ColorToIdent(Color: Integer; var Ident: string): Boolean;
  private
    class function GetCount: Integer; static;
    class procedure ColorIndexCallback(Value: TRggColor); static;
    class procedure EnumeratePartition(Proc: TRggColorIndexCallback); static;
  protected
    class function ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
    class function GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
  public
    class var
    ColorMap: array of TRggColorMapEntry;
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

    class function GetColorIndex(Value: TRggColor): Integer; static;
    class function GetColorKindString(Value: TRggColor): string; static;

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
  AValue: Integer;
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
  i: Integer;
begin
  if IdentToInt(s, i, ColorMap) then
    result := TRggColor(i)
  else
    result := TRggColors.White;
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

class function TRggColorPool.ColorToIdent(Color: Integer; var Ident: string): Boolean;
begin
  result := IntToIdent(Color, Ident, ColorMap);
  if not result then
  begin
    AlphaColorToIdent(Color, Ident); // return value ignored
  end;
end;

class function TRggColorPool.ColorToString(Value: TRggColor): string;
begin
  ColorToIdent(Integer(Value), Result);
end;

class function TRggColorPool.ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Length(ColorMap)-1 do
    if ColorMap[I].Value = Integer(Value) then
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
    if ColorMap[I].Value = Integer(Value) then
    begin
      result := ColorMap[i];
      Exit;
    end;
  result := TRggColorMapEntry.GetEmtpyMapEntry;
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

class function TRggColorPool.GetColorIndex(Value: TRggColor): Integer;
var
  cme: TRggColorMapEntry;
begin
  result := ColorToMapEntry(Value, cme);
end;

class function TRggColorPool.IdentToInt(const Ident: string; var Int: Integer; const Map: array of TRggColorMapEntry): Boolean;
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

class function TRggColorPool.IntToIdent(Int: Integer; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
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
  i := GetColorIndex(c);
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
    if ColorMap[I].Value = Integer(Value) then
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
