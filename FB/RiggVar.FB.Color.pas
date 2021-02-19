unit RiggVar.FB.Color;

interface

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

  TRggWebColorGroup = (
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
    GrayGroup
  );

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
    Rebeccapurple = TRggColor($FF663399);
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
    DarkSilver = claDarkgray;
    Dovegray = claDimgray;

    Aqua = claCyan;
    Fuchsia = claMagenta;

    Gray80 = BackgroundGray;
    Gray50 = TRggWebColors.Gray;
    Gray25 = TRggWebColors.Silver;
  end;

  TRggColorMapEntry = record
    Kind: TRggColorKind;
    Value: Integer;
    Name: string;
    class function Create(AKind: TRggColorKind; AValue: Integer; const AName: string): TRggColorMapEntry; static;
    class function ColorKindToString(Value: TRggColorKind): string; static;
    class function ColorKindToChar(Value: TRggColorKind): char; static;
    class function GetEmtpyMapEntry: TRggColorMapEntry; static;
  end;

const
  CustomColors: array [0..CustomColorCount-1] of TRggColorMapEntry = (
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Windowgray); Name: 'Windowgray'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Porcelain); Name: 'Porcelain'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Mercury); Name: 'Mercury'),

    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.BackgroundWhite); Name: 'BackgroundWhite'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.BackgroundBlue); Name: 'BackgroundBlue'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.BackgroundGray); Name: 'BackgroundGray'),

    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.MoneyGreen); Name: 'MoneyGreen'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.LegacySkyBlue); Name: 'LegacySkyBlue'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Cream); Name: 'Cream'),

    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Gray35); Name: 'Gray35'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Gray15); Name: 'Gray15'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Gray05); Name: 'Gray05'),

    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Darkbrown); Name: 'Darkbrown'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Lightorange); Name: 'Lightorange'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Sea); Name: 'Sea'),
    (Kind: TRggColorKind.CustomColor; Value: Integer(TRggCustomColors.Paleblue); Name: 'Paleblue')
  );

  WebColors: array [0..UniqueWebColorCount-1] of TRggColorMapEntry = (
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Aliceblue); Name: 'Aliceblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Antiquewhite); Name: 'Antiquewhite'),
//    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Aqua); Name: 'Aqua'), // see Cyan
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Aquamarine); Name: 'Aquamarine'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Azure); Name: 'Azure'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Beige); Name: 'Beige'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Bisque); Name: 'Bisque'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Black); Name: 'Black';),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Blanchedalmond); Name: 'Blanchedalmond'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Blue); Name: 'Blue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Blueviolet); Name: 'Blueviolet'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Brown); Name: 'Brown'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Burlywood); Name: 'Burlywood'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Cadetblue); Name: 'Cadetblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Chartreuse); Name: 'Chartreuse'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Chocolate); Name: 'Chocolate'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Coral); Name: 'Coral'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Cornflowerblue); Name: 'Cornflowerblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Cornsilk); Name: 'Cornsilk'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Crimson); Name: 'Crimson'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Cyan); Name: 'Cyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkblue); Name: 'Darkblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkcyan); Name: 'Darkcyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkgoldenrod); Name: 'Darkgoldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkgray); Name: 'Darkgray'), // DarkSilver
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkgreen); Name: 'Darkgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkkhaki); Name: 'Darkkhaki'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkmagenta); Name: 'Darkmagenta'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkolivegreen); Name: 'Darkolivegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkorange); Name: 'Darkorange'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkorchid); Name: 'Darkorchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkred); Name: 'Darkred'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darksalmon); Name: 'Darksalmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkseagreen); Name: 'Darkseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkslateblue); Name: 'Darkslateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkslategray); Name: 'Darkslategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkturquoise); Name: 'Darkturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Darkviolet); Name: 'Darkviolet'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Deeppink); Name: 'Deeppink'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Deepskyblue); Name: 'Deepskyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Dimgray); Name: 'Dimgray'), // DoveGray
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Dodgerblue); Name: 'Dodgerblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Firebrick); Name: 'Firebrick'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Floralwhite); Name: 'Floralwhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Forestgreen); Name: 'Forestgreen'),
//    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Fuchsia); Name: 'Fuchsia'), // see Magenta
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Gainsboro); Name: 'Gainsboro'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Ghostwhite); Name: 'Ghostwhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Gold); Name: 'Gold'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Goldenrod); Name: 'Goldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Gray); Name: 'Gray'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Green); Name: 'Green'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Greenyellow); Name: 'Greenyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Honeydew); Name: 'Honeydew'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Hotpink); Name: 'Hotpink'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Indianred); Name: 'Indianred'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Indigo); Name: 'Indigo'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Ivory); Name: 'Ivory'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Khaki); Name: 'Khaki'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lavender); Name: 'Lavender'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lavenderblush); Name: 'Lavenderblush'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lawngreen); Name: 'Lawngreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lemonchiffon); Name: 'Lemonchiffon'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightblue); Name: 'Lightblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightcoral); Name: 'Lightcoral'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightcyan); Name: 'Lightcyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightgoldenrodyellow); Name: 'Lightgoldenrodyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightgray); Name: 'Lightgray'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightgreen); Name: 'Lightgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightpink); Name: 'Lightpink'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightsalmon); Name: 'Lightsalmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightseagreen); Name: 'Lightseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightskyblue); Name: 'Lightskyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightslategray); Name: 'Lightslategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightsteelblue); Name: 'Lightsteelblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lightyellow); Name: 'Lightyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Lime); Name: 'Lime'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Limegreen); Name: 'Limegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Linen); Name: 'Linen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Magenta); Name: 'Magenta'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Maroon); Name: 'Maroon'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumaquamarine); Name: 'Mediumaquamarine'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumblue); Name: 'Mediumblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumorchid); Name: 'Mediumorchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumpurple); Name: 'Mediumpurple'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumseagreen); Name: 'Mediumseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumslateblue); Name: 'Mediumslateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumspringgreen); Name: 'Mediumspringgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumturquoise); Name: 'Mediumturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mediumvioletred); Name: 'Mediumvioletred'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Midnightblue); Name: 'Midnightblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mintcream); Name: 'Mintcream'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Mistyrose); Name: 'Mistyrose'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Moccasin); Name: 'Moccasin'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Navajowhite); Name: 'Navajowhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Navy); Name: 'Navy'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Oldlace); Name: 'Oldlace'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Olive); Name: 'Olive'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Olivedrab); Name: 'Olivedrab'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Orange); Name: 'Orange'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Orangered); Name: 'Orangered'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Orchid); Name: 'Orchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Palegoldenrod); Name: 'Palegoldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Palegreen); Name: 'Palegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Paleturquoise); Name: 'Paleturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Palevioletred); Name: 'Palevioletred'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Papayawhip); Name: 'Papayawhip'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Peachpuff); Name: 'Peachpuff'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Peru); Name: 'Peru'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Pink); Name: 'Pink'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Plum); Name: 'Plum'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Powderblue); Name: 'Powderblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Purple); Name: 'Purple'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Red); Name: 'Red'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Rosybrown); Name: 'Rosybrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Royalblue); Name: 'Royalblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Saddlebrown); Name: 'Saddlebrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Salmon); Name: 'Salmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Sandybrown); Name: 'Sandybrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Seagreen); Name: 'Seagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Seashell); Name: 'Seashell'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Sienna); Name: 'Sienna'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Silver); Name: 'Silver'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Skyblue); Name: 'Skyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Slateblue); Name: 'Slateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Slategray); Name: 'Slategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Snow); Name: 'Snow'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Springgreen); Name: 'Springgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Steelblue); Name: 'Steelblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Tan); Name: 'Tan'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Teal); Name: 'Teal'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Thistle); Name: 'Thistle'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Tomato); Name: 'Tomato'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Turquoise); Name: 'Turquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Violet); Name: 'Violet'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Wheat); Name: 'Wheat'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.White); Name: 'White'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Whitesmoke); Name: 'Whitesmoke'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Yellow); Name: 'Yellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Yellowgreen); Name: 'Yellowgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer(TRggWebColors.Rebeccapurple); Name: 'Rebeccapurple')
  );

type
  TRggGetColorInfoProc = procedure(
    i: Integer;
    k: TRggColorKind;
    c: TRggColor;
    const s: string) of object;

  TRggColorPool = class(TRggCustomColors)
  strict private
    class procedure InitColorMap;
    class function IntToIdent(Int: Integer; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
    class function IdentToInt(const Ident: string; var Int: Integer; const Map: array of TRggColorMapEntry): Boolean;
    class function ColorToIdent(Color: Integer; var Ident: string): Boolean;
    class function ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
  protected
    class function GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
  public
    class var
    ColorMap: array of TRggColorMapEntry;
    class constructor Create;

    class function ColorFromRGB(R, G, B: Byte): TRggColor;
    class function ColorFromName(const s: string): TRggColor;

    class procedure EnumerateColors(Proc: TRggGetColorInfoProc);
    class procedure UpdateColorName(c: TRggColor; s: string);

    class function ColorToString(Value: TRggColor): string;
    class function ColorToKind(Value: TRggColor): TRggColorKind;

    class function GetColorIndex(Value: TRggColor): Integer;
    class function GetColorKindString(Value: TRggColor): string;

    class procedure UpdateColorNames;
    class procedure RevertColorNames;
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
    result := TColor(i)
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
    ColorMap[i] := WebColors[i];

  { custom colors }
  k := High(WebColors);
  for i := 0 to High(CustomColors) do
  begin
    j := k + 1 + i;
    ColorMap[j] := CustomColors[i];
  end;
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

end.
