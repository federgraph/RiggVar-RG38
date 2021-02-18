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

    Gray35 = TRggColor($FFA5A5A5); // A5
    Gray15 = TRggColor($FFD9D9D9); // D9
    Gray05 = TRggColor($FFF2F2F2); // F2

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
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFF0F0F0); Name: 'Windowgray'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFE0E0E0); Name: 'Porcelain'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFA0A0A0); Name: 'Mercury'),

    (Kind: TRggColorKind.CustomColor; Value: Integer($FFF9F9F9); Name: 'BackgroundWhite'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FF372E69); Name: 'BackgroundBlue'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FF333333); Name: 'BackgroundGray'),

    (Kind: TRggColorKind.CustomColor; Value: Integer($FFC0DCC0); Name: 'MoneyGreen'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFA6CAF0); Name: 'LegacySkyBlue'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFFFFBF0); Name: 'Cream'),

    (Kind: TRggColorKind.CustomColor; Value: Integer($FFA5A5A5); Name: 'Gray35'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFD9D9D9); Name: 'Gray15'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFF2F2F2); Name: 'Gray05'),

    (Kind: TRggColorKind.CustomColor; Value: Integer($FF333300); Name: 'Darkbrown'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FFFF9900); Name: 'Lightorange'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FF339966); Name: 'Sea'),
    (Kind: TRggColorKind.CustomColor; Value: Integer($FF99CCFF); Name: 'Paleblue')
  );

  WebColors: array [0..UniqueWebColorCount-1] of TRggColorMapEntry = (
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0F8FF); Name: 'Aliceblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFAEBD7); Name: 'Antiquewhite'),
//    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FFFF); Name: 'Aqua'), // see Cyan
    (Kind: TRggColorKind.WebColor; Value: Integer($FF7FFFD4); Name: 'Aquamarine'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0FFFF); Name: 'Azure'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF5F5DC); Name: 'Beige'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFE4C4); Name: 'Bisque'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF000000); Name: 'Black';),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFEBCD); Name: 'Blanchedalmond'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF0000FF); Name: 'Blue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF8A2BE2); Name: 'Blueviolet'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFA52A2A); Name: 'Brown'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDEB887); Name: 'Burlywood'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF5F9EA0); Name: 'Cadetblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF7FFF00); Name: 'Chartreuse'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFD2691E); Name: 'Chocolate'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF7F50); Name: 'Coral'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF6495ED); Name: 'Cornflowerblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFF8DC); Name: 'Cornsilk'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDC143C); Name: 'Crimson'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FFFF); Name: 'Cyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00008B); Name: 'Darkblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF008B8B); Name: 'Darkcyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFB8860B); Name: 'Darkgoldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFA9A9A9); Name: 'Darkgray'), // DarkSilver
    (Kind: TRggColorKind.WebColor; Value: Integer($FF006400); Name: 'Darkgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFBDB76B); Name: 'Darkkhaki'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF8B008B); Name: 'Darkmagenta'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF556B2F); Name: 'Darkolivegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF8C00); Name: 'Darkorange'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF9932CC); Name: 'Darkorchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF8B0000); Name: 'Darkred'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFE9967A); Name: 'Darksalmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF8FBC8F); Name: 'Darkseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF483D8B); Name: 'Darkslateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF2F4F4F); Name: 'Darkslategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00CED1); Name: 'Darkturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF9400D3); Name: 'Darkviolet'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF1493); Name: 'Deeppink'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00BFFF); Name: 'Deepskyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF696969); Name: 'Dimgray'), // DoveGray
    (Kind: TRggColorKind.WebColor; Value: Integer($FF1E90FF); Name: 'Dodgerblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFB22222); Name: 'Firebrick'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFAF0); Name: 'Floralwhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF228B22); Name: 'Forestgreen'),
//    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF00FF); Name: 'Fuchsia'), // see Magenta
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDCDCDC); Name: 'Gainsboro'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF8F8FF); Name: 'Ghostwhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFD700); Name: 'Gold'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDAA520); Name: 'Goldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF808080); Name: 'Gray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF008000); Name: 'Green'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFADFF2F); Name: 'Greenyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0FFF0); Name: 'Honeydew'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF69B4); Name: 'Hotpink'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFCD5C5C); Name: 'Indianred'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF4B0082); Name: 'Indigo'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFFF0); Name: 'Ivory'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0E68C); Name: 'Khaki'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFE6E6FA); Name: 'Lavender'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFF0F5); Name: 'Lavenderblush'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF7CFC00); Name: 'Lawngreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFACD); Name: 'Lemonchiffon'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFADD8E6); Name: 'Lightblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF08080); Name: 'Lightcoral'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFE0FFFF); Name: 'Lightcyan'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFAFAD2); Name: 'Lightgoldenrodyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFD3D3D3); Name: 'Lightgray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF90EE90); Name: 'Lightgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFB6C1); Name: 'Lightpink'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFA07A); Name: 'Lightsalmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF20B2AA); Name: 'Lightseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF87CEFA); Name: 'Lightskyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF778899); Name: 'Lightslategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFB0C4DE); Name: 'Lightsteelblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFFE0); Name: 'Lightyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FF00); Name: 'Lime'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF32CD32); Name: 'Limegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFAF0E6); Name: 'Linen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF00FF); Name: 'Magenta'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF800000); Name: 'Maroon'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF66CDAA); Name: 'Mediumaquamarine'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF0000CD); Name: 'Mediumblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFBA55D3); Name: 'Mediumorchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF9370DB); Name: 'Mediumpurple'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF3CB371); Name: 'Mediumseagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF7B68EE); Name: 'Mediumslateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FA9A); Name: 'Mediumspringgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF48D1CC); Name: 'Mediumturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFC71585); Name: 'Mediumvioletred'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF191970); Name: 'Midnightblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF5FFFA); Name: 'Mintcream'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFE4E1); Name: 'Mistyrose'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFE4B5); Name: 'Moccasin'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFDEAD); Name: 'Navajowhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF000080); Name: 'Navy'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFDF5E6); Name: 'Oldlace'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF808000); Name: 'Olive'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF6B8E23); Name: 'Olivedrab'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFA500); Name: 'Orange'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF4500); Name: 'Orangered'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDA70D6); Name: 'Orchid'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFEEE8AA); Name: 'Palegoldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF98FB98); Name: 'Palegreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFAFEEEE); Name: 'Paleturquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDB7093); Name: 'Palevioletred'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFEFD5); Name: 'Papayawhip'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFDAB9); Name: 'Peachpuff'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFCD853F); Name: 'Peru'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFC0CB); Name: 'Pink'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDDA0DD); Name: 'Plum'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFB0E0E6); Name: 'Powderblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF800080); Name: 'Purple'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF0000); Name: 'Red'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFBC8F8F); Name: 'Rosybrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF4169E1); Name: 'Royalblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF8B4513); Name: 'Saddlebrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFA8072); Name: 'Salmon'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF4A460); Name: 'Sandybrown'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF2E8B57); Name: 'Seagreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFF5EE); Name: 'Seashell'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFA0522D); Name: 'Sienna'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFC0C0C0); Name: 'Silver'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF87CEEB); Name: 'Skyblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF6A5ACD); Name: 'Slateblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF708090); Name: 'Slategray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFAFA); Name: 'Snow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FF7F); Name: 'Springgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF4682B4); Name: 'Steelblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFD2B48C); Name: 'Tan'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF008080); Name: 'Teal'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFD8BFD8); Name: 'Thistle'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF6347); Name: 'Tomato'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF40E0D0); Name: 'Turquoise'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFEE82EE); Name: 'Violet'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF5DEB3); Name: 'Wheat'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFFFF); Name: 'White'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF5F5F5); Name: 'Whitesmoke'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFFF00); Name: 'Yellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF9ACD32); Name: 'Yellowgreen'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF663399); Name: 'Rebeccapurple')
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
