unit RiggVar.FB.Color;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

{ I want a custom set of unique color values which includes
  - web colors (all)
  - a few additional colors used in Delphi.
  - PLA Basic filament colors from Bamblab
  - PLA Matte filament colors from Bambulab

  All colors shall support 'picking by name' in the IDE code editor.

  For more info as to the original problem see
    https://en.delphipraxis.net/topic/4412-about-medgray-and-windowwhite

  Note: Special color listbox components serve as 'proof of concept'.
  }

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes;

const
  WebColorCount = 141;
  UniqueWebColorCount = WebColorCount - 2;  // 139
  ExtraColorCount = 16;
  BasicColorCount = 30;
  MatteColorCount = 17;

  ColorMapCount =
    UniqueWebColorCount +
    ExtraColorCount +
    BasicColorCount +
    MatteColorCount;

type
  TRggColor = TAlphaColor;

  TRggColorKind = (
    WebColor,
    ExtraColor,
    BasicColor,
    MatteColor,
    Unknown
  );

  TRggColorDistanceInfo = record
    Count: Integer;
    Distance: double;
    Dist: Integer;
    dr: Integer;
    dg: Integer;
    db: Integer;
  end;

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
    ExtraGroup,
    BasicGroup,
    MatteGroup
  );

  TWebColorGroupSet = set of TRggColorGroup;

  TWebColors = class
  public const
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

  TExtraColors = class
  public const
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

    { Alternative names may be used when assigning a color in code,
      but they do not appear in the ColorMap. }
    WindowWhite = Windowgray;
    BtnFace = Windowgray;
    ButtonFace = Windowgray;
    RectangleGray = Porcelain;
    QuickSilver = Mercury;
    MedGray = Mercury;
    MediumGray = Mercury;
    DarkSilver = TWebColors.Darkgray;
    Dovegray = TWebColors.Dimgray;

    Aqua = TWebColors.Cyan;
    Fuchsia = TWebColors.Magenta;

    Gray80 = BackgroundGray;
    Gray50 = TWebColors.Gray;
    Gray25 = TWebColors.Silver;
  end;

  TBasicColors = class
  public const
    PLABasic_JadeWhite = $FFFFFFFF;
    PLABasic_Beige = $FFF7E6DE;
    PLABasic_Gold = $FFE4BD68;
    PLABasic_Silver = $FFA6A9AA;
    PLABasic_Gray = $FF8E9089;
    PLABasic_Bronze = $FF847D48;
    PLABasic_Brown = $FF9D432C;
    PLABasic_CocoaBrown = $FF6F5034;
    PLABasic_MaroonRed = $FF9D2235;
    PLABasic_Red = $FFC12E1F;

    PLABasic_Magenta = $FFEC008C;
    PLABasic_Pink = $FFF55A74;
    PLABasic_HotPink = $FFF5547C;
    PLABasic_Orange = $FFFF6A13;
    PLABasic_Pumpkin = $FFFF9016;
    PLABasic_SunflowerYellow = $FFFEC600;
    PLABasic_Yellow = $FFF4EE2A;
    PLABasic_BrightGreen = $FFBECF00;
    PLABasic_BambuGreen = $FF00AE42;
    PLABasic_MistletoeGreen = $FF3F8E43;

    PLABasic_Turquoise= $FF00B1B7;
    PLABasic_Cyan = $FF0086D6;
    PLABasic_Blue = $FF0A2989;
    PLABasic_CobaltBlue = $FFB056B8;
    PLABasic_Purple = $FF5E43B7;
    PLABasic_IndigoPurple = $FF482960;
    PLABasic_BlueGray = $FF5B6579;
    PLABasic_LightGray = $FFD1D3D5;
    PLABasic_DarkGray = $FF545454;
    PLABasic_Black = $FF000000;
  end;

  TMatteColors = class
  public const
    PLAMatte_IvoryWhite = $FFFFFFFF;
    PLAMatte_LatteBrown = $FFD3B7A7;
    PLAMatte_DesertTan = $FFE8DBB7;
    PLAMatte_AshGray = $FF9B9EA0;
    PLAMatte_LilacPurple = $FFAE96D4;
    PLAMatte_SakuraPink = $FFE8AFCF;
    PLAMatte_MandarinOrange = $FFF99963;
    PLAMatte_LemonYellow = $FFF7D959;
    PLAMatte_ScarletRed = $FFDE4343;
    PLAMatte_DarkRed = $FFBB3D43;

    PLAMatte_DarkBrown = $FF7D6556;
    PLAMatte_DarkGreen =$FF68724D;
    PLAMatte_GrassGreen = $FF61C680;
    PLAMatte_IceBlue = $FFA3D8E1;
    PLAMatte_MarineBlue = $FF0078BF;
    PLAMatte_DarkBlue = $FF042F56;
    PLAMatte_Charcoal = $FF000000;
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
  BasicColors: array [0..BasicColorCount-1] of TRggColorMapEntry = (
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_JadeWhite; Name: 'PLABasic_JadeWhite'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Beige; Name: 'PLABasic_Beige'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Gold; Name: 'PLABasic_Gold'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Silver; Name: 'PLABasic_Silver'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Gray; Name: 'PLABasic_Gray'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Bronze; Name: 'PLABasic_Bronze'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Brown; Name: 'PLABasic_Brown'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_CocoaBrown; Name: 'PLABasic_CocoaBrown'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_MaroonRed; Name: 'PLABasic_MaroonRed'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Red; Name: 'PLABasic_Red'),

    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Magenta; Name: 'PLABasic_Magenta'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Pink; Name: 'PLABasic_Pink'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_HotPink; Name: 'PLABasic_HotPink'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Orange; Name: 'PLABasic_Orange'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Pumpkin; Name: 'PLABasic_Pumpkin'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_SunflowerYellow; Name: 'PLABasic_SunflowerYellow'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Yellow; Name: 'PLABasic_Yellow'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_BrightGreen; Name: 'PLABasic_BrightGreen'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_BambuGreen; Name: 'PLABasic_BambuGreen'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_MistletoeGreen; Name: 'PLABasic_MistletoeGreen'),

    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Turquoise; Name: 'PLABasic_Turquoise'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Cyan; Name: 'PLABasic_Cyan'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Blue; Name: 'PLABasic_Blue'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_CobaltBlue; Name: 'PLABasic_CobaltBlue'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Purple; Name: 'PLABasic_Purple'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_IndigoPurple; Name: 'PLABasic_IndigoPurple'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_BlueGray; Name: 'PLABasic_BlueGray'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_LightGray; Name: 'PLABasic_LightGray'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_DarkGray; Name: 'PLABasic_DarkGray'),
    (Kind: BasicColor; Group: BasicGroup; Value: TBasicColors.PLABasic_Black; Name: 'PLABasic_Black')
  );

  MatteColors: array [0..MatteColorCount-1] of TRggColorMapEntry = (
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_IvoryWhite; Name: 'PLAMatte_IvoryWhite'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_LatteBrown; Name: 'PLAMatte_LatteBrown'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_DesertTan; Name: 'PLAMatte_DesertTan'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_AshGray; Name: 'PLAMatte_AshGray'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_LilacPurple; Name: 'PLAMatte_LilacPurple'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_SakuraPink; Name: 'PLAMatte_SakuraPink'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_MandarinOrange; Name: 'PLAMatte_MandarinOrange'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_LemonYellow; Name: 'PLAMatte_LemonYellow'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_ScarletRed; Name: 'PLAMatte_ScarletRed'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_DarkRed; Name: 'PLAMatte_DarkRed'),

    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_DarkBrown; Name: 'PLAMatte_DarkBrown'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_DarkGreen; Name: 'PLAMatte_DarkGreen'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_GrassGreen; Name: 'PLAMatte_GrassGreen'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_IceBlue; Name: 'PLAMatte_IceBlue'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_MarineBlue; Name: 'PLAMatte_MarineBlue'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_DarkBlue; Name: 'PLAMatte_DarkBlue'),
    (Kind: MatteColor; Group: MatteGroup; Value: TMatteColors.PLAMatte_Charcoal; Name: 'PLAMatte_Charcoal')
  );

  ExtraColors: array [0..ExtraColorCount-1] of TRggColorMapEntry = (
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Windowgray; Name: 'Windowgray'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Porcelain; Name: 'Porcelain'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Mercury; Name: 'Mercury'),

    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.BackgroundWhite; Name: 'BackgroundWhite'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.BackgroundBlue; Name: 'BackgroundBlue'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.BackgroundGray; Name: 'BackgroundGray'),

    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.MoneyGreen; Name: 'MoneyGreen'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.LegacySkyBlue; Name: 'LegacySkyBlue'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Cream; Name: 'Cream'),

    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Gray35; Name: 'Gray35'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Gray15; Name: 'Gray15'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Gray05; Name: 'Gray05'),

    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Darkbrown; Name: 'Darkbrown'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Lightorange; Name: 'Lightorange'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Sea; Name: 'Sea'),
    (Kind: ExtraColor; Group: ExtraGroup; Value: TExtraColors.Paleblue; Name: 'Paleblue')
  );

  WebColors: array [0..UniqueWebColorCount-1] of TRggColorMapEntry = (
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Aliceblue; Name: 'Aliceblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Antiquewhite; Name: 'Antiquewhite'),
//    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Aqua; Name: 'Aqua'), // see Cyan
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Aquamarine; Name: 'Aquamarine'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Azure; Name: 'Azure'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Beige; Name: 'Beige'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Bisque; Name: 'Bisque'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Black; Name: 'Black'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Blanchedalmond; Name: 'Blanchedalmond'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Blue; Name: 'Blue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Blueviolet; Name: 'Blueviolet'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Brown; Name: 'Brown'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Burlywood; Name: 'Burlywood'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Cadetblue; Name: 'Cadetblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Chartreuse; Name: 'Chartreuse'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Chocolate; Name: 'Chocolate'),
    (Kind: WebColor; Group: OrangeGroup; Value: TWebColors.Coral; Name: 'Coral'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Cornflowerblue; Name: 'Cornflowerblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Cornsilk; Name: 'Cornsilk'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Crimson; Name: 'Crimson'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Cyan; Name: 'Cyan'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Darkblue; Name: 'Darkblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Darkcyan; Name: 'Darkcyan'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Darkgoldenrod; Name: 'Darkgoldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Darkgray; Name: 'Darkgray'), // DarkSilver
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Darkgreen; Name: 'Darkgreen'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Darkkhaki; Name: 'Darkkhaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Darkmagenta; Name: 'Darkmagenta'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Darkolivegreen; Name: 'Darkolivegreen'),
    (Kind: WebColor; Group: OrangeGroup; Value: TWebColors.Darkorange; Name: 'Darkorange'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Darkorchid; Name: 'Darkorchid'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Darkred; Name: 'Darkred'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Darksalmon; Name: 'Darksalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Darkseagreen; Name: 'Darkseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Darkslateblue; Name: 'Darkslateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Darkslategray; Name: 'Darkslategray'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Darkturquoise; Name: 'Darkturquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Darkviolet; Name: 'Darkviolet'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Deeppink; Name: 'Deeppink'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Deepskyblue; Name: 'Deepskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Dimgray; Name: 'Dimgray'), // DoveGray
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Dodgerblue; Name: 'Dodgerblue'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Firebrick; Name: 'Firebrick'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Floralwhite; Name: 'Floralwhite'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Forestgreen; Name: 'Forestgreen'),
//    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Fuchsia; Name: 'Fuchsia'), // see Magenta
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Gainsboro; Name: 'Gainsboro'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Ghostwhite; Name: 'Ghostwhite'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Gold; Name: 'Gold'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Goldenrod; Name: 'Goldenrod'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Gray; Name: 'Gray'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Green; Name: 'Green'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Greenyellow; Name: 'Greenyellow'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Honeydew; Name: 'Honeydew'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Hotpink; Name: 'Hotpink'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Indianred; Name: 'Indianred'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Indigo; Name: 'Indigo'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Ivory; Name: 'Ivory'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Khaki; Name: 'Khaki'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Lavender; Name: 'Lavender'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Lavenderblush; Name: 'Lavenderblush'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Lawngreen; Name: 'Lawngreen'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Lemonchiffon; Name: 'Lemonchiffon'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Lightblue; Name: 'Lightblue'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Lightcoral; Name: 'Lightcoral'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Lightcyan; Name: 'Lightcyan'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Lightgoldenrodyellow; Name: 'Lightgoldenrodyellow'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Lightgray; Name: 'Lightgray'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Lightgreen; Name: 'Lightgreen'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Lightpink; Name: 'Lightpink'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Lightsalmon; Name: 'Lightsalmon'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Lightseagreen; Name: 'Lightseagreen'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Lightskyblue; Name: 'Lightskyblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Lightslategray; Name: 'Lightslategray'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Lightsteelblue; Name: 'Lightsteelblue'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Lightyellow; Name: 'Lightyellow'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Lime; Name: 'Lime'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Limegreen; Name: 'Limegreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Linen; Name: 'Linen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Magenta; Name: 'Magenta'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Maroon; Name: 'Maroon'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Mediumaquamarine; Name: 'Mediumaquamarine'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Mediumblue; Name: 'Mediumblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Mediumorchid; Name: 'Mediumorchid'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Mediumpurple; Name: 'Mediumpurple'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Mediumseagreen; Name: 'Mediumseagreen'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Mediumslateblue; Name: 'Mediumslateblue'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Mediumspringgreen; Name: 'Mediumspringgreen'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Mediumturquoise; Name: 'Mediumturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Mediumvioletred; Name: 'Mediumvioletred'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Midnightblue; Name: 'Midnightblue'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Mintcream; Name: 'Mintcream'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Mistyrose; Name: 'Mistyrose'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Moccasin; Name: 'Moccasin'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Navajowhite; Name: 'Navajowhite'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Navy; Name: 'Navy'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Oldlace; Name: 'Oldlace'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Olive; Name: 'Olive'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Olivedrab; Name: 'Olivedrab'),
    (Kind: WebColor; Group: OrangeGroup; Value: TWebColors.Orange; Name: 'Orange'),
    (Kind: WebColor; Group: OrangeGroup; Value: TWebColors.Orangered; Name: 'Orangered'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Orchid; Name: 'Orchid'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Palegoldenrod; Name: 'Palegoldenrod'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Palegreen; Name: 'Palegreen'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Paleturquoise; Name: 'Paleturquoise'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Palevioletred; Name: 'Palevioletred'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Papayawhip; Name: 'Papayawhip'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Peachpuff; Name: 'Peachpuff'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Peru; Name: 'Peru'),
    (Kind: WebColor; Group: PinkGroup; Value: TWebColors.Pink; Name: 'Pink'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Plum; Name: 'Plum'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Powderblue; Name: 'Powderblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Purple; Name: 'Purple'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Red; Name: 'Red'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Rebeccapurple; Name: 'Rebeccapurple'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Rosybrown; Name: 'Rosybrown'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Royalblue; Name: 'Royalblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Saddlebrown; Name: 'Saddlebrown'),
    (Kind: WebColor; Group: RedGroup; Value: TWebColors.Salmon; Name: 'Salmon'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Sandybrown; Name: 'Sandybrown'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Seagreen; Name: 'Seagreen'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Seashell; Name: 'Seashell'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Sienna; Name: 'Sienna'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Silver; Name: 'Silver'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Skyblue; Name: 'Skyblue'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Slateblue; Name: 'Slateblue'),
    (Kind: WebColor; Group: GrayGroup; Value: TWebColors.Slategray; Name: 'Slategray'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Snow; Name: 'Snow'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Springgreen; Name: 'Springgreen'),
    (Kind: WebColor; Group: BlueGroup; Value: TWebColors.Steelblue; Name: 'Steelblue'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Tan; Name: 'Tan'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Teal; Name: 'Teal'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Thistle; Name: 'Thistle'),
    (Kind: WebColor; Group: OrangeGroup; Value: TWebColors.Tomato; Name: 'Tomato'),
    (Kind: WebColor; Group: CyanGroup; Value: TWebColors.Turquoise; Name: 'Turquoise'),
    (Kind: WebColor; Group: PurpleGroup; Value: TWebColors.Violet; Name: 'Violet'),
    (Kind: WebColor; Group: BrownGroup; Value: TWebColors.Wheat; Name: 'Wheat'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.White; Name: 'White'),
    (Kind: WebColor; Group: WhiteGroup; Value: TWebColors.Whitesmoke; Name: 'Whitesmoke'),
    (Kind: WebColor; Group: YellowGroup; Value: TWebColors.Yellow; Name: 'Yellow'),
    (Kind: WebColor; Group: GreenGroup; Value: TWebColors.Yellowgreen; Name: 'Yellowgreen')
  );

type
  TRggGetColorInfoProc = procedure(
    i: Integer;
    k: TRggColorKind;
    c: TRggColor;
    const s: string) of object;

  TRggColorIndexCallback = procedure(Value: TRggColor) of object;

  TRggColorPoolBase = class
  strict private
    var BingoCounter: Integer;
    var TempIndexN: Integer;
    function CardinalToIdent(Int: Cardinal; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
    function IdentToCardinal(const Ident: string; var Int: Cardinal; const Map: array of TRggColorMapEntry): Boolean;
    function ColorToIdent(Color: Cardinal; var Ident: string): Boolean;
  private
    function GetCount: Integer;
    procedure ColorIndexCallback(Value: TRggColor);
    procedure EnumeratePartition(Proc: TRggColorIndexCallback);
    procedure Bingo;
    procedure Bongo;
  protected
    procedure InitColorMap; virtual;
    procedure AddWebColors;
    procedure AddExtraColors;
    procedure AddBasicColors;
    procedure AddMatteColors;
    function ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
    function GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
  public
    ColorMap: array of TRggColorMapEntry;
    constructor Create;

    procedure Clear;

    function ColorFromRGB(R, G, B: Byte): TRggColor;
    function ColorFromName(const s: string): TRggColor;

    procedure EnumerateColors(Proc: TRggGetColorInfoProc);
    procedure UpdateColorName(c: TRggColor; s: string);

    function ColorToString(Value: TRggColor): string;
    function ColorToKind(Value: TRggColor): TRggColorKind;
    function ColorToGroup(Value: TRggColor): TRggColorGroup;
    function ColorGroupToGroupName(g: TRggColorGroup): string;
    function ColorToGroupName(Value: TRggColor): string;
    function ColorToIndexA(Value: TRggColor): Integer;
    function ColorToIndexN(Value: TRggColor): Integer;
    function GetColorKindString(Value: TRggColor): string;

    function GetColorIndex(Value: TRggColor): Integer;
    function LookupRowForIndexN(IdxN: Integer): Integer;
    function FindNearest1(Value: TRggColor; out Info: TRggColorDistanceInfo): TRggColor;
    function FindNearest2(Value: TRggColor; out Info: TRggColorDistanceInfo): TRggColor;
    function GetColorDistance(cla1, cla2: TAlphaColor): double;

    procedure UpdateIndexN;
    procedure UpdateColorNames;
    procedure RevertColorNames;
    property Count: Integer read GetCount;
  end;

  TRggColorPool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TWebColorPool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TExtraColorPool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TPLABasicPool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TPLAMattePool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TBambuColorPool = class(TRggColorPoolBase)
  protected
    procedure InitColorMap; override;
  end;

  TRggGrayColors = record
  const
    Whitesmoke = TWebColors.Whitesmoke;
    Windowgray = TExtraColors.Windowgray;
    Porcelain = TExtraColors.Porcelain;
    Gainsboro = TWebColors.Gainsboro;
    Lightgray = TWebColors.Lightgray;
    Silver = TWebColors.Silver;
    DarkSilver = TExtraColors.DarkSilver;
    Mercury = TExtraColors.Mercury;
    Gray = TWebColors.Gray;
    Dovegray = TWebColors.Dimgray;
  end;

  TRggPercentGrayColors = record
  const
    Gray80 = TExtraColors.BackgroundGray; // 33
    Gray50 = TWebColors.Gray; // 80
    Gray35 = TExtraColors.Gray35; // A5
    Gray25 = TWebColors.Silver; // C0
    Gray15 = TExtraColors.Gray15; // D9
    Gray05 = TExtraColors.Gray05; // F2
  end;

  ColorVar = class
  public class var
    RggColorPool: TRggColorPool;
    WebColorPool: TWebColorPool;
    ExtraColorPool: TExtraColorPool;

    class constructor Create;
    class destructor Destroy;
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
  result.IndexA := 0;
  result.IndexN := 0;
  result.Group := TRggColorGroup.CombinedGroup;

  result.Name := '-';
  result.Kind := TRggColorKind.Unknown;
  result.Value := 0;
end;

class function TRggColorMapEntry.ColorKindToChar(Value: TRggColorKind): char;
begin
  case Value of
    WebColor: result := 'W';
    ExtraColor: result := 'C';
    BasicColor: result := 'B';
    MatteColor: result := 'M';
    Unknown: result := 'U';
    else result := '-';
  end;
end;

class function TRggColorMapEntry.ColorKindToString(Value: TRggColorKind): string;
begin
  case Value of
    WebColor: result := 'Web';
    ExtraColor: result := 'Extra';
    BasicColor: result := 'Basic';
    MatteColor: result := 'Matte';
    Unknown: result := 'Unknown';
    else result := '-';
  end;
end;

{ TRggColorPoolBase }

constructor TRggColorPoolBase.Create;
begin
  InitColorMap;
end;

function TRggColorPoolBase.ColorFromName(const s: string): TRggColor;
var
  i: Cardinal;
begin
  if IdentToCardinal(s, i, ColorMap) then
    result := TRggColor(i)
  else
    result := TWebColors.White;
end;

function TRggColorPoolBase.ColorFromRGB(R, G, B: Byte): TRggColor;
var
  acr: TAlphaColorRec;
begin
  acr.R := R;
  acr.G := G;
  acr.B := B;
  acr.A := 255;
  result := acr.Color;
end;

function TRggColorPoolBase.ColorToIdent(Color: Cardinal; var Ident: string): Boolean;
var
  i: Integer;
begin
  result := CardinalToIdent(Color, Ident, ColorMap);
  if not result then
  begin
//{$RANGECHECKS OFF}
    i := Integer(Color);
//{$RANGECHECKS ON}
    AlphaColorToIdent(i, Ident); // return value ignored
  end;
end;

function TRggColorPoolBase.ColorToString(Value: TRggColor): string;
begin
  ColorToIdent(Value, Result);
end;

function TRggColorPoolBase.ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Length(ColorMap) - 1 do
    if ColorMap[I].Value = Value then
    begin
      MapEntry := ColorMap[i];
      Result := I;
      Exit;
    end;
end;

function TRggColorPoolBase.GetColorDistance(cla1, cla2: TAlphaColor): double;
var
  acr1, acr2: TAlphaColorRec;
  rmean: Integer;
  dr, dg, db: Integer;
  wr, wg, wb: Integer;
  t1, t2, t3, t: double;
begin
  acr1 := TAlphaColorRec(cla1);
  acr2 := TAlphaColorRec(cla2);

  rmean := (acr1.R + acr2.R) div 2;
  dr := (acr1.R - acr2.R);
  dg := (acr1.G - acr2.G);
  db := (acr1.B - acr2.B);

  wr := (512 + rmean) shr 8;
  wg := 4;
  wb := (767 - rmean) shr 8;

  t1 := wr * dr * dr;
  t2 := wg * dg *dg;
  t3 := wb * db *db;

  t := t1 + t2 + t3;
  result := sqrt(t);
end;

function TRggColorPoolBase.FindNearest1(Value: TRggColor; out Info: TRggColorDistanceInfo): TRggColor;
var
  I: Integer;
  MapEntry: TRggColorMapEntry;
  vr, cr: TAlphaColorRec;
  dr, dg, db: Byte;
  d, dmin: Integer;
begin
  result := claBlack;
  dmin := MaxInt;
  vr := TAlphaColorRec(Value);
  for I := 0 to Length(ColorMap)-1 do
  begin
    MapEntry := ColorMap[i];
    cr := TAlphaColorRec(MapEntry.Value);
    dr := Abs(cr.R - vr.R);
    dg := Abs(cr.G - vr.G);
    db := Abs(cr.B - vr.B);
    d := dr + dg + db;
    if d < dmin then
    begin
      dmin := d;
      result := MapEntry.Value;
      Info.dr := dr;
      Info.dg := dg;
      Info.db := db;
      Info.Count := 0;
    end;
    if d = dmin then
    begin
      Info.Count := 1;
    end;
  end;
  Info.Distance := dmin;
  Info.Dist := dmin;
end;

function TRggColorPoolBase.FindNearest2(Value: TRggColor; out Info: TRggColorDistanceInfo): TRggColor;
var
  I: Integer;
  MapEntry: TRggColorMapEntry;
  d, dmin: double;
begin
  result := claBlack;
  dmin := 10000000;
  for I := 0 to Length(ColorMap)-1 do
  begin
    MapEntry := ColorMap[i];
    d := GetColorDistance(Value, MapEntry.Value);
    if d < dmin then
    begin
      dmin := d;
      result := MapEntry.Value;
      Info.Count := 0;
    end;
    if d = dmin then
    begin
      Info.Count := 1;
    end;
  end;
  Info.Distance := dmin;
  Info.Dist := Round(dmin);
end;

function TRggColorPoolBase.GetColorKindString(Value: TRggColor): string;
begin
  result := TRggColorMapEntry.ColorKindToString(GetColorMapEntry(Value).Kind);
end;

function TRggColorPoolBase.GetColorMapEntry(Value: TRggColor): TRggColorMapEntry;
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

function TRggColorPoolBase.ColorToIndexA(Value: TRggColor): Integer;
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

function TRggColorPoolBase.ColorToIndexN(Value: TRggColor): Integer;
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

function TRggColorPoolBase.LookupRowForIndexN(IdxN: Integer): Integer;
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

function TRggColorPoolBase.GetColorIndex(Value: TRggColor): Integer;
begin
  result := LookupRowForIndexN(ColorToIndexA(Value));
end;

function TRggColorPoolBase.GetCount: Integer;
begin
  result := Length(ColorMap);
end;

function TRggColorPoolBase.ColorToKind(Value: TRggColor): TRggColorKind;
var
  cme: TRggColorMapEntry;
begin
  if ColorToMapEntry(Value, cme) > -1 then
    result := cme.Kind
  else
    result := TRggColorKind.Unknown;
end;

procedure TRggColorPoolBase.EnumerateColors(Proc: TRggGetColorInfoProc);
var
  i: Integer;
begin
  for i := 0 to Length(ColorMap)-1 do
  begin
    Proc(i, ColorMap[i].Kind, ColorMap[i].Value, ColorMap[i].Name);
  end;
end;

function TRggColorPoolBase.IdentToCardinal(const Ident: string; var Int: Cardinal; const Map: array of TRggColorMapEntry): Boolean;
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

procedure TRggColorPoolBase.InitColorMap;
begin

end;

function TRggColorPoolBase.CardinalToIdent(Int: Cardinal; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
var
  I: Integer;
  l: Integer;
  h: Integer;
begin
  l := Low(Map);
  h := High(Map);
  for I := l to h do
    if Map[I].Value = Int then
    begin
      Result := True;
      Ident := Map[I].Name;
      Exit;
    end;
  Result := False;
end;

procedure TRggColorPoolBase.Clear;
begin
  SetLength(ColorMap, 0);
end;

procedure TRggColorPoolBase.AddWebColors;
var
  l: Integer;
  c: Integer;
  k: Integer;
  i: Integer;
  j: Integer;
begin
  l := High(WebColors);
  c := Length(ColorMap);
  k := c;
  c := c + l + 1;
  SetLength(ColorMap, c);

  for i := 0 to l do
  begin
    j := k + i;
    ColorMap[j] := WebColors[i];
    ColorMap[j].IndexA := j;
  end;
end;

procedure TRggColorPoolBase.AddExtraColors;
var
  l: Integer;
  c: Integer;
  k: Integer;
  i: Integer;
  j: Integer;
begin
  l := High(ExtraColors);

  c := Length(ColorMap);
  k := c;
  c := c + l + 1;
  SetLength(ColorMap, c);

  for i := 0 to l do
  begin
    j := k + i;
    ColorMap[j] := ExtraColors[i];
    ColorMap[j].IndexA := j;
  end;
end;

procedure TRggColorPoolBase.AddBasicColors;
var
  l: Integer;
  c: Integer;
  k: Integer;
  i: Integer;
  j: Integer;
begin
  l := High(BasicColors);

  c := Length(ColorMap);
  k := c;
  c := c + l + 1;
  SetLength(ColorMap, c);

  for i := 0 to l do
  begin
    j := k + i;
    ColorMap[j] := BasicColors[i];
    ColorMap[j].IndexA := j;
  end;
end;

procedure TRggColorPoolBase.AddMatteColors;
var
  l: Integer;
  c: Integer;
  k: Integer;
  i: Integer;
  j: Integer;
begin
  l := High(MatteColors);

  c := Length(ColorMap);
  k := c;
  c := c + l + 1;
  SetLength(ColorMap, c);

  for i := 0 to l do
  begin
    j := k + i;
    ColorMap[j] := MatteColors[i];
    ColorMap[j].IndexA := j;
  end;
end;

procedure TRggColorPoolBase.UpdateColorName(c: TRggColor; s: string);
var
  i: Integer;
begin
  i := ColorToIndexA(c);
  if i > -1 then
    ColorMap[i].Name := s;
end;

procedure TRggColorPoolBase.UpdateColorNames;
begin
  { update ColorMap with custom names }
  UpdateColorName(TWebColors.Darkgray, 'DarkSilver');
  UpdateColorName(TWebColors.Dimgray, 'Dovegray');

  UpdateColorName(TExtraColors.Windowgray, 'WindowWhite');
  UpdateColorName(TExtraColors.Porcelain, 'RectangleGray');
  UpdateColorName(TExtraColors.Mercury, 'QuickSilver');
end;

procedure TRggColorPoolBase.RevertColorNames;
begin
  { revert to original web color names }
  UpdateColorName(TWebColors.Darkgray, 'Darkgray');
  UpdateColorName(TWebColors.Dimgray, 'Dimgray');

  { revert to self chosen 'default' names for custom colors }
  UpdateColorName(TExtraColors.Windowgray, 'Windowgray');
  UpdateColorName(TExtraColors.Porcelain, 'Porcelain');
  UpdateColorName(TExtraColors.Mercury, 'Mercury');
end;

function TRggColorPoolBase.ColorToGroup(Value: TRggColor): TRggColorGroup;
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

function TRggColorPoolBase.ColorGroupToGroupName(g: TRggColorGroup): string;
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
    ExtraGroup: result := 'Extra';
    BasicGroup: result := 'Basic';
    MatteGroup: result := 'Matte';
  end;
  result := result + 'Group';
end;

function TRggColorPoolBase.ColorToGroupName(Value: TRggColor): string;
var
  g: TRggColorGroup;
begin
  g := ColorToGroup(Value);
  result := ColorGroupToGroupName(g);
end;

procedure TRggColorPoolBase.UpdateIndexN;
begin
  TempIndexN := 0;
  EnumeratePartition(ColorIndexCallback);
end;

procedure TRggColorPoolBase.EnumeratePartition(Proc: TRggColorIndexCallback);
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
  extraE: TRggExtraColorEnum;
  basicE: TBasicEnum;
  matteE: TMatteEnum;
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

  for extraE := Low(extraE) to High(extraE) do
    Proc(ExtraColorArray[extraE]);

  for basicE := Low(basicE) to High(basicE) do
    Proc(BasicColorArray[basicE]);

  for matteE := Low(matteE) to High(matteE) do
    Proc(MatteColorArray[matteE]);
end;

procedure TRggColorPoolBase.ColorIndexCallback(Value: TRggColor);
var
  cme: TRggColorMapEntry;
  i: Integer;
  l: Integer;
begin
  i := ColorToMapEntry(Value, cme);
  l := Length(ColorMap);
  if i > -1 then
  begin
    { if entry with Value exists in Map, record its IndexA in IndexN }
    if TempIndexN < l then
      ColorMap[TempIndexN].IndexN := cme.IndexA
    else
      Bongo;
  end
  else
  begin
    Bingo;
  end;
  Inc(TempIndexN);
end;

procedure TRggColorPoolBase.Bingo;
begin
  Inc(BingoCounter);
end;

procedure TRggColorPoolBase.Bongo;
begin
  Inc(BingoCounter);
end;

{ TRggColorPool }

procedure TRggColorPool.InitColorMap;
begin
  Assert(Low(WebColors) = 0);
  Assert(High(WebColors) = UniqueWebColorCount-1);
  Assert(ColorMapCount >= UniqueWebColorCount);

  AddWebColors;
//  AddExtraColors;
  AddBasicColors;
  AddMatteColors;

  UpdateIndexN;
end;

{ TPLABasicPool }

procedure TPLABasicPool.InitColorMap;
begin
  AddBasicColors;
end;

{ TPLAMattePool }

procedure TPLAMattePool.InitColorMap;
begin
  AddMatteColors;
end;

{ TBambuColorPool }

procedure TBambuColorPool.InitColorMap;
begin
  AddBasicColors;
  AddMatteColors;
end;

{ TWebColorPool }

procedure TWebColorPool.InitColorMap;
begin
  AddWebColors;
end;

{ TExtraColorPool }

procedure TExtraColorPool.InitColorMap;
begin
  AddExtraColors;
end;

{ ColorVar }

class constructor ColorVar.Create;
begin
  RggColorPool := TRggColorPool.Create;
  WebColorPool := TWebColorPool.Create;
  ExtraColorPool := TExtraColorPool.Create;
end;

class destructor ColorVar.Destroy;
begin
  RggColorPool.Free;
  WebColorPool.Free;
  ExtraColorPool.Free;
  inherited;
end;

end.
