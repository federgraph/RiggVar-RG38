unit RiggVar.FB.ColorGroup;

interface

{ This will enable intellisense in the IDE when assigning a color.

  Keep it in sync:
  - the Enum, used to interate over a color group,
  - the Array, used to access a color value with the enum value as index,
  - the Record, used to pick a color in the IDE with 'intellisense'.

  Notes:
  1. I have placed enum, array, and record adjacent to each other.
  2. You may want to change the order of the colors in a group. }

uses
  RiggVar.FB.Color;

{$region 'Pink'}
type
  TRggPinkWebColorEnum = (
    Pink,
    LightPink,
    HotPink,
    DeepPink,
    PaleVioletRed,
    MediumVioletRed
  );

const
  PinkWebColorArray: array[TRggPinkWebColorEnum] of TRggColor = (
    TWebColors.Pink,
    TWebColors.Lightpink,
    TWebColors.Hotpink,
    TWebColors.Deeppink,
    TWebColors.Palevioletred,
    TWebColors.Mediumvioletred
  );

type
  TRggPinkWebColors = record
  const
    Pink = TWebColors.Pink;
    LightPink = TWebColors.Lightpink;
    HotPink = TWebColors.Hotpink;
    DeepPink = TWebColors.Deeppink;
    PaleVioletRed = TWebColors.Palevioletred;
    MediumVioletRed = TWebColors.Mediumvioletred;
  end;
{$endregion}

{$region 'Purple'}
type
  TRggPurpleWebColorEnum = (
    Lavender,
    Thistle,
    Plum,
    Orchid,
    Violet,
//    Fuchsia,
    Magenta,
    MediumOrchid,
    DarkOrchid,
    DarkViolet,
    BlueViolet,
    DarkMagenta,
    Purple,
    MediumPurple,
    MediumSlateBlue,
    SlateBlue,
    DarkSlateBlue,
    RebeccaPurple,
    Indigo
  );

const
  PurpleWebColorArray: array[TRggPurpleWebColorEnum] of TRggColor = (
    TWebColors.Lavender,
    TWebColors.Thistle,
    TWebColors.Plum,
    TWebColors.Orchid,
    TWebColors.Violet,
//    TWebColors.Fuchsia,
    TWebColors.Magenta,
    TWebColors.MediumOrchid,
    TWebColors.DarkOrchid,
    TWebColors.DarkViolet,
    TWebColors.BlueViolet,
    TWebColors.DarkMagenta,
    TWebColors.Purple,
    TWebColors.MediumPurple,
    TWebColors.MediumSlateBlue,
    TWebColors.SlateBlue,
    TWebColors.DarkSlateBlue,
    TWebColors.RebeccaPurple,
    TWebColors.Indigo
  );

type
  TRggPurpleWebColors = record
  const
    Lavender = TWebColors.Lavender;
    Thistle = TWebColors.Thistle;
    Plum = TWebColors.Plum;
    Orchid = TWebColors.Orchid;
    Violet = TWebColors.Violet;
//    Fuchsia = TWebColors.Fuchsia;
    Magenta = TWebColors.Magenta;
    MediumOrchid = TWebColors.MediumOrchid;
    DarkOrchid = TWebColors.DarkOrchid;
    DarkViolet = TWebColors.DarkViolet;
    BlueViolet = TWebColors.BlueViolet;
    DarkMagenta = TWebColors.DarkMagenta;
    Purple = TWebColors.Purple;
    MediumPurple = TWebColors.MediumPurple;
    MediumSlateBlue = TWebColors.MediumSlateBlue;
    SlateBlue = TWebColors.SlateBlue;
    DarkSlateBlue = TWebColors.DarkSlateBlue;
    RebeccaPurple = TWebColors.RebeccaPurple;
    Indigo = TWebColors.Indigo;
  end;
{$endregion}

{$region 'Red'}
type
  TRggRedWebColorEnum = (
    LightSalmon,
    Salmon,
    DarkSalmon,
    LightCoral,
    IndianRed,
    Crimson,
    Red,
    FireBrick,
    DarkRed
  );

const
  RedWebColorArray: array[TRggRedWebColorEnum] of TRggColor = (
    TWebColors.LightSalmon,
    TWebColors.Salmon,
    TWebColors.DarkSalmon,
    TWebColors.LightCoral,
    TWebColors.IndianRed,
    TWebColors.Crimson,
    TWebColors.Red,
    TWebColors.FireBrick,
    TWebColors.DarkRed
  );

type
  TRggRedWebColors = record
  const
    LightSalmon = TWebColors.LightSalmon;
    Salmon = TWebColors.Salmon;
    DarkSalmon = TWebColors.DarkSalmon;
    LightCoral = TWebColors.LightCoral;
    IndianRed = TWebColors.IndianRed;
    Crimson = TWebColors.Crimson;
    Red = TWebColors.Red;
    FireBrick = TWebColors.FireBrick;
    DarkRed = TWebColors.DarkRed;
  end;
{$endregion}

{$region 'Orange'}
type
  TRggOrangeWebColorEnum = (
    Orange,
    DarkOrange,
    Coral,
    Tomato,
    OrangeRed
  );

const
  OrangeWebColorArray: array[TRggOrangeWebColorEnum] of TRggColor = (
    TWebColors.Orange,
    TWebColors.DarkOrange,
    TWebColors.Coral,
    TWebColors.Tomato,
    TWebColors.OrangeRed
  );

type
  TRggOrangeWebColors = record
  const
    Orange = TWebColors.Orange;
    DarkOrange = TWebColors.DarkOrange;
    Coral = TWebColors.Coral;
    Tomato = TWebColors.Tomato;
    OrangeRed = TWebColors.OrangeRed;
  end;
{$endregion}

{$region 'Yellow'}
type
  TRggYellowWebColorEnum = (
    Gold,
    Yellow,
    LightYellow,
    LemonChiffon,
    LightGoldenRodYellow,
    PapayaWhip,
    Moccasin,
    PeachPuff,
    PaleGoldenRod,
    Khaki,
    DarkKhaki
  );

const
  YellowWebColorArray: array[TRggYellowWebColorEnum] of TRggColor = (
    TWebColors.Gold,
    TWebColors.Yellow,
    TWebColors.LightYellow,
    TWebColors.LemonChiffon,
    TWebColors.LightGoldenRodYellow,
    TWebColors.PapayaWhip,
    TWebColors.Moccasin,
    TWebColors.PeachPuff,
    TWebColors.PaleGoldenRod,
    TWebColors.Khaki,
    TWebColors.DarkKhaki
  );

type
  TRggYellowWebColors = record
  const
    Gold = TWebColors.Gold;
    Yellow = TWebColors.Yellow;
    LightYellow = TWebColors.LightYellow;
    LemonChiffon = TWebColors.LemonChiffon;
    LightGoldenRodYellow = TWebColors.LightGoldenRodYellow;
    PapayaWhip = TWebColors.PapayaWhip;
    Moccasin = TWebColors.Moccasin;
    PeachPuff = TWebColors.PeachPuff;
    PaleGoldenRod = TWebColors.PaleGoldenRod;
    Khaki = TWebColors.Khaki;
    DarkKhaki = TWebColors.DarkKhaki;
  end;
{$endregion}

{$region 'Green'}
type
  TRggGreenWebColorEnum = (
    GreenYellow,
    Chartreuse,
    LawnGreen,
    Lime,
    LimeGreen,
    PaleGreen,
    LightGreen,
    MediumSpringGreen,
    SpringGreen,
    MediumSeaGreen,
    SeaGreen,
    ForestGreen,
    Green,
    DarkGreen,
    YellowGreen,
    OliveDrab,
    DarkOliveGreen,
    MediumAquaMarine,
    DarkSeaGreen,
    LightSeaGreen,
    DarkCyan,
    Teal
  );

const
  GreenWebColorArray: array[TRggGreenWebColorEnum] of TRggColor = (
    TWebColors.GreenYellow,
    TWebColors.Chartreuse,
    TWebColors.LawnGreen,
    TWebColors.Lime,
    TWebColors.LimeGreen,
    TWebColors.PaleGreen,
    TWebColors.LightGreen,
    TWebColors.MediumSpringGreen,
    TWebColors.SpringGreen,
    TWebColors.MediumSeaGreen,
    TWebColors.SeaGreen,
    TWebColors.ForestGreen,
    TWebColors.Green,
    TWebColors.DarkGreen,
    TWebColors.YellowGreen,
    TWebColors.OliveDrab,
    TWebColors.DarkOliveGreen,
    TWebColors.MediumAquaMarine,
    TWebColors.DarkSeaGreen,
    TWebColors.LightSeaGreen,
    TWebColors.DarkCyan,
    TWebColors.Teal
  );

type
  TRggGreenWebColors = record
  const
    GreenYellow = TWebColors.GreenYellow;
    Chartreuse = TWebColors.Chartreuse;
    LawnGreen = TWebColors.LawnGreen;
    Lime = TWebColors.Lime;
    LimeGreen = TWebColors.LimeGreen;
    PaleGreen = TWebColors.PaleGreen;
    LightGreen = TWebColors.LightGreen;
    MediumSpringGreen = TWebColors.MediumSpringGreen;
    SpringGreen = TWebColors.SpringGreen;
    MediumSeaGreen = TWebColors.MediumSeaGreen;
    SeaGreen = TWebColors.SeaGreen;
    ForestGreen = TWebColors.ForestGreen;
    Green = TWebColors.Green;
    DarkGreen = TWebColors.DarkGreen;
    YellowGreen = TWebColors.YellowGreen;
    OliveDrab = TWebColors.OliveDrab;
    DarkOliveGreen = TWebColors.DarkOliveGreen;
    MediumAquaMarine = TWebColors.MediumAquaMarine;
    DarkSeaGreen = TWebColors.DarkSeaGreen;
    LightSeaGreen = TWebColors.LightSeaGreen;
    DarkCyan = TWebColors.DarkCyan;
    Teal = TWebColors.Teal;
  end;
{$endregion}

{$region 'Cyan'}
type
  TRggCyanWebColorEnum = (
//    Aqua,
    Cyan,
    LightCyan,
    PaleTurquoise,
    Aquamarine,
    Turquoise,
    MediumTurquoise,
    DarkTurquoise
  );

const
  CyanWebColorArray: array[TRggCyanWebColorEnum] of TRggColor = (
//    TWebColors.Aqua,
    TWebColors.Cyan,
    TWebColors.LightCyan,
    TWebColors.PaleTurquoise,
    TWebColors.Aquamarine,
    TWebColors.Turquoise,
    TWebColors.MediumTurquoise,
    TWebColors.DarkTurquoise
  );

type
  TRggCyanWebColors = record
  const
//      Aqua = TWebColors.Aqua;
    Cyan = TWebColors.Cyan;
    LightCyan = TWebColors.LightCyan;
    PaleTurquoise = TWebColors.PaleTurquoise;
    Aquamarine = TWebColors.Aquamarine;
    Turquoise = TWebColors.Turquoise;
    MediumTurquoise = TWebColors.MediumTurquoise;
    DarkTurquoise = TWebColors.DarkTurquoise;
  end;
{$endregion}

{$region 'Blue'}
type
  TRggBlueWebColorEnum = (
    CadetBlue,
    SteelBlue,
    LightSteelBlue,
    LightBlue,
    PowderBlue,
    LightSkyBlue,
    SkyBlue,
    CornflowerBlue,
    DeepSkyBlue,
    DodgerBlue,
    RoyalBlue,
    Blue,
    MediumBlue,
    DarkBlue,
    Navy,
    MidnightBlue
  );

const
  BlueWebColorArray: array[TRggBlueWebColorEnum] of TRggColor = (
    TWebColors.CadetBlue,
    TWebColors.SteelBlue,
    TWebColors.LightSteelBlue,
    TWebColors.LightBlue,
    TWebColors.PowderBlue,
    TWebColors.LightSkyBlue,
    TWebColors.SkyBlue,
    TWebColors.CornflowerBlue,
    TWebColors.DeepSkyBlue,
    TWebColors.DodgerBlue,
    TWebColors.RoyalBlue,
    TWebColors.Blue,
    TWebColors.MediumBlue,
    TWebColors.DarkBlue,
    TWebColors.Navy,
    TWebColors.MidnightBlue
  );

type
  TRggBlueWebColors = record
  const
    CadetBlue = TWebColors.CadetBlue;
    SteelBlue = TWebColors.SteelBlue;
    LightSteelBlue = TWebColors.LightSteelBlue;
    LightBlue = TWebColors.LightBlue;
    PowderBlue = TWebColors.PowderBlue;
    LightSkyBlue = TWebColors.LightSkyBlue;
    SkyBlue = TWebColors.SkyBlue;
    CornflowerBlue = TWebColors.CornflowerBlue;
    DeepSkyBlue = TWebColors.DeepSkyBlue;
    DodgerBlue = TWebColors.DodgerBlue;
    RoyalBlue = TWebColors.RoyalBlue;
    Blue = TWebColors.Blue;
    MediumBlue = TWebColors.MediumBlue;
    DarkBlue = TWebColors.DarkBlue;
    Navy = TWebColors.Navy;
    MidnightBlue = TWebColors.MidnightBlue;
  end;
{$endregion}

{$region 'Brown'}
type
  TRggBrownWebColorEnum = (
    Cornsilk,
    BlanchedAlmond,
    Bisque,
    NavajoWhite,
    Wheat,
    BurlyWood,
    Tan,
    RosyBrown,
    SandyBrown,
    GoldenRod,
    DarkGoldenRod,
    Peru,
    Chocolate,
    Olive,
    SaddleBrown,
    Sienna,
    Brown,
    Maroon
  );

const
  BrownWebColorArray: array[TRggBrownWebColorEnum] of TRggColor = (
    TWebColors.Cornsilk,
    TWebColors.BlanchedAlmond,
    TWebColors.Bisque,
    TWebColors.NavajoWhite,
    TWebColors.Wheat,
    TWebColors.BurlyWood,
    TWebColors.Tan,
    TWebColors.RosyBrown,
    TWebColors.SandyBrown,
    TWebColors.GoldenRod,
    TWebColors.DarkGoldenRod,
    TWebColors.Peru,
    TWebColors.Chocolate,
    TWebColors.Olive,
    TWebColors.SaddleBrown,
    TWebColors.Sienna,
    TWebColors.Brown,
    TWebColors.Maroon
  );

type
  TRggBrownWebColors = record
  const
    Cornsilk = TWebColors.Cornsilk;
    BlanchedAlmond = TWebColors.BlanchedAlmond;
    Bisque = TWebColors.Bisque;
    NavajoWhite = TWebColors.NavajoWhite;
    Wheat = TWebColors.Wheat;
    BurlyWood = TWebColors.BurlyWood;
    Tan = TWebColors.Tan;
    RosyBrown = TWebColors.RosyBrown;
    SandyBrown = TWebColors.SandyBrown;
    GoldenRod = TWebColors.GoldenRod;
    DarkGoldenRod = TWebColors.DarkGoldenRod;
    Peru = TWebColors.Peru;
    Chocolate = TWebColors.Chocolate;
    Olive = TWebColors.Olive;
    SaddleBrown = TWebColors.SaddleBrown;
    Sienna = TWebColors.Sienna;
    Brown = TWebColors.Brown;
    Maroon = TWebColors.Maroon;
  end;
{$endregion}

{$region 'White'}
type
  TRggWhiteWebColorEnum = (
    White,
    Snow,
    HoneyDew,
    MintCream,
    Azure,
    AliceBlue,
    GhostWhite,
    WhiteSmoke,
    SeaShell,
    Beige,
    OldLace,
    FloralWhite,
    Ivory,
    AntiqueWhite,
    Linen,
    LavenderBlush,
    MistyRose
  );

const
  WhiteWebColorArray: array[TRggWhiteWebColorEnum] of TRggColor = (
    TWebColors.White,
    TWebColors.Snow,
    TWebColors.HoneyDew,
    TWebColors.MintCream,
    TWebColors.Azure,
    TWebColors.AliceBlue,
    TWebColors.GhostWhite,
    TWebColors.WhiteSmoke,
    TWebColors.SeaShell,
    TWebColors.Beige,
    TWebColors.OldLace,
    TWebColors.FloralWhite,
    TWebColors.Ivory,
    TWebColors.AntiqueWhite,
    TWebColors.Linen,
    TWebColors.LavenderBlush,
    TWebColors.MistyRose
  );

type
  TRggWhiteWebColors = record
  const
    White = TWebColors.White;
    Snow = TWebColors.Snow;
    HoneyDew = TWebColors.HoneyDew;
    MintCream = TWebColors.MintCream;
    Azure = TWebColors.Azure;
    AliceBlue = TWebColors.AliceBlue;
    GhostWhite = TWebColors.GhostWhite;
    WhiteSmoke = TWebColors.WhiteSmoke;
    SeaShell = TWebColors.SeaShell;
    Beige = TWebColors.Beige;
    OldLace = TWebColors.OldLace;
    FloralWhite = TWebColors.FloralWhite;
    Ivory = TWebColors.Ivory;
    AntiqueWhite = TWebColors.AntiqueWhite;
    Linen = TWebColors.Linen;
    LavenderBlush = TWebColors.LavenderBlush;
    MistyRose = TWebColors.MistyRose;
  end;
{$endregion}

{$region 'Gray'}
type
  TRggGrayWebColorEnum = (
    Gainsboro,
    LightGray,
    Silver,
    DarkGray,
    DimGray,
    Gray,
    LightSlateGray,
    SlateGray,
    DarkSlateGray,
    Black
  );

const
  GrayWebColorArray: array[TRggGrayWebColorEnum] of TRggColor = (
    TWebColors.Gainsboro,
    TWebColors.LightGray,
    TWebColors.Silver,
    TWebColors.DarkGray,
    TWebColors.DimGray,
    TWebColors.Gray,
    TWebColors.LightSlateGray,
    TWebColors.SlateGray,
    TWebColors.DarkSlateGray,
    TWebColors.Black
  );

type
  TRggGrayWebColors = record
  const
    Gainsboro = TWebColors.Gainsboro;
    LightGray = TWebColors.LightGray;
    Silver = TWebColors.Silver;
    DarkGray = TWebColors.DarkGray;
    DimGray = TWebColors.DimGray;
    Gray = TWebColors.Gray;
    LightSlateGray = TWebColors.LightSlateGray;
    SlateGray = TWebColors.SlateGray;
    DarkSlateGray = TWebColors.DarkSlateGray;
    Black = TWebColors.Black;
  end;
{$endregion}

{$region 'Custom'}
type
  TRggExtraColorEnum = (
    BackgroundWhite,
    Windowgray,
    Porcelain,
    Mercury,
    Gray35,
    Gray15,
    Gray05,
    Cream,
    Paleblue,
    LegacySkyBlue,
    MoneyGreen,
    Lightorange,
    Sea,
    BackgroundBlue,
    Darkbrown,
    BackgroundGray
  );

const
  ExtraColorArray: array[TRggExtraColorEnum] of TRggColor = (
    TExtraColors.BackgroundWhite,
    TExtraColors.Windowgray,
    TExtraColors.Porcelain,
    TExtraColors.Mercury,
    TExtraColors.Gray35,
    TExtraColors.Gray15,
    TExtraColors.Gray05,
    TExtraColors.Cream,
    TExtraColors.Paleblue,
    TExtraColors.LegacySkyBlue,
    TExtraColors.MoneyGreen,
    TExtraColors.Lightorange,
    TExtraColors.Sea,
    TExtraColors.BackgroundBlue,
    TExtraColors.Darkbrown,
    TExtraColors.BackgroundGray
  );

{$endregion}

{$region 'PLABasic'}
type
  TBasicEnum = (
    PLABasic_JadeWhite,
    PLABasic_Beige,
    PLABasic_Gold,
    PLABasic_Silver,
    PLABasic_Gray,
    PLABasic_Bronze,
    PLABasic_Brown,
    PLABasic_CocoaBrown,
    PLABasic_MaroonRed,
    PLABasic_Red,
    PLABasic_Magenta,
    PLABasic_Pink,
    PLABasic_HotPink,
    PLABasic_Orange,
    PLABasic_Pumpkin,
    PLABasic_SunflowerYellow,
    PLABasic_Yellow,
    PLABasic_BrightGreen,
    PLABasic_BambuGreen,
    PLABasic_MistletoeGreen,
    PLABasic_Turquoise,
    PLABasic_Cyan,
    PLABasic_Blue,
    PLABasic_CobaltBlue,
    PLABasic_Purple,
    PLABasic_IndigoPurple,
    PLABasic_BlueGray,
    PLABasic_LightGray,
    PLABasic_DarkGray,
    PLABasic_Black
  );

const
  BasicColorArray: array[TBasicEnum] of TRggColor = (
    TBasicColors.PLABasic_JadeWhite,
    TBasicColors.PLABasic_Beige,
    TBasicColors.PLABasic_Gold,
    TBasicColors.PLABasic_Silver,
    TBasicColors.PLABasic_Gray,
    TBasicColors.PLABasic_Bronze,
    TBasicColors.PLABasic_Brown,
    TBasicColors.PLABasic_CocoaBrown,
    TBasicColors.PLABasic_MaroonRed,
    TBasicColors.PLABasic_Red,

    TBasicColors.PLABasic_Magenta,
    TBasicColors.PLABasic_Pink,
    TBasicColors.PLABasic_HotPink,
    TBasicColors.PLABasic_Orange,
    TBasicColors.PLABasic_Pumpkin,
    TBasicColors.PLABasic_SunflowerYellow,
    TBasicColors.PLABasic_Yellow,
    TBasicColors.PLABasic_BrightGreen,
    TBasicColors.PLABasic_BambuGreen,
    TBasicColors.PLABasic_MistletoeGreen,

    TBasicColors.PLABasic_Turquoise,
    TBasicColors.PLABasic_Cyan,
    TBasicColors.PLABasic_Blue,
    TBasicColors.PLABasic_CobaltBlue,
    TBasicColors.PLABasic_Purple,
    TBasicColors.PLABasic_IndigoPurple,
    TBasicColors.PLABasic_BlueGray,
    TBasicColors.PLABasic_LightGray,
    TBasicColors.PLABasic_DarkGray,
    TBasicColors.PLABasic_Black
  );
{$endregion}

{$region 'PLAMatte'}
type
  TMatteEnum = (
    PLAMatte_IvoryWhite,
    PLAMatte_LatteBrown,
    PLAMatte_DesertTan,
    PLAMatte_AshGray,
    PLAMatte_LilacPurple,
    PLAMatte_SakuraPink,
    PLAMatte_MandarinOrange,
    PLAMatte_LemonYellow,
    PLAMatte_ScarletRed,
    PLAMatte_DarkRed,
    PLAMatte_DarkBrown,
    PLAMatte_DarkGreen,
    PLAMatte_GrassGreen,
    PLAMatte_IceBlue,
    PLAMatte_MarineBlue,
    PLAMatte_DarkBlue,
    PLAMatte_Charcoal
  );

const
  MatteColorArray: array[TMatteEnum] of TRggColor = (
    TMatteColors.PLAMatte_IvoryWhite,
    TMatteColors.PLAMatte_LatteBrown,
    TMatteColors.PLAMatte_DesertTan,
    TMatteColors.PLAMatte_AshGray,
    TMatteColors.PLAMatte_LilacPurple,
    TMatteColors.PLAMatte_SakuraPink,
    TMatteColors.PLAMatte_MandarinOrange,
    TMatteColors.PLAMatte_LemonYellow,
    TMatteColors.PLAMatte_ScarletRed,
    TMatteColors.PLAMatte_DarkRed,
    TMatteColors.PLAMatte_DarkBrown,
    TMatteColors.PLAMatte_DarkGreen,
    TMatteColors.PLAMatte_GrassGreen,
    TMatteColors.PLAMatte_IceBlue,
    TMatteColors.PLAMatte_MarineBlue,
    TMatteColors.PLAMatte_DarkBlue,
    TMatteColors.PLAMatte_Charcoal
  );
{$endregion}

type
  TExtraColorPicker = record
    const
    Null = TExtraColors.Null;

    Windowgray = TExtraColors.Windowgray;
    Porcelain = TExtraColors.Porcelain;
    Mercury = TExtraColors.Mercury;

    BackgroundWhite = TExtraColors.BackgroundWhite;
    BackgroundBlue = TExtraColors.BackgroundBlue;
    BackgroundGray = TExtraColors.BackgroundGray;

    MoneyGreen = TExtraColors.MoneyGreen;
    LegacySkyBlue = TExtraColors.LegacySkyBlue;
    Cream = TExtraColors.Cream;

    Darkbrown = TExtraColors.Darkbrown;
    Lightorange = TExtraColors.Lightorange;
    Sea = TExtraColors.Sea;
    Paleblue = TExtraColors.Paleblue;

    { Alternative names }
    Alpha = TWebColors.Black;

    Aqua = TExtraColors.Aqua;
    Fuchsia = TExtraColors.Fuchsia;

    WindowWhite = TExtraColors.WindowWhite;
    BtnFace = TExtraColors.BtnFace;
    ButtonFace = TExtraColors.ButtonFace;
    RectangleGray = TExtraColors.RectangleGray;
    QuickSilver = TExtraColors.QuickSilver;
    MedGray = TExtraColors.MedGray;
    MediumGray = TExtraColors.MediumGray;
    DarkSilver = TExtraColors.DarkSilver;
    Dovegray = TExtraColors.Dovegray;

    Gray80 = TExtraColors.Gray80;
    Gray50 = TExtraColors.Gray50;
    Gray35 = TExtraColors.Gray35;
    Gray25 = TExtraColors.Gray25;
    Gray15 = TExtraColors.Gray15;
    Gray05 = TExtraColors.Gray05;
  end;

  TWebColorPicker = record
  const
    Aliceblue = TWebColors.Aliceblue;
    Antiquewhite = TWebColors.Antiquewhite;
//    Aqua = TWebColors.Aqua; // duplicate of Cyan
    Aquamarine = TWebColors.Aquamarine;
    Azure = TWebColors.Azure;
    Beige = TWebColors.Beige;
    Bisque = TWebColors.Bisque;
    Black = TWebColors.Black;
    Blanchedalmond = TWebColors.Blanchedalmond;
    Blue = TWebColors.Blue;
    Blueviolet = TWebColors.Blueviolet;
    Brown = TWebColors.Brown;
    Burlywood = TWebColors.Burlywood;
    Cadetblue = TWebColors.Cadetblue;
    Chartreuse = TWebColors.Chartreuse;
    Chocolate = TWebColors.Chocolate;
    Coral = TWebColors.Coral;
    Cornflowerblue = TWebColors.CornflowerBlue;
    Cornsilk = TWebColors.Cornsilk;
    Crimson = TWebColors.Crimson;
    Cyan = TWebColors.Cyan;
    Darkblue = TWebColors.Darkblue;
    Darkcyan = TWebColors.Darkcyan;
    Darkgoldenrod = TWebColors.Darkgoldenrod;
    Darkgray = TWebColors.Darkgray;
    Darkgreen = TWebColors.Darkgreen;
    Darkkhaki = TWebColors.Darkkhaki;
    Darkmagenta = TWebColors.Darkmagenta;
    Darkolivegreen = TWebColors.Darkolivegreen;
    Darkorange = TWebColors.Darkorange;
    Darkorchid = TWebColors.Darkorchid;
    Darkred = TWebColors.Darkred;
    Darksalmon = TWebColors.Darksalmon;
    Darkseagreen = TWebColors.Darkseagreen;
    Darkslateblue = TWebColors.Darkslateblue;
    Darkslategray = TWebColors.Darkslategray;
    Darkturquoise = TWebColors.Darkturquoise;
    Darkviolet = TWebColors.Darkviolet;
    Deeppink = TWebColors.Deeppink;
    Deepskyblue = TWebColors.Deepskyblue;
    Dimgray = TWebColors.Dimgray;
    Dodgerblue = TWebColors.Dodgerblue;
    Firebrick = TWebColors.Firebrick;
    Floralwhite = TWebColors.Floralwhite;
    Forestgreen = TWebColors.Forestgreen;
//    Fuchsia = TWebColors.Fuchsia; // duplicate of Magenta
    Gainsboro = TWebColors.Gainsboro;
    Ghostwhite = TWebColors.Ghostwhite;
    Gold = TWebColors.Gold;
    Goldenrod = TWebColors.Goldenrod;
    Gray = TWebColors.Gray;
    Green = TWebColors.Green;
    Greenyellow = TWebColors.Greenyellow;
    Honeydew = TWebColors.Honeydew;
    Hotpink = TWebColors.Hotpink;
    Indianred = TWebColors.Indianred;
    Indigo = TWebColors.Indigo;
    Ivory = TWebColors.Ivory;
    Khaki = TWebColors.Khaki;
    Lavender = TWebColors.Lavender;
    Lavenderblush = TWebColors.Lavenderblush;
    Lawngreen = TWebColors.Lawngreen;
    Lemonchiffon = TWebColors.Lemonchiffon;
    Lightblue = TWebColors.Lightblue;
    Lightcoral = TWebColors.Lightcoral;
    Lightcyan = TWebColors.Lightcyan;
    Lightgoldenrodyellow = TWebColors.Lightgoldenrodyellow;
    Lightgray = TWebColors.Lightgray;
    Lightgreen = TWebColors.Lightgreen;
    Lightpink = TWebColors.Lightpink;
    Lightsalmon = TWebColors.Lightsalmon;
    Lightseagreen = TWebColors.Lightseagreen;
    Lightskyblue = TWebColors.Lightskyblue;
    Lightslategray = TWebColors.Lightslategray;
    Lightsteelblue = TWebColors.Lightsteelblue;
    Lightyellow = TWebColors.Lightyellow;
    Lime = TWebColors.Lime;
    Limegreen = TWebColors.Limegreen;
    Linen = TWebColors.Linen;
    Magenta = TWebColors.Magenta;
    Maroon = TWebColors.Maroon;
    Mediumaquamarine = TWebColors.Mediumaquamarine;
    Mediumblue = TWebColors.Mediumblue;
    Mediumorchid = TWebColors.Mediumorchid;
    Mediumpurple = TWebColors.Mediumpurple;
    Mediumseagreen = TWebColors.Mediumseagreen;
    Mediumslateblue = TWebColors.Mediumslateblue;
    Mediumspringgreen = TWebColors.Mediumspringgreen;
    Mediumturquoise = TWebColors.Mediumturquoise;
    Mediumvioletred = TWebColors.Mediumvioletred;
    Midnightblue = TWebColors.Midnightblue;
    Mintcream = TWebColors.Mintcream;
    Mistyrose = TWebColors.Mistyrose;
    Moccasin = TWebColors.Moccasin;
    Navajowhite = TWebColors.Navajowhite;
    Navy = TWebColors.Navy;
    Oldlace = TWebColors.Oldlace;
    Olive = TWebColors.Olive;
    Olivedrab = TWebColors.Olivedrab;
    Orange = TWebColors.Orange;
    Orangered = TWebColors.Orangered;
    Orchid = TWebColors.Orchid;
    Palegoldenrod = TWebColors.Palegoldenrod;
    Palegreen = TWebColors.Palegreen;
    Paleturquoise = TWebColors.Paleturquoise;
    Palevioletred = TWebColors.Palevioletred;
    Papayawhip = TWebColors.Papayawhip;
    Peachpuff = TWebColors.Peachpuff;
    Peru = TWebColors.Peru;
    Pink = TWebColors.Pink;
    Plum = TWebColors.Plum;
    Powderblue = TWebColors.Powderblue;
    Purple = TWebColors.Purple;
    Red = TWebColors.Red;
    Rosybrown = TWebColors.Rosybrown;
    Royalblue = TWebColors.Royalblue;
    Saddlebrown = TWebColors.Saddlebrown;
    Salmon = TWebColors.Salmon;
    Sandybrown = TWebColors.Sandybrown;
    Seagreen = TWebColors.Seagreen;
    Seashell = TWebColors.Seashell;
    Sienna = TWebColors.Sienna;
    Silver = TWebColors.Silver;
    Skyblue = TWebColors.Skyblue;
    Slateblue = TWebColors.Slateblue;
    Slategray = TWebColors.Slategray;
    Snow = TWebColors.Snow;
    Springgreen = TWebColors.Springgreen;
    Steelblue = TWebColors.Steelblue;
    Tan = TWebColors.Tan;
    Teal = TWebColors.Teal;
    Thistle = TWebColors.Thistle;
    Tomato = TWebColors.Tomato;
    Turquoise = TWebColors.Turquoise;
    Violet = TWebColors.Violet;
    Wheat = TWebColors.Wheat;
    White = TWebColors.White;
    Whitesmoke = TWebColors.Whitesmoke;
    Yellow = TWebColors.Yellow;
    Yellowgreen = TWebColors.Yellowgreen;
    RebeccaPurple = TWebColors.Rebeccapurple;
  end;

  TRggColorPicker = class
  public
    class var
    PinkColors: TRggPinkWebColors;
    PurpleColors: TRggPurpleWebColors;
    RedColors: TRggRedWebColors;
    OrangeColors: TRggOrangeWebColors;
    YellowColors: TRggYellowWebColors;
    BrownColors: TRggBrownWebColors;
    GreenColors: TRggGreenWebColors;
    CyanColors: TRggCyanWebColors;
    BlueColors: TRggBlueWebColors;
    WhiteColors: TRggWhiteWebColors;
    GrayColors: TRggGrayWebColors;
    WebColors: TWebColors;
    BasicColors: TBasicColors;
    MatteColors: TMatteColors;
  end;

implementation

end.
