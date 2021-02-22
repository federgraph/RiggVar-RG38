unit RiggVar.FB.ColorGroup;

interface

uses
  RiggVar.FB.Color;

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
    TRggColors.Pink,
    TRggColors.Lightpink,
    TRggColors.Hotpink,
    TRggColors.Deeppink,
    TRggColors.Palevioletred,
    TRggColors.Mediumvioletred
  );

type
  TRggPinkWebColors = record
  const
    Pink = TRggWebColors.Pink;
    LightPink = TRggWebColors.Lightpink;
    HotPink = TRggWebColors.Hotpink;
    DeepPink = TRggWebColors.Deeppink;
    PaleVioletRed = TRggWebColors.Palevioletred;
    MediumVioletRed = TRggWebColors.Mediumvioletred;
  end;

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
    TRggColors.Lavender,
    TRggColors.Thistle,
    TRggColors.Plum,
    TRggColors.Orchid,
    TRggColors.Violet,
//    TRggColors.Fuchsia,
    TRggColors.Magenta,
    TRggColors.MediumOrchid,
    TRggColors.DarkOrchid,
    TRggColors.DarkViolet,
    TRggColors.BlueViolet,
    TRggColors.DarkMagenta,
    TRggColors.Purple,
    TRggColors.MediumPurple,
    TRggColors.MediumSlateBlue,
    TRggColors.SlateBlue,
    TRggColors.DarkSlateBlue,
    TRggColors.RebeccaPurple,
    TRggColors.Indigo
  );

type
  TRggPurpleWebColors = record
  const
    Lavender = TRggWebColors.Lavender;
    Thistle = TRggWebColors.Thistle;
    Plum = TRggWebColors.Plum;
    Orchid = TRggWebColors.Orchid;
    Violet = TRggWebColors.Violet;
//    Fuchsia = TRggWebColors.Fuchsia;
    Magenta = TRggWebColors.Magenta;
    MediumOrchid = TRggWebColors.MediumOrchid;
    DarkOrchid = TRggWebColors.DarkOrchid;
    DarkViolet = TRggWebColors.DarkViolet;
    BlueViolet = TRggWebColors.BlueViolet;
    DarkMagenta = TRggWebColors.DarkMagenta;
    Purple = TRggWebColors.Purple;
    MediumPurple = TRggWebColors.MediumPurple;
    MediumSlateBlue = TRggWebColors.MediumSlateBlue;
    SlateBlue = TRggWebColors.SlateBlue;
    DarkSlateBlue = TRggWebColors.DarkSlateBlue;
    RebeccaPurple = TRggWebColors.RebeccaPurple;
    Indigo = TRggWebColors.Indigo;
  end;

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
    TRggColors.LightSalmon,
    TRggColors.Salmon,
    TRggColors.DarkSalmon,
    TRggColors.LightCoral,
    TRggColors.IndianRed,
    TRggColors.Crimson,
    TRggColors.Red,
    TRggColors.FireBrick,
    TRggColors.DarkRed
  );

type
  TRggRedWebColors = record
  const
    LightSalmon = TRggWebColors.LightSalmon;
    Salmon = TRggWebColors.Salmon;
    DarkSalmon = TRggWebColors.DarkSalmon;
    LightCoral = TRggWebColors.LightCoral;
    IndianRed = TRggWebColors.IndianRed;
    Crimson = TRggWebColors.Crimson;
    Red = TRggWebColors.Red;
    FireBrick = TRggWebColors.FireBrick;
    DarkRed = TRggWebColors.DarkRed;
  end;

  TRggOrangeWebColorEnum = (
    Orange,
    DarkOrange,
    Coral,
    Tomato,
    OrangeRed
  );

const
  OrangeWebColorArray: array[TRggOrangeWebColorEnum] of TRggColor = (
    TRggColors.Orange,
    TRggColors.DarkOrange,
    TRggColors.Coral,
    TRggColors.Tomato,
    TRggColors.OrangeRed
  );

type
  TRggOrangeWebColors = record
  const
    Orange = TRggWebColors.Orange;
    DarkOrange = TRggWebColors.DarkOrange;
    Coral = TRggWebColors.Coral;
    Tomato = TRggWebColors.Tomato;
    OrangeRed = TRggWebColors.OrangeRed;
  end;

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
    TRggWebColors.Gold,
    TRggWebColors.Yellow,
    TRggWebColors.LightYellow,
    TRggWebColors.LemonChiffon,
    TRggWebColors.LightGoldenRodYellow,
    TRggWebColors.PapayaWhip,
    TRggWebColors.Moccasin,
    TRggWebColors.PeachPuff,
    TRggWebColors.PaleGoldenRod,
    TRggWebColors.Khaki,
    TRggWebColors.DarkKhaki
  );

type
  TRggYellowWebColors = record
  const
    Gold = TRggWebColors.Gold;
    Yellow = TRggWebColors.Yellow;
    LightYellow = TRggWebColors.LightYellow;
    LemonChiffon = TRggWebColors.LemonChiffon;
    LightGoldenRodYellow = TRggWebColors.LightGoldenRodYellow;
    PapayaWhip = TRggWebColors.PapayaWhip;
    Moccasin = TRggWebColors.Moccasin;
    PeachPuff = TRggWebColors.PeachPuff;
    PaleGoldenRod = TRggWebColors.PaleGoldenRod;
    Khaki = TRggWebColors.Khaki;
    DarkKhaki = TRggWebColors.DarkKhaki;
  end;

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
    TRggWebColors.GreenYellow,
    TRggWebColors.Chartreuse,
    TRggWebColors.LawnGreen,
    TRggWebColors.Lime,
    TRggWebColors.LimeGreen,
    TRggWebColors.PaleGreen,
    TRggWebColors.LightGreen,
    TRggWebColors.MediumSpringGreen,
    TRggWebColors.SpringGreen,
    TRggWebColors.MediumSeaGreen,
    TRggWebColors.SeaGreen,
    TRggWebColors.ForestGreen,
    TRggWebColors.Green,
    TRggWebColors.DarkGreen,
    TRggWebColors.YellowGreen,
    TRggWebColors.OliveDrab,
    TRggWebColors.DarkOliveGreen,
    TRggWebColors.MediumAquaMarine,
    TRggWebColors.DarkSeaGreen,
    TRggWebColors.LightSeaGreen,
    TRggWebColors.DarkCyan,
    TRggWebColors.Teal
  );

type
  TRggGreenWebColors = record
  const
    GreenYellow = TRggWebColors.GreenYellow;
    Chartreuse = TRggWebColors.Chartreuse;
    LawnGreen = TRggWebColors.LawnGreen;
    Lime = TRggWebColors.Lime;
    LimeGreen = TRggWebColors.LimeGreen;
    PaleGreen = TRggWebColors.PaleGreen;
    LightGreen = TRggWebColors.LightGreen;
    MediumSpringGreen = TRggWebColors.MediumSpringGreen;
    SpringGreen = TRggWebColors.SpringGreen;
    MediumSeaGreen = TRggWebColors.MediumSeaGreen;
    SeaGreen = TRggWebColors.SeaGreen;
    ForestGreen = TRggWebColors.ForestGreen;
    Green = TRggWebColors.Green;
    DarkGreen = TRggWebColors.DarkGreen;
    YellowGreen = TRggWebColors.YellowGreen;
    OliveDrab = TRggWebColors.OliveDrab;
    DarkOliveGreen = TRggWebColors.DarkOliveGreen;
    MediumAquaMarine = TRggWebColors.MediumAquaMarine;
    DarkSeaGreen = TRggWebColors.DarkSeaGreen;
    LightSeaGreen = TRggWebColors.LightSeaGreen;
    DarkCyan = TRggWebColors.DarkCyan;
    Teal = TRggWebColors.Teal;
  end;

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
//    TRggWebColors.Aqua,
    TRggWebColors.Cyan,
    TRggWebColors.LightCyan,
    TRggWebColors.PaleTurquoise,
    TRggWebColors.Aquamarine,
    TRggWebColors.Turquoise,
    TRggWebColors.MediumTurquoise,
    TRggWebColors.DarkTurquoise
  );

type
  TRggCyanWebColors = record
  const
//      Aqua = TRggWebColors.Aqua;
    Cyan = TRggWebColors.Cyan;
    LightCyan = TRggWebColors.LightCyan;
    PaleTurquoise = TRggWebColors.PaleTurquoise;
    Aquamarine = TRggWebColors.Aquamarine;
    Turquoise = TRggWebColors.Turquoise;
    MediumTurquoise = TRggWebColors.MediumTurquoise;
    DarkTurquoise = TRggWebColors.DarkTurquoise;
  end;

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
    TRggWebColors.CadetBlue,
    TRggWebColors.SteelBlue,
    TRggWebColors.LightSteelBlue,
    TRggWebColors.LightBlue,
    TRggWebColors.PowderBlue,
    TRggWebColors.LightSkyBlue,
    TRggWebColors.SkyBlue,
    TRggWebColors.CornflowerBlue,
    TRggWebColors.DeepSkyBlue,
    TRggWebColors.DodgerBlue,
    TRggWebColors.RoyalBlue,
    TRggWebColors.Blue,
    TRggWebColors.MediumBlue,
    TRggWebColors.DarkBlue,
    TRggWebColors.Navy,
    TRggWebColors.MidnightBlue
  );

type
  TRggBlueWebColors = record
  const
    CadetBlue = TRggWebColors.CadetBlue;
    SteelBlue = TRggWebColors.SteelBlue;
    LightSteelBlue = TRggWebColors.LightSteelBlue;
    LightBlue = TRggWebColors.LightBlue;
    PowderBlue = TRggWebColors.PowderBlue;
    LightSkyBlue = TRggWebColors.LightSkyBlue;
    SkyBlue = TRggWebColors.SkyBlue;
    CornflowerBlue = TRggWebColors.CornflowerBlue;
    DeepSkyBlue = TRggWebColors.DeepSkyBlue;
    DodgerBlue = TRggWebColors.DodgerBlue;
    RoyalBlue = TRggWebColors.RoyalBlue;
    Blue = TRggWebColors.Blue;
    MediumBlue = TRggWebColors.MediumBlue;
    DarkBlue = TRggWebColors.DarkBlue;
    Navy = TRggWebColors.Navy;
    MidnightBlue = TRggWebColors.MidnightBlue;
  end;

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
    TRggWebColors.Cornsilk,
    TRggWebColors.BlanchedAlmond,
    TRggWebColors.Bisque,
    TRggWebColors.NavajoWhite,
    TRggWebColors.Wheat,
    TRggWebColors.BurlyWood,
    TRggWebColors.Tan,
    TRggWebColors.RosyBrown,
    TRggWebColors.SandyBrown,
    TRggWebColors.GoldenRod,
    TRggWebColors.DarkGoldenRod,
    TRggWebColors.Peru,
    TRggWebColors.Chocolate,
    TRggWebColors.Olive,
    TRggWebColors.SaddleBrown,
    TRggWebColors.Sienna,
    TRggWebColors.Brown,
    TRggWebColors.Maroon
  );

type
  TRggBrownWebColors = record
  const
    Cornsilk = TRggWebColors.Cornsilk;
    BlanchedAlmond = TRggWebColors.BlanchedAlmond;
    Bisque = TRggWebColors.Bisque;
    NavajoWhite = TRggWebColors.NavajoWhite;
    Wheat = TRggWebColors.Wheat;
    BurlyWood = TRggWebColors.BurlyWood;
    Tan = TRggWebColors.Tan;
    RosyBrown = TRggWebColors.RosyBrown;
    SandyBrown = TRggWebColors.SandyBrown;
    GoldenRod = TRggWebColors.GoldenRod;
    DarkGoldenRod = TRggWebColors.DarkGoldenRod;
    Peru = TRggWebColors.Peru;
    Chocolate = TRggWebColors.Chocolate;
    Olive = TRggWebColors.Olive;
    SaddleBrown = TRggWebColors.SaddleBrown;
    Sienna = TRggWebColors.Sienna;
    Brown = TRggWebColors.Brown;
    Maroon = TRggWebColors.Maroon;
  end;

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
    TRggWebColors.White,
    TRggWebColors.Snow,
    TRggWebColors.HoneyDew,
    TRggWebColors.MintCream,
    TRggWebColors.Azure,
    TRggWebColors.AliceBlue,
    TRggWebColors.GhostWhite,
    TRggWebColors.WhiteSmoke,
    TRggWebColors.SeaShell,
    TRggWebColors.Beige,
    TRggWebColors.OldLace,
    TRggWebColors.FloralWhite,
    TRggWebColors.Ivory,
    TRggWebColors.AntiqueWhite,
    TRggWebColors.Linen,
    TRggWebColors.LavenderBlush,
    TRggWebColors.MistyRose
  );

type
  TRggWhiteWebColors = record
  const
    White = TRggWebColors.White;
    Snow = TRggWebColors.Snow;
    HoneyDew = TRggWebColors.HoneyDew;
    MintCream = TRggWebColors.MintCream;
    Azure = TRggWebColors.Azure;
    AliceBlue = TRggWebColors.AliceBlue;
    GhostWhite = TRggWebColors.GhostWhite;
    WhiteSmoke = TRggWebColors.WhiteSmoke;
    SeaShell = TRggWebColors.SeaShell;
    Beige = TRggWebColors.Beige;
    OldLace = TRggWebColors.OldLace;
    FloralWhite = TRggWebColors.FloralWhite;
    Ivory = TRggWebColors.Ivory;
    AntiqueWhite = TRggWebColors.AntiqueWhite;
    Linen = TRggWebColors.Linen;
    LavenderBlush = TRggWebColors.LavenderBlush;
    MistyRose = TRggWebColors.MistyRose;
  end;

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
    TRggWebColors.Gainsboro,
    TRggWebColors.LightGray,
    TRggWebColors.Silver,
    TRggWebColors.DarkGray,
    TRggWebColors.DimGray,
    TRggWebColors.Gray,
    TRggWebColors.LightSlateGray,
    TRggWebColors.SlateGray,
    TRggWebColors.DarkSlateGray,
    TRggWebColors.Black
  );

type
  TRggGrayWebColors = record
  const
    Gainsboro = TRggWebColors.Gainsboro;
    LightGray = TRggWebColors.LightGray;
    Silver = TRggWebColors.Silver;
    DarkGray = TRggWebColors.DarkGray;
    DimGray = TRggWebColors.DimGray;
    Gray = TRggWebColors.Gray;
    LightSlateGray = TRggWebColors.LightSlateGray;
    SlateGray = TRggWebColors.SlateGray;
    DarkSlateGray = TRggWebColors.DarkSlateGray;
    Black = TRggWebColors.Black;
  end;

  TRggCustomColorEnum = (
    BackgroundWhite,
    Windowgray,
    Porcelain,
    Mercury,
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
  CustomColorArray: array[TRggCustomColorEnum] of TRggColor = (
    TRggCustomColors.BackgroundWhite,
    TRggCustomColors.Windowgray,
    TRggCustomColors.Porcelain,
    TRggCustomColors.Mercury,
    TRggCustomColors.Cream,
    TRggCustomColors.Paleblue,
    TRggCustomColors.LegacySkyBlue,
    TRggCustomColors.MoneyGreen,
    TRggCustomColors.Lightorange,
    TRggCustomColors.Sea,
    TRggCustomColors.BackgroundBlue,
    TRggCustomColors.Darkbrown,
    TRggCustomColors.BackgroundGray
  );

type
  TRggCustomColorPicker = record
    const
    Null = TRggColors.Null;

    Windowgray = TRggCustomColors.Windowgray;
    Porcelain = TRggCustomColors.Porcelain;
    Mercury = TRggCustomColors.Mercury;

    BackgroundWhite = TRggCustomColors.BackgroundWhite;
    BackgroundBlue = TRggCustomColors.BackgroundBlue;
    BackgroundGray = TRggCustomColors.BackgroundGray;

    MoneyGreen = TRggCustomColors.MoneyGreen;
    LegacySkyBlue = TRggCustomColors.LegacySkyBlue;
    Cream = TRggCustomColors.Cream;

    Darkbrown = TRggCustomColors.Darkbrown;
    Lightorange = TRggCustomColors.Lightorange;
    Sea = TRggCustomColors.Sea;
    Paleblue = TRggCustomColors.PaleBlue;

    { Alternative names }
    Alpha = TRggWebColors.Black;

    Aqua = TRggCustomColors.Aqua;
    Fuchsia = TRggCustomColors.Fuchsia;

    WindowWhite = TRggCustomColors.WindowWhite;
    BtnFace = TRggCustomColors.BtnFace;
    ButtonFace = TRggCustomColors.ButtonFace;
    RectangleGray = TRggCustomColors.RectangleGray;
    QuickSilver = TRggCustomColors.QuickSilver;
    MedGray = TRggCustomColors.MedGray;
    MediumGray = TRggCustomColors.MediumGray;
    DarkSilver = TRggCustomColors.DarkSilver;
    Dovegray = TRggCustomColors.Dovegray;

    Gray80 = TRggCustomColors.Gray80;
    Gray50 = TRggCustomColors.Gray50;
    Gray35 = TRggCustomColors.Gray35;
    Gray25 = TRggCustomColors.Gray25;
    Gray15 = TRggCustomColors.Gray15;
    Gray05 = TRggCustomColors.Gray05;
  end;

  TRggWebColorPicker = record
  const
    Aliceblue = TRggWebColors.Aliceblue;
    Antiquewhite = TRggWebColors.Antiquewhite;
//    Aqua = TRggWebColors.Aqua; // duplicate of Cyan
    Aquamarine = TRggWebColors.Aquamarine;
    Azure = TRggWebColors.Azure;
    Beige = TRggWebColors.Beige;
    Bisque = TRggWebColors.Bisque;
    Black = TRggWebColors.Black;
    Blanchedalmond = TRggWebColors.Blanchedalmond;
    Blue = TRggWebColors.Blue;
    Blueviolet = TRggWebColors.Blueviolet;
    Brown = TRggWebColors.Brown;
    Burlywood = TRggWebColors.Burlywood;
    Cadetblue = TRggWebColors.Cadetblue;
    Chartreuse = TRggWebColors.Chartreuse;
    Chocolate = TRggWebColors.Chocolate;
    Coral = TRggWebColors.Coral;
    Cornflowerblue = TRggWebColors.CornflowerBlue;
    Cornsilk = TRggWebColors.Cornsilk;
    Crimson = TRggWebColors.Crimson;
    Cyan = TRggWebColors.Cyan;
    Darkblue = TRggWebColors.Darkblue;
    Darkcyan = TRggWebColors.Darkcyan;
    Darkgoldenrod = TRggWebColors.Darkgoldenrod;
    Darkgray = TRggWebColors.Darkgray;
    Darkgreen = TRggWebColors.Darkgreen;
    Darkkhaki = TRggWebColors.Darkkhaki;
    Darkmagenta = TRggWebColors.Darkmagenta;
    Darkolivegreen = TRggWebColors.Darkolivegreen;
    Darkorange = TRggWebColors.Darkorange;
    Darkorchid = TRggWebColors.Darkorchid;
    Darkred = TRggWebColors.Darkred;
    Darksalmon = TRggWebColors.Darksalmon;
    Darkseagreen = TRggWebColors.Darkseagreen;
    Darkslateblue = TRggWebColors.Darkslateblue;
    Darkslategray = TRggWebColors.Darkslategray;
    Darkturquoise = TRggWebColors.Darkturquoise;
    Darkviolet = TRggWebColors.Darkviolet;
    Deeppink = TRggWebColors.Deeppink;
    Deepskyblue = TRggWebColors.Deepskyblue;
    Dimgray = TRggWebColors.Dimgray;
    Dodgerblue = TRggWebColors.Dodgerblue;
    Firebrick = TRggWebColors.Firebrick;
    Floralwhite = TRggWebColors.Floralwhite;
    Forestgreen = TRggWebColors.Forestgreen;
//    Fuchsia = TRggWebColors.Fuchsia; // duplicate of Magenta
    Gainsboro = TRggWebColors.Gainsboro;
    Ghostwhite = TRggWebColors.Ghostwhite;
    Gold = TRggWebColors.Gold;
    Goldenrod = TRggWebColors.Goldenrod;
    Gray = TRggWebColors.Gray;
    Green = TRggWebColors.Green;
    Greenyellow = TRggWebColors.Greenyellow;
    Honeydew = TRggWebColors.Honeydew;
    Hotpink = TRggWebColors.Hotpink;
    Indianred = TRggWebColors.Indianred;
    Indigo = TRggWebColors.Indigo;
    Ivory = TRggWebColors.Ivory;
    Khaki = TRggWebColors.Khaki;
    Lavender = TRggWebColors.Lavender;
    Lavenderblush = TRggWebColors.Lavenderblush;
    Lawngreen = TRggWebColors.Lawngreen;
    Lemonchiffon = TRggWebColors.Lemonchiffon;
    Lightblue = TRggWebColors.Lightblue;
    Lightcoral = TRggWebColors.Lightcoral;
    Lightcyan = TRggWebColors.Lightcyan;
    Lightgoldenrodyellow = TRggWebColors.Lightgoldenrodyellow;
    Lightgray = TRggWebColors.Lightgray;
    Lightgreen = TRggWebColors.Lightgreen;
    Lightpink = TRggWebColors.Lightpink;
    Lightsalmon = TRggWebColors.Lightsalmon;
    Lightseagreen = TRggWebColors.Lightseagreen;
    Lightskyblue = TRggWebColors.Lightskyblue;
    Lightslategray = TRggWebColors.Lightslategray;
    Lightsteelblue = TRggWebColors.Lightsteelblue;
    Lightyellow = TRggWebColors.Lightyellow;
    Lime = TRggWebColors.Lime;
    Limegreen = TRggWebColors.Limegreen;
    Linen = TRggWebColors.Linen;
    Magenta = TRggWebColors.Magenta;
    Maroon = TRggWebColors.Maroon;
    Mediumaquamarine = TRggWebColors.Mediumaquamarine;
    Mediumblue = TRggWebColors.Mediumblue;
    Mediumorchid = TRggWebColors.Mediumorchid;
    Mediumpurple = TRggWebColors.Mediumpurple;
    Mediumseagreen = TRggWebColors.Mediumseagreen;
    Mediumslateblue = TRggWebColors.Mediumslateblue;
    Mediumspringgreen = TRggWebColors.Mediumspringgreen;
    Mediumturquoise = TRggWebColors.Mediumturquoise;
    Mediumvioletred = TRggWebColors.Mediumvioletred;
    Midnightblue = TRggWebColors.Midnightblue;
    Mintcream = TRggWebColors.Mintcream;
    Mistyrose = TRggWebColors.Mistyrose;
    Moccasin = TRggWebColors.Moccasin;
    Navajowhite = TRggWebColors.Navajowhite;
    Navy = TRggWebColors.Navy;
    Oldlace = TRggWebColors.Oldlace;
    Olive = TRggWebColors.Olive;
    Olivedrab = TRggWebColors.Olivedrab;
    Orange = TRggWebColors.Orange;
    Orangered = TRggWebColors.Orangered;
    Orchid = TRggWebColors.Orchid;
    Palegoldenrod = TRggWebColors.Palegoldenrod;
    Palegreen = TRggWebColors.Palegreen;
    Paleturquoise = TRggWebColors.Paleturquoise;
    Palevioletred = TRggWebColors.Palevioletred;
    Papayawhip = TRggWebColors.Papayawhip;
    Peachpuff = TRggWebColors.Peachpuff;
    Peru = TRggWebColors.Peru;
    Pink = TRggWebColors.Pink;
    Plum = TRggWebColors.Plum;
    Powderblue = TRggWebColors.Powderblue;
    Purple = TRggWebColors.Purple;
    Red = TRggWebColors.Red;
    Rosybrown = TRggWebColors.Rosybrown;
    Royalblue = TRggWebColors.Royalblue;
    Saddlebrown = TRggWebColors.Saddlebrown;
    Salmon = TRggWebColors.Salmon;
    Sandybrown = TRggWebColors.Sandybrown;
    Seagreen = TRggWebColors.Seagreen;
    Seashell = TRggWebColors.Seashell;
    Sienna = TRggWebColors.Sienna;
    Silver = TRggWebColors.Silver;
    Skyblue = TRggWebColors.Skyblue;
    Slateblue = TRggWebColors.Slateblue;
    Slategray = TRggWebColors.Slategray;
    Snow = TRggWebColors.Snow;
    Springgreen = TRggWebColors.Springgreen;
    Steelblue = TRggWebColors.Steelblue;
    Tan = TRggWebColors.Tan;
    Teal = TRggWebColors.Teal;
    Thistle = TRggWebColors.Thistle;
    Tomato = TRggWebColors.Tomato;
    Turquoise = TRggWebColors.Turquoise;
    Violet = TRggWebColors.Violet;
    Wheat = TRggWebColors.Wheat;
    White = TRggWebColors.White;
    Whitesmoke = TRggWebColors.Whitesmoke;
    Yellow = TRggWebColors.Yellow;
    Yellowgreen = TRggWebColors.Yellowgreen;
    RebeccaPurple = TRggWebColors.Rebeccapurple;
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
    CustomColors: TRggCustomColors;
  end;

implementation

end.
