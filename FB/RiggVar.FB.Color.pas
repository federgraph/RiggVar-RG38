﻿unit RiggVar.FB.Color;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes;

type
  TRggColor = TAlphaColor;

  TRggColorKind = (
    WebColor,
    CustomColor,
    Unknown
  );

  TRggColorMapEntry = record
    Kind: TRggColorKind;
    Value: Integer;
    Name: string;
    class function Create(AKind: TRggColorKind; AValue: Integer; const AName: string): TRggColorMapEntry; static;
    class function ColorKindToString(Value: TRggColorKind): string; static;
    class function ColorKindToChar(Value: TRggColorKind): char; static;
    class function GetEmtpyMapEntry: TRggColorMapEntry; static;
  end;

  TRggGetColorInfoProc = procedure(
    i: Integer;
    k: TRggColorKind;
    c: TRggColor;
    const s: string) of object;

  TRggColorBase = class
  private
    class procedure InitColorMap;
  public
    class var
    Alpha: TRggColor;
    Null: TRggColor;
    ColorMap: array of TRggColorMapEntry;
    class constructor Create;
    class function ColorFromRGB(R, G, B: Byte): TRggColor;
    class procedure GetColorValues(Proc: TRggGetColorInfoProc);
    class function ColorToIdent(Color: Integer; var Ident: string): Boolean;
    class function ColorToString(Value: TRggColor): string;
    class function ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
    class function ColorToKind(Value: TRggColor): TRggColorKind;
    class function ColorToInt(const Ident: string; var Int: Integer; const Map: array of TRggColorMapEntry): Boolean;
    class function GetColorToMapEntry(Value: TRggColor): TRggColorMapEntry;
    class function IntToColor(Int: Integer; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
    class function GetMapIndexOfColor(Value: TRggColor): Integer;
    class procedure UpdateColorName(c: TRggColor; s: string);
  end;

  TRggWebColors = class(TRggColorBase)
  public
    class var
    Aliceblue: TRggColor;
    Antiquewhite: TRggColor;
    Aqua: TRggColor;
    Aquamarine: TRggColor;
    Azure: TRggColor;
    Beige: TRggColor;
    Bisque: TRggColor;
    Black: TRggColor;
    Blanchedalmond: TRggColor;
    Blue: TRggColor;
    Blueviolet: TRggColor;
    Brown: TRggColor;
    Burlywood: TRggColor;
    Cadetblue: TRggColor;
    Chartreuse: TRggColor;
    Chocolate: TRggColor;
    Coral: TRggColor;
    Cornflowerblue: TRggColor;
    Cornsilk: TRggColor;
    Crimson: TRggColor;
    Cyan: TRggColor;
    Darkblue: TRggColor;
    Darkcyan: TRggColor;
    Darkgoldenrod: TRggColor;
    Darkgray: TRggColor;
    Darkgreen: TRggColor;
    Darkkhaki: TRggColor;
    Darkmagenta: TRggColor;
    Darkolivegreen: TRggColor;
    Darkorange: TRggColor;
    Darkorchid: TRggColor;
    Darkred: TRggColor;
    Darksalmon: TRggColor;
    Darkseagreen: TRggColor;
    Darkslateblue: TRggColor;
    Darkslategray: TRggColor;
    Darkturquoise: TRggColor;
    Darkviolet: TRggColor;
    Deeppink: TRggColor;
    Deepskyblue: TRggColor;
    Dimgray: TRggColor;
    Dodgerblue: TRggColor;
    Firebrick: TRggColor;
    Floralwhite: TRggColor;
    Forestgreen: TRggColor;
    Fuchsia: TRggColor;
    Gainsboro: TRggColor;
    Ghostwhite: TRggColor;
    Gold: TRggColor;
    Goldenrod: TRggColor;
    Gray: TRggColor;
    Green: TRggColor;
    Greenyellow: TRggColor;
    Honeydew: TRggColor;
    Hotpink: TRggColor;
    Indianred: TRggColor;
    Indigo: TRggColor;
    Ivory: TRggColor;
    Khaki: TRggColor;
    Lavender: TRggColor;
    Lavenderblush: TRggColor;
    Lawngreen: TRggColor;
    Lemonchiffon: TRggColor;
    Lightblue: TRggColor;
    Lightcoral: TRggColor;
    Lightcyan: TRggColor;
    Lightgoldenrodyellow: TRggColor;
    Lightgray: TRggColor;
    Lightgreen: TRggColor;
    Lightpink: TRggColor;
    Lightsalmon: TRggColor;
    Lightseagreen: TRggColor;
    Lightskyblue: TRggColor;
    Lightslategray: TRggColor;
    Lightsteelblue: TRggColor;
    Lightyellow: TRggColor;
    Lime: TRggColor;
    Limegreen: TRggColor;
    Linen: TRggColor;
    Magenta: TRggColor;
    Maroon: TRggColor;
    Mediumaquamarine: TRggColor;
    Mediumblue: TRggColor;
    Mediumorchid: TRggColor;
    Mediumpurple: TRggColor;
    Mediumseagreen: TRggColor;
    Mediumslateblue: TRggColor;
    Mediumspringgreen: TRggColor;
    Mediumturquoise: TRggColor;
    Mediumvioletred: TRggColor;
    Midnightblue: TRggColor;
    Mintcream: TRggColor;
    Mistyrose: TRggColor;
    Moccasin: TRggColor;
    Navajowhite: TRggColor;
    Navy: TRggColor;
    Oldlace: TRggColor;
    Olive: TRggColor;
    Olivedrab: TRggColor;
    Orange: TRggColor;
    Orangered: TRggColor;
    Orchid: TRggColor;
    Palegoldenrod: TRggColor;
    Palegreen: TRggColor;
    Paleturquoise: TRggColor;
    Palevioletred: TRggColor;
    Papayawhip: TRggColor;
    Peachpuff: TRggColor;
    Peru: TRggColor;
    Pink: TRggColor;
    Plum: TRggColor;
    Powderblue: TRggColor;
    Purple: TRggColor;
    Red: TRggColor;
    Rosybrown: TRggColor;
    Royalblue: TRggColor;
    Saddlebrown: TRggColor;
    Salmon: TRggColor;
    Sandybrown: TRggColor;
    Seagreen: TRggColor;
    Seashell: TRggColor;
    Sienna: TRggColor;
    Silver: TRggColor;
    Skyblue: TRggColor;
    Slateblue: TRggColor;
    Slategray: TRggColor;
    Snow: TRggColor;
    Springgreen: TRggColor;
    Steelblue: TRggColor;
    Tan: TRggColor;
    Teal: TRggColor;
    Thistle: TRggColor;
    Tomato: TRggColor;
    Turquoise: TRggColor;
    Violet: TRggColor;
    Wheat: TRggColor;
    White: TRggColor;
    Whitesmoke: TRggColor;
    Yellow: TRggColor;
    Yellowgreen: TRggColor;
    class constructor Create;
  end;

  TRggCustomColors = class(TRggWebColors)
  public
    class var
    Windowgray: TRggColor; // 245 F0
    Porcelain: TRggColor; // 240 E0
    Mercury: TRggColor; // A0 160

    BackgroundWhite: TRggColor;
    BackgroundBlue: TRggColor;
    BackgroundGray: TRggColor;

    { Alternative names can be used when assigning a color in code. }

    { Alternatives for Windowgray }
    WindowWhite: TRggColor;
    BtnFace: TRggColor;
    ButtonFace: TRggColor;

    { Alternative for Porcelain }
    RectangleGray: TRggColor;

    { Alternatives for Mercury }
    QuickSilver: TRggColor;
    MedGray: TRggColor;
    MediumGray: TRggColor;

    { Alternative for Darkgray - a web color }
    DarkSilver: TRggColor;

    { Alternative for Dimgray - a web color }
    Dovegray: TRggColor;

    class constructor Create;
    class procedure UpdateColorNames;
    class procedure RevertColorNames;
  end;

  TRggColors = class(TRggCustomColors);

  TRggColorTest = class(TRggColors)
  public
    class procedure OutputWebColorTable(ML: TStrings);
    class procedure OutputColorTable(ML: TStrings; WebOnly: Boolean = False);
    class procedure OutputGrayTable(ML: TStrings);
    class procedure OutputAlternativeNameTable(ML: TStrings); static;
  end;

implementation

const
  { all 140 web colors }
  WebColors: array [0..139] of TRggColorMapEntry = (
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0F8FF); Name: 'Aliceblue'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFAEBD7); Name: 'Antiquewhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF00FFFF); Name: 'Aqua'),
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
    (Kind: TRggColorKind.WebColor; Value: Integer($FFA9A9A9); Name: 'Darkgray'), //DarkSilver
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
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF00FF); Name: 'Fuchsia'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDCDCDC); Name: 'Gainsboro'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF8F8FF); Name: 'Ghostwhite'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFFD700); Name: 'Gold'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFDAA520); Name: 'Goldenrod'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF808080); Name: 'Gray'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FF008000); Name: 'Green'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFADFF2F); Name: 'Greenyellow'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFF0FFF0); Name: 'Honeydew'),
    (Kind: TRggColorKind.WebColor; Value: Integer($FFFF69B4); Name: 'claHotpink'),
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
    (Kind: TRggColorKind.WebColor; Value: Integer($FF9ACD32); Name: 'Yellowgreen')
  );

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

{ TRggColorBase }

class constructor TRggColorBase.Create;
begin
  Alpha := TAlphaColor($FF000000);
  Null := Alpha or TAlphaColor($FFFFFF);
  InitColorMap;
end;

class function TRggColorBase.ColorFromRGB(R, G, B: Byte): TRggColor;
var
  acr: TAlphaColorRec;
begin
  acr.R := R;
  acr.G := G;
  acr.B := B;
  acr.A := 255;
  result := acr.Color;
end;

class function TRggColorBase.ColorToIdent(Color: Integer; var Ident: string): Boolean;
begin
  result := IntToColor(Color, Ident, ColorMap);
  if not result then
  begin
    AlphaColorToIdent(Color, Ident); // return value ignored
  end;
end;

class function TRggColorBase.ColorToString(Value: TRggColor): string;
begin
  ColorToIdent(Integer(Value), Result);
end;

class function TRggColorBase.ColorToMapEntry(Value: TRggColor; var MapEntry: TRggColorMapEntry): Integer;
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

class function TRggColorBase.GetColorToMapEntry(Value: TRggColor): TRggColorMapEntry;
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

class function TRggColorBase.ColorToKind(Value: TRggColor): TRggColorKind;
var
  cme: TRggColorMapEntry;
begin
  if ColorToMapEntry(Value, cme) > -1 then
    result := cme.Kind
  else
    result := TRggColorKind.Unknown;
end;

class procedure TRggColorBase.GetColorValues(Proc: TRggGetColorInfoProc);
var
  i: Integer;
begin
  for i := 0 to Length(ColorMap)-1 do
  begin
    Proc(i, ColorMap[i].Kind, ColorMap[i].Value, ColorMap[i].Name);
  end;
end;

class function TRggColorBase.GetMapIndexOfColor(Value: TRggColor): Integer;
var
  cme: TRggColorMapEntry;
begin
  result := ColorToMapEntry(Value, cme);
end;

class function TRggColorBase.ColorToInt(const Ident: string; var Int: Integer; const Map: array of TRggColorMapEntry): Boolean;
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

class function TRggColorBase.IntToColor(Int: Integer; var Ident: string; const Map: array of TRggColorMapEntry): Boolean;
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

class procedure TRggColorBase.InitColorMap;
var
  i: Integer;
begin
  { 140 + 6 = 146 entries }
  SetLength(ColorMap, 146);

  for i := Low(WebColors) to High(WebColors) do
    ColorMap[i] := WebColors[i];

  Assert(High(WebColors) = 139);

  ColorMap[140] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FFF0F0F0), 'Windowgray');
  ColorMap[141] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FFE0E0E0), 'Porcelain');
  ColorMap[142] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FFA0A0A0), 'Mercury');

  ColorMap[143] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FFF9F9F9), 'BackgroundWhite');
  ColorMap[144] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FF372E69), 'BackgroundBlue');
  ColorMap[145] := TRggColorMapEntry.Create(TRggColorKind.CustomColor, Integer($FF333333), 'BackgroundGray');
end;

class procedure TRggColorBase.UpdateColorName(c: TRggColor; s: string);
var
  i: Integer;
begin
  i := GetMapIndexOfColor(c);
  if i > 0 then
    ColorMap[i].Name := s;
end;

{ TRggWebColors }

class constructor TRggWebColors.Create;
begin
  inherited;
  Aliceblue := Alpha or TAlphaColor($F0F8FF);
  Antiquewhite := Alpha or TAlphaColor($FAEBD7);
  Aqua := Alpha or TAlphaColor($00FFFF);
  Aquamarine := Alpha or TAlphaColor($7FFFD4);
  Azure := Alpha or TAlphaColor($F0FFFF);
  Beige := Alpha or TAlphaColor($F5F5DC);
  Bisque := Alpha or TAlphaColor($FFE4C4);
  Black := Alpha or TAlphaColor($000000);
  Blanchedalmond := Alpha or TAlphaColor($FFEBCD);
  Blue := Alpha or TAlphaColor($0000FF);
  Blueviolet := Alpha or TAlphaColor($8A2BE2);
  Brown := Alpha or TAlphaColor($A52A2A);
  Burlywood := Alpha or TAlphaColor($DEB887);
  Cadetblue := Alpha or TAlphaColor($5F9EA0);
  Chartreuse := Alpha or TAlphaColor($7FFF00);
  Chocolate := Alpha or TAlphaColor($D2691E);
  Coral := Alpha or TAlphaColor($FF7F50);
  Cornflowerblue := Alpha or TAlphaColor($6495ED);
  Cornsilk := Alpha or TAlphaColor($FFF8DC);
  Crimson := Alpha or TAlphaColor($DC143C);
  Cyan := Alpha or TAlphaColor($00FFFF);
  Darkblue := Alpha or TAlphaColor($00008B);
  Darkcyan := Alpha or TAlphaColor($008B8B);
  Darkgoldenrod := Alpha or TAlphaColor($B8860B);
  Darkgray := Alpha or TAlphaColor($A9A9A9);
  Darkgreen := Alpha or TAlphaColor($006400);
  Darkkhaki := Alpha or TAlphaColor($BDB76B);
  Darkmagenta := Alpha or TAlphaColor($8B008B);
  Darkolivegreen := Alpha or TAlphaColor($556B2F);
  Darkorange := Alpha or TAlphaColor($FF8C00);
  Darkorchid := Alpha or TAlphaColor($9932CC);
  Darkred := Alpha or TAlphaColor($8B0000);
  Darksalmon := Alpha or TAlphaColor($E9967A);
  Darkseagreen := Alpha or TAlphaColor($8FBC8F);
  Darkslateblue := Alpha or TAlphaColor($483D8B);
  Darkslategray := Alpha or TAlphaColor($2F4F4F);
  Darkturquoise := Alpha or TAlphaColor($00CED1);
  Darkviolet := Alpha or TAlphaColor($9400D3);
  Deeppink := Alpha or TAlphaColor($FF1493);
  Deepskyblue := Alpha or TAlphaColor($00BFFF);
  Dimgray := Alpha or TAlphaColor($696969);
  Dodgerblue := Alpha or TAlphaColor($1E90FF);
  Firebrick := Alpha or TAlphaColor($B22222);
  Floralwhite := Alpha or TAlphaColor($FFFAF0);
  Forestgreen := Alpha or TAlphaColor($228B22);
  Fuchsia := Alpha or TAlphaColor($FF00FF);
  Gainsboro := Alpha or TAlphaColor($DCDCDC);
  Ghostwhite := Alpha or TAlphaColor($F8F8FF);
  Gold := Alpha or TAlphaColor($FFD700);
  Goldenrod := Alpha or TAlphaColor($DAA520);
  Gray := Alpha or TAlphaColor($808080);
  Green := Alpha or TAlphaColor($008000);
  Greenyellow := Alpha or TAlphaColor($ADFF2F);
  Honeydew := Alpha or TAlphaColor($F0FFF0);
  Hotpink := Alpha or TAlphaColor($FF69B4);
  Indianred := Alpha or TAlphaColor($CD5C5C);
  Indigo := Alpha or TAlphaColor($4B0082);
  Ivory := Alpha or TAlphaColor($FFFFF0);
  Khaki := Alpha or TAlphaColor($F0E68C);
  Lavender := Alpha or TAlphaColor($E6E6FA);
  Lavenderblush := Alpha or TAlphaColor($FFF0F5);
  Lawngreen := Alpha or TAlphaColor($7CFC00);
  Lemonchiffon := Alpha or TAlphaColor($FFFACD);
  Lightblue := Alpha or TAlphaColor($ADD8E6);
  Lightcoral := Alpha or TAlphaColor($F08080);
  Lightcyan := Alpha or TAlphaColor($E0FFFF);
  Lightgoldenrodyellow := Alpha or TAlphaColor($FAFAD2);
  Lightgray := Alpha or TAlphaColor($D3D3D3);
  Lightgreen := Alpha or TAlphaColor($90EE90);
  Lightpink := Alpha or TAlphaColor($FFB6C1);
  Lightsalmon := Alpha or TAlphaColor($FFA07A);
  Lightseagreen := Alpha or TAlphaColor($20B2AA);
  Lightskyblue := Alpha or TAlphaColor($87CEFA);
  Lightslategray := Alpha or TAlphaColor($778899);
  Lightsteelblue := Alpha or TAlphaColor($B0C4DE);
  Lightyellow := Alpha or TAlphaColor($FFFFE0);
  Lime := Alpha or TAlphaColor($00FF00);
  Limegreen := Alpha or TAlphaColor($32CD32);
  Linen := Alpha or TAlphaColor($FAF0E6);
  Magenta := Alpha or TAlphaColor($FF00FF);
  Maroon := Alpha or TAlphaColor($800000);
  Mediumaquamarine := Alpha or TAlphaColor($66CDAA);
  Mediumblue := Alpha or TAlphaColor($0000CD);
  Mediumorchid := Alpha or TAlphaColor($BA55D3);
  Mediumpurple := Alpha or TAlphaColor($9370DB);
  Mediumseagreen := Alpha or TAlphaColor($3CB371);
  Mediumslateblue := Alpha or TAlphaColor($7B68EE);
  Mediumspringgreen := Alpha or TAlphaColor($00FA9A);
  Mediumturquoise := Alpha or TAlphaColor($48D1CC);
  Mediumvioletred := Alpha or TAlphaColor($C71585);
  Midnightblue := Alpha or TAlphaColor($191970);
  Mintcream := Alpha or TAlphaColor($F5FFFA);
  Mistyrose := Alpha or TAlphaColor($FFE4E1);
  Moccasin := Alpha or TAlphaColor($FFE4B5);
  Navajowhite := Alpha or TAlphaColor($FFDEAD);
  Navy := Alpha or TAlphaColor($000080);
  Oldlace := Alpha or TAlphaColor($FDF5E6);
  Olive := Alpha or TAlphaColor($808000);
  Olivedrab := Alpha or TAlphaColor($6B8E23);
  Orange := Alpha or TAlphaColor($FFA500);
  Orangered := Alpha or TAlphaColor($FF4500);
  Orchid := Alpha or TAlphaColor($DA70D6);
  Palegoldenrod := Alpha or TAlphaColor($EEE8AA);
  Palegreen := Alpha or TAlphaColor($98FB98);
  Paleturquoise := Alpha or TAlphaColor($AFEEEE);
  Palevioletred := Alpha or TAlphaColor($DB7093);
  Papayawhip := Alpha or TAlphaColor($FFEFD5);
  Peachpuff := Alpha or TAlphaColor($FFDAB9);
  Peru := Alpha or TAlphaColor($CD853F);
  Pink := Alpha or TAlphaColor($FFC0CB);
  Plum := Alpha or TAlphaColor($DDA0DD);
  Powderblue := Alpha or TAlphaColor($B0E0E6);
  Purple := Alpha or TAlphaColor($800080);
  Red := Alpha or TAlphaColor($FF0000);
  Rosybrown := Alpha or TAlphaColor($BC8F8F);
  Royalblue := Alpha or TAlphaColor($4169E1);
  Saddlebrown := Alpha or TAlphaColor($8B4513);
  Salmon := Alpha or TAlphaColor($FA8072);
  Sandybrown := Alpha or TAlphaColor($F4A460);
  Seagreen := Alpha or TAlphaColor($2E8B57);
  Seashell := Alpha or TAlphaColor($FFF5EE);
  Sienna := Alpha or TAlphaColor($A0522D);
  Silver := Alpha or TAlphaColor($C0C0C0);
  Skyblue := Alpha or TAlphaColor($87CEEB);
  Slateblue := Alpha or TAlphaColor($6A5ACD);
  Slategray := Alpha or TAlphaColor($708090);
  Snow := Alpha or TAlphaColor($FFFAFA);
  Springgreen := Alpha or TAlphaColor($00FF7F);
  Steelblue := Alpha or TAlphaColor($4682B4);
  Tan := Alpha or TAlphaColor($D2B48C);
  Teal := Alpha or TAlphaColor($008080);
  Thistle := Alpha or TAlphaColor($D8BFD8);
  Tomato := Alpha or TAlphaColor($FF6347);
  Turquoise := Alpha or TAlphaColor($40E0D0);
  Violet := Alpha or TAlphaColor($EE82EE);
  Wheat := Alpha or TAlphaColor($F5DEB3);
  White := Alpha or TAlphaColor($FFFFFF);
  Whitesmoke := Alpha or TAlphaColor($F5F5F5);
  Yellow := Alpha or TAlphaColor($FFFF00);
  Yellowgreen := Alpha or TAlphaColor($9ACD32);
end;

{ TRggCustomColors }

class constructor TRggCustomColors.Create;
begin
  inherited;

  Windowgray := Alpha or TAlphaColor($F0F0F0);
  Porcelain := Alpha or TAlphaColor($E0E0E0);
  Mercury := Alpha or TAlphaColor($A0A0A0);

//  MoneyGreen := Alpha or TAlphaColor($C0DCC0);
//  LegacySkyBlue := Alpha or TAlphaColor($F0CAA6);
//  Cream := Alpha or TAlphaColor($F0FBFF);

  BackgroundWhite := Alpha or TAlphaColor($F9F9F9);
  BackgroundBlue := Alpha or TAlphaColor($372E69);
  BackgroundGray := Alpha or TAlphaColor($333333);

  { Alternative color names }

  WindowWhite := Windowgray;
  BtnFace := Windowgray;
  ButtonFace := Windowgray;

  RectangleGray := Porcelain;

  QuickSilver := Mercury;
  MedGray := Mercury;
  MediumGray := Mercury;

  DarkSilver := Darkgray;

  Dovegray := Dimgray;
end;

class procedure TRggCustomColors.UpdateColorNames;
begin
  { use custom names }
  UpdateColorName(Darkgray, 'DarkSilver');
  UpdateColorName(Dimgray, 'Dovegray');

  UpdateColorName(Windowgray, 'WindowWhite');
  UpdateColorName(Porcelain, 'RectangleGray');
  UpdateColorName(Mercury, 'QuickSilver');
end;

class procedure TRggCustomColors.RevertColorNames;
begin
  { revert to original web color names }
  UpdateColorName(Darkgray, 'Darkgray');
  UpdateColorName(Dimgray, 'Dimgray');

  { revert to self chosen 'default' names for custom colors }
  UpdateColorName(Windowgray, 'Windowgray');
  UpdateColorName(Porcelain, 'Porcelain');
  UpdateColorName(Mercury, 'Mercury');
end;

{ TRggColorTest }

class procedure TRggColorTest.OutputWebColorTable(ML: TStrings);
begin
  OutputColorTable(ML, True);
end;

class procedure TRggColorTest.OutputColorTable(ML: TStrings; WebOnly: Boolean = False);
var
  fs: string;
  s: string;
  i: Integer;
  c: TRggColor;
  r, g, b: Byte;
  ck: TRggColorKind;
  k: Char;
begin
  fs := '%3d | %s | %20s = %x = (%3d, %3d, %3d)';
  for i := 0 to Length(ColorMap)-1 do
  begin
    ck := ColorMap[i].Kind;
    k := TRggColorMapEntry.ColorKindToChar(ck);
    if WebOnly and (ck <> TRggColorKind.WebColor) then
      Continue;
    c := ColorMap[i].Value;
    s := ColorToString(c);
    r := TAlphaColorRec(c).R;
    g := TAlphaColorRec(c).G;
    b := TAlphaColorRec(c).B;
    ML.Add(Format(fs, [i, k, s, c, r, g, b]));
  end;
end;

class procedure TRggColorTest.OutputGrayTable(ML: TStrings);
var
  i: Integer;
  fs: string;
  s: string;
  r, g, b: Byte;
  k: char;

  procedure Test(c: TRggColor);
  begin
    k := TRggColorMapEntry.ColorKindToChar(GetColorToMapEntry(c).Kind);
    s := ColorToString(c);
    r := TAlphaColorRec(c).R;
    g := TAlphaColorRec(c).G;
    b := TAlphaColorRec(c).B;
    ML.Add(Format(fs, [i, k, s, c, r, g, b]));
    Inc(i);
  end;
begin
  i := 0;
  fs := '%3d | %s | %20s = %x = (%3d, %3d, %3d)';
  Test(TRggColors.Whitesmoke);
  Test(TRggColors.WindowWhite);
  Test(TRggColors.RectangleGray);
  Test(tRggColors.Gainsboro);
  Test(TRggColors.Lightgray);
  Test(TRggColors.Silver);
  Test(TRggColors.Darkgray);
  Test(TRggColors.QuickSilver);
  Test(TRggColors.Gray);
  Test(TRggColors.Dimgray);

//0: [   Whitesmoke F5 245]:   Whitesmoke = FFF5F5F5 = (245, 245, 245)
//1: [  WindowWhite F0 240]:   Windowgray = FFF0F0F0 = (240, 240, 240)
//2: [RectangleGray E0 224]:    Porcelain = FFE0E0E0 = (224, 224, 224)
//3: [    Gainsboro DC 220]:    Gainsboro = FFDCDCDC = (220, 220, 220)
//4: [    Lightgray D3 211]:    Lightgray = FFD3D3D3 = (211, 211, 211)
//5: [       Silver C0 192]:       Silver = FFC0C0C0 = (192, 192, 192)
//6: [   DarkSilver A9 169]:     Darkgray = FFA9A9A9 = (169, 169, 169)
//7: [  QuickSilver A0 160]:      Mercury = FFA0A0A0 = (160, 160, 160)
//8: [         Gray 80 128]:         Gray = FF808080 = (128, 128, 128)
//9: [     Dovegray 69 105]:      Dimgray = FF696969 = (105, 105, 105)

end;

class procedure TRggColorTest.OutputAlternativeNameTable(ML: TStrings);
var
  i: Integer;
  fs: string;
  s: string;
  r, g, b: Byte;
  k: char;

  procedure Test(c: TRggColor);
  begin
    k := TRggColorMapEntry.ColorKindToChar(GetColorToMapEntry(c).Kind);
    s := ColorToString(c);
    r := TAlphaColorRec(c).R;
    g := TAlphaColorRec(c).G;
    b := TAlphaColorRec(c).B;
    ML.Add(Format(fs, [i, k, s, c, r, g, b]));
    Inc(i);
  end;
begin
  i := 0;
  fs := '%3d | %s | %20s = %x = (%3d, %3d, %3d)';

  Test(TRggColors.WindowWhite);
  Test(TRggColors.RectangleGray);
  Test(TRggColors.QuickSilver);

  Test(TRggColors.Darkgray);
  Test(TRggColors.Dimgray);
end;

end.
