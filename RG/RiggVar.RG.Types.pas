unit RiggVar.RG.Types;

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

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.Math.Vectors;

const
  LineCount = 100;
  CPMax = 50;
  CLMax = 50;
  BogenMax = 50;
  TransKreisRadius = 32;

var
  WantLogoData: Boolean;
  TKR: Integer = TranskreisRadius;

type
  EFileFormatError = class(Exception);

  TFederParam = (
    fpController,
    fpWinkel,
    fpVorstag,
    fpWante,
    fpWoben,
    fpSalingH,
    fpSalingA,
    fpSalingL,
    fpVorstagOS,
    fpWPowerOS,
    fpSalingW,
    fpMastfallF0C,
    fpMastfallF0F,
    fpMastfallVorlauf,
    fpBiegung,
    fpD0X,
    fpAPW,
    fpEAH,
    fpEAR,
    fpEI,
    fprx,
    fpry,
    fprz,
    fptx,
    fpty,
    fpcz,
    fppx,
    fppy,
    fppz,
    fpva,
    fpnp,
    fpfp
    );

  TFederMessageKind = (
    fmkNoop,
    fmkAction,
    fmkParam,
    fmkParamValue,

    fmkTX,
    fmkTY,
    fmkRX,
    fmkRY,
    fmkRZ,
    fmkCZ
  );

  TGraphRadio = (
    gSimple,
    gNormal,
    gBlau,
    gGrau,
    gMulti,
    gDisplay,
    gQuick
  );

  TYAchseValue = (
    yavDurchbiegungHD,
    yavMastfallF0F,
    yavMastfallF0C,
    yavVorstagSpannung,
    yavWantenSpannung,
    yavAuslenkungC,
    yavRF00,
    yavRF01,
    yavRF03,
    yavRF05,
    yavRF06,
    yavRF10,
    yavRF11,
    yavRF13
    );

  TYAchseRecord = record
    ComboIndex: Integer; { ComboIndex: TYAchseValue; }
    ArrayIndex: Integer; { ArrayIndex: 0..ANr-1 }
    ComboText: string;
    Text: string;
  end;

  TYAchseRecordList = array [TYAchseValue] of TYAchseRecord;

  TsbName = TFederParam.fpController .. TFederParam.fpWPowerOS;

  TsbParam = (IstValue, MinValue, MaxValue, TinyStep, BigStep);
  TsbLabelArray = array [TsbName] of string;
  TTabellenTyp = (itKonstante, itGerade, itParabel, itBezier);
  TViewPoint = (vpSeite, vpAchtern, vpTop, vp3D);
  TSalingTyp = (stFest, stDrehbar, stOhneBiegt, stOhneStarr);
  TControllerTyp = (ctOhne, ctDruck, ctZugDruck);
  TGetriebeStatus = (gsWanteZukurz, gsWanteZulang, gsErrorPsivonPhi);
  TRiggStatus = (rsNichtEntspannbar, rsWanteAufDruck, rsKraftZuGross);

  TCalcTyp = (
    ctQuerKraftBiegung,
    ctBiegeKnicken,
    ctKraftGemessen
    );

  TMastStatus = (
    msBiegungNegativ,
    msControllerJenseits,
    msZugKraftimMast,
    msControllerKraftzuGross
    );

  Linie = array [0 .. LineCount] of TPoint;
  TLineDataR150 = array[0..150] of single;
  TLineDataR100 = array [0 .. 100] of single;
  TLineDataR50 = array [0 .. 50] of single;
  TChartLine = array [0 .. CLMax] of single;
  TChartLineData = array [0 .. CPMax] of single;

  TKoordLine = array [0 .. 100] of TPoint3D;

  TRiggPoint = (
    ooN0,
    ooA0,
    ooB0,
    ooC0,
    ooD0,
    ooE0,
    ooF0,
    ooP0,
    ooA,
    ooB,
    ooC,
    ooD,
    ooE,
    ooF,
    ooP,
    ooM
    );

  TRiggRod = (
    D0C, // Mast
    C0D0, // Vorstag - Mastfuß
    B0C0, // Pütting Bb - Vorstag
    A0C0, // Pütting Stb - Vorstag
    B0D0, // Pütting Bb - Mastfuß
    A0D0, // Pütting Stb - Mastfuß
    A0B0, // Püttingabstand
    B0B, // Wante unten Bb
    A0A, // Wante unten Stb
    BD, // Saling Bb
    AD, // Saling Stb
    AB, // Saling-Verbindung
    BC, // Wante oben Bb
    AC, // Wante oben Stb
    C0C, // Vorstag
    DC, // Mast Oben
    D0D, // Mast Unten
    ED,
    D0E,
    E0E // Controller
  );

  TRiggPointIndexRange = 0 .. 15;
  TRiggPoints = record
    class function CoordName(Index: TRiggPoint): string; static;
    class function CoordLongNameDE(Index: TRiggPoint): string; static;
    class function CoordLongNameEN(Index: TRiggPoint): string; static;
    case Integer of
      0: (V: array [TRiggPoint] of TPoint3D);
      1: (
        N0: TPoint3D;
        A0: TPoint3D;
        B0: TPoint3D;
        C0: TPoint3D;
        D0: TPoint3D;
        E0: TPoint3D;
        F0: TPoint3D;
        P0: TPoint3D;
        A: TPoint3D;
        B: TPoint3D;
        C: TPoint3D;
        D: TPoint3D;
        E: TPoint3D;
        F: TPoint3D;
        P: TPoint3D;
        M: TPoint3D;)
  end;

  TMastKurve = array [0..BogenMax] of TPoint3D;

  TRiggRodIndexRange = 0 .. 19;
  TRiggRods = record
    class function AbstandShortName(Index: TRiggRodIndexRange): string; static;
    class function AbstandNameDE(Index: TRiggRodIndexRange): string; static;
    class function AbstandNameEN(Index: TRiggRodIndexRange): string; static;
    case Integer of
      0: (V: array [0 .. 19] of single);
      1: (
        D0C: single; // Mast
        C0D0: single; // Vorstag - Mastfuß
        B0C0: single; // Pütting Bb - Vorstag
        A0C0: single; // Pütting Stb - Vorstag
        B0D0: single; // Pütting Bb - Mastfuß
        A0D0: single; // Pütting Stb - Mastfuß
        A0B0: single; // Püttingabstand
        B0B: single; // Wante unten Bb
        A0A: single; // Wante unten Stb
        BD: single; // Saling Bb
        AD: single; // Saling Stb
        AB: single; // Saling-Verbindung
        BC: single; // Wante oben Bb
        AC: single; // Wante oben Stb
        C0C: single; // Vorstag
        DC: single; // Mast Oben
        D0D: single; // Mast Unten
        ED: single;
        D0E: single;
        E0E: single; // Controller
      );
      2: (Rod: array [TRiggRod] of single);
  end;

  TTrimm = record
    Mastfall: Integer;
    Spannung: Integer;
    BiegungS: Integer;
    BiegungC: Integer;
    FlexWert: Integer;
  end;

  TTrimmIndex = (
    tiMastfallF0F,
    tiMastfallF0C,
    tiVorstagDiff,
    tiVorstagDiffE,
    tiSpannungW,
    tiSpannungV,
    tiBiegungS,
    tiBiegungC,
    tiFlexWert
    );

  TRealTrimm = record
    MastfallF0F: single;
    VorstagDiff: single;
    VorstagDiffE: single;
    SpannungW: single;
    SpannungV: single;
    BiegungS: single;
    BiegungC: single;
    FlexWert: single;
  end;

  TTrimmControls = record
    Controller: Integer;
    Winkel: Integer;
    Vorstag: Integer;
    Wanten: Integer;
    Woben: Integer;
    Wunten: Integer;
    SalingH: Integer;
    SalingA: Integer;
    SalingL: Integer;
    WPowerOS: Integer;
  end;

  TStreamData = record
    Boot: Integer;
    No: Integer;
    Controller: Integer;
    Winkel: Integer;
    Vorstag: Integer;
    Wante: Integer;
    Woben: Integer;
    SalingH: Integer;
    SalingA: Integer;
    SalingL: Integer;
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
  end;

  TSalingDaten = record
    SalingH: single;
    SalingA: single;
    SalingL: single;
    SalingW: single;
    WantenWinkel: single; { in degrees }
    KraftWinkel: single; { in degrees }
  end;

  TTrimmTabDaten = record { y = a0 + a1*(x-x0) + a2*(x-x1)(x-x0) }
    TabellenTyp: TTabellenTyp;
    a0: single; { a0 = y0 } { a0 ist immer Null }
    a1: single; { a1 = (y1-y0)/(x1-x0) }
    a2: single; { a2 = ((y2-y1)/(x2-x1) - a1)/(x2-x0) }
    x0: single; { KraftAnfang - immer Null }
    x1: single; { KraftMitte }
    x2: single; { KraftEnde } { wird benötigt für Begrenzung }
  end;
  { Biegeknicken }
  (* wird in der Trimmtabelle untergebracht:
    FKorrigiert: Boolean;
    FExcenter: single; { in mm }
    FKnicklaenge: single; { in mm }
    FKorrekturFaktor: single; { dimensionslos }
  *)

  TKoordLabels = array [TRiggPoint] of string;

  TKurvenTyp = (KurveOhneController, KurveMitController);
  TMastStatusSet = set of TMastStatus;

  TMastGraphModel = class
  public
    FLineCountM: Integer;
    LineData: TLineDataR100; { Durchbiegungswerte in mm }
    GetriebeOK: Boolean;
  end;

  TKraftKurven = class
  public
    KurveOhne: TLineDataR150;
    KurveMit: TLineDataR150;
    KurveVerschoben: TLineDataR150;
    KurveVerschobenKorrigiert: TLineDataR150;

    KurveOhneKorrigiert: TLineDataR100;
    KurveMitKorrigiert: TLineDataR100;

    ShowAll: Boolean;

    ControllerTyp: TControllerTyp;
    Korrigiert: Boolean;
    KoppelFaktor: single;
    SalingAlpha: single;

    hd: single;
    FC: single;
  end;

  TRiggAngles = record
    alpha: single;
    alpha1: single;
    alpha2: single;
    beta: single;
    gamma: single;
    delta1: single;
    delta2: single;
    epsilon: single;
    phi: single;
    psi: single;
  end;

  TRegelGraphData = class
  public
    Anfang, Antrieb, Ende: single;
    LimitA, LimitB, TrySalingH: single;
    KurveF: TChartLine;
  end;

  TRggTestData = class
  public
    class function GetKoordinaten420: TRiggPoints; static;
    class function GetMastKurve420: TMastKurve; static;
  end;

  RggMaterial = record
  const
    EModulStahl = 210E3; { N / mm^2 }
    EModulAlu = 70E3; { N / mm^2 }
    EAgross = 100E6; { N }
    EARumpf = 10E6; { N }
    EASaling = 1E6; { N }
  end;

const
  ZeroCtrl: TTrimmControls = (
    Controller: 0;
    Winkel: 0;
    Vorstag: 0;
    Wanten: 0;
    Woben: 0;
    Wunten: 0;
    SalingH: 0;
    SalingA: 0;
    SalingL: 0;
    WPowerOS: 0
    );

  DefaultStreamData: TStreamData = (
    Boot: 0;
    No: 0;
    Controller: 100;
    Winkel: 95;
    Vorstag: 4500;
    Wante: 4120;
    Woben: 2020;
    SalingH: 242;
    SalingA: 850;
    SalingL: 489;
    SalingTyp: TSalingTyp.stFest;
    ControllerTyp: TControllerTyp.ctOhne
    );

  DefaultTrimmTabDaten: TTrimmTabDaten = (
    TabellenTyp: TTabellenTyp.itGerade;
    a0: 0; { not used }
    a1: 0.1;
    a2: 0;
    x0: 0; { not used }
    x1: 500;
    x2: 1000
    );

//  KoordLabels: TKoordLabels = (
//    'Basispunkt',
//    'Pütting Stb',
//    'Pütting Bb',
//    'Vorstag Boot',
//    'Mastfuß',
//    'Controller E0',
//    'SpiegelPunkt',
//    'Punkt P0',
//    'Saling Stb',
//    'Saling Bb',
//    'Vorstag',
//    'Saling Mast',
//    'Controller',
//    'Masttop',
//    'Punkt P',
//    'Punkt M'
//    );

  XMLKoordLabels: TKoordLabels = (
    'Basispunkt',
    'Puetting Stb',
    'Puetting Bb',
    'Vorstag Boot',
    'Mastfuss',
    'Controller Boot',
    'SpiegelPunkt',
    'Punkt P0',
    'Saling Stb',
    'Saling Bb',
    'Vorstag',
    'Saling Mast',
    'Controller',
    'Masttop',
    'Punkt P',
    'Punkt M'
    );

  KoordTexte: TKoordLabels = ('N0',
    'A0', 'B0', 'C0', 'D0', 'E0', 'F0', 'P0',
    'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'P ', 'M '
    );

  KoordTexteXML: TKoordLabels = ('N0',
    'A0', 'B0', 'C0', 'D0', 'E0', 'F0', 'P0',
    'A',  'B',  'C',  'D',  'E',  'F',  'P',  'M'
    );

//  AbstandLabels: array[0..19] of string = (
//    'D0C Mast',
//    'C0D0 Vorstag - Mastfuß',
//    'B0C0 Pütting Bb - Vorstag',
//    'A0C0 Pütting Stb - Vorstag',
//    'B0D0 Pütting Bb - Mastfuß',
//    'A0D0 Pütting Stb - Mastfuß',
//    'A0B0 Püttingabstand',
//    'B0B Wante unten Bb',
//    'A0A Wante unten Stb',
//    'BD Saling Bb',
//    'AD Saling Stb',
//    'AB Saling-Verbindung',
//    'BC Wante oben Bb',
//    'AC Wante oben Stb',
//    'C0C Vorstag',
//    'DC Mast',
//    'D0D Mast',
//    'ED Mast',
//    'D0E Mast',
//    'E0E Controller'
//    );

//  ParamLabels: TsbLabelArray = (
//    { Controller: } 'Zustellung Mast-Controller [mm]',
//    { Winkel: } 'Winkel [Grad]',
//    { Vorstag: } 'Vorstaglänge [mm]',
//    { Wante: } 'Wantenlänge [mm]',
//    { Woben: } 'Länge des oberen Wantenabschnitts [mm]',
//    { SalingH: } 'Höhe des Salingdreiecks [mm]',
//    { SalingA: } 'Saling-Abstand [mm]',
//    { SalingL: } 'Saling-Länge [mm]',
//    { VorstagOS: } 'Vorstaglänge [mm]', { not used }
//    { WPowerOS: } 'Wantenspannung [N]' { not used }
//    );

  XMLSBName: array[TsbName] of string = (
    'E0E',
    'Alpha',
    'C0C',
    'A0AC',
    'AC',
    'PD',
    'AB',
    'AD',
    'VorstagOS',
    'WKraftOS'
    );

  XMLSBNameLabels: array[TsbName] of string = (
    'Controller',
    'Winkel',
    'Vorstag',
    'Wante',
    'WanteOben',
    'SalingHoehe',
    'SalingAbstand',
    'SalingLaenge',
    'VorstagOS',
    'WantenspannungOS'
    );

  XMLSBParamLabels: array[TsbParam] of string = (
    'Value',
    'Min',
    'Max',
    'Small',
    'Big'
   );

procedure InitYAchseRecordList(out RecordList: TYAchseRecordList);
function StrToRiggPoint(const s: string): TRiggPoint;

implementation

procedure InitYAchseRecordList(out RecordList: TYAchseRecordList);
begin
  with RecordList[yavDurchbiegungHD] do
  begin
    ComboText := 'Durchbiegung hd';
    Text := 'Mastbiegung in Salinghöhe [mm]';
    ComboIndex := 0;
    ArrayIndex := 0;
  end;
  with RecordList[yavMastfallF0F] do
  begin
    ComboText := 'Mastfall F0F';
    Text := 'Mastfall FOF [mm]';
    ComboIndex := 1;
    ArrayIndex := 1;
  end;
  with RecordList[yavMastfallF0C] do
  begin
    ComboText := 'Mastfall F0C';
    Text := 'Mastfall F0C [mm]';
    ComboIndex := 2;
    ArrayIndex := 2;
  end;
  with RecordList[yavVorstagSpannung] do
  begin
    ComboText := 'Vorstag-Spannung';
    Text := 'Vorstagspannung [N]';
    ComboIndex := 3;
    ArrayIndex := 3;
  end;
  with RecordList[yavWantenSpannung] do
  begin
    ComboText := 'Wanten-Spannung';
    Text := 'Wantenspannung [N]';
    ComboIndex := 4;
    ArrayIndex := 4;
  end;
  with RecordList[yavAuslenkungC] do
  begin
    ComboText := 'Elastizität Punkt C';
    Text := 'Auslenkung Punkt C [mm]';
    ComboIndex := 5;
    ArrayIndex := 5;
  end;
  with RecordList[yavRF00] do
  begin
    ComboText := 'rF[0] MastDruck';
    Text := 'Kraft im Stab D0C [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF01] do
  begin
    ComboText := 'rF[1] Kraft D0C0';
    Text := 'Kraft im Stab D0C0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF03] do
  begin
    ComboText := 'rF[3] Kraft A0C0';
    Text := 'Kraft im Stab A0C0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF05] do
  begin
    ComboText := 'rF[5] Kraft A0D0';
    Text := 'Kraft im Stab A0D0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF06] do
  begin
    ComboText := 'rF[6] Kraft A0B0';
    Text := 'Kraft im Stab A0B0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF10] do
  begin
    ComboText := 'rF[10] Kraft AD';
    Text := 'Kraft im Stab AD [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF11] do
  begin
    ComboText := 'rF[11] Kraft AB';
    Text := 'Kraft im Stab AB [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF13] do
  begin
    ComboText := 'rF[13] Kraft AC';
    Text := 'Kraft im Stab AC [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
end;

function StrToRiggPoint(const s: string): TRiggPoint;
begin
  result := ooD0;
  if s = 'A0' then
    result := ooA0
  else if s = 'B0' then
    result := ooB0
  else if s = 'C0' then
    result := ooC0
  else if s = 'D0' then
    result := ooD0
  else if s = 'E0' then
    result := ooE0
  else if s = 'F0' then
    result := ooF0
  else if s = 'A' then
    result := ooA
  else if s = 'B' then
    result := ooB
  else if s = 'C' then
    result := ooC
  else if s = 'D' then
    result := ooD
  else if s = 'E' then
    result := ooE
  else if s = 'F' then
    result := ooF;
end;

{ TRiggLrecord }

class function TRiggRods.AbstandShortName(Index: TRiggRodIndexRange): string;
begin

  case Index of
    0: result := 'D0C';
    1: result := 'C0D0';
    2: result := 'B0C0';
    3: result := 'A0C0';
    4: result := 'B0D0';
    5: result := 'A0D0';
    6: result := 'A0B0';
    7: result := 'B0B';
    8: result := 'A0A';
    9: result := 'BD';
    10: result := 'AD';
    11: result := 'AB';
    12: result := 'BC';
    13: result := 'AC';
    14: result := 'C0C';
    15: result := 'DC';
    16: result := 'D0D';
    17: result := 'ED';
    18: result := 'D0E';
    19: result := 'E0E';
  end;
end;

class function TRiggRods.AbstandNameDE(Index: TRiggRodIndexRange): string;
begin
  case Index of
    0: result := 'D0C Mast';
    1: result := 'C0D0 Vorstag - Mastfuß';
    2: result := 'B0C0 Pütting Bb - Vorstag';
    3: result := 'A0C0 Pütting Stb - Vorstag';
    4: result := 'B0D0 Pütting Bb - Mastfuß';
    5: result := 'A0D0 Pütting Stb - Mastfuß';
    6: result := 'A0B0 Püttingabstand';
    7: result := 'B0B Wante unten Bb';
    8: result := 'A0A Wante unten Stb';
    9: result := 'BD Saling Bb';
    10: result := 'AD Saling Stb';
    11: result := 'AB Saling-Verbindung';
    12: result := 'BC Wante oben Bb';
    13: result := 'AC Wante oben Stb';
    14: result := 'C0C Vorstag';
    15: result := 'DC';
    16: result := 'D0D';
    17: result := 'ED';
    18: result := 'D0E';
    19: result := 'E0E Controller';
  end;
end;

class function TRiggRods.AbstandNameEN(Index: TRiggRodIndexRange): string;
begin
  case Index of
    0: result := 'D0C Mast';
    1: result := 'C0D0 Headstay - Mastfoot';
    2: result := 'B0C0 Chain plate left - Headstay';
    3: result := 'A0C0 Chain plate right - Headstay';
    4: result := 'B0D0 Chain plate left - Mastfoot';
    5: result := 'A0D0 Chain plate right - Mastfoot';
    6: result := 'A0B0 Chain plate distance';
    7: result := 'B0B Shroud lower part left';
    8: result := 'A0A Shroud lower part right';
    9: result := 'BD Spreader left';
    10: result := 'AD Spreader right';
    11: result := 'AB Spreader - Connection';
    12: result := 'BC Shroud upper part left';
    13: result := 'AC Shroud upper part right';
    14: result := 'C0C Headstay';
    15: result := 'DC';
    16: result := 'D0D';
    17: result := 'ED';
    18: result := 'D0E';
    19: result := 'E0E Mast controller';
  end;
end;

class function TRggTestData.GetKoordinaten420: TRiggPoints;
var
  rp: TRiggPoints;
begin
  rp.A.X := 2398;
  rp.A.Y := -425;
  rp.A.Z := 2496;

  rp.B.X := 2398;
  rp.B.Y := 425;
  rp.B.Z := 2496;

  rp.C.X := 2354;
  rp.C.Y := 0;
  rp.C.Z := 4470;

  rp.D.X := 2618;
  rp.D.Y := 0;
  rp.D.Z := 2488;

  rp.E.X := 2870;
  rp.E.Y := 0;
  rp.E.Z := 450;

  rp.F.X := 2142;
  rp.F.Y := 0;
  rp.F.Z := 5970;

  rp.P.X := 2398;
  rp.P.Y := 0;
  rp.P.Z := 2496;

  rp.A0.X := 2560;
  rp.A0.Y := -765;
  rp.A0.Z := 430;

  rp.B0.X := 2560;
  rp.B0.Y := 765;
  rp.B0.Z := 430;

  rp.C0.X := 4140;
  rp.C0.Y := 0;
  rp.C0.Z := 340;

  rp.D0.X := 2870;
  rp.D0.Y := 0;
  rp.D0.Z := -100;

  rp.E0.X := 2970;
  rp.E0.Y := 0;
  rp.E0.Z := 450;

  rp.F0.X := -30;
  rp.F0.Y := 0;
  rp.F0.Z := 300;

  rp.P0.X := 2560;
  rp.P0.Y := 0;
  rp.P0.Z := 430;

  rp.M.X := 0;
  rp.M.Y := 0;
  rp.M.Z := 0;

  result := rp;
end;

class function TRggTestData.GetMastKurve420: TMastKurve;

  procedure Add(i: Integer; u, w: single);
  begin
    result[i].X := u;
    result[i].Y := 0;
    result[i].Z := w;
  end;

begin
  Add(0, 2870, -100);
  Add(1, 2861.97363891765, -8.3355171003159);
  Add(2, 2853.94047232883, 83.328196833267);
  Add(3, 2845.89369591162, 174.990372968493);
  Add(4, 2837.82650439645, 266.650242366029);
  Add(5, 2829.73209132921, 358.307035952698);
  Add(6, 2821.60365309872, 449.959984976552);
  Add(7, 2813.43438467231, 541.608320525026);
  Add(8, 2805.21748101735, 633.251273685558);
  Add(9, 2796.94613520588, 724.888075331433);
  Add(10, 2788.61354315291, 816.517956657164);
  Add(11, 2780.21290077342, 908.140148857262);
  Add(12, 2771.73740019184, 999.753882697938);
  Add(13, 2763.18024111372, 1091.35838980201);
  Add(14, 2754.53461376821, 1182.95290072153);
  Add(15, 2745.79371407029, 1274.53664665101);
  Add(16, 2736.95073983023, 1366.10885899912);
  Add(17, 2727.99887938189, 1457.66876810377);
  Add(18, 2718.9313381167, 1549.21560623022);
  Add(19, 2709.74130057792, 1640.74860328808);
  Add(20, 2700.42196647111, 1732.26699090016);
  Add(21, 2690.9665279207, 1823.76999983268);
  Add(22, 2681.36818463225, 1915.25686170844);
  Add(23, 2671.62012873018, 2006.72680729367);
  Add(24, 2661.71555233891, 2098.17906735456);
  Add(25, 2651.647655164, 2189.61287351393);
  Add(26, 2641.40962932988, 2281.02745653800);
  Add(27, 2630.99467075153, 2372.42204762127);
  Add(28, 2620.39597534394, 2463.79587795826);
  Add(29, 2609.60753125263, 2555.14826825899);
  Add(30, 2598.6323974729, 2646.47956416471);
  Add(31, 2587.47937572365, 2737.79076019706);
  Add(32, 2576.15735869758, 2829.08286115696);
  Add(33, 2564.67524287799, 2920.35687227365);
  Add(34, 2553.0419209576, 3011.13798348070);
  Add(35, 2541.26628562912, 3102.85464418115);
  Add(36, 2529.35723716643, 3194.08041543042);
  Add(37, 2517.32366636695, 3285.29211668267);
  Add(38, 2505.17446781869, 3376.49075295298);
  Add(39, 2492.91853610965, 3467.67732925643);
  Add(40, 2480.56476582785, 3558.85285060811);
  Add(41, 2468.12205156128, 3650.01832202311);
  Add(42, 2455.59928600265, 3741.17474830234);
  Add(43, 2443.00536563528, 3832.32313467506);
  Add(44, 2430.34918504716, 3923.46448615634);
  Add(45, 2417.639636931, 4014.59980754712);
  Add(46, 2404.8856177701, 4105.73010407663);
  Add(47, 2392.09601978336, 4196.85638049227);
  Add(48, 2379.27973898025, 4287.97964196973);
  Add(49, 2366.44566947495, 4379.10089347056);
  Add(50, 2353.60270538165, 4470.22113995631);
end;

{ TRiggPoints }

class function TRiggPoints.CoordName(Index: TRiggPoint): string;
begin
  case Index of
    ooN0: result := 'N0';
    ooA0: result := 'A0';
    ooB0: result := 'B0';
    ooC0: result := 'C0';
    ooD0: result := 'D0';
    ooE0: result := 'E0';
    ooF0: result := 'F0';
    ooP0: result := 'P0';
    ooA: result := 'A';
    ooB: result := 'B';
    ooC: result := 'C';
    ooD: result := 'D';
    ooE: result := 'E';
    ooF: result := 'F';
    ooP: result := 'P';
    ooM: result := 'M';
  end;
end;

class function TRiggPoints.CoordLongNameEN(Index: TRiggPoint): string;
begin
  case Index of
    ooN0: result := 'Base Point N0';
    ooA0: result := 'Chain Plate Stb';
    ooB0: result := 'Chain Plate Bb';
    ooC0: result := 'Headstay Hull';
    ooD0: result := 'Mastfoot D0';
    ooE0: result := 'Controller E0';
    ooF0: result := 'Stern Point F0';
    ooP0: result := 'Point P0';
    ooA: result := 'Spreader Stb';
    ooB: result := 'Spreader Bb';
    ooC: result := 'Headstay';
    ooD: result := 'Spreader Mast';
    ooE: result := 'Controller';
    ooF: result := 'Mast Top';
    ooP: result := 'Point P';
    ooM: result := 'Point M';
  end;
end;

class function TRiggPoints.CoordLongNameDE(Index: TRiggPoint): string;
begin
  case Index of
    ooN0: result := 'Basispunkt';
    ooA0: result := 'Pütting Stb';
    ooB0: result := 'Pütting Bb';
    ooC0: result := 'Vorstag Boot';
    ooD0: result := 'Mastfuß';
    ooE0: result := 'Controller E0';
    ooF0: result := 'SpiegelPunkt';
    ooP0: result := 'Punkt P0';
    ooA: result := 'Saling Stb';
    ooB: result := 'Saling Bb';
    ooC: result := 'Vorstag';
    ooD: result := 'Saling Mast';
    ooE: result := 'Controller';
    ooF: result := 'Masttop';
    ooP: result := 'Punkt P';
    ooM: result := 'Punkt M';
  end;
end;

end.
