unit RggTypes;

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
  System.Math.Vectors,
  RiggVar.RG.Def;

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
  TRealPoint = TPoint3D;

  EFileFormatError = class(Exception);

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

//  TsbName = (
//    Controller,
//    Winkel,
//    Vorstag,
//    Wante,
//    Woben,
//    SalingH,
//    SalingA,
//    SalingL,
//    VorstagOS,
//    WPowerOS
//    );

  TsbParam = (Ist, Min, Max, TinyStep, BigStep);
//  TsbArray = array [TsbName, TsbParam] of Integer;
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
  TLineDataR100 = array [0 .. 100] of single;
  TLineDataR50 = array [0 .. 50] of single;
  TChartLine = array [0 .. CLMax] of single;
  TChartLineData = array [0 .. CPMax] of single;

  TKoord = (x, y, z);
//  TRealPoint = array [TKoord] of single;
  TKoordLine = array [0 .. 100] of TPoint3D;
//  TIntPoint = TPoint3D;

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

  TIntRiggPoints = array [TRiggPoint] of TPoint3D; // TIntPoint;
  TRealRiggPoints = array [TRiggPoint] of TPoint3D;
  TMastKurve = array [0..BogenMax] of TPoint3D;
  TRggPolyLine = array of TPoint;

  TRiggLvektor = array [0 .. 19] of single;
  TRiggLIndexRange = 0 .. 19;

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
    a0: 0; {zur Zeit nicht verwendet}
    a1: 0.1;
    a2: 0;
    x0: 0; {zur Zeit nicht verwendet}
    x1: 500;
    x2: 1000
    );

  KoordLabels: TKoordLabels = (
    'Basispunkt',
    'Pütting Stb',
    'Pütting Bb',
    'Vorstag Boot',
    'Mastfuß',
    'Controller E0',
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
    'A', 'B', 'C', 'D', 'E', 'F', 'P', 'M'
    );

  AbstandLabels: array[0..19] of string = (
    'D0C Mast',
    'C0D0 Vorstag - Mastfuß',
    'B0C0 Pütting Bb - Vorstag',
    'A0C0 Pütting Stb - Vorstag',
    'B0D0 Pütting Bb - Mastfuß',
    'A0D0 Pütting Stb - Mastfuß',
    'A0B0 Püttingabstand',
    'B0B Wante unten Bb',
    'A0A Wante unten Stb',
    'BD Saling Bb',
    'AD Saling Stb',
    'AB Saling-Verbindung',
    'BC Wante oben Bb',
    'AC Wante oben Stb',
    'C0C Vorstag',
    'DC Mast',
    'D0D Mast',
    'ED Mast',
    'D0E Mast',
    'E0E Controller'
    );

  ParamLabels: TsbLabelArray = (
    { Controller: } 'Zustellung Mast-Controller [mm]',
    { Winkel: } 'Winkel [Grad]',
    { Vorstag: } 'Vorstaglänge [mm]',
    { Wante: } 'Wantenlänge [mm]',
    { Woben: } 'Länge des oberen Wantenabschnitts [mm]',
    { SalingH: } 'Höhe des Salingdreiecks [mm]',
    { SalingA: } 'Saling-Abstand [mm]',
    { SalingL: } 'Saling-Länge [mm]',
    { VorstagOS: } 'Vorstaglänge [mm]', { wird nicht benutzt }
    { WPowerOS: } 'Wantenspannung [N]' { wird nicht benutzt }
    );

  XMLSBName: array[TsbName] of string = (
    'E0E', // Controller
    'Alpha', // Winkel
    'C0C', // Vorstag
    'A0AC', // Wante
    'AC', // Woben
    'PD', // SalingH
    'AB', // SalingA
    'AD', // SalingL
    'VorstagOS', // VorstagOS
    'WKraftOS' // WPowerOS
    );

  XMLSBNameLabels: array[TsbName] of string = (
    'Controller', // Controller
    'Winkel', // Winkel
    'Vorstag', // Vorstag
    'Wante', // Wante
    'WanteOben', // Woben
    'SalingHoehe', // SalingH
    'SalingAbstand', // SalingA
    'SalingLaenge', // SalingL
    'VorstagOS', // VorstagOS
    'WantenspannungOS' // WPowerOS
    );

  XMLSBParamLabels: array[TsbParam] of string = (
    'Value', //Ist
    'Min', // Min
    'Max', // Max
    'Small', // TinyStep
    'Big' // BigStep
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
    ComboText := 'Vorstag-Spannung'; { Kraft Vorstag [rF14] }
    Text := 'Vorstagspannung [N]';
    ComboIndex := 3;
    ArrayIndex := 3;
  end;
  with RecordList[yavWantenSpannung] do
  begin
    ComboText := 'Wanten-Spannung'; { Kraft WanteUnten rF[8] }
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
    ComboText := 'rF[0] MastDruck'; { Kraft Mast rF[0] }
    Text := 'Kraft im Stab D0C [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF01] do
  begin
    ComboText := 'rF[1] Kraft D0C0'; { Kraft Kiel rF[1] }
    Text := 'Kraft im Stab D0C0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF03] do
  begin
    ComboText := 'rF[3] Kraft A0C0'; { Kraft PüttingVorstag rF[3] }
    Text := 'Kraft im Stab A0C0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF05] do
  begin
    ComboText := 'rF[5] Kraft A0D0'; { Kraft PüttingMastfuß rF[5] }
    Text := 'Kraft im Stab A0D0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF06] do
  begin
    ComboText := 'rF[6] Kraft A0B0'; { Kraft PüttingVerbindung rF[6] }
    Text := 'Kraft im Stab A0B0 [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF10] do
  begin
    ComboText := 'rF[10] Kraft AD'; { Kraft Saling [10] }
    Text := 'Kraft im Stab AD [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF11] do
  begin
    ComboText := 'rF[11] Kraft AB'; { Kraft SalingVerbindung rF[11] }
    Text := 'Kraft im Stab AB [N]';
    ComboIndex := -1;
    ArrayIndex := -1;
  end;
  with RecordList[yavRF13] do
  begin
    ComboText := 'rF[13] Kraft AC'; { Kraft WanteOben rF[13] }
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

end.
