unit RggUnit3;

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
  System.IniFiles,
  System.Math,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggUnit2,
  RggFachwerk;

const
  { in KN / cm^2 }
  EModulStahl = 210E3; { N/mm^2 }
  EModulAlu = 70E3; { N/mm^2 }
  EAgross = 100E6; { N }
  EARumpf = 10E6; { N }
  EASaling = 1E6; { N }

type
  TRiggFS = class(TMast)
  private
    FOnRegelGrafik: TNotifyEvent;
    FProbe: Boolean;
    FRiggOK: Boolean;
    FHullIsFlexible: Boolean;
    KnotenLastD0, KnotenLastC, KnotenLastC0: TRealPoint;

    function GetEA: TRiggLvektor;
    procedure SetEA(Value: TRiggLvektor);
    procedure Kraefte; virtual;
    procedure Split; virtual;
    procedure MakeRumpfKoord;
    procedure MakeKoord; virtual;
    procedure MakeKoordDS; virtual;
    procedure KraefteOS; virtual;
    procedure SplitOS; virtual;
    procedure MakeKoordOS; virtual;
    procedure Probe;
    procedure Entlasten;

  protected
    SplitF: TSplitF;
    TetraF: TTetraF;
    FRiggStatus: set of TRiggStatus;
    procedure SetSalingTyp(Value: TSalingTyp); override;
    procedure GetDefaultChartData;

  public
    Fachwerk: TFachwerk;

    rLe: TRiggLvektor; { Längen entlastet 3d in mm }
    rF: TRiggLvektor; { Stabkräfte 3d in N }
    rEA: TRiggLvektor; { EA Werte 3d in KN }
    rPe: TRealRiggPoints; { Koordinaten entlastet 3d in mm }
    iPe: TIntRiggPoints; { Integerkoordinaten entlastet 3d in mm }

    { Daten für RegelGrafik }
    Anfang, Antrieb, Ende: double;
    limitA, limitB, TrySalingH: double;
    KurveF: TChartLine;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromIniFile(IniFile: TIniFile); override;
    procedure WriteToIniFile(IniFile: TIniFile); override;
    procedure LoadFromStream(S: TStream); override;
    procedure SaveToStream(S: TStream); override;

    procedure UpdateRigg;
    function Regeln(TrimmSoll: TTrimm): Integer;
    function RiggStatusText: string;

    property OnRegelGrafik: TNotifyEvent read FOnRegelGrafik write FOnRegelGrafik;
    property ProofRequired: Boolean read FProbe write FProbe;
    property RiggOK: Boolean read FRiggOK;
    property HullFlexible: Boolean read FHullIsFlexible write FHullIsFlexible;
    property EA: TRiggLvektor read GetEA write SetEA;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TRiggFS.Create;
begin
  inherited Create;
  FOnRegelGrafik := nil;
  GetDefaultChartData;

  SplitF := TSplitF.Create;
  TetraF := TTetraF.Create;
  Fachwerk := TFachwerk.Create;

  FProbe := True;
  FHullIsFlexible := True;

  rEA[0] := EAgross;
  rEA[1] := EARumpf;
  rEA[2] := EARumpf;
  rEA[3] := EARumpf;
  rEA[4] := EARumpf;
  rEA[5] := EARumpf;
  rEA[6] := EARumpf;
  rEA[7] := 13 * EModulStahl;
  rEA[8] := 13 * EModulStahl;
  rEA[9] := EAgross;
  rEA[10] := EAgross;
  rEA[11] := EASaling;
  rEA[12] := 13 * EModulStahl;
  rEA[13] := 13 * EModulStahl;
  rEA[14] := 13 * EModulStahl;
  rEA[15] := EAgross;
  rEA[16] := EAgross;
  rEA[17] := EAgross;
  rEA[18] := EAgross;
  rEA[19] := EAgross;
end;

destructor TRiggFS.Destroy;
begin
  SplitF.Free;
  TetraF.Free;
  Fachwerk.Free;
  inherited Destroy;
end;

procedure TRiggFS.WriteToIniFile(IniFile: TIniFile);
var
  S, S1, S2: String;
  i: Integer;
begin
  inherited WriteToIniFile(IniFile);
  with IniFile do
  begin
    S := 'EA';
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := Format('%.6g', [rEA[i]]);
      WriteString(S, S1, S2);
    end;
  end;
end;

procedure TRiggFS.LoadFromIniFile(IniFile: TIniFile);
var
  S, S1, S2: String;
  i: Integer;
begin
  inherited LoadFromIniFile(IniFile);
  with IniFile do
  begin
    S := 'EA';
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := ReadString(S, S1, '100000');
      rEA[i] := StrToFloat(S2);
    end;
  end;
end;

procedure TRiggFS.LoadFromStream(S: TStream);
begin
  inherited LoadFromStream(S);
  S.ReadBuffer(rEA, SizeOf(TRiggLvektor));
end;

procedure TRiggFS.SaveToStream(S: TStream);
begin
  inherited SaveToStream(S);
  S.WriteBuffer(rEA, SizeOf(TRiggLvektor));
end;

procedure TRiggFS.SetSalingTyp(Value: TSalingTyp);
begin
  if Value <> SalingTyp then
  begin
    inherited SetSalingTyp(Value);
    Fachwerk.SalingTyp := Value;
  end;
end;

function TRiggFS.RiggStatusText: string;
var
  S: string;
begin
  S := '  Rigg:';
  if RiggOK then
    S := S + '    Letzte Rechnung O.K.';
  if rsNichtEntspannbar in FRiggStatus then
    S := S + '    Nicht entspannbar';
  if rsWanteAufDruck in FRiggStatus then
    S := S + '    Wante auf Druck';
  if rsKraftZuGross in FRiggStatus then
    S := S + '    Kraft zu groß';
  Result := S;
end;

procedure TRiggFS.Entlasten;
var
  i: Integer;
begin
  rLe[0] := rL[15] + rL[16];
  for i := 1 to 14 do
  begin
    rLe[i] := rL[i] - rF[i] * rL[i] / rEA[i];
  end;
  for i := 15 to 19 do
  begin
    rLe[i] := rL[i];
  end;
end;

function TRiggFS.GetEA: TRiggLvektor;
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    Result[i] := rEA[i] / 1000;
end;

procedure TRiggFS.SetEA(Value: TRiggLvektor);
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    rEA[i] := Value[i] * 1000;
  with Fachwerk do
  begin
    vektorEA[3] := rEA[8];
    vektorEA[4] := rEA[13];
    vektorEA[5] := rEA[0];
    vektorEA[6] := rEA[14];
    vektorEA[7] := rEA[1];
    vektorEA[8] := rEA[1];
    vektorEA[9] := rEA[1];
  end;
end;

procedure TRiggFS.UpdateRigg;
begin
  FRiggOK := True;
  Exclude(FRiggStatus, rsNichtEntspannbar);
  Exclude(FRiggStatus, rsWanteAufDruck);
  Exclude(FRiggStatus, rsKraftZuGross);

  case SalingTyp of
    stFest:
      begin
        Kraefte;
        Split;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoord;
      end;
    stDrehbar:
      begin
        Kraefte;
        Split;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoordDS;
      end;
    stOhne, stOhne_2:
      begin
        KraefteOS;
        SplitOS;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoordOS;
      end;
  end;

  with FTrimm do
  begin
    { Mastfall }
    Mastfall := Round(Abstand(rP[ooF0], rP[ooF])); { in mm }
    { Vorstagspannung }
    if abs(rF[14]) < 32000 then
      Spannung := Round(rF[14]) { in N }
    else
    begin
      if rF[14] > 32000 then
        Spannung := 32000;
      if rF[14] < -32000 then
        Spannung := -32000;
    end;
    { Biegung an den Salingen }
    BiegungS := Round(hd); { in mm }
    { Biegung am Controller }
    BiegungC := Round(he); { in mm }
    { "Elastizität" }
    FlexWert := Round(Abstand(rP[ooC], rPe[ooC])); { in mm }
  end;
end;

procedure TRiggFS.Kraefte;
begin
  { Kräfte für Verwendung in Probe speichern: }
  KnotenLastD0[x] := FD0x; { in N }
  KnotenLastD0[y] := 0;
  KnotenLastD0[z] := -FD0y;

  KnotenLastC[x] := FCx;
  KnotenLastC[y] := 0;
  KnotenLastC[z] := FCy;

  with Fachwerk do
  begin
    { neue Kräfte in N }
    FS1 := ClearVektorS;
    FX := ClearVektorK;
    FY := ClearVektorK;
    FX[1] := -FEx; { Mastcontrollerkraft }
    FY[1] := FEy;
    FX[2] := -FDx; { Salingkraft }
    FY[2] := FDy;
    FX[3] := FCx; { FB vom Mast }
    FY[3] := FCy;
    FX[5] := FD0x; { FA vom Mast }
    FY[5] := -FD0y;

    { neue Geometrie }
    KX[1] := rP[ooE, x]; { Angriffspunkt Mastcontroller }
    KY[1] := rP[ooE, z];
    KX[2] := rP[ooA, x]; { Saling }
    KY[2] := rP[ooA, z];
    KX[3] := rP[ooC, x]; { Angriffspunkt Wante }
    KY[3] := rP[ooC, z];

    KX[4] := rP[ooC0, x];
    KY[4] := rP[ooC0, z];
    KX[5] := rP[ooD0, x];
    KY[5] := rP[ooD0, z];
    KX[6] := rP[ooA0, x];
    KY[6] := rP[ooA0, z];

    ActionF; { Fachwerk berechnen }

    if Fachwerk.FS[3] < 0 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsWanteAufDruck);
      LogList.Add('TRiggFS.Kraefte: Wante auf Druck');
    end;
  end;
end;

procedure TRiggFS.Split;
var
  P0P, PC, PD, P0C0, P0D0: double;
  j: Integer;
begin
  { Laengen bereitstellen }
  P0C0 := Abstand(rP[ooP0], rP[ooC0]);
  PC := Abstand(rP[ooP], rP[ooC]);
  PD := Abstand(rP[ooP], rP[ooD]);
  P0P := Abstand(rP[ooP0], rP[ooP]);
  P0D0 := Abstand(rP[ooP0], rP[ooD0]);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL[6]; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF[2] := F1;
    rF[3] := F1;
    if abs(l1 - rL[2]) > 0.01 then
      LogList.Add('Rigg.Split: Längenabweichung');

    { Punkt D0 }
    h := P0D0;
    l2 := rL[6]; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF[4] := F1;
    rF[5] := F1;

    { Punkte A, B }
    h := P0P;
    l2 := rL[6] - rL[11]; { PüttingAbstand - SalingAbstand }
    F := Fachwerk.FS[3];
    SplitCalc;
    rF[7] := F1;
    rF[8] := F1;

    { Punkt D }
    h := PD;
    l2 := rL[11]; { SalingAbstand }
    F := -FD;
    SplitCalc;
    rF[9] := F1;
    rF[10] := F1;

    { Punkt C }
    h := PC;
    l2 := rL[11]; { SalingAbstand }
    F := Fachwerk.FS[4];
    SplitCalc;
    rF[12] := F1;
    rF[13] := F1;
  end;

  with Fachwerk do
  begin
    rF[0] := FS[5];
    rF[1] := FS[8];
    rF[14] := FS[6];
    rF[15] := 0;
    rF[16] := 0;
    rF[17] := 0;
    rF[18] := FS[2];
    rF[19] := FS[1];
  end;

  with TetraF do
  begin
    d1 := vsub(rP[ooA], rP[ooA0]);
    d2 := vsub(rP[ooC0], rP[ooA0]);
    d3 := vsub(rP[ooD0], rP[ooA0]);
    d4 := vsub(rP[ooB0], rP[ooA0]);
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL[8];
    l2 := rL[3];
    l3 := rL[5];
    F1 := rF[8];
    F2 := rF[3];
    F3 := rF[5];
    VierteKraft;
    rF[6] := F4;

    d1 := vsub(rP[ooA0], rP[ooA]);
    d2 := vsub(rP[ooC], rP[ooA]);
    d3 := vsub(rP[ooD], rP[ooA]);
    d4 := vsub(rP[ooB], rP[ooA]);
    l1 := rL[8];
    l2 := rL[13];
    l3 := rL[10];
    F1 := rF[8];
    F2 := rF[13];
    F3 := rF[10];
    VierteKraft;
    rF[11] := F4;
  end;

  for j := 0 to 19 do
  begin
    if abs(rF[j]) > 32000 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsKraftZuGross);
      LogList.Add(Format('TRiggFS.Split: Betrag rF[%d] > 32000 N', [j]));
    end;
  end;
end;

procedure TRiggFS.Probe;

  function Probe(o, a, b, c, d: TRiggPoint; al, bl, cl, dl: Integer): Boolean;
  begin
    with TetraF do
    begin
      d1 := vsub(rP[a], rP[o]);
      d2 := vsub(rP[b], rP[o]);
      d3 := vsub(rP[c], rP[o]);
      d4 := vsub(rP[d], rP[o]);

      l1 := rL[al];
      l2 := rL[bl];
      l3 := rL[cl];
      l4 := rL[dl];

      F1 := rF[al];
      F2 := rF[bl];
      F3 := rF[cl];
      F4 := rF[dl];

      Result := Probe; { Aufruf von Probe in class TetraF }
    end;
  end;

var
  tempResult: double;
  temptest: Boolean;
  test: Boolean;

begin
  test := True;
  if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe A0 = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    temptest := Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe B0 = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0[x] := rF[19] * -cos(delta1);
    KnotenLastC0[y] := 0;
    KnotenLastC0[z] := rF[19] * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe C0 = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe D0 = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;
    { Probe Punkt A }
    temptest := Probe(ooA, ooA0, ooB, ooC, ooD, 8, 11, 13, 10);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe A = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt B }
    temptest := Probe(ooB, ooA, ooB0, ooC, ooD, 11, 7, 12, 9);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe B = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: Probe C = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add('TRiggFS.Probe: Probe falsch');
      Main.Logger.Info('Probe falsch!');
    end
    else
      LogList.Add('TRiggFS.Probe: Probe O.K.');
  end;

  if (SalingTyp = stOhne) or (SalingTyp = stOhne_2) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: ProbeOS A0 = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    test := test and Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: ProbeOS B0 = %6.2f', [tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0[x] := rF[19] * -cos(delta1);
    KnotenLastC0[y] := 0;
    KnotenLastC0[z] := rF[19] * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: ProbeOS C0 = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: ProbeOS D0 = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format('TRiggFS.Probe: ProbeOS C = %6.2f', [tempResult]));
    TetraF.KnotenLast := Null;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add('TRigg.Probe: Probe falsch');
      Main.Logger.Info('Probe falsch!');
    end
    else
      LogList.Add('TRigg.Probe: Probe O.K.');
  end;
end;

procedure TRiggFS.MakeRumpfKoord;
var
  r1, r2: double;
begin
  { Festpunkte übernehmen }
  rPe[ooD0] := rP[ooD0];
  rPe[ooE0] := rP[ooE0];
  rPe[ooF0] := rP[ooF0];

  if not FHullIsFlexible then
  begin
    { Rumpf steif angenommen }
    rPe[ooA0] := rP[ooA0];
    rPe[ooB0] := rP[ooB0];
    rPe[ooC0] := rP[ooC0];
    rPe[ooP0] := rP[ooP0];
    Exit;
  end;

  r1 := Abstand(rP[ooP0], Null);
  r2 := sqrt(sqr(rLe[5]) - sqr(rLe[6] / 2));
  if (r1 < 0.1) or (r2 < 0.1) then
  begin
    Exit;
  end;
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: ooP0, ooA0, ooB0 ermitteln }
      Radius1 := r1;
      Radius2 := r2;
      MittelPunkt1 := Null;
      MittelPunkt2 := rPe[ooD0];
      rPe[ooP0] := SchnittPunkt1;
      rPe[ooA0] := rPe[ooP0];
      rPe[ooA0, y] := rLe[6] / 2;
      rPe[ooB0] := rPe[ooP0];
      rPe[ooB0, y] := -rLe[6] / 2;
    end;
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      { 2. Aufruf SchnittKK: ooC0 ermitteln }
      Radius1 := sqrt(sqr(rLe[3]) - sqr(rLe[6] / 2));
      Radius2 := rLe[1];
      MittelPunkt1 := rPe[ooP0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooC0] := SchnittPunkt1;
    end;
  except
    on E: EMathError do
      LogList.Add('TRiggFS.MakeRumpfKoord:  ' + E.Message);
  end;
end;

procedure TRiggFS.MakeKoord;
var
  Temp: TRealPoint;
  s: string;
  s1, s2: double;
  r1, r2: double;
begin
  MakeRumpfKoord;
  rPe[ooE] := rP[ooE];
  try
    s1 := sqr(rLe[10]) - sqr(rLe[11] / 2);
    s2 := sqr(rLe[13]) - sqr(rLe[11] / 2);
    if (s1 < 0.1) or (s2 < 0.1) then
    begin
      Exit;
    end;
    r1 := sqrt(s1);
    r2 := sqrt(s2);
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: Saling2d und WanteOben2d;
        Schnittpunkt Temp wird im 2. Aufruf benötigt }
      Radius1 := r1;
      Radius2 := r2;
      Temp := Null;
      Temp[x] := rL[16];
      MittelPunkt1 := Temp;
      Temp := Null;
      Temp[x] := rL[16] + rL[15];
      MittelPunkt2 := Temp;
      Temp := SchnittPunkt1;
      s := Bemerkung;
      s := Format('TRiggFS.MakeKoord, 1. Aufruf: %s', [s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { 2. Aufruf SchnittKK: WanteUnten2d und Abstand rPe[ooD0]..rPe[ooP];
        ooA, ooB, ooP ermitteln }
      Radius1 := sqrt(sqr(rLe[8]) - sqr((rLe[6] - rLe[11]) / 2));
      Radius2 := Abstand(Temp, Null); { Temp unter 1. ermittelt }
      MittelPunkt1 := rPe[ooA0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooA] := SchnittPunkt1;
      rPe[ooA, y] := rLe[11] / 2;
      s := Bemerkung;
      s := Format('TRiggFS.MakeKoord, 2. Aufruf: %s', [s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      rPe[ooB] := rPe[ooA];
      rPe[ooB, y] := -rPe[ooA, y];
      rPe[ooP] := rPe[ooA];
      rPe[ooP, y] := 0;

      { 3. Aufruf SchnittKK: Saling2d und MastUnten; ooD ermitteln }
      Radius1 := sqrt(sqr(rLe[10]) - sqr(rLe[11] / 2));
      Radius2 := rLe[16];
      MittelPunkt1 := rPe[ooA];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooD] := SchnittPunkt1;
      rPe[ooD, y] := 0;
      s := Bemerkung;
      s := Format('TRiggFS.MakeKoord, 3. Aufruf: %s', [s]);
      LogList.Add(s);

      { 4. Aufruf SchnittKK: WanteOben2d und MastOben; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe[13]) - sqr(rLe[11] / 2));
      Radius2 := rLe[15];
      MittelPunkt1 := rPe[ooA];
      MittelPunkt2 := rPe[ooD];
      rPe[ooC] := SchnittPunkt1;
      rPe[ooC, y] := 0;
      s := Bemerkung;
      s := Format('TRiggFS.MakeKoord, 4. Aufruf: %s', [s]);
      LogList.Add(s);
    end;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - arctan2((rPe[ooC, x] - rPe[ooD0, x]), (rPe[ooC, z] - rPe[ooD0, z]));
    rPe[ooF, x] := rPe[ooD0, x] + FiMastL * cos(gammaE);
    rPe[ooF, y] := 0;
    rPe[ooF, z] := rPe[ooD0, z] + FiMastL * sin(gammaE);

  except
    on E: EMathError do
      LogList.Add('TRiggFS.MakeKoord:  ' + E.Message);
  end;
end;

procedure TRiggFS.MakeKoordDS;
var
  S: String;
  Temp, TempA0, TempA, TempC, TempD: TRealPoint;
  WStrich3d, WStrich2d, W1Strich, Saling1L, Skalar: double;
begin
  Temp := Null;
  MakeRumpfKoord;
  rPe[ooE] := rP[ooE];
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;

      Radius1 := rLe[10]; { FrSalingL; }
      Radius2 := rLe[13]; { FrWoben3d; }
      TempD := Null;
      TempD[x] := rLe[16]; { FrMastunten }
      MittelPunkt1 := TempD;
      TempC := Null;
      TempC[x] := rLe[16] + rLe[15]; { FrMastunten+FrMastoben; }
      MittelPunkt2 := TempC;
      TempA := SchnittPunkt1;
      TempA[y] := 0;
      S := Bemerkung;
      S := Format('TRiggFS.MakeKoordDS/1: %s', [S]);
      LogList.Add(S);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      Radius1 := rLe[5]; { Abstand(rPe[ooD0],rPe[ooA0]); }
      Radius2 := rLe[8]; { FrWunten3d; }
      MittelPunkt1 := Null;
      MittelPunkt2 := TempA;
      TempA0 := SchnittPunkt1;
      TempA0[y] := 0;
      S := Bemerkung;
      S := Format('TRiggFS.MakeKoordDS/2: %s', [S]);
      LogList.Add(S);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      WStrich3d := Abstand(TempA0, TempC);
      WStrich2d := sqrt(sqr(WStrich3d) - sqr(rPe[ooA0, y]));

      Radius1 := WStrich2d;
      Radius2 := rLe[16] + rLe[15]; { FrMastunten + FrMastoben; }
      MittelPunkt1 := rPe[ooP0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooC] := SchnittPunkt1;
      rPe[ooC, y] := 0;
      S := Bemerkung;
      S := Format('TRiggFS.MakeKoordDS/3: %s', [S]);
      LogList.Add(S);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { weiter in der Ebene }
      SchnittGG(TempA0, TempC, TempD, TempA, Temp);
      { Temp enthält jetzt den Schnittpunkt der Diagonalen }
      W1Strich := Abstand(TempA0, Temp);
      Saling1L := Abstand(TempD, Temp);

      { weiter räumlich: }
      Skalar := W1Strich / WStrich3d;
      Temp := vsub(rPe[ooC], rPe[ooA0]);
      Temp := SkalarMult(Temp, Skalar);
      Temp := vadd(rPe[ooA0], Temp);
      { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

      Skalar := rLe[16] / (rLe[16] + rLe[15]);
      rPe[ooD] := vsub(rPe[ooC], rPe[ooD0]);
      rPe[ooD] := SkalarMult(rPe[ooD], Skalar);
      rPe[ooD] := vadd(rPe[ooD0], rPe[ooD]);

      { Berechnung Punkt ooA }
      Skalar := rLe[10] / Saling1L;
      Temp := vsub(Temp, rPe[ooD]);
      Temp := SkalarMult(Temp, Skalar);
      rPe[ooA] := vadd(rPe[ooD], Temp);

      { aktualisieren }
      rPe[ooP] := rPe[ooA];
      rPe[ooP, y] := 0;
      rPe[ooB] := rPe[ooA];
      rPe[ooB, y] := -rPe[ooA, y];

      { Berechnung für Punkt ooF - Masttop }
      gammaE := pi / 2 - arctan2((rPe[ooC, x] - rPe[ooD0, x]),
        (rPe[ooC, z] - rPe[ooD0, z]));
      rPe[ooF, x] := rPe[ooD0, x] + FiMastL * cos(gammaE);
      rPe[ooF, y] := 0;
      rPe[ooF, z] := rPe[ooD0, z] + FiMastL * sin(gammaE);
    end;

  except
    on E: EMathError do
      LogList.Add('TRiggFS.MakeKoord:  ' + E.Message);
  end;
end;

procedure TRiggFS.KraefteOS;
var
  temp: double;
begin
  { für Verwendung in Probe speichern }
  KnotenLastD0[x] := FD0x; { in N }
  KnotenLastD0[y] := 0;
  KnotenLastD0[z] := -FD0y;

  KnotenLastC[x] := FCx;
  KnotenLastC[y] := 0;
  KnotenLastC[z] := FCy;

  with Fachwerk do
  begin
    { neue Kräfte in N }
    FS1 := ClearVektorS;
    FX := ClearVektorK;
    FY := ClearVektorK;
    FX[1] := -FEx; { Mastcontrollerkraft }
    FY[1] := FEy;
    FX[2] := 0; { Null gesetzt, da nicht relevant }
    FY[2] := 0; { Null gesetzt, da nicht relevant }
    FX[3] := FCx; { FB vom Mast }
    FY[3] := FCy;
    FX[5] := FD0x; { FA vom Mast }
    FY[5] := -FD0y;

    { neue Geometrie }
    KX[1] := rP[ooE, x]; { Angriffspunkt Mastcontroller }
    KY[1] := rP[ooE, z];
    KX[2] := rP[ooP, x]; { Saling }
    KY[2] := rP[ooP, z];
    KX[3] := rP[ooC, x]; { Angriffspunkt Wante }
    KY[3] := rP[ooC, z];

    KX[4] := rP[ooC0, x];
    KY[4] := rP[ooC0, z];
    KX[5] := rP[ooD0, x];
    KY[5] := rP[ooD0, z];
    KX[6] := rP[ooA0, x];
    KY[6] := rP[ooA0, z];

    if SalingTyp = stOhne then
    begin
      temp := sqrt(sqr(rL[8] + rL[13]) - sqr(rL[6] / 2));
      temp := arctan2(rL[6] / 2, temp);
      WantenPower := cos(temp) * WantenSpannung * 2;
    end;
    if SalingTyp = stOhne_2 then
      MastDruck := FC;
    ActionF;
  end;
end;

procedure TRiggFS.SplitOS;
var
  P0D0, P0C, P0C0: double;
begin
  { Laengen bereitstellen }
  P0D0 := Abstand(rP[ooP0], rP[ooD0]);
  P0C0 := Abstand(rP[ooP0], rP[ooC0]);
  P0C := Abstand(rP[ooP0], rP[ooC]);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL[6]; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF[2] := F1;
    rF[3] := F1;

    { Punkt D0 }
    h := P0D0;
    l2 := rL[6]; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF[4] := F1;
    rF[5] := F1;

    { Punkt C }
    h := P0C;
    l2 := rL[6]; { PuettingAbstand }
    if SalingTyp = stOhne then
      F := Fachwerk.WantenPower;
    if SalingTyp = stOhne_2 then
      F := Fachwerk.FS[4];
    SplitCalc;
    rF[ 7] := F1;
    rF[ 8] := F1;
    rF[12] := F1;
    rF[13] := F1;
  end;

  with Fachwerk do
  begin
    rF[ 0] := FS[5];
    rF[ 1] := FS[8];
    rF[ 9] := 0;
    rF[10] := 0;
    rF[11] := 0;
    rF[14] := FS[6];
    rF[15] := 0;
    rF[16] := 0;
    rF[17] := 0;
    rF[18] := FS[2];
    rF[19] := FS[1];
  end;

  with TetraF do
  begin
    d1 := vsub(rP[ooC], rP[ooA0]);
    d2 := vsub(rP[ooC0], rP[ooA0]);
    d3 := vsub(rP[ooD0], rP[ooA0]);
    d4 := vsub(rP[ooB0], rP[ooA0]);
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL[8] + rL[13];
    l2 := rL[3];
    l3 := rL[5];
    F1 := rF[8];
    F2 := rF[3];
    F3 := rF[5];
    VierteKraft;
    rF[6] := F4;
  end;
end;

procedure TRiggFS.MakeKoordOS;
var
  Temp: TRealPoint;
  Skalar: double;
  S: String;
begin
  MakeRumpfKoord;
  rPe[ooE] := rP[ooE];
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;

      { 1. Aufruf SchnittKK: Wante2d und Mast; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe[8] + rLe[13]) - sqr(rLe[6] / 2));
      Radius2 := rLe[15] + rLe[16];
      MittelPunkt1 := rPe[ooP0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooC] := SchnittPunkt1;
      S := Bemerkung;
      S := Format('TRiggFS.MakeKoordOS, 1. Aufruf: %s', [S]);
      LogList.Add(S);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;
    end;

    { Punkte ooA, ooB, ooD und ooP ermitteln }
    Temp := vsub(rPe[ooC], rPe[ooD0]);
    Skalar := rLe[16] / (rLe[15] + rLe[16]); { Mastunten / Mast }
    Temp[x] := Skalar * Temp[x];
    Temp[z] := Skalar * Temp[z];
    rPe[ooD] := vadd(rPe[ooD0], Temp);

    Skalar := rLe[13] / (rLe[8] + rLe[13]); { Woben3d / Wante3d }
    rPe[ooP, x] := rPe[ooC, x] - Skalar * (rPe[ooC, x] - rPe[ooP0, x]);
    rPe[ooP, y] := 0;
    rPe[ooP, z] := rPe[ooC, z] - Skalar * (rPe[ooC, z] - rPe[ooP0, z]);

    rPe[ooA] := rPe[ooP];
    rPe[ooA, y] := Skalar * rPe[ooA0, y];
    rPe[ooB] := rPe[ooA];
    rPe[ooB, y] := -rPe[ooA, y];

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - arctan2((rPe[ooC, x] - rPe[ooD0, x]), (rPe[ooC, z] - rPe[ooD0, z]));
    rPe[ooF, x] := rPe[ooD0, x] + FiMastL * cos(gammaE);
    rPe[ooF, y] := 0;
    rPe[ooF, z] := rPe[ooD0, z] + FiMastL * sin(gammaE);

  except
    on E: EMathError do
      LogList.Add('TRiggFS.MakeKoord:  ' + E.Message);
  end;
end;

{ Regeln funktioniert auch bei Durchbiegung Null, weil die
  Ermittlung der Höhe hd immer(!) eine kleine Auslenkung
  zurückliefert. }
function TRiggFS.Regeln(TrimmSoll: TTrimm): Integer;

  procedure Berechnen(Antrieb: double);
  begin
    case SalingTyp of
      stFest:
        MakeSalingHBiggerFS(Antrieb);
      stDrehbar:
        MakeSalingLBiggerDS(Antrieb);
    end;
    SchnittKraefte;
    UpdateRigg;
  end;

var
  Diff, Schranke: double;
  Zaehler, ZaehlerMax: Integer;
  i: Integer;
  KraftMin, KraftMax: double;
  tempFProbe, TrimmSollOut: Boolean;
begin
  Result := 0;
  { Biegen und Neigen }
  case SalingTyp of
    stFest:
      BiegeUndNeigeFS(TrimmSoll, limitA);
    stDrehbar:
      BiegeUndNeigeDS(TrimmSoll, limitA);
    stOhne, stOhne_2:
      Exit; { Regeln nur für stFest und stDrehbar }
  end;

  { mit Salinglhöhe oder Salinglänge die Kraft einstellen: }
  { Zunächst limitA und limitB bestimmen }
  case CalcTyp of
    ctBiegeKnicken:
      begin
        case SalingTyp of
          stFest:
            begin
              limitA := GSB.SalingH.Min; { in mm }
              limitB := GSB.SalingH.Max; { in mm }
            end;
          stDrehbar:
            begin
              limitA := GSB.SalingL.Min; { in mm }
              limitB := GSB.SalingL.Max; { in mm }
            end;
        end;
      end;
    ctQuerKraftBiegung:
      begin
        { limitA := limitA; } { in mm, wird in BiegeUndNeigeF ermittelt }
        limitB := limitA + 400; { in mm }
      end;
    ctKraftGemessen:
      begin
        { Hier nur Biegen und Neigen }
        case SalingTyp of
          stFest:
            Berechnen(FiSalingH); { Saling wiederherstellen }
          stDrehbar:
            Berechnen(FiSalingL);
        end;
        Exit; { weil Kraft schon bekannt }
      end;
  end;

  { Anfang und Ende bestimmen }
  if limitA < limitB then
  begin { normalerweise immer limitA < limitB }
    Anfang := limitA;
    Ende := limitB;
  end
  else
  begin
    Anfang := limitB;
    Ende := limitA;
  end;

  tempFProbe := FProbe;
  FProbe := False;

  { Mittleren Kraftwert ermitteln }
  Antrieb := Anfang + (Ende - Anfang) / 2;
  Berechnen(Antrieb);
  KraftMin := FTrimm.Spannung;
  KraftMax := KraftMin;

  { Daten für Kurve sowie KraftMin und KraftMax ermitteln }
  KurveF[0] := 0;
  KurveF[CLMax] := 2000; { Bereich zwischen 0 und 1700 immer sichtbar }
  for i := CLMax - 2 downto 6 do
  begin
    Antrieb := Anfang + (Ende - Anfang) * i / CLMax;
    Berechnen(Antrieb);
    { FTrimm.Spannung schon begrenzt auf +/- 32000 }
    if rF[14] < KraftMin then
      KraftMin := FTrimm.Spannung;
    if rF[14] > KraftMax then
      KraftMax := FTrimm.Spannung;
    KurveF[i] := FTrimm.Spannung;
    if Assigned(FOnRegelGrafik) then
      FOnRegelGrafik(Self);
  end;

  (*
    if TrimmSoll.Spannung < KraftMin then TrimmSoll.Spannung := Round(KraftMin)+1;
    if TrimmSoll.Spannung > KraftMax then TrimmSoll.Spannung := Round(KraftMax)-1;
  *)
  TrimmSollOut := False;
  if TrimmSoll.Spannung < KraftMin then
  begin
    TrySalingH := Anfang + (Ende - Anfang) * (CLMax - 2) / CLMax;
    TrimmSollOut := True;
  end;
  if TrimmSoll.Spannung > KraftMax then
  begin
    TrySalingH := Anfang + (Ende - Anfang) * 6 / CLMax;
    TrimmSollOut := True;
  end;
  if TrimmSollOut then
  begin
    Berechnen(TrySalingH);
    FOnRegelGrafik(Self);
    FProbe := tempFProbe;
    Exit;
  end;

  { Regelungsschleife }
  Schranke := 1; { in N }
  Zaehler := 0;
  ZaehlerMax := 20; { max. ZaehlerMax Schleifendurchläufe }
  repeat
    Inc(Zaehler);
    TrySalingH := (limitA + limitB) / 2;
    Berechnen(TrySalingH);
    Diff := rF[14] - TrimmSoll.Spannung; { Abweichung der Vorstagspannung in N }
    if Diff > 0 then
      limitA := TrySalingH
    else
      limitB := TrySalingH;
    FOnRegelGrafik(Self);
  until (abs(Diff) < Schranke) or (Zaehler = ZaehlerMax);
  Result := Zaehler;

  FProbe := tempFProbe;
end;

procedure TRiggFS.GetDefaultChartData;
var
  i: Integer;
begin
  Anfang := 0;
  Antrieb := 50;
  Ende := 100;
  limitA := 20;
  limitB := 80;
  for i := 0 to CLMax do
    KurveF[i] := i * (2000 div CLMax);
end;

end.
