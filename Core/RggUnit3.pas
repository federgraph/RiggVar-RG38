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
  System.Math.Vectors,
  RggStrings,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggUnit2,
  RggFachwerk;

type
  TRiggFS = class(TMast)
  private
    FOnRegelGrafik: TNotifyEvent;
    FProbe: Boolean;
    FRiggOK: Boolean;
    FHullIsFlexible: Boolean;
    KnotenLastD0, KnotenLastC, KnotenLastC0: TPoint3D;

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
    function GetRelaxedRiggLengths: TRiggRods;
    function GetStabKraefte: TRiggRods;
    function GetRelaxedRiggPoints: TRiggPoints;
    function GetProofRequired: Boolean;
    procedure SetProofRequired(const Value: Boolean);
    procedure SetSalingTyp(const Value: TSalingTyp); override;
    procedure GetDefaultChartData;
    function GetRiggOK: Boolean;
    function GetEA: TRiggRods;
    procedure SetEA(const Value: TRiggRods);
  public
    Fachwerk: TFachwerk;

    rLe: TRiggRods; { Längen entlastet 3d in mm }
    rF: TRiggRods; { Stabkräfte 3d in N }
    rEA: TRiggRods; { EA Werte 3d in KN }
    rPe: TRiggPoints; { Koordinaten entlastet 3d in mm }
    iPe: TRiggPoints; { Integerkoordinaten entlastet 3d in mm }

    { Daten für RegelGrafik }
    Anfang, Antrieb, Ende: single;
    limitA, limitB, TrySalingH: single;
    KurveF: TChartLine;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromIniFile(IniFile: TIniFile); override;
    procedure WriteToIniFile(IniFile: TIniFile); override;

    procedure UpdateRigg;
    function Regeln(TrimmSoll: TTrimm): Integer;
    function GetRiggStatusText: string;

    property OnRegelGrafik: TNotifyEvent read FOnRegelGrafik write FOnRegelGrafik;
    property ProofRequired: Boolean read GetProofRequired write SetProofRequired;
    property RiggOK: Boolean read GetRiggOK;
    property HullFlexible: Boolean read FHullIsFlexible write FHullIsFlexible;
    property EA: TRiggRods read GetEA write SetEA;
    property RiggStatusText: string read GetRiggStatusText;
    property RelaxedRiggPoints: TRiggPoints read GetRelaxedRiggPoints;
    property RelaxedRiggLengths: TRiggRods read GetRelaxedRiggLengths;
    property StabKraefte: TRiggRods read GetStabKraefte;
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

  rEA.D0C := EAgross;
  rEA.C0D0 := EARumpf;
  rEA.B0C0 := EARumpf;
  rEA.A0C0 := EARumpf;
  rEA.B0D0 := EARumpf;
  rEA.A0D0 := EARumpf;
  rEA.A0B0 := EARumpf;
  rEA.B0B := 13 * EModulStahl;
  rEA.A0A := 13 * EModulStahl;
  rEA.BD := EAgross;
  rEA.AD := EAgross;
  rEA.AB := EASaling;
  rEA.BC := 13 * EModulStahl;
  rEA.AC := 13 * EModulStahl;
  rEA.C0C := 13 * EModulStahl;
  rEA.DC := EAgross;
  rEA.D0D := EAgross;
  rEA.ED := EAgross;
  rEA.D0E := EAgross;
  rEA.E0E := EAgross;
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
  s, s1, s2: string;
  i: Integer;
begin
  inherited WriteToIniFile(IniFile);
  with IniFile do
  begin
    s := EA_IniString;
    for i := 0 to 19 do
    begin
      s1 := IntToStr(i);
      s2 := Format('%.6g', [rEA.V[i]]);
      WriteString(s, s1, s2);
    end;
  end;
end;

procedure TRiggFS.LoadFromIniFile(IniFile: TIniFile);
var
  s, s1, s2: string;
  i: Integer;
begin
  inherited LoadFromIniFile(IniFile);
  with IniFile do
  begin
    s := EA_IniString;
    for i := 0 to 19 do
    begin
      s1 := IntToStr(i);
      s2 := ReadString(s, s1, IntToStr(100000));
      rEA.V[i] := StrToFloat(s2);
    end;
  end;
end;

procedure TRiggFS.SetSalingTyp(const Value: TSalingTyp);
begin
  if Value <> SalingTyp then
  begin
    inherited SetSalingTyp(Value);
    Fachwerk.SalingTyp := Value;
  end;
end;

function TRiggFS.GetRiggStatusText: string;
var
  s: string;
begin
  s := Status_String_Rigg;
  if RiggOK then
    s := s + Status_String_RiggLetzteRechnungOK;
  if rsNichtEntspannbar in FRiggStatus then
    s := s + Status_String_RiggNichtEntspannbar;
  if rsWanteAufDruck in FRiggStatus then
    s := s + Status_String_RiggWanteAufDruck;
  if rsKraftZuGross in FRiggStatus then
    s := s + Status_String_RiggForceTooBig;
  Result := s;
end;

function TRiggFS.GetStabKraefte: TRiggRods;
begin
  result := rF;
end;

procedure TRiggFS.Entlasten;
var
  i: Integer;
begin
  rLe.D0C := rL.DC + rL.D0D;
  for i := 1 to 14 do
  begin
    rLe.V[i] := rL.V[i] - rF.V[i] * rL.V[i] / rEA.V[i];
  end;
  for i := 15 to 19 do
  begin
    rLe.V[i] := rL.V[i];
  end;
end;

function TRiggFS.GetEA: TRiggRods;
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    Result.V[i] := rEA.V[i] / 1000;
end;

function TRiggFS.GetProofRequired: Boolean;
begin
  result := FProbe;
end;

function TRiggFS.GetRelaxedRiggLengths: TRiggRods;
begin
  result := rLe;
end;

function TRiggFS.GetRelaxedRiggPoints: TRiggPoints;
begin
  result := rPe;
end;

function TRiggFS.GetRiggOK: Boolean;
begin
  result := FRiggOK;
end;

procedure TRiggFS.SetEA(const Value: TRiggRods);
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    rEA.V[i] := Value.V[i] * 1000;
  Fachwerk.vektorEA[3] := rEA.A0A;
  Fachwerk.vektorEA[4] := rEA.AC;
  Fachwerk.vektorEA[5] := rEA.D0C;
  Fachwerk.vektorEA[6] := rEA.C0C;
  Fachwerk.vektorEA[7] := rEA.C0D0;
  Fachwerk.vektorEA[8] := rEA.C0D0;
  Fachwerk.vektorEA[9] := rEA.C0D0;
end;

procedure TRiggFS.SetProofRequired(const Value: Boolean);
begin
  FProbe := Value;
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
    stOhneStarr, stOhneBiegt:
      begin
        KraefteOS;
        SplitOS;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoordOS;
      end;
  end;

  { Mastfall }
  FTrimm.Mastfall := Round(rP.F0.Distance(rP.F)); { in mm }
  { Vorstagspannung }
  if abs(rF.C0C) < 32000 then
    FTrimm.Spannung := Round(rF.C0C) { in N }
  else
  begin
    if rF.C0C > 32000 then
      FTrimm.Spannung := 32000;
    if rF.C0C < -32000 then
      FTrimm.Spannung := -32000;
  end;
  { Biegung an den Salingen }
  FTrimm.BiegungS := Round(hd); { in mm }
  { Biegung am Controller }
  FTrimm.BiegungC := Round(he); { in mm }
  { "Elastizität" }
  FTrimm.FlexWert := Round(rP.C.Distance(rPe.C)); { in mm }
end;

procedure TRiggFS.Kraefte;
begin
  { Kräfte für Verwendung in Probe speichern: }
  KnotenLastD0.X := FD0x; { in N }
  KnotenLastD0.Y := 0;
  KnotenLastD0.Z := -FD0y;

  KnotenLastC.X := FCx;
  KnotenLastC.Y := 0;
  KnotenLastC.Z := FCy;

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
    KX[1] := rP.E.X; { Angriffspunkt Mastcontroller }
    KY[1] := rP.E.Z;
    KX[2] := rP.A.X; { Saling }
    KY[2] := rP.A.Z;
    KX[3] := rP.C.X; { Angriffspunkt Wante }
    KY[3] := rP.C.Z;

    KX[4] := rP.C0.X;
    KY[4] := rP.C0.Z;
    KX[5] := rP.D0.X;
    KY[5] := rP.D0.Z;
    KX[6] := rP.A0.X;
    KY[6] := rP.A0.Z;

    ActionF; { Fachwerk berechnen }

    if Fachwerk.FS[3] < 0 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsWanteAufDruck);
      LogList.Add(LogList_String_WanteAufDruck);
    end;
  end;
end;

procedure TRiggFS.Split;
var
  P0P, PC, PD, P0C0, P0D0: single;
  j: Integer;
begin
  { Laengen bereitstellen }
  P0C0 := rP.P0.Distance(rP.C0);
  PC := rP.P.Distance(rP.C);
  PD := rP.P.Distance(rP.D);
  P0P := rP.P0.Distance(rP.P);
  P0D0 := rP.P0.Distance(rP.D0);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF.B0C0 := F1;
    rF.A0C0 := F1;
    if abs(l1 - rL.B0C0) > 0.01 then
      LogList.Add(LogList_String_LengthDeviation);

    { Punkt D0 }
    h := P0D0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF.B0D0 := F1;
    rF.A0D0 := F1;

    { Punkte A, B }
    h := P0P;
    l2 := rL.A0B0 - rL.AB; { PüttingAbstand - SalingAbstand }
    F := Fachwerk.FS[3];
    SplitCalc;
    rF.B0B := F1;
    rF.A0A := F1;

    { Punkt D }
    h := PD;
    l2 := rL.AB; { SalingAbstand }
    F := -FD;
    SplitCalc;
    rF.BD := F1;
    rF.AD := F1;

    { Punkt C }
    h := PC;
    l2 := rL.AB; { SalingAbstand }
    F := Fachwerk.FS[4];
    SplitCalc;
    rF.BC := F1;
    rF.AC := F1;
  end;

  with Fachwerk do
  begin
    rF.D0C := FS[5];
    rF.C0D0 := FS[8];
    rF.C0C := FS[6];
    rF.DC := 0;
    rF.D0D := 0;
    rF.ED := 0;
    rF.D0E := FS[2];
    rF.E0E := FS[1];
  end;

  with TetraF do
  begin
    d1 := rP.A - rP.A0;
    d2 := rP.C0 - rP.A0;
    d3 := rP.D0 - rP.A0;
    d4 := rP.B0 - rP.A0;
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL.A0A;
    l2 := rL.A0C0;
    l3 := rL.A0D0;
    F1 := rF.A0A;
    F2 := rF.A0C0;
    F3 := rF.A0D0;
    VierteKraft;
    rF.A0B0 := F4;

    d1 := rP.A0 - rP.A;
    d2 := rP.C - rP.A;
    d3 := rP.D - rP.A;
    d4 := rP.B - rP.A;
    l1 := rL.A0A;
    l2 := rL.AC;
    l3 := rL.AD;
    F1 := rF.A0A;
    F2 := rF.AC;
    F3 := rF.AD;
    VierteKraft;
    rF.AB := F4;
  end;

  for j := 0 to 19 do
  begin
    if abs(rF.V[j]) > 32000 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsKraftZuGross);
      LogList.Add(Format(LogList_Format_String_BetragTooBig, [j]));
    end;
  end;
end;

procedure TRiggFS.Probe;

  function Probe(o, a, b, c, d: TRiggPoint; al, bl, cl, dl: Integer): Boolean;
  begin
    TetraF.d1 := rP.V[a] - rP.V[o];
    TetraF.d2 := rP.V[b] - rP.V[o];
    TetraF.d3 := rP.V[c] - rP.V[o];
    TetraF.d4 := rP.V[d] - rP.V[o];

    TetraF.l1 := rL.V[al];
    TetraF.l2 := rL.V[bl];
    TetraF.l3 := rL.V[cl];
    TetraF.l4 := rL.V[dl];

    TetraF.F1 := rF.V[al];
    TetraF.F2 := rF.V[bl];
    TetraF.F3 := rF.V[cl];
    TetraF.F4 := rF.V[dl];
    Result := TetraF.Probe; { Aufruf von Probe in class TetraF }
  end;

var
  tempResult: single;
  temptest: Boolean;
  test: Boolean;

begin
  test := True;
  if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA0], tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    temptest := Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB0], tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0.X := rF.E0E * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF.E0E * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooD0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt A }
    temptest := Probe(ooA, ooA0, ooB, ooC, ooD, 8, 11, 13, 10);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA], tempResult]));
    test := test and temptest;
    { Probe Punkt B }
    temptest := Probe(ooB, ooA, ooB0, ooC, ooD, 11, 7, 12, 9);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB], tempResult]));
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add(LogList_String_ProbeFalsch);
      Main.Logger.Info(LogList_String_ProbeFalsch);
    end
    else
      LogList.Add(LogList_String_ProbeOK);
  end;

  if (SalingTyp = stOhneStarr) or (SalingTyp = stOhneBiegt) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA0], tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    test := test and Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB0], tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0.X := rF.E0E * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF.E0E * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooD0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add(LogList_String_ProbeFalsch);
      Main.Logger.Info(LogList_String_ProbeFalsch);
    end
    else
      LogList.Add(LogList_String_ProbeOK);
  end;
end;

procedure TRiggFS.MakeRumpfKoord;
var
  r1, r2: single;
begin
  { Festpunkte übernehmen }
  rPe.D0 := rP.D0;
  rPe.E0 := rP.E0;
  rPe.F0 := rP.F0;

  if not FHullIsFlexible then
  begin
    { Rumpf steif angenommen }
    rPe.A0 := rP.A0;
    rPe.B0 := rP.B0;
    rPe.C0 := rP.C0;
    rPe.P0 := rP.P0;
    Exit;
  end;

  r2 := sqr(rLe.V[5]) - sqr(rLe.V[6] / 2);
  if (r2 < 0.1) then
  begin
    Inc(ExitCounter7);
    Exit;
  end;

  r2 := sqrt(sqr(rLe.V[5]) - sqr(rLe.V[6] / 2));
  r1 := rP.P0.Length;
  if (r1 < 0.1) or (r2 < 0.1) then
  begin
    Inc(ExitCounter5);
    Exit;
  end;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: ooP0, ooA0, ooB0 ermitteln }
      Radius1 := r1;
      Radius2 := r2;
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := rPe.D0;
      rPe.P0 := SchnittPunkt1;
      rPe.A0 := rPe.P0;
      rPe.A0.Y := -rLe.V[6] / 2;
      rPe.B0 := rPe.P0;
      rPe.B0.Y := rLe.V[6] / 2;
    end;

    r1 := sqr(rLe.V[3]) - sqr(rLe.V[6] / 2);
    if (r1 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    r2 := rLe.V[1];
    if (r2 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    with SKK do
    begin
      SchnittEbene := seXZ;
      { 2. Aufruf SchnittKK: ooC0 ermitteln }
      Radius1 := sqrt(r1);
      Radius2 := r2;
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C0 := SchnittPunkt1;
    end;
  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeRumpfKoordExcept + E.Message);
  end;
end;

procedure TRiggFS.MakeKoord;
var
  Temp: TPoint3D;
  s: string;
  s1, s2: single;
  r1, r2: single;
begin
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    s1 := sqr(rLe.V[10]) - sqr(rLe.V[11] / 2);
    s2 := sqr(rLe.V[13]) - sqr(rLe.V[11] / 2);
    if (s1 < 0.1) or (s2 < 0.1) then
    begin
      Inc(ExitCounter6);
      Exit;
    end;
    r1 := sqrt(s1);
    r2 := sqrt(s2);
    with SKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: Saling2d und WanteOben2d;
        Schnittpunkt Temp wird im 2. Aufruf benötigt }
      Radius1 := r1;
      Radius2 := r2;
      Temp := TPoint3D.Zero;
      Temp.X := rL.D0D;
      MittelPunkt1 := Temp;
      Temp := TPoint3D.Zero;
      Temp.X := rL.D0D + rL.DC;
      MittelPunkt2 := Temp;
      Temp := SchnittPunkt1;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [1, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { 2. Aufruf SchnittKK: WanteUnten2d und Abstand rPe.D0]..rPe.P];
        ooA, ooB, ooP ermitteln }
      Radius1 := sqrt(sqr(rLe.V[8]) - sqr((rLe.V[6] - rLe.V[11]) / 2));
      Radius2 := Temp.Length; { Temp unter 1. ermittelt }
      MittelPunkt1 := rPe.A0;
      MittelPunkt2 := rPe.D0;
      rPe.A := SchnittPunkt1;
      rPe.A.Y := -rLe.V[11] / 2;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [2, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      rPe.B := rPe.A;
      rPe.B.Y := -rPe.A.Y;
      rPe.P := rPe.A;
      rPe.P.Y := 0;

      { 3. Aufruf SchnittKK: Saling2d und MastUnten; ooD ermitteln }
      Radius1 := sqrt(sqr(rLe.V[10]) - sqr(rLe.V[11] / 2));
      Radius2 := rLe.V[16];
      MittelPunkt1 := rPe.A;
      MittelPunkt2 := rPe.D0;
      rPe.D := SchnittPunkt1;
      rPe.D.Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [3, s]);
      LogList.Add(s);

      { 4. Aufruf SchnittKK: WanteOben2d und MastOben; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe.V[13]) - sqr(rLe.V[11] / 2));
      Radius2 := rLe.V[15];
      MittelPunkt1 := rPe.A;
      MittelPunkt2 := rPe.D;
      rPe.C := SchnittPunkt1;
      rPe.C.Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [4, s]);
      LogList.Add(s);
    end;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
    rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordExept + E.Message);
  end;
end;

procedure TRiggFS.MakeKoordDS;
var
  s: string;
  Temp, TempA0, TempA, TempC, TempD: TPoint3D;
  WStrich3d, WStrich2d, W1Strich, Saling1L, Skalar: single;
begin
  Temp := TPoint3D.Zero;
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;

      Radius1 := rLe.V[10]; { FrSalingL; }
      Radius2 := rLe.V[13]; { FrWoben3d; }
      TempD := TPoint3D.Zero;
      TempD.X := rLe.V[16]; { FrMastunten }
      MittelPunkt1 := TempD;
      TempC := TPoint3D.Zero;
      TempC.X := rLe.V[16] + rLe.V[15]; { FrMastunten+FrMastoben; }
      MittelPunkt2 := TempC;
      TempA := SchnittPunkt1;
      TempA.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [1, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      Radius1 := rLe.V[5]; { Abstand(rPe.D0],rPe.A0]); }
      Radius2 := rLe.V[8]; { FrWunten3d; }
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := TempA;
      TempA0 := SchnittPunkt1;
      TempA0.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [2, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      WStrich3d := TempA0.Distance(TempC);
      WStrich2d := sqrt(sqr(WStrich3d) - sqr(rPe.A0.Y));

      Radius1 := WStrich2d;
      Radius2 := rLe.V[16] + rLe.V[15]; { FrMastunten + FrMastoben; }
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C := SchnittPunkt1;
      rPe.C.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [3, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { weiter in der Ebene }
      SchnittGG(TempA0, TempC, TempD, TempA, Temp);
      { Temp enthält jetzt den Schnittpunkt der Diagonalen }
      W1Strich := TempA0.Distance(Temp);
      Saling1L := TempD.Distance(Temp);

      { weiter räumlich: }
      Skalar := W1Strich / WStrich3d;
      Temp := rPe.C - rPe.A0;
      Temp := Temp * Skalar;
      Temp := rPe.A0 + Temp;
      { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

      Skalar := rLe.V[16] / (rLe.V[16] + rLe.V[15]);
      rPe.D := rPe.C - rPe.D0;
      rPe.D := rPe.D * Skalar;
      rPe.D := rPe.D0 + rPe.D;

      { Berechnung Punkt ooA }
      Skalar := rLe.V[10] / Saling1L;
      Temp := Temp - rPe.D;
      Temp := Temp * Skalar;
      rPe.A := rPe.D + Temp;

      { aktualisieren }
      rPe.P := rPe.A;
      rPe.P.Y := 0;
      rPe.B := rPe.A;
      rPe.B.Y := -rPe.A.Y;

      { Berechnung für Punkt ooF - Masttop }
      gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
      rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);
    end;

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordDSExept + E.Message);
  end;
end;

procedure TRiggFS.KraefteOS;
var
  temp: single;
begin
  { für Verwendung in Probe speichern }
  KnotenLastD0.X := FD0x; { in N }
  KnotenLastD0.Y := 0;
  KnotenLastD0.Z := -FD0y;

  KnotenLastC.X := FCx;
  KnotenLastC.Y := 0;
  KnotenLastC.Z := FCy;

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
    KX[1] := rP.E.X; { Angriffspunkt Mastcontroller }
    KY[1] := rP.E.Z;
    KX[2] := rP.P.X; { Saling }
    KY[2] := rP.P.Z;
    KX[3] := rP.C.X; { Angriffspunkt Wante }
    KY[3] := rP.C.Z;

    KX[4] := rP.C0.X;
    KY[4] := rP.C0.Z;
    KX[5] := rP.D0.X;
    KY[5] := rP.D0.Z;
    KX[6] := rP.A0.X;
    KY[6] := rP.A0.Z;

    if SalingTyp = stOhneStarr then
    begin
      temp := sqrt(sqr(rL.A0A + rL.AC) - sqr(rL.A0B0 / 2));
      temp := arctan2(rL.A0B0 / 2, temp);
      WantenPower := cos(temp) * WantenSpannung * 2;
    end;
    if SalingTyp = stOhneBiegt then
      MastDruck := FC;
    ActionF;
  end;
end;

procedure TRiggFS.SplitOS;
var
  P0D0, P0C, P0C0: single;
begin
  { Laengen bereitstellen }
  P0D0 := rP.P0.Distance(rP.D0);
  P0C0 := rP.P0.Distance(rP.C0);
  P0C := rP.P0.Distance(rP.C);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF.B0C0 := F1;
    rF.A0C0 := F1;

    { Punkt D0 }
    h := P0D0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF.B0D0 := F1;
    rF.A0D0 := F1;

    { Punkt C }
    h := P0C;
    l2 := rL.A0B0; { PuettingAbstand }
    if SalingTyp = stOhneStarr then
      F := Fachwerk.WantenPower;
    if SalingTyp = stOhneBiegt then
      F := Fachwerk.FS[4];
    SplitCalc;
    rF.B0B := F1;
    rF.A0A := F1;
    rF.BC := F1;
    rF.AC := F1;
  end;

  with Fachwerk do
  begin
    rF.D0C := FS[5];
    rF.C0D0 := FS[8];
    rF.BD := 0;
    rF.AD := 0;
    rF.AB := 0;
    rF.C0C := FS[6];
    rF.DC := 0;
    rF.D0D := 0;
    rF.ED := 0;
    rF.D0E := FS[2];
    rF.E0E := FS[1];
  end;

  with TetraF do
  begin
    d1 := rP.C - rP.A0;
    d2 := rP.C0 - rP.A0;
    d3 := rP.D0 - rP.A0;
    d4 := rP.B0 - rP.A0;
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL.A0A + rL.AC;
    l2 := rL.A0C0;
    l3 := rL.A0D0;
    F1 := rF.A0A;
    F2 := rF.A0C0;
    F3 := rF.A0D0;
    VierteKraft;
    rF.A0B0 := F4;
  end;
end;

procedure TRiggFS.MakeKoordOS;
var
  Temp: TPoint3D;
  Skalar: single;
  s: String;
begin
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;

      { 1. Aufruf SchnittKK: Wante2d und Mast; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe.V[8] + rLe.V[13]) - sqr(rLe.V[6] / 2));
      Radius2 := rLe.V[15] + rLe.V[16];
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C := SchnittPunkt1;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoordOS, [1, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;
    end;

    { Punkte ooA, ooB, ooD und ooP ermitteln }
    Temp := rPe.C - rPe.D0;
    Skalar := rLe.V[16] / (rLe.V[15] + rLe.V[16]); { Mastunten / Mast }
    Temp.X := Skalar * Temp.X;
    Temp.Z := Skalar * Temp.Z;
    rPe.D := rPe.D0 + Temp;

    Skalar := rLe.V[13] / (rLe.V[8] + rLe.V[13]); { Woben3d / Wante3d }
    rPe.P.X := rPe.C.X - Skalar * (rPe.C.X - rPe.P0.X);
    rPe.P.Y := 0;
    rPe.P.Z := rPe.C.Z - Skalar * (rPe.C.Z - rPe.P0.Z);

    rPe.A := rPe.P;
    rPe.A.Y := Skalar * rPe.A0.Y;
    rPe.B := rPe.A;
    rPe.B.Y := -rPe.A.Y;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
    rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordExeptOS + E.Message);
  end;
end;

{ Regeln funktioniert auch bei Durchbiegung Null, weil die
  Ermittlung der Höhe hd immer(!) eine kleine Auslenkung
  zurückliefert. }
function TRiggFS.Regeln(TrimmSoll: TTrimm): Integer;

  procedure Berechnen(Antrieb: single);
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
  Diff, Schranke: single;
  Zaehler, ZaehlerMax: Integer;
  i: Integer;
  KraftMin, KraftMax: single;
  tempFProbe, TrimmSollOut: Boolean;
begin
  Result := 0;
  { Biegen und Neigen }
  case SalingTyp of
    stFest:
      BiegeUndNeigeFS(TrimmSoll, limitA);
    stDrehbar:
      BiegeUndNeigeDS(TrimmSoll, limitA);
    stOhneStarr, stOhneBiegt:
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
            Berechnen(FrSalingH); { Saling wiederherstellen }
          stDrehbar:
            Berechnen(FrSalingL);
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
    if rF.C0C < KraftMin then
      KraftMin := FTrimm.Spannung;
    if rF.C0C > KraftMax then
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
    Diff := rF.C0C - TrimmSoll.Spannung; { Abweichung der Vorstagspannung in N }
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
