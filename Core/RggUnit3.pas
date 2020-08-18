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
    KnotenLastD0, KnotenLastC, KnotenLastC0: TPoint3D;

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
    Anfang, Antrieb, Ende: single;
    limitA, limitB, TrySalingH: single;
    KurveF: TChartLine;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromIniFile(IniFile: TIniFile); override;
    procedure WriteToIniFile(IniFile: TIniFile); override;

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
      s2 := Format('%.6g', [rEA[i]]);
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
      rEA[i] := StrToFloat(s2);
    end;
  end;
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

  with FTrimm do
  begin
    { Mastfall }
    Mastfall := Round((rP[ooF0] - rP[ooF]).Length); { in mm }
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
    FlexWert := Round((rP[ooC] - rPe[ooC]).Length); { in mm }
  end;
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
    KX[1] := rP[ooE].X; { Angriffspunkt Mastcontroller }
    KY[1] := rP[ooE].Z;
    KX[2] := rP[ooA].X; { Saling }
    KY[2] := rP[ooA].Z;
    KX[3] := rP[ooC].X; { Angriffspunkt Wante }
    KY[3] := rP[ooC].Z;

    KX[4] := rP[ooC0].X;
    KY[4] := rP[ooC0].Z;
    KX[5] := rP[ooD0].X;
    KY[5] := rP[ooD0].Z;
    KX[6] := rP[ooA0].X;
    KY[6] := rP[ooA0].Z;

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
  P0C0 := (rP[ooP0] - rP[ooC0]).Length;
  PC := (rP[ooP] - rP[ooC]).Length;
  PD := (rP[ooP] - rP[ooD]).Length;
  P0P := (rP[ooP0] - rP[ooP]).Length;
  P0D0 := (rP[ooP0] - rP[ooD0]).Length;

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
      LogList.Add(LogList_String_LengthDeviation);

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
    d1 := rP[ooA] - rP[ooA0];
    d2 := rP[ooC0] - rP[ooA0];
    d3 := rP[ooD0] - rP[ooA0];
    d4 := rP[ooB0] - rP[ooA0];
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL[8];
    l2 := rL[3];
    l3 := rL[5];
    F1 := rF[8];
    F2 := rF[3];
    F3 := rF[5];
    VierteKraft;
    rF[6] := F4;

    d1 := rP[ooA0] - rP[ooA];
    d2 := rP[ooC] - rP[ooA];
    d3 := rP[ooD] - rP[ooA];
    d4 := rP[ooB] - rP[ooA];
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
      LogList.Add(Format(LogList_Format_String_BetragTooBig, [j]));
    end;
  end;
end;

procedure TRiggFS.Probe;

  function Probe(o, a, b, c, d: TRiggPoint; al, bl, cl, dl: Integer): Boolean;
  begin
    with TetraF do
    begin
      d1 := rP[a] - rP[o];
      d2 := rP[b] - rP[o];
      d3 := rP[c] - rP[o];
      d4 := rP[d] - rP[o];

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
    KnotenLastC0.X := rF[19] * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF[19] * sin(delta1);
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
    KnotenLastC0.X := rF[19] * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF[19] * sin(delta1);
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

  r2 := sqr(rLe[5]) - sqr(rLe[6] / 2);
  if (r2 < 0.1) then
  begin
    Inc(ExitCounter7);
    Exit;
  end;

  r2 := sqrt(sqr(rLe[5]) - sqr(rLe[6] / 2));
  r1 := rP[ooP0].Length;
  if (r1 < 0.1) or (r2 < 0.1) then
  begin
    Inc(ExitCounter5);
    Exit;
  end;
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: ooP0, ooA0, ooB0 ermitteln }
      Radius1 := r1;
      Radius2 := r2;
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := rPe[ooD0];
      rPe[ooP0] := SchnittPunkt1;
      rPe[ooA0] := rPe[ooP0];
      rPe[ooA0].Y := -rLe[6] / 2;
      rPe[ooB0] := rPe[ooP0];
      rPe[ooB0].Y := rLe[6] / 2;
    end;

    r1 := sqr(rLe[3]) - sqr(rLe[6] / 2);
    if (r1 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    r2 := rLe[1];
    if (r2 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      { 2. Aufruf SchnittKK: ooC0 ermitteln }
      Radius1 := sqrt(r1);
      Radius2 := r2;
      MittelPunkt1 := rPe[ooP0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooC0] := SchnittPunkt1;
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
  rPe[ooE] := rP[ooE];
  try
    s1 := sqr(rLe[10]) - sqr(rLe[11] / 2);
    s2 := sqr(rLe[13]) - sqr(rLe[11] / 2);
    if (s1 < 0.1) or (s2 < 0.1) then
    begin
      Inc(ExitCounter6);
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
      Temp := TPoint3D.Zero;
      Temp.X := rL[16];
      MittelPunkt1 := Temp;
      Temp := TPoint3D.Zero;
      Temp.X := rL[16] + rL[15];
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

      { 2. Aufruf SchnittKK: WanteUnten2d und Abstand rPe[ooD0]..rPe[ooP];
        ooA, ooB, ooP ermitteln }
      Radius1 := sqrt(sqr(rLe[8]) - sqr((rLe[6] - rLe[11]) / 2));
      Radius2 := Temp.Length; { Temp unter 1. ermittelt }
      MittelPunkt1 := rPe[ooA0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooA] := SchnittPunkt1;
      rPe[ooA].Y := rLe[11] / 2;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [2, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      rPe[ooB] := rPe[ooA];
      rPe[ooB].Y := -rPe[ooA].Y;
      rPe[ooP] := rPe[ooA];
      rPe[ooP].Y := 0;

      { 3. Aufruf SchnittKK: Saling2d und MastUnten; ooD ermitteln }
      Radius1 := sqrt(sqr(rLe[10]) - sqr(rLe[11] / 2));
      Radius2 := rLe[16];
      MittelPunkt1 := rPe[ooA];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooD] := SchnittPunkt1;
      rPe[ooD].Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [3, s]);
      LogList.Add(s);

      { 4. Aufruf SchnittKK: WanteOben2d und MastOben; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe[13]) - sqr(rLe[11] / 2));
      Radius2 := rLe[15];
      MittelPunkt1 := rPe[ooA];
      MittelPunkt2 := rPe[ooD];
      rPe[ooC] := SchnittPunkt1;
      rPe[ooC].Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [4, s]);
      LogList.Add(s);
    end;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - arctan2((rPe[ooC].X - rPe[ooD0].X), (rPe[ooC].Z - rPe[ooD0].Z));
    rPe[ooF].X := rPe[ooD0].X + FrMastLength * cos(gammaE);
    rPe[ooF].Y := 0;
    rPe[ooF].Z := rPe[ooD0].Z + FrMastLength * sin(gammaE);

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
  rPe[ooE] := rP[ooE];
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;

      Radius1 := rLe[10]; { FrSalingL; }
      Radius2 := rLe[13]; { FrWoben3d; }
      TempD := TPoint3D.Zero;
      TempD.X := rLe[16]; { FrMastunten }
      MittelPunkt1 := TempD;
      TempC := TPoint3D.Zero;
      TempC.X := rLe[16] + rLe[15]; { FrMastunten+FrMastoben; }
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

      Radius1 := rLe[5]; { Abstand(rPe[ooD0],rPe[ooA0]); }
      Radius2 := rLe[8]; { FrWunten3d; }
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

      WStrich3d := (TempA0 - TempC).Length;
      WStrich2d := sqrt(sqr(WStrich3d) - sqr(rPe[ooA0].Y));

      Radius1 := WStrich2d;
      Radius2 := rLe[16] + rLe[15]; { FrMastunten + FrMastoben; }
      MittelPunkt1 := rPe[ooP0];
      MittelPunkt2 := rPe[ooD0];
      rPe[ooC] := SchnittPunkt1;
      rPe[ooC].Y := 0;
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
      W1Strich := (TempA0 - Temp).Length;
      Saling1L := (TempD - Temp).Length;

      { weiter räumlich: }
      Skalar := W1Strich / WStrich3d;
      Temp := rPe[ooC] - rPe[ooA0];
      Temp := Temp * Skalar;
      Temp := rPe[ooA0] + Temp;
      { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

      Skalar := rLe[16] / (rLe[16] + rLe[15]);
      rPe[ooD] := rPe[ooC] - rPe[ooD0];
      rPe[ooD] := rPe[ooD] * Skalar;
      rPe[ooD] := rPe[ooD0] + rPe[ooD];

      { Berechnung Punkt ooA }
      Skalar := rLe[10] / Saling1L;
      Temp := Temp - rPe[ooD];
      Temp := Temp * Skalar;
      rPe[ooA] := rPe[ooD] + Temp;

      { aktualisieren }
      rPe[ooP] := rPe[ooA];
      rPe[ooP].Y := 0;
      rPe[ooB] := rPe[ooA];
      rPe[ooB].Y := -rPe[ooA].Y;

      { Berechnung für Punkt ooF - Masttop }
      gammaE := pi / 2 - arctan2((rPe[ooC].X - rPe[ooD0].X),
        (rPe[ooC].Z - rPe[ooD0].Z));
      rPe[ooF].X := rPe[ooD0].X + FrMastLength * cos(gammaE);
      rPe[ooF].Y := 0;
      rPe[ooF].Z := rPe[ooD0].Z + FrMastLength * sin(gammaE);
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
    KX[1] := rP[ooE].X; { Angriffspunkt Mastcontroller }
    KY[1] := rP[ooE].Z;
    KX[2] := rP[ooP].X; { Saling }
    KY[2] := rP[ooP].Z;
    KX[3] := rP[ooC].X; { Angriffspunkt Wante }
    KY[3] := rP[ooC].Z;

    KX[4] := rP[ooC0].X;
    KY[4] := rP[ooC0].Z;
    KX[5] := rP[ooD0].X;
    KY[5] := rP[ooD0].Z;
    KX[6] := rP[ooA0].X;
    KY[6] := rP[ooA0].Z;

    if SalingTyp = stOhneStarr then
    begin
      temp := sqrt(sqr(rL[8] + rL[13]) - sqr(rL[6] / 2));
      temp := arctan2(rL[6] / 2, temp);
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
  P0D0 := (rP[ooP0] - rP[ooD0]).Length;
  P0C0 := (rP[ooP0] - rP[ooC0]).Length;
  P0C := (rP[ooP0] - rP[ooC]).Length;

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
    if SalingTyp = stOhneStarr then
      F := Fachwerk.WantenPower;
    if SalingTyp = stOhneBiegt then
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
    d1 := rP[ooC] - rP[ooA0];
    d2 := rP[ooC0] - rP[ooA0];
    d3 := rP[ooD0] - rP[ooA0];
    d4 := rP[ooB0] - rP[ooA0];
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
  Temp: TPoint3D;
  Skalar: single;
  s: String;
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
    Temp := rPe[ooC] - rPe[ooD0];
    Skalar := rLe[16] / (rLe[15] + rLe[16]); { Mastunten / Mast }
    Temp.X := Skalar * Temp.X;
    Temp.Z := Skalar * Temp.Z;
    rPe[ooD] := rPe[ooD0] + Temp;

    Skalar := rLe[13] / (rLe[8] + rLe[13]); { Woben3d / Wante3d }
    rPe[ooP].X := rPe[ooC].X - Skalar * (rPe[ooC].X - rPe[ooP0].X);
    rPe[ooP].Y := 0;
    rPe[ooP].Z := rPe[ooC].Z - Skalar * (rPe[ooC].Z - rPe[ooP0].Z);

    rPe[ooA] := rPe[ooP];
    rPe[ooA].Y := Skalar * rPe[ooA0].Y;
    rPe[ooB] := rPe[ooA];
    rPe[ooB].Y := -rPe[ooA].Y;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - arctan2((rPe[ooC].X - rPe[ooD0].X), (rPe[ooC].Z - rPe[ooD0].Z));
    rPe[ooF].X := rPe[ooD0].X + FrMastLength * cos(gammaE);
    rPe[ooF].Y := 0;
    rPe[ooF].Z := rPe[ooD0].Z + FrMastLength * sin(gammaE);

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
