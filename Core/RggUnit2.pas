unit RggUnit2;

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
  System.Types,
  System.Math,
  System.Math.Vectors,
  RggStrings,
  RggTypes,
  RggCalc,
  RggUnit1;

type
  TMast = class(TGetriebeFS)
  private
    l0: single; { in mm }

    FLineCountM: Integer;
    FControllerTyp: TControllerTyp;
    FCalcTyp: TCalcTyp;
    FMastStatus: TMastStatusSet;
    FKorrigiert: Boolean;

    procedure CalcW1W2;
    procedure CalcW1;
    procedure CalcW2;
    procedure CalcWante;
    procedure Clear;
    procedure FanIn;
    procedure FanOut;
    procedure GetEpsilon;
    function GetKoppelFaktor: single;
    procedure SolveKG21(KM, KU1, KU2, KB: TPoint3D; var FU1, FU2, FB: single);
  protected
    FEx, FEy, FDx, FDy, FD0x, FD0y, FCx, FCy: single;
    FE, FD, FAx, FAy, FALx, FALy, FLvon1, FLvon2, FALvon12: single;

    function GetRiggLengths: TRiggRods;
    function GetDurchbiegungHD: single;
    function GetMastBeta: single;
    function GetMastLC: single;

    function GetCalcTyp: TCalcTyp;
    procedure SetCalcTyp(const Value: TCalcTyp);

    function GetKorrigiert: Boolean;
    procedure SetKorrigiert(const Value: Boolean);
    procedure SetEI(const Value: Integer);
    function GetEI: Integer;
    function GetControllerTyp: TControllerTyp;
    procedure SetControllerTyp(const Value: TControllerTyp);
    function GetMastLinie: TLineDataR100;

    procedure LoadFromIniFile(ini: TIniFile); override;
    procedure WriteToIniFile(ini: TIniFile); override;

    procedure Abstaende;
    procedure BerechneF; override;
    procedure KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single); override;
  public
    LineData: TLineDataR100; { Durchbiegungswerte in mm }

    ControllerFree: Boolean;
    BiegungE: single; { in mm }
    MastPositionE: single;
    hd, he, lc, ld, le: single; { in mm }
    F1, F2, FA, FB, FC: single; { in N }
    EI: single; { in Nmm^2 }

    { gammaE bedeutet gammaEntlastet und wird in RggUnit3 verwendet, hier nicht }
    Beta, Gamma, gammaE, delta1, delta2, alpha1, alpha2: single; { in rad }
    eps1, eps2, epsA, epsB: single; { in rad }

    rL: TRiggRods; { Längen belastet 3d in mm }

    FExcenter: single; { in mm, Erfahrungswert }
    FKnicklaenge: single; { in mm }
    FXpos: single; { in mm }
    FSchnittPunktKraft, { in N }
    FwSchnittOhne, { in N }
    FwSchnittMit, { in N }
    FwSchnittOffset: single; { in mm }
    FControllerWeg: single; { in mm }
    FSalingWeg: single; { in mm }
    FSalingWegKnick: single; { in mm }
    FKoppelFaktor: single; { dimensionslos }
    FKorrekturFaktor: single; { dimensionlos }
    FSalingAlpha: single; { in mm/N }
    FControllerAlpha: single; { in mm/N }

    constructor Create;

    procedure CalcWKnick;
    procedure GetSalingWeg;
    procedure GetControllerWeg;
    procedure GetSalingWegKnick;
    function WvonF(f: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
    function FvonW(WSoll: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
    procedure SchnittKraefte;
    procedure ResetMastStatus;
    function GetMastStatusText: string;
    procedure GetMastPositionE;

    procedure UpdateMastGraph(Model: TMastGraphModel);

    property MastEI: Integer read GetEI write SetEI;
    property MastStatus: TMastStatusSet read FMastStatus;
    property LineCountM: Integer read FLineCountM write FLineCountM;
    property KoppelFaktor: single read FKoppelFaktor;
    property SalingAlpha: single read FSalingAlpha;
    property Korrigiert: Boolean read GetKorrigiert write SetKorrigiert;
    property ControllerTyp: TControllerTyp read GetControllerTyp write SetControllerTyp;
    property CalcTyp: TCalcTyp read GetCalcTyp write SetCalcTyp;
    property MastLinie: TLineDataR100 read GetMastLinie;
    property MastStatusText: string read GetMastStatusText;
    property DurchbiegungHD: single read GetDurchbiegungHD;
    property MastBeta: single read GetMastBeta;
    property MastLC: single read GetMastLC;
    property RiggLengths: TRiggRods read GetRiggLengths;
  end;

implementation

uses
  RiggVar.App.Main;

{ TMast }

constructor TMast.Create;
begin
  FKorrigiert := True;
  FCalcTyp := ctBiegeKnicken;
  FControllerTyp := ctDruck;
  FLineCountM := 100;
  EI := 14.7E9; { Nmm^2 }
  FExcenter := 20.0; { in mm }
  FKnicklaenge := 4600.0; { in mm }
  FKorrekturFaktor := 0.8; { dimensionlos }
  { Achtung: inherited Create ruft virtuelle Funktionen auf, deshalb muß
    z.Bsp. EI vorher initialisiert werden, sonst Division durch Null. }
  inherited Create;
end;

procedure TMast.SetCalcTyp(const Value: TCalcTyp);
begin
  FCalcTyp := Value;
end;

procedure TMast.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
end;

procedure TMast.SetEI(const Value: Integer);
begin
  { EI Werte intern in Nmm^2, extern in Nm^2 }
  EI := Value * 1E6;
end;

procedure TMast.SetKorrigiert(const Value: Boolean);
begin
  FKorrigiert := Value;
end;

function TMast.GetEI: Integer;
begin
  { EI Werte intern in Nmm^2, extern in Nm^2 }
  result := Round(EI / 1E6);
end;

procedure TMast.Clear;
var
  i: Integer;
begin
  F1 := 0;
  F2 := 0;
  FA := 0;
  FB := 0;
  FC := 0;
  for i := 0 to FLineCountM do
    LineData[i] := 0;
end;

procedure TMast.ResetMastStatus;
begin
  FMastOK := True;
  Exclude(FMastStatus, msBiegungNegativ);
  Exclude(FMastStatus, msControllerJenseits);
  Exclude(FMastStatus, msZugKraftimMast);
  Exclude(FMastStatus, msControllerKraftzuGross);
end;

function TMast.GetMastStatusText: string;
var
  s: string;
begin
  s := Status_String_Mast;
  if FMastOK then
    s := s + Status_String_OK
  else
  begin
    if msBiegungNegativ in FMastStatus then
      s := s + Status_String_MastBiegungNegativ
    else if msControllerJenseits in FMastStatus then
      s := s + Status_String_MastControllerBeyondMiddle
    else if msZugKraftimMast in FMastStatus then
      s := s + Status_String_MastControllerTooFarBack
    else if msControllerKraftZuGross in FMastStatus then
      s := s + Status_String_MastControllerTooFar;
  end;
  result := s;
end;

function TMast.GetRiggLengths: TRiggRods;
begin
  result := rL;
end;

procedure TMast.WriteToIniFile(ini: TIniFile);
var
  s: string;
  tempEI: Integer;
begin
  inherited WriteToIniFile(ini);
  s := Rigg_IniSectionString;
  ini.WriteInteger(s, ControllerTyp_IniString, Ord(FControllerTyp));
  ini.WriteInteger(s, CalcTyp_IniString, Ord(FCalcTyp));

  s := Mast_IniSectionString;
  tempEI := Round(EI / 1E6);
  ini.WriteInteger(s, EI_IniString, tempEI);
end;

procedure TMast.LoadFromIniFile(ini: TIniFile);
var
  s: String;
begin
  inherited LoadFromIniFile(ini);
  s := Rigg_IniSectionString;
  ControllerTyp := TControllerTyp(ini.ReadInteger(S, ControllerTyp_IniString, Ord(ctDruck)));
  CalcTyp := TCalcTyp(ini.ReadInteger(S, CalcTyp_IniString, Ord(ctBiegeKnicken)));

  s := Mast_IniSectionString;
  EI := ini.ReadInteger(S, EI_IniString, 14700) * 1E6;
end;

procedure TMast.GetEpsilon;
var
  i: Integer;
  DeltaL: single;
begin
  DeltaL := lc / 100;
  epsA := arctan2(LineData[1], DeltaL);
  epsB := -arctan2(LineData[FLineCountM - 1], DeltaL);

  i := Round(le / lc * 100);
  if i < 1 then
  begin
    eps1 := 0;
    eps2 := 0;
    Exit;
  end;
  if i > FLineCountM then
  begin
    eps1 := 0;
    eps2 := 0;
    Exit;
  end;
  eps1 := arctan2((LineData[i] - LineData[i - 1]), DeltaL);

  i := Round(ld / lc * 100);
  if i < 1 then
  begin
    eps2 := 0;
    Exit;
  end;
  if i > FLineCountM then
  begin
    eps2 := 0;
    Exit;
  end;
  eps2 := arctan2((LineData[i] - LineData[i - 1]), DeltaL);
end;

procedure TMast.CalcW1W2;
var
  alpha11, alpha22, alpha12, alpha21: single; { in mm/N }
  a01, a02: single; { in mm/N }
  i: Integer;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3;
  alpha12 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  alpha21 := alpha12;

  { Kräfte in N }
  F1 := -(alpha12 * hd - alpha22 * he) / (alpha11 * alpha22 - alpha21 * alpha12);
  F2 := (alpha11 * hd - alpha21 * he) / (alpha11 * alpha22 - alpha21 * alpha12);
  FA := -(F1 * le + F2 * ld - lc * F1 - lc * F2) / lc;
  FB := (F1 * le + F2 * ld) / lc;

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < le then
    begin
      { Durchbiegung in Feld 1 }
      a01 := l0 * (lc - le) * (lc * lc - l0 * l0 - Sqr(lc - le)) / lc / EI / 6;
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6;
    end
    else if l0 < ld then
    begin
      { Durchbiegung in Feld 2 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6;
    end
    else
    begin
      { Durchbiegung in Feld 3 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;
      a02 := (lc - l0) * ld * (lc * lc - Sqr(lc - l0) - ld * ld) / lc / EI / 6;
    end;

    LineData[i] := a01 * F1 + a02 * F2;
  end;
  ControllerFree := False;
end;

procedure TMast.CalcW1;
var
  alpha11: single;
  a01: single;
  i: Integer;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;

  { Kräfte in N }
  F1 := he / alpha11;
  F2 := 0;
  FA := F1 * (lc - le) / lc;
  FB := F1 * le / lc;

  a01 := (lc - ld) * le * (lc * lc - Sqr(lc - ld) - le * le) / lc / EI / 6;
  FSalingWeg := a01 * F1;

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < le then
      { Durchbiegung in Feld 1 }
      a01 := l0 * (lc - le) * (lc * lc - l0 * l0 - Sqr(lc - le)) / lc / EI / 6
    else
      { Durchbiegung in Feld 2 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;

    LineData[i] := a01 * F1;
  end;
  ControllerFree := False;
end;

procedure TMast.CalcW2;
var
  alpha22: single;
  a02: single;
  i: Integer;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }

  if alpha22 = 0 then
  begin
    { Wenn F0C Position clamped because out of range (not between Min, Max) }
    F1 := 0;
    F2 := 0;
    FA := 0;
    FB := 0;
    BiegungE := 0;
    ControllerFree := True;
    Inc(ExitCounter3);
    Exit;
  end;

  { Kräfte in N }
  F1 := 0;
  F2 := hd / alpha22;
  FA := F2 * (lc - ld) / lc;
  FB := F2 * ld / lc;

  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  FControllerWeg := a02 * F2;
  if (FControllerTyp <> ctOhne) and (FControllerWeg > he) then
    Exit;
  { Weiterrechnen nur, wenn Controller nicht anliegt. }

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < ld then
      { Durchbiegung in Feld 1 }
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6
    else
      { Durchbiegung in Feld 2 }
      a02 := (lc - l0) * ld * (lc * lc - Sqr(lc - l0) - ld * ld) / lc / EI / 6;

    LineData[i] := a02 * F2;
  end;

  { Durchbiegung an der Stelle le }
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  BiegungE := a02 * F2;
  ControllerFree := True;
end;

procedure TMast.CalcWante;
var
  FU1, FU2, FBekannt: single;
  l2, h, alpha: single;
begin
  FU1 := 0;
  FU2 := 0;
  GetWantenSpannung;
  { 1. Wantenkraft3Dto2D; FB ermitteln }
  h := rP.P0.Distance(rP.P);
  l2 := rL.A0B0 - rL.AB; { PüttingAbstand - SalingAbstand }
  alpha := arctan2(l2 / 2, h);
  FBekannt := WantenSpannung * cos(alpha) * 2; { Wantenspannung2d }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      { Gleichgewicht am Punkt ooP }
      {         KM    KU1   KU2   KB     FU1  FU2  FB  }
      SolveKG21(rP.P, rP.D, rP.C, rP.P0, FU1, FU2, FBekannt);
      { Winkel alpha2 ermitteln }
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta2 := SKK.AngleZXM(rP.A, rP.D);
      Beta := Gamma - pi / 2;
      alpha2 := Beta + delta2;
      F1 := 0;
      F2 := -FU1 * cos(alpha2);
      FA := F2 * (lc - ld) / lc;
      FB := F2 * ld / lc;
    end;
    stOhneBiegt:
    begin
      { Gleichgewicht am Punkt ooC }
      {         KM    KU1    KU2    KB     FU1  FU2  FB  }
      SolveKG21(rP.C, rP.D0, rP.C0, rP.P0, FU1, FU2, FBekannt);
      F1 := 0;
      F2 := 0;
      FA := 0;
      FB := 0;
      FC := FU1;
    end;
  end;
end;

procedure TMast.GetSalingWeg;
{ aus CalcW1 abgeleitet. Ermittelt die Durchbiegung hd, wenn he vorgegeben ist
  und die Salingkraft F2 Null ist. }
var
  alpha11, tempF1, a01: single;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;
  FControllerAlpha := alpha11; { im mm/N, wird in CalcWKnick gebraucht! }
  tempF1 := he / alpha11;
  a01 := (lc - ld) * le * (lc * lc - Sqr(lc - ld) - le * le) / lc / EI / 6;
  FSalingWeg := a01 * tempF1; { in mm }
end;

function TMast.GetCalcTyp: TCalcTyp;
begin
  result := FCalcTyp;
end;

function TMast.GetControllerTyp: TControllerTyp;
begin
  result := FControllerTyp;
end;

procedure TMast.GetControllerWeg;
{ aus CalcW2 abgeleitet. Ermittelt die Durchbiegung he, wenn hd vorgegeben ist
  und die Controllerkraft F1 Null ist. }
var
  alpha22, tempF2, a02: single;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }
  tempF2 := hd / alpha22;
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  FControllerWeg := a02 * tempF2; { in mm }
end;

function TMast.GetDurchbiegungHD: single;
begin
  result := hd;
end;

procedure TMast.GetSalingWegKnick;
var
  Zaehler: Integer;
  WegDiff, WegIst, WegSoll: single; { in mm } { steht für FControllerWeg }
  Temp, TempA, TempB: single; { in mm } { steht für FSalingWegKnick }
  Kraft, alpha22, a02: single; { Zwischenwerte }
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;

  if he < 0 then
    Exit;

  WegSoll := he;
  Zaehler := 0;
  TempA := 0; { linke Begrenzung für FSalingWegKnick in mm }
  TempB := 1000; { rechte Begrenzung für FSalingWegKnick in mm }
  repeat
    Zaehler := Zaehler + 1;
    temp := (TempA + TempB) / 2;
    { zwei Zeilen aus GetControllerWeg }
    Kraft := temp / alpha22; { in N }
    WegIst := a02 * Kraft; { in mm } { FControllerWeg }

    WegDiff := WegIst - WegSoll; { Diff in mm }
    if WegDiff > 0 then
      TempB := temp
    else
      TempA := temp;
  until (abs(WegDiff) < 0.1) or (Zaehler = 100); { kleiner 0.1 mm }

  FSalingWegKnick := Temp;
end;

procedure TMast.CalcWKnick;
var
  Kurve: TKurvenTyp;
  WSoll: single; { in mm }
  Mastdruck: single; { in N }
  ControllerKraft: single; { in N }
  SalingKraft: single; { in N }
begin
  ResetMastStatus;
  GetControllerWeg; { berechnet FControllerWeg und FSalingAlpha }
  GetSalingWeg; { berechnet FSalingWeg und FControllerAlpha }

  FKnicklaenge := lc;
  FXpos := ld;
  FKoppelFaktor := GetKoppelFaktor; { wird in FvonW --> WvonF schon gebraucht }
  GetSalingWegKnick;

  if hd < 0 then
  begin
    FMastOK := False;
    Include(FMastStatus, msBiegungNegativ);
  end;

  if ControllerTyp <> ctOhne then
    if ((hd > 0) and (he < 0)) or ((hd < 0) and (he > 0)) then
    begin
      FMastOK := False;
      Include(FMastStatus, msControllerJenseits);
    end;

  { zutreffende Knickkurve bestimmen }
  Kurve := KurveOhneController;
  case FControllerTyp of
    ctOhne:
      Kurve := KurveOhneController;
    ctDruck:
      begin
        Kurve := KurveOhneController;
        if FSalingWegKnick < hd then
          Kurve := KurveMitController;
    end;
    ctZugDruck:
      Kurve := KurveMitController;
  end;

  { FwSchnittOffset für Knickkurve festlegen }
  if Kurve = KurveOhneController then
    FwSchnittOffset := 0
  else if Kurve = KurveMitController then
  begin
    FwSchnittOhne := FSalingWegKnick;
    FSchnittPunktKraft := FvonW(FwSchnittOhne, KurveOhneController, FKorrigiert);
    FwSchnittMit := WvonF(FSchnittPunktKraft, KurveMitController, False);
    FwSchnittOffset := FwSchnittOhne - FwSchnittMit;
  end;

  { MastDruck bestimmen }
  WSoll := hd - FwSchnittOffset;
  if WSoll < 0 then
  begin
    FMastOK := False;
    Include(FMastStatus, msZugKraftimMast);
  end;
  if Kurve = KurveOhneController then
    MastDruck := FvonW(WSoll, Kurve, FKorrigiert)
  else
    MastDruck := FvonW(WSoll, Kurve, False);

  { Controllerkraft ermitteln }
  if Kurve = KurveOhneController then
    ControllerKraft := 0
  else
  begin
    ControllerKraft := (FControllerWeg - he) / FControllerAlpha;
    if abs(ControllerKraft) > 1000 then
    begin
      ControllerKraft := 1000;
      FMastOK := False;
      Include(FMastStatus, msControllerKraftzuGross);
    end;
  end;

  { Salingkraft ermitteln }
  SalingKraft := FKoppelFaktor * MastDruck;

  if SalingTyp <> stOhneBiegt then
  begin
    F1 := -ControllerKraft;
    F2 := SalingKraft;
    FA := -(F1 * le + F2 * ld - lc * F1 - lc * F2) / lc;
    { Mastfuß ohne Einfluß der Druckkraft im Mast }
    FB := (F1 * le + F2 * ld) / lc;
    { Wantangriffspunkt ohne Einfluß der Druckkraft im Mast }
    FC := -MastDruck;
    { Druckkraft ist negativ. FC wird nur bei stOhneBiegt verwendet,
      die Druckkraft im Mast ergibt sich sonst über den Umweg der Salingkraft. }
  end;

  if SalingTyp = stOhneBiegt then
  begin
    F1 := -ControllerKraft;
    F2 := 0; { Salingkraft, hier immer Null }
    FA := F1 * (lc - le) / lc; { Mastfuß ohne Einfluß von FC }
    FB := F1 * le / lc; { Wantangriffspunkt ohne Einfluß von FC }
    FC := -MastDruck; { neg. Vorzeichen, da Druckkraft }
  end;
end;

function TMast.FvonW(WSoll: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
{ WSoll in mm, result in N }
var
  Zaehler: Integer;
  Knicklaenge: single; { in mm }
  Diff, WIst: single; { in mm }
  FTemp, FTempA, FTempB: single; { in N }
begin
  result := 0;
  if WSoll < 0 then
    Exit;
  { Die Kraft numerisch ermitteln, da die Umkehrfunktion zu WvonF noch nicht
    explizit vorliegt. }
  Knicklaenge := FKnicklaenge;
  if Kurve = KurveMitController then
    Knicklaenge := FKorrekturFaktor * FKnicklaenge;

  Zaehler := 0;
  FTempA := 0; { in N }
  FTempB := EI * 3.14 * 3.14 / Knicklaenge / Knicklaenge; { Knicklast in N }
  repeat
    Zaehler := Zaehler + 1;
    FTemp := (FTempA + FTempB) / 2;
    WIst := WvonF(FTemp, Kurve, Korrigiert); { WIst in mm }
    Diff := WIst - WSoll; { Diff in mm }
    if Diff > 0 then
      FTempB := FTemp
    else
      FTempA := FTemp;
  until (abs(Diff) < 0.1) or (Zaehler = 100);

  result := FTemp;
end;

function TMast.WvonF(f: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
{ F in N, result in mm }
var
  k: single;
  Knicklaenge: single;
begin
  Knicklaenge := FKnicklaenge;
  if Kurve = KurveMitController then
    Knicklaenge := FKorrekturFaktor * FKnicklaenge;

  k := sqrt(f / EI); { k in mm^-1, F in N, EI in Nmm^2 }
  result := FExcenter * (tan(k * Knicklaenge / 2) * sin(k * FXpos) + cos(k * FXpos) - 1);

  { Jetzt noch berücksichtigen, daß Auslenkung des Puntes D teilweise durch
    die Salingkraft bedingt ist. }
  if Korrigiert = True then
    result := result + FKoppelFaktor * f * FSalingAlpha;
end;

function TMast.GetKoppelFaktor: single;
var
  FU1, FU2, FB: single;
begin
  FU1 := 0;
  FU2 := 0;
  result := 0;
  case SalingTyp of
    stOhneStarr, stOhneBiegt:
      result := 0;
    stFest, stDrehbar:
    begin
      FB := 1; { bekannte Kraft vom Betrag 1 im Mast }
      (*
      KM  betrachteter Knoten
          KU1 Knoten der zur 1. unbekannten Stabkraft FU1 gehört
          KU2 Knoten der zur 2. unbekannten Stabkraft FU2 gehört
          KB  Knoten der zur bekannten Stabkraft FB gehört
                KM    KU1    KU2   KB     FU1  FU2  FB *)
      SolveKG21(rP.C, rP.C0, rP.P, rP.D0, FU1, FU2, FB);

      FB := FU2;
      {         KM    KU1   KU2    KB    FU1  FU2  FB  }
      SolveKG21(rP.P, rP.D, rP.P0, rP.C, FU1, FU2, FB);
        result := FU1; { selbe Einheit wie FB }
    end;
  end;
end;

function TMast.GetKorrigiert: Boolean;
begin
  result := FKorrigiert;
end;

procedure TMast.SolveKG21(KM, KU1, KU2, KB: TPoint3D; var FU1, FU2, FB: single);
var
  DX1, DY1, W1: single;
  DX2, DY2, W2: single;
  DX3, DY3, W3: single;
  D, D1, D2: single;
  BekanntFX, BekanntFY: single;
  s: string;
begin
  W1 := -1;
  W2 := -1;
  W3 := -1;
  D := -1;
  try
    { unbekannte Kraft Nr.1 }
    DX1 := KU1.X - KM.X; { delta x }
    DY1 := KU1.Z - KM.Z; { delta y }
    W1 := sqrt(Sqr(DX1) + Sqr(DY1)); { Stablänge }
    { unbekannte Kraft Nr.2 }
    DX2 := KU2.X - KM.X; { delta x }
    DY2 := KU2.Z - KM.Z; { delta y }
    W2 := sqrt(Sqr(DX2) + Sqr(DY2)); { Stablänge }
    { bekannte Kraft Nr.3 }
    DX3 := KB.X - KM.X; { delta x }
    DY3 := KB.Z - KM.Z; { delta y }
    W3 := sqrt(Sqr(DX3) + Sqr(DY3)); { Stablänge }
    { Summe der bekannten Kräfte }
    { mit DX/W = cos alpha, DY/W = sin alpha }
    BekanntFX := -FB * DX3 / W3;
    BekanntFY := -FB * DY3 / W3;
    { Ausrechnen der Stabkräfte }
    D := DX1 * DY2 - DX2 * DY1;
    if D = 0 then
    begin
      { we do not want exceptions when running in the debugger }
      FU1 := 0;
      FU2 := 0;
      Inc(ExitCounter4);
      Exit;
    end;
    D1 := BekanntFX * DY2 - BekanntFY * DX2;
    D2 := BekanntFY * DX1 - BekanntFX * DY1;
    FU1 := D1 / D * W1; { 1. neu ermittelte Stabkraft }
    FU2 := D2 / D * W2; { 2. neu ermittelte Stabkraft }
  except
    on EZeroDivide do
    begin
      { D ist Null, wenn FU1 und FU2 auf einer Geraden liegen. }
      FU1 := 0;
      FU2 := 0;
      s := LogString_SolveKG21_Except;
      if W1 = 0 then
        s := s + LogString_W1;
      if W2 = 0 then
        s := s + LogString_W2;
      if W3 = 0 then
        s := s + LogString_W3;
      if D = 0 then
        s := s + LogString_D;
      s := s + LogString_AreNull;
      Main.Logger.Info(s);
    end;
  end;
end;

procedure TMast.SchnittKraefte;
begin
  FanIn;

  case SalingTyp of
    stFest, stDrehbar:
    begin
      case ControllerTyp of
        ctOhne: CalcW2;
        ctDruck:
        begin
          CalcW2;
          if (FControllerWeg > he) then
            CalcW1W2;
        end;
        ctZugDruck: CalcW1W2;
      end;
      if CalcTyp = ctBiegeKnicken then
        CalcWKnick;
      if CalcTyp = ctKraftGemessen then
        CalcWante;
    end;

    stOhneStarr:
    begin
      case ControllerTyp of
        ctOhne: ; { do nothing }
        ctDruck:
          if (he < 0) then
            CalcW1;
        ctZugDruck: CalcW1;
      end;
    end;

    stOhneBiegt:
    begin
      case ControllerTyp of
        ctOhne: CalcW2;
        ctDruck:
        begin
          CalcW2;
          if (FControllerWeg > he) then
            CalcW1W2;
        end;
        ctZugDruck: CalcW1W2;
      end;
      if CalcTyp = ctKraftGemessen then
        CalcWante
      else
        CalcWKnick; { sonst immer BiegeKnicken }
    end;

  end;
  FanOut;
end;

procedure TMast.FanIn;
var
  SPSaling, SPController: TPoint3D;
  k1, k2, EC: single;
begin
  Abstaende;

  { Geometrie für Mastsystem }
  case SalingTyp of
    stFest, stDrehbar, stOhneBiegt:
    begin
      SchnittGG(rP.D0, rP.C, rP.P, rP.D, SPSaling);
      SchnittGG(rP.D0, rP.C, rP.E, rP.E0, SPController);
      ld := rP.D0.Distance(SPSaling);
      le := rP.D0.Distance(SPController);
      lc := rL.D0C;
      EC := rP.C.Distance(rP.E);
      hd := Hoehe(lc - 0.0001, rL.D0D, rL.DC, k2);
      he := Hoehe(lc - 0.0001, rL.D0E, EC, k1);
      if SPSaling.X - rP.D.X > 0 then
        hd := -hd;
      if SPController.X - rP.E.X > 0 then
        he := -he;
    end;

    stOhneStarr:
    begin
      SchnittGG(rP.D0, rP.C, rP.E, rP.E0, SPController);
      ld := rL.D0D;
      le := rP.D0.Distance(SPController);
      lc := rL.D0C;
      EC := rP.C.Distance(rP.E);
      hd := 0; { Null gesetzt, da nicht relevant }
      he := Hoehe(lc - 0.0001, rL.D0E, EC, k1);
      if SPController.X - rP.E.X > 0 then
        he := -he;
    end;
  end;

  Clear; { bei ctOhne wird hier die Mastlinie genullt }
end;

procedure TMast.FanOut;
begin
  { Winkel }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta1 := SKK.AngleZXM(rP.E, rP.C0);
      delta2 := SKK.AngleZXM(rP.A, rP.D);
      Beta := Gamma - pi / 2;
      alpha1 := Beta + delta1;
      alpha2 := Beta + delta2;
    end;

    stOhneStarr, stOhneBiegt:
    begin
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta1 := SKK.AngleZXM(rP.E, rP.C0);
      delta2 := 0; { Null gesetzt, da nicht relevant }
      Beta := Gamma - pi / 2;
      alpha1 := Beta + delta1;
      alpha2 := 0; { Null gesetzt, da nicht relevant }
    end;
  end;

  GetMastPositionE;

  { Kraftkomponenten bereitstellen }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      try
        FE := F1 / cos(alpha1);
        FD := F2 / cos(alpha2);
      except
        on EZeroDivide do
        begin
          FE := 0;
          FD := 0;
          Main.Logger.Info(LogString_ZeroDivideAlpha);
        end;
      end;
      FLvon1 := FE * sin(alpha1);
      FLvon2 := FD * sin(alpha2);
      FALvon12 := FLvon1 + FLvon2;

      FAx := FA * cos(Beta);
      FAy := FA * sin(Beta);
      FALx := FALvon12 * sin(Beta);
      FALy := FALvon12 * cos(Beta);

      FD0x :=  FAx + FALx;
      FD0y := -FAy + FALy;
      FCx := FB * cos(Beta);
      FCy := FB * sin(Beta);
      { Mastdruckkraft FC hier nicht enthalten, im Fachwerkmodul
        wird später spezielle Prozedur für Stabkräfte aufgerufen. }

      FEx := FE * cos(delta1);
      FEy := FE * sin(delta1);
      FDx := FD * cos(delta2);
      FDy := FD * sin(delta2);
    end;

    stOhneStarr, stOhneBiegt:
    begin
      try
        FE := F1 / cos(alpha1);
        FD := 0; { Null gesetzt, da nicht relevant }
      except
        on EZeroDivide do
        begin
          FE := 0;
          FD := 0;
          Main.Logger.Info(LogString_ZeroDivideAlpha);
        end;
      end;
      FLvon1 := FE * sin(alpha1);
      FLvon2 := 0; { Null gesetzt, da nicht relevant }
      FALvon12 := FLvon1 + FLvon2;

      FAx := FA * cos(Beta);
      FAy := FA * sin(Beta);
      FALx := FALvon12 * sin(Beta);
      FALy := FALvon12 * cos(Beta);

      FD0x :=  FAx + FALx;
      FD0y := -FAy + FALy;
      FCx := FB * cos(Beta);
      FCy := FB * sin(Beta);
      { Mastdruckkraft FC hier nicht enthalten,
          im Fachwerkmodul wird später spezielle Prozedur für Stabkräfte aufgerufen. }

      FEx := FE * cos(delta1);
      FEy := FE * sin(delta1);
      FDx := 0; { Null gesetzt, da nicht relevant }
      FDy := 0; { Null gesetzt, da nicht relevant }
    end;
  end;
end;

procedure TMast.Abstaende;
begin
  { Abstände ermitteln }
  rL.V[0] := rP.D0.Distance(rP.C);
  rL.V[1] := rP.D0.Distance(rP.C0);
  rL.V[2] := rP.B0.Distance(rP.C0);
  rL.V[3] := rP.A0.Distance(rP.C0);
  rL.V[4] := rP.B0.Distance(rP.D0);
  rL.V[5] := rP.A0.Distance(rP.D0);
  rL.V[6] := rP.A0.Distance(rP.B0);
  rL.V[7] := rP.B0.Distance(rP.B);
  rL.V[8] := rP.A0.Distance(rP.A);
  rL.V[9] := rP.B.Distance(rP.D);
  rL.V[10] := rP.A.Distance(rP.D);
  rL.V[11] := rP.A.Distance(rP.B);
  rL.V[12] := rP.B.Distance(rP.C);
  rL.V[13] := rP.A.Distance(rP.C);
  rL.V[14] := rP.C0.Distance(rP.C);
  rL.V[15] := rP.C.Distance(rP.D);
  rL.V[16] := rP.D0.Distance(rP.D);
  rL.V[17] := rP.D.Distance(rP.E);
  rL.V[18] := rP.D0.Distance(rP.E);
  rL.V[19] := rP.E0.Distance(rP.E);
end;

procedure TMast.BerechneF;
begin
  { überschriebene virtuelle Methode von TGetriebeFS }
  { Berechnung Punkt F - Masttop }
  SchnittKraefte;
  GetEpsilon;
  FrEpsilon := pi / 2 - SKK.AngleXZ(rP.C, rP.D0) - epsB;
  rP.F := SKK.AnglePointXZ(rP.C, FrMastEnde, FrEpsilon);
end;

procedure TMast.KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single);
var
  k8, temp: single;
begin
  { überschriebene virtuelle Methode von TGetriebeFS }
  { k3 und Beta näherungsweise(!) neu bestimmen:
    dazu als erstes epsB bestimmen }
  he := 200; { Controller soll nicht anliegen }
  hd := tempH; { Durchbiegung an den Salingen }
  le := 500; { oder alter Wert le, ohne Bedeutung }
  ld := k1; { oder alter Wert ld }
  lc := (k1 + k2); { oder alter Wert lc }
  CalcW2;
  GetEpsilon;
  k8 := sin(-epsB) * FrMastEnde * 0.9; { 0.9 empirisch }
  temp := k8 * sin(Gamma - Beta); { delta k3, siehe Zeichnung }
  k3 := k3 - temp;
  Beta := Beta + k8 * cos(Gamma - Beta) / k3 * 0.6; { 0.6 empirisch }
end;

function TMast.GetMastBeta: single;
begin
  result := Beta;
end;

function TMast.GetMastLC: single;
begin
  result := lc;
end;

function TMast.GetMastLinie: TLineDataR100;
begin
  result := LineData;
end;

procedure TMast.GetMastPositionE;
var
  PositionEStrich: single;
begin
  MastPositionE := rP.E.X - rP.D0.X;
  if not ControllerFree then
    Exit;
  PositionEStrich := -le * sin(Beta) + BiegungE * cos(Beta);
  PositionEStrich := PositionEStrich + BiegungE * tan(alpha1) * sin(Beta);
  MastPositionE := PositionEStrich;
end;

procedure TMast.UpdateMastGraph(Model: TMastGraphModel);
begin
  Model.LineData := LineData;
  Model.FLineCountM := FLineCountM;
  Model.GetriebeOK := GetriebeOK;
end;

end.
