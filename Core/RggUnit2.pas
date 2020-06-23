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
  RggStrings,
  RggTypes,
  RggCalc,
  RggUnit1;

type
  TKurvenTyp = (KurveOhneController, KurveMitController);
  TMastStatusSet = set of TMastStatus;

  TMast = class(TGetriebeFS)
  private
    l0: double; { in mm }

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
    function GetKoppelFaktor: double;
    procedure SolveKG21(KM, KU1, KU2, KB: TRealPoint; var FU1, FU2, FB: double);

    procedure SetEI(Value: Integer);
    function GetEI: Integer;

  protected
    FEx, FEy, FDx, FDy, FD0x, FD0y, FCx, FCy: double;
    FE, FD, FAx, FAy, FALx, FALy, FLvon1, FLvon2, FALvon12: double;

    procedure LoadFromIniFile(ini: TIniFile); override;
    procedure WriteToIniFile(ini: TIniFile); override;
    procedure LoadFromStream(S: TStream); override;
    procedure SaveToStream(S: TStream); override;

    procedure Abstaende;
    procedure BerechneF; override;
    procedure KorrekturF(tempH, k1, k2: double; var k3, Beta, Gamma: double); override;
  public
    LineData: TLineDataR100; { Durchbiegungswerte in mm }

    ControllerFree: Boolean;
    BiegungE: double; { in mm }
    MastPositionE: double;
    hd, he, lc, ld, le: double; { in mm }
    F1, F2, FA, FB, FC: double; { in N }
    EI: double; { in Nmm^2 }

    { gammaE bedeutet gammaEntlastet und wird in RggUnit3 verwendet, hier nicht }
    Beta, Gamma, gammaE, delta1, delta2, alpha1, alpha2: double; { in rad }
    eps1, eps2, epsA, epsB: double; { in rad }

    rL: TRiggLvektor; { Längen belastet 3d in mm }

    FExcenter: double; { in mm, Erfahrungswert }
    FKnicklaenge: double; { in mm }
    FXpos: double; { in mm }
    FSchnittPunktKraft, { in N }
    FwSchnittOhne, { in N }
    FwSchnittMit, { in N }
    FwSchnittOffset: double; { in mm }
    FControllerWeg: double; { in mm }
    FSalingWeg: double; { in mm }
    FSalingWegKnick: double; { in mm }
    FKoppelFaktor: double; { dimensionslos }
    FKorrekturFaktor: double; { dimensionlos }
    FSalingAlpha: double; { in mm/N }
    FControllerAlpha: double; { in mm/N }

    constructor Create;

    procedure CalcWKnick;
    procedure GetSalingWeg;
    procedure GetControllerWeg;
    procedure GetSalingWegKnick;
    function WvonF(f: double; Kurve: TKurvenTyp; Korrigiert: Boolean): double;
    function FvonW(WSoll: double; Kurve: TKurvenTyp; Korrigiert: Boolean): double;
    procedure SchnittKraefte;
    procedure ResetMastStatus;
    function MastStatusText: string;
    procedure GetMastPositionE;

    property MastEI: Integer read GetEI write SetEI;
    property MastStatus: TMastStatusSet read FMastStatus;
    property LineCountM: Integer read FLineCountM write FLineCountM;
    property KoppelFaktor: double read FKoppelFaktor;
    property SalingAlpha: double read FSalingAlpha;
    property Korrigiert: Boolean read FKorrigiert write FKorrigiert;
    property MastOK: Boolean read FMastOK;
    property ControllerTyp: TControllerTyp read FControllerTyp write FControllerTyp;
    property CalcTyp: TCalcTyp read FCalcTyp write FCalcTyp;
    property MastLinie: TLineDataR100 read LineData write LineData;
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
  { Achtung: inherited Create() ruft virtuelle Funktionen auf, deshalb muß
    z.Bsp. EI vorher initialisiert werden, sonst Division durch Null! }
  inherited Create;
end;

procedure TMast.SetEI(Value: Integer);
begin
  { EI Werte intern in Nmm^2, extern in Nm^2 }
  EI := Value * 1E6;
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

function TMast.MastStatusText: string;
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

procedure TMast.SaveToStream(S: TStream);
begin
  inherited SaveToStream(S);
  S.WriteBuffer(ControllerTyp, SizeOf(Integer));
  S.WriteBuffer(EI, SizeOf(double));
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

procedure TMast.LoadFromStream(S: TStream);
var
  temp: Integer;
begin
  temp := 0;
  inherited LoadFromStream(S);
  S.ReadBuffer(temp, SizeOf(Integer));
  ControllerTyp := TControllerTyp(temp);
  S.ReadBuffer(EI, SizeOf(double));
end;

procedure TMast.GetEpsilon;
var
  i: Integer;
  DeltaL: double;
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
  alpha11, alpha22, alpha12, alpha21: double; { in mm/N }
  a01, a02: double; { in mm/N }
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
  alpha11: double;
  a01: double;
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
  alpha22: double;
  a02: double;
  i: Integer;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }

  if alpha22 = 0 then
  begin
    //Wenn F0C Position clamped because out of range (not between Min, Max)
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
  FU1, FU2, FBekannt: double;
  l2, h, alpha: double;
begin
  FU1 := 0;
  FU2 := 0;
  GetWantenSpannung;
  { 1. Wantenkraft3Dto2D; FB ermitteln }
  h := Abstand(rP[ooP0], rP[ooP]);
  l2 := rL[6] - rL[11]; { PüttingAbstand - SalingAbstand }
  alpha := arctan2(l2 / 2, h);
  FBekannt := WantenSpannung * cos(alpha) * 2; { Wantenspannung2d }
  case SalingTyp of
    stFest, stDrehbar:
      begin
        { Gleichgewicht am Punkt ooP }
        {         KM       KU1      KU2      KB        FU1  FU2  FB }
        SolveKG21(rP[ooP], rP[ooD], rP[ooC], rP[ooP0], FU1, FU2, FBekannt);
        { Winkel alpha2 ermitteln }
        Gamma := pi / 2 - arctan2((rP[ooC, x] - rP[ooD0, x]), (rP[ooC, z] - rP[ooD0, z]));
        delta2 := arctan2((rP[ooA, z] - rP[ooD, z]), (rP[ooD, x] - rP[ooA, x]));
        Beta := Gamma - pi / 2;
        alpha2 := Beta + delta2;
        F1 := 0;
        F2 := -FU1 * cos(alpha2);
        FA := F2 * (lc - ld) / lc;
        FB := F2 * ld / lc;
      end;
    stOhne_2:
      begin
        { Gleichgewicht am Punkt ooC }
        {         KM       KU1       KU2       KB        FU1  FU2  FB }
        SolveKG21(rP[ooC], rP[ooD0], rP[ooC0], rP[ooP0], FU1, FU2, FBekannt);
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
  alpha11, tempF1, a01: double;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;
  FControllerAlpha := alpha11; { im mm/N, wird in CalcWKnick gebraucht! }
  tempF1 := he / alpha11;
  a01 := (lc - ld) * le * (lc * lc - Sqr(lc - ld) - le * le) / lc / EI / 6;
  FSalingWeg := a01 * tempF1; { in mm }
end;

procedure TMast.GetControllerWeg;
{ aus CalcW2 abgeleitet. Ermittelt die Durchbiegung he, wenn hd vorgegeben ist
  und die Controllerkraft F1 Null ist. }
var
  alpha22, tempF2, a02: double;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }
  tempF2 := hd / alpha22;
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  FControllerWeg := a02 * tempF2; { in mm }
end;

procedure TMast.GetSalingWegKnick;
var
  Zaehler: Integer;
  WegDiff, WegIst, WegSoll: double; { in mm } { steht für FControllerWeg }
  Temp, TempA, TempB: double; { in mm } { steht für FSalingWegKnick }
  Kraft, alpha22, a02: double; { Zwischenwerte }
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
  WSoll: double; { in mm }
  Mastdruck: double; { in N }
  ControllerKraft: double; { in N }
  SalingKraft: double; { in N }
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

  if SalingTyp <> stOhne_2 then
  begin
    F1 := -ControllerKraft;
    F2 := SalingKraft;
    FA := -(F1 * le + F2 * ld - lc * F1 - lc * F2) / lc;
    { Mastfuß ohne Einfluß der Druckkraft im Mast }
    FB := (F1 * le + F2 * ld) / lc;
    { Wantangriffspunkt ohne Einfluß der Druckkraft im Mast }
    FC := -MastDruck;
    { Druckkraft ist negativ. FC wird nur bei stOhne_2 verwendet,
      die Druckkraft im Mast ergibt sich sonst über den Umweg der Salingkraft }
  end;

  if SalingTyp = stOhne_2 then
  begin
    F1 := -ControllerKraft;
    F2 := 0; { Salingkraft, hier immer Null }
    FA := F1 * (lc - le) / lc; { Mastfuß ohne Einfluß von FC }
    FB := F1 * le / lc; { Wantangriffspunkt ohne Einfluß von FC }
    FC := -MastDruck; { neg. Vorzeichen, da Druckkraft }
  end;
end;

function TMast.FvonW(WSoll: double; Kurve: TKurvenTyp; Korrigiert: Boolean): double;
{ WSoll in mm, result in N }
var
  Zaehler: Integer;
  Knicklaenge: double; { in mm }
  Diff, WIst: double; { in mm }
  FTemp, FTempA, FTempB: double; { in N }
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

function TMast.WvonF(f: double; Kurve: TKurvenTyp; Korrigiert: Boolean): double;
{ F in N, result in mm }
var
  k: double;
  Knicklaenge: double;
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

function TMast.GetKoppelFaktor: double;
var
  FU1, FU2, FB: double;
begin
  FU1 := 0;
  FU2 := 0;
  result := 0;
  case SalingTyp of
    stOhne, stOhne_2:
      result := 0;
    stFest, stDrehbar:
      begin
        FB := 1; { bekannte Kraft vom Betrag 1 im Mast }
        (*
          KM  betrachteter Knoten
          KU1 Knoten der zur 1. unbekannten Stabkraft FU1 gehört
          KU2 Knoten der zur 2. unbekannten Stabkraft FU2 gehört
          KB  Knoten der zur bekannten Stabkraft FB gehört
                  KM       KU1       KU2      KB        FU1  FU2  FB *)
        SolveKG21(rP[ooC], rP[ooC0], rP[ooP], rP[ooD0], FU1, FU2, FB);

        FB := FU2;
        {         KM       KU1      KU2       KB       FU1  FU2  FB }
        SolveKG21(rP[ooP], rP[ooD], rP[ooP0], rP[ooC], FU1, FU2, FB);
        result := FU1; { selbe Einheit wie FB }
      end;
  end;
end;

procedure TMast.SolveKG21(KM, KU1, KU2, KB: TRealPoint; var FU1, FU2, FB: double);
var
  DX1, DY1, W1: double;
  DX2, DY2, W2: double;
  DX3, DY3, W3: double;
  D, D1, D2: double;
  BekanntFX, BekanntFY: double;
  s: string;
begin
  W1 := -1;
  W2 := -1;
  W3 := -1;
  D := -1;
  try
    { unbekannte Kraft Nr.1 }
    DX1 := KU1[x] - KM[x]; { delta x }
    DY1 := KU1[z] - KM[z]; { delta y }
    W1 := sqrt(Sqr(DX1) + Sqr(DY1)); { Stablänge }
    { unbekannte Kraft Nr.2 }
    DX2 := KU2[x] - KM[x]; { delta x }
    DY2 := KU2[z] - KM[z]; { delta y }
    W2 := sqrt(Sqr(DX2) + Sqr(DY2)); { Stablänge }
    { bekannte Kraft Nr.3 }
    DX3 := KB[x] - KM[x]; { delta x }
    DY3 := KB[z] - KM[z]; { delta y }
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
end; { KG21 }

procedure TMast.SchnittKraefte;
begin
  FanIn;

  case SalingTyp of
    stFest, stDrehbar:
      begin
        case ControllerTyp of
          ctOhne:
            CalcW2;
          ctDruck:
            begin
              CalcW2;
              if (FControllerWeg > he) then
                CalcW1W2;
            end;
          ctZugDruck:
            CalcW1W2;
        end;
        if CalcTyp = ctBiegeKnicken then
          CalcWKnick;
        if CalcTyp = ctKraftGemessen then
          CalcWante;
      end;

    stOhne:
      begin
        case ControllerTyp of
          ctOhne:
            ; { nichts machen normalerweise }
          ctDruck:
            if (he < 0) then
              CalcW1;
          ctZugDruck:
            CalcW1;
        end;
      end;

    stOhne_2:
      begin
        case ControllerTyp of
          ctOhne:
            CalcW2;
          ctDruck:
            begin
              CalcW2;
              if (FControllerWeg > he) then
                CalcW1W2;
            end;
          ctZugDruck:
            CalcW1W2;
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
  SPSaling, SPController: TRealPoint;
  k1, k2, EC: double;
begin
  Abstaende;

  { Geometrie für Mastsystem }
  case SalingTyp of
    stFest, stDrehbar, stOhne_2:
      begin
        SchnittGG(rP[ooD0], rP[ooC], rP[ooP], rP[ooD], SPSaling);
        SchnittGG(rP[ooD0], rP[ooC], rP[ooE], rP[ooE0], SPController);
        ld := Abstand(rP[ooD0], SPSaling);
        le := Abstand(rP[ooD0], SPController);
        lc := rL[0];
        EC := Abstand(rP[ooC], rP[ooE]);
        hd := Hoehe(lc - 0.0001, rL[16], rL[15], k2);
        he := Hoehe(lc - 0.0001, rL[18], EC, k1);
        if SPSaling[x] - rP[ooD, x] > 0 then
          hd := -hd;
        if SPController[x] - rP[ooE, x] > 0 then
          he := -he;
      end;

    stOhne:
      begin
        SchnittGG(rP[ooD0], rP[ooC], rP[ooE], rP[ooE0], SPController);
        ld := rL[16];
        le := Abstand(rP[ooD0], SPController);
        lc := rL[0];
        EC := Abstand(rP[ooC], rP[ooE]);
        hd := 0; { Null gesetzt, da nicht relevant }
        he := Hoehe(lc - 0.0001, rL[18], EC, k1);
        if SPController[x] - rP[ooE, x] > 0 then
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
        Gamma := pi / 2 - arctan2((rP[ooC, x] - rP[ooD0, x]), (rP[ooC, z] - rP[ooD0, z]));
        delta1 := arctan2((rP[ooE, z] - rP[ooC0, z]), (rP[ooC0, x] - rP[ooE, x]));
        delta2 := arctan2((rP[ooA, z] - rP[ooD, z]), (rP[ooD, x] - rP[ooA, x]));
        Beta := Gamma - pi / 2;
        alpha1 := Beta + delta1;
        alpha2 := Beta + delta2;
      end;

    stOhne, stOhne_2:
      begin
        Gamma := pi / 2 - arctan2((rP[ooC, x] - rP[ooD0, x]), (rP[ooC, z] - rP[ooD0, z]));
        delta1 := arctan2((rP[ooE, z] - rP[ooC0, z]), (rP[ooC0, x] - rP[ooE, x]));
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

        FD0x := FAx + FALx;
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

    stOhne, stOhne_2:
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

        FD0x := FAx + FALx;
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
  rL[0] := Abstand(rP[ooD0], rP[ooC]);
  rL[1] := Abstand(rP[ooD0], rP[ooC0]);
  rL[2] := Abstand(rP[ooB0], rP[ooC0]);
  rL[3] := Abstand(rP[ooA0], rP[ooC0]);
  rL[4] := Abstand(rP[ooB0], rP[ooD0]);
  rL[5] := Abstand(rP[ooA0], rP[ooD0]);
  rL[6] := Abstand(rP[ooA0], rP[ooB0]);
  rL[7] := Abstand(rP[ooB0], rP[ooB]);
  rL[8] := Abstand(rP[ooA0], rP[ooA]);
  rL[9] := Abstand(rP[ooB], rP[ooD]);
  rL[10] := Abstand(rP[ooA], rP[ooD]);
  rL[11] := Abstand(rP[ooA], rP[ooB]);
  rL[12] := Abstand(rP[ooB], rP[ooC]);
  rL[13] := Abstand(rP[ooA], rP[ooC]);
  rL[14] := Abstand(rP[ooC0], rP[ooC]);
  rL[15] := Abstand(rP[ooC], rP[ooD]);
  rL[16] := Abstand(rP[ooD0], rP[ooD]);
  rL[17] := Abstand(rP[ooD], rP[ooE]);
  rL[18] := Abstand(rP[ooD0], rP[ooE]);
  rL[19] := Abstand(rP[ooE0], rP[ooE]);
end;

{ überschriebene virtuelle Methode von TGetriebeFS }
procedure TMast.BerechneF;
begin
  { Berechnung Punkt F - Masttop }
  SchnittKraefte;
  GetEpsilon;
  FrEpsilon := pi / 2 - arctan2((rP[ooC, x] - rP[ooD0, x]), (rP[ooC, z] - rP[ooD0, z])) - epsB;
  rP[ooF, x] := rP[ooC, x] + FrMastEnde * cos(FrEpsilon);
  rP[ooF, y] := 0;
  rP[ooF, z] := rP[ooC, z] + FrMastEnde * sin(FrEpsilon);
end;

{ überschriebene virtuelle Methode von TGetriebeFS }
procedure TMast.KorrekturF(tempH, k1, k2: double; var k3, Beta, Gamma: double);
var
  k8, temp: double;
begin
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

procedure TMast.GetMastPositionE;
var
  PositionEStrich: double;
begin
  MastPositionE := rP[ooE, x] - rP[ooD0, x];
  if not ControllerFree then
    Exit;
  PositionEStrich := -le * sin(Beta) + BiegungE * cos(Beta);
  PositionEStrich := PositionEStrich + BiegungE * tan(alpha1) * sin(Beta);
  MastPositionE := PositionEStrich;
end;

end.
