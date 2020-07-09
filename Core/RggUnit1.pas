unit RggUnit1;

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
  System.Math,
  RggStrings,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggUnit0;

type
  TGetriebeFS = class(TGetriebe)
  private
    FrVorstagDiff: double;
    FrSpannungW: double;
  protected
    procedure BerechneF; virtual;
    procedure KorrekturF(tempH, k1, k2: double; var k3, Beta, Gamma: double); virtual; //deprecated;
  public
    UpdateGetriebeCounter: Integer;

    ExitCounter1: Integer;
    ExitCounter2: Integer;
    ExitCounter3: Integer;
    ExitCounter4: Integer;
    ExitCounter5: Integer;
    ExitCounter6: Integer;
    ExitCounter7: Integer;

    Temp1: double;
    Temp2: double;
    Temp3: double;
    Temp4: double;

    WantToPlayWithExtendedSearchRange: Boolean;

    procedure ResetExitCounters;

    constructor Create;

    procedure ResetStatus;
    procedure UpdateGetriebe;
    procedure UpdateGetriebeFS;
    procedure UpdateGetriebeDS;
    procedure UpdateGetriebeOSS;
    procedure UpdateGetriebeOSB;
    procedure Rest;
    procedure BerechneWinkel;
    procedure BerechneM;
    function Koppelkurve: TKoordLine;
    procedure BiegeUndNeigeF1(Mastfall, Biegung: double);
    procedure NeigeF(Mastfall: double);
    procedure BiegeUndNeigeC(MastfallC, Biegung: double);
    procedure BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: double);
    procedure BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: double);
    procedure MakeSalingHBiggerFS(SalingHplus: double);
    procedure MakeSalingLBiggerDS(SalingLplus: double);
    procedure GetWantenspannung;
    function WantenKraftvonVorstag(WegSoll: double): double;
    function GetVorstagNull: double;

    property VorstagDiff: double read FrVorstagDiff;
    property SpannungW: double read FrSpannungW;
  end;

implementation

constructor TGetriebeFS.Create;
begin
  inherited Create;
  BerechneWinkel;
end;

procedure TGetriebeFS.ResetStatus;
begin
  FGetriebeOK := True;
  Exclude(FGetriebeStatus, gsWanteZukurz);
  Exclude(FGetriebeStatus, gsWanteZulang);
end;

procedure TGetriebeFS.ResetExitCounters;
begin
  ExitCounter1 := 0;
  ExitCounter2 := 0;
  ExitCounter3 := 0;
  ExitCounter4 := 0;
  ExitCounter5 := 0;
  ExitCounter6 := 0;
  ExitCounter7 := 0;
end;

procedure TGetriebeFS.UpdateGetriebe;
begin
  Inc(UpdateGetriebeCounter);
  LogList.Clear;
  case SalingTyp of
    stOhneStarr:
      UpdateGetriebeOSS;
    stOhneBiegt:
      UpdateGetriebeOSB;
    stDrehbar:
      UpdateGetriebeDS;
    stFest:
      begin
        if ManipulatorMode then
          UpdateGetriebeFS
        else
          BerechneWinkel;
      end;
  end;
end;

procedure TGetriebeFS.UpdateGetriebeFS;
{ FrWinkel gegeben, FrVorstag ergibt sich }
var
  svar: Boolean;
begin
  ResetStatus;
  FrPhi := FrWinkel + FrAlpha;
  Wanten3dTo2d;

  { Berechnung der Punkte A, B, P und D }
  FrPsi := PsiVonPhi(FrPhi, FrBasis, FrWunten2d, FrSalingH, FrMastunten, svar);
  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(LogList_String_InUpdateGetriebeFS);
    LogList.Add(LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter1);
    Exit;
  end;

  rP[ooA, x] := rP[ooA0, x] + FrWunten2d * cos(FrPhi - FrAlpha);
  rP[ooA, y] := FrSalingA / 2;
  rP[ooA, z] := rP[ooA0, z] + FrWunten2d * sin(FrPhi - FrAlpha);

  rP[ooB] := rP[ooA];
  rP[ooB, y] := -rP[ooA, y];

  rP[ooP] := rP[ooA];
  rP[ooP, y] := 0;

  rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(FrPsi - FrAlpha);
  rP[ooD, y] := 0;
  rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(FrPsi - FrAlpha);

  { Berechnung Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWoben2d;
    Radius2 := FrMastoben;
    MittelPunkt1 := rP[ooA];
    MittelPunkt2 := rP[ooD];
    rP[ooC] := SchnittPunkt1;
  end;

  FrVorstag := Abstand(rP[ooC0], rP[ooC]);
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest;
end;

procedure TGetriebeFS.Rest;
var
  i: TRiggPoint;
  j: TKoord;
begin
  { Berechnung Punkt ooE }
  rP[ooE, x] := rP[ooE0, x] - FrController;
  rP[ooE, y] := 0;
  rP[ooE, z] := rP[ooE0, z];

  { Berechnung Punkt ooF, ooM }
  BerechneF; { virtuelle Methode }
  BerechneM;

  { zweite Hälfte von iP füllen }
  for i := ooA to ooP do
  begin
    for j := x to z do
    begin
      iP[i, j] := rP[i, j];
    end;
  end;
end;

procedure TGetriebeFS.BerechneF;
var
  temp: double;
begin
  { Berechnung Punkt F - Masttop }
  FrEpsilon := pi / 2 - arctan2((rP[ooC, x] - rP[ooD, x]), (rP[ooC, z] - rP[ooD, z]));
  temp := FiMastL - FiMastunten;
  rP[ooF, x] := rP[ooD, x] + temp * cos(FrEpsilon);
  rP[ooF, y] := 0;
  rP[ooF, z] := rP[ooD, z] + temp * sin(FrEpsilon);
end;

procedure TGetriebeFS.BerechneM;
var
  ooTemp: TRealPoint;
  a, t: double;
begin
  a := Abstand(rp[ooF0], rp[ooF]);
  t := (a - MastfallVorlauf) / a;
  ooTemp := vsub(rp[ooF], rp[ooF0]);
  ooTemp := vadd(rp[ooF0], SkalarMult(ooTemp, t));
  rp[ooM] := ooTemp;
end;

procedure TGetriebeFS.BerechneWinkel;
{ FrVorstag gegeben, FrWinkel gesucht }
var
  Counter: Integer;
  svar: Boolean;
  VorstagIst, Diff: double;
  psiStart, psiEnde, psiA, psiB: double;
  ooTemp, ooTemp1, ooTemp2: TRealPoint;

  function VorstagLaenge(psi: double): double;
  { Viergelenk P0 P D D0, Koppelpunkt C }
  begin
    rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(psi - FrAlpha);
    rP[ooD, y] := 0;
    rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(psi - FrAlpha);

    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWunten2d;
      Radius2 := FrSalingH;
      MittelPunkt1 := rP[ooP0];
      MittelPunkt2 := rP[ooD];
      rP[ooP] := SchnittPunkt1;
    end;

    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWoben2d;
      Radius2 := FrMastoben;
      MittelPunkt1 := rP[ooP];
      MittelPunkt2 := rP[ooD];
      rP[ooC] := SchnittPunkt1;
    end;
    Result := Abstand(rP[ooC0], rP[ooC]);
  end;

begin
  ResetStatus;
  Wanten3dTo2d;

  { 1. Startwinkel ermitteln }
  { Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastunten + FrMastoben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    ooTemp := SchnittPunkt1;
  end;
  psiStart := arctan2((rP[ooD0, x] - ooTemp[x]), (ooTemp[z] - rP[ooD0, z]));
  psiStart := pi / 2 + psiStart + FrAlpha;

  { Test, ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag.
    Ermittlung der Koordinaten für diesen Fall. }
  FrPsi := psiStart;
  rP[ooC] := ooTemp;
  rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(FrPsi - FrAlpha);
  rP[ooD, y] := 0;
  rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(FrPsi - FrAlpha);
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingH;
    Radius2 := FrWoben2d;
    MittelPunkt1 := rP[ooD];
    MittelPunkt2 := rP[ooC];
    rP[ooP] := SchnittPunkt1;
  end;
  FrWanteZulang := Abstand(rP[ooP0], rP[ooP]) + Abstand(rP[ooP], rP[ooC]) -
    FrWunten2d - FrWoben2d;
  if FrWanteZulang < 0 then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end
  else
  begin
    { wenn Wante nicht zu locker dann Suche fortsetzen }

    { may jump to 'other' intersection of circle C0C and KoppelKurve }
    if WantToPlayWithExtendedSearchRange then
    begin
      { Suchbereich erweitern, wenn Durchbiegung negativ
        für psi = psiStart im VierGelenk P0 P D D0 }
      ooTemp1 := vsub(rP[ooD], rP[ooD0]);
      ooTemp2 := vsub(rP[ooC], rP[ooD]);
      ooTemp := vprod(ooTemp1, ooTemp2);
      if (ooTemp[y] > 0) then
        psiStart := psiStart + 45 * pi / 180;
    end;

    { 2. Endwinkel ermitteln - Mastoben parallel zu Vorstag
      rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastunten;
      Radius2 := FrVorstag - FrMastoben;
      MittelPunkt1 := rP[ooD0];
      MittelPunkt2 := rP[ooC0];
      ooTemp := SchnittPunkt1;
    end;
    psiEnde := arctan2((rP[ooD0, x] - ooTemp[x]), (ooTemp[z] - rP[ooD0, z]));
    psiEnde := pi / 2 + psiEnde + FrAlpha;

    { 3. Winkel ermitteln, für den gilt: VorstagIst gleich FrVorstag }
    { Viergelenk P0 P D D0, Koppelpunkt C }
    psiB := psiStart;
    psiA := psiEnde + 0.01 * pi / 180;

    Temp1 := psiA * 180 / pi;
    Temp2 := psiB * 180 / pi;

    Counter := 0;
    repeat
      Counter := Counter + 1;
      FrPsi := (psiA + psiB) / 2;
      VorstagIst := VorstagLaenge(FrPsi);
      Diff := VorstagIst - FrVorstag;
      if Diff > 0 then
        psiB := FrPsi
      else
        psiA := FrPsi;
    until (abs(Diff) < 0.01) or (Counter = 200);

    Temp3 := Counter;
  end;

  { aktualisieren }
  rP[ooA] := rP[ooP];
  rP[ooA, y] := FrSalingA / 2;
  rP[ooB] := rP[ooP];
  rP[ooB, y] := -rP[ooA, y];
  { We actually want PhiVonPsi, but we can use function PsiVonPhi;
    imagine we are looking from behind - the mechanism appears mirrored,
    angle Psi needs to be transformed back and forth,
    and member length values passed according to mirrored model. }
  FrPhi := pi - PsiVonPhi(pi - FrPsi, FrBasis, FrMastunten, FrSalingH,
    FrWunten2d, svar);
  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(LogList_String_InBerechneWinkel);
    LogList.Add(LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter2);
    Exit;
  end;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest; { think: refactored away with 'extract method refactoring' }
end;

function TGetriebeFS.Koppelkurve: TKoordLine;
{ Koppelkurve Viergelenk P0, P, D, D0 }
{ Wanten2d neu bereitgestellt,
  sonst interne Felder nicht verändert! }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: double;
  ooTemp: TRealPoint;
  oooTemp: TRealRiggPoints;
begin
  oooTemp := rP; { aktuelle Koordinaten sichern }
  Wanten3dTo2d;

  { 1. Startwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWunten2d + FrSalingH;
    Radius2 := FrMastunten;
    MittelPunkt1 := rP[ooP0];
    MittelPunkt2 := rP[ooD0];
    ooTemp := SchnittPunkt1;
  end;
  phiA := arctan2((rP[ooP0, x] - ooTemp[x]), (ooTemp[z] - rP[ooP0, z]));
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWunten2d;
    Radius2 := FrSalingH + FrMastunten;
    MittelPunkt1 := rP[ooP0];
    MittelPunkt2 := rP[ooD0];
    ooTemp := SchnittPunkt1;
  end;
  if SchnittKK.Status = bmK1inK2 then
    phiE := FrAlpha + 130 * pi / 180
  else
  begin
    phiE := arctan2((rP[ooP0, x] - ooTemp[x]), (ooTemp[z] - rP[ooP0, z]));
    phiE := phiE + pi / 2 + FrAlpha;
  end;

  { 3. Koppelkurve }
  phiA := phiA + 1 * pi / 180;
  phiE := phiE - 1 * pi / 180;
  WinkelStep := (phiE - phiA) / 100;
  phiM := phiA;
  for i := 0 to 100 do
  begin
    psiM := PsiVonPhi(phiM, FrBasis, FrWunten2d, FrSalingH, FrMastunten, svar);
    rP[ooP, x] := rP[ooP0, x] + FrWunten2d * cos(phiM - FrAlpha);
    rP[ooP, z] := rP[ooP0, z] + FrWunten2d * sin(phiM - FrAlpha);
    rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(psiM - FrAlpha);
    rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWoben2d;
      Radius2 := FrMastoben;
      MittelPunkt1 := rP[ooP];
      MittelPunkt2 := rP[ooD];
      rP[ooC] := SchnittPunkt1;
    end;
    Result[i, x] := rP[ooC, x];
    Result[i, y] := 0;
    Result[i, z] := rP[ooC, z];
    phiM := phiM + WinkelStep;
  end;

  rP := oooTemp; { aktuelle Koordinaten wiederherstellen }
end;

procedure TGetriebeFS.KorrekturF(tempH, k1, k2: double; var k3, Beta, Gamma: double);
{ Prozedur ist virtuell und wird später überschrieben,
  die genauere Mastbiegung wird dann verwendet
  um k3 und tempBeta neu zu bestimmen. }
begin
end;

procedure TGetriebeFS.MakeSalingHBiggerFS(SalingHplus: double);
{ FrSalingH größer machen, FrWoben2d, Neigung und Biegung beibehalten;
  FrWunten2d neu berechnen }
begin
  FrSalingH := SalingHplus;

  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingH; { neuer Wert }
    Radius2 := FrWoben2d;
    MittelPunkt1 := rP[ooD];
    MittelPunkt2 := rP[ooC];
    rP[ooP] := SchnittPunkt1;
  end;
  FrWunten2d := Abstand(rP[ooP], rP[ooP0]);

  { aktualisieren }
  rP[ooA] := rP[ooP];
  rP[ooA, y] := FrSalingA / 2;
  rP[ooB] := rP[ooP];
  rP[ooB, y] := -FrSalingA / 2;
  FrPhi := arctan2((rP[ooA0, x] - rP[ooA, x]), (rP[ooA, z] - rP[ooA0, z]));
  FrPhi := FrAlpha + pi / 2 + FrPhi;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  FrController := FiControllerAnschlag;
  Wanten2dTo3d;
end;

procedure TGetriebeFS.UpdateGetriebeDS;
{ gegeben: Woben3d, Wunten3d, Mastunten, Mastoben, SalingL,
  Vorstag, Rumpfkoordinaten. }
{ gesucht: Riggkoordinaten ooA, ooB, ooC, ooD, ooP, ooF }
var
  Counter: Integer;
  psiStart, psiEnde, psiEnde2, psiA, psiB: double;
  WobenMin, WobenMax, WobenIst, Diff: double;
  Saling1L, WStrich, W1Strich, Basis, Skalar: double;
  Temp, TempA, TempC, TempD: TRealPoint;

  function WobenIstVonPsi(psi: double): double;
  begin
    { Berechnungen im Vierelenk D0 D C C0 }
    { 1. Berechnung von ooD }
    rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(psi - FrAlpha);
    rP[ooD, y] := 0;
    rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(psi - FrAlpha);

    { 2. Berechnung Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastoben;
      Radius2 := FrVorstag;
      MittelPunkt1 := rP[ooD];
      MittelPunkt2 := rP[ooC0];
      rP[ooC] := SchnittPunkt1;
    end;

    WStrich := Abstand(rP[ooA0], rP[ooC]);
    Basis := Abstand(rP[ooA0], rP[ooD]);

    { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
    { Berechnung TempD }
    TempD := Null;
    TempD[x] := Basis;
    { Berechnung TempC }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := WStrich;
      Radius2 := FrMastoben;
      MittelPunkt1 := Null;
      MittelPunkt2 := TempD;
      TempC := SchnittPunkt1;
    end;
    { Berechnung TempA }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWunten3d;
      Radius2 := FrSalingL;
      MittelPunkt1 := Null;
      MittelPunkt2 := TempD;
      TempA := SchnittPunkt1;
    end;
    Result := Abstand(TempA, TempC);
  end;

begin
  ResetStatus;

  { Vorstag gegeben, Winkel numerisch ermitteln! }
  { Startwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastunten;
    Radius2 := FrVorstag - FrMastoben;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    Temp := SchnittPunkt1;
  end;
  psiStart := arctan2((rP[ooD0,x]-Temp[x]), (Temp[z]-rP[ooD0,z]));
  psiStart := psiStart + pi / 2 + FrAlpha + 0.1 * pi / 180;

  { Endwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastunten + FrMastoben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    Temp := SchnittPunkt1;
  end;
  psiEnde := arctan2((rP[ooD0,x]-Temp[x]), (Temp[z]-rP[ooD0,z]));
  psiEnde := psiEnde + pi / 2 + FrAlpha;
  psiEnde2 := psiEnde + 50 * pi / 180;

  WobenMin := WobenIstVonPsi(psiStart);
  WobenMax := WobenIstVonPsi(psiEnde);

  psiA := 0;
  psiB := 0;
  if (FrWoben3d < WobenMin) then
  begin
    psiA := psiEnde2;
    psiB := psiEnde;
    WobenMin := WobenIstVonPsi(psiEnde2);
    if (FrWoben3d < WobenMin) then
    begin
      FGetriebeOK := False;
      Include(FGetriebeStatus, gsWanteZukurz);
    end;
  end
  else if (FrWoben3d > WobenMax) then
  begin
    FrWanteZulang := FrWoben3d - WobenMax;
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end
  else
  begin
    psiA := psiStart;
    psiB := psiEnde;
  end;

  { FrPsi ermitteln }
  { Mast gerade zeichnen, wenn Wante zu lang oder zu kurz ist }
  if gsWanteZulang in FGetriebeStatus then
    FrPsi := psiEnde
  else if gsWanteZukurz in FGetriebeStatus then
  begin
    WobenIstVonPsi(psiEnde);
    FrPsi := psiEnde;
  end
  { if GetriebeOK then : den richtigen Winkel FrPsi numerisch ermitteln }
  else
  begin
    Counter := 0;
    repeat
      Counter := Counter + 1;
      FrPsi := (psiA + psiB) / 2;
      WobenIst := WobenIstVonPsi(FrPsi);
      Diff := WobenIst - FrWoben3d;
      if Diff > 0 then
        psiB := FrPsi
      else
        psiA := FrPsi;
    until
      (abs(Diff) < 0.01) or (Counter = 200);
  end;

  { weiter im ebenen Trapez }
  SchnittGG(Null, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := Abstand(Null, temp);
  Saling1L := Abstand(TempD, temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := vsub(rP[ooC], rP[ooA0]);
  temp := SkalarMult(temp, Skalar);
  temp := vadd(rP[ooA0], temp);
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := vsub(temp, rP[ooD]);
  temp := SkalarMult(temp, Skalar);
  rP[ooA] := vadd(rP[ooD], temp);

  { aktualisieren }
  rP[ooP] := rP[ooA];
  rP[ooP, y] := 0;
  rP[ooB] := rP[ooA];
  rP[ooB, y] := -rP[ooA, y];
  FrSalingA := 2 * rP[ooA, y];
  FrSalingH := Abstand(rP[ooP], rP[ooD]);
  FrPhi := arctan2((rP[ooP0, x] - rP[ooP, x]), (rP[ooP, z] - rP[ooP0, z]));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  Rest;
end;

procedure TGetriebeFS.MakeSalingLBiggerDS(SalingLplus: double);
var
  TempA, TempC, TempD, temp: TRealPoint;
  Basis, Skalar, WStrich, W1Strich, Saling1L: double;
begin
  temp := Null;

  { Punkte D, C und F schon bekannt, FrWoben3d bleibt erhalten }
  FrSalingL := SalingLplus;

  WStrich := Abstand(rP[ooA0], rP[ooC]);
  Basis := Abstand(rP[ooA0], rP[ooD]);
  { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
  { Berechnung TempD }
  TempD := Null;
  TempD[x] := Basis;
  { Berechnung TempC }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := WStrich;
    Radius2 := FrMastoben;
    MittelPunkt1 := Null;
    MittelPunkt2 := TempD;
    TempC := SchnittPunkt1; { bleibt beim Regeln unverändert }
  end;
  { Berechnung TempA }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingL; { verändert sich beim Regeln }
    Radius2 := FrWoben3d; { bleibt gleich beim Regeln }
    MittelPunkt1 := TempD;
    MittelPunkt2 := TempC;
    TempA := SchnittPunkt1; { verändert sich beim Regeln }
  end;

  SchnittGG(Null, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := Abstand(Null, temp);
  Saling1L := Abstand(TempD, temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := vsub(rP[ooC], rP[ooA0]);
  temp := SkalarMult(temp, Skalar);
  temp := vadd(rP[ooA0], temp);
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := vsub(temp, rP[ooD]);
  temp := SkalarMult(temp, Skalar);
  rP[ooA] := vadd(rP[ooD], temp);

  { FrWunten3d ermitteln und aktualisieren }
  FrWunten3d := Abstand(Null, TempA); { bzw. rP[ooA0],rP[ooA] }
  rP[ooP] := rP[ooA];
  rP[ooP, y] := 0;
  rP[ooB] := rP[ooA];
  rP[ooB, y] := -rP[ooA, y];
  FrPhi := arctan2((rP[ooA0, x] - rP[ooA, x]), (rP[ooA, z] - rP[ooA0, z]));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingA := 2 * rP[ooA, y];
  FrSalingH := Abstand(rP[ooP], rP[ooD]);
  FrController := FiControllerAnschlag;
end;

procedure TGetriebeFS.UpdateGetriebeOSS;
{ FrVorstag und FrWoben2d gegeben }
var
  temp: TRealPoint;
  Skalar: double;
begin
  ResetStatus;
  { Berechnung Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastunten + FrMastoben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    rP[ooC] := SchnittPunkt1;
  end;
  FrWunten2d := Abstand(rP[ooP0], rP[ooC]) - FrWoben2d;
  { Punkt P }
  Skalar := FrWoben2d / (FrWunten2d + FrWoben2d);
  rP[ooP, x] := rP[ooC, x] - Skalar * (rP[ooC, x] - rP[ooP0, x]);
  rP[ooP, y] := 0;
  rP[ooP, z] := rP[ooC, z] - Skalar * (rP[ooC, z] - rP[ooP0, z]);
  { Punkte A, B }
  rP[ooA] := rP[ooP];
  rP[ooA, y] := Skalar * rP[ooA0, y];
  rP[ooB] := rP[ooA];
  rP[ooB, y] := -rP[ooA, y];
  { Punkt D }
  temp := vsub(rP[ooC], rP[ooD0]);
  Skalar := FrMastunten / (FrMastunten + FrMastoben);
  temp[x] := Skalar * temp[x];
  { Temp[y] := 0; }
  temp[z] := Skalar * temp[z];
  rP[ooD] := vadd(rP[ooD0], temp);
  { aktualisieren }
  FrSalingH := Abstand(rP[ooP], rP[ooD]);
  FrSalingA := 2 * rP[ooA, y];
  FrSalingL := Abstand(rP[ooA], rP[ooD]);
  FrPhi := arctan2((rP[ooP0, x] - rP[ooP, x]), (rP[ooP, z] - rP[ooP0, z]));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := arctan2((rP[ooD0, x] - rP[ooD, x]), (rP[ooD, z] - rP[ooD0, z]));
  FrPsi := FrPsi + pi / 2 + FrAlpha;
  Wanten2dTo3d;
  Rest;
end;

procedure TGetriebeFS.UpdateGetriebeOSB;
{ FrVorstag und FrWoben3d und FrWunten3d gegeben }
var
  TempW, Skalar, TempWunten2d, TempWoben2d: double;
  temp: TRealPoint;
begin
  ResetStatus;
  { Wanten3dto2d }
  TempW := sqrt(sqr(FrWunten3d + FrWoben3d) - sqr(FrPuettingA / 2));
  Skalar := FrWunten3d / (FrWoben3d + FrWunten3d);
  TempWunten2d := TempW * Skalar;
  TempWoben2d := TempW * (1 - Skalar);
  { Berechnung Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := TempWunten2d + TempWoben2d;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooP0];
    MittelPunkt2 := rP[ooC0];
    rP[ooC] := SchnittPunkt1;
  end;

  { wenn die Wanten nicht straff sind: }
  if Abstand(rP[ooD0], rP[ooC]) > FrMastunten + FrMastoben then
  begin
    { Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastunten + FrMastoben;
      Radius2 := FrVorstag;
      MittelPunkt1 := rP[ooD0];
      MittelPunkt2 := rP[ooC0];
      rP[ooC] := SchnittPunkt1;
    end;
    { Punkt D }
    temp := vsub(rP[ooC], rP[ooD0]);
    Skalar := FrMastunten / (FrMastunten + FrMastoben);
    temp[x] := Skalar * temp[x]; { Temp[y] := 0; }
    temp[z] := Skalar * temp[z];
    rP[ooD] := vadd(rP[ooD0], temp);
    { Wantenlängen }
    (*
      FrWoben2d := TempWoben2d;
      FrWunten2d := Abstand(rP[ooP0],rP[ooC]) - FrWoben2d;
    *)
    FrWanteZulang := FrWunten3d + FrWoben3d - Abstand(rP[ooC], rP[ooA0]);
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end

  { wenn die Wanten straff sind: }
  else
  begin
    { Punkt C oben schon berechnet }
    { Punkt D }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastoben;
      Radius2 := FrMastunten;
      MittelPunkt1 := rP[ooC];
      MittelPunkt2 := rP[ooD0];
      rP[ooD] := SchnittPunkt1;
    end;
    { Wantenlängen }
    (*
      FrWunten2d := TempWunten2d;
      FrWoben2d := TempWoben2d;
    *)
  end;

  { Punkt P }
  Skalar := FrWoben2d / (FrWunten2d + FrWoben2d);
  rP[ooP, x] := rP[ooC, x] - Skalar * (rP[ooC, x] - rP[ooP0, x]);
  rP[ooP, y] := 0;
  rP[ooP, z] := rP[ooC, z] - Skalar * (rP[ooC, z] - rP[ooP0, z]);
  { Punkte A, B }
  rP[ooA] := rP[ooP];
  rP[ooA, y] := Skalar * rP[ooA0, y];
  rP[ooB] := rP[ooA];
  rP[ooB, y] := -rP[ooA, y];
  { aktualisieren }
  FrSalingH := Abstand(rP[ooP], rP[ooD]);
  FrSalingA := 2 * rP[ooA, y];
  FrSalingL := Abstand(rP[ooA], rP[ooD]);
  FrPhi := arctan2((rP[ooP0, x] - rP[ooP, x]), (rP[ooP, z] - rP[ooP0, z]));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := arctan2((rP[ooD0, x] - rP[ooD, x]), (rP[ooD, z] - rP[ooD0, z]));
  FrPsi := FrPsi + pi / 2 + FrAlpha;
  { Wanten2dTo3d; Wantenlängen3d bleiben unverändert }
  Rest;
end;

procedure TGetriebeFS.GetWantenspannung;
var
  VorstagNull: double;
begin
  VorstagNull := GetVorstagNull;
  FrVorstagDiff := VorstagNull - FrVorstag;
  if VorstagDiff < 0 then
    FrSpannungW := 0
  else
    FrSpannungW := WantenKraftvonVorstag(VorstagDiff);
  Wantenspannung := SpannungW;
end;

function TGetriebeFS.WantenKraftvonVorstag(WegSoll: double): double;
{ liefert Wantenspannung 3D in Abhängigkeit von der Auslenkung des Vorstags }
begin
  result := TrimmTab.EvalX(WegSoll);
end;

function TGetriebeFS.GetVorstagNull: double;
var
  Temp, TempP, TempD, TempC: TRealPoint;
  s: string;
  WStrich, WStrich2d: double;
begin
  result := 0;
  try
    with SchnittKK do
    begin
      SchnittEbene := seXZ;

      case SalingTyp of
        stFest:
          begin
            { 1. Aufruf SchnittKK: Saling2d und WanteOben2d;
              Schnittpunkt Temp wird im 2. Aufruf benötigt }
            Radius1 := FrSalingH;
            Radius2 := FrWoben2d;
          Temp := Null;
          Temp[x] := FrMastunten;
          MittelPunkt1 := Temp;
          { Temp := Null; }
          Temp[x] := FrMastunten + FrMastoben;
          MittelPunkt2 := Temp;
          Temp := SchnittPunkt1;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [1, s]);
            LogList.Add(s);

            { 2. Aufruf SchnittKK: TempP ermitteln }
            Radius1 := FrWunten2d;
            Radius2 := Abstand(Temp, Null); { Temp unter 1. ermittelt }
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempP := SchnittPunkt1;
            TempP[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [2, s]);
            LogList.Add(s);

            { 3. Aufruf SchnittKK: Saling2d und MastUnten; TempD ermitteln }
            Radius1 := FrSalingH;
            Radius2 := FrMastunten;
            MittelPunkt1 := TempP;
            MittelPunkt2 := rP[ooD0];
            TempD := SchnittPunkt1;
            TempD[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [3, s]);
            LogList.Add(s);

            { 4. Aufruf SchnittKK: WanteOben2d und MastOben; TempC ermitteln }
            Radius1 := FrWoben2d;
            Radius2 := FrMastoben;
            MittelPunkt1 := TempP;
            MittelPunkt2 := TempD;
            TempC := SchnittPunkt1;
            TempC[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [4, s]);
            LogList.Add(s);

            result := Abstand(rP[ooC0], TempC);
          end;

        stDrehbar:
          begin
            Radius1 := FrSalingL;
            Radius2 := FrWoben3d;
            TempD := Null;
            TempD[x] := FrMastunten;
            MittelPunkt1 := TempD;
            TempC := Null;
            TempC[x] := FrMastunten + FrMastoben;
            MittelPunkt2 := TempC;
            TempP := SchnittPunkt1;
            TempP[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [1, s]);
            LogList.Add(s);

            Radius1 := Abstand(rP[ooD0], rP[ooA0]);
            Radius2 := FrWunten3d;
            MittelPunkt1 := Null;
            MittelPunkt2 := TempP;
          Temp := SchnittPunkt1;
          Temp[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [2, s]);
            LogList.Add(s);

          WStrich := Abstand(Temp, TempC);
            WStrich2d := sqrt(sqr(WStrich) - sqr(rP[ooA0, y]));

            Radius1 := WStrich2d;
            Radius2 := FrMastunten + FrMastoben;
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempC := SchnittPunkt1;
            TempC[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [3, s]);
            LogList.Add(s);

          result := Abstand(rP[ooC0], TempC);
          end;

        stOhneStarr, stOhneBiegt:
          begin
            { 1. Aufruf SchnittKK: Wante2d und Mast; TempC ermitteln }
            Radius1 := FrWunten2d + FrWoben2d;
            Radius2 := FrMastunten + FrMastoben;
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempC := SchnittPunkt1;
            TempC[y] := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullOhne, [1, s]);
            LogList.Add(s);
            result := Abstand(rP[ooC0], TempC);
          end;
      end;
    end;

  except
    on E: EMathError do
    begin
      s := Format(LogList_Format_String_GetVorstagNullException, [E.Message]);
      LogList.Add(s);
    end;
  end;
end;

procedure TGetriebeFS.NeigeF(Mastfall: double);
var
  D0: TRealPoint;

  oldF: TRealPoint;
  oldC: TRealPoint;
  oldD: TRealPoint;

  newF: TRealPoint;
  newC: TRealPoint;
  newD: TRealPoint;

  D0F: double; // k3
  D0C: double; // k1 + k2
  D0D: double; // l4 (FrMastUnten)

  newF0F: double;

  oldPsi: double;
  newPsi: double;
  delta: double;
  w: double;
begin
  oldF := rp[ooF];
  oldC := rp[ooC];
  oldD := rp[ooD];
  D0 := rp[ooD0];
  D0F := Abstand(rp[ooD0], rp[ooF]);
  D0C := Abstand(rp[ooD0], rp[ooC]);
  D0D := FrMastUnten;

  { compute new Point F }

  newF0F := Mastfall + FiMastfallVorlauf;
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := newF0F;
    Radius2 := D0F; // unchanged
    MittelPunkt1 := rP[ooF0];
    MittelPunkt2 := rP[ooD0];
    rP[ooF] := SchnittPunkt1;
  end;

  { compute new Points C and D }

  newF := rp[ooF];
  oldPsi := Pi/2 - arctan2(oldF[x] - D0[x], oldF[z] - D0[z]);
  newPsi := Pi/2 - arctan2(newF[x] - D0[x], newF[z] - D0[z]);
  delta := newPsi - oldPsi;

  w := Pi/2 - arctan2(oldC[x] - D0[x], oldC[z] - D0[z]);
  w := w + delta;
  newC[x] := D0[x] + D0C * cos(w);
  newC[y] := 0;
  newC[z] := D0[z] + D0C * sin(w);

  w := Pi/2 - arctan2(oldD[x] - D0[x], oldD[z] - D0[z]);
  w := w + delta;
  newD[x] := D0[x] + D0D * cos(w);
  newD[y] := 0;
  newD[z] := D0[z] + D0D * sin(w);

  rp[ooC] := newC;
  rp[ooD] := newD;

  { continue as in original BiegeUndNeigeF }

  FrVorstag := Abstand(rP[ooC0], rP[ooC]);

  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TGetriebeFS.BiegeUndNeigeF1(Mastfall, Biegung: double);
var
  k1, k2, k3, k4, k5, k6, k7: double;
  tempAlpha, tempBeta, tempGamma: double;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { 1. Berechnung Länge D0F aus Durchbiegung }
  k1 := sqrt((sqr(FrMastunten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastoben) - sqr(Biegung)));
  tempAlpha := arctan2(Biegung, k1);
  k4 := (k1 + k2) * sin(tempAlpha);
  k6 := (k1 + k2) * cos(tempAlpha);
  tempGamma := arctan2(k4, (k6 - FrMastunten));
  k5 := (FrMastoben + FrMastEnde) * sin(tempGamma);
  k7 := (FrMastoben + FrMastEnde) * cos(tempGamma);
  tempBeta := arctan2(k5, (FrMastunten + k7));
  k3 := sqrt(sqr(k5) + sqr(FrMastunten + k7));
  { oder k3 := k5 / sin(tempBeta) }
  { k3 = Abstand D0F }

  { Bessere Werte für k3 und tempBeta bestimmen }
  KorrekturF(Biegung, k1, k2, k3, tempBeta, tempGamma); { virtuelle Methode }

  { 2. Berechnung Punkt F mit Mastfall }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := Mastfall + FiMastfallVorlauf;
    Radius2 := k3;
    MittelPunkt1 := rP[ooF0];
    MittelPunkt2 := rP[ooD0];
    rP[ooF] := SchnittPunkt1;
  end;

  { 3. psi, D, und C ermitteln }
  FrPsi := arctan2((rP[ooD0, x] - rP[ooF, x]), (rP[ooF, z] - rP[ooD0, z]));
  FrPsi := FrPsi + pi / 2 + FrAlpha - tempBeta;

  rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(FrPsi - FrAlpha);
  rP[ooD, y] := 0;
  rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(FrPsi - FrAlpha);

  rP[ooC, x] := rP[ooD, x] + FrMastoben * cos(FrPsi - FrAlpha + tempGamma);
  rP[ooC, y] := 0;
  rP[ooC, z] := rP[ooD, z] + FrMastoben * sin(FrPsi - FrAlpha + tempGamma);

  FrVorstag := Abstand(rP[ooC0], rP[ooC]);

  { 4. restliche Aktualisierungen vornehmen }
  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TGetriebeFS.BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: double);
{ var Parameter SalingHStart wird vom Regler benötigt }
var
  ooTemp: TRealPoint;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { 4. Startwert für FrSalingH ermitteln }
  ooTemp := EVektor(rP[ooC], rP[ooP0]);
  ooTemp := SkalarMult(ooTemp, FrWoben2d);
  rP[ooP] := vadd(rP[ooC], ooTemp);
  SalingHStart := Abstand(rP[ooP], rP[ooD]);
  FrSalingH := Trunc(SalingHStart) + 1; { FiSalingH garantiert größer }

  { 5. restliche Aktualisierungen in MakeSalingHBiggerFS vornehmen! }
  MakeSalingHBiggerFS(SalingHStart);
end;

procedure TGetriebeFS.BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: double);
{ var Parameter SalingLStart wird vom Regler benötigt }
var
  ooTemp: TRealPoint;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { Startwert für SalingL ermitteln }
  ooTemp := EVektor(rP[ooC], rP[ooA0]);
  ooTemp := SkalarMult(ooTemp, FrWoben3d);
  rP[ooA] := vadd(rP[ooC], ooTemp);
  SalingLStart := Abstand(rP[ooA], rP[ooD]);
  FrSalingL := Trunc(SalingLStart) + 1; { FiSalingL dann garantiert größer! }

  { restliche Aktualisierungen in MakeSalingLBiggerDS vornehmen }
  MakeSalingLBiggerDS(SalingLStart);
end;

procedure TGetriebeFS.BiegeUndNeigeC(MastfallC, Biegung: double);
var
  k1, k2: double;
  tempAlpha: double;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { Zweischlag ausgehend von Durchbiegung }
  k1 := sqrt((sqr(FrMastunten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastoben) - sqr(Biegung)));
  tempAlpha := arctan2(Biegung, k1);

  { Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := MastfallC;
    Radius2 := k1 + k2;
    MittelPunkt1 := rP[ooF0];
    MittelPunkt2 := rP[ooD0];
    rP[ooC] := SchnittPunkt1;
  end;

  { psi und Punkt D }
  FrPsi := arctan2((rP[ooD0, x] - rP[ooC, x]), (rP[ooC, z] - rP[ooD0, z]));
  FrPsi := FrPsi + pi / 2 + FrAlpha - tempAlpha;

  rP[ooD, x] := rP[ooD0, x] + FrMastunten * cos(FrPsi - FrAlpha);
  rP[ooD, y] := 0;
  rP[ooD, z] := rP[ooD0, z] + FrMastunten * sin(FrPsi - FrAlpha);

  { Vorstag }
  FrVorstag := Abstand(rP[ooC0], rP[ooC]);

  { Punkt F, M }
  BerechneF;
  BerechneM;

  { restliche Aktualisierungen }
  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;
end;

end.
