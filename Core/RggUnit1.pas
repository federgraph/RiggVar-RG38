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
  System.Math.Vectors,
  RggStrings,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggUnit0;

type
  TGetriebeFS = class(TGetriebe)
  private
    FrVorstagDiff: single;
    FrSpannungW: single;
  protected
    procedure BerechneF; virtual;
    procedure KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single); virtual; //deprecated;
  public
    UpdateGetriebeCounter: Integer;

    ExitCounter1: Integer;
    ExitCounter2: Integer;
    ExitCounter3: Integer;
    ExitCounter4: Integer;
    ExitCounter5: Integer;
    ExitCounter6: Integer;
    ExitCounter7: Integer;

    Temp1: single;
    Temp2: single;
    Temp3: single;
    Temp4: single;

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
    procedure BiegeUndNeigeF1(Mastfall, Biegung: single);
    procedure NeigeF(Mastfall: single);
    procedure BiegeUndNeigeC(MastfallC, Biegung: single);
    procedure BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: single);
    procedure BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: single);
    procedure MakeSalingHBiggerFS(SalingHplus: single);
    procedure MakeSalingLBiggerDS(SalingLplus: single);
    procedure GetWantenspannung;
    function WantenKraftvonVorstag(WegSoll: single): single;
    function GetVorstagNull: single;

    property VorstagDiff: single read FrVorstagDiff;
    property SpannungW: single read FrSpannungW;
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
  FrPsi := PsiVonPhi(FrPhi, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
  if FrPsi < 0 then
    FrPsi := FrPsi + 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(LogList_String_InUpdateGetriebeFS);
    LogList.Add(LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter1);
    Exit;
  end;

  rP[ooA].X := rP[ooA0].X + FrWunten2D * cos(FrPhi - FrAlpha);
  rP[ooA].Y := -FrSalingA / 2;
  rP[ooA].Z := rP[ooA0].Z + FrWunten2D * sin(FrPhi - FrAlpha);

  rP[ooB] := rP[ooA];
  rP[ooB].Y := -rP[ooA].Y;

  rP[ooP] := rP[ooA];
  rP[ooP].Y := 0;

  rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(FrPsi - FrAlpha);
  rP[ooD].Y := 0;
  rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(FrPsi - FrAlpha);

  { Berechnung Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWoben2D;
    Radius2 := FrMastOben;
    MittelPunkt1 := rP[ooA];
    MittelPunkt2 := rP[ooD];
    rP[ooC] := SchnittPunkt1;
  end;

  FrVorstag := (rP[ooC0] - rP[ooC]).Length;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest;
end;

procedure TGetriebeFS.Rest;
//var
//  i: TRiggPoint;
//  j: TKoord;
begin
  { Berechnung Punkt ooE }
  rP[ooE].X := rP[ooE0].X - FrController;
  rP[ooE].Y := 0;
  rP[ooE].Z := rP[ooE0].Z;

  { Berechnung Punkt ooF, ooM }
  BerechneF; { virtuelle Methode }
  BerechneM;

  { zweite Hälfte von iP füllen }
//  for i := ooA to ooP do
//  begin
//    for j := x to z do
//    begin
//      iP[i, j] := rP[i, j];
//    end;
//  end;
end;

procedure TGetriebeFS.BerechneF;
var
  temp: single;
begin
  { Berechnung Punkt F - Masttop }
  FrEpsilon := pi / 2 - arctan2((rP[ooC].X - rP[ooD].X), (rP[ooC].Z - rP[ooD].Z));
  temp := FrMastLength - FrMastUnten;
  rP[ooF].X := rP[ooD].X + temp * cos(FrEpsilon);
  rP[ooF].Y := 0;
  rP[ooF].Z := rP[ooD].Z + temp * sin(FrEpsilon);
end;

procedure TGetriebeFS.BerechneM;
var
  ooTemp: TPoint3D;
  a, t: single;
begin
  a := (rp[ooF0] - rp[ooF]).Length;
  t := (a - MastfallVorlauf) / a;
  ooTemp := rp[ooF] - rp[ooF0];
  ooTemp := rp[ooF0] + ooTemp * t;
  rp[ooM] := ooTemp;
end;

procedure TGetriebeFS.BerechneWinkel;
{ FrVorstag gegeben, FrWinkel gesucht }
var
  Counter: Integer;
  svar: Boolean;
  VorstagIst, Diff: single;
  psiStart, psiEnde, psiA, psiB: single;
  ooTemp, ooTemp1, ooTemp2: TPoint3D;

  function VorstagLaenge(psi: single): single;
  { Viergelenk P0 P D D0, Koppelpunkt C }
  begin
    rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(psi - FrAlpha);
    rP[ooD].Y := 0;
    rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(psi - FrAlpha);

    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWunten2D;
      Radius2 := FrSalingH;
      MittelPunkt1 := rP[ooP0];
      MittelPunkt2 := rP[ooD];
      rP[ooP] := SchnittPunkt1;
    end;

    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWoben2D;
      Radius2 := FrMastOben;
      MittelPunkt1 := rP[ooP];
      MittelPunkt2 := rP[ooD];
      rP[ooC] := SchnittPunkt1;
    end;
    Result := (rP[ooC0] - rP[ooC]).Length;
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
    Radius1 := FrMastUnten + FrMastOben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    ooTemp := SchnittPunkt1;
  end;
  psiStart := arctan2((rP[ooD0].X - ooTemp.X), (ooTemp.Z - rP[ooD0].Z));
  psiStart := pi / 2 + psiStart + FrAlpha;

  { Test, ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag.
    Ermittlung der Koordinaten für diesen Fall. }
  FrPsi := psiStart;
  rP[ooC] := ooTemp;
  rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(FrPsi - FrAlpha);
  rP[ooD].Y := 0;
  rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(FrPsi - FrAlpha);
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingH;
    Radius2 := FrWoben2D;
    MittelPunkt1 := rP[ooD];
    MittelPunkt2 := rP[ooC];
    rP[ooP] := SchnittPunkt1;
  end;
  FrWanteZulang := (rP[ooP0] - rP[ooP]).Length + (rP[ooP] - rP[ooC]).Length -
    FrWunten2D - FrWoben2D;
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
      ooTemp1 := rP[ooD] - rP[ooD0];
      ooTemp2 := rP[ooC] - rP[ooD];
      ooTemp := ooTemp1.CrossProduct(ooTemp2);
      if (ooTemp.Y > 0) then
        psiStart := psiStart + 45 * pi / 180;
    end;

    { 2. Endwinkel ermitteln - Mastoben parallel zu Vorstag
      rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastUnten;
      Radius2 := FrVorstag - FrMastOben;
      MittelPunkt1 := rP[ooD0];
      MittelPunkt2 := rP[ooC0];
      ooTemp := SchnittPunkt1;
    end;
    psiEnde := arctan2((rP[ooD0].X - ooTemp.X), (ooTemp.Z - rP[ooD0].Z));
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
  rP[ooA].Y := -FrSalingA / 2;
  rP[ooB] := rP[ooP];
  rP[ooB].Y := -rP[ooA].Y;
  { We actually want PhiVonPsi, but we can use function PsiVonPhi;
    imagine we are looking from behind - the mechanism appears mirrored,
    angle Psi needs to be transformed back and forth,
    and member length values passed according to mirrored model. }
  FrPhi := pi - PsiVonPhi(pi - FrPsi, FrBasis, FrMastUnten, FrSalingH, FrWunten2D, svar);
  if FrPhi > 2 * PI then
    FrPhi := FrPhi - 2 * PI;

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
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;
  oooTemp: TRealRiggPoints;
begin
  oooTemp := rP; { aktuelle Koordinaten sichern }
  Wanten3dTo2d;

  { 1. Startwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWunten2D + FrSalingH;
    Radius2 := FrMastUnten;
    MittelPunkt1 := rP[ooP0];
    MittelPunkt2 := rP[ooD0];
    ooTemp := SchnittPunkt1;
  end;
  phiA := arctan2((rP[ooP0].X - ooTemp.X), (ooTemp.Z - rP[ooP0].Z));
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrWunten2D;
    Radius2 := FrSalingH + FrMastUnten;
    MittelPunkt1 := rP[ooP0];
    MittelPunkt2 := rP[ooD0];
    ooTemp := SchnittPunkt1;
  end;
  if SchnittKK.Status = bmK1inK2 then
    phiE := FrAlpha + 130 * pi / 180
  else
  begin
    phiE := arctan2((rP[ooP0].X - ooTemp.X), (ooTemp.Z - rP[ooP0].Z));
    phiE := phiE + pi / 2 + FrAlpha;
  end;

  { 3. Koppelkurve }
  phiA := phiA + 1 * pi / 180;
  phiE := phiE - 1 * pi / 180;
  WinkelStep := (phiE - phiA) / 100;
  phiM := phiA;
  for i := 0 to 100 do
  begin
    psiM := PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP[ooP].X := rP[ooP0].X + FrWunten2D * cos(phiM - FrAlpha);
    rP[ooP].Z := rP[ooP0].Z + FrWunten2D * sin(phiM - FrAlpha);
    rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(psiM - FrAlpha);
    rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWoben2D;
      Radius2 := FrMastOben;
      MittelPunkt1 := rP[ooP];
      MittelPunkt2 := rP[ooD];
      rP[ooC] := SchnittPunkt1;
    end;
    Result[i].X := rP[ooC].X;
    Result[i].Y := 0;
    Result[i].Z := rP[ooC].Z;
    phiM := phiM + WinkelStep;
  end;

  rP := oooTemp; { aktuelle Koordinaten wiederherstellen }
end;

procedure TGetriebeFS.KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single);
{ Prozedur ist virtuell und wird später überschrieben,
  die genauere Mastbiegung wird dann verwendet
  um k3 und tempBeta neu zu bestimmen. }
begin
end;

procedure TGetriebeFS.MakeSalingHBiggerFS(SalingHplus: single);
{ FrSalingH größer machen, FrWoben2d, Neigung und Biegung beibehalten;
  FrWunten2d neu berechnen }
begin
  FrSalingH := SalingHplus;

  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingH; { neuer Wert }
    Radius2 := FrWoben2D;
    MittelPunkt1 := rP[ooD];
    MittelPunkt2 := rP[ooC];
    rP[ooP] := SchnittPunkt1;
  end;
  FrWunten2D := (rP[ooP] - rP[ooP0]).Length;

  { aktualisieren }
  rP[ooA] := rP[ooP];
  rP[ooA].Y := -FrSalingA / 2;
  rP[ooB] := rP[ooP];
  rP[ooB].Y := -FrSalingA / 2;
  FrPhi := arctan2((rP[ooA0].X - rP[ooA].X), (rP[ooA].Z - rP[ooA0].Z));
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
  psiStart, psiEnde, psiEnde2, psiA, psiB: single;
  WobenMin, WobenMax, WobenIst, Diff: single;
  Saling1L, WStrich, W1Strich, Basis, Skalar: single;
  Temp, TempA, TempC, TempD: TPoint3D;

  function WobenIstVonPsi(psi: single): single;
  begin
    { Berechnungen im Vierelenk D0 D C C0 }
    { 1. Berechnung von ooD }
    rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(psi - FrAlpha);
    rP[ooD].Y := 0;
    rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(psi - FrAlpha);

    { 2. Berechnung Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastOben;
      Radius2 := FrVorstag;
      MittelPunkt1 := rP[ooD];
      MittelPunkt2 := rP[ooC0];
      rP[ooC] := SchnittPunkt1;
    end;

    WStrich := (rP[ooA0] - rP[ooC]).Length;
    Basis := (rP[ooA0] - rP[ooD]).Length;

    { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
    { Berechnung TempD }
    TempD := TPoint3D.Zero;
    TempD.X := Basis;
    { Berechnung TempC }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := WStrich;
      Radius2 := FrMastOben;
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := TempD;
      TempC := SchnittPunkt1;
    end;
    { Berechnung TempA }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrWunten3D;
      Radius2 := FrSalingL;
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := TempD;
      TempA := SchnittPunkt1;
    end;
    Result := (TempA - TempC).Length;
  end;

begin
  ResetStatus;

  { Vorstag gegeben, Winkel numerisch ermitteln! }
  { Startwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastUnten;
    Radius2 := FrVorstag - FrMastOben;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    Temp := SchnittPunkt1;
  end;
  psiStart := arctan2((rP[ooD0].X - Temp.X), (Temp.Z - rP[ooD0].Z));
  psiStart := psiStart + pi / 2 + FrAlpha + 0.1 * pi / 180;

  { Endwinkel }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastUnten + FrMastOben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    Temp := SchnittPunkt1;
  end;
  psiEnde := arctan2((rP[ooD0].X - Temp.X), (Temp.Z - rP[ooD0].Z));
  psiEnde := psiEnde + pi / 2 + FrAlpha;
  psiEnde2 := psiEnde + 50 * pi / 180;

  WobenMin := WobenIstVonPsi(psiStart);
  WobenMax := WobenIstVonPsi(psiEnde);

  psiA := 0;
  psiB := 0;
  if (FrWoben3D < WobenMin) then
  begin
    psiA := psiEnde2;
    psiB := psiEnde;
    WobenMin := WobenIstVonPsi(psiEnde2);
    if (FrWoben3D < WobenMin) then
    begin
      FGetriebeOK := False;
      Include(FGetriebeStatus, gsWanteZukurz);
    end;
  end
  else if (FrWoben3D > WobenMax) then
  begin
    FrWanteZulang := FrWoben3D - WobenMax;
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
      Diff := WobenIst - FrWoben3D;
      if Diff > 0 then
        psiB := FrPsi
      else
        psiA := FrPsi;
    until
      (abs(Diff) < 0.01) or (Counter = 200);
  end;

  { weiter im ebenen Trapez }
  SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := (TempD - temp).Length;

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP[ooC] - rP[ooA0];
  temp := temp * Skalar;
  temp := rP[ooA0] + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP[ooD];
  temp := temp * Skalar;
  rP[ooA] := rP[ooD] + temp;

  { aktualisieren }
  rP[ooP] := rP[ooA];
  rP[ooP].Y := 0;
  rP[ooB] := rP[ooA];
  rP[ooB].Y := -rP[ooA].Y;
  FrSalingA := 2 * rP[ooB].Y;
  FrSalingH := (rP[ooP] - rP[ooD]).Length;
  FrPhi := arctan2((rP[ooP0].X - rP[ooP].X), (rP[ooP].Z - rP[ooP0].Z));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  Rest;
end;

procedure TGetriebeFS.MakeSalingLBiggerDS(SalingLplus: single);
var
  TempA, TempC, TempD, temp: TPoint3D;
  Basis, Skalar, WStrich, W1Strich, Saling1L: single;
begin
  temp := TPoint3D.Zero;

  { Punkte D, C und F schon bekannt, FrWoben3d bleibt erhalten }
  FrSalingL := SalingLplus;

  WStrich := (rP[ooA0] - rP[ooC]).Length;
  Basis := (rP[ooA0] - rP[ooD]).Length;
  { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
  { Berechnung TempD }
  TempD := TPoint3D.Zero;
  TempD.X := Basis;
  { Berechnung TempC }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := WStrich;
    Radius2 := FrMastOben;
    MittelPunkt1 := TPoint3D.Zero;
    MittelPunkt2 := TempD;
    TempC := SchnittPunkt1; { bleibt beim Regeln unverändert }
  end;
  { Berechnung TempA }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingL; { verändert sich beim Regeln }
    Radius2 := FrWoben3D; { bleibt gleich beim Regeln }
    MittelPunkt1 := TempD;
    MittelPunkt2 := TempC;
    TempA := SchnittPunkt1; { verändert sich beim Regeln }
  end;

  SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := (TempD - temp).Length;

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP[ooC] - rP[ooA0];
  temp := temp * Skalar;
  temp := rP[ooA0] + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP[ooD];
  temp := temp * Skalar;
  rP[ooA] := rP[ooD] + temp;

  { FrWunten3d ermitteln und aktualisieren }
  FrWunten3D := TempA.Length; { bzw. (rP[ooA0] - rP[ooA]).Length }
  rP[ooP] := rP[ooA];
  rP[ooP].Y := 0;
  rP[ooB] := rP[ooA];
  rP[ooB].Y := -rP[ooA].Y;
  FrPhi := arctan2((rP[ooA0].X - rP[ooA].X), (rP[ooA].Z - rP[ooA0].Z));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingA := 2 * rP[ooB].Y;
  FrSalingH := (rP[ooP] - rP[ooD]).Length;
  FrController := FiControllerAnschlag;
end;

procedure TGetriebeFS.UpdateGetriebeOSS;
{ FrVorstag und FrWoben2d gegeben }
var
  temp: TPoint3D;
  Skalar: single;
begin
  ResetStatus;
  { Berechnung Punkt C }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastUnten + FrMastOben;
    Radius2 := FrVorstag;
    MittelPunkt1 := rP[ooD0];
    MittelPunkt2 := rP[ooC0];
    rP[ooC] := SchnittPunkt1;
  end;
  FrWunten2D := (rP[ooP0] - rP[ooC]).Length - FrWoben2D;
  { Punkt P }
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP[ooP].X := rP[ooC].X - Skalar * (rP[ooC].X - rP[ooP0].X);
  rP[ooP].Y := 0;
  rP[ooP].Z := rP[ooC].Z - Skalar * (rP[ooC].Z - rP[ooP0].Z);
  { Punkte A, B }
  rP[ooA] := rP[ooP];
  rP[ooA].Y := Skalar * rP[ooA0].Y;
  rP[ooB] := rP[ooA];
  rP[ooB].Y := -rP[ooA].Y;
  { Punkt D }
  temp := rP[ooC] - rP[ooD0];
  Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
  temp.X := Skalar * temp.X;
  { Temp[y] := 0; }
  temp.Z := Skalar * temp.Z;
  rP[ooD] := rP[ooD0] + temp;
  { aktualisieren }
  FrSalingH := (rP[ooP] - rP[ooD]).Length;
  FrSalingA := 2 * rP[ooB].Y;
  FrSalingL := (rP[ooA] - rP[ooD]).Length;
  FrPhi := arctan2((rP[ooP0].X - rP[ooP].X), (rP[ooP].Z - rP[ooP0].Z));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := arctan2((rP[ooD0].X - rP[ooD].X), (rP[ooD].Z - rP[ooD0].Z));
  FrPsi := FrPsi + pi / 2 + FrAlpha;
  Wanten2dTo3d;
  Rest;
end;

procedure TGetriebeFS.UpdateGetriebeOSB;
{ FrVorstag und FrWoben3d und FrWunten3d gegeben }
var
  TempW, Skalar, TempWunten2d, TempWoben2d: single;
  temp: TPoint3D;
begin
  ResetStatus;
  { Wanten3dto2d }
  TempW := sqrt(sqr(FrWunten3D + FrWoben3D) - sqr(FrPuettingA / 2));
  Skalar := FrWunten3D / (FrWoben3D + FrWunten3D);
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
  if (rP[ooD0] - rP[ooC]).Length > FrMastUnten + FrMastOben then
  begin
    { Punkt C }
    with SchnittKK do
    begin
      SchnittEbene := seXZ;
      Radius1 := FrMastUnten + FrMastOben;
      Radius2 := FrVorstag;
      MittelPunkt1 := rP[ooD0];
      MittelPunkt2 := rP[ooC0];
      rP[ooC] := SchnittPunkt1;
    end;
    { Punkt D }
    temp := rP[ooC]- rP[ooD0];
    Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
    temp.X := Skalar * temp.X;
  { Temp.Y := 0; }
    temp.Z := Skalar * temp.Z;
    rP[ooD] := rP[ooD0] + temp;
    { Wantenlängen }
    (*
      FrWoben2d := TempWoben2d;
      FrWunten2d := (rP[ooP0] - rP[ooC]).Length - FrWoben2d;
    *)
    FrWanteZulang := FrWunten3D + FrWoben3D - (rP[ooC] - rP[ooA0]).Length;
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
      Radius1 := FrMastOben;
      Radius2 := FrMastUnten;
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
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP[ooP].X := rP[ooC].X - Skalar * (rP[ooC].X - rP[ooP0].X);
  rP[ooP].Y := 0;
  rP[ooP].Z := rP[ooC].Z - Skalar * (rP[ooC].Z - rP[ooP0].Z);
  { Punkte A, B }
  rP[ooA] := rP[ooP];
  rP[ooA].Y := Skalar * rP[ooA0].Y;
  rP[ooB] := rP[ooA];
  rP[ooB].Y := -rP[ooA].Y;
  { aktualisieren }
  FrSalingH := (rP[ooP] - rP[ooD]).Length;
  FrSalingA := 2 * rP[ooB].Y;
  FrSalingL := (rP[ooA] - rP[ooD]).Length;
  FrPhi := arctan2((rP[ooP0].X - rP[ooP].X), (rP[ooP].Z - rP[ooP0].Z));
  FrPhi := FrPhi + pi / 2 + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := arctan2((rP[ooD0].X - rP[ooD].X), (rP[ooD].Z - rP[ooD0].Z));
  FrPsi := FrPsi + pi / 2 + FrAlpha;
  { Wanten2dTo3d; Wantenlängen3d bleiben unverändert }
  Rest;
end;

procedure TGetriebeFS.GetWantenspannung;
var
  VorstagNull: single;
begin
  VorstagNull := GetVorstagNull;
  FrVorstagDiff := VorstagNull - FrVorstag;
  if VorstagDiff < 0 then
    FrSpannungW := 0
  else
    FrSpannungW := WantenKraftvonVorstag(VorstagDiff);
  Wantenspannung := SpannungW;
end;

function TGetriebeFS.WantenKraftvonVorstag(WegSoll: single): single;
{ liefert Wantenspannung 3D in Abhängigkeit von der Auslenkung des Vorstags }
begin
  result := TrimmTab.EvalX(WegSoll);
end;

function TGetriebeFS.GetVorstagNull: single;
var
  Temp, TempP, TempD, TempC: TPoint3D;
  s: string;
  WStrich, WStrich2d: single;
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
            Radius2 := FrWoben2D;
            Temp := TPoint3D.Zero;
            Temp.X := FrMastUnten;
            MittelPunkt1 := Temp;
            { Temp := Null; }
            Temp.X := FrMastUnten + FrMastOben;
            MittelPunkt2 := Temp;
            Temp := SchnittPunkt1;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [1, s]);
            LogList.Add(s);

            { 2. Aufruf SchnittKK: TempP ermitteln }
            Radius1 := FrWunten2D;
            Radius2 := Temp.Length; { Temp unter 1. ermittelt }
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempP := SchnittPunkt1;
            TempP.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [2, s]);
            LogList.Add(s);

            { 3. Aufruf SchnittKK: Saling2d und MastUnten; TempD ermitteln }
            Radius1 := FrSalingH;
            Radius2 := FrMastUnten;
            MittelPunkt1 := TempP;
            MittelPunkt2 := rP[ooD0];
            TempD := SchnittPunkt1;
            TempD.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [3, s]);
            LogList.Add(s);

            { 4. Aufruf SchnittKK: WanteOben2d und MastOben; TempC ermitteln }
            Radius1 := FrWoben2D;
            Radius2 := FrMastOben;
            MittelPunkt1 := TempP;
            MittelPunkt2 := TempD;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [4, s]);
            LogList.Add(s);

            result := (rP[ooC0] - TempC).Length;
          end;

        stDrehbar:
          begin
            Radius1 := FrSalingL;
            Radius2 := FrWoben3D;
            TempD := TPoint3D.Zero;
            TempD.X := FrMastUnten;
            MittelPunkt1 := TempD;
            TempC := TPoint3D.Zero;
            TempC.X := FrMastUnten + FrMastOben;
            MittelPunkt2 := TempC;
            TempP := SchnittPunkt1;
            TempP.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [1, s]);
            LogList.Add(s);

            Radius1 := (rP[ooD0] - rP[ooA0]).Length;
            Radius2 := FrWunten3D;
            MittelPunkt1 := TPoint3D.Zero;
            MittelPunkt2 := TempP;
            Temp := SchnittPunkt1;
            Temp.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [2, s]);
            LogList.Add(s);

            WStrich := (Temp - TempC).Length;
            WStrich2d := sqrt(sqr(WStrich) - sqr(rP[ooA0].Y));

            Radius1 := WStrich2d;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [3, s]);
            LogList.Add(s);

            result := (rP[ooC0] - TempC).Length;
          end;

        stOhneStarr, stOhneBiegt:
          begin
            { 1. Aufruf SchnittKK: Wante2d und Mast; TempC ermitteln }
            Radius1 := FrWunten2D + FrWoben2D;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP[ooP0];
            MittelPunkt2 := rP[ooD0];
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullOhne, [1, s]);
            LogList.Add(s);
            result := (rP[ooC0] - TempC).Length;
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

procedure TGetriebeFS.NeigeF(Mastfall: single);
var
  D0: TPoint3D;

  oldF: TPoint3D;
  oldC: TPoint3D;
  oldD: TPoint3D;

  newF: TPoint3D;
  newC: TPoint3D;
  newD: TPoint3D;

  D0F: single; // k3
  D0C: single; // k1 + k2
  D0D: single; // l4 (FrMastUnten)

  newF0F: single;

  oldPsi: single;
  newPsi: single;
  delta: single;
  w: single;
begin
  oldF := rp[ooF];
  oldC := rp[ooC];
  oldD := rp[ooD];
  D0 := rp[ooD0];
  D0F := (rp[ooD0] - rp[ooF]).Length;
  D0C := (rp[ooD0] - rp[ooC]).Length;
  D0D := FrMastUnten;

  { compute new Point F }

  newF0F := Mastfall + FrMastfallVorlauf;
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
  oldPsi := Pi/2 - arctan2(oldF.X - D0.X, oldF.Z - D0.Z);
  newPsi := Pi/2 - arctan2(newF.X - D0.X, newF.Z - D0.Z);
  delta := newPsi - oldPsi;

  w := Pi/2 - arctan2(oldC.X - D0.X, oldC.Z - D0.Z);
  w := w + delta;
  newC.X := D0.X + D0C * cos(w);
  newC.Y := 0;
  newC.Z := D0.Z + D0C * sin(w);

  w := Pi/2 - arctan2(oldD.X - D0.X, oldD.Z - D0.Z);
  w := w + delta;
  newD.X := D0.X + D0D * cos(w);
  newD.Y := 0;
  newD.Z := D0.Z + D0D * sin(w);

  rp[ooC] := newC;
  rp[ooD] := newD;

  { continue as in original BiegeUndNeigeF }

  FrVorstag := (rP[ooC0] - rP[ooC]).Length;

  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TGetriebeFS.BiegeUndNeigeF1(Mastfall, Biegung: single);
var
  k1, k2, k3, k4, k5, k6, k7: single;
  tempAlpha, tempBeta, tempGamma: single;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { 1. Berechnung Länge D0F aus Durchbiegung }
  k1 := sqrt((sqr(FrMastUnten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastOben) - sqr(Biegung)));
  tempAlpha := arctan2(Biegung, k1);
  k4 := (k1 + k2) * sin(tempAlpha);
  k6 := (k1 + k2) * cos(tempAlpha);
  tempGamma := arctan2(k4, (k6 - FrMastUnten));
  k5 := (FrMastOben + FrMastEnde) * sin(tempGamma);
  k7 := (FrMastOben + FrMastEnde) * cos(tempGamma);
  tempBeta := arctan2(k5, (FrMastUnten + k7));
  k3 := sqrt(sqr(k5) + sqr(FrMastUnten + k7));
  { oder k3 := k5 / sin(tempBeta) }
  { k3 = Abstand D0F }

  { Bessere Werte für k3 und tempBeta bestimmen }
  KorrekturF(Biegung, k1, k2, k3, tempBeta, tempGamma); { virtuelle Methode }

  { 2. Berechnung Punkt F mit Mastfall }
  with SchnittKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := Mastfall + FrMastfallVorlauf;
    Radius2 := k3;
    MittelPunkt1 := rP[ooF0];
    MittelPunkt2 := rP[ooD0];
    rP[ooF] := SchnittPunkt1;
  end;

  { 3. psi, D, und C ermitteln }
  FrPsi := arctan2((rP[ooD0].X - rP[ooF].X), (rP[ooF].Z - rP[ooD0].Z));
  FrPsi := FrPsi + pi / 2 + FrAlpha - tempBeta;

  rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(FrPsi - FrAlpha);
  rP[ooD].Y := 0;
  rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(FrPsi - FrAlpha);

  rP[ooC].X := rP[ooD].X + FrMastOben * cos(FrPsi - FrAlpha + tempGamma);
  rP[ooC].Y := 0;
  rP[ooC].Z := rP[ooD].Z + FrMastOben * sin(FrPsi - FrAlpha + tempGamma);

  FrVorstag := (rP[ooC0] - rP[ooC]).Length;

  { 4. restliche Aktualisierungen vornehmen }
  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TGetriebeFS.BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: single);
{ var Parameter SalingHStart wird vom Regler benötigt }
var
  ooTemp: TPoint3D;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { 4. Startwert für FrSalingH ermitteln }
  ooTemp := (rP[ooC] - rP[ooP0]).Normalize;
  ooTemp := ooTemp * FrWoben2D;
  rP[ooP] := rP[ooC] + ooTemp;
  SalingHStart := (rP[ooP] - rP[ooD]).Length;
  FrSalingH := Trunc(SalingHStart) + 1; { FiSalingH garantiert größer }

  { 5. restliche Aktualisierungen in MakeSalingHBiggerFS vornehmen! }
  MakeSalingHBiggerFS(SalingHStart);
end;

procedure TGetriebeFS.BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: single);
{ var Parameter SalingLStart wird vom Regler benötigt }
var
  ooTemp: TPoint3D;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { Startwert für SalingL ermitteln }
  ooTemp := (rP[ooC] - rP[ooA0]).Normalize;
  ooTemp := ooTemp * FrWoben3D;
  rP[ooA] := rP[ooC] + ooTemp;
  SalingLStart := (rP[ooA] - rP[ooD]).Length;
  FrSalingL := Trunc(SalingLStart) + 1; { FiSalingL dann garantiert größer! }

  { restliche Aktualisierungen in MakeSalingLBiggerDS vornehmen }
  MakeSalingLBiggerDS(SalingLStart);
end;

procedure TGetriebeFS.BiegeUndNeigeC(MastfallC, Biegung: single);
var
  k1, k2: single;
  tempAlpha: single;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { Zweischlag ausgehend von Durchbiegung }
  k1 := sqrt((sqr(FrMastUnten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastOben) - sqr(Biegung)));
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
  FrPsi := arctan2((rP[ooD0].X - rP[ooC].X), (rP[ooC].Z - rP[ooD0].Z));
  FrPsi := FrPsi + pi / 2 + FrAlpha - tempAlpha;

  rP[ooD].X := rP[ooD0].X + FrMastUnten * cos(FrPsi - FrAlpha);
  rP[ooD].Y := 0;
  rP[ooD].Z := rP[ooD0].Z + FrMastUnten * sin(FrPsi - FrAlpha);

  { Vorstag }
  FrVorstag := (rP[ooC0] - rP[ooC]).Length;

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
