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
  RiggVar.App.Strings,
  RiggVar.RG.Types,
  RiggVar.RG.Calc,
  RggUnit0;

type
  TGetriebeFS = class(TGetriebe)
  protected
    FrVorstagDiff: single;
    FrSpannungW: single;
    psiStart: single;
    psiEnde: single;
    tempD: TPoint3D;
    tempC: TPoint3D;
    tempP: TPoint3D;
    LoopCounterS: Integer;
    LoopCounterE: Integer;
    LoopCounterT: Integer;
    Msg: string;
    HasError: Boolean;
    procedure UpdateGetriebeTemp(anglePsi: single);
    procedure ComputeStartAngle1;
    procedure ComputeStartAngle2;
    procedure ComputeEndAngle1;
    procedure ComputeEndAngle2;
    procedure LoopForPsi;
    function GetVorstagLaenge(psi: single): single;
    procedure ComputeWinkel;
    procedure TestWante;
    procedure BerechneWinkel1;
    procedure BerechneWinkel2;
    procedure RecordInitialPosition;
    procedure ComputeInitialPosition(angle: single);
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

    function GetCounterValue(Idx: Integer): Integer;
    function GetTempValue(Idx: Integer): single;

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
    function GetKoppelKurve: TKoordLine;
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
    property KoppelKurve: TKoordLine read GetKoppelKurve;
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
  FrPsi := TRggCalc.PsiVonPhi(FrPhi, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
  if FrPsi < 0 then
    FrPsi := FrPsi + 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(RggStrings.LogList_String_InUpdateGetriebeFS);
    LogList.Add(RggStrings.LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter1);
    Exit;
  end;

  rP.A := SKK.AnglePointXZ(rP.A0, FrWunten2D, FrPhi - FrAlpha);
  rP.A.Y := -FrSalingA / 2;

  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;

  rP.P := rP.A;
  rP.P.Y := 0;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);

  rP.C := SKK.IntersectionXZ1(rP.A, rP.D, FrWoben2D, FrMastOben);

  FrVorstag := rP.C0.Distance(rP.C);
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest;
end;

procedure TGetriebeFS.Rest;
begin
  { Berechnung Punkt ooE }
  rP.E.X := rP.E0.X - FrController;
  rP.E.Y := 0;
  rP.E.Z := rP.E0.Z;

  { Berechnung Punkt ooF, ooM }
  BerechneF; { virtual }
  BerechneM;
end;

procedure TGetriebeFS.BerechneF;
begin
  { Berechnung Punkt F - Masttop }
  FrEpsilon := pi / 2 - SKK.AngleXZ(rP.C, rP.D);
  rP.F := SKK.AnglePointXZ(rP.D, FrMastLength - FrMastUnten, FrEpsilon);
end;

procedure TGetriebeFS.BerechneM;
var
  ooTemp: TPoint3D;
  a, t: single;
begin
  a := rp.F0.Distance(rp.F);
  t := (a - MastfallVorlauf) / a;
  ooTemp := rp.F - rp.F0;
  ooTemp := rp.F0 + ooTemp * t;
  rp.M := ooTemp;
end;

procedure TGetriebeFS.BerechneWinkel;
begin
  BerechneWinkel2;
end;

procedure TGetriebeFS.BerechneWinkel1;
{ FrVorstag gegeben, FrWinkel gesucht }
var
  Counter: Integer;
  svar: Boolean;
  VorstagIst, Diff: single;
  psiA, psiB: single;
  localC, ooTemp1, ooTemp2: TPoint3D;

  function VorstagLaenge(psi: single): single;
  { Viergelenk P0 P D D0, Koppelpunkt C }
  begin
    rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, psi - FrAlpha);
    rP.P := SKK.IntersectionXZ1(rP.P0, rP.D, FrWunten2D, FrSalingH);
    rP.C := SKK.IntersectionXZ1(rP.P, rP.D, FrWoben2D, FrMastOben);
    Result := rP.C0.Distance(rP.C);
  end;

begin
  ResetStatus;
  Wanten3dTo2d;

  { 1. Startwinkel ermitteln }
  { Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }
  localC := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);

  psiStart := SKK.AngleXZM(rP.D0, localC);
  psiStart := pi / 2 + psiStart + FrAlpha;

  { Test, ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag.
    Ermittlung der Koordinaten für diesen Fall. }
  FrPsi := psiStart;

  rP.C := localC;
  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);
  rP.P := SKK.IntersectionXZ1(rP.D, rP.C, FrSalingH, FrWoben2D);

  FrWanteZulang := rP.P0.Distance(rP.P) + rP.P.Distance(rP.C) - (FrWunten2D + FrWoben2D);
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
      ooTemp1 := rP.D - rP.D0;
      ooTemp2 := rP.C - rP.D;
      localC := ooTemp1.CrossProduct(ooTemp2);
      if (localC.Y > 0) then
        psiStart := psiStart + DegToRad(45);
    end;

    { 2. Endwinkel ermitteln - Mastoben parallel zu Vorstag
      rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
    localC := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten, FrVorstag - FrMastOben);
    psiEnde := SKK.AngleXZM(rP.D0, localC);
    psiEnde := pi / 2 + psiEnde + FrAlpha;

    { 3. Winkel ermitteln, für den gilt: VorstagIst gleich FrVorstag }
    { Viergelenk P0 P D D0, Koppelpunkt C }
    psiB := psiStart;
    psiA := psiEnde + DegToRad(0.01);

    Temp1 := RadToDeg(psiA);
    Temp2 := RadToDeg(psiB);

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
  rP.A := rP.P;
  rP.A.Y := -FrSalingA / 2;
  rP.B := rP.P;
  rP.B.Y := -rP.A.Y;
  { We actually want PhiVonPsi, but we can use function PsiVonPhi;
    imagine we are looking from behind - the mechanism appears mirrored,
    angle Psi needs to be transformed back and forth,
    and member length values passed according to mirrored model. }
  FrPhi := pi - TRggCalc.PsiVonPhi(pi - FrPsi, FrBasis, FrMastUnten, FrSalingH, FrWunten2D, svar);
  if FrPhi > 2 * PI then
    FrPhi := FrPhi - 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(RggStrings.LogList_String_InBerechneWinkel);
    LogList.Add(RggStrings.LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter2);
    Exit;
  end;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest; { think: refactored away with 'extract method refactoring' }
end;

function TGetriebeFS.GetKoppelKurve: TKoordLine;
{ Koppelkurve Viergelenk P0, P, D, D0 }
{ Wanten2d neu bereitgestellt,
  sonst interne Felder nicht verändert! }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;
  oooTemp: TRiggPoints;
begin
  oooTemp := rP; { aktuelle Koordinaten sichern }
  Wanten3dTo2d;

  { 1. Startwinkel }
  ooTemp := SKK.IntersectionXZ1(rP.P0, rP.D0, FrWunten2D + FrSalingH, FrMastUnten);
  phiA := SKK.AngleXZM(rP.P0, ooTemp);
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  ooTemp := SKK.IntersectionXZ1(rP.P0, rP.D0, FrWunten2D, FrSalingH + FrMastUnten);
  if SKK.Status = bmK1inK2 then
    phiE := FrAlpha + DegToRad(130)
  else
  begin
    phiE := SKK.AngleXZM(rP.P0, ooTemp);
    phiE := phiE + pi / 2 + FrAlpha;
  end;

  { 3. Koppelkurve }
  phiA := phiA + DegToRad(1);
  phiE := phiE - DegToRad(1);
  WinkelStep := (phiE - phiA) / 100;
  phiM := phiA;
  for i := 0 to 100 do
  begin
    psiM := TRggCalc.PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP.P.X := rP.P0.X + FrWunten2D * cos(phiM - FrAlpha);
    rP.P.Z := rP.P0.Z + FrWunten2D * sin(phiM - FrAlpha);
    rP.D.X := rP.D0.X + FrMastUnten * cos(psiM - FrAlpha);
    rP.D.Z := rP.D0.Z + FrMastUnten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    rP.C := SKK.IntersectionXZ1(rP.P, rP.D, FrWoben2D, FrMastOben);
    Result[i].X := rP.C.X;
    Result[i].Y := 0;
    Result[i].Z := rP.C.Z;
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

  rP.P := SKK.IntersectionXZ1(rP.D, rP.C, FrSalingH, FrWoben2D);
  FrWunten2D := rP.P.Distance(rP.P0);

  { aktualisieren }
  rP.A := rP.P;
  rP.A.Y := -FrSalingA / 2;
  rP.B := rP.P;
  rP.B.Y := -FrSalingA / 2;
  FrPhi := SKK.AngleXZM(rP.A0, rP.A);
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
    rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, psi - FrAlpha);
    rP.C := SKK.IntersectionXZ1(rP.D, rP.C0, FrMastOben, FrVorstag);

    WStrich := rP.A0.Distance(rP.C);
    Basis := rP.A0.Distance(rP.D);

    { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
    TempD := TPoint3D.Zero;
    TempD.X := Basis;
    TempC := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, WStrich, FrMastOben);
    TempA := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, FrWunten3D, FrSalingL);
    Result := TempA.Distance(TempC);
  end;

begin
  ResetStatus;

  { Vorstag gegeben, Winkel numerisch ermitteln! }
  { Startwinkel }
  Temp := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten, FrVorstag - FrMastOben);
  psiStart := SKK.AngleXZM(rP.D0, Temp);
  psiStart := psiStart + pi / 2 + FrAlpha + DegToRad(0.1);

  { Endwinkel }
  Temp := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
  psiEnde := SKK.AngleXZM(rP.D0, Temp);
  psiEnde := pi / 2 + psiEnde + FrAlpha;
  psiEnde2 := psiEnde + DegToRad(50);

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
  TRggCalc.SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := TempD.Distance(temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP.C - rP.A0;
  temp := temp * Skalar;
  temp := rP.A0 + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP.D;
  temp := temp * Skalar;
  rP.A := rP.D + temp;

  { aktualisieren }
  rP.P := rP.A;
  rP.P.Y := 0;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  FrSalingA := 2 * rP.B.Y;
  FrSalingH := rP.P.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
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

  WStrich := rP.A0.Distance(rP.C);
  Basis := rP.A0.Distance(rP.D);
  { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
  { Berechnung TempD }
  TempD := TPoint3D.Zero;
  TempD.X := Basis;
  TempC := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, WStrich, FrMastOben);
  { TempC bleibt beim Regeln unverändert }

  TempA := SKK.IntersectionXZ1(TempD, TempC, FrSalingL, FrWOben3D);
  { Radius1 = FrSalingL verändert sich beim Regeln
    Radius2 = FrWoben3D bleibt gleich beim Regeln
    TempA = SchnittPunkt1 verändert sich beim Regeln }

  TRggCalc.SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := TempD.Distance(temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP.C - rP.A0;
  temp := temp * Skalar;
  temp := rP.A0 + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP.D;
  temp := temp * Skalar;
  rP.A := rP.D + temp;

  { FrWunten3d ermitteln und aktualisieren }
  FrWunten3D := TempA.Length;
  rP.P := rP.A;
  rP.P.Y := 0;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  FrPhi := SKK.AngleXZM(rP.A0, rP.A);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingA := 2 * rP.B.Y;
  FrSalingH := rP.P.Distance(rP.D);
  FrController := FiControllerAnschlag;
end;

procedure TGetriebeFS.UpdateGetriebeOSS;
{ FrVorstag und FrWoben2d gegeben }
var
  temp: TPoint3D;
  Skalar: single;
begin
  ResetStatus;
  rP.C := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
  FrWunten2D := rP.P0.Distance(rP.C) - FrWoben2D;
  { Punkt P }
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP.P.X := rP.C.X - Skalar * (rP.C.X - rP.P0.X);
  rP.P.Y := 0;
  rP.P.Z := rP.C.Z - Skalar * (rP.C.Z - rP.P0.Z);
  { Punkte A, B }
  rP.A := rP.P;
  rP.A.Y := Skalar * rP.A0.Y;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  { Punkt D }
  temp := rP.C - rP.D0;
  Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
  temp.X := Skalar * temp.X;
  { Temp[y] := 0; }
  temp.Z := Skalar * temp.Z;
  rP.D := rP.D0 + temp;
  { aktualisieren }
  FrSalingH := rP.P.Distance(rP.D);
  FrSalingA := 2 * rP.B.Y;
  FrSalingL := rP.A.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := SKK.AngleXZM(rP.D0, rP.D);
  FrPsi := pi / 2 + FrPsi + FrAlpha;
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
  rP.C := SKK.IntersectionXZ1(rP.P0, rP.C0, TempWunten2d + TempWoben2d, FrVorstag);

  { wenn die Wanten nicht straff sind: }
  if rP.D0.Distance(rP.C) > FrMastUnten + FrMastOben then
  begin
    rP.C := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
    temp := rP.C- rP.D0;
    Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
    temp.X := Skalar * temp.X;
    temp.Z := Skalar * temp.Z;
    rP.D := rP.D0 + temp;
    FrWanteZulang := FrWunten3D + FrWoben3D - rP.C.Distance(rP.A0);
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end

  { wenn die Wanten straff sind: }
  else
  begin
    rP.D := SKK.IntersectionXZ1(rP.C, rP.D0, FrMastOben, FrMastUnten);
  end;

  { Punkt P }
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP.P.X := rP.C.X - Skalar * (rP.C.X - rP.P0.X);
  rP.P.Y := 0;
  rP.P.Z := rP.C.Z - Skalar * (rP.C.Z - rP.P0.Z);
  { Punkte A, B }
  rP.A := rP.P;
  rP.A.Y := Skalar * rP.A0.Y;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  { aktualisieren }
  FrSalingH := rP.P.Distance(rP.D);
  FrSalingA := 2 * rP.B.Y;
  FrSalingL := rP.A.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := SKK.AngleXZM(rP.D0, rP.D);
  FrPsi := pi / 2 + FrPsi + FrAlpha;
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
    with SKK do
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
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullFest, [1, s]);
            LogList.Add(s);

            { 2. Aufruf SchnittKK: TempP ermitteln }
            Radius1 := FrWunten2D;
            Radius2 := Temp.Length; { Temp unter 1. ermittelt }
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempP := SchnittPunkt1;
            TempP.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullFest, [2, s]);
            LogList.Add(s);

            { 3. Aufruf SchnittKK: Saling2d und MastUnten; TempD ermitteln }
            Radius1 := FrSalingH;
            Radius2 := FrMastUnten;
            MittelPunkt1 := TempP;
            MittelPunkt2 := rP.D0;
            TempD := SchnittPunkt1;
            TempD.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullFest, [3, s]);
            LogList.Add(s);

            { 4. Aufruf SchnittKK: WanteOben2d und MastOben; TempC ermitteln }
            Radius1 := FrWoben2D;
            Radius2 := FrMastOben;
            MittelPunkt1 := TempP;
            MittelPunkt2 := TempD;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullFest, [4, s]);
            LogList.Add(s);

            result := rP.C0.Distance(TempC);
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
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullDrehbar, [1, s]);
            LogList.Add(s);

            Radius1 := rP.D0.Distance(rP.A0);
            Radius2 := FrWunten3D;
            MittelPunkt1 := TPoint3D.Zero;
            MittelPunkt2 := TempP;
            Temp := SchnittPunkt1;
            Temp.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullDrehbar, [2, s]);
            LogList.Add(s);

            WStrich := Temp.Distance(TempC);
            WStrich2d := sqrt(sqr(WStrich) - sqr(rP.A0.Y));

            Radius1 := WStrich2d;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullDrehbar, [3, s]);
            LogList.Add(s);

            result := rP.C0.Distance(TempC);
          end;

        stOhneStarr, stOhneBiegt:
          begin
            { 1. Aufruf SchnittKK: Wante2d und Mast; TempC ermitteln }
            Radius1 := FrWunten2D + FrWoben2D;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(RggStrings.LogList_Format_String_GetVorstagNullOhne, [1, s]);
            LogList.Add(s);
            result := rP.C0.Distance(TempC);
          end;
      end;
    end;

  except
    on E: EMathError do
    begin
      s := Format(RggStrings.LogList_Format_String_GetVorstagNullException, [E.Message]);
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
  oldF := rp.F;
  oldC := rp.C;
  oldD := rp.D;
  D0 := rp.D0;
  D0F := rp.D0.Distance(rp.F);
  D0C := rp.D0.Distance(rp.C);
  D0D := FrMastUnten;

  { compute new Point F }

  newF0F := Mastfall + FrMastfallVorlauf;
  with SKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := newF0F;
    Radius2 := D0F; // unchanged
    MittelPunkt1 := rP.F0;
    MittelPunkt2 := rP.D0;
    rP.F := SchnittPunkt1;
  end;

  { compute new Points C and D }

  newF := rp.F;
  oldPsi := Pi/2 - SKK.AngleXZ(oldF, D0);
  newPsi := Pi/2 - SKK.AngleXZ(newF, D0);
  delta := newPsi - oldPsi;

  w := Pi/2 - SKK.AngleXZ(oldC, D0);
  w := w + delta;
  newC.X := D0.X + D0C * cos(w);
  newC.Y := 0;
  newC.Z := D0.Z + D0C * sin(w);

  w := Pi/2 - SKK.AngleXZ(oldD, D0);
  w := w + delta;
  newD.X := D0.X + D0D * cos(w);
  newD.Y := 0;
  newD.Z := D0.Z + D0D * sin(w);

  rp.C := newC;
  rp.D := newD;

  { continue as in original BiegeUndNeigeF }

  FrVorstag := rP.C0.Distance(rP.C);

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
  tempGamma := arctan2(k4, k6 - FrMastUnten);
  k5 := (FrMastOben + FrMastEnde) * sin(tempGamma);
  k7 := (FrMastOben + FrMastEnde) * cos(tempGamma);
  tempBeta := arctan2(k5, (FrMastUnten + k7));
  k3 := sqrt(sqr(k5) + sqr(FrMastUnten + k7));
  { oder k3 := k5 / sin(tempBeta) }
  { k3 = Abstand D0F }

  { Bessere Werte für k3 und tempBeta bestimmen }
  KorrekturF(Biegung, k1, k2, k3, tempBeta, tempGamma); { virtuelle Methode }

  { 2. Berechnung Punkt F mit Mastfall }
  rP.F := SKK.IntersectionXZ1(rP.F0, rP.D0, Mastfall + FrMastfallVorlauf, k3);

  { 3. psi, D, und C ermitteln }
  FrPsi := SKK.AngleXZM(rP.D0, rP.F);
  FrPsi := pi / 2 + FrPsi + FrAlpha - tempBeta;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);
  rP.C := SKK.AnglePointXZ(rP.D, FrMastOben, FrPsi - FrAlpha + tempGamma);

  FrVorstag := rP.C0.Distance(rP.C);

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
  ooTemp := (rP.C - rP.P0).Normalize;
  ooTemp := ooTemp * FrWoben2D;
  rP.P := rP.C + ooTemp;
  SalingHStart := rP.P.Distance(rP.D);
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
  ooTemp := (rP.C - rP.A0).Normalize;
  ooTemp := ooTemp * FrWoben3D;
  rP.A := rP.C + ooTemp;
  SalingLStart := rP.A.Distance(rP.D);
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

  rP.C := SKK.IntersectionXZ1(rP.F0, rP.D0, MastfallC, k1 + k2);

  { psi und Punkt D }
  FrPsi := SKK.AngleXZM(rP.D0, rP.C);
  FrPsi := pi / 2 + FrPsi + FrAlpha - tempAlpha;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);

  { Vorstag }
  FrVorstag := rP.C0.Distance(rP.C);

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

procedure TGetriebeFS.ComputeStartAngle1;
var
  localC: TPoint3D;
begin
  { 1. Startwinkel ermitteln - Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }

  SKK.SchnittEbene := seXZ;
  SKK.Radius1 := FrMastUnten + FrMastOben;
  SKK.Radius2 := FrVorstag;
  SKK.MittelPunkt1 := rP.D0;
  SKK.MittelPunkt2 := rP.C0;
  localC := SKK.SchnittPunkt1;

  if not SKK.SPVorhanden then
  begin
    Msg := 'ComputeStartAngle - cannot compute localC';
    HasError := True;
  end;

  psiStart := arctan2( (rP.D0.X - localC.X), (localC.Z - rP.D0.Z) );
  psiStart := pi / 2 + psiStart;
end;

procedure TGetriebeFS.ComputeStartAngle2;
var
  Test: Boolean;
  v1, v2, v: TPoint3D;
begin
  { 1. Startwinkel ermitteln - Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }

  psiStart := DegToRad(110);

  LoopCounterS := 0;
  repeat
    LoopCounterS := LoopCounterS + 1;
    psiStart := psiStart - DegToRad(0.5);
    UpdateGetriebeTemp(psiStart);
    v1 := (rP.D0 - tempD).Normalize;
    v2 := (tempD - tempC).Normalize;
    v := v1.CrossProduct(v2);
    Test := abs(v.Y) < 0.01;
  until Test or (LoopCounterS = 200);

  if LoopCounterS >= 200 then
  begin
    Msg := 'ComputeStartAngle2 - psiStart not found';
    HasError := True;
  end;
end;

procedure TGetriebeFS.ComputeEndAngle1;
var
  localC: TPoint3D;
begin
  { 2. Endwinkel ermitteln - MastOben parallel zu Vorstag
    rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
  SKK.SchnittEbene := seXZ;
  SKK.Radius1 := FrMastUnten;
  SKK.Radius2 := FrVorstag - FrMastOben;
  SKK.MittelPunkt1 := rP.D0;
  SKK.MittelPunkt2 := rP.C0;
  localC := SKK.SchnittPunkt1;

  if not SKK.SPVorhanden then
  begin
    Msg := 'ComputeEndAngle1 - cannot compute LocalC';
  end;

  psiEnde := arctan2((rP.D0.X - localC.X), (localC.Z - rP.D0.Z));
  psiEnde := pi / 2 + psiEnde;
end;

procedure TGetriebeFS.ComputeEndAngle2;
var
  Test: Boolean;
  v1, v2, v: TPoint3D;
begin
  { 2. Endwinkel ermitteln - MastOben parallel zu Vorstag
    rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }

  psiEnde := psiStart;

  LoopCounterE := 0;
  repeat
    LoopCounterE := LoopCounterE + 1;
    psiEnde := psiEnde - DegToRad(0.5);
    UpdateGetriebeTemp(psiEnde);
    v1 := (rP.C0 - tempC).Normalize;
    v2 := (tempD - tempC).Normalize;
    v := v1.CrossProduct(v2);
    Test := abs(v.Y) < 0.01;
  until Test or (LoopCounterE = 200);

  if LoopCounterE >= 200 then
  begin
    Msg := 'EndAngle not found';
    HasError := True;
  end;
end;

procedure TGetriebeFS.LoopForPsi;
var
  VorstagIst: single;
  Diff: single;
  psiA: single;
  psiB: single;
begin
  { 3. Winkel ermitteln, für den gilt: VorstagIst gleich FrVorstag
    Viergelenk P0 P D D0, Koppelpunkt C }
  psiB := psiStart - DegToRad(0.01);
  psiA := psiEnde + DegToRad(0.01);

  Temp1 := RadToDeg(psiA);
  Temp2 := RadToDeg(psiB);

  LoopCounterT := 0;
  repeat
    LoopCounterT := LoopCounterT + 1;
    psi := (psiA + psiB) / 2;
    VorstagIst := GetVorstagLaenge(psi);
    Diff := VorstagIst - FrVorstag;
    if Diff > 0 then
      psiB := psi
    else
      psiA := psi;
  until (abs(Diff) < 0.1) or (LoopCounterT = 200);

  Temp3 := LoopCounterT;

  if LoopCounterT >= 200 then
  begin
    Msg := 'LoopForPsi - cannot find psi';
    HasError := True;
  end;
end;

procedure TGetriebeFS.UpdateGetriebeTemp(anglePsi: single);
var
  localP0: TPoint3D;
  localD0: TPoint3D;
begin
  localP0 := rP.P0;
  localD0 := rP.D0;

  { tempD }
  tempD.X := localD0.X + FrMastUnten * cos(anglePsi);
  tempD.Y := 0;
  tempD.Z := localD0.Z + FrMastUnten * sin(anglePsi);

  { tempP }
  SKK.SchnittEbene := seXZ;
  SKK.Radius1 := FrWunten2D;
  SKK.Radius2 := FrSalingH;
  SKK.MittelPunkt1 := localP0;
  SKK.MittelPunkt2 := tempD;
  tempP := SKK.SchnittPunkt1;

  if not SKK.SPVorhanden then
  begin
    Msg := 'UpdateGetriebeTemp - cannot compute tempP';
    HasError := True;
  end;

  { tempC }
  SKK.SchnittEbene := seXZ;
  SKK.Radius1 := FrWoben2D;
  SKK.Radius2 := FrMastOben;
  SKK.MittelPunkt1 := tempP;
  SKK.MittelPunkt2 := tempD;
  tempC := SKK.SchnittPunkt1;

  if not SKK.SPVorhanden then
  begin
    Msg := 'UpdateGetriebeTemp - cannot compute tempC';
    HasError := True;
  end;
end;

function TGetriebeFS.GetVorstagLaenge(psi: single): single;
{ Viergelenk P0 P D D0, Koppelpunkt C }
begin
  UpdateGetriebeTemp(psi);
  result := tempC.Distance(rP.C0);
end;

procedure TGetriebeFS.TestWante;
var
  localP0: TPoint3D;
  localD0: TPoint3D;
  localC0: TPoint3D;

  localPsi: single;

  WDiff: single;
begin
  { Test ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag. }

  localD0 := rP.D0;
  localP0 := rP.P0;
  localC0 := rP.C0;

  with SKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrMastUnten + FrMastOben;
    Radius2 := FrVorstag;
    MittelPunkt1 := localD0;
    MittelPunkt2 := localC0;
    tempC := SchnittPunkt1;
  end;

  localPsi := arctan2((localD0.X - tempC.X), (tempC.Z - localD0.Z));
  localPsi := pi / 2 + localPsi;

  tempD.X := localD0.X + FrMastUnten * cos(localPsi);
  tempD.Y := 0;
  tempD.Z := localD0.Z + FrMastUnten * sin(localPsi);

  with SKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := FrSalingH;
    Radius2 := FrWoben2D;
    MittelPunkt1 := tempD;
    MittelPunkt2 := tempC;
    tempP := SchnittPunkt1;
  end;

  WDiff := tempP.Distance(localP0) + tempC.Distance(tempP) - (FrWunten2D + FrWoben2D);
  if WDiff < 0 then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
    Msg := 'TestWante - no tension in shroud';
    HasError := True;
  end;

  FrWanteZulang := wDiff;
end;

procedure TGetriebeFS.RecordInitialPosition;
begin
  rP.D := tempD;
  rP.P := tempP;
  rP.C := tempC;
  FrPsi := psiStart + FrAlpha;
end;

procedure TGetriebeFS.ComputeInitialPosition(angle: single);
begin
  UpdateGetriebeTemp(angle);
  rP.D := tempD;
  rP.C := tempC;
  rP.P := tempP;
  FrPsi := angle + FrAlpha;
end;

procedure TGetriebeFS.ComputeWinkel;
begin
  { FrVorstag gegeben, FrWinkel gesucht }

  Msg := 'ok';
  HasError := False;

  psi := 0.0;

  ComputeStartAngle1;

  TestWante; // and record initial position in case of WanteZulang
  if HasError then
  begin
    RecordInitialPosition;
//    ComputeInitialPosition(psiStart); // that would be a little different
    Exit;
  end;

  ComputeEndAngle1;
  if HasError then
  begin
//    ComputeInitialPosition(psiStart);
    Exit;
  end;

  LoopForPsi;
  if HasError then
  begin
//    ComputeInitialPosition(psiStart);
    Exit;
  end;

  ComputeInitialPosition(psi);
  if HasError then
  begin
    Msg := 'Compute Winkel has Error';
    Exit;
  end;
end;

procedure TGetriebeFS.BerechneWinkel2;
{ FrVorstag gegeben, FrWinkel gesucht }
var
  svar: Boolean;
begin
  ResetStatus;
  Wanten3dTo2d;

  ComputeWinkel;

  { aktualisieren }
  rP.A := rP.P;
  rP.A.Y := -FrSalingA / 2;
  rP.B := rP.P;
  rP.B.Y := -rP.A.Y;
  { We actually want PhiVonPsi, but we can use function PsiVonPhi;
    imagine we are looking from behind - the mechanism appears mirrored,
    angle Psi needs to be transformed back and forth,
    and member length values passed according to mirrored model. }
  FrPhi := pi - TRggCalc.PsiVonPhi(pi - FrPsi, FrBasis, FrMastUnten, FrSalingH, FrWunten2D, svar);
  if FrPhi > 2 * PI then
    FrPhi := FrPhi - 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(RggStrings.LogList_String_InBerechneWinkel);
    LogList.Add(RggStrings.LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter2);
    Exit;
  end;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest;
end;

function TGetriebeFS.GetCounterValue(Idx: Integer): Integer;
begin
  result := 0;
  case Idx of
    0: result := UpdateGetriebeCounter;
    1: result := ExitCounter1;
    2: result := ExitCounter2;
    3: result := ExitCounter3;
    4: result := ExitCounter4;
    5: result := ExitCounter5;
    6: result := ExitCounter6;
    7: result := ExitCounter7;
  end;
end;

function TGetriebeFS.GetTempValue(Idx: Integer): single;
begin
  result := 0;
  case Idx of
    1: result := Temp1;
    2: result := Temp2;
    3: result := Temp3;
    4: result := Temp4;
  end;
end;

end.
