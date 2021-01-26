unit RiggVar.FZ.Z18_BerechneWinkel;

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
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  RiggVar.RG.Types,
  RiggVar.FB.Color,
  RiggVar.FD.Chart,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TAngleMode = (AngleMode1, AngleMode2, Manual);

  TRggDrawingZ18 = class(TRggDrawingKK)
  private
    StartAngleMode: TAngleMode;
    EndAngleMode: TAngleMode;

    FMsg: string;

    WDiff: single;
    FGetriebeOK: Boolean;

    tempP: TPoint3D;
    tempD: TPoint3D;
    tempC: TPoint3D;

    TempA: single;
    TempB: single;

    ComputeCounter: Integer;
    LoopCounterT: Integer;
    LoopCounterS: Integer;
    LoopCounterE: Integer;

    VorstagDefault: single;
    StartAngleDefault: single;
    EndAngleDefault: single;

    psiStart: single;
    psiEnde: single;
    psi: single;

    psiStartDegrees: single;
    psiEndeDegrees: single;
    psiDegrees: single;

    FrBasis: single;
    FrVorstag: single;
    FrSalingH: single;
    FrWunten2D: single;
    FrWoben2D: single;
    FrMastUnten: single;
    FrMastOben: single;

    Raster: single;

    colorStart: TAlphaColor;
    colorEnde: TAlphaColor;

    HasError: Boolean;

    procedure InitLengthValues;

    function GetVorstagLaenge(apsi: single): single;

    procedure BerechneWinkel;

    procedure ComputeStartAngle;
    procedure TestWante;
    procedure ComputeEndAngle;
    procedure LoopForPsi;

    procedure UpdateChart;
    procedure UpdateGetriebe;
    procedure UpdateGetriebeStart;
    procedure UpdateGetriebeEnde;

    procedure ComputeStartAngle1;
    procedure ComputeStartAngle2;
    procedure ComputeEndAngle1;
    procedure ComputeEndAngle2;
    procedure ComputeKK;

    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure Btn3Click(Sender: TObject);

    procedure SetMsg(const Value: string);
    property Msg: string read FMsg write SetMsg;
  public
    P0, P: TRggCircle;
    D0, D: TRggCircle;
    C0, C: TRggCircle;

    P0D0: TRggLine;
    D0D: TRggLine;
    C0C: TRggLine;
    P0P: TRggLine;
    PD: TRggLine;
    PC: TRggLine;
    DC: TRggLine;

    StartC: TRggCircle;
    StartD: TRggCircle;
    StartP: TRggCircle;
    StartC0C: TRggLine;
    StartD0D: TRggLine;
    StartDC: TRggLine;

    EndC: TRggCircle;
    EndD: TRggCircle;
    EndP: TRggCircle;
    EndC0C: TRggLine;
    EndD0D: TRggLine;
    EndDC: TRggLine;

    ParamV: TRggParam;
    ParamS: TRggParam;
    ParamE: TRggParam;

    Label1: TRggLabel;
    Label2: TRggLabel;
    Label3: TRggLabel;
    Label4: TRggLabel;
    Label5: TRggLabel;
    Label6: TRggLabel;

    Chart: TRggChart;

    Arc: TRggBigArc;
    KK: TRggPolyCurve;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
  end;

implementation

{ TRggDrawingZ18 }

procedure TRggDrawingZ18.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 0;
  oy := 30;

  P0.Center.X := ox + 175;
  P0.Center.Y := oy + 600;
  P0.Center.Z := 0;

  D0.Center.X := ox + 390;
  D0.Center.Y := oy + 730;
  D0.Center.Z := 0;

  C0.Center.X := ox + 620;
  C0.Center.Y := oy + 650;
  C0.Center.Z := 0;

  P.Center.X := ox + 210;
  P.Center.Y := oy + 320;
  P.Center.Z := 0;

  D.Center.X := ox + 360;
  D.Center.Y := oy + 340;
  D.Center.Z := 0;

  C.Center.X := ox + 300;
  C.Center.Y := oy + 100;
  C.Center.Z := 0;

  ParamV.Scale := 0.5;
  ParamS.Scale := 0.2;
  ParamE.Scale := 0.2;

  ParamV.BaseValue := VorstagDefault;
  ParamS.BaseValue := StartAngleDefault;
  ParamE.BaseValue := EndAngleDefault;

  InitLengthValues;
end;

constructor TRggDrawingZ18.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z18-BerechneWinkel';
  WantSort := False;

  Raster := 35;

  StartAngleMode := TAngleMode.AngleMode2;
  EndAngleMode := TAngleMode.AngleMode2;

  VorstagDefault := 633;
  StartAngleDefault := 110;
  EndAngleDefault := 85;

  colorStart := TRggColors.Plum;
  colorEnde := TRggColors.Antiquewhite;

  P0 := TRggCircle.Create('P0');
  P0.StrokeColor := TRggColors.Orangered;

  P := TRggCircle.Create('P');
  P.StrokeColor := TRggColors.Orangered;
  P.IsComputed := True;

  D0 := TRggCircle.Create('D0');
  D0.StrokeColor := TRggColors.Blue;

  D := TRggCircle.Create('D');
  D.StrokeColor := TRggColors.Blue;
  D.IsComputed := True;

  C0 := TRggCircle.Create('C0');
  C0.StrokeColor := TRggColors.Blue;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Blue;
  C.IsComputed := True;

  StartC := TRggCircle.Create('StartC');
  StartC.ShowCaption := False;
  StartC.StrokeColor := colorStart;
  StartC.IsComputed := True;

  StartD := TRggCircle.Create('StartD');
  StartD.ShowCaption := False;
  StartD.StrokeColor := colorStart;
  StartD.IsComputed := True;

  StartP := TRggCircle.Create('StartP');
  StartP.ShowCaption := False;
  StartP.StrokeColor := colorStart;
  StartP.IsComputed := True;

  EndC := TRggCircle.Create('EndC');
  EndC.ShowCaption := False;
  EndC.StrokeColor := colorEnde;
  EndC.IsComputed := True;

  EndD := TRggCircle.Create('EndD');
  EndD.ShowCaption := False;
  EndD.StrokeColor := colorEnde;
  EndD.IsComputed := True;

  EndP := TRggCircle.Create('EndP');
  EndP.ShowCaption := False;
  EndP.StrokeColor := colorEnde;
  EndP.IsComputed := True;

  ParamV := TRggParam.Create;
  ParamV.Caption := 'Vorstag';
  ParamV.StartPoint.Y := 1 * Raster;
  Add(ParamV);

  ParamS := TRggParam.Create;
  ParamS.Caption := 'Start Angle';
  ParamS.StartPoint.Y := 2 * Raster;
  Add(ParamS);

  ParamE := TRggParam.Create;
  ParamE.Caption := 'End Angle';
  ParamE.StartPoint.Y := 3 * Raster;
  Add(ParamE);

  Label1 := TRggLabel.Create;
  Label1.Caption := 'L1';
  Label1.Text := 'L1';
  Label1.Position.Y := 4 * Raster;
  Add(Label1);

  Label2 := TRggLabel.Create;
  Label2.Caption := 'L2';
  Label2.Text := 'L2';
  Label2.Position.Y := 5 * Raster;
  Add(Label2);

  Label3 := TRggLabel.Create;
  Label3.Caption := 'L3';
  Label3.Text := 'L3';
  Label3.Position.Y := 6 * Raster;
  Add(Label3);

  Label4 := TRggLabel.Create;
  Label4.Caption := 'L4';
  Label4.Text := 'L4';
  Label4.Position.Y := 7 * Raster;
  Add(Label4);

  Label5 := TRggLabel.Create;
  Label5.Caption := 'L5';
  Label5.Text := 'L5';
  Label5.Position.Y := 8 * Raster;
  Add(Label5);

  Label6 := TRggLabel.Create;
  Label6.Caption := 'L6';
  Label6.Text := 'L6';
  Label6.IsMemoLabel := True;
  Label6.Position.Y := 9 * Raster;
  Label6.StrokeColor := TRggColors.Green;
  Add(Label6);

  L := TRggLine.Create('P0D0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := P0;
  L.Point2 := D0;
  Add(L);
  P0D0 := L;

  L := TRggLine.Create('P0P');
  L.IsComputed := True;
  L.StrokeColor := TRggColors.Red;
  L.Point1 := P0;
  L.Point2 := P;
  Add(L);
  P0P := L;

  L := TRggRotaLine.Create('D0D');
  L.IsComputed := True;
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D0;
  L.Point2 := D;
  Add(L);
  D0D := L;

  L := TRggLine.Create('PD');
  L.IsComputed := True;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := P;
  L.Point2 := D;
  Add(L);
  PD := L;

  L := TRggLine.Create('PC');
  L.StrokeThickness := 0.5;
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := P;
  L.Point2 := C;
  L.IsComputed := True;
  Add(L);
  PC := L;

  L := TRggLine.Create('DC');
  L.IsComputed := True;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := D;
  L.Point2 := C;
  Add(L);
  DC := L;

  L := TRggLine.Create('C0C');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := C0;
  L.Point2 := C;
  L.IsComputed := True;
  Add(L);
  C0C := L;

  { Start Situation }

  L := TRggLine.Create('StartC0C');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorStart;
  L.Point1 := C0;
  L.Point2 := StartC;
  L.IsComputed := True;
  Add(L);
  StartC0C := L;

  L := TRggLine.Create('StartD0D');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorStart;
  L.Point1 := D0;
  L.Point2 := StartD;
  L.IsComputed := True;
  Add(L);
  StartD0D := L;

  L := TRggLine.Create('StartDC');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorStart;
  L.Point1 := StartD;
  L.Point2 := StartC;
  L.IsComputed := True;
  Add(L);
  StartDC := L;

  { End Situation }

  L := TRggLine.Create('EndC0C');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorEnde;
  L.Point1 := C0;
  L.Point2 := EndC;
  L.IsComputed := True;
  Add(L);
  EndC0C := L;

  L := TRggLine.Create('EndD0D');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorEnde;
  L.Point1 := D0;
  L.Point2 := EndD;
  L.IsComputed := True;
  Add(L);
  EndD0D := L;

  L := TRggLine.Create('EndDC');
  L.ShowCaption := False;
  L.StrokeThickness := 1;
  L.StrokeColor := colorEnde;
  L.Point1 := EndD;
  L.Point2 := EndC;
  L.IsComputed := True;
  Add(L);
  EndDC := L;

  Arc := TRggBigArc.Create('ArcC0');
  Arc.Point1 := C0;
  Arc.Point2 := C;
  Arc.StrokeThickness := 1;
  Arc.StrokeColor := TRggColors.Plum;
  Add(Arc);

  KK := TRggPolyCurve.Create('KK', 50);
  KK.StrokeThickness := 3;
  KK.StrokeColor := TRggColors.Aquamarine;
  KK.ShowCaption := True;
  Add(KK);

  Add(P0);
  Add(P);
  Add(D0);
  Add(D);
  Add(C0);
  Add(C);

  Add(StartC);
  Add(StartD);
  Add(StartP);
  Add(EndC);
  Add(EndD);
  Add(EndP);

  Chart := TRggChart.Create;
  Chart.Caption := 'Test';
  Chart.StrokeThickness := 1;
  Chart.StrokeColor := TRggColors.Dodgerblue;
  Chart.InitDefault;
  Chart.Box.X := 250;
  Chart.Box.Y := 250;
  Chart.Box.Width := 400;
  Chart.Box.Height := 200;
  Chart.WantRectangles := True;
  Chart.WantCurve := True;
  Add(Chart);

  WantRotation := False;
  WantSort := False;

  DefaultElement := ParamV;

  InitDefaultPos;
  SaveAll;

  WantMemoLines := True;
end;

procedure TRggDrawingZ18.InitLengthValues;
begin
  FrBasis := P0D0.LineLength;
  FrSalingH := PD.LineLength;

  FrWunten2D := P0P.LineLength;
  FrWoben2D := PC.LineLength;

  FrMastUnten := D0D.LineLength;
  FrMastOben := DC.LineLength;

  FrVorstag := VorstagDefault;
end;

procedure TRggDrawingZ18.BerechneWinkel;
begin
  { FrVorstag gegeben, FrWinkel gesucht }

  Msg := 'ok';
  HasError := False;

  psi := 0.0;
  psiDegrees := 0;

  ComputeStartAngle;
//  if HasError then
//    Exit;

  TestWante;
//  if HasError then
//  begin
//    RecordInitialPosition;
//    Exit;
//  end;

  ComputeEndAngle;
//  if HasError then
//  begin
//    Exit;
//  end;

  LoopForPsi;
//  if HasError then
//  begin
//    Exit;
//  end;

  psiDegrees := RadToDeg(psi);
end;

procedure TRggDrawingZ18.UpdateGetriebe;
begin
  D.Center.C := SKK.AnglePoint(D0.Center, FrMastUnten, psi);
  P.Center.C := SKK.Intersection2(P0.Center, D.Center, FrWunten2D, FrSalingH);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebe - cannot compute P';

  C.Center.C := SKK.Intersection2(P.Center, D.Center, FrWoben2D, FrMastOben);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebe - cannot compute C';
end;

procedure TRggDrawingZ18.UpdateGetriebeStart;
begin
  StartD.Center.C := SKK.AnglePoint(D0.Center, FrMastUnten, psiStart);
  StartP.Center.C := SKK.Intersection2(P0.Center, StartD.Center, FrWunten2D, FrSalingH);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebeStart - cannot compute StartP';

  { StartC }
  StartC.Center.C := SKK.Intersection2(StartP.Center, StartD.Center, FrWoben2D, FrMastOben);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebeStart - cannot compute StartC';
end;

procedure TRggDrawingZ18.UpdateGetriebeEnde;
begin
  EndD.Center.C := SKK.AnglePoint(D0.Center, FrMastUnten, psiEnde);
  EndP.Center.C := SKK.Intersection2(P0.Center, EndD.Center, FrWunten2D, FrSalingH);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebeEnde - cannot compute EndC';

  EndC.Center.C := SKK.Intersection2(EndP.Center, EndD.Center, FrWoben2D, FrMastOben);

  if not SKK.SPVorhanden then
    Msg := 'UpdateGetriebeEnde - cannot compute EndC';
end;

function TRggDrawingZ18.GetVorstagLaenge(apsi: single): single;
{ Viergelenk P0 P D D0, Koppelpunkt C }
var
  localD, localP, localC: TPoint3D;
begin
  localD := SKK.AnglePoint(D0.Center, FrMastUnten, apsi);
  localP := SKK.Intersection2(P0.Center.C, localD, FrWunten2D, FrSalingH);

  if not SKK.SPVorhanden then
    Msg := 'GetVorstagLänge - cannot compute localP';

  localC := SKK.Intersection1(localD, localP, FrMastOben, FrWoben2D);

  if not SKK.SPVorhanden then
    Msg := 'GetVorstagLänge - cannot compute localC';

  result := localC.Distance(C0.Center.C);
end;

procedure TRggDrawingZ18.UpdateChart;
var
  i: Integer;
  j: single;
  k: single;
begin
  k := (psiEnde - psiStart) / Chart.Count;
  for i := 0 to Chart.Count do
  begin
    j := psiStart + i * k;
    Chart.Poly[i] := GetVorstagLaenge(j);
  end;
  Chart.LookForYMinMax;
end;

procedure TRggDrawingZ18.Compute;
begin
  Inc(ComputeCounter);
  ML.Clear;

  FrVorstag := ParamV.RelativeValue;

  BerechneWinkel;

  UpdateChart;

  ParamV.Text := Format('Param Vo = %.2f', [ParamV.RelativeValue]);
  ParamS.Text := Format('Param Sa = %.2f', [ParamS.RelativeValue]);
  ParamE.Text := Format('Param Ea = %.2f', [ParamE.RelativeValue]);

  Label1.Text := Format('Psi = %.2f (%.2f .. %.2f)', [psiDegrees, psiStartDegrees, psiEndeDegrees]);
  Label2.Text := Format('Counter = %d (%d, %d, %d)', [ComputeCounter, LoopCounterS, LoopCounterE, LoopCounterT]);
  Label3.Text := Format('FrVorstag = %.2f, C0C.LineLength = %.2f;', [FrVorstag, C0C.LineLength]);
  Label4.Text := Format('Range = (%.2f .. %.2f)', [TempA, TempB]);
  Label5.Text := Format('AngleMode = (%d, %d)', [Ord(StartAngleMode), Ord(EndAngleMode)]);
  Label6.Text := Format('ML.Text = %s', [ML.Text]);

  UpdateGetriebe;
  UpdateGetriebeStart;
  UpdateGetriebeEnde;

  ComputeKK;
end;

procedure TRggDrawingZ18.ComputeStartAngle1;
var
  localC: TPoint3D;
begin
  { 1. Startwinkel ermitteln - Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }

  localC := SKK.Intersection2(D0.Center, C0.Center, FrMastUnten + FrMastOben, FrVorstag);

  if not SKK.SPVorhanden then
  begin
    Msg := 'ComputeStartAngle - cannot compute localC';
    HasError := True;
  end;

  psiStart := SKK.OuterAngle(D0.Center.C, localC);
end;

procedure TRggDrawingZ18.ComputeStartAngle2;
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
    UpdateGetriebeStart;
    v1 := (D0.Center - StartD.Center).Normalize.C;
    v2 := (StartD.Center - StartC.Center).Normalize.C;
    v := v1.CrossProduct(v2);
    Test := abs(v.Z) < 0.01;
  until Test or (LoopCounterS = 200);

  if LoopCounterS >= 200 then
  begin
    Msg := 'ComputeStartAngle2 - psiStart not found';
    HasError := True;
  end;
end;

procedure TRggDrawingZ18.ComputeEndAngle1;
var
  localC: TPoint3D;
begin
  { 2. Endwinkel ermitteln - MastOben parallel zu Vorstag
    rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
  localC := SKK.Intersection2(D0.Center, C0.Center, FrMastUnten, FrVorstag - FrMastOben);

  if not SKK.SPVorhanden then
  begin
    Msg := 'ComputeEndAngle1 - cannot compute LocalC';
  end;

  psiEnde := SKK.OuterAngle(D0.Center.C, localC);
end;

procedure TRggDrawingZ18.ComputeEndAngle2;
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
    UpdateGetriebeEnde;
    v1 := (C0.Center - EndC.Center).Normalize.C;
    v2 := (EndD.Center - EndC.Center).Normalize.C;
    v := v1.CrossProduct(v2);
    Test := abs(v.Z) < 0.01;
  until Test or (LoopCounterE = 200);

  if LoopCounterE >= 200 then
  begin
    Msg := 'ComputeEndAngle2 - _psiEnde not found';
  end;
end;

procedure TRggDrawingZ18.LoopForPsi;
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

  TempA := RadToDeg(psiA);
  TempB := RadToDeg(psiB);

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

  if LoopCounterT >= 200 then
  begin
    Msg := 'LoopForPsi - cannot find psi';
    HasError := True;
  end;
end;

procedure TRggDrawingZ18.TestWante;
var
  localP0: TPoint3D;
  localD0: TPoint3D;
  localC0: TPoint3D;

  localPsi: single;
begin
  { Test ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag. }

  localD0 := D0.Center.C;
  localP0 := P0.Center.C;
  localC0 := C0.Center.C;

  tempC := SKK.Intersection2(localD0, localC0, FrMastUnten + FrMastOben, FrVorstag);
  localPsi := SKK.OuterAngle(localD0, tempC);
  tempD := SKK.AnglePoint(D0.Center, FrMastUnten, localPsi);
  tempP := SKK.Intersection1(tempD, tempC, FrSalingH, FrWoben2D);

  WDiff := tempP.Distance(localP0) + tempC.Distance(tempP) - (FrWunten2D + FrWoben2D);
  if WDiff < 0 then
  begin
    FGetriebeOK := False;
//    Include(FGetriebeStatus, gsWanteZulang);
    Msg := 'TestWante - no tension in shroud';
    HasError := True;
  end;
end;

procedure TRggDrawingZ18.ComputeStartAngle;
begin
  case StartAngleMode of
    AngleMode1: ComputeStartAngle1;
    AngleMode2: ComputeStartAngle2;
    Manual: psiStart := DegToRad(ParamS.RelativeValue);
  end;
  psiStartDegrees := RadToDeg(psiStart);
  psi := psiStart;
end;

procedure TRggDrawingZ18.ComputeEndAngle;
begin
  case EndAngleMode of
    AngleMode1: ComputeEndAngle1;
    AngleMode2: ComputeEndAngle2;
    Manual: psiEnde := DegToRad(ParamE.RelativeValue);
  end;
  psiEndeDegrees := RadToDeg(psiEnde);
end;

procedure TRggDrawingZ18.InitButtons(BG: TRggButtonGroup);
begin
  inherited;
  BG.Btn1.OnClick := Btn1Click;
  BG.Btn2.OnClick := Btn2Click;
  BG.Btn3.OnClick := Btn3Click;

  BG.Btn1.Text := 'AM1';
  BG.Btn2.Text := 'AM2';
  BG.Btn3.Text := 'AMM';

  BG.Btn1.Hint := 'AngleMode 1';
  BG.Btn2.Hint := 'AngleMode 2';
  BG.Btn3.Hint := 'AngleMode Manual';
end;

procedure TRggDrawingZ18.Btn1Click(Sender: TObject);
begin
  StartAngleMode := TAngleMode.AngleMode1;
  EndAngleMode := TAngleMode.AngleMode1;
  UpdateDrawing;
end;

procedure TRggDrawingZ18.Btn2Click(Sender: TObject);
begin
  StartAngleMode := TAngleMode.AngleMode2;
  EndAngleMode := TAngleMode.AngleMode2;
  UpdateDrawing;
end;

procedure TRggDrawingZ18.Btn3Click(Sender: TObject);
begin
  StartAngleMode := TAngleMode.Manual;
  EndAngleMode := TAngleMode.Manual;
  UpdateDrawing;
end;

procedure TRggDrawingZ18.SetMsg(const Value: string);
begin
  FMsg := Value;
  if ML.Count < 10 then
    ML.Add(Value);
  if ML.Count > 1 then
    HasError := True;
end;

procedure TRggDrawingZ18.ComputeKK;
var
  psiA: single;
  psiB: single;
  i: Integer;
  angle: single;
  step: single;

  localD, localP, localC: TPoint3D;

  procedure  GetLocalC(psi: single);
  { Viergelenk P0 P D D0, Koppelpunkt C }
  begin
    localD := SKK.AnglePoint(D0.Center, FrMastUnten, psi);
    localP := SKK.Intersection2(P0.Center.C, localD, FrWUnten2D, FrSalingH);

    if not SKK.SPVorhanden then
    begin
      localC := TPoint3D.Zero;
      Exit;
    end;

    localC := SKK.Intersection1(localD, localP, FrMastOben, FrWoben2D);

    if not SKK.SPVorhanden then
    begin
      localC := TPoint3D.Zero;
      Exit;
    end;
  end;

begin
  psiA := psiEnde + DegToRad(0);
  psiB := psiStart - DegToRad(0);
  step := (psiB - PsiA) / KK.Count;
  for i := 0 to KK.Count-1 do
  begin
    angle := psiA + i * step;
    GetLocalC(angle);
    KK.Poly[i].X := localC.X;
    KK.Poly[i].Y := localC.Y;
  end;
end;

end.
