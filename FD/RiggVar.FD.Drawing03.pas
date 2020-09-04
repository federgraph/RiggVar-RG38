unit RiggVar.FD.Drawing03;

interface

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD03 = class(TRggDrawing)
  private
    Count: Integer;
    ox: single;
    oy: single;
    SchnittKK: TSchnittKK;
    rP: TRiggPoints;
    procedure UpdateKoppelkurve;
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TSchnittKKCircleLL;
    C: TSchnittKKCircleLL;

    A0B0: TRggLine;
    A0A: TRggLine;
    B0B: TRggLine;
    AB: TRggLine;
    AC: TRggLine;
    BC: TRggLine;

    LabelB: TRggLabel;
    LabelC: TRggLabel;
    LabelPhi: TRggLabel;
    LabelPsi: TRggLabel;

    KK: TRggPolyLine;

    Phi: TRggArc;
    Psi: TRggArc;

    constructor Create;
    destructor Destroy; override;

    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingD03 }

procedure TRggDrawingD03.InitDefaultPos;
begin
  ox := 100;
  oy := 600;

  A0.Center.X := ox + 0;
  A0.Center.Y := oy - 0;
  A0.Center.Z := 0;

  B0.Center.X := ox + 400;
  B0.Center.Y := oy - 0;
  B0.Center.Z := 0;

  A.Center.X := ox + 100;
  A.Center.Y := oy - 200;
  A.Center.Z := 0;

  B.Center.X := ox + 400;
  B.Center.Y := oy - 200;
  B.Center.Z := 0;

  C.Center.X := ox + 300;
  C.Center.Y := oy - 400;
  C.Center.Z := 0;
end;

procedure TRggDrawingD03.Compute;
var
  fs: string;
begin
  B.Compute;
  C.Compute;

  fs := '%s = %s - (%.2f, %.2f)';
  LabelB.Text := Format(fs, [B.Caption, B.SchnittKK.Bemerkung, B.Center.X, B.Center.Y]);
  LabelC.Text := Format(fs, [C.Caption, C.SchnittKK.Bemerkung, C.Center.X, C.Center.Y]);
  LabelPhi.Text := Format('%s = %.2f', [Phi.Caption, Phi.SweepAngle]);
  LabelPsi.Text := Format('%s = %.2f', [Psi.Caption, Psi.SweepAngle]);

  UpdateKoppelkurve;
end;

constructor TRggDrawingD03.Create;
var
  L: TRggLine;
  Temp: TRggCircle;
begin
  inherited;
  Count := 51;

  SchnittKK := TSchnittKK.Create;

  Name := 'D03-Viergelenk';
  WantSort := False;

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := TRggColors.Orangered;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := TRggColors.Blue;

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Orangered;

  B := TSchnittKKCircleLL.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Blue;

  C := TSchnittKKCircleLL.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  A0B0 := TRggLine.Create('A0B0');
  L := A0B0;
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  A0A := TRggRotaLine.Create('A0A');
  L := A0A;
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);

  B0B := TRggLine.Create('B0B');
  L := B0B;
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  AB := TRggLine.Create('AB');
  L := AB;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  AC := TRggLine.Create('AC');
  L := AC;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  BC := TRggLine.Create('BC');
  L := BC;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  LabelB := TRggLabel.Create;
  LabelB.Caption := 'B';
  Add(LabelB);

  LabelC := TRggLabel.Create;
  LabelC.Caption := 'C';
  LabelC.Position.Y := LabelB.Position.Y + 30;
  Add(LabelC);

  LabelPhi := TRggLabel.Create;
  LabelPhi.Caption := 'Phi';
  LabelPhi.Position.Y := LabelC.Position.Y + 30;
  Add(LabelPhi);

  LabelPsi := TRggLabel.Create;
  LabelPsi.Caption := 'Psi';
  LabelPsi.Position.Y := LabelPhi.Position.Y + 30;
  Add(LabelPsi);

  Add(A0);
  Add(B0);
  Add(A);
  Add(B);
  Add(C);

  B.L1 := AB;
  B.L2 := B0B;
  B.InitRadius;

  C.L1 := AC;
  C.L2 := BC;
  C.InitRadius;

  KK := TRggPolyLine.Create('KK', Count);
  KK.StrokeThickness := 2;
  KK.StrokeColor := TRggColors.Orangered;
  Add(KK);

  Temp := TRggCircle.Create;
  Temp.Caption := 'KK[0]';
  Temp.Center.C := TPoint3D.Create(200, 100, 0);
  KK.Point1 := Temp;
  Add(Temp);

  Temp := TRggCircle.Create;
  Temp.Caption := Format('KK[%d]', [Count]);
  Temp.Center.C := TPoint3D.Create(300, 100, 0);
  KK.Point2 := Temp;
  Add(Temp);

  Phi := TRggArc.Create('Phi');
  Phi.Point1 := A0;
  Phi.Point2 := B0;
  Phi.Point3 := A;
  Add(Phi);

  Psi := TRggArc.Create('pi-Psi');
  Psi.Point1 := B0;
  Psi.Point2 := B;
  Psi.Point3 := A0;
  Add(Psi);

  DefaultElement := A0A;
end;

destructor TRggDrawingD03.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

procedure TRggDrawingD03.UpdateKoppelkurve;
{ Koppelkurve Viergelenk A0, A, B, B0 }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;
  oooTemp: TRiggPoints;

  FrAlpha: single;

  FrBasis: single;
  FrWunten2D: single;
  FrMastUnten: single;
  FrSalingH: single;
  FrWoben2D: single;
  FrMastOben: single;
begin
  FrBasis := A0B0.LineLength;
  FrWunten2D := A0A.LineLength;
  FrMastUnten := B0B.LineLength;
  FrSalingH := AB.LineLength;
  FrWoben2D := AC.LineLength;
  FrMastOben := BC.LineLength;

  rP.A0 := A0.Center.C;
  rP.B0 := B0.Center.C;
  rP.A := A.Center.C;
  rP.B := B.Center.C;
  rP.C := C.Center.C;

  oooTemp := rP; { aktuelle Koordinaten sichern }

  FrAlpha := arctan2((rP.P0.Y - rP.D0.Y), (rP.P0.X - rP.D0.X));

  { 1. Startwinkel }
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := FrWunten2D + FrSalingH;
  SchnittKK.Radius2 := FrMastUnten;
  SchnittKK.MittelPunkt1 := rP.A0;
  SchnittKK.MittelPunkt2 := rP.B0;
  ooTemp := SchnittKK.SchnittPunkt2;
  phiA := arctan2((rP.A0.X - ooTemp.X), (rP.A0.Y - ooTemp.Y));
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := FrWunten2D;
  SchnittKK.Radius2 := FrSalingH + FrMastUnten;
  SchnittKK.MittelPunkt1 := rP.A0;
  SchnittKK.MittelPunkt2 := rP.B0;
  ooTemp := SchnittKK.SchnittPunkt2;
  if SchnittKK.Status = bmK1inK2 then
    phiE := FrAlpha + DegToRad(130)
  else
  begin
    phiE := arctan2((rP.A0.X - ooTemp.X), (rP.A0.Y - ooTemp.Y));
    phiE := phiE + pi / 2 + FrAlpha;
  end;

  { 3. Koppelkurve }
  phiA := phiA + DegToRad(1);
  phiE := phiE - DegToRad(1);
  WinkelStep := (phiE - phiA) / (Count-1);
  phiM := phiA;
  for i := 0 to Count-1 do
  begin
    psiM := PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP.A.X := rP.A0.X + FrWunten2D * cos(phiM - FrAlpha);
    rP.A.Y := rP.A0.Y - FrWunten2D * sin(phiM - FrAlpha);
    rP.B.X := rP.B0.X + FrMastUnten * cos(psiM - FrAlpha);
    rP.B.Y := rP.B0.Y - FrMastUnten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    SchnittKK.SchnittEbene := seXY;
    SchnittKK.Radius1 := FrWoben2D;
    SchnittKK.Radius2 := FrMastOben;
    SchnittKK.MittelPunkt1 := rP.A;
    SchnittKK.MittelPunkt2 := rP.B;
    rP.C := SchnittKK.SchnittPunkt2;
    KK.Poly[i].X := rP.C.X;
    KK.Poly[i].Y := rP.C.Y;
    phiM := phiM + WinkelStep;
  end;

  rP := oooTemp; { aktuelle Koordinaten wiederherstellen }

  A0.Center.C := rP.A0;
  B0.Center.C := rP.B0;
  A.Center.C := rP.A;
  B.Center.C := rP.B;
  C.Center.C := rP.C;

  KK.Point1.Center.X := KK.Poly[0].X;
  KK.Point1.Center.Y := KK.Poly[0].Y;

  KK.Point2.Center.X := KK.Poly[Count-1].X;
  KK.Point2.Center.Y := KK.Poly[Count-1].Y;

  KK.ShowPoly := True;
end;

end.
