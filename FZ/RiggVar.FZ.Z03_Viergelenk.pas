unit RiggVar.FZ.Z03_Viergelenk;

interface

uses
  System.SysUtils,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing03 = class(TRggDrawing)
  private
    Count: Integer;
    ox: single;
    oy: single;
    SchnittKK: TSchnittKK;
    rP: TRealRiggPoints;
    procedure UpdateKoppelkurve;
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TSchnittKKCircle;
    C: TSchnittKKCircle;

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

{ TRggDrawing03 }

procedure TRggDrawing03.InitDefaultPos;
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

procedure TRggDrawing03.Compute;
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

constructor TRggDrawing03.Create;
var
  L: TRggLine;
  Temp: TRggCircle;
begin
  inherited;
  Count := 51;

  SchnittKK := TSchnittKK.Create;

  Name := '03-Viergelenk';
  WantSort := False;

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := claOrangered;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := claBlue;

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := claOrangered;

  B := TSchnittKKCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := claBlue;

  C := TSchnittKKCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := claBlue;

  InitDefaultPos;

  A0B0 := TRggLine.Create;
  L := A0B0;
  L.Caption := 'A0B0';
  L.StrokeColor := claGray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  A0A := TRggRotaLine.Create;
  L := A0A;
  L.Caption := 'A0A';
  L.StrokeColor := claRed;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);
  A0A := L;

  B0B := TRggLine.Create;
  L := B0B;
  L.Caption := 'B0B';
  L.StrokeColor := claBlue;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  AB := TRggLine.Create;
  L := AB;
  L.Caption := 'AB';
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  AC := TRggLine.Create;
  L := AC;
  L.Caption := 'AC';
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  BC := TRggLine.Create;
  L := BC;
  L.Caption := 'BC';
  L.StrokeColor := claLime;
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

  KK := TRggPolyLine.Create;
  KK.Caption := 'KK';
  KK.StrokeThickness := 2.0;
  KK.StrokeColor := claOrangered;
  SetLength(KK.Poly, Count);
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

destructor TRggDrawing03.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

procedure TRggDrawing03.UpdateKoppelkurve;
{ Koppelkurve Viergelenk A0, A, B, B0 }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;
  oooTemp: TRealRiggPoints;

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

  rP[ooA0] := A0.Center.C;
  rP[ooB0] := B0.Center.C;
  rP[ooA] := A.Center.C;
  rP[ooB] := B.Center.C;
  rP[ooC] := C.Center.C;

  oooTemp := rP; { aktuelle Koordinaten sichern }

  FrAlpha := arctan2((rP[ooP0].Y - rP[ooD0].Y), (rP[ooP0].X - rP[ooD0].X));

//  Wanten3dTo2d;

  { 1. Startwinkel }
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := FrWunten2D + FrSalingH;
  SchnittKK.Radius2 := FrMastUnten;
  SchnittKK.MittelPunkt1 := rP[ooA0];
  SchnittKK.MittelPunkt2 := rP[ooB0];
  ooTemp := SchnittKK.SchnittPunkt2;
  phiA := arctan2((rP[ooA0].X - ooTemp.X), (rP[ooA0].Y - ooTemp.Y));
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := FrWunten2D;
  SchnittKK.Radius2 := FrSalingH + FrMastUnten;
  SchnittKK.MittelPunkt1 := rP[ooA0];
  SchnittKK.MittelPunkt2 := rP[ooB0];
  ooTemp := SchnittKK.SchnittPunkt2;
  if SchnittKK.Status = bmK1inK2 then
    phiE := FrAlpha + 130 * pi / 180
  else
  begin
    phiE := arctan2((rP[ooA0].X - ooTemp.X), (rP[ooA0].Y - ooTemp.Y));
    phiE := phiE + pi / 2 + FrAlpha;
  end;

//  FrAlpha := 0;
//  phiA := 25 * PI / 180;
//  phiE := 70 * PI / 180;

  { 3. Koppelkurve }
  phiA := phiA + 1 * PI / 180;
  phiE := phiE - 1 * PI / 180;
  WinkelStep := (phiE - phiA) / (Count-1);
  phiM := phiA;
  for i := 0 to Count-1 do
  begin
    psiM := PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP[ooA].X := rP[ooA0].X + FrWunten2D * cos(phiM - FrAlpha);
    rP[ooA].Y := rP[ooA0].Y - FrWunten2D * sin(phiM - FrAlpha);
    rP[ooB].X := rP[ooB0].X + FrMastUnten * cos(psiM - FrAlpha);
    rP[ooB].Y := rP[ooB0].Y - FrMastUnten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    SchnittKK.SchnittEbene := seXY;
    SchnittKK.Radius1 := FrWoben2D;
    SchnittKK.Radius2 := FrMastOben;
    SchnittKK.MittelPunkt1 := rP[ooA];
    SchnittKK.MittelPunkt2 := rP[ooB];
    rP[ooC] := SchnittKK.SchnittPunkt2;
    KK.Poly[i].X := rP[ooC].X;
    KK.Poly[i].Y := rP[ooC].Y;
    phiM := phiM + WinkelStep;
  end;

  rP := oooTemp; { aktuelle Koordinaten wiederherstellen }

  A0.Center.C := rP[ooA0];
  B0.Center.C := rP[ooB0];
  A.Center.C := rP[ooA];
  B.Center.C := rP[ooB];
  C.Center.C := rP[ooC];

  KK.Point1.Center.X := KK.Poly[0].X;
  KK.Point1.Center.Y := KK.Poly[0].Y;

  KK.Point2.Center.X := KK.Poly[Count-1].X;
  KK.Point2.Center.Y := KK.Poly[Count-1].Y;

  KK.ShowPoly := True;
end;

end.
