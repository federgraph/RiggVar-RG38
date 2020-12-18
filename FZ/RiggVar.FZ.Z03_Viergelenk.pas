unit RiggVar.FZ.Z03_Viergelenk;

interface

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  RiggVar.RG.Types,
  RiggVar.RG.Calc,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ03 = class(TRggDrawing)
  private
    Count: Integer;
    ox: single;
    oy: single;
    SchnittKK: TSchnittKK;
    rP: TRiggPoints;
    procedure UpdateKoppelkurve;
    function GetHelpText: string;
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TSchnittKKCircle;
    C: TSchnittKKCircle;

    A0B0: TRggLine;
    A0A: TRggLine;

    LabelB: TRggLabel;
    LabelC: TRggLabel;
    LabelPhi: TRggLabel;

    KK: TRggPolyCurve;

    Phi: TRggArc;

    HT: TRggLabel;

    constructor Create;
    destructor Destroy; override;

    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ03 }

procedure TRggDrawingZ03.InitDefaultPos;
begin
  ox := 100;
  oy := 530;

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
  B.InitRadius;

  C.Center.X := ox + 300;
  C.Center.Y := oy - 400;
  C.Center.Z := 0;
  C.InitRadius;
end;

procedure TRggDrawingZ03.Compute;
var
  fs: string;
begin
  B.Compute;
  C.Compute;

  fs := '%s = %s - (%.2f, %.2f)';
  LabelB.Text := Format(fs, [B.Caption, B.Bemerkung, B.Center.X, B.Center.Y]);
  LabelC.Text := Format(fs, [C.Caption, C.Bemerkung, C.Center.X, C.Center.Y]);
  LabelPhi.Text := Format('%s = %.2f', [Phi.Caption, Phi.SweepAngle]);

  UpdateKoppelkurve;
end;

constructor TRggDrawingZ03.Create;
var
  L: TRggLine;
begin
  inherited;
  Count := 51;

  SchnittKK := TSchnittKK.Create;

  Name := 'Z03-Viergelenk';
  WantSort := False;

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  HT.Position.Y := 600;
  Add(HT);

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := TRggColors.Orangered;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := TRggColors.Blue;

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Orangered;

  B := TSchnittKKCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Blue;
  B.MP1 := A;
  B.MP2 := B0;

  C := TSchnittKKCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Lime;
  C.MP1 := A;
  C.MP2 := B;

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
  A0A := L;

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

  Add(C);
  Add(B);
  Add(A);
  Add(B0);
  Add(A0);

  KK := TRggPolyCurve.Create('KK', Count);
  KK.Caption := 'KK';
  KK.StrokeThickness := 3;
  KK.StrokeColor := TRggColors.Yellow;
  KK.Opacity := 1.0;
  Add(KK);

  Phi := TRggArc.Create('Phi');
  Phi.Point1 := A0;
  Phi.Point2 := B0;
  Phi.Point3 := A;
  Add(Phi);

  WantSort := False;
  DefaultElement := A0A;
end;

destructor TRggDrawingZ03.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

procedure TRggDrawingZ03.UpdateKoppelkurve;
{ Koppelkurve: Bahn von Punkt C im Viergelenk A0, A, B, B0 }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;

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
  FrMastUnten := B.L2;
  FrSalingH := B.L1;
  FrWoben2D := C.L1;
  FrMastOben := C.L2;

  rP.A0 := A0.Center.C;
  rP.B0 := B0.Center.C;
  rP.A := A.Center.C;
  rP.B := B.Center.C;
  rP.C := C.Center.C;

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
    psiM := TRggCalc.PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP.A.X := rP.A0.X + FrWunten2D * cos(phiM - FrAlpha);
    rP.A.Y := rP.A0.Y - FrWunten2D * sin(phiM - FrAlpha);
    rP.B.X := rP.B0.X + FrMastUnten * cos(psiM - FrAlpha);
    rP.B.Y := rP.B0.Y - FrMastUnten * sin(psiM - FrAlpha);
    { Punkt C }
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
end;

function TRggDrawingZ03.GetHelpText: string;
begin
  ML.Add('Viergelenk A0ABB0 = mechanism A0 A B B0 = a four bar linkage?');
  ML.Add('');
  ML.Add('Line Element AB is called "Koppel".');
  ML.Add('  Points A, B and C are located in a plain, the "Koppelebene".');
  ML.Add('');
  ML.Add('Yellow curve is called "Koppelkurve".');
  ML.Add('  Point C will move along the Koppelkurve if you change Phi.');
  ML.Add('    Change angle phi via element RotaLine A0A.');
  result := ML.Text;
  ML.Clear;
end;

end.
