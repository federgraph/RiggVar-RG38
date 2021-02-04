unit RiggVar.FZ.Z20_Epsilon;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Math,
  System.Math.Vectors,
  RiggVar.RG.Calc,
  RiggVar.FB.Color,
  RiggVar.FD.Chart,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ20 = class(TRggDrawingKK)
  private
    IntervalCount: Integer;
    Raster: single;
    function GetHelpText: string;
    procedure BtnAClick(Sender: TObject);
    procedure BtnBClick(Sender: TObject);
    procedure BtnEClick(Sender: TObject);
    procedure UpdateText;
  public
    M1: TRggCircle;
    M2: TRggCircle;
    S1: TRggCircle;
    S2: TRggCircle;
    Bem: TRggLabel;

    ParamA: TRggParam;
    ParamR: TRggParam;

    ChartX: TRggChart;
    ChartY: TRggChart;
    Epsilon1: TRggLabel;
    Epsilon2: TRggLabel;

    HT: TRggLabel;

    WantConstantRadius: Boolean;
    WantNegativeStartValue: Boolean;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
  end;

implementation

{ TRggDrawingZ20 }

procedure TRggDrawingZ20.BtnAClick(Sender: TObject);
begin
  WantConstantRadius := not WantConstantRadius;
  ML.Text := 'A';
  UpdateText;
  UpdateDrawing;
end;

procedure TRggDrawingZ20.BtnBClick(Sender: TObject);
begin
  WantNegativeStartValue := not WantNegativeStartValue;
  ML.Text := 'B';
  UpdateText;
  UpdateDrawing;
end;

procedure TRggDrawingZ20.BtnEClick(Sender: TObject);
begin
  ParamA.ParamValue := 0;
  ParamR.ParamValue := ParamR.BaseValue;
//  ParamR.PixelValue := ParamR.OriginValue;

  ML.Text := 'E';
  UpdateText;
  UpdateDrawing;
end;

procedure TRggDrawingZ20.UpdateText;
begin
  if WantConstantRadius then
    ML.Add('ConstantRadius = True')
  else
    ML.Add('ConstantRadius = False');

  if WantNegativeStartValue then
    ML.Add('NegativeStartValue = True')
  else
    ML.Add('NegativeStartValue = False');
end;

procedure TRggDrawingZ20.InitButtons(BG: TRggButtonGroup);
begin
  inherited;
  BG.BtnA.OnClick := BtnAClick;
  BG.BtnB.OnClick := BtnBClick;
  BG.BtnE.OnClick := BtnEClick;

  { ToDo: Find better short names for button actions. }
  BG.BtnA.Text := 'A*';
  BG.BtnB.Text := 'B*';
  BG.BtnE.Text := 'E*';
end;

procedure TRggDrawingZ20.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 200;
  oy := 500;

  M1.Center.X := ox;
  M1.Center.Y := oy;
  M1.Center.Z := 0;

  M2.Center.X := ox + 400;
  M2.Center.Y := oy;
  M2.Center.Z := 0;

  ParamR.ParamValue := ParamR.BaseValue;
  ParamA.ParamValue := ParamA.BaseValue;
end;

constructor TRggDrawingZ20.Create;
begin
  inherited;
  Name := 'Z20-Epsilon';
  Raster := 25;

  WantConstantRadius := False;
  WantNegativeStartValue := True;

  { Labels and Params }

  Bem := TRggLabel.Create;
  Bem.Caption := 'Bemerkung';
  Bem.Text := 'Bemerkung';
  Bem.Position.X := 2 * Raster;
  Bem.Position.Y := 1 * Raster;
  Add(Bem);

  Epsilon1 := TRggLabel.Create;
  Epsilon1.Caption := 'Epsilon1';
  Epsilon1.Text := 'Epsilon1';
  Epsilon1.Position.X := 2 * Raster;
  Epsilon1.Position.Y := 2 * Raster;
  Add(Epsilon1);

  Epsilon2 := TRggLabel.Create;
  Epsilon2.Caption := 'Epsilon2';
  Epsilon2.Text := 'Epsilon2';
  Epsilon2.Position.X := 2 * Raster;
  Epsilon2.Position.Y := 3 * Raster;
  Add(Epsilon2);

  ParamA := TRggParam.Create;
  ParamA.Caption := 'Abstand';
  ParamA.StrokeColor := TRggColors.Teal;
  ParamA.StartPoint.Y := 5 * Raster;
  ParamA.Scale := 0.05;
  ParamA.BaseValue := 2;
  Add(ParamA);

  ParamR := TRggParam.Create;
  ParamR.Caption := 'Range';
  ParamR.StrokeColor := TRggColors.Teal;
  ParamR.StartPoint.Y := 7 * Raster;
  ParamR.Scale := 0.1;
  ParamR.BaseValue := 10;
  Add(ParamR);

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.IsMemoLabel := True;
  HT.Position.Y := 11 * Raster;
  Add(HT);

  { Points }

  M1 := TRggCircle.Create('M1');
  M1.StrokeColor := TRggColors.Red;

  M2 := TRggCircle.Create('M2');
  M2.StrokeColor := TRggColors.Blue;

  S1 := TRggCircle.Create('S1');
  S1.StrokeColor := TRggColors.Yellow;
  S1.IsComputed := True;

  S2 := TRggCircle.Create('S2');
  S2.StrokeColor := TRggColors.Lime;
  S2.IsComputed := True;

  Add(M1);
  Add(M2);
  Add(S1);
  Add(S2);

  { Charts }

  IntervalCount := 51;

  ChartX := TRggChart.Create(IntervalCount);
  ChartX.Caption := 'SP.X Blue';
  ChartX.StrokeThickness := 1;
  ChartX.StrokeColor := TRggColors.Dodgerblue;
  ChartX.InitDefault;
  ChartX.Box.X := 100;
  ChartX.Box.Y := 200;
  ChartX.Box.Width := 600;
  ChartX.Box.Height := 400;
  ChartX.WantRectangles := True;
  ChartX.WantCurve := True;
  Add(ChartX);

  ChartY := TRggChart.Create(IntervalCount);
  ChartY.Caption := 'SP.Y Red';
  ChartY.StrokeThickness := 1;
  ChartY.StrokeColor := TRggColors.Tomato;
  ChartY.InitDefault;
  ChartY.Box.X := 100;
  ChartY.Box.Y := 200;
  ChartY.Box.Width := 600;
  ChartY.Box.Height := 400;
  ChartY.WantRectangles := True;
  ChartY.WantCurve := True;
  Add(ChartY);

  InitDefaultPos;

  FixPoint3D := M1.Center.C;
  WantRotation := False;
  WantSort := False;
  WantMemoLines := True;

  DefaultElement := ParamA;

  UpdateText;
end;

procedure TRggDrawingZ20.Compute;
var
  i: Integer;
  k: single;
  P: TPoint3D;
  Range: single;
  StartValue: single;
begin
  { update the visible intersection Circles, and the Label }

  SKK.SchnittEbene := seXY;
  SKK.Radius1 := 300;
  SKK.Radius2 := 300;
  SKK.MittelPunkt1 := M1.Center.C;
  SKK.MittelPunkt2 := M2.Center.C;
  S1.Center.C := SKK.SchnittPunkt1;
  S2.Center.C := SKK.SchnittPunkt2;

  Bem.Text := 'Bemerkung: ' + SKK.Bemerkung;

  { now reuse SKK to do the Chart }

  SKK.MittelPunkt1 := TPoint3D.Zero;
  SKK.SchnittEbene := seXY;

  if WantConstantRadius then
    SKK.Radius1 := 300
  else
  SKK.Radius1 := ParamR.ParamValue;

  SKK.Radius2 := SKK.Radius1;

  Range := ParamR.ParamValue;

  if WantNegativeStartValue then
    StartValue := -Range * 0.5
  else
    StartValue := 0;

  P.X := StartValue;
  P.Y := ParamA.ParamValue;
  P.Z := 0;

  {
    A Chart has IntervalCount + 1 points.

    At minimum we need one interval, or at least two points,
      a start point and an end point.

    If x is in 0..2 and IntervalWidth = 1 then 3 samples are taken at 0, 1, 2.

    For x in -2..2 and IntervalWidth = 1:
      5 samples will be taken at -2, -1, 0, 1, 2.
      We need 5 = 2 * 2 + 1 samples. (+1 being the one at zero).
      We need 5 = end - start + 1 = 2 -(-2) + 1 samples.
      We need 5 - 1 = 4 Intervals. --> TRggChart.Create(4);

    We often specify 10, 20, 50, or 100 as IntervalCount.
  }
  Assert(IntervalCount > 0);
  Assert(IntervalCount = ChartX.IntervalCount);
  Assert(IntervalCount = ChartY.IntervalCount);
  k := Range / IntervalCount;
  for i := 0 to IntervalCount do
  begin
    P.X := StartValue + i * k;
    SKK.MittelPunkt2 := P;
    ChartX.Poly[i] := SKK.SchnittPunkt1.X;
    ChartY.Poly[i] := SKK.SchnittPunkt1.Y;
  end;
  ChartX.LookForYMinMax;
  ChartY.LookForYMinMax;

  i := 1;
  P.X := ChartX.Poly[i];
  P.Y := ParamA.ParamValue;
  Epsilon1.Text := Format('Epsilon1 = %9.4f; // P%.2d := (%7.4f, %7.4f)', [P.Length, i, P.X, P.Y]);

  i := IntervalCount div 2;
  P.X := ChartX.Poly[i];
  P.Y := ParamA.ParamValue;
  Epsilon2.Text := Format('Epsilon2 = %9.4f; // P%.2d := (%7.4f, %7.4f)', [P.Length, i, P.X, P.Y]);

  ParamA.Text := Format('Param A = %.2f', [ParamA.ParamValue]);
  ParamR.Text := Format('Param R = %.2f', [ParamR.ParamValue]);
end;

function TRggDrawingZ20.GetHelpText: string;
begin
  ML.Add('Epsilon Test');
  ML.Add('  A manual test for TSchnittKK class.');
  ML.Add('  SchnittKK = Intersection Circle Circle');
  ML.Add('  SchnittKK = Schnitt Kreis Kreis');
  ML.Add('');
  ML.Add('Select a Parameter element.');
  ML.Add('  Scroll the wheel.');
  ML.Add('');
  ML.Add('see github.com/federgraph/');
  ML.Add('  repository RiggVar-RG38 or');
  ML.Add('  repository documentation-drawings');
  ML.Add('    drawings are in folder FZ');
  result := ML.Text;
  ML.Clear;
end;

end.
