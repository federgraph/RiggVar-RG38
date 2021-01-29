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

    HT: TRggLabel;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ20 }

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

  ParamA.Scale := 0.05;
  ParamA.BaseValue := 2;

  ParamR.Scale := 0.1;
  ParamR.BaseValue := 10;
end;

constructor TRggDrawingZ20.Create;
begin
  inherited;
  Name := 'Z20-Epsilon';
  Raster := 25;

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  HT.Position.Y := 200;
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

  { Other Elements }

  Bem := TRggLabel.Create;
  Bem.Caption := 'Bemerkung';
  Bem.Text := 'Bemerkung';
  Bem.Position.X := 1 * Raster;
  Bem.Position.X := 50;
  Bem.StrokeColor := TRggColors.Tomato;

  Add(M1);
  Add(M2);
  Add(S1);
  Add(S2);

  Add(Bem);

  ParamA := TRggParam.Create;
  ParamA.Caption := 'Abstand';
  ParamA.StrokeColor := TRggColors.Teal;
  ParamA.StartPoint.Y := 3 * Raster;
  Add(ParamA);

  ParamR := TRggParam.Create;
  ParamR.Caption := 'Range';
  ParamR.StrokeColor := TRggColors.Teal;
  ParamR.StartPoint.Y := 5 * Raster;
  Add(ParamR);

  IntervalCount := 51;

  ChartX := TRggChart.Create(IntervalCount);
  ChartX.Caption := 'SP.X';
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
  ChartY.Caption := 'SP.Y';
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

  DefaultElement := ParamA;
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
  SKK.Radius1 := ParamR.RelativeValue;
  SKK.Radius2 := SKK.Radius1;

  Range := ParamR.RelativeValue;
  StartValue := -Range * 0.5;
  P.X := StartValue;
  P.Y := ParamA.RelativeValue;
  P.Z := 0;

  {
    A Chart has IntervalCount + 1 points.

    At minimum we need one interval, or at least two points,
      a start point and an end point.

    If x is in 0..2 and IntervalWidth = 1 then 3 samples are taken at 0, 1, 2.

    For x in -2..2 and IntervalWidth = 1, 5 samples will be taken at -2, -1, 0, 1, 2.
      We need 5 = 2 * 2 + 1 samples.
      We need 5 - 1 = 4 Intervals. --> TRggChart.Create(4);
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

  ParamA.Text := Format('Param A = %.2f', [ParamA.RelativeValue]);
  ParamR.Text := Format('Param R = %.2f', [ParamR.RelativeValue]);
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
  ML.Add('    Examine code at GitHub:');
  ML.Add('');
  ML.Add('see github.com/federgraph/');
  ML.Add('  repository RiggVar-RG38');
  ML.Add('  or repository documentation-drawings');
  ML.Add('    drawings are in folder FZ');
  result := ML.Text;
  ML.Clear;
end;

end.
