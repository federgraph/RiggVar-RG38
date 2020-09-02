unit RiggVar.FZ.Z19_Chart;

interface

uses
  System.UIConsts,
  RiggVar.FD.Chart,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ19 = class(TRggDrawing)
  public
    B0: TRggCircle;
    A: TRggCircle;

    Chart: TRggChart;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ19 }

procedure TRggDrawingZ19.InitDefaultPos;
begin
  B0.Center.X := 400;
  B0.Center.Y := 400;
  B0.Center.Z := 0;

  A.Center.X := 100;
  A.Center.Y := 100;
  A.Center.Z := 0;
end;

constructor TRggDrawingZ19.Create;
begin
  inherited;
  Name := 'Z19-Chart';
  WantSort := False;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := claBlue;

  A := TRggCircle.Create('A');
  A.StrokeColor := claOrangered;

  InitDefaultPos;

  Chart := TRggChart.Create;
  Chart.Caption := 'Test';
  Chart.StrokeThickness := 1.0;
  Chart.StrokeColor := claDodgerblue;
  Chart.InitDefault;
  Chart.Box.X := 250;
  Chart.Box.Y := 250;
  Chart.Box.Width := 400;
  Chart.Box.Height := 200;
  Chart.WantRectangles := True;
  Chart.WantCurve := True;

  Add(A);
  Add(B0);
  Add(Chart);

  DefaultElement := B0;
end;

procedure TRggDrawingZ19.Compute;
var
  t: TRggPoint3D;
begin
  t := B0.Center - A.Center;

  Chart.Box.X := Round(A.Center.X);
  Chart.Box.Y := Round(A.Center.Y);

  Chart.Box.Width := Round(t.X);
  Chart.Box.Height := Round(t.Y);
end;

end.
