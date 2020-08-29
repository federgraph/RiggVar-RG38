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
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TRggCircle;

    A0B0: TRggLine;

    Chart: TRggChart;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ19 }

procedure TRggDrawingZ19.InitDefaultPos;
begin
  A0.Center.X := 100;
  A0.Center.Y := 400;
  A0.Center.Z := 0;

  B0.Center.X := 400;
  B0.Center.Y := 400;
  B0.Center.Z := 0;

  A.Center.X := 100;
  A.Center.Y := 100;
  A.Center.Z := 0;

  B.Center.X := 400;
  B.Center.Y := 100;
  B.Center.Z := 0;
end;

constructor TRggDrawingZ19.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z19-Chart';
  WantSort := False;

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := claOrangered;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := claBlue;

  A := TRggCircle.Create('A');
  A.StrokeColor := claOrangered;

  B := TRggCircle.Create('B');
  B.StrokeColor := claBlue;

  InitDefaultPos;

  A0B0 := TRggLine.Create('A0B0');
  L := A0B0;
  L.StrokeColor := claGray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create('A0A');
  L.StrokeColor := claRed;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);

  L := TRggLine.Create('B0B');
  L.StrokeColor := claBlue;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AB');
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

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
  Add(Chart);

  Add(A0);
  Add(B0);
  Add(A);
  Add(B);
end;

procedure TRggDrawingZ19.Compute;
begin
  Chart.Box.X := Round(A.Center.X);
  Chart.Box.Y := Round(A.Center.Y);

  Chart.Box.Width := Round(A0B0.LineLength);
  Chart.Box.Height := Round((B0.Center.C - B.Center.C).Y);
end;

end.
