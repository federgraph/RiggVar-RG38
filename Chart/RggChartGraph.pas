unit RggChartGraph;

interface

uses
  RiggVar.FD.Image,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Objects,
  RggChart;

type
  TRggBox = class
  public
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TChartGraph = class(TChartModel)
  private
    FImage: TOriginalImage; // injected, not owned
    procedure InitBitmap;
    procedure SetImage(const Value: TOriginalImage);
  private
    Box: TRggBox;
    Raster: Integer;
    Padding: Integer;
  private
    procedure DrawToCanvas(g: TCanvas);
    procedure DrawChart(g: TCanvas);
    procedure DrawLabels(g: TCanvas);
    procedure DrawLegend(g: TCanvas);
  public
    Width: Integer;
    Height: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure ImageScreenScaleChanged(Sender: TObject);
    property Image: TOriginalImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main,
  RggTypes;

{ TChartGraph }

constructor TChartGraph.Create;
begin
  inherited;

  Width := 650;
  Height := 400;

  Box := TRggBox.Create;
  Box.X := 120;
  Box.Y := 80;
  Box.Width := 500;
  Box.Height := 300;

  Raster := 24;
  Padding := 2;

  WantRectangles := True;
  WantTextRect := False;
  WantLegend := True;
end;

destructor TChartGraph.Destroy;
begin
  Box.Free;
  inherited;
end;

procedure TChartGraph.InitBitmap;
begin
  Image.Width := Width;
  Image.Height := Height;
end;

procedure TChartGraph.SetImage(const Value: TOriginalImage);
begin
  FImage := Value;
  InitBitmap;
  Image.OnScreenScaleChanged := ImageScreenScaleChanged;
end;

procedure TChartGraph.ImageScreenScaleChanged(Sender: TObject);
begin
  Draw;
end;

procedure TChartGraph.Draw;
begin
  if (Image <> nil) then
  begin
    DrawToCanvas(Image.Bitmap.Canvas);
  end;
end;

procedure TChartGraph.DrawToCanvas(g: TCanvas);
var
  ss: single;
begin
  ss := Image.Scene.GetSceneScale;
  if g.BeginScene then
  try
    g.SetMatrix(TMatrix.CreateScaling(ss, ss));
    g.Clear(claNull);
    DrawLegend(g);
    DrawChart(g);
  finally
    g.EndScene;
  end;
end;

procedure TChartGraph.DrawChart(g: TCanvas);
var
  LineToPoint: TPointF;
  P: TPoint;
  i, param: Integer;
  Radius: Integer;
  tempX, tempY: single;
  WantChartPunktX: Boolean;

  function Limit(a: single): single;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

  procedure LineTo(x2, y2: single);
  begin
    g.DrawLine(LineToPoint, PointF(x2, y2), 0.5);
    LineToPoint := PointF(x2, y2);
  end;

  procedure DrawVerticalLine;
  begin
    P.X := Round(Box.X + Limit(tempX));
    P.Y := Round(Box.Y + Limit(tempY));
    LineToPoint := PointF(P.X, P.Y);
    P.Y := Box.Y;
    LineTo(P.X, P.Y);
  end;

begin
  DrawLabels(g);

  g.Stroke.Thickness := 1;

  { ChartPunktX }
  WantChartPunktX := True;
  if WantChartPunktX then
  begin
    g.Stroke.Color := claRed;
    tempX := Box.Width * ((ChartPunktX) - Xmin) / (XMax - Xmin);
    tempY := Box.Height;
    DrawVerticalLine;

    g.Stroke.Color := claSilver;
    tempX := Box.Width * ((ChartPunktX-APWidth) - Xmin) / (XMax - Xmin);
    DrawVerticalLine;

    tempX := Box.Width * ((ChartPunktX+APWidth) - Xmin) / (XMax - Xmin);
    DrawVerticalLine;
  end;

  Radius := 3;

  for param := 0 to ParamCount-1 do
  begin
    { Kurve }
    g.Stroke.Color := cf[param];
    tempY := Box.Height - Box.Height * (bf[param, 0] - Ymin) / (Ymax - Ymin);
    P.X := Box.X;
    P.Y := Box.Y + Round(Limit(tempY));
    LineToPoint := PointF(P.X, P.Y);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * (i / LNr);
      tempY := Box.Height - Box.Height * (bf[param, i] - Ymin) / (Ymax - Ymin);
      P.X := Box.X + Round(Limit(tempX));
      P.Y := Box.Y + Round(Limit(tempY));
      LineTo(P.X, P.Y);
    end;

    if WantRectangles then
    begin
      { Rechtecke }
      g.Stroke.Thickness := 1.0;
      g.Stroke.Color := claWhite;
      g.Fill.Color := cf[param];
      for i := 0 to LNr do
      begin
        tempX := Box.Width * i / LNr;
        tempY := Box.Height - Box.Height * (bf[param, i] - Ymin) / (Ymax - Ymin);
        P.X := Box.X + Round(Limit(tempX));
        P.Y := Box.Y + Round(Limit(tempY));
        g.FillRect(
          RectF(P.x - Radius, P.y - Radius,
                P.x + Radius, P.y + Radius), 0, 0, [], 1.0);
      end;
    end;

  end;
end;

procedure TChartGraph.DrawLabels(g: TCanvas);
var
  PosX: single;
  PosY: single;
  R: TRectF;
  s: string;

  w: single;
  h: single;

  procedure TextRect(s: string; ha, va: TTextAlign);
  begin
    R := RectF(PosX, PosY, PosX + w, PosY + h);
    if WantTextRect then
      g.DrawRect(R, 0, 0, [], 1.0);
    g.FillText(
      R,
      s,
      false, // WordWrap
      1.0, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

begin
  if not WantLegend then
    Exit;

  g.Stroke.Thickness := 0.5;
  g.Stroke.Color := claYellow;
  g.Fill.Color := claSilver;
  g.Font.Family := 'Consolas';
  g.Font.Size := 16;

  w := 290;
  h := 25;

  { Column 1 }
  PosX := Padding;

  { Column 1, Row 1 }
  PosY := Padding;
  s := Format('Xmin..Xmax = %.1f .. %.1f', [Xmin, Xmax]);
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  { Column 1, Row 2 }
  PosY := PosY + Raster;
  s := Format('Ymin..Ymax = %.1f .. %.1f', [Ymin, Ymax]);
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  { Column 1, Row 3 }
  PosX := 20;
  PosY := PosY + Raster;
  s := 'Parameter';
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  { Column 2 }
  PosX := 300;

  { Column 2, Row 1 }
  PosY := Padding;
  s := XTitle;
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  { Column 2, Row 2 }
  PosY := PosY + Raster;
  s := YTitle;
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  { Column 2, Row 3 }
  if ParamCount > 1 then
  begin
    PosX := PosX + 15;
    PosY := PosY + Raster;
    s := PTitle;
    TextRect(s, TTextAlign.Leading, TTextAlign.Leading);
  end;
end;

procedure TChartGraph.DrawLegend(g: TCanvas);
var
  p, PosX, PosY: Integer;
  R: TRectF;
  s: string;

  procedure TextOut(x, y: single; const s: string; ha, va: TTextAlign);
  begin
    R := RectF(x, y, x + 200, y + 20);
    if WantTextRect then
      g.DrawRect(R, 0, 0, [], 1.0);
    g.FillText(
      R,
      s,
      false, // WordWrap
      1.0, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

var
  bw, bh: Integer;
begin
  if not WantLegend then
    Exit;

  if ParamCount < 2 then
    Exit;

  bw := 16;
  bh := 3;

  { continue in Colum  1, Row 4 }
  PosX := 20;
  PosY := 3 * Raster + 10;
  g.Fill.Color := claSilver;
  g.Font.Family := 'Consolas';
  g.Font.Size := 16;
  for p := 0 to ParamCount-1 do
  begin
    { Bullet }
    g.Stroke.Thickness := 1.0;
    g.Stroke.Color := claWhite;
    g.Fill.Color := cf[p];
    g.FillRect(RectF(PosX, PosY, PosX + bw, PosY + bh), 0, 0, [], 1.0);

    { Text }
    g.Fill.Color := claSilver;
    PosY := PosY + 16;
    if Valid then
      s := PText[p]
    else
      s := PColorText[p];
    TextOut(PosX, PosY, s, TTextAlign.Leading, TTextAlign.Leading);
    PosY := PosY + 30;
  end;
end;

end.
