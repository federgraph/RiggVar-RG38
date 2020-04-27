﻿unit RggChartGraph;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
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

  TChartGraph = class(TChartForm)
  private
    FImage: TImage; // injected, not owned
    FBitmap: TBitmap; // owned, created in InitBitmap
    procedure InitBitmap;
    procedure SetImage(const Value: TImage);
  private
    Box: TRggBox;
    Raster: Integer;
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
    property Image: TImage read FImage write SetImage;
  end;

implementation

uses
  RggTypes;

{ TChartGraph }

constructor TChartGraph.Create;
begin
  inherited;

  Width := 800;
  Height := 600;

  Box := TRggBox.Create;
  Box.X := 120;
  Box.Y := 80;
  Box.Width := 500;
  Box.Height := 300;

  Raster := 30;

  WantRectangles := False;
  WantTextRect := False;
  WantLegend := True;
end;

destructor TChartGraph.Destroy;
begin
  FBitmap.Free;
  Box.Free;
  inherited;
end;

procedure TChartGraph.InitBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Free;
  end;
  FBitmap := TBitmap.Create(Width, Height);
  Image.Bitmap := FBitmap;
  Image.WrapMode := TImageWrapMode.Original;
  Image.Width := Width;
  Image.Height := Height;
end;

procedure TChartGraph.SetImage(const Value: TImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TChartGraph.Draw;
begin
  if (Image <> nil) and (FBitmap <> nil) then
  begin
    DrawToCanvas(Image.Bitmap.Canvas);
  end;
end;

procedure TChartGraph.DrawToCanvas(g: TCanvas);
begin
  if g.BeginScene then
  try
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
  oy: single;

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    Result := a;
  end;

  procedure LineTo(x2, y2: single);
  begin
    g.DrawLine(LineToPoint, PointF(x2, oy + y2), 0.5);
    LineToPoint := PointF(x2, oy + y2);
  end;

var
  Pt: TPoint;
  i, p, Radius: Integer;
  tempX, tempY: double;
begin
  DrawLabels(g);

  oy := 0;
  Radius := 2;

  for p := 0 to ParamCount-1 do
  begin

    { Kurve }
    g.Stroke.Color := cf[p];
    tempY := Box.Height * (bf[p, 0] - Ymin) / (Ymax - Ymin);
    Pt.X := Box.X;
    Pt.y := Box.Y + Round(Limit(tempY));
    LineToPoint := PointF(Pt.X, Pt.Y);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * (i / LNr);
      tempY := Box.Height * (bf[p, i] - Ymin) / (Ymax - Ymin);
      Pt.x := Box.X + Round(Limit(tempX));
      Pt.y := Box.Y + Round(Limit(tempY));
      LineTo(Pt.x, Pt.y);
    end;

    if WantRectangles then
    begin
      { Rechtecke }
      g.Stroke.Thickness := 1.0;
      g.Stroke.Color := claWhite;
      g.Fill.Color := cf[p];
      for i := 0 to LNr do
      begin
        tempX := Box.Width * (i / LNr);
        tempY := Box.Height * (bf[p, i] - Ymin) / (Ymax - Ymin);
        Pt.x := Box.X + Round(Limit(tempX));
        Pt.y := Box.Y + Round(Limit(tempY));
        g.FillRect(
          RectF(Pt.x - Radius, Pt.y - Radius,
                Pt.x + Radius, Pt.y + Radius), 0, 0, [], 1.0);
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

  PosX := 0;
  PosY := 0;

  s := Format('Ymin..Ymax = %.1f .. %.1f', [Ymin, Ymax]);
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  PosY := PosY + Raster;
  s := Format('Xmin..Xmax = %.1f .. %.1f', [Xmin, Xmax]);
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  PosX := 300;
  PosY := 0;

  s := LeftTitle;
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  PosY := PosY + Raster;
  s := BottomTitle;
  TextRect(s, TTextAlign.Leading, TTextAlign.Leading);

  if ParamCount > 1 then
  begin
    PosY := PosY + Raster;
    s := RightTitle;
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

  PosX := 20;
  PosY := 2 * Raster + 10;
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