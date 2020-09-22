unit RggTrimmTabGraph;

interface

uses
  RiggVar.FD.Image,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects,
  RggTypes,
  RggTrimmTab;

type
  TTrimmTabGraph = class
  private
    { Fixed Width and Height }
    FWidth: Integer;
    FHeight: Integer;
    procedure ClearBackground(g: TCanvas);
  private
    FImage: TOriginalImage; // injected, not owned
    procedure InitBitmap;
    procedure SetImage(const Value: TOriginalImage);
  protected
    SavedMatrix: TMatrix;
    NewMatrix: TMatrix;
    TempMatrix: TMatrix;
    procedure BeginTransform(g: TCanvas);
    procedure EndTransform(g: TCanvas);
    procedure DrawGraph(g: TCanvas);
    procedure DrawText(g: TCanvas);
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  public
    Margin: single;
    Raster: single;
    BackgroundColor: TAlphaColor;
    ImageOpacity: single;
    Poly: TPolygon;
    PD: TPathData;
  public
    FScale: single;
    Model: TTrimmTabGraphModel;
    DrawCounter: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure DrawToCanvas(g: TCanvas);
    procedure Draw;
    property Image: TOriginalImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TTrimmTabGraph.Create;
begin
  FScale := Main.Scale;
  PD := TPathData.Create;
  SetLength(Poly, 101);
  Model := TTrimmTabGraphModel.Create;
  BackgroundColor := claWhite;
  FWidth := 319;
  FHeight := 158;
  Margin := 10;
  Raster := 20;
  ImageOpacity := 1.0;
end;

destructor TTrimmTabGraph.Destroy;
begin
  PD.Free;
  Model.Free;
  inherited;
end;


procedure TTrimmTabGraph.DrawText(g: TCanvas);
var
  R: TRectF;
  PosX, PosY: single;
  s: string;
  w: single;
  h: single;

  procedure TextOut(s: string;
    ha: TTextAlign = TTextAlign.Leading;
    va: TTextAlign = TTextAlign.Leading);
  begin
    R := RectF(PosX, PosY, PosX + w, PosY + h);
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
  w := 100;
  h := 18;

  g.Stroke.Thickness := 1.0;
  g.Stroke.Color := claNull;
  g.Fill.Color := claBlack;
  g.Font.Family := 'Consolas';
  g.Font.Size := 14;

  { Texte }
  PosX := Margin;
  PosY := Margin;
  TextOut('Kraft [N]');

  PosY := PosY + Raster;
  s := Format('(%d ... %d)', [0, Model.EndwertKraft]);
  TextOut(s);

  PosX := FWidth - Margin - w;
  PosY := FHeight - Margin - h;
  TextOut('Weg [mm]', TTextAlign.Trailing, TTextAlign.Trailing);

  PosY := PosY - Raster;
  s := Format('(%d ... %d)', [0, Model.EndwertWeg]);
  TextOut(s, TTextAlign.Trailing, TTextAlign.Trailing);
end;

procedure TTrimmTabGraph.DrawGraph(g: TCanvas);
var
  P: TPointF;
  R: TRectF;
  i: Integer;
  RadiusX, RadiusY: single;
  tempX, tempY: single;
  W, H: single;

  function Limit(a: double): double;
  begin
    if a < -32000 then
      a := -32000
    else if a > 32000 then
      a := 32000;
    result := a;
  end;

  procedure DrawR;
  begin
    g.FillRect(RectF(
      P.X - RadiusX,
      P.Y - RadiusY,
      P.X + RadiusX,
      P.Y + RadiusY), 0, 0, [], 1.0);
    g.DrawRect(RectF(
      P.X - RadiusX,
      P.Y - RadiusY,
      P.X + RadiusX,
      P.Y + RadiusY), 0, 0, [], 1.0);
  end;

begin
  W := FWidth;
  H := FHeight;

  { Radius }
  R.Left := 0;
  R.Top := 0;
  R.Bottom := Round(4 * FScale);
  R.Right := R.Bottom;
  RadiusX := R.Right - R.Left;
  RadiusY := R.Bottom - R.Top;

  { Kurve }
  g.Stroke.Thickness := 0.5;
  g.Stroke.Color := claBlue;
  case Model.TabellenTyp of
    itKonstante:
      begin
        tempY := H * (Model.x1 / Model.EndwertKraft);
        P.X := 0;
        P.Y := Limit(tempY);
        g.DrawLine(P, PointF(W, P.Y), 1.0);
      end;
    itGerade:
      begin
        g.DrawLine(Point(0, 0), PointF(W, H), 1.0);
      end;
    itParabel, itBezier:
      begin
        for i := 0 to 100 do
        begin
          Poly[i].X := Round(Limit(W * Model.LineDataX[i]));
          Poly[i].Y := Round(Limit(H * Model.LineDataY[i]));
        end;
        { g.DrawPolygon(Poly, 1.0); } // will close figure, not wanted
        PD.Clear;
        PD.MoveTo(Poly[0]);
        for i := 1 to Length(Poly) - 1 do
          PD.LineTo(Poly[i]);
        g.DrawPath(PD, 1.0);
      end;
  end;

  { Rechtecke }
  g.Stroke.Thickness := 1.0;
  g.Stroke.Color := claBlack;
  g.Fill.Color := claYellow;
  for i := 1 to Model.PunkteAnzahl do
  begin
    tempX := W * Model.Kurve[i].y / Model.EndwertWeg;
    tempY := H * Model.Kurve[i].x / Model.EndwertKraft;
    P.x := Round(Limit(tempX));
    P.y := Round(Limit(tempY));
    DrawR;
  end;

  g.Stroke.Color := claBlack;
  g.Fill.Color := claRed;

  if Model.TabellenTyp > itGerade then
  begin
    tempX := W * Model.y1 / Model.EndwertWeg;
    tempY := H * Model.x1 / Model.EndwertKraft;
    P.X := Round(Limit(tempX));
    P.y := Round(Limit(tempY));
    DrawR;
  end;

  P := Point(0, 0);
  DrawR;

  tempX := W * Model.y2 / Model.EndwertWeg;
  tempY := H * Model.x2 / Model.EndwertKraft;
  P.X := Round(Limit(tempX));
  P.Y := Round(Limit(tempY));
  DrawR;
end;

procedure TTrimmTabGraph.InitBitmap;
begin
  Image.Width := Width;
  Image.Height := Height;
end;

procedure TTrimmTabGraph.SetImage(const Value: TOriginalImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TTrimmTabGraph.BeginTransform(g: TCanvas);
var
  ScaleX: single;
  ScaleY: single;
  OriginX: single;
  OriginY: single;
begin
  OriginX := 0;
  OriginY := FHeight;

  ScaleX := 1.0;
  ScaleY := -1.0;

  SavedMatrix := g.Matrix;
  NewMatrix := TMatrix.Identity;

  TempMatrix := TMatrix.CreateScaling(ScaleX, ScaleY);
  NewMatrix := NewMatrix * TempMatrix;
  TempMatrix := TMatrix.CreateTranslation(OriginX, OriginY);
  NewMatrix := NewMatrix * TempMatrix;

  g.SetMatrix(NewMatrix);
end;

procedure TTrimmTabGraph.EndTransform(g: TCanvas);
begin
  g.SetMatrix(SavedMatrix);
end;

procedure TTrimmTabGraph.ClearBackground(g: TCanvas);
var
  R: TRectF;
begin
  g.Clear(claNull);
  if Image = nil then
  begin
    g.Clear(BackgroundColor);
  end
  else
  begin
    R := RectF(0, 0, Width, Height);
    g.Fill.Color := BackgroundColor;
    g.FillRect(R, 0, 0, [], ImageOpacity);
  end;
end;

procedure TTrimmTabGraph.DrawToCanvas(g: TCanvas);
begin
  Inc(DrawCounter);
  if g.BeginScene then
  try
    ClearBackground(g);
    BeginTransform(g);
    try
      DrawGraph(g);
    finally
      EndTransform(g);
    end;
    DrawText(g);
  finally
    g.EndScene;
  end;
end;

procedure TTrimmTabGraph.Draw;
begin
  if (Image <> nil) then
  begin
    DrawToCanvas(Image.Bitmap.Canvas);
  end;
end;

end.
