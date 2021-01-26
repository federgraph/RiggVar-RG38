unit RiggVar.FD.Chart;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  RiggVar.FD.Elements;

type
  TRggBox = record
  public
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TRggChart = class(TRggElement)
  protected
    PD: TPathData;
    LNr: Integer;
    procedure DrawText(g: TCanvas);
    procedure DrawPoly1(g: TCanvas);
    procedure DrawPoly2(g: TCanvas);
  public
    Poly: array of single;

    Box: TRggBox;

    Xmin: single;
    Xmax: single;
    Ymin: single;
    Ymax: single;

    ChartPunktX: single;

    WantChartPunktX: Boolean;
    WantRectangles: Boolean;
    WantCurve: Boolean;

    PointRadius: single;
    CurveOpacity: single;

    constructor Create(ACount: Integer = 20);
    destructor Destroy; override;
    procedure Draw(g: TCanvas); override;

    procedure InitDefault;

    procedure LookForYMinMax;
    property Count: Integer read LNr;
  end;

implementation

{ TRggChart }

procedure TRggChart.LookForYMinMax;
var
  i: Integer;
  t: single;
begin
  Ymax := Poly[0];
  Ymin := Ymax;
  for i := 0 to LNr do
  begin
    t := Poly[i];
    if t > Ymax then
      Ymax := t;
    if t < Ymin then
      Ymin := t;
  end;
end;

constructor TRggChart.Create(ACount: Integer = 20);
begin
  inherited Create;
  TypeName := 'Chart';
  IndentItem := True;

  PD := TPathData.Create;

  LNr := ACount;
  if ACount > 9 then
    LNr := ACount;

  SetLength(Poly, LNr + 1);

  Box.X := 0;
  Box.Y := 0;
  Box.Width := 800;
  Box.Height := 800;

  PointRadius := 3.0;
  CurveOpacity := 1.0;

  WantCurve := True;
end;

destructor TRggChart.Destroy;
begin
  PD.Free;
  inherited;
end;

procedure TRggChart.Draw(g: TCanvas);
begin
  DrawPoly1(g);
end;

procedure TRggChart.DrawPoly2(g: TCanvas);
var
  LineToPoint: TPointF;
  P: TPointF;
  i: Integer;
  tempX: single;
  tempY: single;

  ox, oy: single;

  procedure LineTo(x2, y2: single);
  begin
    g.DrawLine(LineToPoint, PointF(x2, y2), 1.0);
    LineToPoint := PointF(x2, y2);
  end;

begin
  ox := Box.X + Drawing.FaxPoint3D.X;
  oy := Box.Y + Drawing.FaxPoint3D.Y;

  g.Stroke.Thickness := StrokeThickness;
  g.Stroke.Color := StrokeColor;

  g.Stroke.Join := TStrokeJoin.Round;
  g.Stroke.Cap := TStrokeCap.Round;

  if WantCurve then
  begin
    g.Stroke.Color := StrokeColor;
    tempY := Box.Height - Box.Height * (Poly[0] - Ymin) / (Ymax - Ymin);
    P.X := ox;
    P.Y := oy + tempY;
    LineToPoint := P;
    for i := 1 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + tempX;
      P.Y := oy + tempY;
      LineTo(P.X, P.Y);
    end;
  end;

  if WantRectangles then
  begin
    g.Stroke.Thickness := 1.0;
    g.Stroke.Color := claWhite;
    g.Fill.Color := StrokeColor;
    for i := 0 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + tempX;
      P.Y := oy + tempY;
      g.FillRect(
        RectF(P.X - PointRadius, P.Y - PointRadius,
              P.X + PointRadius, P.Y + PointRadius), 0, 0, [], 1.0);
    end;
  end;

  if WantChartPunktX then
  begin
    g.Stroke.Color := claRed;
    tempX := Box.Width * ((ChartPunktX) - Xmin) / (XMax - Xmin);
    tempY := Box.Height;
    P.X := ox + tempX;
    P.Y := oy + tempY;
    g.FillRect(
      RectF(P.X - PointRadius, P.Y - PointRadius,
            P.X + PointRadius, P.Y + PointRadius), 0, 0, [], 1.0);

  end;
end;

procedure TRggChart.DrawText(g: TCanvas);
begin
  if ShowCaption or GlobalShowCaption then
  begin
    g.Fill.Color := claBlack;
    TextOutLeading(g, Caption);
  end;
end;

procedure TRggChart.InitDefault;
var
  i: Integer;
begin
  for i := 0 to LNr do
  begin
    Poly[i] := sin(i / LNr * 2 * Pi);
  end;
  LookForYMinMax;
end;

procedure TRggChart.DrawPoly1(g: TCanvas);
var
  P: TPointF;
  i: Integer;
  tempX: single;
  tempY: single;
  ox, oy: single;
begin
  if Length(Poly) = 0 then
    Exit;

  ox := Box.X + Drawing.FaxPoint3D.X;
  oy := Box.Y + Drawing.FaxPoint3D.Y;

  g.Stroke.Thickness := StrokeThickness;
  g.Stroke.Color := StrokeColor;

  g.Stroke.Join := TStrokeJoin.Round;
  g.Stroke.Cap := TStrokeCap.Round;

  { Kurve }
  if WantCurve then
  begin
    PD.Clear;
    tempY := Box.Height - Box.Height * (Poly[0] - Ymin) / (Ymax - Ymin);
    P.X := ox;
    P.Y := oy + tempY;
    PD.MoveTo(P);
    for i := 1 to LNr do
    begin
      tempX := Box.Width * (i / LNr);
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + tempX;
      P.Y := oy + tempY;
      PD.LineTo(P);
    end;
    g.DrawPath(PD, CurveOpacity);
  end;

  if WantRectangles then
  begin
    g.Stroke.Thickness := 1.0;
    g.Stroke.Color := claWhite;
    g.Fill.Color := StrokeColor;
    for i := 0 to LNr do
    begin
      tempX := Box.Width * i / LNr;
      tempY := Box.Height - Box.Height * (Poly[i] - Ymin) / (Ymax - Ymin);
      P.X := ox + tempX;
      P.Y := oy + tempY;
      g.FillRect(
        RectF(P.X - PointRadius, P.Y - PointRadius,
              P.X + PointRadius, P.Y + PointRadius), 0, 0, [], 1.0);
    end;
  end;

  if WantChartPunktX then
  begin
    g.Stroke.Color := claRed;
    tempX := Box.Width * ((ChartPunktX) - Xmin) / (XMax - Xmin);
    tempY := Box.Height;
    P.X := ox + tempX;
    P.Y := oy + tempY;
    g.FillRect(
      RectF(P.X - PointRadius, P.Y - PointRadius,
            P.X + PointRadius, P.Y + PointRadius), 0, 0, [], 1.0);

  end;
end;

end.
