unit RggTest;

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Graphics;

type
  TTestCode = class
  protected
    Raster: Integer;
    FederPoly: TPolygon;
    ColorArray: array of TAlphaColor;

    procedure InitColorArray;
    procedure InitColorArray1;
    procedure InitColorArray2;
  public
    ShowColorBox: Boolean;

    constructor Create;

    function CreateRotationMatrix3D(const AnAxis: TPoint3D; Angle: Single): TMatrix3D;
    function RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;

    function CircleRect(x, y, r: single): TRectF;

    procedure FederPoint(g: TCanvas; P: TPointF);
    procedure FederStroke(g: TCanvas; P, Q: TPointF);

    procedure UpdateColorBox(g: TCanvas);
  end;

implementation

procedure TTestCode.InitColorArray;
begin
  InitColorArray2;
end;

procedure TTestCode.InitColorArray1;
var
  i: Integer;
  ac: TAlphaColor;
  bc: TAlphaColor;
begin
  ac := claRed;
  bc := claRed;
  SetLength(ColorArray, 257);
  for i := 0 to 256 do
  begin
    if i mod 100 = 0 then
    begin
      if ac = claRed then
        ac := claBlue
      else
        ac := claRed;
      bc := ac;
      TAlphaColorRec(bc).A := 200;
    end;
    ColorArray[i] := bc;
  end;
end;

procedure TTestCode.InitColorArray2;
var
  i: Integer;
  ac: TAlphaColor;
begin
  SetLength(ColorArray, 257);
  for i := 0 to 256 do
  begin
    ac := CorrectColor(HSLtoRGB(i/256, 0.8, 0.5));
    TAlphaColorRec(ac).A := 200;
    ColorArray[i] := ac;
  end;
end;

procedure TTestCode.UpdateColorBox(g: TCanvas);
var
  i: Integer;
  p0, p1: TPointF;
begin
  p0.X := 2 * Raster + 10;
  p1.X := p0.X + 10;
  g.Stroke.Thickness := 1;
  for i := 0 to 256 do
  begin
    p0.Y := Raster + i + 10;
    p1.Y := Raster + i + 10;
    g.Stroke.Color := ColorArray[i];
    g.DrawLine(p0, p1, 1.0);
  end;
end;

function TTestCode.CircleRect(x, y, r: single): TRectF;
begin
  result := RectF(x-r, y-r, x+r, y+r);
end;

procedure TTestCode.FederPoint(g: TCanvas; P: TPointF);
var
  r: Integer;
begin
  r := 10;
  g.FillEllipse(RectF(P.X-r, P.Y-r, P.X+r, P.Y+r), 1);
end;

procedure TTestCode.FederStroke(g: TCanvas; P, Q: TPointF);
var
  i: Integer;
  l: single;
  a: single;
  b: single;
  LCount: Integer;
  vp, vq: TPointF;
  vn, wn: TPointF;
  v, w: TPointF;

  p0, p1: TPointF;
  vx, vy: TPoint3D;
begin
  vp := P;
  vq := Q;

  v := vq - vp;

  vn := v.Normalize;
  vx := TPoint3D.Create(vn.X, vn.Y, 0);
  vy := RotateDegrees(vx, 90);
  wn := TPointF.Create(vy.X, vy.Y);

  l := v.Length;
  a := l / 3 / 8;
  b := 20.0;

  LCount := 8;
  SetLength(FederPoly, LCount);

  FederPoly[0] := P;

  v := vn  * 8 * a;
  p0.X := P.X + v.X;
  p0.Y := P.Y + v.Y;
  FederPoly[1] := p0;

  v := vn * a;
  w := wn *  b;
  for i := 2 to LCount-3 do
  begin
    p0 := p0 + v;
    if i mod 2 = 0 then
      p1 := p0 + w
    else
      p1 := p0 - w;
    FederPoly[i] := p1;
  end;

  p0 := p0 + v;
  FederPoly[LCount-2] := p0;

  FederPoly[LCount-1] := Q;

  g.DrawPolygon(FederPoly, 1.0);
end;

function TTestCode.RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
var
  a: single;
  m: TMatrix3D;
begin
  a := DegToRad(DegNormalize(Abs(wi)));
  if wi >= 0 then
    m := CreateRotationMatrix3D(TPoint3D.Create(0,0,1), a)
  else
    m := CreateRotationMatrix3D(TPoint3D.Create(0,0,-1), a);
  result := ov * m;
end;

constructor TTestCode.Create;
begin
  Raster := 70;
  InitColorArray;
end;

function TTestCode.CreateRotationMatrix3D(const AnAxis: TPoint3D; Angle: Single): TMatrix3D;
var
  Axis: TPoint3D;
  Cos, Sin, OneMinusCos: Extended;
const
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;
begin
  SinCos(Angle, Sin, Cos);
  OneMinusCos := 1 - Cos;
  Axis := AnAxis.Normalize;

  FillChar(Result, SizeOf(Result), 0);

  Result.M[X].V[X] := (OneMinusCos * Axis.V[0] * Axis.V[0]) + Cos;
  Result.M[X].V[Y] := (OneMinusCos * Axis.V[0] * Axis.V[1]) - (Axis.V[2] * Sin);
  Result.M[X].V[Z] := (OneMinusCos * Axis.V[2] * Axis.V[0]) + (Axis.V[1] * Sin);

  Result.M[Y].V[X] := (OneMinusCos * Axis.V[0] * Axis.V[1]) + (Axis.V[2] * Sin);
  Result.M[Y].V[Y] := (OneMinusCos * Axis.V[1] * Axis.V[1]) + Cos;
  Result.M[Y].V[Z] := (OneMinusCos * Axis.V[1] * Axis.V[2]) - (Axis.V[0] * Sin);

  Result.M[Z].V[X] := (OneMinusCos * Axis.V[2] * Axis.V[0]) - (Axis.V[1] * Sin);
  Result.M[Z].V[Y] := (OneMinusCos * Axis.V[1] * Axis.V[2]) + (Axis.V[0] * Sin);
  Result.M[Z].V[Z] := (OneMinusCos * Axis.V[2] * Axis.V[2]) + Cos;

  Result.M[W].V[W] := 1;
end;

end.
