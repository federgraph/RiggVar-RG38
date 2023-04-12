unit RiggVar.FederModel.Circle;

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
  System.Math,
  System.Math.Vectors,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  RiggVar.FB.Formula,
  RiggVar.FB.Equation,
  RiggVar.FB.Classes;

type
  TFederPoly = class
  private
    function RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
  public
    WantLL: Boolean;
    LL: TPolygon;
    D1: TPointF;
    D2: TPointF;

    WantLC: Boolean;
    LC: TPolygon;

    LLCount: Integer;
    LCCount: Integer;

    WantDash: Boolean;
    sw1: single;
    sw2: single;

    ParamBahnRadius: single;
    ParamBahnPosition: TPoint3D;
    ParamBahnAngle: single;

    EQ: TFederEquation;

    Flip: single;

    constructor Create;
    destructor Destroy; override;

    procedure DrawTest(g: TCanvas);
    procedure DrawPolyLine(p: TPolygon; opa: single);
    function CircleRect(x, y, r: single): TRectF;

    procedure UpdateSize;
    procedure InitLL;
    procedure InitLC;
    procedure Compute;
  end;

implementation

constructor TFederPoly.Create;
var
  t: single;
begin
  { flip direction of of LL }
  Flip := -1;

  ParamBahnRadius := 100;
  ParamBahnPosition := TPoint3D.Create(400, 400, 0);
  ParamBahnAngle := 0;

  EQ := TFederEquation.Create;
  EQ.InitFormula;

  t := 90;
  EQ.l1 := t;
  EQ.l2 := t;
  EQ.l3 := t;

  t := 0.1;
  EQ.k1 := t;
  EQ.k2 := t;
  EQ.k3 := t;

  UpdateSize;
end;

destructor TFederPoly.Destroy;
begin
  EQ.Free;
  inherited;
end;

procedure TFederPoly.Compute;
begin
  InitLL;
  InitLC;
end;

procedure TFederPoly.UpdateSize;
begin
  LLCount := 100;
  LCCount := 360;
  SetLength(LL, LLCount + 1);
  SetLength(LC, LCCount + 1);
end;

procedure TFederPoly.InitLC;
var
  i: Integer;
  u, v: single;
  p: TPoint3D;
  phi: single;
  pstep: single;
  cphi: single;
  sphi: single;
  ox: single;
  oy: single;
begin
  ox := ParamBahnPosition.X;
  oy := ParamBahnPosition.Y;

  pstep := 2 * pi / LCCount;
  for i := 0 to LCCount do
  begin
    phi := i * pstep;

    cphi := cos(phi);
    sphi := sin(phi);

    u := ox + ParamBahnRadius * cphi;
    v := oy + ParamBahnRadius * sphi;

    p.Z := EQ.GetValue(u, v) / 50;
    p.X := ox + (ParamBahnRadius + p.Z) * cphi;
    p.Y := oy + (ParamBahnRadius + p.Z) * sphi;

    LC[i].X := p.X;
    LC[i].Y := p.Y;
  end;
end;

procedure TFederPoly.InitLL;
var
  i: Integer;
  z: single;
  u, v: single;
  mx, ms: single;
  msx: single;
  ox, oy, wi: single;

  v0, v1, v2, v3, v4, v5, vx, vz: TPoint3D;
  f: single;
begin
  mx := ParamBahnRadius;
  ms := 2 * mx / LLCount;
  msx := ms;

  ox := ParamBahnPosition.X;
  oy := ParamBahnPosition.Y;
  wi := ParamBahnAngle;

  v0 := TPoint3D.Create(ox, oy, 0);
  v1 := TPoint3D.Create(1, 0, 0);
  vx := RotateDegrees(v1, wi);

  vz := RotateDegrees(v1, wi + 90);

  for i := 0 to LLCount do
  begin
    f := -mx + i * msx;
    v2 := vx * f;
    v3 := v0 + v2;

    u := v3.X;
    v := v3.Y;
    z := EQ.GetValue(u, v);

    v4 := vz * Flip * z;
    v5 := v3 + v4;

    if i = 0 then
    begin
      D1.X := u;
      D1.Y := v;
    end;
    if i = LLCount then
    begin
      D2.X := u;
      D2.Y := v;
    end;

    LL[i].X := v5.X;
    LL[i].Y := v5.Y;
  end;
end;

procedure TFederPoly.DrawPolyLine(p: TPolygon; opa: single);
begin

end;

function TFederPoly.CircleRect(x, y, r: single): TRectF;
begin
  result := RectF(x - r, y - r, x + r, y + r);
end;

procedure TFederPoly.DrawTest(g: TCanvas);
var
  cr: TRectF;
begin
  sw1 := 1.0;
  sw2 := 1.0;

  if WantLC then
  begin
    { base circle }
    g.Stroke.Thickness := sw1;
    g.Stroke.Color := claAqua;
    cr := CircleRect(ParamBahnPosition.X, ParamBahnPosition.Y, ParamBahnRadius);
    g.DrawEllipse(cr, 1);

    { function poly over probe circle }
    g.Stroke.Thickness := sw2;
    g.Stroke.Color := claRed;
    g.Stroke.Dash := TStrokeDash.Solid;
    DrawPolyLine(LC, 1.0);
  end;

  if WantLL then
  begin
    { base line }
    g.Stroke.Thickness := sw1;
    g.Stroke.Color := claOrange;
    g.DrawLine(D1, D2, 1.0);

    { function poly over probe diameter }
    g.Stroke.Thickness := sw2;
    g.Stroke.Color := claOrange;
    DrawPolyLine(LL, 1.0);
  end;
end;

function TFederPoly.RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
var
  a: single;
  m: TMatrix3D;
begin
  a := DegToRad(DegNormalize(Abs(wi)));
  if wi >= 0 then
    m := TMatrix3D.CreateRotation(TPoint3D.Create(0, 0, 1), a)
  else
    m := TMatrix3D.CreateRotation(TPoint3D.Create(0, 0, -1), a);
  result := ov * m;
end;

end.
