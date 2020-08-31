unit RggCalc;

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
  System.Math,
  System.Math.Vectors;

type
  TBemerkungGG = (
    g1Vertical,
    g2Vertical,
    ggParallel,
    ggOK
  );

function SchnittGG(P1, P2, P3, P4: TPoint3D; out SP: TPoint3D): Boolean;
function PsiVonPhi(phi, l1, l2, l3, l4: single; out sv: Boolean): single;
function StartWinkel(l1, l2, l3, l4: single; out sv: Boolean): single;
function Hoehe(a, b, c: single; out k: single): single;

implementation

function SchnittGG(P1, P2, P3, P4: TPoint3D; out SP: TPoint3D): Boolean;
var
  a1, a2: single;
  sx, sz, x1, z1, x3, z3: single;
  Quotient: single;
  Fall: TBemerkungGG;
begin
  result := True;
  Fall := ggOK;

  a1 := 0;
  a2 := 0;
  sx := 0;
  sz := 0;

  x1 := P1.X;
  z1 := P1.Z;
  x3 := P3.X;
  z3 := P3.Z;

  Quotient := P2.X - P1.X;
  if abs(Quotient) > 0.001 then
    a1 := (P2.Z - P1.Z) / Quotient
  else
    Fall := g1Vertical;

  Quotient := P4.X - P3.X;
  if abs(Quotient) > 0.001 then
    a2 := (P4.Z - P3.Z) / Quotient
  else
    Fall := g2Vertical;

  if (Fall = ggOK) and (abs(a2-a1) < 0.001) then
    Fall := ggParallel;

  case Fall of
    ggParallel:
    begin
      sx := 0;
      sz := 0;
      result := False;
    end;

    ggOK:
      begin
        sx := (-a1 * x1 + a2 * x3 - z3 + z1) / (-a1 + a2);
        sz := (-a2 * a1 * x1 + a2 * z1 + a2 * x3 * a1 - z3 * a1) / (-a1 + a2);
      end;

    g1Vertical:
      begin
        sz := a2 * x1 - a2 * x3 + z3;
        sx := x1;
      end;

    g2Vertical:
      begin
        sz := a1 * x3 - a1 * x1 + z1;
        sx := x3;
      end;
  end;

  SP.X := sx;
  SP.Y := 0;
  SP.Z := sz;
end;

function Hoehe(a, b, c: single; out k: single): single;
var
  t: single;
begin
  k := sqr(a) + sqr(b) - sqr(c);
  k := k / 2 / a / a;
  t := sqr(b) - sqr(k) * sqr(a);
  if t < 0.001 then
    result := 0
  else
    result := sqrt(t);
  if IsNan(result) then
    result := 0;
end;

function PsiVonPhi(phi, l1, l2, l3, l4: single; out sv: Boolean): single;
var
  a, b, c, Rad: single;
begin
  sv := True;
  a := 2 * l1 * l4 - 2 * l2 * l4 * cos(phi);
  b := -2 * l2 * l4 * sin(phi);
  c := sqr(l1) + sqr(l2) - sqr(l3) + sqr(l4);
  c := c - 2 * l1 * l2 * cos(phi);
  Rad := sqr(a) + sqr(b) - sqr(c);
  if (a - c) = 0 then
    sv := False;
  if Rad < 0 then
    sv := False;
  if sv = True then
    result := 2 * arctan2((b + sqrt(Rad)), (a - c))
  else
    result := 0;
end;

function StartWinkel(l1, l2, l3, l4: single; out sv: Boolean): single;
var
  cosphi: single;
  sw: single;
begin
  if l1 + l2 > l3 + l4 then
  begin
    sv := False;
    result := 0;
  end
  else
  begin
    sv := True;
    cosphi := (sqr(l1) + sqr(l2 + l3) - sqr(l4)) / (2 * l1 * (l2 + l3));
    sw := arctan2(sqrt(1 - sqr(cosphi)), cosphi);
    result := sw;
    if cosphi < 0 then
      result := pi - abs(sw);
  end;
end;

end.
