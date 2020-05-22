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
  RggTypes;

var
  Null: TRealPoint;

type
  TBemerkungGG = (
    g1Vertical,
    g2Vertical,
    ggParallel,
    ggOK
  );

function SchnittGG(P1, P2, P3, P4: TRealPoint; out SP: TRealPoint): Boolean;
function Abstand(P1, P2: TRealPoint): double;
function PsiVonPhi(phi, l1, l2, l3, l4: double; out sv: Boolean): double;
function StartWinkel(l1, l2, l3, l4: double; out sv: Boolean): double;
function Hoehe(a, b, c: double; out k: double): double;
function vadd(a, b: TRealPoint): TRealPoint;
function vsub(a, b: TRealPoint): TRealPoint;
function vprod(a, b: TRealPoint): TRealPoint;
function sprod(a, b: TRealPoint): double;
function EVektor(a, b: TRealPoint): TRealPoint;
function SkalarMult(a: TRealPoint; b: double): TRealPoint;
function CleanUpReal(a: double): double;
function Maximum(a, b: double): double;
function Minimum(a, b: double): double;

implementation

function Maximum(a, b: double): double;
begin
  result := a;
  if b > a then
    result := b;
end;

function Minimum(a, b: double): double;
begin
  result := a;
  if b < a then
    result := b;
end;

function CleanUpReal(a: double): double;
const
  epsilon = 1E-5;
begin
  if abs(a) < epsilon then
    result := 0
  else
    result := a;
end;

function EVektor(a, b: TRealPoint): TRealPoint;
var
  temp: double;
  ooTemp: TRealPoint;
begin
  ooTemp := vsub(b, a);
  temp := 1 / Abstand(a, b);
  ooTemp := SkalarMult(ooTemp, temp);
  result := ooTemp;
end;

function SkalarMult(a: TRealPoint; b: double): TRealPoint;
begin
  a[x] := a[x] * b;
  a[y] := a[y] * b;
  a[z] := a[z] * b;
  result := a;
end;

function sprod(a, b: TRealPoint): double;
begin
  result := a[x] * b[x] + a[y] * b[y] + a[z] * b[z];
end;

function vadd(a, b: TRealPoint): TRealPoint;
begin
  result[x] := a[x] + b[x];
  result[y] := a[y] + b[y];
  result[z] := a[z] + b[z];
end;

function vsub(a, b: TRealPoint): TRealPoint;
begin
  result[x] := a[x] - b[x];
  result[y] := a[y] - b[y];
  result[z] := a[z] - b[z];
end;

function vprod(a, b: TRealPoint): TRealPoint;
begin
  result[x] := a[y] * b[z] - a[z] * b[y];
  result[y] := a[z] * b[x] - a[x] * b[z];
  result[z] := a[x] * b[y] - a[y] * b[x];
end;

function Abstand(P1, P2: TRealPoint): double;
var
  P3: TRealPoint;
  h4: double;
begin
  P3[x] := P2[x] - P1[x];
  P3[y] := P2[y] - P1[y];
  P3[z] := P2[z] - P1[z];
  h4 := (P3[x] * P3[x] + P3[y] * P3[y] + P3[z] * P3[z]);
  Abstand := sqrt(h4);
end;

function SchnittGG(P1, P2, P3, P4: TRealPoint; out SP: TRealPoint): Boolean;
var
  a1, a2: double;
  sx, sz, x1, z1, x3, z3: double;
  Quotient: double;
  Fall: TBemerkungGG;
begin
  result := True;
  Fall := ggOK;

  a1 := 0;
  a2 := 0;
  sx := 0;
  sz := 0;

  x1 := P1[x];
  z1 := P1[z];
  x3 := P3[x];
  z3 := P3[z];

  Quotient := P2[x] - P1[x];
  if abs(Quotient) > 0.001 then
    a1 := (P2[z] - P1[z]) / Quotient
  else
    Fall := g1Vertical;

  Quotient := P4[x] - P3[x];
  if abs(Quotient) > 0.001 then
    a2 := (P4[z] - P3[z]) / Quotient
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

  SP[x] := sx;
  SP[y] := 0;
  SP[z] := sz;
end;

function Hoehe(a, b, c: double; out k: double): double;
begin
  k := sqr(a) + sqr(b) - sqr(c);
  k := k / 2 / a / a;
  result := sqrt(sqr(b) - sqr(k) * sqr(a));
end;

function PsiVonPhi(phi, l1, l2, l3, l4: double; out sv: Boolean): double;
var
  a, b, c, Rad: double;
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

function StartWinkel(l1, l2, l3, l4: double; out sv: Boolean): double;
var
  cosphi: double;
  sw: double;
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

initialization

Null[x] := 0;
Null[y] := 0;
Null[z] := 0;

end.
