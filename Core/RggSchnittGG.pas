unit RggSchnittGG;

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
  RggTypes,
  RggCalc;

type
  TSchnittGG = class
  protected
    A, B: TRealPoint;
    C, D: TRealPoint;
    SP: TRealPoint;
    Bem: TBemerkungGG;
    NeedCalc: Boolean;
    sv: Boolean;
    function GetBemerkung: string;
    procedure SetPA(const Value: TRealPoint);
    procedure SetPB(const Value: TRealPoint);
    procedure SetPC(const Value: TRealPoint);
    function Vorhanden: Boolean;
    procedure SetPD(const Value: TRealPoint);
    function GetSchnittPunkt: TRealPoint;
  public
    constructor Create;
    function Schnitt: Boolean;

    property PA: TRealPoint read A write SetPA;
    property PB: TRealPoint read B write SetPB;
    property PC: TRealPoint read C write SetPC;
    property PD: TRealPoint read D write SetPD;

    property SchnittPunkt: TRealPoint read GetSchnittPunkt;
    property SPVorhanden: Boolean read Vorhanden;
    property Bemerkung: string read GetBemerkung;
  end;

  TSchnittGGEx = class(TSchnittGG)
  private
    FR1: double;
    FR2: double;
    FA1: double;
    FA2: double;
    FH1: double;
    FH2: double;
    FV1: double;
    FV2: double;
    procedure SetM1(const Value: TPointF);
    procedure SetM2(const Value: TPointF);
    procedure SetA1(const Value: double);
    procedure SetA2(const Value: double);
    procedure SetR1(const Value: double);
    procedure SetR2(const Value: double);
    procedure SetE1(const Value: double);
    procedure SetE2(const Value: double);
  public
    constructor Create;
    property Radius1: double write SetR1;
    property Radius2: double write SetR2;
    property M1: TPointF write SetM1;
    property M2: TPointF write SetM2;
    property Angle1: double write SetA1;
    property Angle2: double write SetA2;
    property Elevation1: double write SetE1;
    property Elevation2: double write SetE2;
    property H1: double read FH1;
    property H2: double read FH2;
    property V1: double read FV1;
    property V2: double read FV2;
  end;

implementation

{ TSchnittGG }

constructor TSchnittGG.Create;
begin
  A := Null;
  B := Null;
  C := Null;
  D := Null;
  B[x] := 100.0;
  D[x] := 200.0;
  D[z] := 100.0;
end;

function TSchnittGG.GetBemerkung: string;
begin
  case Bem of
    g1Vertical: result := 'g1Vertical';
    g2Vertical: result := 'g2Vertical';
    ggParallel: result := 'ggParallel';
    ggOK: result := 'ggOK';
  end;
end;

function TSchnittGG.GetSchnittPunkt: TRealPoint;
begin
  if NeedCalc = True then
    Schnitt;
  result := SP;
end;

function TSchnittGG.Schnitt: Boolean;
var
  a1, a2: double;
  sx, sz, x1, z1, x3, z3: double;
  Quotient: double;
begin
  NeedCalc := False;
  result := True;
  Bem := ggOK;
  sv := False;

  a1 := 0;
  a2 := 0;
  sx := 0;
  sz := 0;

  x1 := A[x];
  z1 := A[z];
  x3 := C[x];
  z3 := C[z];

  Quotient := B[x] - A[x];
  if abs(Quotient) > 0.001 then
    a1 := (B[z] - A[z]) / Quotient
  else
    Bem := g1Vertical;

  Quotient := D[x] - C[x];
  if abs(Quotient) > 0.001 then
    a2 := (D[z] - C[z]) / Quotient
  else
    Bem := g2Vertical;

  if (Bem = ggOK) and (abs(a2-a1) < 0.001) then
    Bem := ggParallel;

  case Bem of
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
  sv := Bem <> ggParallel;
end;

function TSchnittGG.Vorhanden: Boolean;
begin
  if NeedCalc = True then
    Schnitt;
  result := sv;
end;

procedure TSchnittGG.SetPA(const Value: TRealPoint);
begin
  A := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPB(const Value: TRealPoint);
begin
  B := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPC(const Value: TRealPoint);
begin
  C := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPD(const Value: TRealPoint);
begin
  D := Value;
  NeedCalc := True;
end;

{ TSchnittGGEx }

constructor TSchnittGGEx.Create;
begin
  inherited;
  FR1 := 100.0;
  FR2 := 100.0;
  FH1 := 1.0;
  FH2 := 1.0;
end;

procedure TSchnittGGEx.SetA1(const Value: double);
var
  w: single;
  t: TRealPoint;
begin
  FA1 := Value;
  while FA1 > 360 do
  begin
    FA1 := FA1 - 360;
  end;
  while FA1 < 0 do
  begin
    FA1 := FA1 + 360;
  end;

  w := FA1 * 2 * PI / 360;
  t[x] := cos(w);
  t[y] := 0;
  t[z] := sin(w);
  t := SkalarMult(t, FR1 * FH1);
  B := vadd(A, t);
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetA2(const Value: double);
var
  w: single;
  t: TRealPoint;
begin
  FA2 := Value;
  while FA2 > 360 do
  begin
    FA2 := FA2 - 360;
  end;
  while FA2 < 0 do
  begin
    FA2 := FA2 + 360;
  end;

  w := FA2 * 2 * PI / 360;
  t[x] := cos(w);
  t[y] := 0;
  t[z] := sin(w);
  t := SkalarMult(t, FR2 * FH2);
  D := vadd(C, t);
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetM1(const Value: TPointF);
var
  T: TRealPoint;
begin
  T := vsub(B, A);
  A[x] := Value.X;
  A[y] := 0;
  A[z] := Value.Y;
  B := vadd(A, T);
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetM2(const Value: TPointF);
var
  T: TRealPoint;
begin
  T := vsub(D, C);
  C[x] := Value.X;
  C[y] := 0;
  C[z] := Value.Y;
  D := vadd(C, T);
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetR1(const Value: double);
var
  T: TRealPoint;
begin
  if Value > 0.5 then
  begin
    FR1 := Value;
    T := EVektor(A, B);
    T := SkalarMult(T, Value * FH1);
    B := vadd(A, T);
    NeedCalc := True;
  end;
end;

procedure TSchnittGGEx.SetR2(const Value: double);
var
  T: TRealPoint;
begin
  if Value > 0.5 then
  begin
    FR2 := Value;
    T := EVektor(C, D);
    T := SkalarMult(T, Value * FH2);
    D := vadd(C, T);
    NeedCalc := True;
  end;
end;

procedure TSchnittGGEx.SetE1(const Value: double);
begin
  if (Value >= -89) and (Value < 89) then
  begin
    FH1 := cos(Value * PI / 180);
    FV1 := sin(Value * PI / 180);
    SetR1(FR1);
  end;
end;

procedure TSchnittGGEx.SetE2(const Value: double);
begin
  if (Value >= -89) and (Value <= 89) then
  begin
    FH2 := cos(Value * PI / 180);
    FV2 := sin(Value * PI / 180);
    SetR2(FR2);
  end;
end;

end.
