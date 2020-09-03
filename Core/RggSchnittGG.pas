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
  System.Math.Vectors,
  RggTypes,
  RggCalc;

type
  TSchnittGG = class
  protected
    A, B: TPoint3D;
    C, D: TPoint3D;
    SP: TPoint3D;
    Bem: TBemerkungGG;
    NeedCalc: Boolean;
    sv: Boolean;
    function GetBemerkung: string;
    procedure SetPA(const Value: TPoint3D);
    procedure SetPB(const Value: TPoint3D);
    procedure SetPC(const Value: TPoint3D);
    function Vorhanden: Boolean;
    procedure SetPD(const Value: TPoint3D);
    function GetSchnittPunkt: TPoint3D;
  public
    constructor Create;
    function Schnitt: Boolean;

    property PA: TPoint3D read A write SetPA;
    property PB: TPoint3D read B write SetPB;
    property PC: TPoint3D read C write SetPC;
    property PD: TPoint3D read D write SetPD;

    property SchnittPunkt: TPoint3D read GetSchnittPunkt;
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
  A := TPoint3D.Zero;
  B := TPoint3D.Zero;
  C := TPoint3D.Zero;
  D := TPoint3D.Zero;
  B.X := 100.0;
  D.X := 200.0;
  D.Z := 100.0;
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

function TSchnittGG.GetSchnittPunkt: TPoint3D;
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

  x1 := A.X;
  z1 := A.Z;
  x3 := C.X;
  z3 := C.Z;

  Quotient := B.X - A.X;
  if abs(Quotient) > 0.001 then
    a1 := (B.Z - A.Z) / Quotient
  else
    Bem := g1Vertical;

  Quotient := D.X - C.X;
  if abs(Quotient) > 0.001 then
    a2 := (D.Z - C.Z) / Quotient
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

  SP.X := sx;
  SP.Y := 0;
  SP.Z := sz;
  sv := Bem <> ggParallel;
end;

function TSchnittGG.Vorhanden: Boolean;
begin
  if NeedCalc = True then
    Schnitt;
  result := sv;
end;

procedure TSchnittGG.SetPA(const Value: TPoint3D);
begin
  A := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPB(const Value: TPoint3D);
begin
  B := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPC(const Value: TPoint3D);
begin
  C := Value;
  NeedCalc := True;
end;

procedure TSchnittGG.SetPD(const Value: TPoint3D);
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
  t: TPoint3D;
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
  t.X := cos(w);
  t.Y := 0;
  t.Z := sin(w);
  t := t * (FR1 * FH1);
  B := A + t;
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetA2(const Value: double);
var
  w: single;
  t: TPoint3D;
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
  t.X := cos(w);
  t.Y := 0;
  t.Z := sin(w);
  t := t * (FR2 * FH2);
  D := C + t;
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetM1(const Value: TPointF);
var
  T: TPoint3D;
begin
  T := B - A;
  A.X := Value.X;
  A.Y := 0;
  A.Z := Value.Y;
  B := A + T;
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetM2(const Value: TPointF);
var
  T: TPoint3D;
begin
  T := D - C;
  C.X := Value.X;
  C.Y := 0;
  C.Z := Value.Y;
  D := C + T;
  NeedCalc := True;
end;

procedure TSchnittGGEx.SetR1(const Value: double);
var
  T: TPoint3D;
begin
  if Value > 0.5 then
  begin
    FR1 := Value;
    T := (A - B).Normalize;
    T := T * (Value * FH1);
    B := A + T;
    NeedCalc := True;
  end;
end;

procedure TSchnittGGEx.SetR2(const Value: double);
var
  T: TPoint3D;
begin
  if Value > 0.5 then
  begin
    FR2 := Value;
    T := (C - D).Normalize;
    T := T * (Value * FH2);
    D := C + T;
    NeedCalc := True;
  end;
end;

procedure TSchnittGGEx.SetE1(const Value: double);
var
  t: double;
begin
  if (Value >= -89) and (Value < 89) then
  begin
    t := DegToRad(Value);
    FH1 := cos(t);
    FV1 := sin(t);
    SetR1(FR1);
  end;
end;

procedure TSchnittGGEx.SetE2(const Value: double);
var
  t: double;
begin
  if (Value >= -89) and (Value <= 89) then
  begin
    t := DegToRad(Value);
    FH2 := cos(t);
    FV2 := sin(t);
    SetR2(FR2);
  end;
end;

end.
