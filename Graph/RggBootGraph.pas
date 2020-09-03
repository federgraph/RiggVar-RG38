unit RggBootGraph;

interface

uses
  System.IniFiles,
  System.Math.Vectors,
  RggCalc,
  RggTypes,
  RggGraph;

type
  TBootGraph = class(TRggGraph)
  private
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FKoppelKurve: TKoordLine;
    FKoppel: Boolean;
    FBogen: Boolean;
    FGestrichelt: Boolean;
    FViewPoint: TViewPoint;
    FRiggLED: Boolean;
    procedure SetKoppel(const Value: Boolean);
    procedure SetKoordinaten(const Value: TRealRiggPoints);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);
  protected
    BogenIndexD: Integer;
    function FindBogenIndexOf(P: TRealPoint): Integer;
    function GetFreshRiggPoints: TRealRiggPoints;
  public
    rP: TRealRiggPoints;
    Kurve: TMastKurve;

    constructor Create;

    procedure LoadFromIniFile(FileName: string);

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetKoppelKurve(const Value: TKoordLine);
    function GetMastKurvePoint(const Index: Integer): TRealPoint;

    property Koordinaten: TRealRiggPoints read rP write SetKoordinaten;
    property KoppelKurve: TKoordLine read FKoppelKurve write SetKoppelKurve;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property Bogen: Boolean read FBogen write SetBogen;
    property WanteGestrichelt: Boolean read FGestrichelt write SetWanteGestrichelt;
    property RiggLED: Boolean read FRiggLED write SetRiggLED;
  end;

implementation

constructor TBootGraph.Create;
begin
  inherited Create;
  FSalingTyp := stFest;
  FControllerTyp := ctOhne;
end;

procedure TBootGraph.SetKoordinaten(const Value: TRealRiggPoints);
begin
  rP := Value;
  GrafikOK := True;
  Updated := False;
end;

procedure TBootGraph.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  RaumGraphProps.Koppel := True;
end;

procedure TBootGraph.SetKoppelKurve(const Value: TKoordLine);
begin
  FKoppelKurve := Value;
  KoppelKurveNeedFill := True;
end;

procedure TBootGraph.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
  RaumGraphProps.ControllerTyp := Value;
end;

procedure TBootGraph.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: single;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - Beta);
  temp2 := cos(beta);
  temp3 := sin(pi / 2 - Beta);
  temp4 := sin(beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    Kurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    Kurve[j].Y := 0;
    Kurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TBootGraph.SetRiggLED(const Value: Boolean);
begin
  FRiggLED := Value;
end;

procedure TBootGraph.SetMastKurve(const Value: TMastKurve);
begin
  Kurve := Value;
end;

procedure TBootGraph.SetSalingTyp(const Value: TSalingTyp);
begin
  FSalingTyp := Value;
  RaumGraphProps.SalingTyp := Value;
end;

procedure TBootGraph.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  RaumGraphProps.Bogen := Value;
  Updated := False;
end;

procedure TBootGraph.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  Updated := False;
end;

procedure TBootGraph.SetWanteGestrichelt(const Value: Boolean);
begin
  FGestrichelt := Value;
  RaumGraphProps.Gestrichelt := Value;
end;

function TBootGraph.GetMastKurvePoint(const Index: Integer): TRealPoint;
begin
  if (Index >= 0) and (Index < Length(Kurve)) then
    result := Kurve[Index]
  else
  begin
    result := TRealPoint.Zero;
  end;
end;

function TBootGraph.FindBogenIndexOf(P: TRealPoint): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: single;
  a: single;
begin
  j := Length(Kurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := (P - Kurve[i]).Length;
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

function TBootGraph.GetFreshRiggPoints: TRealRiggPoints;
var
  i: TRiggPoint;
begin
  for i := Low(TRiggPoint) to High(TRiggPoint) do
  begin
    result.V[i] := TPoint3D.Zero;
  end;
end;

procedure TBootGraph.LoadFromIniFile(FileName: string);
var
  IniFile: TIniFile;
  S: string;
  i: TRiggPoint;
  iP: TRealRiggPoints;
begin
  iP := GetFreshRiggPoints;
  IniFile := TIniFile.Create(FileName);
  S := 'Koordinaten Rumpf';
  try
    with IniFile do
    begin
      iP.A0.X := ReadInteger(S, 'A0x', Round(iP.A0.X));
      iP.A0.Y := ReadInteger(S, 'A0y', Round(iP.A0.Y));
      iP.A0.Z := ReadInteger(S, 'A0z', Round(iP.A0.Z));
      iP.B0.X := ReadInteger(S, 'B0x', Round(iP.B0.X));
      iP.B0.Y := ReadInteger(S, 'B0y', Round(iP.B0.Y));
      iP.B0.Z := ReadInteger(S, 'B0z', Round(iP.B0.Z));
      iP.C0.X := ReadInteger(S, 'C0x', Round(iP.C0.X));
      iP.C0.Y := ReadInteger(S, 'C0y', Round(iP.C0.Y));
      iP.C0.Z := ReadInteger(S, 'C0z', Round(iP.C0.Z));
      iP.D0.X := ReadInteger(S, 'D0x', Round(iP.D0.X));
      iP.D0.Y := ReadInteger(S, 'D0y', Round(iP.D0.Y));
      iP.D0.Z := ReadInteger(S, 'D0z', Round(iP.D0.Z));
      iP.E0.X := ReadInteger(S, 'E0x', Round(iP.E0.X));
      iP.E0.Y := ReadInteger(S, 'E0y', Round(iP.E0.Y));
      iP.E0.Z := ReadInteger(S, 'E0z', Round(iP.E0.Z));
      iP.F0.X := ReadInteger(S, 'F0x', Round(iP.F0.X));
      iP.F0.Y := ReadInteger(S, 'F0y', Round(iP.F0.Y));
      iP.F0.Z := ReadInteger(S, 'F0z', Round(iP.F0.Z));

      S := 'Koordinaten Rigg';
      iP.A.X := ReadInteger(S, 'Ax', Round(iP.A.X));
      iP.A.Y := ReadInteger(S, 'Ay', Round(iP.A.Y));
      iP.A.Z := ReadInteger(S, 'Az', Round(iP.A.Z));
      iP.B.X := ReadInteger(S, 'Bx', Round(iP.B.X));
      iP.B.Y := ReadInteger(S, 'By', Round(iP.B.Y));
      iP.B.Z := ReadInteger(S, 'Bz', Round(iP.B.Z));
      iP.C.X := ReadInteger(S, 'Cx', Round(iP.C.X));
      iP.C.Y := ReadInteger(S, 'Cy', Round(iP.C.Y));
      iP.C.Z := ReadInteger(S, 'Cz', Round(iP.C.Z));
      iP.D.X := ReadInteger(S, 'Dx', Round(iP.D.X));
      iP.D.Y := ReadInteger(S, 'Dy', Round(iP.D.Y));
      iP.D.Z := ReadInteger(S, 'Dz', Round(iP.D.Z));
      iP.E.X := ReadInteger(S, 'Ex', Round(iP.E.X));
      iP.E.Y := ReadInteger(S, 'Ey', Round(iP.E.Y));
      iP.E.Z := ReadInteger(S, 'Ez', Round(iP.E.Z));
      iP.F.X := ReadInteger(S, 'Fx', Round(iP.F.X));
      iP.F.Y := ReadInteger(S, 'Fy', Round(iP.F.Y));
      iP.F.Z := ReadInteger(S, 'Fz', Round(iP.F.Z));
    end;
    for i := ooA0 to ooF0 do
    begin
      rP.V[i] := iP.V[i];
    end;
    for i := ooA to ooF do
    begin
      rP.V[i] := iP.V[i];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

end.
