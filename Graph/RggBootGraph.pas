unit RggBootGraph;

interface

uses
  System.IniFiles,
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
    function GetFreshRiggPoints: TIntRiggPoints;
  public
    rP: TRealRiggPoints;
    Kurve: TMastKurve;

    constructor Create;

    procedure LoadFromIniFile(FileName: string);

    procedure SetMastLineData(const Value: TLineDataR100; L: double; Beta: double);
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

procedure TBootGraph.SetMastLineData(const Value: TLineDataR100; L: double; Beta: double);
var
  temp1, temp2, temp3, temp4, tempL: double;
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
    Kurve[j, x] := rP[ooD0, x] - tempL * temp1 + Value[k] * temp2;
    Kurve[j, y] := 0;
    Kurve[j, z] := rP[ooD0, z] + tempL * temp3 + Value[k] * temp4;
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
    result[x] := 0;
    result[y] := 0;
    result[z] := 0;
  end;
end;

function TBootGraph.FindBogenIndexOf(P: TRealPoint): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: double;
  a: double;
begin
  j := Length(Kurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := Abstand(P, Kurve[i]);
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

function TBootGraph.GetFreshRiggPoints: TIntRiggPoints;
var
  i: TRiggPoint;
begin
  for i := Low(TRiggPoint) to High(TRiggPoint) do
  begin
    Result[i, x] := 0;
    Result[i, y] := 0;
    Result[i, z] := 0;
  end;
end;

procedure TBootGraph.LoadFromIniFile(FileName: string);
var
  IniFile: TIniFile;
  S: string;
  i: TRiggPoint;
  iP: TIntRiggPoints;
begin
  iP := GetFreshRiggPoints;
  IniFile := TIniFile.Create(FileName);
  S := 'Koordinaten Rumpf';
  try
    with IniFile do
    begin
      iP[ooA0, x] := ReadInteger(S, 'A0x', Round(iP[ooA0, x]));
      iP[ooA0, y] := ReadInteger(S, 'A0y', Round(iP[ooA0, y]));
      iP[ooA0, z] := ReadInteger(S, 'A0z', Round(iP[ooA0, z]));
      iP[ooB0, x] := ReadInteger(S, 'B0x', Round(iP[ooB0, x]));
      iP[ooB0, y] := ReadInteger(S, 'B0y', Round(iP[ooB0, y]));
      iP[ooB0, z] := ReadInteger(S, 'B0z', Round(iP[ooB0, z]));
      iP[ooC0, x] := ReadInteger(S, 'C0x', Round(iP[ooC0, x]));
      iP[ooC0, y] := ReadInteger(S, 'C0y', Round(iP[ooC0, y]));
      iP[ooC0, z] := ReadInteger(S, 'C0z', Round(iP[ooC0, z]));
      iP[ooD0, x] := ReadInteger(S, 'D0x', Round(iP[ooD0, x]));
      iP[ooD0, y] := ReadInteger(S, 'D0y', Round(iP[ooD0, y]));
      iP[ooD0, z] := ReadInteger(S, 'D0z', Round(iP[ooD0, z]));
      iP[ooE0, x] := ReadInteger(S, 'E0x', Round(iP[ooE0, x]));
      iP[ooE0, y] := ReadInteger(S, 'E0y', Round(iP[ooE0, y]));
      iP[ooE0, z] := ReadInteger(S, 'E0z', Round(iP[ooE0, z]));
      iP[ooF0, x] := ReadInteger(S, 'F0x', Round(iP[ooF0, x]));
      iP[ooF0, y] := ReadInteger(S, 'F0y', Round(iP[ooF0, y]));
      iP[ooF0, z] := ReadInteger(S, 'F0z', Round(iP[ooF0, z]));

      S := 'Koordinaten Rigg';
      iP[ooA, x] := ReadInteger(S, 'Ax', Round(iP[ooA, x]));
      iP[ooA, y] := ReadInteger(S, 'Ay', Round(iP[ooA, y]));
      iP[ooA, z] := ReadInteger(S, 'Az', Round(iP[ooA, z]));
      iP[ooB, x] := ReadInteger(S, 'Bx', Round(iP[ooB, x]));
      iP[ooB, y] := ReadInteger(S, 'By', Round(iP[ooB, y]));
      iP[ooB, z] := ReadInteger(S, 'Bz', Round(iP[ooB, z]));
      iP[ooC, x] := ReadInteger(S, 'Cx', Round(iP[ooC, x]));
      iP[ooC, y] := ReadInteger(S, 'Cy', Round(iP[ooC, y]));
      iP[ooC, z] := ReadInteger(S, 'Cz', Round(iP[ooC, z]));
      iP[ooD, x] := ReadInteger(S, 'Dx', Round(iP[ooD, x]));
      iP[ooD, y] := ReadInteger(S, 'Dy', Round(iP[ooD, y]));
      iP[ooD, z] := ReadInteger(S, 'Dz', Round(iP[ooD, z]));
      iP[ooE, x] := ReadInteger(S, 'Ex', Round(iP[ooE, x]));
      iP[ooE, y] := ReadInteger(S, 'Ey', Round(iP[ooE, y]));
      iP[ooE, z] := ReadInteger(S, 'Ez', Round(iP[ooE, z]));
      iP[ooF, x] := ReadInteger(S, 'Fx', Round(iP[ooF, x]));
      iP[ooF, y] := ReadInteger(S, 'Fy', Round(iP[ooF, y]));
      iP[ooF, z] := ReadInteger(S, 'Fz', Round(iP[ooF, z]));
    end;
    for i := ooA0 to ooF0 do
    begin
      rP[i, x] := iP[i, x];
      rP[i, y] := iP[i, y];
      rP[i, z] := iP[i, z];
    end;
    for i := ooA to ooF do
    begin
      rP[i, x] := iP[i, x];
      rP[i, y] := iP[i, y];
      rP[i, z] := iP[i, z];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

end.
