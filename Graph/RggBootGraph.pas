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
    function GetFreshRiggPoints: TIntRiggPoints;
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
    Kurve[j].X := rP[ooD0].X - tempL * temp1 + Value[k] * temp2;
    Kurve[j].Y := 0;
    Kurve[j].Z := rP[ooD0].Z + tempL * temp3 + Value[k] * temp4;
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

function TBootGraph.GetFreshRiggPoints: TIntRiggPoints;
var
  i: TRiggPoint;
begin
  for i := Low(TRiggPoint) to High(TRiggPoint) do
  begin
    result[i] := TPoint3D.Zero;
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
      iP[ooA0].X := ReadInteger(S, 'A0x', Round(iP[ooA0].X));
      iP[ooA0].Y := ReadInteger(S, 'A0y', Round(iP[ooA0].Y));
      iP[ooA0].Z := ReadInteger(S, 'A0z', Round(iP[ooA0].Z));
      iP[ooB0].X := ReadInteger(S, 'B0x', Round(iP[ooB0].X));
      iP[ooB0].Y := ReadInteger(S, 'B0y', Round(iP[ooB0].Y));
      iP[ooB0].Z := ReadInteger(S, 'B0z', Round(iP[ooB0].Z));
      iP[ooC0].X := ReadInteger(S, 'C0x', Round(iP[ooC0].X));
      iP[ooC0].Y := ReadInteger(S, 'C0y', Round(iP[ooC0].Y));
      iP[ooC0].Z := ReadInteger(S, 'C0z', Round(iP[ooC0].Z));
      iP[ooD0].X := ReadInteger(S, 'D0x', Round(iP[ooD0].X));
      iP[ooD0].Y := ReadInteger(S, 'D0y', Round(iP[ooD0].Y));
      iP[ooD0].Z := ReadInteger(S, 'D0z', Round(iP[ooD0].Z));
      iP[ooE0].X := ReadInteger(S, 'E0x', Round(iP[ooE0].X));
      iP[ooE0].Y := ReadInteger(S, 'E0y', Round(iP[ooE0].Y));
      iP[ooE0].Z := ReadInteger(S, 'E0z', Round(iP[ooE0].Z));
      iP[ooF0].X := ReadInteger(S, 'F0x', Round(iP[ooF0].X));
      iP[ooF0].Y := ReadInteger(S, 'F0y', Round(iP[ooF0].Y));
      iP[ooF0].Z := ReadInteger(S, 'F0z', Round(iP[ooF0].Z));

      S := 'Koordinaten Rigg';
      iP[ooA].X := ReadInteger(S, 'Ax', Round(iP[ooA].X));
      iP[ooA].Y := ReadInteger(S, 'Ay', Round(iP[ooA].Y));
      iP[ooA].Z := ReadInteger(S, 'Az', Round(iP[ooA].Z));
      iP[ooB].X := ReadInteger(S, 'Bx', Round(iP[ooB].X));
      iP[ooB].Y := ReadInteger(S, 'By', Round(iP[ooB].Y));
      iP[ooB].Z := ReadInteger(S, 'Bz', Round(iP[ooB].Z));
      iP[ooC].X := ReadInteger(S, 'Cx', Round(iP[ooC].X));
      iP[ooC].Y := ReadInteger(S, 'Cy', Round(iP[ooC].Y));
      iP[ooC].Z := ReadInteger(S, 'Cz', Round(iP[ooC].Z));
      iP[ooD].X := ReadInteger(S, 'Dx', Round(iP[ooD].X));
      iP[ooD].Y := ReadInteger(S, 'Dy', Round(iP[ooD].Y));
      iP[ooD].Z := ReadInteger(S, 'Dz', Round(iP[ooD].Z));
      iP[ooE].X := ReadInteger(S, 'Ex', Round(iP[ooE].X));
      iP[ooE].Y := ReadInteger(S, 'Ey', Round(iP[ooE].Y));
      iP[ooE].Z := ReadInteger(S, 'Ez', Round(iP[ooE].Z));
      iP[ooF].X := ReadInteger(S, 'Fx', Round(iP[ooF].X));
      iP[ooF].Y := ReadInteger(S, 'Fy', Round(iP[ooF].Y));
      iP[ooF].Z := ReadInteger(S, 'Fz', Round(iP[ooF].Z));
    end;
    for i := ooA0 to ooF0 do
    begin
      rP[i] := iP[i];
    end;
    for i := ooA to ooF do
    begin
      rP[i] := iP[i];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

end.
