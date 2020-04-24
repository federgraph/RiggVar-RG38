unit RggDisplayTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  RggTypes,
  RggCalc;

type
  TDisplayEdge = (
    deNone,

    deA0D0,
    deB0D0,
    deC0D0,

    deA0B0,
    deB0C0,
    deA0C0,

    deA0A,
    deB0B,
    deC0C,
    deD0D,

    deAC,
    deBC,
    deCD,

    deAB,
    deAD,
    deBD,

    deCF,

    deE0E,

    deNX,
    deNY,
    deNZ,

    deFixPunkt,
    deHullEdge,
    deMastFall,
    deHullFrame
  );

  TDisplayGroup = (
    dgSalingTetra,
    dgHullTetra
  );

  TRiggEdge = (
    A0D0,
    B0D0,
    C0D0,

    A0B0,
    B0C0,
    C0A0,

    A0A,
    B0B,
    C0C,
    D0D,
    E0E,

    AC,
    BC,
    DC,

    AB,
    AD,
    BD,

    CF,

    { reverse edges }
    D0A0,
    D0B0,
    D0C0,

    B0A0,
    C0B0,
    A0C0,

    AA0,
    BB0,
    CC0,
    DD0,
    EE0,

    CA,
    CB,
    CD,

    BA,
    DA,
    DB,

    FC,

    DP,
    PD,
    PC,
    CP
  );

  TRiggFace = (
    A0B0D0,
    B0C0D0,
    C0A0D0,
    B0A0C0,

    ABD,
    BAC,
    ADC,
    DBC,

    CDP
  );

  TEdgeNames = array[TRiggEdge] of string;
  TFaceNames = array[TRiggFace] of string;
  TDisplayEdgeNames = array[TDisplayEdge] of string;

const
  EdgeNames: TEdgeNames = (
    'A0D0',
    'B0D0',
    'C0D0',

    'A0B0',
    'B0C0',
    'C0A0',

    'A0A',
    'B0B',
    'C0C',
    'D0D',
    'E0E',

    'AC',
    'BC',
    'DC',

    'AB',
    'AD',
    'BD',

    'CF',

    'D0A0',
    'D0B0',
    'D0C0',

    'B0A0',
    'C0B0',
    'A0C0',

    'AA0',
    'BB0',
    'CC0',
    'DD0',
    'EE0',

    'CA',
    'CB',
    'CD',

    'BA',
    'DA',
    'DB',

    'FC',
    'DP',
    'PD',
    'PC',
    'CP'
  );

  FaceNames: TFaceNames = (
    'A0B0D0',
    'B0C0D0',
    'C0A0D0',
    'B0A0C0',

    'ABD',
    'BAC',
    'ADC',
    'DBC',

    'C0D0P0'
  );

  DisplayEdgeNames: TDisplayEdgeNames = (
    '---', //'deNone',

    'A0D0',
    'B0D0',
    'C0D0',

    'A0B0',
    'B0C0',
    'A0C0',

    'A0A',
    'B0B',
    'C0C',
    'D0D',

    'AC',
    'BC',
    'CD',

    'AB',
    'AD',
    'BD',

    'CF',

    'E0E',

    'NX',
    'NY',
    'NZ',

    'FixPunkt',
    'HullEdge',
    'MastFall',
    'HullFrame'
  );

type
  TLineSegmentCompareCase = (
    ccNone,
    ccNil,
    ccHardcodedAbove,
    ccHardcodedBelow,
    ccParallel,
    ccNoVisibleCrossing,
    ccTotallyAbove,
    ccTotallyBelow,
    ccTotallySame,
    ccCommonNone,
    ccCommonAbove,
    ccCommonBelow,
    ccCommonSame,
    ccAbove,
    ccBelow,
    ccSame,
    ccUnknown
  );

  TDisplayItemType = (
    diLine,
    diPolyLine,
    diEllipse
  );

  TRggPoint = record
    P: TRealPoint;
    function IsEqual(B: TRggPoint): Boolean;
    function Compare(Q: TRggPoint): Integer;
  end;

  TRggLine = record
    Name: string;
    A: TRggPoint;
    B: TRggPoint;
    function IsTotallyAbove(Other: TRggLine): Boolean;
    function IsTotallyBelow(Other: TRggLine): Boolean;
    function IsSame(Other: TRggLine): Boolean;
    function HasVisibleCrossing(SP: TRealPoint): Boolean;
    function ComputeSPY(SP: TRealPoint): double;
    procedure ReportData(ML: TStrings);
  end;

  TRggLinePair = record
    L1: TRggLine;
    L2: TRggLine;
    SP: TRealPoint;
    function HasCommonPoint: Boolean;
    function CompareCommon: Integer;
    function IsParallel: Boolean;
    function DoesNotHaveVisibleCrossing: Boolean;
    function CompareSPY: Integer;
    procedure ReportData(ML: TStrings);
    function CompareVV(v1, v2: TRealPoint): Integer;
  end;

implementation

const
  Eps = 0.0001;

{ TRggPoint }

function TRggPoint.Compare(Q: TRggPoint): Integer;
begin
  if P[y] > Q.P[y] then
    result := -1
  else if P[y] < Q.P[y] then
    result := 1
  else
    result := 0;
end;

function TRggPoint.IsEqual(B: TRggPoint): Boolean;
begin
  result :=
    (P[x] = B.P[x]) and
    (P[y] = B.P[y]) and
    (P[z] = B.P[z]);
end;

{ TRggLine }

function TRggLine.IsSame(Other: TRggLine): Boolean;
begin
  result := False;
  if A.IsEqual(Other.A) and B.IsEqual(Other.B) then
    result := True
  else if A.IsEqual(Other.B) and B.IsEqual(Other.A) then
    result := True;
end;

function TRggLine.IsTotallyAbove(Other: TRggLine): Boolean;
begin
  result :=
    (A.P[y] < Other.A.P[y]) and
    (A.P[y] < Other.B.P[y]) and
    (B.P[y] < Other.A.P[y]) and
    (B.P[y] < Other.B.P[y]);
end;

function TRggLine.IsTotallyBelow(Other: TRggLine): Boolean;
begin
  result :=
    (A.P[y] > Other.A.P[y]) and
    (A.P[y] > Other.B.P[y]) and
    (B.P[y] > Other.A.P[y]) and
    (B.P[y] > Other.B.P[y]);
end;

function TRggLine.HasVisibleCrossing(SP: TRealPoint): Boolean;
var
  vSP: TRealPoint;
  vAB: TRealPoint; // Rgg Vector 3D

  vABxz: TVector;
  vSPxz: TVector; // Delphi 2D Vectors
  lengthABxz, lengthSPxz: double;
  RatioSPtoAB, g: double;
begin
  result := False;

  vSP := vsub(SP, A.P);
  vAB := vsub(B.P, A.P);

  vABxz := TVector.Create(vAB[x], vAB[z]);
  lengthABxz := vABxz.Length;

  vSPxz := TVector.Create(vSP[x], vSP[z]);
  lengthSPxz := vSPxz.Length;

  if lengthABxz < Eps then
  begin
    Exit;
  end;

  RatioSPtoAB := lengthSPxz / lengthABxz;

  g := Abs(RatioSPtoAB);

  result := (g > Eps) and (g < 1-Eps);
end;

function TRggLine.ComputeSPY(SP: TRealPoint): double;
var
  vSP: TRealPoint;
  vAB: TRealPoint; // Rgg Vector 3D

  vABxz: TVector;
  vSPxz: TVector; // Delphi 2D Vectors
  lengthABxz, lengthSPxz: double;
  RatioSPtoAB, g: double;
begin
  result := (A.P[y] + B.P[y]) / 2;

  vSP := vsub(SP, A.P);
  vAB := vsub(B.P, A.P);

  vABxz := TVector.Create(vAB[x], vAB[z]);
  lengthABxz := vABxz.Length;

  vSPxz := TVector.Create(vSP[x], vSP[z]);
  lengthSPxz := vSPxz.Length;

  if lengthABxz < Eps then
  begin
    Exit;
  end;

  RatioSPtoAB := lengthSPxz / lengthABxz;

  g := RatioSPtoAB;

  if Sign(vAB[x]) <> Sign(vSP[x]) then
    g := -RatioSPtoAB;

  if Abs(g) > 10000 then
  begin
    { does not come in here }
    result := A.P[y];
    Exit;
  end;

  result := A.P[y] + g * vAB[y];
end;

procedure TRggLine.ReportData(ML: TStrings);
  procedure AddPoint(LN, PN: string; P: TRealPoint);
  begin
    ML.Add(Format('%s [%s] = (%.2f, %.2f, %.2f)', [PN, LN, P[x], P[y], P[z]]));
  end;
begin
  AddPoint(Name, 'A', A.P);
  AddPoint(Name, 'B', B.P);
end;

{ TRggLinePair }

function TRggLinePair.CompareVV(v1, v2: TRealPoint): Integer;
var
  m1, m2: TPoint3D;
  r: double;
begin
  m1 := TPoint3D.Create(v1[x], v1[y], v1[z]).Normalize;
  m2 := TPoint3D.Create(v2[x], v2[y], v2[z]).Normalize;
  r := m2.Y - m1.Y;
  if r > 0 then
    result := 1
  else if r < 0 then
    result := -1
  else
    result := 0;
end;

function TRggLinePair.CompareCommon: Integer;
var
  v1, v2: TRealPoint;
begin
  result := 0;
  if L1.A.IsEqual(L2.A) then
  begin
    v1 := vsub(L1.B.P, L1.A.P);
    v2 := vsub(L2.B.P, L2.A.P);
    result := CompareVV(v1, v2);
//    result := L1.B.Compare(L2.B)
  end
  else if L1.A.IsEqual(L2.B) then
  begin
    v1 := vsub(L1.B.P, L1.A.P);
    v2 := vsub(L2.A.P, L2.B.P);
    result := CompareVV(v1, v2);
//    result := L1.B.Compare(L2.A)
  end
  else if L1.B.IsEqual(L2.A) then
  begin
    v1 := vsub(L1.A.P, L1.B.P);
    v2 := vsub(L2.B.P, L2.A.P);
    result := CompareVV(v1, v2);
//    result := -L1.A.Compare(L2.B)
  end
  else if L1.B.IsEqual(L2.B) then
  begin
    v1 := vsub(L1.A.P, L1.B.P);
    v2 := vsub(L2.A.P, L2.B.P);
    result := CompareVV(v1, v2);
//    result := -L1.A.Compare(L2.A);
  end;
end;

function TRggLinePair.HasCommonPoint: Boolean;
begin
  result :=
    L1.A.IsEqual(L2.A) or
    L1.A.IsEqual(L2.B) or
    L1.B.IsEqual(L2.A) or
    L1.B.IsEqual(L2.B);
end;

function TRggLinePair.IsParallel: Boolean;
begin
  result := not SchnittGG(L1.A.P, L1.B.P, L2.A.P, L2.B.P, SP);
end;

function TRggLinePair.DoesNotHaveVisibleCrossing: Boolean;
begin
  result := not L1.HasVisibleCrossing(SP);
end;

procedure TRggLinePair.ReportData(ML: TStrings);
  procedure AddPoint(LN, PN: string; P: TRealPoint);
  begin
    ML.Add(Format('%s [%s] = (%.2f, %.2f, %.2f)', [PN, LN, P[x], P[y], P[z]]));
  end;
begin
  AddPoint(L1.Name, 'A', L1.A.P);
  AddPoint(L1.Name, 'B', L1.B.P);
  AddPoint(L2.Name, 'C', L2.A.P);
  AddPoint(L2.Name, 'D', L2.B.P);
end;

function TRggLinePair.CompareSPY: Integer;
var
  ya, yb, dy: double;
begin
  ya := L1.ComputeSPY(SP);
  yb := L2.ComputeSPY(SP);

  dy := yb - ya;

  if dy > 0 then
    result := 1
  else if dy < 0 then
    result := -1
  else
    result := 0;
end;

end.
