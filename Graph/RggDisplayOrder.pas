unit RggDisplayOrder;

interface

uses
  RggTypes,
  RggDisplayTypes,
  RiggVar.FB.Classes,
  System.SysUtils,
  System.Classes,
  System.Math.Vectors;

type
  TDisplayOrderReport = (
    dorFace,
    dorCross,
    dorLook,
    dorEdgeSort,
    dorHull,
    dorSaling,
    dorTempList
  );

  TDisplayOrderReports = set of TDisplayOrderReport;
  TDisplayOrderReportNames = array[TDisplayOrderReport] of string;

const
  ReportNames: TDisplayOrderReportNames = (
    'Face',
    'Cross',
    'Look',
    'EdgeSort',
    'Hull',
    'Saling',
    'TempList'
  );

type

  TRggFrame = class; // forward declaration

  TRggVert = class
  public
    oo: TRiggPoint;
    Name: string;
    Point: TRealPoint;
    constructor Create(o: TRiggPoint);
  end;

  TRggEdge = class
  private
    function GetReverseEdge: TRiggEdge;
    function GetDisplayEdge: TDisplayEdge;
  public
    ee: TRiggEdge;
    oo1: TRiggPoint;
    oo2: TRiggPoint;
    Vert1: TRggVert;
    Vert2: TRggVert;
    V1: TPoint3D;
    V2: TPoint3D;
    Direction: TPoint3D;
    constructor Create(Aee: TRiggEdge; Ao1, Ao2: TRiggPoint);
    procedure Init(Frame: TRggFrame);
    procedure Update;
    property ReverseEdge: TRiggEdge read GetReverseEdge;
    property DisplayEdge: TDisplayEdge read GetDisplayEdge;
  end;

  TRggFace = class
  public
    ff: TRiggFace;
    ee1: TRiggEdge;
    ee2: TRiggEdge;
    ee3: TRiggEdge;
    Edge1: TRggEdge;
    Edge2: TRggEdge;
    Edge3: TRggEdge;
    IsFrontFacing: Boolean;
    E1: TPoint3D;
    E2: TPoint3D;
    E3: TPoint3D;
    SortedEdges: array[0..2] of TRggEdge;
    EdgeCount: Integer;
    constructor Create(Aff: TRiggFace; Ae1, Ae2, Ae3: TRiggEdge);
    procedure Init(Frame: TRggFrame);
    procedure Update;
    procedure SortEdges;
    procedure CheckOrientation;
  end;

  TRggSortedEdgeHelper = class
  private
    EdgeCount: Integer;
    Count: Integer;
    procedure AddFace(f: TRggFace);
  public
    PlacedEdges: set of TRiggEdge;
    SortedEdges: array[0..5] of TRggEdge;
    constructor Create;
    procedure Reset;
    procedure Add(e: TRggEdge);
  end;

  TRggTetra = class
  private
    EdgeHelper: TRggSortedEdgeHelper;
    procedure UpdateHull;
    procedure UpdateSaling;
    procedure SortFaces;
    procedure CollectEdges;
  public
    Name: string;
    DisplayGroup: TDisplayGroup;
    ff1: TRiggFace;
    ff2: TRiggFace;
    ff3: TRiggFace;
    ff4: TRiggFace;
    Face1: TRggFace;
    Face2: TRggFace;
    Face3: TRggFace;
    Face4: TRggFace;
    Faces: array[0..3] of TRggFace;
    SortedFaces: array[0..3] of TRggFace;
    SortedEdges: array[0..5] of TRggEdge;
    EdgeCount: Integer;
    FrontFacingCount: Integer;
    BackFacingCount: Integer;
    SwapIndex: Integer;
    constructor Create(Af1, Af2, Af3, Af4: TRiggFace);
    destructor Destroy; override;
    procedure Init(Frame: TRggFrame);
    procedure Update;
  end;

  TRggEdgeGroup = class
  public
    ee1: TRiggEdge;
    ee2: TRiggEdge;
    ee3: TRiggEdge;
    ee4: TRiggEdge;
    ee5: TRiggEdge;
    Edge1: TRggEdge;
    Edge2: TRggEdge;
    Edge3: TRggEdge;
    Edge4: TRggEdge;
    Edge5: TRggEdge;
    SortedEdges: array[0..4] of TRggEdge;
    EdgeCount: Integer;
    procedure Init(Frame: TRggFrame);
    procedure Update;
  end;

  TRggFrame = class
  private
    FEdgeCount: Integer;
    FKoordinaten: TRealRiggPoints;
    procedure SetKoordinaten(const Value: TRealRiggPoints);
    procedure InitStructure;
    procedure PlaceEdges;
    procedure PlaceSalingTetra(Index: Integer);
    procedure PlaceHullTetra(Index, GapIndex, GapCount: Integer);
    procedure ClearTempList;
  protected
    SalingTetraFirst: Boolean;
    HullTetraFirst: Boolean;
    LookingFromAbove: Boolean;
    LookingFromStb: Boolean;
    LookingFromBehind: Boolean;
    LookingFromTop: Boolean;
  public
    Rods: TRggEdgeGroup;
    HullTetra: TRggTetra;
    SalingTetra: TRggTetra;

    Verts: array[TRiggPoint] of TRggVert;
    Edges: array[TRiggEdge] of TRggEdge;
    Faces: array[TRiggFace] of TRggFace;

    TempList: array[0..40] of TDisplayEdge;

    WantController: Boolean;
    WantAchsen: Boolean;

    Reports: TDisplayOrderReports;

    constructor Create;
    destructor Destroy; override;

    procedure Sort;

    procedure FaceReport(ML: TStrings);
    procedure CrossReport(ML: TStrings);
    procedure LookReport(ML: TStrings);
    procedure EdgeSortReport(ML: TStrings);
    procedure HullReport(ML: TStrings);
    procedure SalingReport(ML: TStrings);
    procedure TempListReport(ML: TStrings);

    procedure Report(ML: TStrings);

    property Koordinaten: TRealRiggPoints read FKoordinaten write SetKoordinaten;
    property EdgeCount: Integer read FEdgeCount;
  end;

implementation

{ TRggVert }

constructor TRggVert.Create(o: TRiggPoint);
begin
  oo := o;
  Name := KoordTexte[oo];
end;

{ TRggEdge }

constructor TRggEdge.Create(Aee: TRiggEdge; Ao1, Ao2: TRiggPoint);
begin
  ee := Aee;
  oo1 := Ao1;
  oo2 := Ao2;
end;

function TRggEdge.GetDisplayEdge: TDisplayEdge;
begin
  case ee of
    A0D0: result := deA0D0;
    B0D0: result := deB0D0;
    C0D0: result := deC0D0;
    A0B0: result := deA0B0;
    B0C0: result := deB0C0;

    C0A0: result := deA0C0;

    A0A: result := deA0A;
    B0B: result := deB0B;
    C0C: result := deC0C;
    D0D: result := deD0D;
    E0E: result := deE0E;

    AC: result := deAC;
    BC: result := deBC;
    DC: result := deCD;

    AB: result := deAB;
    AD: result := deAD;
    BD: result := deBD;

    CF: result := deCF;

    { reverse edges }
    D0A0: result := deA0D0;
    D0B0: result := deB0D0;
    D0C0: result := deC0D0;
    B0A0: result := deA0B0;
    C0B0: result := deB0C0;
    A0C0: result := deA0C0;

    AA0: result := deA0A;
    BB0: result := deB0B;
    CC0: result := deC0C;
    DD0: result := deD0D;
    EE0: result := deE0E;

    CA: result := deAC;
    CB: result := deBC;
    CD: result := deCD;
    BA: result := deAB;
    DA: result := deAD;
    DB: result := deBD;

    FC: result := deCF;

    else
      result := deNone;
  end;
end;

function TRggEdge.GetReverseEdge: TRiggEdge;
begin
  case ee of
    A0D0: result := D0A0;
    B0D0: result := D0B0;
    C0D0: result := D0C0;
    A0B0: result := B0A0;
    B0C0: result := C0B0;
    C0A0: result := A0C0;

    A0A: result := AA0;
    B0B: result := BB0;
    C0C: result := CC0;
    D0D: result := DD0;
    E0E: result := EE0;

    AC: result := CA;
    BC: result := CB;
    DC: result := CD;
    AB: result := BA;
    AD: result := DA;
    BD: result := DB;

    CF: result := FC;

    D0A0: result := A0D0;
    D0B0: result := B0D0;
    D0C0: result := C0D0;
    B0A0: result := A0B0;
    C0B0: result := B0C0;
    A0C0: result := C0A0;

    AA0: result := A0A;
    BB0: result := B0B;
    CC0: result := C0C;
    DD0: result := D0D;
    EE0: result := E0E;

    CA: result := AC;
    CB: result := BC;
    CD: result := CD;
    BA: result := AB;
    DA: result := AD;
    DB: result := BD;

    FC: result := CF;

    else
      result := ee;
  end;
end;

procedure TRggEdge.Init(Frame: TRggFrame);
begin
  Vert1 := Frame.Verts[oo1];
  Vert2 := Frame.Verts[oo2];
end;

procedure TRggEdge.Update;
begin
  V1 :=  Vert1.Point;
  V2 := Vert2.Point;
  Direction := (V2-V1).Normalize;
end;

{ TRggFace }

procedure TRggFace.CheckOrientation;
begin
  IsFrontFacing := Edge1.Direction.CrossProduct(Edge2.Direction).Y < 0;
end;

constructor TRggFace.Create(Aff: TRiggFace; Ae1, Ae2, Ae3: TRiggEdge);
begin
  ff := Aff;
  ee1 := Ae1;
  ee2 := Ae2;
  ee3 := Ae3;
  EdgeCount := 3;
end;

procedure TRggFace.Init(Frame: TRggFrame);
begin
  Edge1 := Frame.Edges[ee1];
  Edge2 := Frame.Edges[ee2];
  Edge3 := Frame.Edges[ee3];

  SortedEdges[0] := Edge1;
  SortedEdges[1] := Edge2;
  SortedEdges[2] := Edge3;
end;

procedure TRggFace.SortEdges;
var
  cr12: TRggLinePair;
  cr23: TRggLinePair;
  cr31: TRggLinePair;
begin
  cr12.L1.A.P := Edge1.Vert1.Point;
  cr12.L1.B.P := Edge1.Vert2.Point;
  cr12.L2.A.P := Edge2.Vert1.Point;
  cr12.L2.B.P := Edge2.Vert2.Point;

  cr23.L1.A.P := Edge2.Vert1.Point;
  cr23.L1.B.P := Edge2.Vert2.Point;
  cr23.L2.A.P := Edge3.Vert1.Point;
  cr23.L2.B.P := Edge3.Vert2.Point;

  cr31.L1.A.P := Edge3.Vert1.Point;
  cr31.L1.B.P := Edge3.Vert2.Point;
  cr31.L2.A.P := Edge1.Vert1.Point;
  cr31.L2.B.P := Edge1.Vert2.Point;

  if cr12.CompareCommon > 0 then
  begin
    if cr23.CompareCommon > 0 then
    begin
      SortedEdges[0] := Edge3;
      SortedEdges[1] := Edge2;
      SortedEdges[2] := Edge1;
    end
    else
    begin
      SortedEdges[0] := Edge2;
      SortedEdges[1] := Edge3;
      SortedEdges[2] := Edge1;
    end;
  end
  else
  begin
    if cr23.CompareCommon > 0 then
    begin
      SortedEdges[0] := Edge1;
      SortedEdges[1] := Edge3;
      SortedEdges[2] := Edge2;
    end
    else
    begin
      SortedEdges[0] := Edge1;
      SortedEdges[1] := Edge2;
      SortedEdges[2] := Edge3;
    end;
  end;
end;

procedure TRggFace.Update;
begin
  CheckOrientation;
  SortEdges;
end;

{ TRggSortedEdgeList }

constructor TRggSortedEdgeHelper.Create;
begin
  EdgeCount := 6;
end;

procedure TRggSortedEdgeHelper.Reset;
var
  i: Integer;
begin
  Count := 0;
  PlacedEdges := [];
  for i := 0 to 5 do
  begin
    SortedEdges[i] := nil;
  end;
end;

procedure TRggSortedEdgeHelper.AddFace(f: TRggFace);
var
  e: TRggEdge;
begin
  for e in f.SortedEdges do
    Add(e);
end;

procedure TRggSortedEdgeHelper.Add(e: TRggEdge);
var
  ee: TRiggEdge;
  re: TRiggEdge;
  b: Boolean;
begin
  if Count >= EdgeCount then
    Exit;

  ee := e.ee;
  re := e.ReverseEdge;
  b := (ee in PlacedEdges) or (re in PlacedEdges);

  if not b then
  begin
    SortedEdges[Count] := e;
    Include(PlacedEdges, ee);
    Include(PlacedEdges, re);
    Inc(Count);
  end;
end;

{ TRggTetra }

constructor TRggTetra.Create(Af1, Af2, Af3, Af4: TRiggFace);
begin
  ff1 := Af1;
  ff2 := Af2;
  ff3 := Af3;
  ff4 := Af4;
  EdgeCount := 6;
  EdgeHelper := TRggSortedEdgeHelper.Create;
end;

destructor TRggTetra.Destroy;
begin
  EdgeHelper.Free;
  inherited;
end;

procedure TRggTetra.Init(Frame: TRggFrame);
begin
  Face1 := Frame.Faces[ff1];
  Face2 := Frame.Faces[ff2];
  Face3 := Frame.Faces[ff3];
  Face4 := Frame.Faces[ff4];

  Faces[0] := Face1;
  Faces[1] := Face2;
  Faces[2] := Face3;
  Faces[3] := Face4;

  SortedFaces[0] := Face1;
  SortedFaces[1] := Face2;
  SortedFaces[2] := Face3;
  SortedFaces[3] := Face4;

  if DisplayGroup = dgHullTetra then
  begin
  { HullTetra := TRggTetra.Create(A0B0D0, B0C0D0, C0A0D0, B0A0C0); }
//  Faces[A0B0D0] := TRggFace.Create(A0B0D0, A0B0, B0D0, D0A0);
//  Faces[B0C0D0] := TRggFace.Create(B0C0D0, B0C0, C0D0, D0B0);
//  Faces[C0A0D0] := TRggFace.Create(C0A0D0, C0A0, A0D0, D0C0);
//  Faces[B0A0C0] := TRggFace.Create(B0A0C0, B0A0, A0C0, C0B0);
    SortedEdges[0] := Face1.Edge2; // B0D0
    SortedEdges[1] := Face2.Edge2; // B0C0
    SortedEdges[2] := Face3.Edge2; // C0A0
    SortedEdges[3] := Face1.Edge1; // A0B0
    SortedEdges[4] := Face2.Edge1; // B0C0
    SortedEdges[5] := Face4.Edge2; // A0C0
  end;

  if DisplayGroup = dgSalingTetra then
  begin
  { SalingTetra := TRggTetra.Create(ABD, BAC, ADC, DBC); }
//  Faces[ABD] := TRggFace.Create(ABD, AB, BD, DA);
//  Faces[BAC] := TRggFace.Create(BAC, BA, AC, CB);
//  Faces[ADC] := TRggFace.Create(ADC, AD, DC, CA);
//  Faces[DBC] := TRggFace.Create(DBC, DB, BC, CD);
    SortedEdges[0] := Face1.Edge1; // AB
    SortedEdges[1] := Face1.Edge2; // BD
    SortedEdges[2] := Face1.Edge3; // DA
    SortedEdges[3] := Face2.Edge2; // AC
    SortedEdges[4] := Face4.Edge2; // BC
    SortedEdges[5] := Face3.Edge2; // DA
  end;

end;

procedure TRggTetra.Update;
begin
  SortFaces;
  case DisplayGroup of
    dgHullTetra: UpdateHull;
    dgSalingTetra: UpdateSaling;
  end;
end;

procedure TRggTetra.UpdateHull;
var
  j: Integer;
  f: TRggFace;
  DeckFace: TRggFace;
begin
  EdgeHelper.Reset;
  DeckFace := Face4;
  SwapIndex := 6;

  for j := 0 to 3 do
  begin
    f := SortedFaces[j];
    if f.ff <> DeckFace.ff then
    begin
      if (SwapIndex = 6) and f.IsFrontFacing then
        SwapIndex := EdgeHelper.Count;
      EdgeHelper.AddFace(f);
    end;
  end;

  { to process three faces out of four is enough }
//  EdgeHelper.AddFace(DeckFace);
  Assert(EdgeHelper.Count = 6);

  CollectEdges;
end;

procedure TRggTetra.UpdateSaling;
var
  f: TRggFace;
begin
  EdgeHelper.Reset;
  for f in SortedFaces do
    EdgeHelper.AddFace(f);
  CollectEdges;
end;

procedure TRggTetra.CollectEdges;
var
  j: Integer;
begin
  for j := 0 to 5 do
  begin
    if EdgeHelper.SortedEdges[j] <> nil then
      SortedEdges[j] := EdgeHelper.SortedEdges[j];
  end;
end;

procedure TRggTetra.SortFaces;
var
  i: Integer;
  j: Integer;

  procedure IncJ;
  begin
    if i < 4 then
      Inc(j);
  end;

begin
  j := 0;

  BackFacingCount := 0;
  for i := 0 to 3 do
    if not Faces[i].IsFrontFacing then
    begin
      Inc(BackFacingCount);
      SortedFaces[j] := Faces[i];
      IncJ;
    end;

  FrontFacingCount := 0;
  for i := 0 to 3 do
    if Faces[i].IsFrontFacing then
    begin
      Inc(FrontFacingCount);
      SortedFaces[j] := Faces[i];
      IncJ;
    end;
end;

{ TRggEdgeGroup }

procedure TRggEdgeGroup.Init(Frame: TRggFrame);
begin
  Edge1 := Frame.Edges[ee1];
  Edge2 := Frame.Edges[ee2];
  Edge3 := Frame.Edges[ee3];
  Edge4 := Frame.Edges[ee4];
  Edge5 := Frame.Edges[ee5];

  EdgeCount := 5;

  SortedEdges[0] := Edge1;
  SortedEdges[1] := Edge2;
  SortedEdges[2] := Edge3;
  SortedEdges[3] := Edge4;
  SortedEdges[4] := Edge5;
end;

procedure TRggEdgeGroup.Update;
begin

end;

{ TRggFrame }

constructor TRggFrame.Create;
var
  oo: TRiggPoint;
begin
//  Reports := [
//    dorFace,
//    dorCross,
//    dorLook,
//    dorEdgeSort,
//    dorHull,
//    dorSaling,
//    dorTempList
//  ];

  Reports := [];

//  Reports := Reports + [dorFace];
//  Reports := Reports + [dorCross];
  Reports := Reports + [dorLook];
//  Reports := Reports + [dorEdgeSort];
  Reports := Reports + [dorHull];
//  Reports := Reports + [dorSaling];
//  Reports := Reports + [dorTempList];

  FEdgeCount := High(TempList) + 1;

  { Vertices }
  for oo := Low(TRiggPoint) to High(TRiggPoint) do
    Verts[oo] := TRggVert.Create(oo);

  { Edges }
  Edges[A0D0] := TRggEdge.Create(A0D0, ooA0, ooD0);
  Edges[B0D0] := TRggEdge.Create(B0D0, ooB0, ooD0);
  Edges[C0D0] := TRggEdge.Create(C0D0, ooC0, ooD0);

  Edges[A0B0] := TRggEdge.Create(A0B0, ooA0, ooB0);
  Edges[B0C0] := TRggEdge.Create(B0C0, ooB0, ooC0);
  Edges[C0A0] := TRggEdge.Create(C0A0, ooC0, ooA0);

  Edges[A0A] := TRggEdge.Create(A0A, ooA0, ooA);
  Edges[B0B] := TRggEdge.Create(B0B, ooB0, ooB);
  Edges[C0C] := TRggEdge.Create(C0C, ooC0, ooC);
  Edges[D0D] := TRggEdge.Create(D0D, ooD0, ooD);
  Edges[E0E] := TRggEdge.Create(E0E, ooE0, ooE);

  Edges[AC] := TRggEdge.Create(AC, ooA, ooC);
  Edges[BC] := TRggEdge.Create(BC, ooB, ooC);
  Edges[DC] := TRggEdge.Create(DC, ooD, ooC);

  Edges[AB] := TRggEdge.Create(AB, ooA, ooB);
  Edges[AD] := TRggEdge.Create(AD, ooA, ooD);
  Edges[BD] := TRggEdge.Create(BD, ooB, ooD);

  Edges[CF] := TRggEdge.Create(CF, ooC, ooF);

  { reverse edges }
  Edges[D0A0] := TRggEdge.Create(D0A0, ooD0, ooA0);
  Edges[D0B0] := TRggEdge.Create(D0B0, ooD0, ooB0);
  Edges[D0C0] := TRggEdge.Create(D0C0, ooD0, ooC0);

  Edges[B0A0] := TRggEdge.Create(B0A0, ooB0, ooA0);
  Edges[C0B0] := TRggEdge.Create(C0B0, ooC0, ooB0);
  Edges[A0C0] := TRggEdge.Create(A0C0, ooA0, ooC0);

  Edges[AA0] := TRggEdge.Create(AA0, ooA, ooA0);
  Edges[BB0] := TRggEdge.Create(BB0, ooB, ooB0);
  Edges[CC0] := TRggEdge.Create(CC0, ooC, ooC0);
  Edges[DD0] := TRggEdge.Create(DD0, ooD, ooD0);
  Edges[EE0] := TRggEdge.Create(EE0, ooE, ooE0);

  Edges[CA] := TRggEdge.Create(CA, ooC, ooA);
  Edges[CB] := TRggEdge.Create(CB, ooC, ooB);
  Edges[CD] := TRggEdge.Create(CD, ooC, ooD);

  Edges[BA] := TRggEdge.Create(BA, ooB, ooA);
  Edges[DA] := TRggEdge.Create(DA, ooD, ooA);
  Edges[DB] := TRggEdge.Create(DB, ooD, ooB);

  Edges[FC] := TRggEdge.Create(FC, ooF, ooC);

  { helper edges, for special faces }
  Edges[DP] := TRggEdge.Create(DP, ooD, ooP);
  Edges[PD] := TRggEdge.Create(PD, ooP, ooD);
  Edges[PC] := TRggEdge.Create(PC, ooP, ooC);
  Edges[CP] := TRggEdge.Create(CP, ooC, ooP);

  { Faces of HullTetra }
  Faces[A0B0D0] := TRggFace.Create(A0B0D0, A0B0, B0D0, D0A0);
  Faces[B0C0D0] := TRggFace.Create(B0C0D0, B0C0, C0D0, D0B0);
  Faces[C0A0D0] := TRggFace.Create(C0A0D0, C0A0, A0D0, D0C0);
  Faces[B0A0C0] := TRggFace.Create(B0A0C0, B0A0, A0C0, C0B0);

  { Faces of SalingTetra }
  Faces[ABD] := TRggFace.Create(ABD, AB, BD, DA);
  Faces[BAC] := TRggFace.Create(BAC, BA, AC, CB);
  Faces[ADC] := TRggFace.Create(ADC, AD, DC, CA);
  Faces[DBC] := TRggFace.Create(DBC, DB, BC, CD);

  { Special Faces, to support sorting }
  Faces[CDP] := TRggFace.Create(CDP, CD, DP, PC);

  Rods := TRggEdgeGroup.Create;
  Rods.ee1 := A0A;
  Rods.ee2:= B0B;
  Rods.ee3:= C0C;
  Rods.ee4:= D0D;
  Rods.ee5:= CF;

  HullTetra := TRggTetra.Create(A0B0D0, B0C0D0, C0A0D0, B0A0C0);
  HullTetra.Name := 'Hull';
  HullTetra.DisplayGroup := dgHullTetra;

  SalingTetra := TRggTetra.Create(ABD, BAC, ADC, DBC);
  SalingTetra.Name := 'Saling';
  SalingTetra.DisplayGroup := dgSalingTetra;

  InitStructure;

  PlaceEdges;
end;

destructor TRggFrame.Destroy;
var
  oo: TRiggPoint;
  ee: TRiggEdge;
  ff: TRiggFace;
begin
  for oo := Low(TRiggPoint) to High(TRiggPoint) do
    Verts[oo].Free;
  for ee := Low(TRiggEdge) to High(TRiggEdge) do
    Edges[ee].Free;
  for ff := Low(TRiggFace) to High(TRiggFace) do
    Faces[ff].Free;

  Rods.Free;
  HullTetra.Free;
  SalingTetra.Free;
  inherited;
end;

procedure TRggFrame.InitStructure;
var
  ee: TRiggEdge;
  ff: TRiggFace;
begin
  for ee := Low(TRiggEdge) to High(TRiggEdge) do
    Edges[ee].Init(Self);
  for ff := Low(TRiggFace) to High(TRiggFace) do
    Faces[ff].Init(Self);

  Rods.Init(Self);
  HullTetra.Init(Self);
  SalingTetra.Init(Self);
end;

procedure TRggFrame.SetKoordinaten(const Value: TRealRiggPoints);
var
  oo: TRiggPoint;
  ee: TRiggEdge;
  ff: TRiggFace;
begin
  FKoordinaten := Value;
  for oo := Low(TRiggPoint) to High(TRiggPoint) do
    Verts[oo].Point := FKoordinaten.V[oo];

  for ee := Low(TRiggEdge) to High(TRiggEdge) do
    Edges[ee].Update;

  for ff := Low(TRiggFace) to High(TRiggFace) do
    Faces[ff].Update;

  SalingTetra.Update;
  HullTetra.Update;
  Rods.Update;
end;

procedure TRggFrame.FaceReport(ML: TStrings);
var
  f: TRggFace;
begin
  ML.Add('');
  ML.Add('FrontFacing:');
  for f in Faces do
    if f.IsFrontFacing then
      ML.Add(FaceNames[f.ff]);

  ML.Add('');
  ML.Add('BackFacing:');
  for f in Faces do
    if not f.IsFrontFacing then
      ML.Add(FaceNames[f.ff]);
end;

procedure TRggFrame.CrossReport(ML: TStrings);
var
  e1, e2: TRggEdge;
  s1, s2: string;
  p: TPoint3D;
begin
  ML.Add('');

  e1 := Edges[A0B0];
  e2 := Edges[B0C0];
  s1 := EdgeNames[e1.ee];
  s2 := EdgeNames[e2.ee];
  p := e1.Direction.CrossProduct(e2.Direction);
  ML.Add(Format('%s x %s = (%.2f, %.2f, %.2f)', [s1, s2, p.X, p.Y, p.Z]));

  e1 := Edges[B0A0];
  e2 := Edges[A0C0];
  s1 := EdgeNames[e1.ee];
  s2 := EdgeNames[e2.ee];
  p := e1.Direction.CrossProduct(e2.Direction);
  ML.Add(Format('%s x %s = (%.2f, %.2f, %.2f)', [s1, s2, p.X, p.Y, p.Z]));
end;

procedure TRggFrame.LookReport(ML: TStrings);
begin
  ML.Add('');
  ML.Add(Format('LookingFromAbove = %s', [BoolStr[LookingFromAbove]]));
  ML.Add(Format('LookingFromBehind = %s', [BoolStr[LookingFromBehind]]));
  ML.Add(Format('LookingFromStb = %s', [BoolStr[LookingFromStb]]));
  ML.Add(Format('LookingFromTop = %s', [BoolStr[LookingFromTop]]));
end;

procedure TRggFrame.EdgeSortReport(ML: TStrings);
var
  cr: TRggFace;
begin
  cr := Faces[ABD];
  ML.Add('');
  ML.Add(Format('ABD.SortedEdges[0] = %s', [EdgeNames[cr.SortedEdges[0].ee]]));
  ML.Add(Format('ABD.SortedEdges[1] = %s', [EdgeNames[cr.SortedEdges[1].ee]]));
  ML.Add(Format('ABD.SortedEdges[2] = %s', [EdgeNames[cr.SortedEdges[2].ee]]));
end;

procedure TRggFrame.HullReport(ML: TStrings);
begin
  ML.Add('');
  ML.Add(Format('HullTetra.BackFacingCount = %d', [HullTetra.BackFacingCount]));
  ML.Add(Format('HullTetra.FrontFacingCount = %d', [HullTetra.FrontFacingCount]));
  ML.Add(Format('HullTetra.SwapIndex = %d', [HullTetra.SwapIndex]));
end;

procedure TRggFrame.SalingReport(ML: TStrings);
begin
  ML.Add('');
  ML.Add(Format('SalingTetra.BackFacingCount = %d', [SalingTetra.BackFacingCount]));
  ML.Add(Format('SalingTetra.FrontFacingCount = %d', [SalingTetra.FrontFacingCount]));
//  ML.Add(Format('ADC.IsFrontFacing = %s', [BoolStr[Faces[ADC].IsFrontFacing]]));
//  ML.Add(Format('DBC.IsFrontFacing = %s', [BoolStr[Faces[DBC].IsFrontFacing]]));
//  ML.Add(Format('BAC.IsFrontFacing = %s', [BoolStr[Faces[BAC].IsFrontFacing]]));
//  ML.Add(Format('ABD.IsFrontFacing = %s', [BoolStr[Faces[ABD].IsFrontFacing]]));
  ML.Add(Format('LookingFromTop = %s', [BoolStr[LookingFromTop]]));
end;

procedure TRggFrame.TempListReport(ML: TStrings);
var
  i: Integer;
  de: TDisplayEdge;
begin
  ML.Add('');
  for i := 0 to 31 do
  begin
    de := TempList[i];
    ML.Add(Format('[%d] = %s', [i, DisplayEdgeNames[de]]));
  end;
end;

procedure TRggFrame.Report(ML: TStrings);
begin
  if dorTempList in Reports then
    TempListReport(ML);
  if dorHull in Reports then
  HullReport(ML);
  if dorSaling in Reports then
  SalingReport(ML);
  if dorEdgeSort in Reports then
  EdgeSortReport(ML);
  if dorLook in Reports then
  LookReport(ML);
  if dorCross in Reports then
    CrossReport(ML);
  if dorFace in Reports then
    FaceReport(ML);
end;

procedure TRggFrame.ClearTempList;
var
  j: Integer;
begin
  for j := 0 to EdgeCount - 1 do
  begin
    TempList[j] := deNone;
  end;
end;

procedure TRggFrame.PlaceSalingTetra(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to 5 do
    if SalingTetra.SortedEdges[i] <> nil then
      TempList[Index + i] := SalingTetra.SortedEdges[i].DisplayEdge;
end;

procedure TRggFrame.PlaceHullTetra(Index, GapIndex, GapCount: Integer);
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  j := Index;
  for i := 0 to 5 do
  begin
    if HullTetra.SortedEdges[i] <> nil then
    begin
      if (i = GapIndex) then
      begin
        for k := 0 to GapCount-1 do
        begin
          Inc(j);
        end;
      end;
      TempList[j] := HullTetra.SortedEdges[i].DisplayEdge;
      Inc(j);
    end;
  end;
end;

procedure TRggFrame.PlaceEdges;
var
  BackIndex: Integer;
  MiddleIndex: Integer;
  FrontIndex: Integer;
  TetraGapIndex: Integer;
  BackTetraIndex: Integer;
  FrontTetraIndex: Integer;
  b1, b2, b3: Boolean;
begin
  ClearTempList;

  HullTetraFirst := Faces[B0A0C0].IsFrontFacing;
  SalingTetraFirst := not HullTetraFirst;

  b1 := Faces[ADC].IsFrontFacing;
  b2 := Faces[DBC].IsFrontFacing;
  b3 := Faces[BAC].IsFrontFacing;

  LookingFromTop := b1 and b2 and b3;
  LookingFromAbove := Faces[B0A0C0].IsFrontFacing;
  LookingFromStb := Faces[CDP].IsFrontFacing;
  LookingFromBehind := Faces[BAC].IsFrontFacing;

  BackIndex := 1;
  BackTetraIndex := 7;
  MiddleIndex := 16;
  FrontTetraIndex := 21;
  FrontIndex := 29;

  TempList[0] := deFixPunkt;

  if SalingTetraFirst then
  begin
    PlaceSalingTetra(BackTetraIndex);
    PlaceHullTetra(FrontTetraIndex, HullTetra.SwapIndex, 2);
    TetraGapIndex := FrontTetraIndex + HullTetra.SwapIndex;
  end
  else // if HullTetraFirst then
  begin
    PlaceHullTetra(BackTetraIndex, HullTetra.SwapIndex, 2);
    PlaceSalingTetra(FrontTetraIndex);
    TetraGapIndex := BackTetraIndex + HullTetra.SwapIndex;
  end;

  if LookingFromBehind then
  begin
    if WantAchsen then
    begin
      TempList[FrontIndex + 2] := deNX;
      TempList[FrontIndex + 3] := deNY;
      TempList[FrontIndex + 4] := deNZ;
    end;

    if LookingFromStb then
    begin
      TempList[MiddleIndex + 1] := deA0a;
      TempList[MiddleIndex + 2] := deB0B;
    end
    else
    begin
      TempList[MiddleIndex + 2] := deA0A;
      TempList[MiddleIndex + 1] := deB0B;
    end;

    if WantController then
    begin
      TempList[TetraGapIndex] := deE0E;
    end;
    TempList[TetraGapIndex + 1] := deD0D;

    if LookingFromTop then
    begin
      TempList[FrontIndex + 1] := deC0C;
      TempList[FrontIndex] := deCF;
    end
    else if LookingFromAbove then
    begin
      TempList[MiddleIndex + 3] := deC0C;
      TempList[FrontIndex + 5] := deCF;
    end
    else
    begin
      TempList[BackIndex + 1] := deCF;
      TempList[BackIndex + 2] := deC0C;
    end;
  end

  else // if not LookingFromBehind then
  begin
    if WantAchsen then
    begin
      TempList[BackIndex + 1] := deNX;
      TempList[BackIndex + 2] := deNY;
      TempList[BackIndex + 3] := deNZ;
    end;

    if LookingFromStb then
    begin
      TempList[MiddleIndex + 1] := deA0a;
      TempList[MiddleIndex + 2] := deB0B;
    end
    else
    begin
      TempList[MiddleIndex + 2] := deA0A;
      TempList[MiddleIndex + 1] := deB0B;
    end;

    TempList[TetraGapIndex] := deD0D;
    if WantController then
    begin
      TempList[TetraGapIndex + 1] := deE0E;
    end;

    if LookingFromAbove then
    begin
      TempList[FrontIndex + 1] := deCF;
      TempList[FrontIndex + 2] := deC0C;
    end
    else
    begin
      TempList[BackIndex + 4] := deCF;
      TempList[BackIndex + 5] := deC0C;
    end;
  end;

end;

procedure TRggFrame.Sort;
begin
  PlaceEdges;
end;

end.
