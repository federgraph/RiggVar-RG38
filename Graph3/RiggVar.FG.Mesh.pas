unit RiggVar.FG.Mesh;

interface

uses
  System.Classes,
  System.Types,
  FMX.Types3D,
  FMX.Objects3D,
  RiggVar.FG.MeshData420;

type
  THullMesh = class(TCustomMesh)
  public
    MeshData: THullMeshData;
    FederModel_FT1: single;
    FederModel_FT2: single;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitMesh;
    procedure InitData;
    procedure InitTexko;
    procedure UpdateScale;
    procedure UpdatePosition;
  end;

implementation

uses
  RiggVar.FG.Rota;

{ THullMesh }

constructor THullMesh.Create(AOwner: TComponent);
begin
  inherited;
  MeshData := THullMeshData.Create;
  WrapMode := TMeshWrapMode.Original;
end;

destructor THullMesh.Destroy;
begin
  MeshData.Free;
  inherited;
end;

procedure THullMesh.InitMesh;
begin
  InitData;
  UpdateScale;
  UpdatePosition;
end;

procedure THullMesh.UpdateScale;
begin
  Scale.X := 1.0 / TRotaForm3.RggGlobalScale;
  Scale.Y := 1.0 / TRotaForm3.RggGlobalScale;
  Scale.Z := 1.0 / TRotaForm3.RggGlobalScale;

  if not (WrapMode = TMeshWrapMode.Original) then
  begin
    Width := MeshData.Width;
    Height := MeshData.Height;
    Depth := MeshData.Depth;
  end;
end;

procedure THullMesh.UpdatePosition;
begin
  if WrapMode = TMeshWrapMode.Original then
  begin
    Position.X := -TRotaForm3.RggGlobalOffsetX / TRotaForm3.RggGlobalScale;
    Position.Y := -TRotaForm3.RggGlobalOffsetY / TRotaForm3.RggGlobalScale;
    Position.Z := -TRotaForm3.RggGlobalOffsetZ / TRotaForm3.RggGlobalScale;
  end
  else
  begin
    Position.X := ( (MeshData.xmin + MeshData.xmax) / 2 - TRotaForm3.RggGlobalOffsetX) / TRotaForm3.RggGlobalScale;
    Position.Y := ( (MeshData.ymin + MeshData.ymax) / 2 - TRotaForm3.RggGlobalOffsetY) / TRotaForm3.RggGlobalScale;
    Position.Z := ( (MeshData.zmin + MeshData.zmax) / 2 - TRotaForm3.RggGlobalOffsetZ) / TRotaForm3.RggGlobalScale;
  end;
end;

{$Hints Off}
procedure THullMesh.InitData;
var
  i, j: Integer;
  VB: TVertexBuffer;
  IB: TIndexBuffer;
  MD: THullMeshData;
begin
  MeshData.InitMesh;

  Data.Clear;
  VB := Data.VertexBuffer;
  IB := Data.IndexBuffer;

  MD := MeshData;

  VB.Length := MD.VertexCount;

  for i := 0 to MD.VertexCount-1 do
  begin
    VB.Vertices[i] := MD.G[i];
  end;

  InitTexko;

  IB.Length := MD.TriangleCount * 3;
  for i := 0 to MD.TriangleCount-1 do
  begin
    j := i * 3;
    IB[j] := MD.T1[i];
    IB[j+1] := MD.T2[i];
    IB[j+2] := MD.T3[i];
  end;

  Data.CalcFaceNormals;
end;

procedure THullMesh.InitTexko;
var
  i: Integer;
  te1, te2: single;
  u, v: single;
  MD: THullMeshData;
begin
  MD := MeshData;
  te1 := FederModel_FT1;
  te2 := 10 * FederModel_FT2;
  for i := 0 to MD.VertexCount-1 do
  begin
    u := (MD.G[i].X + te1) / te2;
    v := (MD.G[i].Z + te1) / te2;
    Data.VertexBuffer.TexCoord0[i] := PointF(u, v);
  end;
end;

{$Hints On}

end.
