unit RiggVar.FG.MeshDataBase;

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
  System.Math.Vectors,
  System.Types,
  System.Generics.Collections;

type
  TMeshDataBase = class
  private
    function GetDepth: single;
    function GetHeight: single;
    function GetWidth: single;
    function GetCenter: TPoint3D;
  protected
    P: TPoint3D;
    u, v, w: double;
  public
    xmin, ymin, zmin: single;
    xmax, ymax, zmax: single;

    VertexCount: Integer;
    TriangleCount: Integer;

    G: array of TPoint3D;

    T1: array of Integer;
    T2: array of Integer;
    T3: array of Integer;

    H1: TList<Integer>;
    H2: TList<Integer>;
    H3: TList<Integer>;

    Inside: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure InitMesh; virtual; abstract;

    procedure UpdateSize(VBL, IBL: Integer);

    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property Depth: single read GetDepth;
    property Center: TPoint3D read GetCenter;
  end;

implementation

{ TMeshDataBase }

constructor TMeshDataBase.Create;
begin
  H1 := TList<Integer>.Create;
  H2 := TList<Integer>.Create;
  H3 := TList<Integer>.Create;
end;

destructor TMeshDataBase.Destroy;
begin
  H1.Free;
  H2.Free;
  H3.Free;
  inherited;
end;

function TMeshDataBase.GetDepth: single;
begin
  result := zmax - zmin;
end;

function TMeshDataBase.GetHeight: single;
begin
  result := ymax - ymin;
end;

function TMeshDataBase.GetWidth: single;
begin
  result := xmax - xmin;
end;

function TMeshDataBase.GetCenter: TPoint3D;
begin
  result := TPoint3D.Create((xmin + xmax) / 2, (ymin + ymax) / 2, (zmin + zmax) / 2);
end;

procedure TMeshDataBase.UpdateSize(VBL, IBL: Integer);
begin
  if VBL > VertexCount then
    SetLength(G, VBL);

  if IBL > TriangleCount then
  begin
    SetLength(T1, IBL);
    SetLength(T2, IBL);
    SetLength(T3, IBL);
  end;

  VertexCount := VBL;
  TriangleCount := IBL;
end;

end.
