unit RggTransformer;

interface

uses
 System.Math.Vectors,
  RggCalc,
  RggTypes,
  RggMatrix,
  RggPolarKar;

type
  TRggGetFixPunkt = function: TRealPoint of object;

  { Base version with OnGetFixPunkt }
  TRggTransformer00 = class
  protected
    Updated: Boolean;
    FFixPoint: TRiggPoint;
    FZoom: double;
    FFixPunkt: TRealPoint;
    FTransformedFixPunkt: TRealPoint;
    FOnGetFixPunkt: TRggGetFixPunkt;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(const Value: double);
    procedure SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
  public
    Rotator: TPolarKar; // injected, not owned
    constructor Create;
    property Zoom: double read FZoom write SetZoom;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property TransformedFixPunkt: TRealPoint read FTransformedFixPunkt;
    property OnGetFixPunkt: TRggGetFixPunkt read FOnGetFixPunkt write SetOnGetFixPunkt;
  end;

  { not used, FixPunkt not acessible }
  TRggTransformer01 = class(TRggTransformer00)
  protected
    procedure SetFixPunkt(const Value: TRealPoint);
  public
    procedure UpdateTransformedFixPunkt;
    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
  end;

  { using Matrix4x4, see Hull }
  TRggTransformer4x4 = class(TRggTransformer00)
  private
    procedure BuildMatrix;
  public
    Mat: TMatrix4x4;
    constructor Create;
    destructor Destroy; override;
    function TransformPoint(p: TRealPoint): TRealPoint;
  end;

  { version with TMatrix3D }
  TRggTransformer3D = class(TRggTransformer00)
  private
    procedure BuildMatrix;
  public
    mat3D: TMatrix3D;
    function TransformPoint(p: TRealPoint): TRealPoint;
  end;

  TRggTransformer = TRggTransformer4x4;

implementation

uses
  RggVector;

{ TRggTransformer00 }

constructor TRggTransformer00.Create;
begin
  FFixPoint := ooD0;
  FZoom := 0.05;
end;

procedure TRggTransformer00.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  Updated := False;
end;

procedure TRggTransformer00.SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
begin
  FOnGetFixPunkt := Value;
end;

procedure TRggTransformer00.SetZoom(const Value: double);
begin
  FZoom := Value;
  Updated := False;
end;

{ TRggTransformer01 }

procedure TRggTransformer01.SetFixPunkt(const Value: TRealPoint);
begin
  FFixPunkt := Value;
  Updated := False;
end;

procedure TRggTransformer01.UpdateTransformedFixPunkt;
begin
  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);
end;

{ TRggTransformer4x4 }

constructor TRggTransformer4x4.Create;
begin
  inherited;
  Mat := TMatrix4x4.Create;
end;

destructor TRggTransformer4x4.Destroy;
begin
  Mat.Free;
  inherited;
end;

procedure TRggTransformer4x4.BuildMatrix;
begin
  if Assigned(OnGetFixPunkt) then
    FFixPunkt := OnGetFixPunkt;

  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);

  Mat.Identity;
  Mat.Translate(
    -FTransformedFixPunkt[x],
    -FTransformedFixPunkt[y],
    -FTransformedFixPunkt[z]
  );
  Mat.Multiply(Rotator.Matrix);
  Mat.ScaleXYZ(Zoom, Zoom, Zoom);
end;

function TRggTransformer4x4.TransformPoint(p: TRealPoint): TRealPoint;
var
  pt: vec3;
begin
  if not Updated then
    BuildMatrix;

  pt.x := p[x];
  pt.y := p[y];
  pt.z := p[z];
  Mat.TransformPoint(pt);
  result[x] := pt.x;
  result[y] := pt.y;
  result[z] := pt.z;
end;

{ TRggTransformer3D }

procedure TRggTransformer3D.BuildMatrix;
var
  pt: TPoint3D;
  ps: TPoint3D;

  mt: TMatrix3D;
  ms: TMatrix3D;
  mr: TMatrix3D;
begin
  if Assigned(OnGetFixPunkt) then
    FFixPunkt := OnGetFixPunkt;

  FTransformedFixPunkt := Rotator.Rotiere(FFixPunkt);

  pt := TPoint3D.Create(
    -FTransformedFixPunkt[x],
    -FTransformedFixPunkt[y],
    -FTransformedFixPunkt[z]
  );
  mt := TMatrix3D.CreateTranslation(pt);

  ps := TPoint3D.Create(Zoom, Zoom, Zoom);
  ms := TMatrix3D.CreateScaling(ps);

  mr := Rotator.Mat.GetDelphiMatrix3D;

  mat3D := TMatrix3D.Identity;
  mat3D := mat3D * mr;
  mat3D := mat3D * mt;
  mat3D := mat3D * ms;
end;

function TRggTransformer3D.TransformPoint(p: TRealPoint): TRealPoint;
var
  p1, p2: TPoint3D;
begin
  if not Updated then
    BuildMatrix;

  p1 := TPoint3D.Create(p[x], p[y], p[z]);
  p2 := p1 * mat3D;
  result[x] := p2.X;
  result[y] := p2.Y;
  result[z] := p2.Z;
end;

end.
