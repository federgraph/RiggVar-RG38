unit RggTransformer;

interface

uses
 System.Math.Vectors,
  RggCalc,
  RggPolarKar,
  RggTypes;

type
  TRggGetFixPunkt = function: TRealPoint of object;

  { Base version with OnGetFixPunkt }
  TRggTransformer00 = class
  protected
    Updated: Boolean;
    FFixPoint: TRiggPoint;
    FZoom: single;
    FFixPunkt: TRealPoint;
    FTransformedFixPunkt: TRealPoint;
    FOnGetFixPunkt: TRggGetFixPunkt;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(const Value: single);
    procedure SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
  public
    Rotator: TPolarKar; // injected, not owned
    constructor Create;
    property Zoom: single read FZoom write SetZoom;
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

  { version with TMatrix3D }
  TRggTransformer3D = class(TRggTransformer00)
  private
    procedure BuildMatrix;
  public
    mat3D: TMatrix3D;
    function TransformPoint(p: TRealPoint): TRealPoint;
  end;

  TRggTransformer = TRggTransformer3D;

implementation


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

procedure TRggTransformer00.SetZoom(const Value: single);
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
    -FTransformedFixPunkt.X,
    -FTransformedFixPunkt.Y,
    -FTransformedFixPunkt.Z
  );
  mt := TMatrix3D.CreateTranslation(pt);

  ps := TPoint3D.Create(Zoom, Zoom, Zoom);
  ms := TMatrix3D.CreateScaling(ps);

  mr := Rotator.Mat;

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

  p1 := TPoint3D.Create(p.X, p.Y, p.Z);
  p2 := p1 * mat3D;
  result.X := p2.X;
  result.Y := p2.Y;
  result.Z := p2.Z;
end;

end.
