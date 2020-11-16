unit RggTransformer;

interface

uses
  System.Types,
  System.Math.Vectors,
  RggPolarKar,
  RggTypes;

type
  TRggGetFixPunkt = function: TPoint3D of object;

  TRggTransformer = class
  private
    FOffset: TPointF;
    procedure SetOffset(AValue: TPointF);
    procedure BuildMatrix;
  protected
    Updated: Boolean;
    FFixPoint: TRiggPoint;
    FZoom: single;
    FFixPunkt: TPoint3D;
    FTransformedFixPunkt: TPoint3D;
    FOnGetFixPunkt: TRggGetFixPunkt;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(const Value: single);
    procedure SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
  public
    Rotator: TPolarKar; // injected, not owned
    Matrix: TMatrix3D;
    WantOffset: Boolean;
    function TransformPoint(p: TPoint3D): TPoint3D;
    constructor Create;
    property Zoom: single read FZoom write SetZoom;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property Offset: TPointF read FOffset write SetOffset;
    property TransformedFixPunkt: TPoint3D read FTransformedFixPunkt;
    property OnGetFixPunkt: TRggGetFixPunkt read FOnGetFixPunkt write SetOnGetFixPunkt;
  end;

implementation


{ TRggTransformer }

constructor TRggTransformer.Create;
begin
  FFixPoint := ooD0;
  FZoom := 0.05;
end;

procedure TRggTransformer.SetOffset(AValue: TPointF);
begin
  FOffset := AValue;
  Updated := False;
end;

procedure TRggTransformer.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  Updated := False;
end;

procedure TRggTransformer.SetOnGetFixPunkt(const Value: TRggGetFixPunkt);
begin
  FOnGetFixPunkt := Value;
end;

procedure TRggTransformer.SetZoom(const Value: single);
begin
  FZoom := Value;
  Updated := False;
end;

procedure TRggTransformer.BuildMatrix;
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

  Matrix := TMatrix3D.Identity;
  Matrix := Matrix * mr;
  Matrix := Matrix * mt;
  Matrix := Matrix * ms;

  if WantOffset then
  begin
    pt := TPoint3D.Create(
      Offset.X,
      0,
      -Offset.Y
    );
    mt := TMatrix3D.CreateTranslation(pt);
    Matrix := Matrix * mt;
  end;
end;

function TRggTransformer.TransformPoint(p: TPoint3D): TPoint3D;
var
  p1, p2: TPoint3D;
begin
  if not Updated then
    BuildMatrix;

  p1 := TPoint3D.Create(p.X, p.Y, p.Z);
  p2 := p1 * Matrix;
  result.X := p2.X;
  result.Y := p2.Y;
  result.Z := p2.Z;
end;

end.
