unit RiggVar.FD.TransformHelper;

interface

uses
  System.Types,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  RiggVar.FD.RotationHelper,
  RiggVar.FD.Elements;

type
  TTransformHelper = class
  private
    FOnDrawToCanvas: TNotifyEvent;
    FOnShowRotation: TNotifyEvent;
    procedure SetOnDrawToCanvas(const Value: TNotifyEvent);
    procedure SetOnShowRotation(const Value: TNotifyEvent);
  public
    NewMatrix: TMatrix3D;
    AccuMatrix: TMatrix3D;

    RotationHelper: TRotationHelper;

    DirX: TPoint3D;
    DirY: TPoint3D;
    DirZ: TPoint3D;

    Offset: TPointF;
    Rotation: TPoint3D;
    ZoomDelta: single;

    CurrentDrawing: TRggDrawingBase;

    RotD: TPoint3D;
    RotR: TPoint3D;
    RotB: Boolean;

    IsRightMouseBtn: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure DoOnMouse(Shift: TShiftState; dx, dy: single);

    procedure BuildMatrix(mr: TMatrix3D);
    procedure BuildMatrixM;
    procedure ResetTransform;
    procedure InitTransform(mr: TMatrix3D);
    procedure UpdateTransform;
    function BuildMatrixG(NewFixPoint: TPoint3D): TMatrix3D;
    function BuildMatrixF: TMatrix3D;
    function BuildMatrixI: TMatrix3D;

    procedure GetEulerAngles; virtual;

    procedure Reset;
    procedure Draw;
    procedure DrawToCanvas;
    procedure ShowRotation(r: TPoint3D; b: Boolean);

    property OnDrawToCanvas: TNotifyEvent read FOnDrawToCanvas write SetOnDrawToCanvas;
    property OnShowRotation: TNotifyEvent read FOnShowRotation write SetOnShowRotation;
  end;

implementation

constructor TTransformHelper.Create;
begin
  ZoomDelta := 1;

  DirX := TPoint3D.Create(1, 0, 0);
  DirY := TPoint3D.Create(0, 1, 0);
  DirZ := TPoint3D.Create(0, 0, 1);

  RotationHelper := TRotationHelper.Create;
  ResetTransform;
end;

destructor TTransformHelper.Destroy;
begin
  RotationHelper.Free;
  inherited;
end;

procedure TTransformHelper.Draw;
begin
  if CurrentDrawing.WantRotation then
  begin
    BuildMatrixM;
    UpdateTransform;
  end;

  DrawToCanvas;
end;

procedure TTransformHelper.DrawToCanvas;
begin
  if Assigned(OnDrawToCanvas) then
    FOnDrawToCanvas(self);
end;

procedure TTransformHelper.ShowRotation(r: TPoint3D; b: Boolean);
begin
  RotR := r;
  RotB := b;
  if Assigned(OnShowRotation) then
    FOnShowRotation(self);
end;

function TTransformHelper.BuildMatrixG(NewFixPoint: TPoint3D): TMatrix3D;
var
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
  ra: TPoint3D;
begin
  if CurrentDrawing.WantRotation then
  begin
    ra := RotationHelper.EulerAnglesFromMatrix(AccuMatrix);

    { Variante 1 }
//    mr := RotationHelper.EulerAnglesToMatrix(ra.X, ra.Y, ra.Z);

    { Variante 2 }
    mx := TMatrix3D.CreateRotationX(ra.X);
    my := TMatrix3D.CreateRotationY(ra.Y);
    mz := TMatrix3D.CreateRotationZ(ra.Z);
    mr := mx * my * mz;

    CurrentDrawing.FixPoint := NewFixPoint;

    BuildMatrix(mr);
    result := NewMatrix;
    NewMatrix := TMatrix3D.Identity;
  end
  else
  begin
    result := TMatrix3D.Identity;
  end;
end;

function TTransformHelper.BuildMatrixF: TMatrix3D;
var
  mr: TMatrix3D;
  ra: TPoint3D;
begin
  if CurrentDrawing.WantRotation then
  begin
    ra := RotationHelper.EulerAnglesFromMatrix(AccuMatrix);
    mr := RotationHelper.EulerAnglesToMatrix(ra.X, ra.Y, ra.Z);

    BuildMatrix(mr);
    result := NewMatrix;
    NewMatrix := TMatrix3D.Identity;
  end
  else
  begin
    result := TMatrix3D.Identity;
  end;
end;

function TTransformHelper.BuildMatrixI: TMatrix3D;
var
//  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
  ra: TPoint3D;
begin
  if CurrentDrawing.WantRotation then
  begin
    ra := RotationHelper.EulerAnglesFromMatrix(AccuMatrix);
    mr := RotationHelper.EulerAnglesToMatrix(ra.X, ra.Y, ra.Z);

//    mx := TMatrix3D.CreateRotationX(ra.X);
//    my := TMatrix3D.CreateRotationY(ra.Y);
//    mz := TMatrix3D.CreateRotationZ(ra.Z);
//    mr := mx * my * mz;

    mr := mr.Transpose;

    BuildMatrix(mr);
    result := NewMatrix;
    NewMatrix := TMatrix3D.Identity;
  end
  else
  begin
    result := TMatrix3D.Identity;
  end;
end;

procedure TTransformHelper.UpdateTransform;
begin
  AccuMatrix := AccuMatrix * NewMatrix;
  CurrentDrawing.Transform(NewMatrix);
end;

procedure TTransformHelper.InitTransform(mr: TMatrix3D);
begin
  BuildMatrix(mr);
  UpdateTransform;
  DrawToCanvas;
end;

procedure TTransformHelper.ResetTransform;
begin
  TRggCircle.Matrix := TMatrix3D.Identity;
  AccuMatrix := TMatrix3D.Identity;
  Rotation := TPoint3D.Zero;
  Offset := TPointF.Zero;
end;

procedure TTransformHelper.Reset;
begin
  ResetTransform;
  CurrentDrawing.Reset;
  Draw;
end;

procedure TTransformHelper.SetOnDrawToCanvas(const Value: TNotifyEvent);
begin
  FOnDrawToCanvas := Value;
end;

procedure TTransformHelper.SetOnShowRotation(const Value: TNotifyEvent);
begin
  FOnShowRotation := Value;
end;

procedure TTransformHelper.BuildMatrixM;
var
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  mx := TMatrix3D.CreateRotationX(Rotation.X);
  my := TMatrix3D.CreateRotationY(Rotation.Y);
  mz := TMatrix3D.CreateRotationZ(Rotation.Z);
  mr := mx * my * mz;

  BuildMatrix(mr);
end;

procedure TTransformHelper.BuildMatrix(mr: TMatrix3D);
var
  mt1, mt2, ms: TMatrix3D;
begin
  mt1 := TMatrix3D.CreateTranslation(-CurrentDrawing.FixPoint);
  ms := TMatrix3D.CreateScaling(TPoint3D.Create(ZoomDelta, ZoomDelta, ZoomDelta));
  mt2 := TMatrix3D.CreateTranslation(CurrentDrawing.FixPoint);
  NewMatrix := mt1 * mr * ms * mt2;
end;

procedure TTransformHelper.GetEulerAngles;
begin
  RotR := RotationHelper.EulerAnglesFromMatrix(AccuMatrix);
  RotD := RotationHelper.RotD(RotR);
  ShowRotation(RotR, True);
end;

procedure TTransformHelper.DoOnMouse(Shift: TShiftState; dx, dy: single);
begin
  if (ssShift in Shift) or (ssMiddle in Shift) then
  begin
    if dy > 0 then
      ZoomDelta := 1 - 0.01
    else
      ZoomDelta := 1 + 0.01;
  end
  else if ssCtrl in Shift then
  begin
    Offset.X := Offset.X + dx;
    Offset.Y := Offset.Y + dy;
  end

  else
  begin
    if IsRightMouseBtn then
    begin
      Rotation.Z := Rotation.Z + dx * 0.005;
    end
    else
    begin
      Rotation.X := Rotation.X - dy * 0.005;
      Rotation.Y := Rotation.Y + dx * 0.005;
    end;
  end;

  Draw;

  ZoomDelta := 1;
  Rotation := TPoint3D.Zero;
end;

end.
