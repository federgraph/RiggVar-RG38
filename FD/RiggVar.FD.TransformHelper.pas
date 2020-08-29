﻿unit RiggVar.FD.TransformHelper;

interface

uses
  System.Types,
  System.Classes,
  System.Math,
  System.Math.Vectors,
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

    DrawCounter: Integer;

    constructor Create;

    procedure BuildMatrix(mr: TMatrix3D);
    procedure BuildMatrixM;
    procedure ResetTransform;
    procedure InitTransform(mr: TMatrix3D);
    procedure UpdateTransform;

    procedure Reset;
    procedure Draw;
    procedure DrawToCanvas;
    procedure ShowRotation(r: TPoint3D; b: Boolean);

    procedure GetEulerAngles; virtual;

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

  ResetTransform;
end;

procedure TTransformHelper.Draw;
begin
  Inc(DrawCounter);

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

procedure TTransformHelper.GetEulerAngles;
begin

end;

procedure TTransformHelper.ShowRotation(r: TPoint3D; b: Boolean);
begin
  RotR := r;
  RotB := b;
  if Assigned(OnShowRotation) then
    FOnShowRotation(self);
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

end.
