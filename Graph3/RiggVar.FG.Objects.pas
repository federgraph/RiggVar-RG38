unit RiggVar.FG.Objects;

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

{.$define WantOrtho}

uses
  System.Math,
  System.Types,
  System.Classes,
  System.Math.Vectors,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.MaterialSources;

type
  TStrokeLine = class(TExtrudedShape3D)
  protected
    procedure SpecialRot;
    procedure Render; override;
  public
    StartPoint: TPoint3D;
    EndPoint: TPoint3D;
    procedure Update; virtual;
  end;

implementation

uses
  RiggVar.App.Main;

{ TStrokeLine }

procedure TStrokeLine.Render;
var
  S: TPointF;
  VP: TPolygon;
  r: TRectF;
  Path: TPathData;
begin
  Path := TPathData.Create;
  try
    r := RectF(0, 0, Width, Height);
    Path.AddEllipse(r);

    S := Path.FlattenToPolygon(VP, Flatness);
    if (S.X > 0) and (S.Y > 0) then
    begin
      r := RectF(0, 0, S.X, S.Y);
      { front }
      if TExtrudedShapeSide.Front in Sides then
      begin
        Context.FillPolygon(Point3D(0, 0, Depth/2), Point3D(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity, False, True, False);
      end;
      { back }
      if TExtrudedShapeSide.Back in Sides then
      begin
        Context.FillPolygon(Point3D(0, 0, Depth/2), Point3D(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialBackSource), AbsoluteOpacity, True, False, False);
      end;
      { left }
      if TExtrudedShapeSide.Shaft in Sides then
      begin
        Context.FillPolygon(Point3D(0, 0, Depth/2), Point3D(RectWidth(r), RectHeight(r), Depth),
          RectF(0, 0, 0, 0), VP, TMaterialSource.ValidMaterial(MaterialShaftSource), AbsoluteOpacity, False, False, True);
      end;
    end;
  finally
    Path.Free;
  end;
end;

procedure TStrokeLine.SpecialRot;
var
  LeftVector, DirectionVector, UpVector: TPoint3D;
begin
  DirectionVector := (EndPoint - StartPoint);
  DirectionVector.Normalize;
  UpVector := Point3D(0, 1, 0);
  LeftVector := UpVector.CrossProduct(DirectionVector);

  FSavedRotationAngle := Point3D(0, 0, 0);
  FQuaternion := TQuaternion3D.Identity;

  FLocalMatrix.M[0] := LeftVector * FScale.X;
  FLocalMatrix.m14 := 0;
  FLocalMatrix.M[1] := UpVector * FScale.Y;
  FLocalMatrix.m24 := 0;
  FLocalMatrix.M[2] := DirectionVector * FScale.Z;
  FLocalMatrix.m34 := 0;
  FLocalMatrix.m41 := FPosition.X;
  FLocalMatrix.m42 := FPosition.Y;
  FLocalMatrix.m43 := FPosition.Z;
end;

procedure TStrokeLine.Update;
begin
  Depth := 1.0;
  Position.Point := StartPoint;
  SpecialRot;
end;

end.
