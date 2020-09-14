unit RiggVar.FD.RotationHelper;

interface

uses
  System.Math,
  System.Math.Vectors,
  System.Types,
  FMX.Controls3D;

type
  TRotationHelper = class
  protected
    DirX: TPoint3D;
    DirY: TPoint3D;
    DirZ: TPoint3D;
    FLocalMatrix: TMatrix3D;
    FQuaternion: TQuaternion3D;
    FPosition: TPoint3D;
    FRotationCenter: TPoint3D;
    FRotationAngle: TPoint3D;
    FSavedRotationAngle: TPoint3D;
    procedure MatrixChanged;
    procedure ResetRotationAngle;
    procedure RotateXChanged;
    procedure RotateYChanged;
    procedure RotateZChanged;
    procedure RotationChanged;
  protected
    procedure SetRotX(const Value: single);
    procedure SetRotY(const Value: single);
    procedure SetRotZ(const Value: single);

    function CreateTaitBryanY1X2Z3(const EA: TPoint3D): TMatrix3D;
    function CreateTaitBryanZ1X2Y3(const EA: TPoint3D): TMatrix3D;

    procedure FederFrame3DLoadEulerAngle(CameraDummy: TControl3D; ed: TPoint3D);
    procedure FederDummyRotateObject(dx, dy, dz: single);
  public
    AngleX: single;
    AngleY: single;
    AngleZ: single;

    DegX: single;
    DegY: single;
    DegZ: single;

    Mode: Integer;

    constructor Create;

    procedure SinCosSingle(const Theta: Single; out ASin, ACos: Single);
    function IsEssentiallyZero(const Value: Single): Boolean;

    function RotD(Value: TPoint3D): TPoint3D;
    function RotR(Value: TPoint3D): TPoint3D;

    function EulerAnglesFromMatrix(m: TMatrix3D): TPoint3D;
    function EulerAnglesToMatrix(heading, attitude, bank: single): TMatrix3D;

    function EulerAnglesFromQuaternion(q: TQuaternion3D): TPoint3D;
    function EulerAnglesToQuaternion(yaw, pitch, roll: single): TQuaternion3D;

    function GetFedergraphRotationInfo(LM: TMatrix3D; RA: TPoint3D): TPoint3D;
    function GetFederFrameRotationInfo(LM: TMatrix3D; RA: TPoint3D): TPoint3D;

    function ExtractRotationD(M: TMatrix3D): TPoint3D;
    function ExtractRotationR(M: TMatrix3D): TPoint3D;

    procedure SaveRotation(C3D: TControl3D);
    procedure LoadRotation(C3D: TControl3D);

    function GetRotationInfo(rm: TMatrix3D): TPoint3D;
    function GetRotationInfoHPB(rm: TMatrix3D): TPoint3D;

    property RotX: single read FRotationAngle.X write SetRotX;
    property RotY: single read FRotationAngle.Y write SetRotY;
    property RotZ: single read FRotationAngle.Z write SetRotZ;
  end;

implementation

{ TRotationHelper }

procedure TRotationHelper.SinCosSingle(const Theta: Single; out ASin, ACos: Single);
begin
  ASin := sin(Theta);
  ACos := cos(Theta);
end;

function TRotationHelper.IsEssentiallyZero(const Value: Single): Boolean;
begin
  Result := ((Value < Epsilon2) and (Value > -Epsilon2));
end;

function TRotationHelper.GetRotationInfo(rm: TMatrix3D): TPoint3D;
var
  psi, the, phi: single;
//an3, an2, an1: single;

//  ct: single;
begin
  { this is Tait-Bryan angles Z1 Y2 X3 - second row from bottom in wkipedia article }

{
  m11 := c1 c2
  m12 := c1 s2 s3 - c3 s1
  m13 := c1 c2 s2

  m21 := c2 s1
  m22 := c1 c3    + s1 s 2 s3
  m23 := c3 s1 s2 - c1 s3

  m31 := -s2
  m32 := c2 s3
  m33 := c2 c3
}

  the := -ArcSin(rm.m31);
  if IsEssentiallyZero(the) then
  begin
    phi := 0;
    if IsEssentiallyZero(rm.m31 + 1) then
    begin
      the := pi/2;
      psi := { phi + } arctan2(rm.m12, rm.m13);
    end
    else
    begin
      the := -pi/2;
      psi := { -phi + } arctan2(-rm.m12, -rm.m13);
    end;
  end
  else
  begin
//    ct := cos(the);
//    psi := arctan2(rm.m32/ct, rm.m33/ct);
//    phi := arctan2(rm.m21/ct, rm.m11/ct);

    psi := arctan2(rm.m32, rm.m33); // c2 s3 / c2 c3 = s3 / c3 = tan(psi)
    phi := arctan2(rm.m21, rm.m11); // c2 s1 / c1 c2 = s1 / c1 = tan(phi)
  end;

  AngleX := psi;
  AngleY := the;
  AngleZ := phi;

  DegX := RadToDeg(psi);
  DegY := RadToDeg(the);
  DegZ := RadToDeg(phi);

  result.X := DegX;
  result.Y := DegY;
  result.Z := DegZ;
end;

procedure TRotationHelper.SaveRotation(C3D: TControl3D);
var
  p: TPoint3D;
begin
  p := GetRotationInfo(C3D.LocalMatrix);
  AngleX := -p.X;
  AngleY := -p.Y;
  AngleZ := -p.Z;
end;

procedure TRotationHelper.SetRotX(const Value: single);
begin
  if FRotationAngle.X <> Value then
  begin
    FRotationAngle.X := Value;
    RotateXChanged;
    RotationChanged;
  end;
end;

procedure TRotationHelper.SetRotY(const Value: single);
begin
  if FRotationAngle.Y <> Value then
  begin
    FRotationAngle.Y := Value;
    RotateYChanged;
    RotationChanged;
  end;
end;

procedure TRotationHelper.SetRotZ(const Value: single);
begin
  if FRotationAngle.Z <> Value then
  begin
    FRotationAngle.Z := Value;
    RotateZChanged;
    RotationChanged;
  end;
end;

procedure TRotationHelper.LoadRotation(C3D: TControl3D);
begin
  C3D.ResetRotationAngle;
  C3D.RotationAngle.X := AngleX;
  C3D.RotationAngle.Y := AngleY;
  C3D.RotationAngle.Z := AngleZ;
end;

constructor TRotationHelper.Create;
begin
  DirX := TPoint3D.Create(1, 0, 0);
  DirY := TPoint3D.Create(0, 1, 0);
  DirZ := TPoint3D.Create(0, 0, 1);
end;

function TRotationHelper.ExtractRotationD(M: TMatrix3D): TPoint3D;
begin
  GetRotationInfo(M);
  result.X := DegX;
  result.Y := DegY;
  result.Z := DegZ;
end;

function TRotationHelper.ExtractRotationR(M: TMatrix3D): TPoint3D;
begin
  GetRotationInfo(M);
  result.X := AngleX;
  result.Y := AngleY;
  result.Z := AngleZ;
end;

function TRotationHelper.GetFedergraphRotationInfo(LM: TMatrix3D; RA: TPoint3D): TPoint3D;
var
  ax, ay, az: single;
  ayt: single;
//  ct: single;
  mt: single;
begin
{
Euler angles from rotation matrix.

  m11, m12, m13
  m21, m22, m23
  m31, m32, m33

psi = ax := arctan2(rm.m32, rm.m33);
the = ay := arctan2(-rm.m31, sqrt(sqr(rm.m32) + sqr(rm.m33)));
phi = az := arctan2(rm.m21, rm.m11);

In special case where angle around y-axis is 90° or -90°
all elements in first column and last row are zero,
except element m31,
which is either 1 or -1.

fix for special case:
  rotate around the x-axis 180°
  and compute az as atan2(m12, -m22).
}

  //LM := FModelGroup.LocalMatrix;
  //RA := OrthoInf;

  ay := arctan2(-LM.m31, sqrt(sqr(LM.m32) + sqr(LM.m33)));

  mt := LM.m31;
  { argument for ArcSin must be in -1..1 }
  if mt > 1 then
    mt := 1
  else if LM.m31 < -1 then
    mt := -1;
  ayt := -ArcSin(mt);
  if IsEssentiallyZero(ayt) then
  begin
    ax := 0;
    ay := 0;
    az := 0;

    if (RA.X = 0) and  (RA.Y = 0) then
    begin
      az := -RA.Z;
      if az > 180 then
        az := az - 360;
      if az < -180 then
        az := 360 + az;
      az := DegToRad(az);
    end
    else if (RA.X = 0) and (RA.Z = 0) then
    begin
      ax := RA.Y;
      if ax > 180 then
        ax := ax - 360;
      ax := DegToRad(ax);
      ax := -ax;
    end;
//    else
//    if TUtils.IsEssentiallyZero(rm.m31 + 1) then
//    begin
//      az := pi/2;
//      ax := arctan2(rm.m12, rm.m13);
//    end
//    else
//    begin
//      az := -pi/2;
//      ax := arctan2(-rm.m12, -rm.m13);
//    end;
  end

  else
  begin
//    ct := cos(ay);
//    ax := arctan2(rm.m32/ct, rm.m33/ct);
//    az := arctan2(rm.m21/ct, rm.m11/ct);

    ax := arctan2(LM.m32, LM.m33);
    az := arctan2(LM.m21, LM.m11);
  end;

  if Mode = 0 then //if UseMain.UpdatingData and Main.IsOrthoProjection then
  begin
    result.X := RadToDeg(ax);
    result.Y := RadToDeg(ay);
    result.Z := RadToDeg(az);
  end
  else
  begin
    result.X := RadToDeg(ax);
    result.Y := RadToDeg(-ay);
    result.Z := RadToDeg(-az);
  end;
end;

function TRotationHelper.GetFederFrameRotationInfo(LM: TMatrix3D; RA: TPoint3D): TPoint3D;
var
  ax, ay, az: single;
  mt: single;
  ayt: single;
begin
//  if CameraDummy = nil then
//    Exit;
//  LM := CameraDummy.LocalMatrix;
//  RA := CameraDummy.RotationAngle;

  ay := arctan2(-LM.m31, sqrt(sqr(LM.m32) + sqr(LM.m33)));

  mt := LM.m31;
  { argument mt for ArcSin function must be in range -1..1 }
  if mt > 1 then
    mt := 1
  else if LM.m31 < -1 then
    mt := -1;
  ayt := -ArcSin(mt);

  if IsEssentiallyZero(ayt) then
  begin
    az := 0;
    ay := 0;
    ax := 0;
    if (RA.Y = 0) and (RA.Z = 0)
    then
    begin
      ax := RA.X;
      if ax > 180 then
        ax := ax - 360;
      ax := DegToRad(ax);
      ax := -ax;
    end
    else if (RA.X = 180) and  (RA.Y = 0)
    then
    begin
      ax := PI;
      az := RA.Z;
      if az > 180 then
        az := az - 360;
      az := DegToRad(az);
      az := -az;
    end
  end
  else
  begin
    ax := arctan2(LM.m32, LM.m33);
    az := arctan2(LM.m21, LM.m11);
  end;

  result.X := RadToDeg(ax);
  result.Y := RadToDeg(ay);
  result.Z := RadToDeg(az);
end;

procedure TRotationHelper.FederFrame3DLoadEulerAngle(CameraDummy: TControl3D; ed: TPoint3D);
begin
//  if Assigned(CameraDummy)
//    and (Camera.ViewType = TViewType.Perspective)
//  then
//  begin
    CameraDummy.ResetRotationAngle;
//    CameraDummy.RotationAngle.X := DefaultRX;
    CameraDummy.RotationAngle.X := ed.X;
    CameraDummy.RotationAngle.Y := ed.Y;
    CameraDummy.RotationAngle.Z := ed.Z;
//  end;
end;

procedure TRotationHelper.FederDummyRotateObject(dx, dy, dz: single);
var
  mx, my, mz, m: TMatrix3D;
begin
  mx := TMatrix3D.CreateRotationX(dy);
  my := TMatrix3D.CreateRotationY(dx);
  mz := TMatrix3D.CreateRotationZ(dz);

  m := mx * my * mz;

//  FLocalMatrix := FLocalMatrix * m;
//  RecalcAbsolute;
//  Repaint;
end;

procedure TRotationHelper.MatrixChanged;
var
  AuxiliarMatrix: TMatrix3D;
  RotMatrix: TMatrix3D;
begin
  FLocalMatrix := TMatrix3D.Identity;

//  if not(
//    SameValue(FScale.X, 1, TEpsilon.Scale) and
//    SameValue(FScale.Y, 1, TEpsilon.Scale) and
//    SameValue(FScale.Z, 1, TEpsilon.Scale)) then
//    FLocalMatrix := FLocalMatrix * TMatrix3D.CreateScaling(FScale.Point);

  if not(
    SameValue(FRotationAngle.X, 0, TEpsilon.Vector) and
    SameValue(FRotationAngle.Y, 0, TEpsilon.Vector) and
    SameValue(FRotationAngle.Z, 0, TEpsilon.Vector)) then
  begin
    if not(
      SameValue(FRotationCenter.X, 0, TEpsilon.Position) and
      SameValue(FRotationCenter.Y, 0, TEpsilon.Position) and
      SameValue(FRotationCenter.Z, 0, TEpsilon.Position)) then
    begin
      AuxiliarMatrix := FQuaternion;
      RotMatrix :=
        TMatrix3D.CreateTranslation(-FRotationCenter)
        * AuxiliarMatrix *
        TMatrix3D.CreateTranslation(FRotationCenter);
    end
    else
      RotMatrix := FQuaternion;
    FLocalMatrix := FLocalMatrix * RotMatrix;
  end
  else
  begin
//    FSavedRotationAngle := TPoint3D.Zero;
//    FQuaternion := TQuaternion3D.Identity;
  end;

//  if not(
//    SameValue(FPosition.X, 0, TEpsilon.Position) and
//    SameValue(FPosition.Y, 0, TEpsilon.Position) and
//    SameValue(FPosition.Z, 0, TEpsilon.Position)) then
//  begin
//    FLocalMatrix := FLocalMatrix * TMatrix3D.CreateTranslation(FPosition);
//  end;

//  RecalcAbsolute;
//  RebuildRenderingList;
//  Repaint;
end;

procedure TRotationHelper.RotationChanged;
var
  a: Single;
  NeedChange: Boolean;
  NewValue: TPoint3D;
begin
  NeedChange := False;
  NewValue := FRotationAngle;

  a := DegNormalize(FRotationAngle.X - FSavedRotationAngle.X);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirX, DegToRad(a));
    NeedChange := True;
    NewValue.X := DegNormalize(FRotationAngle.X);
  end;

  a := DegNormalize(FRotationAngle.Y - FSavedRotationAngle.Y);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirY, DegToRad(a));
    NeedChange := True;
    NewValue.Y := DegNormalize(FRotationAngle.Y);
  end;

  a := DegNormalize(FRotationAngle.Z - FSavedRotationAngle.Z);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirZ, DegToRad(a));
    NeedChange := True;
    NewValue.Z := DegNormalize(FRotationAngle.Z);
  end;

  if NeedChange then
  begin
    FSavedRotationAngle := FRotationAngle;
    FRotationAngle := NewValue;
    MatrixChanged;
  end;
end;

procedure TRotationHelper.RotateXChanged;
var
  a: Single;
begin
  a := DegNormalize(FRotationAngle.X - FSavedRotationAngle.X);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirX, DegToRad(a));
    MatrixChanged;
    FSavedRotationAngle.X := FRotationAngle.X;
    FRotationAngle := TPoint3D.Create(DegNormalize(FRotationAngle.X), FRotationAngle.Y, FRotationAngle.Z);
  end;
end;

procedure TRotationHelper.RotateYChanged;
var
  a: Single;
begin
  a := DegNormalize(FRotationAngle.Y - FSavedRotationAngle.Y);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirY, DegToRad(a));
    MatrixChanged;
    FSavedRotationAngle.Y := FRotationAngle.Y;
    FRotationAngle := TPoint3D.Create(FRotationAngle.X, DegNormalize(FRotationAngle.Y), FRotationAngle.Z);
  end;
end;

procedure TRotationHelper.RotateZChanged;
var
  a: Single;
begin
  a := DegNormalize(FRotationAngle.Z - FSavedRotationAngle.Z);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(DirZ, DegToRad(a));
    MatrixChanged;
    FSavedRotationAngle.Z := FRotationAngle.Z;
    FRotationAngle := TPoint3D.Create(FRotationAngle.X, FRotationAngle.Y, DegNormalize(FRotationAngle.Z));
  end;
end;

procedure TRotationHelper.ResetRotationAngle;
begin
  FQuaternion := TQuaternion3D.Identity;
  MatrixChanged;
  FSavedRotationAngle := TPoint3D.Zero;
  FRotationAngle := TPoint3D.Zero;
end;

function TRotationHelper.EulerAnglesFromQuaternion(q: TQuaternion3D): TPoint3D;
var
  test: single;
  sqx: single;
  sqy: single;
  sqz: single;
  heading: single;
  attitude: single;
  bank: single;
begin
  test := q.ImagPart.X * q.ImagPart.Y + q.ImagPart.Z * q.RealPart;
  if (test > 0.499) then
  begin
    { singularity at north pole }
    heading := 2 * arctan2(q.ImagPart.X, q.RealPart);
    attitude := PI/2;
    bank := 0;
  end
  else if (test < -0.499) then
  begin
    { singularity at south pole }
    heading := -2 * arctan2(q.ImagPart.X, q.RealPart);
    attitude := - PI/2;
    bank := 0;
  end
  else
  begin
    sqx := q.ImagPart.X * q.ImagPart.X;
    sqy := q.ImagPart.Y * q.ImagPart.Y;
    sqz := q.ImagPart.Z * q.ImagPart.Z;

    heading := arctan2(
      2 * q.ImagPart.Y * q.RealPart - 2 * q.ImagPart.X * q.ImagPart.Z,
      1 - 2 * sqy - 2 * sqz);

    attitude := arcsin(2 * test);

    bank := arctan2(
      2 * q.ImagPart.X * q.RealPart - 2 * q.ImagPart.Y * q.ImagPart.Z,
      1 - 2 * sqx - 2 * sqz);
  end;

  result := TPoint3D.Create(heading, attitude, bank);
end;

function TRotationHelper.EulerAnglesToQuaternion(yaw, pitch, roll: single): TQuaternion3D;
var
  cy, sy, cp, sp, cr, sr: single;
  q: TQuaternion3D;
begin
  { Z = yaw,
    Y = pitch,
    X = roll }

  cy := cos(yaw * 0.5);
  sy := sin(yaw * 0.5);

  cp := cos(pitch * 0.5);
  sp := sin(pitch * 0.5);

  cr := cos(roll * 0.5);
  sr := sin(roll * 0.5);

  q.ImagPart.X := sr * cp * cy - cr * sp * sy;
  q.ImagPart.Y := cr * sp * cy + sr * cp * sy;
  q.ImagPart.Z := cr * cp * sy - sr * sp * cy;

  q.RealPart := cr * cp * cy + sr * sp * sy;

  result := q;
end;

function TRotationHelper.EulerAnglesFromMatrix(m: TMatrix3D): TPoint3D;
var
  heading: single;
  attitude: single;
  bank: single;
begin

{  this conversion uses conventions as described on page:
   https://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm

   Coordinate System: right hand
   Positive angle: right hand
   Order of euler angles: heading first, then attitude, then bank

   matrix row column ordering in code on website:
   [m00 m01 m02]
   [m10 m11 m12]
   [m20 m21 m22]

   matrix row column ordering in TMatrix3D:
   [m11 m12 m13]
   [m21 m22 m23]
   [m31 m32 m33]
}

  { Tait-Bryan angles Y1 Z2 X3 }

  { Assuming the angles are in radians. }
  if (m.m21 > 0.998) then
  begin
    { singularity at north pole }
    heading := arctan2(m.m13, m.m33);
    attitude := PI/2;
    bank := 0;
  end
  else if (m.m21 < -0.998) then
  begin
    { singularity at south pole }
    heading := arctan2(m.m13, m.m33);
    attitude := -PI/2;
    bank := 0;
  end
  else
  begin
    heading := arctan2(-m.m31, m.m11);
    attitude := arcsin(m.m21);
    bank := arctan2(-m.m23, m.m22);
  end;

  result := TPoint3D.Create(heading, attitude, bank);
end;

function TRotationHelper.EulerAnglesToMatrix(heading, attitude, bank: single): TMatrix3D;
var
  c1, s1: single;
  c2, s2: single;
  c3, s3: single;
begin
  result := TMatrix3D.Identity;

  { angles are in radians }
  c1 := cos(heading);
  s1 := sin(heading);

  c2 := cos(attitude);
  s2 := sin(attitude);

  c3 := cos(bank);
  s3 := sin(bank);

  { http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToMatrix/index.htm }
  { https://en.wikipedia.org/wiki/Euler_angles }

  { Tait-Bryan angles Y1 Z2 X3 }

  { first row}
  result.m11 := c1 * c2; { first column }
  result.m12 := s1 * s3 - c1 * s2 * c3;
  result.m13 := c1 * s2 * s3 + s1 * c3;

  { second row}
  result.m21 := s2;
  result.m22 := c2 * c3;
  result.m23 := -c2 * s3;

  { third row }
  result.m31 := -s1 * c2;
  result.m32 := s1 * s2 * c3 + c1 * s3;
  result.m33 := -s1 * s2 * s3 + c1 * c3;
end;

function TRotationHelper.RotD(Value: TPoint3D): TPoint3D;
begin
  result := TPoint3D.Create(
    RadToDeg(Value.X),
    RadToDeg(Value.Y),
    RadToDeg(Value.Z));
end;

function TRotationHelper.RotR(Value: TPoint3D): TPoint3D;
begin
  result := TPoint3D.Create(
    DegToRad(Value.X),
    DegToRad(Value.Y),
    DegToRad(Value.Z));
end;

function TRotationHelper.CreateTaitBryanZ1X2Y3(const EA: TPoint3D): TMatrix3D;
begin
  { compare with https://en.wikipedia.org/wiki/Euler_angles }
  result := TMatrix3D.CreateRotationYawPitchRoll(-EA.X, EA.Y, EA.Z);
  result := result.Transpose;
end;

function TRotationHelper.CreateTaitBryanY1X2Z3(const EA: TPoint3D): TMatrix3D;
begin
  { compare with https://en.wikipedia.org/wiki/Euler_angles }
  result := TMatrix3D.CreateRotationHeadingPitchBank(EA.X, EA.Y, EA.Z);
end;

function TRotationHelper.GetRotationInfoHPB(rm: TMatrix3D): TPoint3D;
var
  psi, the, phi: single;
begin
  { intended to match Y1 X2 Z2 = CreateRotationHeadingPitchBank }

{ Y1 X2 Z2

  m11 := c1 c3    + s1 s2 s3
  m12 := c3 s1 s2 - c1 s3
  m13 := c2 s1

  m21 := c2 s3
  m22 := c2 c3
  m23 := -s2

  m31 := -c1 s2 s3 - c3 s1
  m32 :=  c1 c3 s2 + s1 s3
  m33 := c1 c2
}

  the := -ArcSin(rm.m23);
  if IsEssentiallyZero(the) then
  begin
    phi := 0;
    if IsEssentiallyZero(rm.m13 + 1) then
    begin
      the := pi/2;
      psi := arctan2(-rm.m12, rm.m11);
    end
    else
    begin
      the := -pi/2;
      psi := arctan2(rm.m12, -rm.m11);
    end;
  end
  else
  begin
    psi := arctan2(rm.m21, rm.m22); // c2 s3 / c2 c3 = s3/c3 = tan(psi)
    phi := arctan2(rm.m13, rm.m33); // c2 s1 / c1 c2 = s1/c1 = tan(phi)
  end;

  result.X := phi;
  result.Y := the;
  result.Z := psi;
end;

end.
