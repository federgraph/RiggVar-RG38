unit RiggVar.FD.RotationHelper;

interface

uses
  System.Types,
  System.Math,
  System.Math.Vectors;

type
  TRotationHelper = class
  public
    function IsEssentiallyZero(const Value: Single): Boolean;

    function RotD(Value: TPoint3D): TPoint3D;
    function RotR(Value: TPoint3D): TPoint3D;

    function EulerAnglesFromMatrix(m: TMatrix3D): TPoint3D;
    function EulerAnglesToMatrix(heading, attitude, bank: single): TMatrix3D;

    function EulerAnglesFromQuaternion(q: TQuaternion3D): TPoint3D;
    function EulerAnglesToQuaternion(yaw, pitch, roll: single): TQuaternion3D;

    function GetRotationInfoHPB(rm: TMatrix3D): TPoint3D;
  end;

implementation

{ TRotationHelper }

function TRotationHelper.IsEssentiallyZero(const Value: Single): Boolean;
begin
  Result := ((Value < Epsilon2) and (Value > -Epsilon2));
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

  { first row }
  result.m11 := c1 * c2; { first column }
  result.m12 := s1 * s3 - c1 * s2 * c3;
  result.m13 := c1 * s2 * s3 + s1 * c3;

  { second row }
  result.m21 := s2;
  result.m22 := c2 * c3;
  result.m23 := -c2 * s3;

  { third row }
  result.m31 := -s1 * c2;
  result.m32 := s1 * s2 * c3 + c1 * s3;
  result.m33 := -s1 * s2 * s3 + c1 * c3;
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
