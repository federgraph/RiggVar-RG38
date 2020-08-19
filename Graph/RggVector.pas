unit RggVector;

interface

type
  vec2 = record
    x, y, z: single;
  end;

  vec3 = record
    x, y, z: single;
  end;

function InitPoint2D(ax: single; ay: single): vec2;
function Mag2D(v: vec2): single;
function Normalize2D(v: vec2): vec2;

function InitPoint3D(ax: single; ay: single; az: single): vec3;
function Mag3D(v: vec3): single;
function Normalize3D(v: vec3): vec3;
function Subtract(v1, v2: vec3): vec3;
function Cross(v1, v2: vec3): vec3;
function Divide(v :vec3; num: single): vec3;
function Dot(v1, v2: vec3): single;

function LookUpRa10(Index: Integer): single;

const
  vec3Null: vec3 = (x: 0; y: 0; z: 0);

implementation

function InitPoint2D(ax: single; ay: single): vec2;
begin
  result.x := ax;
  result.y := ay;
end;

function InitPoint3D(ax: single; ay: single; az: single): vec3;
begin
  result.x := ax;
  result.y := ay;
  result.z := az;
end;

function Mag2D(v: vec2): single;
begin
  result := sqrt(v.x * v.x + v.y * v.y);
end;

function Normalize2D(v: vec2): vec2;
var
  d: single;
begin
  result := InitPoint2D(v.x, v.y);
  d := Mag2D(result);
  if (d <> 0) then
  begin
    result.x := result.x / d;
    result.y := result.y / d;
  end;
end;

{ Calculate the magnitude of the vector }
function Mag3D(v: vec3): single;
begin
  result := sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
end;

{ Subtract the two vectors }
function Subtract(v1, v2: vec3): vec3;
var
  d: vec3;
begin
  d.x := v1.x - v2.x;
  d.y := v1.y - v2.y;
  d.z := v1.z - v2.z;
  result := d;
end;

{ Cross multiply the two vectors v1 and v2 }
function Cross(v1, v2: vec3): vec3;
var
  c: vec3;
begin
  c.x := v1.y * v2.z - v2.y * v1.z;
  c.y := v1.z * v2.x - v2.z * v1.x;
  c.z := v1.x * v2.y - v2.x * v1.y;
  result := c;
end;

{ Divide the scalar number into the vector v }
function Divide(v: vec3; num: single): vec3;
var
  d: vec3;
begin
  if (num <> 0) then
  begin
    d.x := v.x /num;
    d.y := v.y /num;
    d.z := v.z /num;
  end;
  result := d;
end;

{ Normalize the vector v }
procedure Normalize(var v: vec3);
var
  d: single;
begin
  d := sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  if (d <> 0) then
  begin
    v.x := v.x / d;
    v.y := v.y / d;
    v.z := v.z / d;
  end;
end;

function Normalize3D(v: vec3): vec3;
var
  d: single;
begin
  result := InitPoint3D(v.x, v.y, v.z);
  d := Mag3D(result);
  if (d <> 0) then
  begin
    result.x := result.x / d;
    result.y := result.y / d;
    result.z := result.z / d;
  end;
end;

{ Calculate the dot product of the two vectors v1 and v2 }
function Dot(v1, v2: vec3): single;
begin
  result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function LookUpRa10(Index: Integer): single;
var
  temp: single;
begin
  { dezimalgeometrische Reihe Ra10 }
  temp := 1;
  case Index of
    1: temp := 1;
    2: temp := 1.2;
    3: temp := 1.6;
    4: temp := 2;
    5: temp := 2.5;
    6: temp := 3.2;
    7: temp := 4;
    8: temp := 5;
    9: temp := 6.3;
    10: temp := 8;
    11: temp := 10;
  end;
  result := temp;
end;

end.
