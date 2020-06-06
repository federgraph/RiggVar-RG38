unit RggPolarKar;

interface

uses
  System.SysUtils,
  System.Math,
  RggVector,
  RggTypes,
  RggMatrix;

type
  TCalcAngleEvent = procedure(Sender: TObject; var wx, wy, wz: double) of object;

  TPolarKar = class
  private
    FPhi, FTheta, FGamma, FXRot, FYRot, FZRot: double;
    FValid: Boolean;
    FMode: Boolean;
    FOnCalcAngle: TCalcAngleEvent;
    procedure SetPhi(Value: double);
    procedure SetTheta(Value: double);
    procedure SetGamma(Value: double);
    procedure SetXrot(Value: double);
    procedure SetYrot(Value: double);
    procedure SetZrot(Value: double);
    procedure SetRotMode(Value: Boolean);
    procedure SetRotAngle(index: TRotationAngle; Value: double);
    function GetPhi: double;
    function GetTheta: double;
    function GetGamma: double;
    function GetXrot: double;
    function GetYrot: double;
    function GetZrot: double;
    function GetRotAngle(index: TRotationAngle): double;
  protected
    p1, p2: vec3;
    Angle: double;
    tmat: TMatrix4x4;
    procedure GetMat;
    procedure FillMatrix;
    procedure FillMatrixInc;
    function GetMatrix: Matrix4x4;
    procedure SetMatrix(Value: Matrix4x4);
  public
    Mat: TMatrix4x4;
    constructor Create;
    destructor Destroy; override;
    function Rotiere(Punkt: TRealPoint): TRealPoint;
    procedure Reset;
    procedure GetAngle(var wx, wy, wz: double);
    procedure GetAngle1(Sender: TObject; var wx, wy, wz: double);
    procedure GetAngle2(Sender: TObject; var wp, wt, wg: double);
    property DeltaTheta: double read GetTheta write SetTheta;
    property DeltaPhi: double read GetPhi write SetPhi;
    property DeltaGamma: double read GetGamma write SetGamma;
    property XRot: double read GetXrot write SetXrot;
    property YRot: double read GetYrot write SetYrot;
    property ZRot: double read GetZrot write SetZrot;
    property RotAngle[index: TRotationAngle]: double read GetRotAngle write SetRotAngle;
    property Matrix: Matrix4x4 read GetMatrix write SetMatrix;
    property Mode: Boolean read FMode write SetRotMode;
    property OnCalcAngle: TCalcAngleEvent read FOnCalcAngle write FOnCalcAngle;
  end;

implementation

constructor TPolarKar.Create;
begin
  Mat := TMatrix4x4.Create;
  tmat := TMatrix4x4.Create;
  Reset;
end;

destructor TPolarKar.Destroy;
begin
  Mat.Free;
  tmat.Free;
end;

procedure TPolarKar.SetPhi(Value: double);
begin
  FPhi := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetTheta(Value: double);
begin
  FTheta := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetGamma(Value: double);
begin
  FGamma := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetXrot(Value: double);
begin
  FXRot := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetYrot(Value: double);
begin
  FYRot := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetZrot(Value: double);
begin
  FZRot := Value * pi / 180;
  FValid := False;
end;

function TPolarKar.GetPhi: double;
begin
  Result := Int(FPhi * 180 / pi);
end;

function TPolarKar.GetTheta: double;
begin
  Result := Int(FTheta * 180 / pi);
end;

function TPolarKar.GetGamma: double;
begin
  Result := Int(FGamma * 180 / pi);
end;

function TPolarKar.GetXrot: double;
begin
  Result := Int(FXRot * 180 / pi);
end;

function TPolarKar.GetYrot: double;
begin
  Result := Int(FYRot * 180 / pi);
end;

function TPolarKar.GetZrot: double;
begin
  Result := Int(FZRot * 180 / pi);
end;

procedure TPolarKar.SetRotAngle(index: TRotationAngle; Value: double);
var
  temp: double;
begin
  temp := Value * pi / 180;
  case index of
    raPhi:
      FPhi := temp;
    raTheta:
      FTheta := temp;
    raGamma:
      FGamma := temp;
    raXrot:
      FXRot := temp;
    raYrot:
      FYRot := temp;
    raZrot:
      FZRot := temp;
  end;
end;

function TPolarKar.GetRotAngle(index: TRotationAngle): double;
var
  temp: double;
begin
  temp := 0;
  case index of
    raPhi:
      temp := FPhi;
    raTheta:
      temp := FTheta;
    raGamma:
      temp := FGamma;
    raXrot:
      temp := FXRot;
    raYrot:
      temp := FYRot;
    raZrot:
      temp := FZRot;
  end;
  Result := Int(temp * 180 / pi);
end;

function TPolarKar.GetMatrix: Matrix4x4;
begin
  if FValid = False then
    GetMat;
  Result := Mat.Mat;
end;

procedure TPolarKar.SetMatrix(Value: Matrix4x4);
begin
  Reset;
  Mat.Mat := Value;
end;

procedure TPolarKar.GetMat;
begin
  if Mode = False then
    FillMatrixInc
  else
    FillMatrix;
end;

procedure TPolarKar.FillMatrixInc;
begin
  tmat.Identity;
  p1 := NullVec;
  if FTheta <> 0 then
  begin
    p2.x := Mat.Mat[1, 2];
    p2.y := Mat.Mat[2, 2];
    p2.z := Mat.Mat[3, 2];
    Angle := FTheta;
    tmat.Rotate(p1, p2, Angle);
    FTheta := 0;
  end;
  if FPhi <> 0 then
  begin
    p2.x := Mat.Mat[1, 1];
    p2.y := Mat.Mat[2, 1];
    p2.z := Mat.Mat[3, 1];
    Angle := -FPhi;
    tmat.Rotate(p1, p2, Angle);
    FPhi := 0;
  end;
  if FGamma <> 0 then
  begin
    p2.x := Mat.Mat[1, 3];
    p2.y := Mat.Mat[2, 3];
    p2.z := Mat.Mat[3, 3];
    Angle := FGamma;
    tmat.Rotate(p1, p2, Angle);
    FGamma := 0;
  end;
  if FZRot <> 0 then
  begin
    p2 := yVec;
    Angle := FZRot;
    tmat.Rotate(p1, p2, Angle);
    FZRot := 0;
  end;
  if FYRot <> 0 then
  begin
    p2 := xVec;
    Angle := FYRot;
    tmat.Rotate(p1, p2, Angle);
    FYRot := 0;
  end;
  if FXRot <> 0 then
  begin
    p2 := zVec;
    Angle := FXRot;
    tmat.Rotate(p1, p2, Angle);
    FXRot := 0;
  end;
  FValid := True;
  Mat.PreMultiply(tmat.Mat);
end;

procedure TPolarKar.FillMatrix;
begin
  { für Absolutmodus }
  Mat.Identity;
  { 1. Rotation um globale y-Achse, gleichzeitig lokale y-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 1.0;
  p2.z := 0.0;
  Angle := FTheta;
  Mat.Rotate(p1, p2, Angle);
  { 2. Rotation um globale z-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 0.0;
  p2.z := 1.0;
  Angle := FPhi;
  Mat.Rotate(p1, p2, Angle);
  { 3. Rotation um locale x-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := Mat.Mat[1, 1];
  p2.y := Mat.Mat[2, 1];
  p2.z := Mat.Mat[3, 1];
  Angle := FGamma;
  Mat.Rotate(p1, p2, Angle);
  FValid := True;
end;

function TPolarKar.Rotiere(Punkt: TRealPoint): TRealPoint;
var
  temp: vec3;
begin
  if FValid = False then
    GetMat;
  temp.x := Punkt[x];
  temp.y := Punkt[y];
  temp.z := Punkt[z];
  Mat.TransformPoint(temp);
  Result[x] := temp.x;
  Result[y] := temp.y;
  Result[z] := temp.z;
end;

procedure TPolarKar.Reset;
begin
  Mat.Identity;
  FPhi := 0;
  FTheta := 0;
  FGamma := 0;
  FXRot := 0;
  FYRot := 0;
  FZRot := 0;
  FValid := True;
end;

procedure TPolarKar.GetAngle(var wx, wy, wz: double);
begin
  wx := 0;
  wy := 0;
  wz := 0;
  if Assigned(OnCalcAngle) then
    OnCalcAngle(Self, wx, wy, wz);
end;

procedure TPolarKar.GetAngle1(Sender: TObject; var wx, wy, wz: double);

  function angle(a, b: vec3): double;
  var
    temp: double;
  begin
    temp := Dot(a, b);
    if temp > 1 then
      temp := 1;
    if temp < -1 then
      temp := -1;
    Result := ArcCos(temp) * 180 / pi;
  end;

var
  FLocalX, FlocalY, FLocalZ: vec3;
begin
  Mat.GetLocals(FLocalX, FlocalY, FLocalZ);
  wx := angle(FLocalX, xVec);
  wy := angle(FlocalY, yVec);
  wz := angle(FLocalZ, zVec);
end;

procedure TPolarKar.SetRotMode(Value: Boolean);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    FValid := False;
    if FMode then
    begin
      { Absolute }
      GetAngle(FPhi, FTheta, FGamma);
      FXRot := 0;
      FYRot := 0;
      FZRot := 0;
    end;
    if FMode = False then
    begin
      { Incremental }
      FPhi := 0;
      FTheta := 0;
      FGamma := 0;
      FXRot := 0;
      FYRot := 0;
      FZRot := 0;
    end;
  end;
end;

procedure TPolarKar.GetAngle2(Sender: TObject; var wp, wt, wg: double);

  function CheckSinCos(c: Extended): Extended;
  begin
    Assert(c <= 1, Format('sincos > 1 (%6.5f)', [c]));
    Assert(c >= -1, Format('sincos < -1 (%6.5f)', [c]));
    if c > 1 then
      c := 1;
    if c < -1 then
      c := -1;
    Result := c;
  end;

var
  tempcos, tempsin: double;
  ux, uy, uz, tempVec, tempY, tempZ: vec3;
  tempmat: TMatrix4x4;
  Theta90: Boolean;
begin
  wp := 0;
  wt := 0;
  wg := 0;

  tempmat := TMatrix4x4.Create;
  try
    tempmat.CopyFrom((Sender as TPolarKar).Mat);
    tempmat.GetLocals(ux, uy, uz);

    { Winkel Theta ermitteln im Bereich -90..90 Grad }
    tempsin := -ux.z;
    //tempcos := Dot(ux,zVec); //nicht verwendet
    wt := arcsin(CheckSinCos(tempsin));
    Theta90 := abs(tempsin * 180 / pi) > 89.9; //Theta90 := abs(tempsin) > 0.99;

    { Winkel Gamma ermitteln im Bereich -180..180 Grad }
    if Theta90 then
    begin
      //Winkel Gamma immer Null setzen, wenn lokale x-Achse senkrecht!
      //tempcos := 1;
      //tempsin := 0;
      wg := 0;
    end
    else
    begin
      tempY := Normalize3D(Cross(zVec, ux));
      tempZ := Normalize3D(Cross(ux, tempY));
      tempcos := dot(uz, tempZ);
      tempsin := -dot(uz, tempY);
      wg := ArcCos(CheckSinCos(tempcos));
      if tempsin < 0 then
        wg := -wg;
    end;

    { Winkel Phi ermitteln im Bereich -180..180 Grad }
    if Theta90 then
    begin
      tempVec := Normalize3D(Cross(uy, zVec));
      tempcos := tempVec.x; //tempcos := -uz.x;
      tempsin := tempVec.y; //tempsin := -uz.y;
    end
    else
    begin
      tempVec := ux;
      tempVec.z := 0;
      tempVec := Normalize3D(tempVec); //d := Hypot(ux.x,ux.y);
      tempcos := dot(xVec, tempVec); //tempcos := ux.x/d;
      tempsin := dot(yVec, tempVec); //tempsin := ux.y/d;
    end;
    wp := ArcCos(CheckSinCos(tempcos));
    if tempsin < 0 then
      wp := -wp;

    wg := wg * 180 / pi;
    wt := wt * 180 / pi;
    wp := wp * 180 / pi;

    wg := Round(wg * 10) / 10;
    wt := Round(wt * 10) / 10;
    wp := Round(wp * 10) / 10;

  finally
    tempmat.Free;
  end;
end;

end.
