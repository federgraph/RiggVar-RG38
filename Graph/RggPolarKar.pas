unit RggPolarKar;

interface

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  RggMatrix,
  RggTypes;

type
  TCalcAngleEvent = procedure(Sender: TObject; var wx, wy, wz: single) of object;

  TPolarKar = class
  private
    FPhi, FTheta, FGamma, FXRot, FYRot, FZRot: single;
    FValid: Boolean;
    FMode: Boolean;
    FOnCalcAngle: TCalcAngleEvent;
    procedure SetPhi(Value: single);
    procedure SetTheta(Value: single);
    procedure SetGamma(Value: single);
    procedure SetXrot(Value: single);
    procedure SetYrot(Value: single);
    procedure SetZrot(Value: single);
    procedure SetRotMode(Value: Boolean);
    procedure SetRotAngle(index: TRotationAngle; Value: single);
    function GetPhi: single;
    function GetTheta: single;
    function GetGamma: single;
    function GetXrot: single;
    function GetYrot: single;
    function GetZrot: single;
    function GetRotAngle(index: TRotationAngle): single;
    procedure GetLocals(M: TMatrix3D; out ux, uy, uz: TPoint3D);
    function Rotate(M: TMatrix3D; p1, p2: TPoint3D; angle: single): TMatrix3D;
  protected
    p1, p2: TPoint3D;
    Angle: single;
    tmat: TMatrix3D;
    procedure GetMat;
    procedure FillMatrix;
    procedure FillMatrixInc;
    function GetMatrix: TMatrix3D;
    procedure SetMatrix(Value: TMatrix3D);
  public
    Mat: TMatrix3D;
    constructor Create;
    destructor Destroy; override;
    function Rotiere(Punkt: TPoint3D): TPoint3D;
    procedure Reset;
    procedure GetAngle(var wx, wy, wz: single);
    procedure GetAngle1(Sender: TObject; var wx, wy, wz: single);
    procedure GetAngle2(Sender: TObject; var wp, wt, wg: single);
    property DeltaTheta: single read GetTheta write SetTheta;
    property DeltaPhi: single read GetPhi write SetPhi;
    property DeltaGamma: single read GetGamma write SetGamma;
    property XRot: single read GetXrot write SetXrot;
    property YRot: single read GetYrot write SetYrot;
    property ZRot: single read GetZrot write SetZrot;
    property RotAngle[index: TRotationAngle]: single read GetRotAngle write SetRotAngle;
    property Matrix: TMatrix3D read GetMatrix write SetMatrix;
    property Mode: Boolean read FMode write SetRotMode;
    property OnCalcAngle: TCalcAngleEvent read FOnCalcAngle write FOnCalcAngle;
  end;

implementation

constructor TPolarKar.Create;
begin
  Mat := TMatrix3D.Identity;
  tmat := TMatrix3D.Identity;
  Reset;
end;

destructor TPolarKar.Destroy;
begin
end;

procedure TPolarKar.SetPhi(Value: single);
begin
  FPhi := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetTheta(Value: single);
begin
  FTheta := Value * pi / 180;
  FValid := False;
end;

procedure TPolarKar.SetGamma(Value: single);
begin
  FGamma := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetXrot(Value: single);
begin
  FXRot := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetYrot(Value: single);
begin
  FYRot := DegToRad(Value);
  FValid := False;
end;

procedure TPolarKar.SetZrot(Value: single);
begin
  FZRot := DegToRad(Value);
  FValid := False;
end;

function TPolarKar.GetPhi: single;
begin
  Result := Int(RadToDeg(FPhi));
end;

function TPolarKar.GetTheta: single;
begin
  Result := Int(RadToDeg(FTheta));
end;

function TPolarKar.GetGamma: single;
begin
  Result := Int(RadToDeg(FGamma));
end;

function TPolarKar.GetXrot: single;
begin
  Result := Int(RadToDeg(FXRot));
end;

function TPolarKar.GetYrot: single;
begin
  Result := Int(RadToDeg(FYRot));
end;

function TPolarKar.GetZrot: single;
begin
  Result := Int(RadToDeg(FZRot));
end;

procedure TPolarKar.SetRotAngle(index: TRotationAngle; Value: single);
var
  temp: single;
begin
  temp := DegToRad(Value);
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

function TPolarKar.GetRotAngle(index: TRotationAngle): single;
var
  temp: single;
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
  Result := Int(RadToDeg(temp));
end;

function TPolarKar.GetMatrix: TMatrix3D;
begin
  if FValid = False then
    GetMat;
  Result := Mat;
end;

procedure TPolarKar.SetMatrix(Value: TMatrix3D);
begin
  Reset;
  Mat := Value;
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
  tmat := TMatrix3D.Identity;
  p1 := TPoint3D.Zero;
  if FTheta <> 0 then
  begin
    p2.x := Mat.m12;
    p2.y := Mat.m22;
    p2.z := Mat.m32;
    Angle := FTheta;
    tmat := Rotate(tmat, p1, p2, Angle);
    FTheta := 0;
  end;
  if FPhi <> 0 then
  begin
    p2.x := Mat.m11;
    p2.y := Mat.m21;
    p2.z := Mat.m31;
    Angle := -FPhi;
    tmat := Rotate(tmat, p1, p2, Angle);
    FPhi := 0;
  end;
  if FGamma <> 0 then
  begin
    p2.x := Mat.m13;
    p2.y := Mat.m23;
    p2.z := Mat.m33;
    Angle := FGamma;
    tmat := Rotate(tmat, p1, p2, Angle);
    FGamma := 0;
  end;
  if FZRot <> 0 then
  begin
    p2 := yVec;
    Angle := FZRot;
    tmat := Rotate(tmat, p1, p2, Angle);
    FZRot := 0;
  end;
  if FYRot <> 0 then
  begin
    p2 := xVec;
    Angle := FYRot;
    tmat := Rotate(tmat, p1, p2, Angle);
    FYRot := 0;
  end;
  if FXRot <> 0 then
  begin
    p2 := zVec;
    Angle := FXRot;
    tmat := Rotate(tmat, p1, p2, Angle);
    FXRot := 0;
  end;
  FValid := True;
  Mat := Mat * tmat;
end;

procedure TPolarKar.FillMatrix;
begin
  { für Absolutmodus }
  Mat := Mat.Identity;
  { 1. Rotation um globale y-Achse, gleichzeitig lokale y-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 1.0;
  p2.z := 0.0;
  Angle := FTheta;
  Mat := Rotate(Mat, p1, p2, Angle);
  { 2. Rotation um globale z-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := 0.0;
  p2.y := 0.0;
  p2.z := 1.0;
  Angle := FPhi;
  Mat := Rotate(Mat, p1, p2, Angle);
  { 3. Rotation um locale x-Achse }
  p1.x := 0.0;
  p1.y := 0.0;
  p1.z := 0.0;
  p2.x := Mat.m11;
  p2.y := Mat.m21;
  p2.z := Mat.m31;
  Angle := FGamma;
  Mat := Rotate(Mat, p1, p2, Angle);
  FValid := True;
end;

function TPolarKar.Rotiere(Punkt: TPoint3D): TPoint3D;
begin
  if FValid = False then
    GetMat;
  result := Punkt * Mat;
end;

procedure TPolarKar.Reset;
begin
  Mat := Mat.Identity;
  FPhi := 0;
  FTheta := 0;
  FGamma := 0;
  FXRot := 0;
  FYRot := 0;
  FZRot := 0;
  FValid := True;
end;

procedure TPolarKar.GetAngle(var wx, wy, wz: single);
begin
  wx := 0;
  wy := 0;
  wz := 0;
  if Assigned(OnCalcAngle) then
    OnCalcAngle(Self, wx, wy, wz);
end;

procedure TPolarKar.GetAngle1(Sender: TObject; var wx, wy, wz: single);

  function angle(a, b: TPoint3D): single;
  var
    temp: single;
  begin
    temp := a.DotProduct(b);
    if temp > 1 then
      temp := 1;
    if temp < -1 then
      temp := -1;
    Result := RadToDeg(ArcCos(temp));
  end;

var
  FLocalX, FlocalY, FLocalZ: TPoint3D;
begin
  GetLocals(Mat, FLocalX, FlocalY, FLocalZ);
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

procedure TPolarKar.GetAngle2(Sender: TObject; var wp, wt, wg: single);

  function CheckSinCos(c: Extended): Extended;
  begin
//    Assert(c <= 1, Format('sincos > 1 (%6.5f)', [c]));
//    Assert(c >= -1, Format('sincos < -1 (%6.5f)', [c]));
    if c > 1 then
      c := 1;
    if c < -1 then
      c := -1;
    Result := c;
  end;

var
  tempcos, tempsin: single;
  ux, uy, uz, tempVec, tempY, tempZ: TPoint3D;
  tempmat: TMatrix3D;
  Theta90: Boolean;
begin
  wp := 0;
  wt := 0;
  wg := 0;

  tempmat := (Sender as TPolarKar).Mat;
  GetLocals(tempMat, ux, uy, uz);

  { Winkel Theta ermitteln im Bereich -90..90 Grad }
  tempsin := -ux.z;
  //tempcos := Dot(ux,zVec); //nicht verwendet
  wt := arcsin(CheckSinCos(tempsin));
  Theta90 := abs(RadToDeg(tempsin)) > 89.9; //Theta90 := abs(tempsin) > 0.99;

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
    tempY := zVec.CrossProduct(ux).Normalize;
    tempZ := ux.CrossProduct(tempY).Normalize;
    tempcos := uz.DotProduct(tempZ);
    tempsin := -uz.DotProduct(tempY);
    wg := ArcCos(CheckSinCos(tempcos));
    if tempsin < 0 then
      wg := -wg;
  end;

  { Winkel Phi ermitteln im Bereich -180..180 Grad }
  if Theta90 then
  begin
    tempVec := uy.CrossProduct(zVec).Normalize;
    tempcos := tempVec.x;
    tempsin := tempVec.y;
  end
  else
  begin
    tempVec := ux;
    tempVec.z := 0;
    tempVec := tempVec.Normalize;
    tempcos := xVec.DotProduct(tempVec);
    tempsin := yVec.DotProduct(tempVec);
  end;
  wp := ArcCos(CheckSinCos(tempcos));
  if tempsin < 0 then
    wp := -wp;

  wg := RadToDeg(wg);
  wt := RadToDeg(wt);
  wp := RadToDeg(wp);

  wg := Round(wg * 10) / 10;
  wt := Round(wt * 10) / 10;
  wp := Round(wp * 10) / 10;
end;

procedure TPolarKar.GetLocals(M: TMatrix3D; out ux, uy, uz: TPoint3D);
begin
  ux.x := M.m11;
  ux.y := M.m21;
  ux.z := M.m31;

  uy.x := M.m12;
  uy.y := M.m22;
  uy.z := M.m32;

  uz.x := M.m13;
  uz.y := M.m23;
  uz.z := M.m33;
end;

function TPolarKar.Rotate(M: TMatrix3D; p1, p2: TPoint3D; angle: single): TMatrix3D;
var
  MT1, MR, MT2: TMatrix3D;
begin
  MT1 := TMatrix3D.CreateTranslation(-p1);
  MR := TMatrix3D.CreateRotation(p2-p1, angle);
  MT2 := TMatrix3D.CreateTranslation(p1);
  result := M * MT1 * MR * MT2;
end;

end.
