unit RggMatrix;

interface

uses
  System.UIConsts,
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  RggTypes,
  FMX.Graphics;

const
  maxvert = 400;
  maxcon = 1000;

type
  TRotationAngle = (raPhi, raTheta, raGamma, raXrot, raYrot, raZrot);

  TVertArrayF = array [0 .. maxvert] of single;
  TVertArrayI = array [0 .. maxvert] of Integer;
  TConArray = array [0 .. maxcon] of Integer;

  TRotaData = record
    Xpos: single;
    Ypos: single;
    IncrementIndex: Integer;
    IncrementT: single;
    IncrementW: single;
    ZoomIndex: Integer;
    FixpunktIndex: Integer;
    Matrix: TMatrix3D;
  end;

  TRotaParams = class
  private
    FZoomBase: single;
    FZoomIndex: Integer;
    FIncrementIndex: Integer;
    procedure SetIncrementIndex(Value: Integer);
    procedure SetFixPunktIndex(Value: Integer);
    procedure SetZoomIndex(Value: Integer);
    function GetIncrementT: Integer;
    function GetIncrementW: single;
    function GetFixPunktIndex: Integer;
    function GetZoom: single;
  public
    Xpos: Integer;
    Ypos: Integer;
    FixPunkt: TRiggPoint;
    Phi, Theta, Gamma, XRot, YRot, ZRot: Integer;
    constructor Create;
    class function LookUpRa10(Index: Integer): single;
    property IncrementIndex: Integer read FIncrementIndex write SetIncrementIndex;
    property FixPunktIndex: Integer read GetFixPunktIndex write SetFixPunktIndex;
    property ZoomIndex: Integer read FZoomIndex write SetZoomIndex;
    property IncrementT: Integer read GetIncrementT;
    property IncrementW: single read GetIncrementW;
    property Zoom: single read GetZoom;
  end;

const
  xVec: TPoint3D = (x: 1; y: 0; z: 0);
  yVec: TPoint3D = (x: 0; y: 1; z: 0);
  zVec: TPoint3D = (x: 0; y: 0; z: 1);

implementation

{ TRotaParams }

constructor TRotaParams.Create;
begin
  FIncrementIndex := 3;
  FZoomBase := 0.05;
  FZoomIndex := 7;
  FixPunkt := ooD0;
  Phi := 0;
  Theta := -90;
  Gamma := 0;
  XRot := -87;
  YRot := 0;
  ZRot := 0;
end;

procedure TRotaParams.SetIncrementIndex(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 5 then
    Value := 5;
  FIncrementIndex := Value;
end;

procedure TRotaParams.SetZoomIndex(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 11 then
    Value := 11;
  FZoomIndex := Value;
end;

procedure TRotaParams.SetFixPunktIndex(Value: Integer);
begin
  case Value of
    0: FixPunkt := ooA0;
    1: FixPunkt := ooA;
    2: FixPunkt := ooB0;
    3: FixPunkt := ooB;
    4: FixPunkt := ooC0;
    5: FixPunkt := ooC;
    6: FixPunkt := ooD0;
    7: FixPunkt := ooD;
    8: FixPunkt := ooE0;
    9: FixPunkt := ooE;
   10: FixPunkt := ooF0;
   11: FixPunkt := ooF;
   else
     FixPunkt := ooD0;
  end;
end;

function TRotaParams.GetFixPunktIndex: Integer;
begin
  case FixPunkt of
    ooA0:  result :=  0;
    ooA:   result :=  1;
    ooB0:  result :=  2;
    ooB:   result :=  3;
    ooC0:  result :=  4;
    ooC:   result :=  5;
    ooD0:  result :=  6;
    ooD:   result :=  7;
    ooE0:  result :=  8;
    ooE:   result :=  9;
    ooF0:  result := 10;
    ooF:   result := 11;
    else
      result := 6;
  end;
end;

function TRotaParams.GetIncrementT: Integer;
begin
  case IncrementIndex of
    1: result := 1;
    2: result := 5;
    3: result := 10;
    4: result := 30;
    5: result := 100;
    else
      result := 1;
  end;
end;

function TRotaParams.GetIncrementW: single;
begin
  case IncrementIndex of
    1: result := 0.1;
    2: result := 1;
    3: result := 5;
    4: result := 10;
    5: result := 30;
    else
      result := 1;
  end;
end;

function TRotaParams.GetZoom: single;
begin
  result := FZoomBase * LookUpRa10(FZoomIndex);
end;

class function TRotaParams.LookUpRa10(Index: Integer): single;
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
