unit RggSaling3Eck;

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

uses
  System.Math,
  RggTypes,
  RggCalc,
  RggUnit4;

type
  TSalingDreieck = class
  private
    FSalingHMin, FSalingH, FSalingHMax: double;
    FSalingAMin, FSalingA, FSalingAMax: double;

    procedure SetSalingH(Value: double);
    procedure SetSalingA(Value: double);
    procedure SetSalingL(Value: double);
    procedure SetSalingW(Value: double);

    function GetSalingL: double;
    function GetSalingW: double;

    function GetSalingLMin: double;
    function GetSalingLMax: double;
    function GetSalingWMin: double;
    function GetSalingWMax: double;
  public
    constructor Create;
    procedure CopyFromRigg(Rigg: TRigg);
    procedure GetLW(H, A: double; out L, W: double);

    property Saling_H: double read FSalingH write SetSalingH;
    property Saling_A: double read FSalingH write SetSalingA;
    property Saling_L: double read GetSalingL write SetSalingL;
    property Saling_W: double read GetSalingW write SetSalingW;

    property Saling_AMin: double read FSalingAMin write FSalingAMin;
    property Saling_AMax: double read FSalingAMax write FSalingAMax;
    property Saling_HMin: double read FSalingHMin write FSalingHMin;
    property Saling_HMax: double read FSalingHMax write FSalingHMax;

    property Saling_LMin: double read GetSalingLMin;
    property Saling_LMax: double read GetSalingLMax;
    property Saling_WMin: double read GetSalingWMin;
    property Saling_WMax: double read GetSalingWMax;
  end;

implementation

constructor TSalingDreieck.Create;
begin
  FSalingHMin := 150;
  FSalingH := 200;
  FSalingHMax := 300;
  FSalingAMin := 700;
  FSalingA := 850;
  FSalingAMax := 1100;
end;

procedure TSalingDreieck.GetLW(H, A: double; out L, W: double);
begin
  L := Hypot(H, A / 2);
  W := arctan2(H, A / 2);
end;

function TSalingDreieck.GetSalingL: double;
begin
  result := Hypot(FSalingH, FSalingA / 2);
end;

function TSalingDreieck.GetSalingW: double;
begin
  result := arctan2(FSalingH, FSalingA / 2);
end;

procedure TSalingDreieck.SetSalingH(Value: double);
begin
  if Value < FSalingHMin then
    FSalingH := FSalingHMin
  else if Value > FSalingHMax then
    FSalingH := FSalingHMax
  else
    FSalingH := Value;
end;

procedure TSalingDreieck.SetSalingA(Value: double);
begin
  if Value < FSalingAMin then
    FSalingA := FSalingAMin
  else if Value > FSalingHMax then
    FSalingA := FSalingAMax
  else
    FSalingA := Value;
end;

procedure TSalingDreieck.SetSalingL(Value: double);
var
  temp: double;
begin
  temp := Value / Saling_L;
  FSalingH := temp * FSalingH;
  FSalingA := temp * FSalingA;
end;

procedure TSalingDreieck.SetSalingW(Value: double);
var
  tempW, tempL: double;
begin
  tempW := Value * pi / 180;
  tempL := Saling_L;
  FSalingH := tempL * sin(tempW);
  FSalingA := 2 * tempL * cos(tempW);
end;

function TSalingDreieck.GetSalingLMin: double;
var
  tempL, tempW: double;
begin
  GetLW(FSalingHMin, FSalingAMin, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingLMax: double;
var
  tempL, tempW: double;
begin
  GetLW(FSalingHMax, FSalingAMax, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingWMin: double;
var
  tempL, tempW: double;
begin
  GetLW(FSalingHMin, FSalingAMax, tempL, tempW);
  result := tempW;
end;

function TSalingDreieck.GetSalingWMax: double;
var
  tempL, tempW: double;
begin
  GetLW(FSalingHMax, FSalingAMin, tempL, tempW);
  result := tempW;
end;

procedure TSalingDreieck.CopyFromRigg(Rigg: TRigg);
begin
  FSalingHMin := Rigg.GSB.SalingH.Min;
  FSalingHMax := Rigg.GSB.SalingH.Max;
  FSalingH := Rigg.GSB.SalingH.Ist;
  FSalingAMin := Rigg.GSB.SalingA.Min;
  FSalingAMax := Rigg.GSB.SalingA.Max;
  FSalingA := Rigg.GSB.SalingA.Ist;
end;

end.
