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
  RggUnit4;

type
  TSalingDreieck = class
  private
    FSalingHMin, FSalingH, FSalingHMax: single;
    FSalingAMin, FSalingA, FSalingAMax: single;

    procedure SetSalingH(Value: single);
    procedure SetSalingA(Value: single);
    procedure SetSalingL(Value: single);
    procedure SetSalingW(Value: single);

    function GetSalingL: single;
    function GetSalingW: single;

    function GetSalingLMin: single;
    function GetSalingLMax: single;
    function GetSalingWMin: single;
    function GetSalingWMax: single;
  public
    constructor Create;
    procedure CopyFromRigg(Rigg: TRigg);
    procedure GetLW(H, A: single; out L, W: single);

    property Saling_H: single read FSalingH write SetSalingH;
    property Saling_A: single read FSalingH write SetSalingA;
    property Saling_L: single read GetSalingL write SetSalingL;
    property Saling_W: single read GetSalingW write SetSalingW;

    property Saling_AMin: single read FSalingAMin write FSalingAMin;
    property Saling_AMax: single read FSalingAMax write FSalingAMax;
    property Saling_HMin: single read FSalingHMin write FSalingHMin;
    property Saling_HMax: single read FSalingHMax write FSalingHMax;

    property Saling_LMin: single read GetSalingLMin;
    property Saling_LMax: single read GetSalingLMax;
    property Saling_WMin: single read GetSalingWMin;
    property Saling_WMax: single read GetSalingWMax;
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

procedure TSalingDreieck.GetLW(H, A: single; out L, W: single);
begin
  L := Hypot(H, A / 2);
  W := arctan2(H, A / 2);
end;

function TSalingDreieck.GetSalingL: single;
begin
  result := Hypot(FSalingH, FSalingA / 2);
end;

function TSalingDreieck.GetSalingW: single;
begin
  result := arctan2(FSalingH, FSalingA / 2);
end;

procedure TSalingDreieck.SetSalingH(Value: single);
begin
  if Value < FSalingHMin then
    FSalingH := FSalingHMin
  else if Value > FSalingHMax then
    FSalingH := FSalingHMax
  else
    FSalingH := Value;
end;

procedure TSalingDreieck.SetSalingA(Value: single);
begin
  if Value < FSalingAMin then
    FSalingA := FSalingAMin
  else if Value > FSalingHMax then
    FSalingA := FSalingAMax
  else
    FSalingA := Value;
end;

procedure TSalingDreieck.SetSalingL(Value: single);
var
  temp: single;
begin
  temp := Value / Saling_L;
  FSalingH := temp * FSalingH;
  FSalingA := temp * FSalingA;
end;

procedure TSalingDreieck.SetSalingW(Value: single);
var
  tempW, tempL: single;
begin
  tempW := Value * pi / 180;
  tempL := Saling_L;
  FSalingH := tempL * sin(tempW);
  FSalingA := 2 * tempL * cos(tempW);
end;

function TSalingDreieck.GetSalingLMin: single;
var
  tempL, tempW: single;
begin
  GetLW(FSalingHMin, FSalingAMin, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingLMax: single;
var
  tempL, tempW: single;
begin
  GetLW(FSalingHMax, FSalingAMax, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingWMin: single;
var
  tempL, tempW: single;
begin
  GetLW(FSalingHMin, FSalingAMax, tempL, tempW);
  result := tempW;
end;

function TSalingDreieck.GetSalingWMax: single;
var
  tempL, tempW: single;
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
