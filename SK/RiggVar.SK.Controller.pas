unit RiggVar.SK.Controller;

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
  System.SysUtils,
  System.Classes,
  System.Math,
  System.UIConsts,
  RiggVar.RG.Def,
  RiggVar.RG.Types;

type
  TMain0 =  class
  private
    FParam: TCircleParam;
    procedure SetParam(const Value: TCircleParam);
  public
    IsUp: Boolean;

    constructor Create;

    function Param2Text(fp: TCircleParam): string;
    function Text2Param(T: string): TCircleParam;

    procedure DoWheel(delta: single);

    property Param: TCircleParam read FParam write SetParam;
  end;

implementation

uses
  FrmSK03,
  RiggVar.SK.Main;

{ TMain0 }

constructor TMain0.Create;
begin
  Main := self;

  FParam := fpR1;
end;

function TMain0.Text2Param(T: string): TCircleParam;
begin
  result := fpR1;
  if T = Param2Text(fpR1) then
    result := fpR1
  else if T = Param2Text(fpR2) then
    result := fpR2
  else if T = Param2Text(fpM1X) then
    result := fpM1X
  else if T = Param2Text(fpM1Y) then
    result := fpM1Y
  else if T = Param2Text(fpM1Z) then
    result := fpM1Z
  else if T = Param2Text(fpM2X) then
    result := fpM2X
  else if T = Param2Text(fpM2Y) then
    result := fpM2Y
  else if T = Param2Text(fpM2Z) then
    result := fpM2Z
  else if T = Param2Text(fpA1) then
    result := fpA1
  else if T = Param2Text(fpA2) then
    result := fpA2
  else if T = Param2Text(fpE1) then
    result := fpE1
  else if T = Param2Text(fpE2) then
    result := fpE2
end;

function TMain0.Param2Text(fp: TCircleParam): string;
begin
  case fp of
    fpR1: result := 'Radius 1';
    fpR2: result := 'Radius 2';
    fpM1X: result := 'Center Point 1.X';
    fpM1Y: result := 'Center Point 1.Y';
    fpM2X: result := 'Center Point 2.X';
    fpM2Y: result := 'Center Point 2.Y';
    fpA1: result := 'Line 1 Angle';
    fpA2: result := 'Line 2 Angle';
    else
      result := 'None';
  end;
end;

procedure TMain0.SetParam(const Value: TCircleParam);
begin
  FParam := Value;
  FormMain.UpdateParam(Value);
end;

procedure TMain0.DoWheel(delta: single);
begin
  FormMain.HandleWheel(Round(delta));
end;

end.
