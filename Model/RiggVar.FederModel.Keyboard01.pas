unit RiggVar.FederModel.Keyboard01;

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
  System.Classes,
  System.UITypes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionKeys;

type
  TFederKeyboard01 = class(TFederKeyboard)
  public
    constructor Create;
    function KeyUpAction(var Key: Word; var KeyChar: WideChar; Shift: TShiftState): TFederAction; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TFederKeyboard01 }

constructor TFederKeyboard01.Create;
begin
  inherited;
  TestName := 'Keyboard';
end;

function TFederKeyboard01.KeyUpAction(var Key: Word; var KeyChar: WideChar; Shift: TShiftState): TFederAction;
var
  fa: Integer;
begin
  fa := Main.MainView.GetActionFromKey(Shift, Key);
  if fa = faNoop then
    fa := Main.MainView.GetActionFromKeyChar(KeyChar);
  result := fa;
end;

end.
