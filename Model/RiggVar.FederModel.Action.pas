unit RiggVar.FederModel.Action;

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
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action;

type
  TFederActionHandler = class(TFederActionHandlerBase)
  public
    procedure Execute(fa: TFederAction); override;
    function GetEnabled(fa: TFederAction): Boolean; override;
    function GetChecked(fa: TFederAction): Boolean; override;
    function GetCaption(fa: TFederAction): string; override;
    function GetShortCaption(fa: TFederAction): string; override;
    function GetShortcutString(fa: TFederAction): string; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionShortEN,
  RiggVar.FB.ActionShortDE,
  RiggVar.FB.ActionLongEN,
  RiggVar.FB.ActionLongDE;

procedure TFederActionHandler.Execute(fa: TFederAction);
var
  M: TMain;
begin
  M := Main;

  if not Assigned(M) then
    Exit;
  if not M.IsUp then
    Exit;

  M.HandleAction(fa);
  M.FederTextCheckState;
end;

function TFederActionHandler.GetChecked(fa: TFederAction): Boolean;
var
  M: TMain;
begin
  M := Main;
  result := false;
  if not Assigned(M) then
    Exit;
  if not M.IsUp then
    Exit;

    result := Main.GetChecked(fa);
end;

function TFederActionHandler.GetEnabled(fa: TFederAction): Boolean;
begin
  if (Main <> nil) and (Main.MainView <> nil) then
    result := Main.MainView.GetEnabled(fa)
    else
      result := True;
end;

function TFederActionHandler.GetShortCaption(fa: TFederAction): string;
var
  M: TMain;
begin
  result := '';
  M := Main;

  if not Assigned(M) then
    Exit;
  if not M.IsUp then
    Exit;

  if not MainVar.WantLocalizedText then
  begin
    result := inherited
  end
  else
  begin
    if MainVar.WantGermanText then
      result := GetFederActionShortDE(fa)
    else
      result := GetFederActionShortEN(fa)
  end;
end;

function TFederActionHandler.GetShortcutString(fa: TFederAction): string;
begin
  result := Main.Keyboard.GetShortcut(fa);
end;

function TFederActionHandler.GetCaption(fa: TFederAction): string;
begin
  result := '';

  if not MainVar.WantLocalizedText then
  begin
    result := inherited
  end
  else
  begin
    if MainVar.WantGermanText then
      result := GetFederActionLongDE(fa)
    else
      result := GetFederActionLongEN(fa)
  end;
end;

end.
