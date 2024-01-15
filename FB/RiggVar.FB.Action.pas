unit RiggVar.FB.Action;

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
  RiggVar.FB.ActionConst;

type
  IFederActionHandler = interface
  ['{F4DE90A6-6D46-42F0-8119-F309A3802479}']
    procedure Execute(fa: TFederAction);
    function GetEnabled(fa: TFederAction): Boolean;
    function GetChecked(fa: TFederAction): Boolean;
    function GetCaption(fa: TFederAction): string;
    function GetShortCaption(fa: TFederAction): string;
    function GetShortcutString(fa: TFederAction): string;
  end;

  TFederActionHandlerBase = class(TInterfacedObject, IFederActionHandler)
  public
    procedure Execute(fa: TFederAction); virtual;
    function GetEnabled(fa: TFederAction): Boolean; virtual;
    function GetChecked(fa: TFederAction): Boolean; virtual;
    function GetCaption(fa: TFederAction): string; virtual;
    function GetShortCaption(fa: TFederAction): string; virtual;
    function GetShortcutString(fa: TFederAction): string; virtual;
  end;

implementation

uses
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

function TFederActionHandlerBase.GetShortCaption(fa: TFederAction): string;
begin
  result := GetFederActionShort(fa);
end;

function TFederActionHandlerBase.GetShortcutString(fa: TFederAction): string;
begin
  { virtual, do nothing here }
  result := '';
end;

function TFederActionHandlerBase.GetCaption(fa: TFederAction): string;
begin
  result := GetFederActionLong(fa);
end;

function TFederActionHandlerBase.GetChecked(fa: TFederAction): Boolean;
begin
  result := False;
end;

function TFederActionHandlerBase.GetEnabled(fa: TFederAction): Boolean;
begin
  result := True;
end;

procedure TFederActionHandlerBase.Execute(fa: TFederAction);
begin
  { virtual, do nothing here }
end;

end.
