﻿unit RiggVar.FederModel.ActionList;

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
  System.Actions,
  FMX.ActnList,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

type
  TRggAction = class(TCustomAction)
  public
    UseShortCaption: Boolean;
    UseLocalization: Boolean;
  end;

  TRggActionList = class(TActionList)
  private
    procedure DoOnExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoOnUpdate(Action: TBasicAction; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function FindFederAction(fa: Integer; WantLocalizedCaption: Boolean; WantShortCaption: Boolean): TContainedAction;
    function GetFederAction(fa: Integer; WantLocalizedCaption: Boolean; WantShortCaption: Boolean): TContainedAction;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRggActionList }

constructor TRggActionList.Create(AOwner: TComponent);
begin
  inherited;
  OnExecute := DoOnExecute;
  OnUpdate := DoOnUpdate;
end;

procedure TRggActionList.DoOnExecute(Action: TBasicAction; var Handled: Boolean);
begin
  Main.ActionHandler.Execute(Action.Tag);
  Handled := True;
end;

procedure TRggActionList.DoOnUpdate(Action: TBasicAction; var Handled: Boolean);
var
  cr: TRggAction;
begin
  cr := TRggAction(Action);
  cr.Checked := Main.GetChecked(cr.Tag);
  cr.Enabled := Main.GetEnabled(cr.Tag);
  Handled := True;
end;

function TRggActionList.FindFederAction(fa: Integer; WantLocalizedCaption: Boolean; WantShortCaption: Boolean): TContainedAction;
var
  i: Integer;
  ca: TContainedAction;
  cr: TRggAction;
begin
  result := nil;
  for i := 0 to ActionCount-1 do
  begin
    ca := Actions[i];
    if (ca is TRggAction) then
    begin
      cr := TRggAction(ca);
      if (cr.Tag = fa) and (cr.UseShortCaption = WantShortCaption) and ((cr.UseLocalization = WantLocalizedCaption)) then
      begin
        result := cr;
        Exit;
      end;
    end;
  end;
end;

function TRggActionList.GetFederAction(fa: Integer; WantLocalizedCaption: Boolean; WantShortCaption: Boolean): TContainedAction;
var
  ca: TContainedAction;
  cr: TRggAction;
begin
  ca := FindFederAction(fa, WantLocalizedCaption, WantShortCaption);

  if ca = nil then
  begin
    cr := TRggAction.Create(Self);
    cr.Tag := fa;
    cr.UseLocalization := WantLocalizedCaption;
    cr.UseShortCaption := WantShortCaption;
    if WantShortCaption then
      cr.Caption := Main.ActionHandler.GetShortCaption(fa)
    else
      cr.Caption := Main.ActionHandler.GetCaption(fa);
    cr.Hint := Main.ActionHandler.GetCaption(fa);
    AddAction(cr);
    ca := cr;
  end;

  result := ca;
end;


end.
