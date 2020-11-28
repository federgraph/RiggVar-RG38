unit RiggVar.FederModel.ActionList;

interface

uses
  System.Classes,
  System.Actions,
  FMX.ActnList,
  RiggVar.FB.ActionShort,
  RiggVAr.FB.ActionLong;

type
  TRggActionList = class(TActionList)
  private
    procedure DoOnExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoOnUpdate(Action: TBasicAction; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function FindFederAction(fa: Integer): TContainedAction;
    function GetFederAction(fa: Integer; WantShortCaption: Boolean): TContainedAction;
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
  cr: TContainedAction;
begin
  cr := TContainedAction(Action);
  cr.Checked := Main.GetChecked(cr.Tag);
  Handled := True;
end;

function TRggActionList.FindFederAction(fa: Integer): TContainedAction;
var
  i: Integer;
  cr: TContainedAction;
begin
  result := nil;
  for i := 0 to ActionCount-1 do
  begin
    cr := Actions[i];
    if cr.Tag = fa then
    begin
      result := cr;
      Exit;
    end;
  end;
end;

function TRggActionList.GetFederAction(fa: Integer; WantShortCaption: Boolean): TContainedAction;
var
  ca: TContainedAction;
  cr: TContainedAction;
begin
  ca := FindFederAction(fa);

  if ca = nil then
  begin
    cr := TCustomAction.Create(Self);
    cr.Tag := fa;
    if WantShortCaption then
      cr.Caption := GetFederActionShort(fa)
    else
      cr.Caption := GetFederActionLong(fa);
    AddAction(cr);
    ca := cr;
  end;

  result := ca;
end;


end.
