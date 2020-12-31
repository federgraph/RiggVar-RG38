unit RiggVar.App.Controller;

interface

{.$define WantFederText}

uses
  System.Classes,
  RiggVar.RG.Data,
  RiggVar.FB.Action,
  RiggVar.FB.ActionKeys,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionMap,
  RiggVar.FederModel.ActionList,
  RiggVar.Util.Logger;

type
  TRggMain = class
  public
    Logger: TLogger;
    RggData: TRggData;
    Trimm0: TRggData;
    Trimm1: TRggData;
    Trimm2: TRggData;
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;
    Trimm7: TRggData;
    Trimm8: TRggData;

    Keyboard: TFederKeyboard;

    ActionGroupList: TActionGroupList;

    IsUp: Boolean;

    ActionHandler: TFederActionHandlerBase;

    ActionList: TRggActionList;

    ActionMapTablet: TActionMap;
    ActionMapPhone: TActionMap;

    IsLandscape: Boolean;

    constructor Create;
    destructor Destroy; override;
    function GetTrimmItem(idx: Integer): TRggData;
    procedure CollectShortcuts(fa: Integer; ML: TStrings);
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    procedure FederTextCheckState;
    procedure DoTouchbarBottom(Value: single);
    procedure DoTouchbarLeft(Value: single);
    procedure DoTouchbarRight(Value: single);
    procedure DoTouchbarTop(Value: single);

  end;

implementation

uses
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet;

{ TRggMain }

constructor TRggMain.Create;
begin
  Logger := TLogger.Create;

  RggData := TRggData.Create;

  Trimm0 := TRggData.Create;
  Trimm1 := TRggData.Create;
  Trimm2 := TRggData.Create;
  Trimm3 := TRggData.Create;
  Trimm4 := TRggData.Create;
  Trimm5 := TRggData.Create;
  Trimm6 := TRggData.Create;
  Trimm7 := TRggData.Create;
  Trimm8 := TRggData.Create;

  Keyboard := TFederKeyboard.Create;

  ActionGroupList := TActionGroupList.Create;

  ActionHandler := TFederActionHandlerBase.Create;

  ActionList := TRggActionList.Create(nil);

  ActionMapTablet := TActionMapTablet.Create;
  ActionMapPhone := TActionMapPhone.Create;
end;

destructor TRggMain.Destroy;
begin
  ActionMapPhone.Free;
  ActionMapTablet.Free;

  ActionList.Free;

  ActionHandler.Free;

  ActionGroupList.Free;

  Keyboard.Free;

  Trimm0.Free;
  Trimm1.Free;
  Trimm2.Free;
  Trimm3.Free;
  Trimm4.Free;
  Trimm5.Free;
  Trimm6.Free;
  Trimm7.Free;
  Trimm8.Free;

  RggData.Free;

  Logger.Free;

  inherited;
end;

function TRggMain.GetTrimmItem(idx: Integer): TRggData;
begin
  result := RggData;
end;

procedure TRggMain.CollectShortcuts(fa: Integer; ML: TStrings);
begin

end;

procedure TRggMain.HandleAction(fa: Integer);
begin

end;

procedure TRggMain.FederTextCheckState;
begin
{$ifdef WantFederText}
  FederText.CheckState;
{$endif}
end;

procedure TRggMain.DoTouchbarBottom(Value: single);
begin

end;

procedure TRggMain.DoTouchbarLeft(Value: single);
begin

end;

procedure TRggMain.DoTouchbarRight(Value: single);
begin

end;

procedure TRggMain.DoTouchbarTop(Value: single);
begin

end;

function TRggMain.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

end.
