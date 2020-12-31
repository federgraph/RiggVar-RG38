unit RiggVar.RG.View;

interface

uses
  System.Classes;

type
  IFormMain = interface
  ['{6309C56E-FD06-413A-923E-1C808A7DD459}']
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;

    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;

    procedure ShowTrimm;
    procedure UpdateColorScheme;
    procedure ToggleButtonSize;

    procedure RotaFormRotateZ(Delta: single);
    procedure RotaFormZoom(Delta: single);

    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;

    procedure UpdateOnParamValueChanged;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;

    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    procedure SetClientHeight(const Value: Integer);
    procedure SetClientWidth(const Value: Integer);

    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);

    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  end;

  TDummmyFormMain = class(TInterfacedObject, IFormMain)
  private
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    procedure SetClientHeight(const Value: Integer);
    procedure SetClientWidth(const Value: Integer);

    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);
  public
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;

    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;

    procedure ShowTrimm;
    procedure UpdateColorScheme;
    procedure ToggleButtonSize;
    procedure UpdateOnParamValueChanged;

    procedure RotaFormRotateZ(Delta: single);
    procedure RotaFormZoom(Delta: single);

    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;

    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;

    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  end;

implementation

uses
  RiggVar.App.Main;

{ TDummmyMain }

function TDummmyFormMain.GetActionFromKey(Shift: TShiftState;
  Key: Word): Integer;
begin
  result := 0;
end;

function TDummmyFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
begin
  result := 0;
end;

function TDummmyFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

function TDummmyFormMain.GetClientHeight: Integer;
begin
  result := MainVar.ClientHeight;
end;

function TDummmyFormMain.GetClientWidth: Integer;
begin
  result := MainVar.ClientWidth;
end;

function TDummmyFormMain.GetOpenFileName(dn, fn: string): string;
begin
  result := '';
end;

function TDummmyFormMain.GetSaveFileName(dn, fn: string): string;
begin
  result := '';
end;

function TDummmyFormMain.GetShowDataText: Boolean;
begin
  result := False;
end;

function TDummmyFormMain.GetShowDiffText: Boolean;
begin
  result := False;
end;

function TDummmyFormMain.GetShowTrimmText: Boolean;
begin
  result := False;
end;

procedure TDummmyFormMain.HandleAction(fa: Integer);
begin

end;

procedure TDummmyFormMain.RotaFormRotateZ(Delta: single);
begin

end;

procedure TDummmyFormMain.RotaFormZoom(Delta: single);
begin

end;

procedure TDummmyFormMain.SetClientHeight(const Value: Integer);
begin
  MainVar.ClientHeight := Value;
end;

procedure TDummmyFormMain.SetClientWidth(const Value: Integer);
begin
  MainVar.ClientWidth := Value;
end;

procedure TDummmyFormMain.SetShowDataText(const Value: Boolean);
begin

end;

procedure TDummmyFormMain.SetShowDiffText(const Value: Boolean);
begin

end;

procedure TDummmyFormMain.SetShowTrimmText(const Value: Boolean);
begin

end;

procedure TDummmyFormMain.ShowTrimm;
begin

end;

procedure TDummmyFormMain.ToggleButtonSize;
begin

end;

procedure TDummmyFormMain.UpdateColorScheme;
begin

end;

procedure TDummmyFormMain.UpdateItemIndexParams;
begin

end;

procedure TDummmyFormMain.UpdateItemIndexReports;
begin

end;

procedure TDummmyFormMain.UpdateItemIndexTrimms;
begin

end;

procedure TDummmyFormMain.UpdateOnParamValueChanged;
begin

end;

end.
