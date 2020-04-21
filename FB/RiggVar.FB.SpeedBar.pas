unit RiggVar.FB.SpeedBar;

interface

uses
  System.Classes,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  FMX.Types,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Styles.Objects,
  FMX.Controls.Presentation;

{$define FMX}

type
  TActionSpeedBar = class(TLayout)
  private
    function FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
    procedure InitLayoutProps;
  protected
    BtnColor: TAlphaColor;
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    SpeedPanelHeight: Integer;
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedButton;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;
    procedure InitSpeedButton(SB: TSpeedButton);
    procedure SpeedButtonClick(Sender: TObject); virtual;
    procedure UpdateCaptions;
    procedure UpdateHints;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ToggleSpeedButtonText;

    procedure InitSpeedButtons; virtual;
    procedure UpdateSpeedButtonDown; virtual;
    procedure UpdateSpeedButtonEnabled; virtual;
  end;

  TActionSpeedBarExample = class(TActionSpeedBar)
  private
    procedure TestBtnClick(Sender: TObject);
  public
    TestBtn: TSpeedButton;
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

constructor TActionSpeedBar.Create(AOwner: TComponent);
begin
  inherited;
  InitLayoutProps;
//  StyleLookUp := 'panelstyle';
//  InitSpeedButtons; // Main is still nil
end;

procedure TActionSpeedBar.InitLayoutProps;
begin
  BtnColor := claBlue;
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 2;
  BtnGroupSpace := 16;
  BtnWidth := 50;
  BtnHeight := 50;
  SpeedPanelHeight := BtnHeight + 2 * BtnTop;
end;

function TActionSpeedBar.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(Self);
  result.Parent := Self;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TActionSpeedBar.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
begin
  result := B;
  BtnLeft := BtnLeft + AGroupSpace;
{$ifdef Vcl}
  B.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace;
  B.Top := BtnTop;
{$endif}
{$ifdef FMX}
  B.Position.X := BtnLeft + BtnCounter * (BtnWidth + BtnSpace);
  B.Position.Y := BtnTop;
{$endif}
  B.Width := BtnWidth;
  B.Height := BtnHeight;
{$ifdef FMX}
  { Does not work.
    Because B not assigned yet to actual SpeedButton instance ? }
//  InitSpeedButton(B);
{$endif}
{$ifdef Vcl}
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
{$endif}
  Inc(BtnCounter);
end;

procedure TActionSpeedBar.UpdateCaptions;
var
  i: Integer;
  sb: TSpeedButton;
begin
  for i := 0 to Self.ChildrenCount-1 do
  begin
    if self.Children.Items[i] is TSpeedButton then
    begin
      sb := self.Children[i] as TSpeedButton;
      sb.Text := GetFederActionShort(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.UpdateHints;
var
  i: Integer;
  sb: TSpeedButton;
begin
  for i := 0 to Self.ChildrenCount-1 do
  begin
    if self.Children.Items[i] is TSpeedButton then
    begin
      sb := self.Children[i] as TSpeedButton;
      sb.Text := GetFederActionLong(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.InitSpeedButton(SB: TSpeedButton);
var
  cr: TButtonStyleTextObject;
begin
  if SB.Tag <> faNoop then
  begin
    sb.Text := Main.ActionHandler.GetShortCaption(SB.Tag);
    sb.Hint := Main.ActionHandler.GetCaption(SB.Tag);
    sb.OnClick := SpeedButtonClick;
  end;

  { Text must be set before changing Font.Size }

  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  if cr <> nil then
  begin
    cr.HotColor := claBeige;
    cr.NormalColor := BtnColor;
    cr.PressedColor := BtnColor;
    cr.Font.Size := 24;
  end;
end;

procedure TActionSpeedBar.ToggleSpeedButtonText;
var
  Settings: ITextSettings;
  Instance: TComponent;
  I: Integer;
begin
  { http://docwiki.embarcadero.com/RADStudio/Rio/en/Setting_Text_Parameters_in_FireMonkey }

  { This will toggle Font.Size for all Buttons on the SpeedBar. }
  for I := 0 to ChildrenCount - 1 do
  begin
    Instance := Children[I];
    if IInterface(Instance).QueryInterface(ITextSettings, Settings) = S_OK then
    begin
      Settings.TextSettings.BeginUpdate;
      try
        Settings.TextSettings.Font.Size := 32;
        if TStyledSetting.Size in Settings.StyledSettings then
          Settings.StyledSettings := Settings.StyledSettings - [TStyledSetting.Size]
        else
          Settings.StyledSettings := Settings.StyledSettings + [TStyledSetting.Size];
      finally
        Settings.TextSettings.EndUpdate;
      end;
    end;
  end;
end;

procedure TActionSpeedBar.InitSpeedButtons;
begin
  { virtual }
end;

function TActionSpeedBar.FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
var
  i: Integer;
  AObj: TFMXObject;
begin
  result := nil;
  for i := 0 to AParent.ChildrenCount - 1 do
  begin
    AObj := AParent.Children[i];
    if AObj.StyleName = AName then
      Result := AObj
    else
      Result := FindStyleByName(AObj, AName);
    if Assigned(result) then
      break;
  end;
end;

procedure TActionSpeedBar.SpeedButtonClick(Sender: TObject);
begin
  { virgual }
end;

procedure TActionSpeedBar.UpdateSpeedButtonDown;
begin
  { virtual }
end;

procedure TActionSpeedBar.UpdateSpeedButtonEnabled;
begin
  { virtual }
end;

{ TActionSpeedBarExample }

procedure TActionSpeedBarExample.InitSpeedButtons;
var
  sb: TSpeedButton;
begin
  { Test Buttons }

  BtnColor := claTeal;

  sb := AddSpeedBtn('TestBtn');
  TestBtn := sb;
  sb.Text := 'LC';
  sb.Hint := 'Toggle WantLineColors';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.OnClick := TestBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarExample.TestBtnClick(Sender: TObject);
begin

end;

end.
