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
  FMX.Controls.Presentation,
  RiggVar.FB.SpeedColor;

{$define FMX}

type
  TSpeedBtn = class(TSpeedButton)
  public
    ColorValue: TSpeedColorValue;
    IsFirstInGroup: Boolean;
    SpecialWidth: Integer;
  end;

  TActionSpeedBar = class(TLayout)
  private
    FDarkMode: Boolean;
    FBigMode: Boolean;
    function FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
    procedure InitLayoutProps;
    procedure SetDarkMode(const Value: Boolean);
    procedure SetBigMode(const Value: Boolean);
  protected
    BtnColorHot: TAlphaColor;
    BtnColorValue: TSpeedColorValue;
    BtnColor: TAlphaColor;
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanelFontSize: Integer;
    TempGroupSpace: Integer;
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedBtn;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;
    procedure UpdateLayoutForBtn(B: TSpeedButton; AGroupSpace: Integer);
    procedure InitSpeedButton(SB: TSpeedBtn);
    procedure UpdateSpeedButtonFontColor(SB: TSpeedButton);
    procedure UpdateSpeedButtonFontSize(SB: TSpeedButton);
    procedure SpeedButtonClick(Sender: TObject); virtual;
    procedure UpdateCaptions;
    procedure UpdateHints;
    procedure ToggleColorModeBtnClick(Sender: TObject);
    procedure ToggleFontSizeBtnClick(Sender: TObject);
  public
    class var SpeedColorScheme: TSpeedColorScheme; // injected

    constructor Create(AOwner: TComponent); override;

    procedure UpdateLayout;
    procedure UpdateColor;
    procedure ToggleBigMode;

    procedure InitSpeedButtons; virtual;

    property DarkMode: Boolean read FDarkMode write SetDarkMode;
    property BigMode: Boolean read FBigMode write SetBigMode;
    property PanelHeight: Integer read SpeedPanelHeight;
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
//  InitSpeedButtons; // Main is still nil (by design)
  DarkMode := True;
  BigMode := True;
end;

procedure TActionSpeedBar.InitLayoutProps;
begin
  BtnColorHot := claBeige;
  BtnColor := claBlue;
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 2;
  BtnGroupSpace := 16;
  BtnWidth := 50;
  BtnHeight := 50;
  SpeedPanelFontSize := 24;
  SpeedPanelHeight := BtnHeight + 2 * BtnTop;
end;

function TActionSpeedBar.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedBtn;
begin
  result := TSpeedBtn.Create(Self);
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
  { Does not work. }
  { Because B not assigned yet to actual SpeedButton instance ? }
//  InitSpeedButton(B);
  { also maybe not wanted to do this here }
{$endif}
{$ifdef Vcl}
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
{$endif}
  Inc(BtnCounter);
  TempGroupSpace := AGroupSpace;
end;

procedure TActionSpeedBar.UpdateLayoutForBtn(B: TSpeedButton; AGroupSpace: Integer);
begin
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
  Inc(BtnCounter);
end;

procedure TActionSpeedBar.UpdateLayout;
var
  cr: TFmxObject;
  sb: TSpeedBtn;
  gs: Integer;
begin
  InitLayoutProps;
  BigMode := BigMode;
  for cr in Children do
  begin
    if cr is TSpeedBtn then
    begin
      sb := cr as TSpeedBtn;
      if sb.IsFirstInGroup then
        gs := BtnGroupSpace
      else
        gs := 0;
      UpdateLayoutForBtn(sb, gs);
      if sb.SpecialWidth > 0 then
      begin
        BtnLeft := BtnLeft + Round(sb.SpecialWidth - sb.Width);
        sb.Width := sb.SpecialWidth;
      end;
      sb.Visible := sb.Position.X < self.Width - sb.Width;
    end;
  end;
end;

procedure TActionSpeedBar.UpdateCaptions;
var
  cr: TFmxObject;
  sb: TSpeedButton;
begin
  for cr in Children do
  begin
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      sb.Text := GetFederActionShort(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.UpdateHints;
var
  cr: TFmxObject;
  sb: TSpeedButton;
begin
  for cr in Children do
  begin
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      sb.Text := GetFederActionLong(sb.Tag);
    end;
  end;
end;

procedure TActionSpeedBar.UpdateColor;
var
  cr: TFmxObject;
  sb: TSpeedBtn;
begin
  for cr in Children do
  begin
    if cr is TSpeedBtn then
    begin
      sb := cr as TSpeedBtn;
      BtnColorValue := sb.ColorValue;
      UpdateSpeedButtonFontColor(sb);
    end;
  end;
end;

procedure TActionSpeedBar.ToggleBigMode;
var
  cr: TFmxObject;
  sb: TSpeedButton;
begin
  BigMode := not BigMode;

  UpdateLayout;

  for cr in Children do
  begin
    if cr is TSpeedButton then
    begin
      sb := cr as TSpeedButton;
      UpdateSpeedButtonFontSize(sb);
    end;
  end;
end;

procedure TActionSpeedBar.InitSpeedButton(SB: TSpeedBtn);
var
  cr: TButtonStyleTextObject;
  cla: TAlphaColor;
begin
  cla := SpeedColorScheme.GetColor(BtnColorValue);

  if SB.Tag <> faNoop then
  begin
    sb.Text := GetFederActionShort(SB.Tag);
    sb.Hint := GetFederActionLong(SB.Tag);
    sb.Action := Main.ActionList.GetFederAction(sb.Tag, True);
  end;

  { Text must be set before changing Font.Size }

  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  if cr <> nil then
  begin
    cr.HotColor := BtnColorHot;
    cr.NormalColor := cla;
    cr.PressedColor := cla;
    cr.Font.Size := SpeedPanelFontSize;
  end;

  SB.ColorValue := BtnColorValue;
  if TempGroupSpace = BtnGroupSpace then
    SB.IsFirstInGroup := True
  else
    SB.IsFirstInGroup := False;

  if sb.Width < sb.SpecialWidth then
  begin
    BtnLeft := BtnLeft + Round(sb.SpecialWidth - sb.Width);
    sb.Width := sb.SpecialWidth;
  end;
end;

procedure TActionSpeedBar.UpdateSpeedButtonFontColor(SB: TSpeedButton);
var
  cr: TButtonStyleTextObject;
  cla: TAlphaColor;
begin
  cla := SpeedColorScheme.GetColor(BtnColorValue);
  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  if cr <> nil then
  begin
//    cr.HotColor := BtnColorHot;
    cr.NormalColor := cla;
    cr.PressedColor := cla;
//    cr.Font.Size := SpeedPanelFontSize;
  end;
end;

procedure TActionSpeedBar.UpdateSpeedButtonFontSize(SB: TSpeedButton);
var
  cr: TButtonStyleTextObject;
begin
  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  if cr <> nil then
  begin
//    cr.HotColor := BtnColorHot;
//    cr.NormalColor := cla;
//    cr.PressedColor := cla;
    cr.Font.Size := SpeedPanelFontSize;
  end;
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

procedure TActionSpeedBar.SetBigMode(const Value: Boolean);
begin
  FBigMode := Value;
  if Value then
  begin
    BtnWidth := 50;
    BtnHeight := 50;
    SpeedPanelFontSize := 24;
    SpeedPanelHeight := BtnHeight + 2 * BtnTop;
  end
  else
  begin
    BtnWidth := 35;
    BtnHeight := 30;
    SpeedPanelFontSize := 16;
    SpeedPanelHeight := BtnHeight + 2 * BtnTop;
  end;
end;

procedure TActionSpeedBar.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  if Value then
    SpeedColorScheme.InitDark
  else
    SpeedColorScheme.InitLight;
end;

procedure TActionSpeedBar.InitSpeedButtons;
begin
  { virtual }
end;

procedure TActionSpeedBar.SpeedButtonClick(Sender: TObject);
begin
  { virtual }
end;

{ TActionSpeedBarExample }

procedure TActionSpeedBarExample.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Test Buttons }

  BtnColor := claTeal;
  BtnColorValue := clvProp;

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

procedure TActionSpeedBar.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBar.ToggleFontSizeBtnClick(Sender: TObject);
begin
  Main.ToggleSpeedPanelFontSize;
end;

end.
