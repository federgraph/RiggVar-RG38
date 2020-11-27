unit RiggVar.RG.Speed01;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  System.UIConsts,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls;

type
  TActionSpeedBarRG01 = class(TActionSpeedBar)
  private
    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmFileBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    M1Btn: TSpeedButton;
    M10Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;

    SandboxedBtn: TSpeedButton;
    AllPropsBtn: TSpeedButton;
    AllTagsBtn: TSpeedButton;

    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    procedure ToggleColorModeBtnClick(Sender: TObject);
    procedure ToggleFontSizeBtnClick(Sender: TObject);
  protected
    procedure SpeedButtonClick(Sender: TObject); override;
  public
    procedure InitSpeedButtons; override;
    procedure UpdateSpeedButtonDown; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG01 }

procedure TActionSpeedBarRG01.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Special Buttons }

  BtnColorValue := clvData;

  sb := AddSpeedBtn('FontSizeBtn', BtnGroupSpace);
  FontSizeBtn := sb;
  sb.Text := 'FS';
  sb.Hint := 'Toggle FontSize';
  sb.OnClick := ToggleFontSizeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ColorModeBtn');
  ColorModeBtn := sb;
  sb.Text := 'CM';
  sb.Hint := 'Toggle ColorMode';
  sb.OnClick := ToggleColorModeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  { Check Box Buttons }

  BtnColorValue := clvOption;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Text := 'SX';
  if MainConst.MustBeSandboxed then
  begin
    sb.Hint := 'Sandboxed (Store)';
    sb.StaysPressed := True;
    sb.IsPressed := True;
    sb.Tag := faNoop;
    InitSpeedButton(sb);
    sb.Enabled := False;
  end
  else
  begin
    sb.Hint := 'Sandboxed';
    sb.StaysPressed := True;
    sb.IsPressed := False;
    sb.Tag := faToggleSandboxed;
    InitSpeedButton(sb);
  end;

  BtnColorValue := clvProp;

  sb := AddSpeedBtn('AllPropsBtn');
  AllPropsBtn := sb;
  sb.Text := 'AP';
  sb.Hint := 'All Trimm Props';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('AllTagsBtn');
  AllTagsBtn := sb;
  sb.Text := 'AT';
  sb.Hint := 'All Xml Tags';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.Tag := faToggleAllTags;
  InitSpeedButton(sb);

  { Data Buttons }

  BtnColorValue := clvData;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Text := 'MT0';
  sb.Hint := 'Update Trimm 0';
  sb.Tag := faUpdateTrimm0;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn');
  ReadTrimmFileBtn := sb;
  sb.Text := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.Tag := faReadTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Text := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.Tag := faSaveTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmFileBtn');
  CopyTrimmFileBtn := sb;
  sb.Text := 'ctf';
  sb.Hint := 'Copy Trimm File';
  sb.Tag := faCopyTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Text := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.Tag := faCopyTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Text := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.Tag := faPasteTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Text := 'M';
  sb.Hint := 'Copy And Paste';
  sb.Tag := faCopyAndPaste;
  InitSpeedButton(sb);

  { Param Value Buttons }

  BtnColorValue := clvWheel;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Text := '-10';
  sb.Hint := 'Param Value Minus 10';
  sb.Tag := faParamValueMinus10;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('M1Btn');
  M1Btn := sb;
  sb.Text := '-1';
  sb.Hint := 'Param Value Minus 1';
  sb.Tag := faParamValueMinus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P1Btn');
  P1Btn := sb;
  sb.Text := '+1';
  sb.Hint := 'Param Value Plus 1';
  sb.Tag := faParamValuePlus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P10Btn');
  P10Btn := sb;
  sb.Text := '+10';
  sb.Hint := 'Param Value Plus 10';
  sb.Tag := faParamValuePlus10;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarRG01.UpdateSpeedButtonDown;
begin
  SandboxedBtn.IsPressed := MainConst.MustBeSandboxed or MainVar.IsSandboxed;
  AllPropsBtn.IsPressed := MainVar.AllProps;
  AllTagsBtn.IsPressed := MainVar.AllTags;
end;

procedure TActionSpeedBarRG01.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;
  Main.ActionHandler.Execute(fa);
end;

procedure TActionSpeedBarRG01.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBarRG01.ToggleFontSizeBtnClick(Sender: TObject);
begin
  Main.ToggleSpeedPanelFontSize;
end;

end.
