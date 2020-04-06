unit FrmMain;

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
  RiggVar.RG.Def,
  RiggVar.RG.Report,
  RggTypes,
  RggUnit4,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Platform,
  FMX.Graphics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Listbox,
  FMX.Dialogs,
  FMX.Styles.Objects,
  FMX.Controls.Presentation;

{$define FMX}

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    procedure UpdateFormat(w, h: Integer);
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
  private
    FScale: single;
    FWantResizeNormalizing: Boolean;
    DefaultCaption: string;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure HandleShowHint(Sender: TObject);
    procedure ToggleFontColor;
    procedure Flash(s: string);
    procedure Reset;
  protected
    SL: TStrings;
    TL: TStrings;
    RL: TStrings;
    procedure InitHelpText;
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure ACI(fp: TFederParam);
    procedure ShowTrimm;
  public
    FWantButtonFrameReport: Boolean;
    procedure UpdateReport;
    procedure UpdateBackgroundColor(AColor: TAlphaColor);
    property WantButtonFrameReport: Boolean read FWantButtonFrameReport;
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    HintText: TText;
    SpeedPanel: TPanel;
    TrimmMemo: TMemo;
    ReportListbox: TListBox;
    ReportLabel: TText;
    ReportMemo: TMemo;

    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;

    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;

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

    procedure ReportListboxClick(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure M10BtnClick(Sender: TObject);
    procedure M1BtnClick(Sender: TObject);
    procedure P1BtnClick(Sender: TObject);
    procedure P10BtnClick(Sender: TObject);
    procedure SandboxedBtnClick(Sender: TObject);
    procedure AllTagsBtnClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure MT0BtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
  private
    Raster: Integer;
    Margin: Integer;
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    SpeedPanelHeight: Integer;
    BtnColor: TAlphaColor;
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedButton;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;

    procedure CreateComponents;
    procedure InitLayoutProps;
    procedure LayoutComponents;
    procedure InitSpeedButtonsRG38;
    procedure LinkComponentsRG38;
  protected
    function FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
    procedure InitSpeedButton(SB: TSpeedButton);
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
  public
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
    procedure HA(fa: Integer);
    procedure UpdateSpeedPanel;
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    function GetIsUp: Boolean;
    property IsUp: Boolean read GetIsUp;
  protected
    procedure DestroyForms;
    procedure MemoBtnClick(Sender: TObject);
    procedure ActiBtnClick(Sender: TObject);
    procedure CheckFormBounds(AForm: TForm);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FrmMemo,
  FrmAction,
  RiggVar.RG.Main,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

const
  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'RG38';

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  rggm: TRggMain;
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FScale := 1.0;
{$ifdef MSWINDOWS}
  { On MACOS Screen.WorkAreaHeight is not scaled,
    so it would be wrong to div by scale.

    On Windows Screen.WorkAreaHeight is scaled and should be divved. }
  FScale := Handle.Scale;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;
  Caption := 'RG38';
  Top := 20;
  Width := 1200;
  Height := 1000;
  Margin := 10;
  Raster := MainVar.Raster;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

  CreateComponents;
  InitLayoutProps;
  LayoutComponents;

  SetupMemo(TrimmMemo);
  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
  SetupMemo(ReportMemo);
  SetupText(HintText);
  SetupText(ReportLabel);
  SetupListbox(ReportListbox);

  Rigg := TRigg.Create;
  Rigg.ControllerTyp := ctOhne;

  rggm := TRggMain.Create(Rigg); // rggm owns Rigg
  Main := TMain.Create(rggm); // Main owns rggm
  Main.Logger.Verbose := True;
  rggm.InitLogo; // sets WantLogoData to true
  rggm.Init420;
  WantLogoData := False;

  Main.Trimm := 1;
  Main.Logger.Verbose := True;

  Main.InitText;
  Main.IsUp := True;

  InitSpeedButtonsRG38;
  LinkComponentsRG38;

  { Params }

  { Reports }
  RL := ReportMemo.Lines;
  SL := TStringList.Create;
  ReportManager := TRggReportManager.Create(SL);
  ReportManager.CurrentReport := rgDiffText;

  if ReportListbox <> nil then
  begin
    ReportManager.InitLB(ReportListbox.Items);
  end;

  TL := TrimmMemo.Lines;
  TrimmMemo.ShowScrollBars := True;
  TrimmMemo.Width := ReportListbox.Width;

  InitTrimmCombo;
  InitParamCombo;

  TrimmCombo.ItemIndex := 0;
  ParamCombo.ItemIndex := 0;
  ReportListbox.ItemIndex := 4;

  Main.Trimm := 1;
  MT0BtnClick(nil);
  Main.FederText.UpdateText;
  ShowTrimm;

  ReportLabel.TextSettings.FontColor := claOrange;

  Reset;

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claYellow;

  InitHelpText;

  if MainVar.ColorScheme.claBackground <> claSlateBlue then
  begin
    Fill.Kind := TBrushKind.Solid; // because is still TBrushKind.None
    Fill.Color := claSlateBlue;
    UpdateBackgroundColor(MainVar.ColorScheme.claBackground);
  end;

  Caption := HelpCaptionText;

  Main.FederText.CheckState;

{$ifdef MACOS}
  { OnKeyUp does not work well on Mac, RSP-2766 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead }
  OnKeyDown := FormKeyUp;
{$endif}

  Application.OnHint := HandleShowHint;
  SetupListboxItems(ReportListbox, claDodgerblue);

  UpdateSpeedPanel;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  DestroyForms;

  SL.Free;
  ReportManager.Free;
  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  fa: Integer;
begin
  fa := GetActionFromKey(Key);
  if fa = faNoop then
    fa := GetActionFromKeyChar(KeyChar);

  if fa <> faNoop then
    HA(fa);

  UpdateReport;

  Main.FederText.CheckState;
end;

procedure TFormMain.UpdateReport;
begin
  if not ReportMemo.Visible then
    Exit;
  if ReportManager = nil then
    Exit;

  SL.Clear;

  if WantButtonFrameReport then
  begin
    Main.FederText.Report(SL);
  end
  else
  begin
    ReportManager.ShowCurrentReport;
  end;

  ReportMemo.Text := SL.Text;
end;

procedure TFormMain.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Flash(Format('%d x %d', [ClientWidth, ClientHeight]));
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  MainVar.ClientWidth := ClientWidth;
  MainVar.ClientHeight := ClientHeight;
  if (Main <> nil) and Main.IsUp then
  begin
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    Main.UpdateText;
  end;
  UpdateReport;
end;

procedure TFormMain.ToggleFontColor;
begin
  if HintText.TextSettings.FontColor = claYellow then
  begin
    HintText.TextSettings.FontColor := claNavy;
//    ReportText.TextSettings.FontColor := claBlack;
//    HelpText.TextSettings.FontColor := claBlack;
  end
  else
  begin
    HintText.TextSettings.FontColor := claYellow;
//    ReportText.TextSettings.FontColor := claWhite;
//    HelpText.TextSettings.FontColor := claWhite;
  end;
end;

procedure TFormMain.UpdateBackgroundColor(AColor: TAlphaColor);
begin
  Self.Fill.Color := AColor;
end;

procedure TFormMain.Reset;
begin
  DefaultCaption := 'RG38';

  Flash(DefaultCaption);
end;

procedure TFormMain.GotoNormal;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

  Screen.UpdateDisplayInformation;

  { workaround, because of RSP-26601 }
  if FWantResizeNormalizing then
  begin
    Top := 100;
    Left := 100;
    ClientWidth := 800;
    ClientHeight := 600;
  end;
end;

procedure TFormMain.GotoLandscape;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 4 / 3);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth * 3 / 4);
    Left := 0;
  end;
  Flash('Landscape');
end;

procedure TFormMain.GotoPortrait;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 3 / 4);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth * 4 / 3);
    Left := 0;
    Top := 0;
  end;
  Flash('Portrait');
end;

procedure TFormMain.GotoSquare;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth);
    Left := 0
  end;
  Flash('Square');
end;

procedure TFormMain.Flash(s: string);
begin
  Caption := s;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  HintText.Text := Application.Hint;
end;

procedure TFormMain.HandleAction(fa: Integer);
begin
  HA(fa);
end;

procedure TFormMain.HA(fa: Integer);
begin
  case fa of
    faMemeToggleHelp:
    begin
//      HelpText.Visible := not HelpText.Visible;
//      ReportText.Visible := False;
    end;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faMemeToggleReport:
    begin
      Flash(HelpCaptionText);
//      HelpText.Visible := False;
//      ReportText.Visible := not ReportText.Visible;
      UpdateReport;
    end;

    faMemeFormat1: UpdateFormat(1000, 750);
    faMemeFormat2: UpdateFormat(800, 600);
    faMemeFormat3: UpdateFormat(640, 480);
    faMemeFormat4: UpdateFormat(480, 480);
    faMemeFormat5: UpdateFormat(512, 512);
    faMemeFormat6: UpdateFormat(600, 600);
    faMemeFormat7: UpdateFormat(700, 700);
    faMemeFormat8: UpdateFormat(800, 800);
    faMemeFormat9: UpdateFormat(900, 900);
    faMemeFormat0:
    begin
      Top := 0;
      UpdateFormat(750, 1000)
    end;

    faActionPageP: Main.ActionHandler.Execute(faActionPageP);
    faActionPageM: Main.ActionHandler.Execute(faActionPageM);
    faCycleColorSchemeP: Main.ActionHandler.Execute(faCycleColorSchemeP);
    faCycleColorSchemeM: Main.ActionHandler.Execute(faCycleColorSchemeM);

    faButtonFrameReport:
    begin
      FWantButtonFrameReport := not WantButtonFrameReport;
      UpdateReport;
    end;

    faReportNone..faReportReadme:
    begin
      ReportManager.HA(fa);
      UpdateReport;
    end;

    faToggleAllTags:
    begin
      ReportManager.XmlAllTags := not ReportManager.XmlAllTags;
      UpdateSpeedPanel;
    end;

    faToggleFontColor: ToggleFontColor;

    faShowActi: ActiBtnClick(nil);
    faShowMemo: MemoBtnClick(nil);

    else
    begin
      { do nothing }
    end;

  end;
end;

function TFormMain.GetActionFromKey(Key: Word): Integer;
begin
  result := faNoop;
//  case Key of
//    vkF12: result := faMemeSaveBitmap;
//    vkC: result := faMemeCopyBitmap;
//    vkV: result := faMemePasteBitmap;
//  end
end;

function TFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  fa := faNoop;
  case KeyChar of
    'a': ;
    'A': ;

    'b': ;
    'B': ;

    'h': fa := faMemeToggleHelp;

    'i': ;
    'I': ;

    'j': ;
    'J': ;

    'k': ;
    'K': ;

    'l': fa := faMemeGotoLandscape;

    'r': fa := faMemeToggleReport;
    'R': ;

    'p': fa := faMemeGotoPortrait;
    's': fa := faMemeGotoSquare;

    't': fa := faToggleFontColor;

    'v': ;

    '0': fa := faMemeFormat0;
    '1': fa := faMemeFormat1;
    '2': fa := faMemeFormat2;
    '3': fa := faMemeFormat3;
    '4': fa := faMemeFormat4;
    '5': fa := faMemeFormat5;
    '6': fa := faMemeFormat6;
    '7': fa := faMemeFormat7;
    '8': fa := faMemeFormat8;
    '9': fa := faMemeFormat9;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    'c': fa := faCycleColorSchemeP;
    'C': fa := faCycleColorSchemeM;

    else fa := faNoop;

  end;
  result := fa;
end;

function TFormMain.GetIsUp: Boolean;
begin
  result := (Main <> nil) and Main.IsUp;
end;

procedure TFormMain.InitHelpText;
var
  HL: TStringList;
begin
  HL := TStringList.Create;

  HL.Add('Toggle Text with Keys:');
  HL.Add('  h    - toggle help');
  HL.Add('  r    - toggle Report');
  HL.Add('');
  HL.Add('Select current param with Button:');
  HL.Add('');
  HL.Add('Change param value with Wheel!');
  HL.Add('  Shift-Wheel = small step');
  HL.Add('  Ctrl-Wheel  = bigger step');
  HL.Add('');
  HL.Add('Another Test: change Format of Window.');
  HL.Add('  1..9, 0 - Format selection');
  HL.Add('  1, p, s - Landscape, Portrait, Square');
  HL.Add('');
  HL.Add('Window Status:');
  HL.Add(Format('  Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  HL.Add(Format('  Handle.Scale = %.1f', [Handle.Scale]));

//  HelpText.Text := HL.Text;

  HL.Free;
end;

function TFormMain.GetOpenFileName(dn, fn: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(self);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TFormMain.GetSaveFileName(dn, fn: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(self);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;

procedure TFormMain.SetupText(T: TText);
begin
  T.WordWrap := False;
  T.AutoSize := True;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 18;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
{$ifdef Vcl}
  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
{$endif}
end;

procedure TFormMain.SetupListBox(LB: TListBox);
begin
{$ifdef Vcl}
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
{$endif}
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
{$ifdef FMX}
  MM.ControlType := TControlType.Styled;
  MM.StyledSettings := [];
  MM.ShowScrollBars := True;
  MM.TextSettings.Font.Family := 'Consolas';
  MM.TextSettings.Font.Size := 14;
  MM.TextSettings.FontColor := claBlue;
{$endif}

{$ifdef Vcl}
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := ssBoth;
{$endif}
end;

function TFormMain.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(SpeedPanel);
  result.Parent := SpeedPanel;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TFormMain.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
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

procedure TFormMain.InitSpeedButtonsRG38;
var
  sb: TSpeedButton;
begin
  { Data Buttons }

  BtnColor := claTeal;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Text := 'MT0';
  sb.Hint := 'Memory Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := MT0BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn', BtnGroupSpace);
  ReadTrimmFileBtn := sb;
  sb.Text := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := ReadTrimmFileBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Text := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := SaveTrimmFileBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Text := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := CopyTrimmItemBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Text := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := PasteTrimmItemBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Text := 'M';
  sb.Hint := 'Copy And Paste';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := CopyAndPasteBtnClick;
  InitSpeedButton(sb);

  { Param Value Buttons }

  BtnColor := claBlue;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Text := '-10';
  sb.Hint := 'Param Value Minus 10';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := M10BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('M1Btn');
  M1Btn := sb;
  sb.Text := '-1';
  sb.Hint := 'Param Value Minus 1';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := M1BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P1Btn');
  P1Btn := sb;
  sb.Text := '+1';
  sb.Hint := 'Param Value Plus 1';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := P1BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P10Btn');
  P10Btn := sb;
  sb.Text := '+10';
  sb.Hint := 'Param Value Plus 10';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := P10BtnClick;
  InitSpeedButton(sb);

  { Check Box Buttons }

  BtnColor := claPurple;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Text := 'SB';
  sb.Hint := 'Sandboxed';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.OnClick := SandboxedBtnClick;
  InitSpeedButton(sb);

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
  sb.OnClick := AllTagsBtnClick;
  InitSpeedButton(sb);
end;

procedure TFormMain.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  ShowTrimm;
end;

procedure TFormMain.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  ShowTrimm;
end;

procedure TFormMain.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  ShowTrimm;
end;

procedure TFormMain.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  ShowTrimm;
end;

procedure TFormMain.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  ShowTrimm;
end;

procedure TFormMain.SandboxedBtnClick(Sender: TObject);
begin
  IsSandboxed := SandboxedBtn.IsPressed;
end;

procedure TFormMain.AllTagsBtnClick(Sender: TObject);
begin
  ReportManager.XmlAllTags := AllTagsBtn.IsPressed;
  UpdateReport;
end;

procedure TFormMain.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TFormMain.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TFormMain.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TFormMain.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TFormMain.ReportListboxClick(Sender: TObject);
var
  ii: Integer;
begin
  RL.Clear;
  ii := ReportListbox.ItemIndex;
  if (ii >= 0) and (ii <= Integer(High(TRggReport)))then
  begin
    ReportManager.CurrentIndex := ii;
    UpdateReport;
  end;
end;

procedure TFormMain.ACI(fp: TFederParam);
var
  s: string;
begin
  s := Main.RggMain.Param2Text(fp);
  ParamCombo.Items.AddObject(s, TObject(fp));
end;

procedure TFormMain.InitParamCombo;
begin
  ACI(fpVorstag);
  ACI(fpWinkel);
  ACI(fpController);
  ACI(fpWante);
  ACI(fpWoben);
  ACI(fpSalingH);
  ACI(fpSalingA);
  ACI(fpSalingL);
  ACI(fpSalingW);
  ACI(fpMastfallF0C);
  ACI(fpMastfallF0F);
  ACI(fpMastfallVorlauf);
  ACI(fpBiegung);
  ACI(fpD0X);
end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  if ParamCombo <> nil then
  begin
    ii := ParamCombo.ItemIndex;
    fp := TFederParam(ParamCombo.Items.Objects[ii]);
    Main.RggMain.Param := fp;
    ShowTrimm;
  end;
end;

procedure TFormMain.InitTrimmCombo;
var
  cl: TStrings;
begin
  if TrimmCombo <> nil then
  begin
    cl := TrimmCombo.Items;
    cl.AddObject('Trimm1', TObject(1));
    cl.AddObject('Trimm2', TObject(2));
    cl.AddObject('Trimm3', TObject(3));
    cl.AddObject('Trimm4', TObject(4));
    cl.AddObject('Trimm5', TObject(5));
    cl.AddObject('Trimm6', TObject(6));
    cl.AddObject('Trimm7 (420)', TObject(7));
    cl.AddObject('Trimm8 (Logo)', TObject(8));
  end;
end;

procedure TFormMain.TrimmComboChange(Sender: TObject);
var
  t: Integer;
  ii: Integer;
begin
  if TrimmCombo <> nil then
  begin
    ii := TrimmCombo.ItemIndex;
    t := Integer(TrimmCombo.Items.Objects[ii]);
    Main.Trimm := t;

    RL.BeginUpdate;
    try
      RL.Clear;

      //Main.CurrentTrimm.SaveTrimmFile(ML);

      Main.CurrentTrimm.WantAll := AllPropsBtn.IsPressed;
      Main.CurrentTrimm.SaveTrimmItem(RL);
      Main.CurrentTrimm.WantAll := False;

      //Main.CurrentTrimm.WriteReport(ML);

      if ReportLabel <> nil then
      begin
        ReportLabel.Text := 'Trimm' + IntToStr(t);
      end;
    finally
      RL.EndUpdate;
    end;
  end;
end;

procedure TFormMain.ShowTrimm;
begin
  if TL <> nil then
    Main.RggMain.UpdateTrimmText(TL);
  UpdateReport;
end;

procedure TFormMain.InitLayoutProps;
begin
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 2;
  BtnGroupSpace := 16;
  BtnWidth := 35;
  BtnHeight := 30;
  BtnColor := claBlue;
  SpeedPanelHeight := BtnHeight + 2 * BtnTop;
end;

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  HintText.Parent := Self;
//  HintText.WordWrap := False;
//  HintText.AutoSize := True;
//  HintText.HorzTextAlign := TTextAlign.Leading;
//  HintText.Font.Family := 'Consolas';
//  HintText.Font.Size := 18;

  ReportLabel := TText.Create(Self);
  ReportLabel.Parent := Self;

//  HelpText := TText.Create(Self);
//  HelpText.Parent := Self;
//  HelpText.WordWrap := False;
//  HelpText.HorzTextAlign := TTextAlign.Leading;
//  HelpText.Font.Family := 'Courier New';
//  HelpText.Font.Size := 16;
//  HelpText.AutoSize := True;
//
//  ReportText := TText.Create(Self);
//  ReportText.Parent := Self;
//  ReportText.WordWrap := False;
//  ReportText.HorzTextAlign := TTextAlign.Leading;
//  ReportText.Font.Family := 'Courier New';
//  ReportText.Font.Size := 16;
//  ReportText.AutoSize := True;

  SpeedPanel := TPanel.Create(Self);
  SpeedPanel.Parent := Self;
  SpeedPanel.ShowHint := True;
  SpeedPanel.Opacity := OpacityValue;

  TrimmMemo := TMemo.Create(Self);
  TrimmMemo.Parent := Self;
  TrimmMemo.ReadOnly := True;
  TrimmMemo.CanFocus := False;
  TrimmMemo.Opacity := OpacityValue;

  TrimmCombo := TComboBox.Create(Self);
  TrimmCombo.Parent := Self;

  ParamCombo := TComboBox.Create(Self);
  ParamCombo.Parent := Self;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;

  ReportMemo := TMemo.Create(Self);
  ReportMemo.Parent := Self;
  ReportMemo.ReadOnly := True;
  ReportMemo.CanFocus := False;
  ReportMemo.Opacity := OpacityValue;

//  Bitmap := TBitmap.Create(1024, 768);

//  Image := TImage.Create(Self);
//  Image.Parent := Self;
//  Image.Bitmap := Bitmap;
//  Image.WrapMode := TImageWrapMode.Original;

//  MT0Btn := TSpeedButton.Create(Self);
//  ReadTrimmFileBtn := TSpeedButton.Create(Self);
//  SaveTrimmFileBtn := TSpeedButton.Create(Self);
//  PasteTrimmItemBtn := TSpeedButton.Create(Self);
//  CopyTrimmItemBtn := TSpeedButton.Create(Self);
//  CopyAndPasteBtn := TSpeedButton.Create(Self);
//
//  M10Btn := TSpeedButton.Create(Self);
//  M1Btn := TSpeedButton.Create(Self);
//  P1Btn := TSpeedButton.Create(Self);
//  P10Btn := TSpeedButton.Create(Self);
//
//  SandboxedBtn := TSpeedButton.Create(Self);
//  WantAllBtn := TSpeedButton.Create(Self);
end;

procedure TFormMain.LayoutComponents;
begin
  SpeedPanel.Position.X := 2 * Raster + Margin;
  SpeedPanel.Position.Y := Raster + Margin;
  SpeedPanel.Width := ClientWidth - SpeedPanel.Position.X - Raster - Margin;
  SpeedPanel.Height := SpeedPanelHeight;
  SpeedPanel.Anchors := SpeedPanel.Anchors + [TAnchorKind.akRight];

  HintText.Position.X := 2 * Raster + 30;
  HintText.Position.Y := Raster + SpeedPanelHeight + Margin + 4;
  HintText.Height := 30;

  ReportLabel.Position.X := 600;
  ReportLabel.Position.Y := Raster + SpeedPanelHeight + Margin + 4;
  ReportLabel.Height := 30;

  TrimmMemo.Position.X := Raster + Margin;
  TrimmMemo.Position.Y := Raster + SpeedPanel.Height + HintText.Height + Margin;
  TrimmMemo.Height := 185;
  TrimmMemo.Width := 200;

  TrimmCombo.Position.X := TrimmMemo.Position.X;
  ParamCombo.Position.X := TrimmCombo.Position.X;

  TrimmCombo.Width := TrimmMemo.Width;
  ParamCombo.Width := TrimmCombo.Width;

  TrimmCombo.Position.Y := TrimmMemo.Position.Y + TrimmMemo.Height + Margin;
  ParamCombo.Position.Y := TrimmCombo.Position.Y + TrimmCombo.Height + Margin;

  ReportListbox.Position.X := TrimmMemo.Position.X;
  ReportListbox.Position.Y := ParamCombo.Position.Y + ParamCombo.Height + Margin;
  ReportListbox.Width := TrimmMemo.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  ReportMemo.Position.X := ReportListbox.Position.X + ReportListbox.Width + Margin;
  ReportMemo.Position.Y := TrimmMemo.Position.Y;
  ReportMemo.Width := ClientWidth - ReportMemo.Position.X - Raster - Margin;
  ReportMemo.Height := ClientHeight - ReportMemo.Position.Y - Raster - Margin;
  ReportMemo.Anchors := ReportMemo.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];

//  Image.Position.X := ReportMemo.Position.X + ReportMemo.Width + Margin;
//  Image.Position.Y := SpeedPanel.Position.Y + SpeedPanel.Height + Margin;
//  Image.Width := ClientWidth - Image.Position.X - Margin;
//  Image.Height := ClientHeight - Image.Position.Y - Margin;
//  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
end;

procedure TFormMain.LinkComponentsRG38;
begin
//  MT0Btn.OnClick := MT0BtnClick;
//  CopyAndPasteBtn.OnClick := CopyAndPasteBtnClick;
//  PasteTrimmItemBtn.OnClick := PasteTrimmItemBtnClick;
//  CopyTrimmItemBtn.OnClick := CopyTrimmItemBtnClick;
//  ReadTrimmFileBtn.OnClick := ReadTrimmFileBtnClick;
//  SaveTrimmFileBtn.OnClick := SaveTrimmFileBtnClick;
//
//  M1Btn.OnClick := M1BtnClick;
//  M10Btn.OnClick := M10BtnClick;
//  P1Btn.OnClick := P1BtnClick;
//  P10Btn.OnClick := P10BtnClick;
//  SandboxedBtn.OnClick := SandboxedBtnClick;

  ReportListbox.OnClick := ReportListboxClick;
  ReportListbox.OnChange := ReportListboxClick;

  TrimmCombo.OnChange := TrimmComboChange;
  ParamCombo.OnChange := ParamComboChange;

  ReportMemo.OnMouseWheel := FormMouseWheel;
end;

procedure TFormMain.SetupListboxItems(LB: TListbox; cla: TAlphaColor);
var
  i: Integer;
  cr: TListBoxItem;
  T: TText;
begin
  if LB.Items.Count > 0 then
  for i := 0 to LB.Items.Count - 1 do
  begin
    cr := LB.ItemByIndex(i);
    T := cr.FindStyleResource('text') as TText;
    if Assigned(T) then
    begin
      T.Font.Family := 'Consolas';
      T.Font.Size := 14;
      T.TextSettings.FontColor := cla;
    end;
  end;
end;

procedure TFormMain.InitSpeedButton(SB: TSpeedButton);
var
  cr: TButtonStyleTextObject;
begin
  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  cr.NormalColor := BtnColor;
  cr.PressedColor := BtnColor;
  cr.Font.Family := 'Consolas';
  cr.Font.Size := 16;
end;

function TFormMain.FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
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

procedure TFormMain.UpdateSpeedPanel;
begin
  SandboxedBtn.IsPressed := IsSandboxed;
  AllTagsBtn.IsPressed := ReportManager.XMLAllTags;
end;

function TFormMain.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := IsSandboxed;
    faToggleAllTags: result := ReportManager.XmlAllTags;

//    faMemeToggleHelp: result := HelpText.Visible;
//    faMemeToggleReport: result := ReportText.Visible;
    faButtonFrameReport: result := WantButtonFrameReport;
//    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
//    faToggleSegmentF..faToggleSegmentA: result := RotaForm.RaumGraph.GetChecked(fa);

//    faToggleLineColor: result := DL.WantLineColors;
//    faToggleShowLegend: result := RotaForm.LegendItemChecked;

//    faViewpointS: result := RotaForm.ViewPoint = vpSeite;
//    faViewpointA: result := RotaForm.ViewPoint = vpAchtern;
//    faViewpointT: result := RotaForm.ViewPoint = vpTop;
//    faViewpoint3: result := RotaForm.ViewPoint = vp3D;

//    ZoomInBtn: result := False;
//    ZoomOutBtn: result := False;

//    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
//    faToggleUseQuickSort: result := RotaForm.RaumGraph.DL.UseQuickSort;
//    faRggBogen: result := RotaForm.Bogen;

    faSofortBtn: result := Main.RggMain.SofortBerechnen;
    faGrauBtn: result := Main.RggMain.BtnGrauDown;
    faBlauBtn: result := Main.RggMain.BtnBlauDown;
    faMemoryBtn: result := False;
//    faMultiBtn: result := RotaForm.WantOverlayedRiggs;
//    faKoppelBtn: result := RotaForm.RaumGraph.Koppel;

//    faToggleChartGraph: result := ChartImage.IsVisible;
//    faToggleSalingGraph: result := SalingImage.IsVisible;
//    faToggleControllerGraph: result := ControllerImage.IsVisible;
//    faToggleMatrixText: result := RotaForm.MatrixItemChecked;
  end;

end;

procedure TFormMain.CheckFormBounds(AForm: TForm);
begin
  if Screen.Height <= 768 then
    AForm.Top := 0;
  if Screen.Width <= 768 then
    AForm.Left := 0;
  if AForm.Left + AForm.Width > Screen.Width then
    AForm.Width := Screen.Width - AForm.Left - 20;
  if AForm.Top + AForm.Height > Screen.Height then
    AForm.Height := Screen.Width - AForm.Top - 20;
end;

procedure TFormMain.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Parent := self; //needed for Alt-Tab
    FormMemo.Memo.Lines.Clear;
    //Main.WriteVersion1Diff(FormMemo.Memo.Lines);
    CheckFormBounds(FormMemo);
  end;
  FormMemo.Visible := True;
  FormMemo.Show; //needed on Mac
end;

procedure TFormMain.ActiBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
    FormAction.Parent := self;
    CheckFormBounds(FormAction);
  end;
  FormAction.Visible := True;
  FormAction.Show;
end;

procedure TFormMain.DestroyForms;
begin
  if FormAction <> nil then
  begin
    FormAction.DisposeOf;
    FormAction := nil;
  end;
  if FormMemo <> nil then
  begin
    FormMemo.DisposeOf;
    FormMemo := nil;
  end;
end;

end.
