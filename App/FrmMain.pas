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
  RiggVar.FB.SpeedBar,
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
  FMX.Dialogs;

{$define FMX}

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    FormShown: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure HandleShowHint(Sender: TObject);
    procedure ToggleFontColor;
    procedure Flash(s: string);
    procedure Reset;
  protected
    RL: TStrings;
    TL: TStrings;
    procedure InitHelpText;
    procedure InitParamListbox;
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure InitReportCombo;
  public
    AllProps: Boolean;
    procedure ShowTrimm;
    procedure ShowTrimmData;
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
    HelpText: TText;
    ReportText: TText;
    TrimmMemo: TMemo;
    ParamListbox: TListBox;
    ReportListbox: TListBox;
    ReportLabel: TText;
    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;
    ReportCombo: TComboBox;
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexParamsLB;
    procedure UpdateItemIndexParamsCB;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;
    procedure ParamListboxChange(Sender: TObject);
    procedure ReportListboxChange(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure ReportComboChange(Sender: TObject);
  public
    procedure ShowReport(const Value: TRggReport);
    function GetShowDataText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowTrimmText: Boolean;
    procedure SetShowDataText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowTrimmText(const Value: Boolean);
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  protected
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure CheckSpaceForImages;
    procedure CheckSpaceForMemo;
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
  private
    Raster: Integer;
    Margin: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
  public
    procedure SpeedButtonClick(Sender: TObject);
    procedure SofortBtnClick(Sender: TObject);
    procedure GrauBtnClick(Sender: TObject);
    procedure BlauBtnClick(Sender: TObject);
    procedure MemoryBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);
  public
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
    procedure HA(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    function GetIsUp: Boolean;
    property IsUp: Boolean read GetIsUp;
  protected
    Bitmap: TBitmap;
    Image: TImage;
    ImagePositionX: single;
    ImagePositionY: single;
    TextPositionX: single;
    TextPositionY: single;
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
  RiggVar.RG.Speed01,
//  RiggVar.RG.Speed02,
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
  Caption := ApplicationTitleText;
  Top := 20;
  Width := 1600;
  Height := 1000;
  Margin := 10;
  Raster := MainVar.Raster;
  SpeedPanelHeight := Raster;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

  CreateComponents;

  SetupListbox(ParamListbox);
  SetupMemo(TrimmMemo);
  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
  SetupComboBox(ReportCombo);
  SetupText(HintText);
  SetupListbox(ReportListbox);

  Rigg := TRigg.Create;
  Rigg.ControllerTyp := ctOhne;

  rggm := TRggMain.Create(Rigg); // rggm owns Rigg
  Main := TMain.Create(rggm); // Main owns rggm
  Main.Logger.Verbose := True;
  rggm.InitLogo; // sets WantLogoData to true
  rggm.Init420; // resets WantLogoData to false
  WantLogoData := False;

  Main.Trimm := 1;
  Main.Logger.Verbose := True;

  Main.InitText;
  Main.IsUp := True;

  { Params }
  Main.RggMain.Param := fpVorstag;
  if ParamListbox <> nil then
  begin
    InitParamListbox;
    ParamListbox.OnChange := ParamListboxChange;
    ParamListbox.ItemIndex := ParamListbox.Items.IndexOf('Vorstag');
  end;
  if ParamCombo <> nil then
  begin
    InitParamCombo;
    ParamCombo.ItemIndex := ParamCombo.Items.IndexOf('Vorstag');
    ParamCombo.OnChange := ParamComboChange;
  end;

  { Reports }
  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  ReportManager.CurrentReport := rgDiffText;
  if ReportListbox <> nil then
  begin
    ReportManager.InitLB(ReportListbox.Items);
    ReportListbox.OnChange := ReportListboxChange;
    ReportListbox.ItemIndex := ReportListbox.Items.IndexOf(
      ReportManager.GetReportCaption(TRggReport.rgDiffText));
  end;
  if ReportCombo <> nil then
  begin
    InitReportCombo;
    ReportCombo.OnChange := ReportComboChange;
    ReportManager.InitLB(ReportListbox.Items);
  end;

  if TrimmCombo <> nil then
  begin
    InitTrimmCombo;
    TrimmCombo.ItemIndex := 0;
    TrimmCombo.OnChange := TrimmComboChange;
  end;

  TL := TrimmMemo.Lines;
  Main.UpdateTrimm0;
  ShowTrimm;

  Reset;

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claYellow;

  HelpText.BringToFront;
  HelpText.TextSettings.FontColor := claWhite;
  HelpText.Visible := False;

  ReportText.BringToFront;
  ReportText.TextSettings.FontColor := claWhite;
  ReportText.Visible := True;

  InitHelpText;

  if MainVar.ColorScheme.claBackground <> claSlateBlue then
  begin
    Fill.Kind := TBrushKind.Solid; // because is still TBrushKind.None
    Fill.Color := claSlateBlue;
    UpdateBackgroundColor(MainVar.ColorScheme.claBackground);
  end;

  Caption := HelpCaptionText;

{$ifdef MACOS}
  { OnKeyUp does not work well on Mac, RSP-2766 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead }
  OnKeyDown := FormKeyUp;
{$endif}

  Application.OnHint := HandleShowHint;
  SetupListboxItems(ParamListbox, claDodgerblue);
  SetupListboxItems(ReportListbox, claDodgerblue);

  Main.RggMain.Draw;

  Main.FederText.CheckState;

  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  DestroyForms;

  RL.Free;
  ReportManager.Free;
  Main.Free;
  Main := nil;

  Image.Free;
  Bitmap.Free;
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
  begin
    HA(fa);
    Main.FederText.CheckState;
  end;

  UpdateReport;
end;

procedure TFormMain.UpdateReport;
begin
  if not ReportText.Visible then
    Exit;
  if ReportManager = nil then
    Exit;
  if RL = nil then
    Exit;
  if not IsUp then
    Exit;

  RL.Clear;

  if WantButtonFrameReport then
  begin
    Main.FederText.Report(RL);
  end
  else
  begin
    ReportManager.ShowCurrentReport;
  end;

  ReportText.Text := RL.Text;
end;

procedure TFormMain.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Flash(Format('%d x %d', [ClientWidth, ClientHeight]));
end;

procedure TFormMain.UpdateItemIndexParams;
begin
  UpdateItemIndexParamsLB;
  UpdateItemIndexParamsCB;
end;

procedure TFormMain.UpdateItemIndexParamsLB;
var
  ii: Integer;
  ik: Integer;
begin
  if ParamListbox = nil then
    Exit;
  ii := ParamListbox.ItemIndex;
  ik := FindItemIndexOfParam(ParamListbox.Items);
  if ii <> ik then
  begin
    ParamListbox.OnChange := nil;
    ParamListbox.ItemIndex := ik;
    ParamListbox.OnChange := ParamListboxChange;
  end;
end;

procedure TFormMain.UpdateItemIndexParamsCB;
var
  ii: Integer;
  ik: Integer;
begin
  if ParamCombo = nil then
    Exit;
  ii := ParamCombo.ItemIndex;
  ik := FindItemIndexOfParam(ParamCombo.Items);
  if ii <> ik then
  begin
    ParamCombo.OnChange := nil;
    ParamCombo.ItemIndex := ik;
    ParamCombo.OnChange := ParamComboChange;
  end;
end;

function TFormMain.FindItemIndexOfParam(ML: TStrings): Integer;
var
  fp: TFederParam;
  i: Integer;
begin
  fp := Main.RggMain.Param;
  result := -1;
  for i := 0 to ML.Count-1 do
  begin
    if TFederParam(ML.Objects[i]) = fp then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TFormMain.UpdateItemIndexReports;
var
  ii: Integer;
  ij: Integer;
begin
  if ReportListbox = nil then
    Exit;
  ii := ReportListbox.ItemIndex;
  ij := ReportManager.GetItemIndexOfReport(ReportManager.CurrentReport);
  if ii <> ij then
  begin
    ReportListbox.OnChange := nil;
    ReportListbox.ItemIndex := ij;
    ReportListbox.OnChange := ReportListboxChange;
  end;
end;

procedure TFormMain.UpdateItemIndexTrimms;
var
  ii: Integer;
  ij: Integer;
begin
  if TrimmCombo = nil then
    Exit;
  ii := TrimmCombo.ItemIndex;
  ij := Main.Trimm-1;
  if ii <> ij then
  begin
    TrimmCombo.OnChange := nil;
    TrimmCombo.ItemIndex := ij;
    TrimmCombo.OnChange := TrimmComboChange;
  end;
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

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    { Prove that ClientHeigt is available when FormShow is called: }
//    Caption := IntToStr(ClientHeight);
    LayoutComponents;
//    LayoutImages;
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
  CheckSpaceForImages;
  CheckSpaceForMemo;
end;

procedure TFormMain.CheckSpaceForMemo;
begin
  if not FormShown then
    Exit;

  if (ClientWidth < 1100) or (ClientHeight < 800) then
  begin
    SpeedPanel.Visible := False;
    TrimmMemo.Visible := False;
    TrimmCombo.Visible := False;
    ParamListbox.Visible := False;
    ReportListbox.Visible := False;
    Image.Position.X := 0;
    Image.Position.Y := 0;
    Image.Width := Bitmap.Width;
    Image.Height := Bitmap.Height;
    Image.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
    HelpText.Position.X := Raster + 30;
    ReportText.Position.X := Raster + 30;
  end
  else
  begin
    SpeedPanel.Visible := True;
    TrimmMemo.Visible := True;
    TrimmCombo.Visible := True;
    ParamListbox.Visible := True;
    ReportListbox.Visible := True;
    Image.Position.X := ImagePositionX;
    Image.Position.Y := ImagePositionY;
    Image.Width := ClientWidth - Image.Position.X - Raster - Margin;
    Image.Height := ClientHeight - Image.Position.Y - Raster - Margin;
    Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
    HelpText.Position.X := TextPositionX;
    ReportText.Position.X := TextPositionX;
  end;
end;

procedure TFormMain.CheckSpaceForImages;
begin
  { At aplication start up FormResize is called serveral times,
    but always before FormShow always called. }

  { ClientWidth and ClientHeight are not yet available when starting up.
    ClientHeigt is available when FormShow is called.
   }
//  if FormShown then
//  begin
//    { when FormResize is called after FormShow }
//    if (ClientWidth < 1200) or (ClientHeight < 600) then
//      ChartImage.Visible := False;
//    if (ClientWidth < 1500) or (ClientHeight < 655) then
//      ControllerImage.Visible := False;
//    if (ClientWidth < 1500) or (ClientHeight < 875) then
//      SalingImage.Visible := False;
//  end
//  else
//  begin
//    { when FormResize is called before FormShow }
//    if (Width < 1200) or (Height < 600) then
//      ChartImage.Visible := False;
//    if (Width < 1500) or (Height < 655) then
//      ControllerImage.Visible := False;
//    if (Width < 1500) or (Height < 875) then
//      SalingImage.Visible := False;
//  end;
//
//  Main.FederText.CheckState;
//  UpdateSpeedButtonDown;
end;

procedure TFormMain.ToggleFontColor;
begin
  if HintText.TextSettings.FontColor = claYellow then
  begin
    HintText.TextSettings.FontColor := claNavy;
    ReportText.TextSettings.FontColor := claBlack;
    HelpText.TextSettings.FontColor := claBlack;
  end
  else
  begin
    HintText.TextSettings.FontColor := claYellow;
    ReportText.TextSettings.FontColor := claWhite;
    HelpText.TextSettings.FontColor := claWhite;
  end;
end;

procedure TFormMain.UpdateBackgroundColor(AColor: TAlphaColor);
begin
  Self.Fill.Color := AColor;
end;

procedure TFormMain.Reset;
begin
  DefaultCaption := ApplicationTitleText;
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
      HelpText.Visible := not HelpText.Visible;
      ReportText.Visible := False;
    end;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faMemeToggleReport:
    begin
      Flash(HelpCaptionText);
      HelpText.Visible := False;
      ReportText.Visible := not ReportText.Visible;
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

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);
    faKoppelBtn: KoppelBtnClick(nil);
    faMemoryBtn: MemoryBtnClick(nil);

    faToggleFontColor: ToggleFontColor;

    faShowActi: ActiBtnClick(nil);
    faShowMemo: MemoBtnClick(nil);

    faToggleSandboxed: IsSandboxed := not IsSandboxed;
    faToggleAllProps: AllProps := not AllProps;
    faToggleAllTags: ReportManager.XmlAllTags := not ReportManager.XmlAllTags;

    else
    begin
      { do nothing }
    end;

  end;
  UpdateSpeedButtonDown;
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

  HelpText.Text := HL.Text;

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

function TFormMain.GetShowDataText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgDataText);
end;

function TFormMain.GetShowDiffText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgDiffText);
end;

function TFormMain.GetShowTrimmText: Boolean;
begin
  result := ReportText.Visible and (ReportManager.CurrentReport = TRggReport.rgTrimmText);
end;

procedure TFormMain.ShowReport(const Value: TRggReport);
begin
  HelpText.Visible := False;
  ReportText.Visible := True;
  ReportManager.CurrentReport := Value;
  UpdateReport;
  UpdateItemIndexReports;
end;

procedure TFormMain.SetShowDataText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgDataText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetShowDiffText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgDiffText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetShowTrimmText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgTrimmText);
  end
  else
  begin
    ReportText.Visible := False;
  end;
end;

procedure TFormMain.SetupText(T: TText);
begin
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 18;
  T.AutoSize := True;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
  if CB = nil then
    Exit;

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
  if LB = nil then
    Exit;

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

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  HintText.Parent := Self;
  HintText.WordWrap := False;
  HintText.HorzTextAlign := TTextAlign.Leading;
  HintText.Font.Family := 'Consolas';
  HintText.Font.Size := 18;
  HintText.AutoSize := True;
  HintText.HitTest := False;

  HelpText := TText.Create(Self);
  HelpText.Parent := Self;
  HelpText.WordWrap := False;
  HelpText.HorzTextAlign := TTextAlign.Leading;
  HelpText.Font.Family := 'Courier New';
  HelpText.Font.Size := 16;
  HelpText.AutoSize := True;
  HelpText.HitTest := False;

  ReportText := TText.Create(Self);
  ReportText.Parent := Self;
  ReportText.WordWrap := False;
  ReportText.HorzTextAlign := TTextAlign.Leading;
  ReportText.Font.Family := 'Courier New';
  ReportText.Font.Size := 16;
  ReportText.AutoSize := True;
  ReportText.HitTest := False;

  SpeedPanel := TActionSpeedBarRG01.Create(Self);
  SpeedPanel.Parent := Self;
  SpeedPanel.ShowHint := True;
  SpeedPanel.Opacity := OpacityValue;

  ParamListbox := TListbox.Create(Self);
  ParamListbox.Parent := Self;
  ParamListbox.Opacity := OpacityValue;

  TrimmMemo := TMemo.Create(Self);
  TrimmMemo.Parent := Self;
  TrimmMemo.ReadOnly := True;
  TrimmMemo.CanFocus := False;
  TrimmMemo.Opacity := OpacityValue;

  TrimmCombo := TComboBox.Create(Self);
  TrimmCombo.Parent := Self;

//  ParamCombo := TComboBox.Create(Self);
//  ParamCombo.Parent := Self;

//  ReportCombo := TComboBox.Create(Self);
//  ReportCombo.Parent := Self;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;

  Bitmap := TBitmap.Create(1024, 768);

  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.Bitmap := Bitmap;
  Image.WrapMode := TImageWrapMode.Original;
end;

procedure TFormMain.LayoutComponents;
begin
  { Problem: ClientWidht and ClientHeight are still at DesignTime Values.}
  { It only 'works' if these are big enough, }
  { So that computed values for Height and Width are > 0 }
  SpeedPanel.Position.X := 2 * Raster + Margin;
  SpeedPanel.Position.Y := Raster + Margin;
  SpeedPanel.Width := ClientWidth - 3 * Raster - 2 * Margin;
  SpeedPanel.Height := SpeedPanelHeight;
  SpeedPanel.Anchors := Image.Anchors + [TAnchorKind.akRight];

  TrimmMemo.Position.Y := 2 * Raster + Margin;
  TrimmMemo.Position.X := Raster + Margin;
  TrimmMemo.Width := 200;
  TrimmMemo.Height := 150;

  TrimmCombo.Position.X := TrimmMemo.Position.X;
  TrimmCombo.Position.Y := TrimmMemo.Position.Y + TrimmMemo.Height + Margin;
  TrimmCombo.Width := TrimmMemo.Width;

//  ParamCombo.Position.X := TrimmCombo.Position.X;
//  ParamCombo.Position.Y := TrimmCombo.Position.Y + TrimmCombo.Height + Margin;
//  ParamCombo.Width := TrimmMemo.Width;

//  ReportCombo.Position.X := ParamCombo.Position.X;
//  ReportCombo.Position.Y := ParamCombo.Position.Y + ParamCombo.Height + Margin;
//  ReportCombo.Width := TrimmMemo.Width;

  ParamListbox.Position.X := TrimmMemo.Position.X;
  ParamListbox.Position.Y := TrimmCombo.Position.Y + TrimmCombo.Height + Margin;
  ParamListbox.Width := TrimmMemo.Width;
  ParamListbox.Height := 270;
//  ParamListbox.Height := ClientHeight - ParamListbox.Position.Y - Margin - Raster;
//  ParamListbox.Anchors := ParamListbox.Anchors + [TAnchorKind.akBottom];

  ReportListbox.Position.X := ParamListbox.Position.X;
  ReportListbox.Position.Y := ParamListbox.Position.Y + ParamListbox.Height + Margin;
  ReportListbox.Width := ParamListbox.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  Image.Position.Y := TrimmMemo.Position.Y;
  Image.Position.X := TrimmMemo.Position.X + TrimmMemo.Width + Margin;
  Image.Width := ClientWidth - Image.Position.X - Raster - Margin;
  Image.Height := ClientHeight - Image.Position.Y - Raster - Margin;
  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
  ImagePositionX := Image.Position.X;
  ImagePositionY := Image.Position.Y;

  HintText.Position.X := Image.Position.X + 150;
  HintText.Position.Y := Image.Position.Y;

  HelpText.Position.X := Image.Position.X + 150;
  HelpText.Position.Y := Image.Position.Y + HintText.Height + Margin;

  ReportText.Position.X := HelpText.Position.X;
  ReportText.Position.Y := HelpText.Position.Y;
  TextPositionX := ReportText.Position.X;
  TextPositionY := ReportText.Position.Y;
end;

procedure TFormMain.ReportListboxChange(Sender: TObject);
var
  ii: Integer;
begin
  RL.Clear;
  ii := ReportListbox.ItemIndex;
  if (ii >= 0) and (ii <= Integer(High(TRggReport)))then
  begin
    HelpText.Visible := False;
    ReportText.Visible := True;
    ReportManager.CurrentIndex := ii;
    UpdateReport;
    Main.FederText.CheckState;
  end;
end;

procedure TFormMain.InitReportCombo;
begin
  ReportManager.InitLB(ReportCombo.Items);
end;

procedure TFormMain.ReportComboChange(Sender: TObject);
var
  ii: Integer;
begin
  if ReportCombo <> nil then
  begin
    ii := ReportCombo.ItemIndex;
    ReportManager.CurrentIndex := ii;
    UpdateReport;
    Main.FederText.CheckState;
  end;
end;

procedure TFormMain.ParamListboxChange(Sender: TObject);
begin
  if ParamListbox.ItemIndex <> -1 then
    Main.RggMain.Param := Main.RggMain.Text2Param(ParamListbox.Items[ParamListbox.ItemIndex]);
  ShowTrimm;
  Main.FederText.CheckState;
end;

procedure TFormMain.InitParamListbox;
var
  rm: TRggMain;
  LI: TStrings;
  fp: TFederParam;
  s: string;

  procedure Add(fp: TFederParam);
  begin
    LI.AddObject(rm.Param2Text(fp), TObject(fp));
  end;
begin
  rm := Main.RggMain;
  LI := ParamListbox.Items;
  LI.Clear;

  { Add a subset of available Params }
//  Add(fpController);
//  Add(fpWinkel);
  Add(fpVorstag);
  Add(fpWante);
  Add(fpWoben);
  Add(fpSalingH);
  Add(fpSalingA);
  Add(fpSalingL);
  Add(fpSalingW);
  Add(fpMastfallF0C);
  Add(fpMastfallF0F);
  Add(fpBiegung);
  Add(fpD0X);

  { Init ItemIndex }
  fp := rm.Param;
  s := rm.Param2Text(fp);
  ParamListbox.ItemIndex := LI.IndexOf(s);
end;

procedure TFormMain.InitParamCombo;
  procedure Add(fp: TFederParam);
  begin
    ParamCombo.Items.AddObject(Main.RggMain.Param2Text(fp), TObject(fp));
  end;
begin
  if ParamCombo <> nil then
    Exit;
  Add(fpVorstag);
  Add(fpWinkel);
  Add(fpController);
  Add(fpWante);
  Add(fpWoben);
  Add(fpSalingH);
  Add(fpSalingA);
  Add(fpSalingL);
  Add(fpSalingW);
  Add(fpMastfallF0C);
  Add(fpMastfallF0F);
  Add(fpMastfallVorlauf);
  Add(fpBiegung);
  Add(fpD0X);
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
    Main.FederText.CheckState;
    if ReportText.Visible then
    begin
      ShowTrimmData;
      ReportText.Text := RL.Text;
    end;
  end;
end;

procedure TFormMain.ShowTrimmData;
begin
  RL.BeginUpdate;
  try
    RL.Clear;
    Main.CurrentTrimm.WantAll := AllProps;
    Main.CurrentTrimm.SaveTrimmItem(RL);
    Main.CurrentTrimm.WantAll := False;
    if ReportLabel <> nil then
    begin
      ReportLabel.Text := 'Trimm' + IntToStr(Main.Trimm);
    end;
  finally
    RL.EndUpdate;
  end;
end;

procedure TFormMain.ShowTrimm;
begin
  if TL <> nil then
    Main.RggMain.UpdateTrimmText(TL);
  UpdateReport;
end;

procedure TFormMain.SetupListboxItems(LB: TListbox; cla: TAlphaColor);
var
  i: Integer;
  cr: TListBoxItem;
  T: TText;
begin
  if LB = nil then
    Exit;
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

procedure TFormMain.SofortBtnClick(Sender: TObject);
begin
  Main.RggMain.SofortBerechnen := not Main.RggMain.SofortBerechnen;
end;

procedure TFormMain.GrauBtnClick(Sender: TObject);
begin
  Main.RggMain.BtnGrauDown := not Main.RggMain.BtnGrauDown;
end;

procedure TFormMain.BlauBtnClick(Sender: TObject);
begin
  Main.RggMain.BtnBlauDown := not Main.RggMain.BtnBlauDown;
end;

procedure TFormMain.MemoryBtnClick(Sender: TObject);
begin
  Main.RggMain.StrokeRigg.KoordinatenR := Main.RggMain.Rigg.rP;
  Main.RggMain.Draw;
end;

procedure TFormMain.MultiBtnClick(Sender: TObject);
begin
//  RotaForm.WantOverlayedRiggs := not RotaForm.WantOverlayedRiggs;
//  Main.RggMain.Draw;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
//  RotaForm.KoppelBtnClick(Sender);
end;

function TFormMain.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := IsSandboxed;
    faToggleAllProps: result := AllProps;
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

procedure TFormMain.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TControl).Tag;
  case fa of
    faSofortBtn: SofortBtnClick(Sender);
    faGrauBtn: GrauBtnClick(Sender);
    faBlauBtn: BlauBtnClick(Sender);
    faMemoryBtn: MemoryBtnClick(Sender);
    faMultiBtn: MultiBtnClick(Sender);
    faKoppelBtn: KoppelBtnClick(Sender);
  end;

  { When not called via Action }
  if Sender <> nil then
  begin
    UpdateSpeedButtonEnabled;
    Main.FederText.CheckState;
    UpdateSpeedButtonDown;
  end;
end;

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel <> nil then
    SpeedPanel.InitSpeedButtons;
end;

procedure TFormMain.UpdateSpeedButtonDown;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonDown;
end;

procedure TFormMain.UpdateSpeedButtonEnabled;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonEnabled;
end;

end.
