﻿unit FrmMain;

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
  RggScroll,
  RggDisplayTypes,
  RggDisplay,
  RggRaumGraph,
  RggRota,
  RggTestData,
  RggCtrls,
  RggChartGraph,
  FMX.Platform,
  FMX.Graphics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Listbox,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Surfaces,
  FMX.Layouts;

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
    TrimmText: TText;
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
    ComponentsCreated: Boolean;
    procedure CreateComponents;
    procedure CheckSpaceForImages;
    procedure CheckSpaceForMemo;
    procedure CheckSpaceForListbox;
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
    procedure ListboxItemStyleLookup(Sender: TObject);
  private
    Raster: Integer;
    Margin: Integer;
    ListboxWidth: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
  public
    procedure ChartImageBtnClick(Sender: TObject);
    procedure SalingImageBtnClick(Sender: TObject);
    procedure ControllerImageBtnClick(Sender: TObject);

    procedure LineColorBtnClick(Sender: TObject);
    procedure SeiteBtnClick(Sender: TObject);
    procedure AchternBtnClick(Sender: TObject);
    procedure TopBtnClick(Sender: TObject);
    procedure NullBtnClick(Sender: TObject);

    procedure MemoryBtnClick(Sender: TObject);
    procedure MemoryRecallBtnClick(Sender: TObject);

    procedure SofortBtnClick(Sender: TObject);
    procedure GrauBtnClick(Sender: TObject);
    procedure BlauBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);
  public
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
  public
    DL: TRggDisplayList;
    RotaForm: TRotaForm;
    procedure HandleSegment(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    FViewPoint: TViewPoint;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    procedure SetViewPoint(const Value: TViewPoint);
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property IsUp: Boolean read GetIsUp write SetIsUp;
  protected
    Bitmap: TBitmap;
    Image: TImage;
    ImagePositionX: single;
    ImagePositionY: single;
    TextPositionX: single;
    TextPositionY: single;
  public
    SalingImage: TImage;
    SalingGraph: TSalingGraph;
    ControllerImage: TImage;
    ControllerGraph: TSalingGraph;
    ChartImage: TImage;
    ChartGraph: TChartGraph;
    procedure InitSalingGraph;
    procedure InitControllerGraph;
    procedure InitChartGraph;
    procedure UpdateSalingGraph;
    procedure UpdateControllerGraph;
    procedure UpdateChartGraph;
    procedure LayoutImages;
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
  RiggVar.RG.Speed02,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

const
  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'RG38';

{ TFormMain }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
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
  Top := 30;
  Width := 1700;
  Height := 960;
  Margin := 2;
  Raster := MainVar.Raster;
  SpeedPanelHeight := Raster;
  ListboxWidth := 200;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

  CreateComponents;

  SetupListbox(ParamListbox);
  SetupListbox(ReportListbox);

  Rigg := TRigg.Create;
  Rigg.ControllerTyp := ctOhne;

  rggm := TRggMain.Create(Rigg); // rggm owns Rigg
  Main := TMain.Create(rggm); // Main owns rggm
  Main.Logger.Verbose := True;
  rggm.InitLogo; // sets WantLogoData to true
  rggm.Init420; // resets WantLogoData to false

  Main.Trimm := 1;

  Main.InitText;
  Main.IsUp := True;

  RotaForm := TRotaForm.Create; // ownership kept
  rggm.StrokeRigg := RotaForm;
  RotaForm.Image := Image;
  RotaForm.Init;
  DL := RotaForm.RaumGraph.DL;
  RotaForm.IsUp := True;
  RotaForm.ViewPoint := vp3D;
  RotaForm.ZoomIndex := 8;
//  RotaForm.LegendItemChecked := False;
  RotaForm.FixPoint := ooD;
//  RotaForm.RaumGraph.Koppel := True;

  RotaForm.Draw;

  { Params }
  Main.RggMain.Param := fpVorstag; // --> TempIst wird gesetzt, SetupTrackBar() aufgerufen
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

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claYellow;

  HelpText.BringToFront;
  HelpText.TextSettings.FontColor := claWhite;
  HelpText.Visible := False;

  ReportText.BringToFront;
  ReportText.TextSettings.FontColor := claAntiquewhite;
  ReportText.Visible := True;

  TrimmText.BringToFront;
  TrimmText.TextSettings.FontColor := claBeige;
  TrimmText.Visible := True;

  InitHelpText;

  TL := TStringList.Create;
  Main.UpdateTrimm0;
  ShowTrimm;

  Reset;

  { Background }
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

  InitSalingGraph;
  InitControllerGraph;
  InitChartGraph;

  Main.RggMain.Draw;
  Main.RggMain.MemoryBtnClick;
  Main.FederText.CheckState;

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
  UpdateColorScheme;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  DestroyForms;

  TL.Free;
  RL.Free;
  ReportManager.Free;
  RotaForm := nil;
  Main.Free;
  Main := nil;

  Image.Free;
  Bitmap.Free;

  SalingGraph.Free;
  ControllerGraph.Free;
  ChartGraph.Free;
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
    Main.ActionHandler.Execute(fa);
  end;

  ShowTrimm;
end;

procedure TFormMain.UpdateReport;
begin
  if ReportText = nil then
    Exit;
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
    UpdateSalingGraph;
    UpdateControllerGraph;
    UpdateChartGraph;
    Handled := True;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    { ClientHeigt is now available }
    LayoutComponents;
    LayoutImages;
    SetupListboxItems(ParamListbox, claAqua);
    SetupListboxItems(ReportListbox, claAquamarine);
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
//  CheckSpaceForListbox;
  CheckSpaceForMemo;
  CheckSpaceForImages;
end;

procedure TFormMain.CheckSpaceForListbox;
begin
  if not FormShown then
    Exit;
  ReportListbox.Visible := ClientHeight > 910;
end;

procedure TFormMain.CheckSpaceForMemo;
begin
  if not FormShown then
    Exit;
  if not ComponentsCreated then
    Exit;

  if (ClientWidth < 1100) or (ClientHeight < 800) then
  begin
    if RotaForm.LegendItemChecked then
    begin
      RotaForm.LegendBtnClick(nil);
    end;
    SpeedPanel.Visible := False;
    TrimmText.Visible := False;
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
    TrimmText.Visible := True;
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
  if not ComponentsCreated then
    Exit;

  { At aplication start up FormResize is called serveral times,
    but always before FormShow always called. }

  { ClientWidth and ClientHeight are not yet available when starting up.
    ClientHeigt is available when FormShow is called.
   }
  if FormShown then
  begin
    { when FormResize is called after FormShow }
    if (ClientWidth < 1200) or (ClientHeight < 600) then
      ChartImage.Visible := False;
    if (ClientWidth < 1500) or (ClientHeight < 655) then
      ControllerImage.Visible := False;
    if (ClientWidth < 1500) or (ClientHeight < 875) then
      SalingImage.Visible := False;
  end
  else
  begin
    { when FormResize is called before FormShow }
    if (Width < 1200) or (Height < 600) then
      ChartImage.Visible := False;
    if (Width < 1500) or (Height < 655) then
      ControllerImage.Visible := False;
    if (Width < 1500) or (Height < 875) then
      SalingImage.Visible := False;
  end;

  Main.FederText.CheckState;
  UpdateSpeedButtonDown;
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

    faChartRect..faChartReset: ChartGraph.HA(fa);

    faToggleLineColor: LineColorBtnClick(nil);

    faToggleSegmentF..faToggleSegmentA: HandleSegment(fa);

    faRggZoomIn: RotaForm.ZoomInBtnClick(nil);
    faRggZoomOut: RotaForm.ZoomOutBtnClick(nil);

    faToggleUseDisplayList:
    begin
      RotaForm.UseDisplayListBtnClick(nil);
      UpdateSpeedButtonEnabled;
    end;

    faToggleShowLegend: RotaForm.LegendBtnClick(nil);
    faToggleUseQuickSort: RotaForm.UseQuickSortBtnClick(nil);
    faRggBogen: RotaForm.BogenBtnClick(nil);
    faToggleChartGraph: ChartImageBtnClick(nil);
    faToggleSalingGraph: SalingImageBtnClick(nil);
    faToggleControllerGraph: ControllerImageBtnClick(nil);
    faToggleMatrixText: RotaForm.MatrixItemClick(nil);

    faMemoryBtn: MemoryBtnClick(nil);
    faMemoryRecallBtn: MemoryRecallBtnClick(nil);

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);
    faKoppelBtn: KoppelBtnClick(nil);

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
    'a': fa := faSalingA;
    'A': fa := faFixpointA0;

    'b': fa := faFixpointB; // fa := faCycleBitmapP;
    'B': fa := faFixpointB0; // fa := faCycleBitmapM;

    'c': fa := faCycleColorSchemeP;
    'C': fa := faCycleColorSchemeM;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faWheelLeft; // fa := faFixpointF;
    'F': fa := faFixpointF0;

    'g': fa := faWheelDown;
    'G': ;

    'h': fa := faMemeToggleHelp; // fa := faSalingH;
    'H': fa := faSalingH; // fa := faHull;

    'i': fa := faToggleLineColor;
    'I': fa := faToggleUseDisplayList;

    'j': fa := faWheelRight;
    'J': ;

    'k': fa := faWheelUp;
    'K': fa := faKoppelBtn;

    'l': fa := faMemeGotoLandscape;
    'L': fa := faToggleShowLegend;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': ; // fa := faRandomBlack;
    'N': ; // fa := faRandomWhite;

    'r': fa := faMemeToggleReport;
    'R': fa := faReadTrimmFile;

    'o': fa := faWoben;

    'p': fa := faMemeGotoPortrait;

    'Q': fa := faToggleUseQuickSort;

    's': fa := faMemeGotoSquare;

    't': fa := faToggleFontColor; // fa := faParamT1
    'T': ; // fa := faParamT2;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;
    'w': fa := faWante;

    'z': ; // fa := faResetZoom;
    'Z': fa := faUpdateTrimm0;

    '0': fa := faTrimm0;
    '1': fa := faTrimm1;
    '2': fa := faTrimm2;
    '3': fa := faTrimm3;
    '4': fa := faTrimm4;
    '5': fa := faTrimm5;
    '6': fa := faTrimm6;
    '7': fa := fa420;
    '8': fa := faLogo;
    '9': ;

    '!': ; //fa := faParamT1;
    '"': ; //fa := faParamT2;

    '=': ; //fa := faActionPageE;
    '?': ; //fa := faActionPageX;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': ; //fa := faBitmapEscape;

    else fa := faNoop;

  end;
  result := fa;
end;

function TFormMain.GetIsUp: Boolean;
begin
  if not MainVar.AppIsClosing and Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
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
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 16;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
  if CB = nil then
    Exit;

{$ifdef FMX}
  CB.StyleLookup := 'comboboxstyle';
{$endif}

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

{$ifdef FMX}
  LB.ShowScrollBars := False;
  LB.StyleLookup := 'transparentlistboxstyle';
//  LB.StyleLookup := 'listboxstyle';
{$endif}

{$ifdef Vcl}
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
{$endif}
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
  if MM = nil then
    Exit;

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
  SetupText(HintText);
  HintText.Font.Family := 'Consolas';
//  HintText.Font.Size := 18;

  HelpText := TText.Create(Self);
  SetupText(HelpText);
  HelpText.Font.Family := 'Courier New';

  ReportText := TText.Create(Self);
  SetupText(ReportText);
  ReportText.Font.Family := 'Courier New';

  TrimmText := TText.Create(Self);
  SetupText(TrimmText);

  SpeedPanel := TActionSpeedBarRG02.Create(Self);
  SpeedPanel.Parent := Self;
  SpeedPanel.ShowHint := True;
  SpeedPanel.Opacity := OpacityValue;

  ParamListbox := TListbox.Create(Self);
  ParamListbox.Parent := Self;
  ParamListbox.Opacity := OpacityValue;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;

  Bitmap := TBitmap.Create(1024, 768);

  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.Bitmap := Bitmap;
  Image.WrapMode := TImageWrapMode.Original;

  ComponentsCreated := True;
end;

procedure TFormMain.LayoutComponents;
begin
  if not ComponentsCreated then
    Exit;

  { Problem: ClientWidht and ClientHeight are still at DesignTime Values.}
  { It only 'works' if these are big enough, }
  { So that computed values for Height and Width are > 0 }
  SpeedPanel.Position.X := 2 * Raster + Margin;
  SpeedPanel.Position.Y := Raster + Margin;
  SpeedPanel.Width := ClientWidth - 3 * Raster - 2 * Margin;
  SpeedPanel.Height := SpeedPanelHeight;
  SpeedPanel.Anchors := SpeedPanel.Anchors + [TAnchorKind.akRight];

  TrimmText.Position.X := Raster + Margin;
  TrimmText.Position.Y := 2 * Raster + Margin;
  TrimmText.Width := ListboxWidth;
  TrimmText.Height := 150;

  ParamListbox.Position.X := TrimmText.Position.X;
  ParamListbox.Position.Y := TrimmText.Position.Y + TrimmText.Height + Margin;
  ParamListbox.Width := ListboxWidth;
  ParamListbox.Height := 270;
//  ParamListbox.Height := ClientHeight - ParamListbox.Position.Y - Margin - Raster;
//  ParamListbox.Anchors := ParamListbox.Anchors + [TAnchorKind.akBottom];

  ReportListbox.Position.X := ParamListbox.Position.X;
  ReportListbox.Position.Y := ParamListbox.Position.Y + ParamListbox.Height + Margin;
  ReportListbox.Width := ParamListbox.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  Image.Position.Y := TrimmText.Position.Y;
  Image.Position.X := TrimmText.Position.X + ListboxWidth + Margin;
  Image.Width := ClientWidth - Image.Position.X - Raster - Margin;
  Image.Height := ClientHeight - Image.Position.Y - Raster - Margin;
  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
  ImagePositionX := Image.Position.X;
  ImagePositionY := Image.Position.Y;

  HintText.Position.X := Image.Position.X + 150;
  HintText.Position.Y := Image.Position.Y;

  HelpText.Position.X := Image.Position.X + 150;
  HelpText.Position.Y := Image.Position.Y + 30 + Margin;

  ReportText.Position.X := HelpText.Position.X;
  ReportText.Position.Y := HelpText.Position.Y;

  TextPositionX := ReportText.Position.X;
  TextPositionY := ReportText.Position.Y;
end;

procedure TFormMain.LineColorBtnClick(Sender: TObject);
var
  b: Boolean;
begin
  b := not DL.WantLineColors;
  DL.WantLineColors := b;
  UpdateSpeedButtonDown;
  RotaForm.Draw;
end;

procedure TFormMain.HandleSegment(fa: Integer);
var
  rg: TRaumGraph;
begin
  rg := RotaForm.RaumGraph;

  case fa of
    faToggleSegmentF: rg.WantFixpunkt := not rg.WantFixPunkt;
    faToggleSegmentM: rg.WantMast:= not rg.WantMast;
    faToggleSegmentW: rg.WantWante:= not rg.WantWante;
    faToggleSegmentV: rg.WantVorstag := not rg.WantVorstag;
    faToggleSegmentS: rg.WantSaling := not rg.WantSaling;
    faToggleSegmentR: rg.WantRumpf := not rg.WantRumpf;
    faToggleSegmentC: rg.WantController := not rg.WantController;
    faToggleSegmentA: rg.WantAchsen := not rg.WantAchsen;
  end;

  RotaForm.Draw;
end;

procedure TFormMain.ChartImageBtnClick(Sender: TObject);
begin
  ChartImage.Visible := not ChartImage.Visible;
end;

procedure TFormMain.SalingImageBtnClick(Sender: TObject);
begin
  SalingImage.Visible := not SalingImage.Visible;
end;

procedure TFormMain.ControllerImageBtnClick(Sender: TObject);
begin
  ControllerImage.Visible := not ControllerImage.Visible;
  if ControllerImage.Visible then
    UpdateControllerGraph;
end;

procedure TFormMain.SeiteBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpSeite;
end;

procedure TFormMain.AchternBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpAchtern;
end;

procedure TFormMain.TopBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vpTop;
end;

procedure TFormMain.NullBtnClick(Sender: TObject);
begin
  RotaForm.ViewPoint := vp3D;
end;

procedure TFormMain.InitSalingGraph;
begin
  SalingImage := TImage.Create(Self);
  SalingImage.Parent := Self;
  SalingImage.HitTest := False;

  SalingGraph := TSalingGraph.Create;
  SalingGraph.BackgroundColor := claAntiquewhite;
  SalingGraph.ImageOpacity := 0.2;
  SalingGraph.SalingA := 850;
  SalingGraph.SalingH := 120;
  SalingGraph.SalingL := 479;
  SalingGraph.SalingHOffset := 37;
  SalingGraph.Image := SalingImage;
  UpdateSalingGraph;
end;

procedure TFormMain.UpdateSalingGraph;
begin
  if IsUp and SalingImage.Visible then
  begin
    SalingGraph.SalingA := Round(Main.RggMain.ParamValue[fpSalingA]);
    SalingGraph.SalingH := Round(Main.RggMain.ParamValue[fpSalingH]);
    SalingGraph.SalingL := Round(Main.RggMain.ParamValue[fpSalingL]);
    SalingGraph.Draw(TFigure.dtSalingDetail);
  end;
end;

procedure TFormMain.InitControllerGraph;
begin
  ControllerImage := TImage.Create(Self);
  ControllerImage.Parent := Self;
  ControllerImage.HitTest := False;
  ControllerImage.Visible := False;

  ControllerGraph := TSalingGraph.Create;
  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  ControllerGraph.ImageOpacity := 0.2;

  ControllerGraph.ControllerTyp := TControllerTyp.ctDruck;
  ControllerGraph.EdgePos := 25;
  ControllerGraph.ControllerPos := 80;
  ControllerGraph.ParamXE := -20;
  ControllerGraph.ParamXE0 := 110;

  ControllerGraph.Image := ControllerImage;
  UpdateControllerGraph;
end;

procedure TFormMain.UpdateControllerGraph;
//var
//  TrimmRec: TTrimmControls;
begin
  if IsUp and ControllerImage.Visible then
  begin
//    TrimmRec := Rigg.Glieder;

    ControllerGraph.ControllerTyp := Rigg.ControllerTyp;
//    ControllerGraph.ControllerPos := TrimmRec.Controller;
    ControllerGraph.ControllerPos := Round(Main.RggMain.ParamValue[fpController]);
    ControllerGraph.ParamXE := Round(Rigg.MastPositionE);
    ControllerGraph.ParamXE0 := Round(Rigg.iP[ooE0, x] - Rigg.iP[ooD0, x]);
    ControllerGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);

    ControllerGraph.Draw(TFigure.dtController);
  end;
end;

procedure TFormMain.InitChartGraph;
begin
  ChartImage := TImage.Create(Self);
  ChartImage.Parent := Self;
  ChartImage.HitTest := False;

  ChartGraph := TChartGraph.Create;
  ChartGraph.Image := ChartImage;
  ChartGraph.SuperInit;
  ChartGraph.WantRectangles := True;

  UpdateChartGraph;
end;

procedure TFormMain.UpdateChartGraph;
begin
  if IsUp and ChartImage.Visible then
  begin
    ChartGraph.SuperCalc;
  end;
end;

procedure TFormMain.LayoutImages;
var
  PosX: single;
  PosY: single;
begin
  if not ComponentsCreated then
    Exit;

  PosX := ClientWidth - Raster - Margin - ControllerImage.Width;
  PosY := SpeedPanel.Position.Y + SpeedPanel.Height + Margin;
  PosY := PosY + ControllerImage.Height;

  ControllerImage.Position.X := PosX;
  ControllerImage.Position.Y := PosY;
  ControllerImage.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];

  SalingImage.Position.X := PosX;
  SalingImage.Position.Y := PosY + ControllerImage.Height + Margin;
  SalingImage.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];

  ChartImage.Position.X := Image.Position.X + 700;
  ChartImage.Position.Y := Image.Position.Y + 0;
end;

procedure TFormMain.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  RotaForm.ViewPoint := Value;
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
  Add(fpController);
  Add(fpWinkel);
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
  begin
    Main.RggMain.UpdateTrimmText(TL);
    TrimmText.Text := TL.Text;
  end;
  UpdateReport;
end;

procedure TFormMain.SetupListboxItems(LB: TListbox; cla: TAlphaColor);
var
  i: Integer;
  cr: TListBoxItem;
begin
  if LB = nil then
    Exit;

  if LB.Items.Count > 0 then
  for i := 0 to LB.Items.Count - 1 do
  begin
    cr := LB.ItemByIndex(i);
    cr.Tag := cla;
    cr.StyledSettings := cr.StyledSettings - [TStyledSetting.FontColor, TStyledSetting.Size];
    cr.OnApplyStyleLookup := ListboxItemStyleLookup;

    ListboxItemStyleLookup(cr);
  end;
end;

procedure TFormMain.ListboxItemStyleLookup(Sender: TObject);
var
  cr: TListBoxItem;
  T: TText;
begin
  cr := Sender as TListboxItem;
  T := cr.FindStyleResource('text') as TText;
  if Assigned(T) then
  begin
//    T.Font.Family := 'Consolas';
    T.Font.Size := 14;
    T.TextSettings.FontColor := cr.Tag;
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
  Main.RggMain.MemoryBtnClick;
  UpdateReport;
end;

procedure TFormMain.MemoryRecallBtnClick(Sender: TObject);
begin
  Main.RggMain.MemoryRecallBtnClick;
  ShowTrimm;
end;

procedure TFormMain.MultiBtnClick(Sender: TObject);
begin
  RotaForm.WantOverlayedRiggs := not RotaForm.WantOverlayedRiggs;
  Main.RggMain.Draw;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  RotaForm.KoppelBtnClick(Sender);
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

    faMemeToggleHelp: result := HelpText.Visible;
    faMemeToggleReport: result := ReportText.Visible;
    faButtonFrameReport: result := WantButtonFrameReport;
    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
    faToggleSegmentF..faToggleSegmentA: result := RotaForm.RaumGraph.GetChecked(fa);

    faToggleLineColor: result := DL.WantLineColors;
    faToggleShowLegend: result := RotaForm.LegendItemChecked;

//    faViewpointS: result := RotaForm.ViewPoint = vpSeite;
//    faViewpointA: result := RotaForm.ViewPoint = vpAchtern;
//    faViewpointT: result := RotaForm.ViewPoint = vpTop;
//    faViewpoint3: result := RotaForm.ViewPoint = vp3D;

//    ZoomInBtn: result := False;
//    ZoomOutBtn: result := False;

    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
    faToggleUseQuickSort: result := RotaForm.RaumGraph.DL.UseQuickSort;
    faRggBogen: result := RotaForm.Bogen;

    faSofortBtn: result := Main.RggMain.SofortBerechnen;
    faGrauBtn: result := Main.RggMain.BtnGrauDown;
    faBlauBtn: result := Main.RggMain.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;
    faKoppelBtn: result := RotaForm.RaumGraph.Koppel;

    faToggleChartGraph: result := ChartImage.IsVisible;
    faToggleSalingGraph: result := SalingImage.IsVisible;
    faToggleControllerGraph: result := ControllerImage.IsVisible;
    faToggleMatrixText: result := RotaForm.MatrixItemChecked;
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

procedure TFormMain.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  UpdateBackgroundColor(SpeedPanel.SpeedColorScheme.claBack);

  if ReportLabel <> nil then
    ReportLabel.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claReport;

  HintText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claHintText;
  TrimmText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claTrimmText;
  ReportText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claReportText;
  HelpText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claHelpText;

end;

end.
