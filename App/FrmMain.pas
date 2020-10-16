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
  RiggVar.RG.Rota,
  RiggVar.FB.SpeedBar,
  RiggVar.FD.Image,
  RggCtrls,
  RggChartGraph,
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
  FMX.ExtCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Listbox,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Surfaces,
  FMX.Layouts,
  FMX.Controls.Presentation;

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
    procedure InitScreenPos;
    procedure InitScreenPos1;
    procedure InitScreenPos2;
  private
    FScale: single;
    FWantResizeNormalizing: Boolean;
    DefaultCaption: string;
    FormShown: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure HandleShowHint(Sender: TObject);
    procedure Flash(s: string);
    procedure Reset;
    procedure InitRiggAndMain;
    procedure ShowZOrder;
    procedure ShowHelp;
  protected
    HL: TStringList;
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
    FWantButtonReport: Boolean;
    procedure UpdateReport;
    procedure UpdateBackgroundColor(AColor: TAlphaColor);
    property WantButtonReport: Boolean read FWantButtonReport;
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
  public
    ComponentsCreated: Boolean;
    procedure CreateComponents;
    procedure CheckSpaceForImages;
    procedure CheckSpaceForMemo;
    procedure CheckSpaceForListbox;
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText; fs: single = 16);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
    procedure ListboxItemStyleLookup(Sender: TObject);
  public
    Raster: Integer;
    Margin: Integer;
    ListboxWidth: Integer;
    ReportMemoWidth: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    SpeedPanel1: TActionSpeedBar;
    SpeedPanel2: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure LayoutSpeedPanel(SP: TActionSpeedBar);
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
    procedure ToggleSpeedPanel;
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

    procedure BogenBtnClick(Sender: TObject);
    procedure KoppelBtnClick(Sender: TObject);

    procedure SuperSimpleBtnClick(Sender: TObject);
    procedure SuperNormalBtnClick(Sender: TObject);
    procedure SuperGrauBtnClick(Sender: TObject);
    procedure SuperBlauBtnClick(Sender: TObject);
    procedure SuperMultiBtnClick(Sender: TObject);
    procedure SuperDisplayBtnClick(Sender: TObject);
    procedure SuperQuickBtnClick(Sender: TObject);
  public
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
  public
    RotaForm: TRotaForm;
    procedure HandleSegment(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    FViewPoint: TViewPoint;
    procedure UpdateOnParamValueChanged;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    procedure SetViewPoint(const Value: TViewPoint);
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property IsUp: Boolean read GetIsUp write SetIsUp;
  public
    BitmapWidth: Integer;
    BitmapHeight: Integer;
    Image: TOriginalImage;
    ImagePositionX: single;
    ImagePositionY: single;
    TextPositionX: single;
    TextPositionY: single;
  public
    SalingImage: TOriginalImage;
    SalingGraph: TSalingGraph;
    ControllerImage: TOriginalImage;
    ControllerGraph: TSalingGraph;
    ChartImage: TOriginalImage;
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
    procedure ActionsBtnClick(Sender: TObject);
    procedure DrawingsBtnClick(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure TrimmTabBtnClick(Sender: TObject);
    procedure CheckFormBounds(AForm: TForm);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FrmMemo,
  FrmAction,
  FrmDrawing,
  FrmConfig,
  FrmTrimmTab,
  RiggVar.RG.Main,
  RiggVar.RG.Speed01,
  RiggVar.RG.Speed02,
  RiggVar.RG.Speed03,
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

procedure TFormMain.InitRiggAndMain;
begin
  Rigg := TRigg.Create;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg);
  Main.Logger.Verbose := True;
  Main.InitLogo; // sets WantLogoData to true
  Main.Init420; // resets WantLogoData to false

  Main.Trimm := 1;

  Main.InitText;
  Main.IsUp := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
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
  InitScreenPos;

  Margin := 2;
  Raster := MainVar.Raster;
  MainVar.Scale := FScale;
  MainVar.ScaledRaster := Raster;

  SpeedPanelHeight := Raster;
  ListboxWidth := 200;

  BitmapWidth := 1024;
  BitmapHeight := 800;

  CreateComponents;

  SetupListbox(ParamListbox);
  SetupListbox(ReportListbox);

  InitRiggAndMain;

  RotaForm := TRotaForm.Create;
  RotaForm.Image := Image;
  RotaForm.Init;
  RotaForm.SwapRota(1);

  { Params }
  Main.Param := fpVorstag;
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
  HL := TStringList.Create;
  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  ReportManager.CurrentReport := rgDiffText;
  if ReportListbox <> nil then
  begin
    ReportManager.InitLB(ReportListbox.Items);
    ReportListbox.OnChange := ReportListboxChange;
    ReportListbox.ItemIndex := ReportListbox.Items.IndexOf(
    ReportManager.GetReportCaption(ReportManager.CurrentReport));
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

  HintText.TextSettings.FontColor := claYellow;

  HelpText.TextSettings.FontColor := claWhite;
  HelpText.Visible := False;

  ReportText.TextSettings.FontColor := claAntiquewhite;
  ReportText.Visible := True;

  TrimmText.TextSettings.FontColor := claBeige;
  TrimmText.Visible := True;

  TL := TStringList.Create;
  Main.UpdateTrimm0;
  ShowTrimm;

  Reset;

  { Background }
  if MainVar.ColorScheme.claBackground <> claSlateBlue then
  begin
    Fill.Kind := TBrushKind.Solid; // because is still TBrushKind.None
    Fill.Color := claSlateblue;
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

  Main.Draw;
  Main.MemoryBtnClick;
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
  HL.Free;
  ReportManager.Free;

  Main.Free;
  Main := nil;

  Image.Free;

  SalingGraph.Free;
  ControllerGraph.Free;
  ChartGraph.Free;

  RotaForm.Free;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  fa: Integer;
begin
  fa := GetActionFromKey(Shift, Key);
  if fa = faNoop then
    fa := GetActionFromKeyChar(KeyChar);

  if fa <> faNoop then
  begin
    Main.ActionHandler.Execute(fa);
  end;
  ShowTrimm;
end;

procedure TFormMain.UpdateOnParamValueChanged;
begin
  ShowTrimm;
  UpdateSalingGraph;
  UpdateControllerGraph;
  UpdateChartGraph;
end;

procedure TFormMain.UpdateReport;
begin
  if not FormShown then
    Exit;

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

  if WantButtonReport then
  begin
    Main.FederText.Report(RL);
    ReportText.Text := RL.Text;
  end
  else
  begin
    ReportManager.ShowCurrentReport;
    ReportText.Text := RL.Text;
  end;
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
  ShowTrimm;
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
  fp := Main.Param;
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
    Main.DoMouseWheel(Shift, WheelDelta div 120);
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

    InitHelpText;

    UpdateSpeedButtonDown;
    UpdateReport;

    RotaForm.IsUp := True;
    RotaForm.Draw;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  //if not FormShown then
  //   Exit;

  { will be done via Resize and UpdateTouch }
//  MainVar.ClientWidth := ClientWidth;
//  MainVar.ClientHeight := ClientHeight;
  { 10.3.3. ClientHeight not (yet) correct when moving form between monitors }

  if (Main <> nil) and Main.IsUp then
  begin
    MainVar.Scale := Handle.Scale;
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    Main.UpdateText;
  end;

  if FormShown then
  begin
    SpeedPanel.UpdateLayout;
    UpdateReport;
//  CheckSpaceForListbox; // not necessary because Listbox is transparent
    CheckSpaceForMemo;
    CheckSpaceForImages;
  end;
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

  if (ClientWidth < 900) or (ClientHeight < 700) then
  begin
    if RotaForm.LegendItemChecked then
    begin
      RotaForm.LegendBtnClick(nil);
    end;
    TrimmText.Visible := False;
    ParamListbox.Visible := False;
    if ReportListbox <> nil then
      ReportListbox.Visible := False;

    HelpText.Position.X := Raster + 30;
    ReportText.Position.X := Raster + 30;

    Image.Position.X := 0;
    Image.Position.Y := 0;
    Image.Width := BitmapWidth;
    Image.Height := BitmapHeight;
    Image.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  end
  else
  begin
    TrimmText.Visible := True;
    ParamListbox.Visible := True;
    if ReportListbox <> nil then
      ReportListbox.Visible := True;

    HelpText.Position.X := TextPositionX;
    ReportText.Position.X := TextPositionX;

    Image.Position.X := ImagePositionX;
    Image.Position.Y := ImagePositionY;
    Image.Width := ClientWidth - Image.Position.X - Raster - Margin;
    Image.Height := ClientHeight - Image.Position.Y - Raster - Margin;
    Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
  end;
end;

procedure TFormMain.CheckSpaceForImages;
begin
  if not ComponentsCreated then
    Exit;

  { At aplication start up FormResize is called serveral times,
    but always before FormShow always called. }

  { ClientWidth and ClientHeight are not yet available when starting up.
    ClientHeigt is available when FormShow is called. }

  if FormShown then
  begin
    { when FormResize is called after FormShow }
    if (ChartImage.BoundsRect.Right > ClientWidth - Raster) or
       (ChartImage.BoundsRect.Bottom > ClientHeight - Raster) then
      ChartImage.Visible := False;

    if (ControllerImage.BoundsRect.Left < ReportText.BoundsRect.Right) or
       (ControllerImage.BoundsRect.Bottom > ClientHeight - Raster) then
      ControllerImage.Visible := False;

    if (SalingImage.BoundsRect.Left < ReportText.BoundsRect.Right) or
       (SalingImage.BoundsRect.Bottom > ClientHeight - Raster) then
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

procedure TFormMain.UpdateBackgroundColor(AColor: TAlphaColor);
begin
  Self.Fill.Color := AColor;
  RotaForm.BackgroundColor := AColor;
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
    faToggleSpeedPanel: ToggleSpeedPanel;

    faToggleHelp:
    begin
      HelpText.Visible := not HelpText.Visible;
      ReportText.Visible := False;
    end;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faToggleReport:
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

    faToggleButtonReport:
    begin
      FWantButtonReport := not WantButtonReport;
      UpdateReport;
    end;

    faReportNone..faReportReadme:
    begin
      ReportManager.HandleAction(fa);
      UpdateReport;
    end;

    faChartRect..faChartReset: ChartGraph.HandleAction(fa);

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
    faToggleSalingGraph: SalingImageBtnClick(nil);
    faToggleControllerGraph: ControllerImageBtnClick(nil);
    faToggleChartGraph: ChartImageBtnClick(nil);
    faToggleMatrixText: RotaForm.MatrixItemClick(nil);

    faMemoryBtn: MemoryBtnClick(nil);
    faMemoryRecallBtn: MemoryRecallBtnClick(nil);

    faRggBogen: BogenBtnClick(nil);
    faRggKoppel: KoppelBtnClick(nil);

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);

    faShowMemo: MemoBtnClick(nil);
    faShowActions: ActionsBtnClick(nil);
    faShowDrawings: DrawingsBtnClick(nil);
    faShowConfig: ConfigBtnClick(nil);
    faShowTrimmTab: TrimmTabBtnClick(nil);

    faToggleSandboxed: MainVar.IsSandboxed := MainConst.MustBeSandboxed or (not MainVar.IsSandboxed);
    faToggleAllProps: AllProps := not AllProps;
    faToggleAllTags: ReportManager.XmlAllTags := not ReportManager.XmlAllTags;

    faRotaForm1: RotaForm.SwapRota(1);
    faRotaForm2: RotaForm.SwapRota(2);

    faReset,
    faResetPosition,
    faResetRotation,
    faResetZoom: RotaForm.HandleAction(fa);

    faPan:
    begin
      Main.SetParameter(faPan);
      ShowTrimm;
    end;

    faShowHelp: ShowHelp;
    faShowZOrder: ShowZOrder;

    else
    begin
      { do nothing }
    end;

  end;
  UpdateSpeedButtonDown;
end;

function TFormMain.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkF12: result := faMemeGotoSquare;
//    vkC: result := faCopyTrimmItem;
//    vkV: result := faPasteTrimmItem;
    VKEscape:
    begin
      if Shift = [ssShift] then
        result := faResetPosition
      else
        result := faReset;
    end;
  end;
end;

function TFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  fa := faNoop;
  case KeyChar of
    'a': fa := faSalingA;
    'A': fa := faFixpointA0;

    'b': fa := faFixpointB;
    'B': fa := faFixpointB0;

    'c': fa := faCycleColorSchemeP;
    'C': fa := faCycleColorSchemeM;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faFixpointF;
    'F': fa := faFixpointF0;

    'g': ;
    'G': ;

    'h': fa := faToggleHelp;
    'H': fa := faSalingH;

    'i': fa := faWheelRight;
    'I': fa := faWheelLeft;

    'j': fa := faWheelUp;
    'J': fa := faWheelDown;

    'k': ;
    'K': fa := faRggKoppel;

    'l': fa := faMemeGotoLandscape;
    'L': fa := faToggleShowLegend;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': ;
    'N': ;

    'r': fa := faToggleReport;
    'R': fa := faReadTrimmFile;

    'o': fa := faWoben;

    'p': fa := faMemeGotoPortrait;

    'Q': fa := faToggleUseQuickSort;

    's': fa := faMemeGotoSquare;

    't': fa := faToggleFontColor;
    'T': fa := faToggleSpeedPanel;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;
    'w': fa := faWante;

    'z': ;
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

    '!': fa := faRotaForm1;
    '"': fa := faRotaForm2;
    '§': fa := faRotaForm3;

    '=': fa := faShowZOrder;
    '?': fa := faShowHelp;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': fa := faActionPage4;

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
begin
  HL.Clear;
  HL.Add('Toggle Text with Keys:');
  HL.Add('  h    - toggle help');
  HL.Add('  r    - toggle Report');
  HL.Add('');
  HL.Add('Select current parameter:');
  HL.Add('  with Button, Key, or in ListBox');
  HL.Add('');
  HL.Add('Change param value with Wheel!');
  HL.Add('  Shift-Wheel = small step');
  HL.Add('  Ctrl-Wheel  = bigger step');
  HL.Add('');
  HL.Add('Goto stored Trimm');
  HL.Add('  1..8, 0 - Trimm selection');
  HL.Add('Change Format of Window');
  HL.Add('  l, p, s - Landscape, Portrait, Square');
  HL.Add('');
  HL.Add('Window-Info:');
  HL.Add(Format('  Initial-Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  HL.Add(Format('  Handle.Scale = %.1f', [Handle.Scale]));

  if HelpText <> nil then
    HelpText.Text := HL.Text;
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

procedure TFormMain.SetupText(T: TText; fs: single);
begin
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.SetupCombobox(CB: TComboBox);
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

procedure TFormMain.SetupListbox(LB: TListBox);
begin
  if LB = nil then
    Exit;

{$ifdef FMX}
  LB.ShowScrollBars := False;
  LB.StyleLookup := 'transparentlistboxstyle';
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
  MM.Parent := Self;
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := TScrollStyle.ssBoth;
{$endif}
end;

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  HintText.Name := 'HintText';
  SetupText(HintText);

  HelpText := TText.Create(Self);
  HelpText.Name := 'HelpText';
  SetupText(HelpText);

  ReportText := TText.Create(Self);
  ReportText.Name := 'ReportText';
  SetupText(ReportText);

  TrimmText := TText.Create(Self);
  TrimmText.Name := 'TrimmText';
  SetupText(TrimmText);

  SpeedPanel1 := TActionSpeedBarRG01.Create(Self);
  SpeedPanel1.Name := 'SpeedPanel1';
  SpeedPanel1.Parent := Self;
  SpeedPanel1.ShowHint := True;
  SpeedPanel1.Visible := False;
  SpeedPanel1.Opacity := OpacityValue;

  SpeedPanel2 := TActionSpeedBarRG03.Create(Self);
  SpeedPanel2.Name := 'SpeedPanel2';
  SpeedPanel2.Parent := Self;
  SpeedPanel2.ShowHint := True;
  SpeedPanel2.Visible := False;
  SpeedPanel2.Opacity := OpacityValue;

  SpeedPanel := SpeedPanel2;
  SpeedPanel.Visible := True;

  ParamListbox := TListbox.Create(Self);
  ParamListbox.Name := 'ParamListbox';
  ParamListbox.Parent := Self;
  ParamListbox.Opacity := OpacityValue;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Name := 'ReportListbox';
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;

  Image := TOriginalImage.Create(Self, BitmapWidth, BitmapHeight);
  Image.Name := 'Image';
  Image.Parent := Self;

  ComponentsCreated := True;
end;

procedure TFormMain.ToggleSpeedPanel;
begin
  SpeedPanel.Visible := False;

  if SpeedPanel = SpeedPanel1 then
    SpeedPanel := SpeedPanel2
  else
    SpeedPanel := SpeedPanel1;

  SpeedPanel.Visible := True;
  SpeedPanel.UpdateSpeedButtonEnabled;
  SpeedPanel.UpdateSpeedButtonDown;
end;

procedure TFormMain.LayoutSpeedPanel(SP: TActionSpeedBar);
begin
  SP.Anchors := [];
  SP.Position.X := 2 * Raster + Margin;
  SP.Position.Y := Raster + Margin;
  SP.Width := ClientWidth - 3 * Raster - 2 * Margin;
  SP.Height := SpeedPanelHeight;
  SP.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
  SP.UpdateLayout;
end;

procedure TFormMain.LayoutComponents;
begin
  if not ComponentsCreated then
    Exit;

  { ClientWidth and ClientHeight may still be at DesignTime Values, }
  { when called earlier than FormShow. }
  { Then it only 'works' if these values are big enough, }
  { so that computed values for Height and Width are > 0 }

  LayoutSpeedPanel(SpeedPanel1);
  LayoutSpeedPanel(SpeedPanel2);

  TrimmText.Position.X := Raster + Margin;
  TrimmText.Position.Y := 2 * Raster + Margin;
  TrimmText.Width := ListboxWidth;
  TrimmText.Height := 150;

  ParamListbox.Position.X := TrimmText.Position.X;
  ParamListbox.Position.Y := TrimmText.Position.Y + TrimmText.Height + Margin;
  ParamListbox.Width := ListboxWidth;
  ParamListbox.Height := 270;

  ReportListbox.Position.X := ParamListbox.Position.X;
  ReportListbox.Position.Y := ParamListbox.Position.Y + ParamListbox.Height + Margin;
  ReportListbox.Width := ParamListbox.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  ImagePositionX := TrimmText.Position.X + ListboxWidth + Margin;
  ImagePositionY := TrimmText.Position.Y;

  Image.Position.X := ImagePositionX;
  Image.Position.Y := ImagePositionY;
  Image.Width := ClientWidth - Image.Position.X - Raster - Margin;
  Image.Height := ClientHeight - Image.Position.Y - Raster - Margin;
  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
  ImagePositionX := Image.Position.X;
  ImagePositionY := Image.Position.Y;

  HintText.Position.X := ImagePositionX + 150;
  HintText.Position.Y := ImagePositionY;

  HelpText.Position.X := ImagePositionX + 150;
  HelpText.Position.Y := ImagePositionY + 30 + Margin;

  ReportText.Position.X := HelpText.Position.X;
  ReportText.Position.Y := HelpText.Position.Y;

  TextPositionX := ReportText.Position.X;
  TextPositionY := ReportText.Position.Y;
end;

procedure TFormMain.LineColorBtnClick(Sender: TObject);
begin
  RotaForm.WantLineColors := not RotaForm.WantLineColors;
  UpdateSpeedButtonDown;
  RotaForm.Draw;
end;

procedure TFormMain.HandleSegment(fa: Integer);
var
  b: Boolean;
begin
  b := RotaForm.GetChecked(fa);
  b := not b;
  RotaForm.SetChecked(fa, b);

  RotaForm.Draw;
end;

procedure TFormMain.ChartImageBtnClick(Sender: TObject);
begin
  ChartImage.Visible := not ChartImage.Visible;
  if ChartImage.Visible then
    ChartImage.BringToFront;
  if ChartImage.Visible then
    UpdateChartGraph;
end;

procedure TFormMain.SalingImageBtnClick(Sender: TObject);
begin
  SalingImage.Visible := not SalingImage.Visible;
  if SalingImage.Visible then
    SalingImage.BringToFront;
  if SalingImage.Visible then
    UpdateSalingGraph;
end;

procedure TFormMain.ControllerImageBtnClick(Sender: TObject);
begin
  ControllerImage.Visible := not ControllerImage.Visible;
  if ControllerImage.Visible then
    ControllerImage.BringToFront;
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
  SalingImage := TOriginalImage.Create(Self, 453, 220);
  SalingImage.Name := 'SalingImage';
  SalingImage.Parent := Self;
  SalingImage.HitTest := False;
  SalingImage.Visible := False;

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
    SalingGraph.SalingA := Round(Main.ParamValue[fpSalingA]);
    SalingGraph.SalingH := Round(Main.ParamValue[fpSalingH]);
    SalingGraph.SalingL := Round(Main.ParamValue[fpSalingL]);
    SalingGraph.Draw(TFigure.dtSalingDetail);
  end;
end;

procedure TFormMain.InitControllerGraph;
begin
  ControllerImage := TOriginalImage.Create(Self, 453, 220);
  ControllerImage.Name := 'ControllerImage';
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
begin
  if IsUp and ControllerImage.Visible then
  begin
    ControllerGraph.ControllerTyp := Rigg.ControllerTyp;
    ControllerGraph.ControllerPos := Round(Main.ParamValue[fpController]);
    ControllerGraph.ParamXE := Rigg.MastPositionE;
    ControllerGraph.ParamXE0 := Round(Rigg.rP.E0.X - Rigg.rP.D0.X);
    ControllerGraph.EdgePos := Round(Rigg.GSB.Find(fpController).Min);

    ControllerGraph.Draw(TFigure.dtController);
    ControllerImage.Repaint;
  end;
end;

procedure TFormMain.InitChartGraph;
begin
  ChartImage := TOriginalImage.Create(Self, 650, 400);
  ChartImage.Name := 'ChartImage';
  ChartImage.Parent := Self;
  ChartImage.HitTest := False;
  ChartImage.Visible := False;

  ChartGraph := TChartGraph.Create;
  ChartGraph.Image := ChartImage;

  UpdateChartGraph;
end;

procedure TFormMain.UpdateChartGraph;
begin
  if IsUp and ChartImage.Visible then
  begin
    ChartGraph.SuperCalc;
    ChartImage.Repaint;
  end;
end;

procedure TFormMain.LayoutImages;
var
  PosX: single;
  PosY: single;
begin
  if not ComponentsCreated then
    Exit;

  PosX := ClientWidth - (Raster + Margin + ControllerImage.Width);
  PosY := SpeedPanel.Position.Y + SpeedPanel.Height + Margin;
  if Screen.Height < 1000 then
    PosY := PosY + 100
  else
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
    Main.Param := Main.Text2Param(ParamListbox.Items[ParamListbox.ItemIndex]);
  ShowTrimm;
  UpdateControllerGraph;
  Main.FederText.CheckState;
end;

procedure TFormMain.InitParamListbox;
var
  rm: TMain;
  LI: TStrings;
  fp: TFederParam;
  s: string;

  procedure Add(fp: TFederParam);
  begin
    LI.AddObject(rm.Param2Text(fp), TObject(fp));
  end;
begin
  rm := Main;
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
    ParamCombo.Items.AddObject(Main.Param2Text(fp), TObject(fp));
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
    Main.Param := fp;
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
    Main.UpdateTrimmText(TL);
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
    cr.Tag := NativeInt(cla);
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
    T.Font.Size := 14;
    T.TextSettings.FontColor := Cardinal(cr.Tag);
  end;
end;

procedure TFormMain.SofortBtnClick(Sender: TObject);
begin
  Main.SofortBerechnen := not Main.SofortBerechnen;
end;

procedure TFormMain.GrauBtnClick(Sender: TObject);
begin
  Main.BtnGrauDown := not Main.BtnGrauDown;
end;

procedure TFormMain.BlauBtnClick(Sender: TObject);
begin
  Main.BtnBlauDown := not Main.BtnBlauDown;
end;

procedure TFormMain.MemoryBtnClick(Sender: TObject);
begin
  Main.MemoryBtnClick;
  UpdateReport;
end;

procedure TFormMain.MemoryRecallBtnClick(Sender: TObject);
begin
  Main.MemoryRecallBtnClick;
  ShowTrimm;
end;

procedure TFormMain.MultiBtnClick(Sender: TObject);
begin
  RotaForm.WantOverlayedRiggs := not RotaForm.WantOverlayedRiggs;
  Main.Draw;
end;

procedure TFormMain.BogenBtnClick(Sender: TObject);
begin
  Main.Bogen := not Main.Bogen;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  Main.Koppel := not Main.Koppel;
  if Sender <> nil then
    Main.FederText.CheckState;
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := MainVar.IsSandboxed;
    faToggleAllProps: result := AllProps;
    faToggleAllTags: result := ReportManager.XmlAllTags;

    faToggleHelp: result := HelpText.Visible;
    faToggleReport: result := ReportText.Visible;
    faToggleButtonReport: result := WantButtonReport;
    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
    faToggleSegmentF..faToggleSegmentA: result := RotaForm.GetChecked(fa);

    faToggleLineColor: result := RotaForm.WantLineColors;
    faToggleShowLegend: result := RotaForm.LegendItemChecked;

    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
    faToggleUseQuickSort: result := RotaForm.UseQuickSort;

    faRggBogen: result := Main.Bogen;
    faRggKoppel: result := Main.Koppel;

    faSofortBtn: result := Main.SofortBerechnen;
    faGrauBtn: result := Main.BtnGrauDown;
    faBlauBtn: result := Main.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;

    faToggleChartGraph: result := ChartImage.IsVisible;
    faToggleSalingGraph: result := SalingImage.IsVisible;
    faToggleControllerGraph: result := ControllerImage.IsVisible;
    faToggleMatrixText: result := RotaForm.MatrixItemChecked;

    faRotaForm1: result := RotaForm.Current = 1;
    faRotaForm2: result := RotaForm.Current = 2;
    faRotaForm3: result := RotaForm.Current = 3;
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

procedure TFormMain.InitScreenPos;
begin
  if FScale = 1.0 then
    InitScreenPos1
  else
    InitScreenPos2;
end;

procedure TFormMain.InitScreenPos1;
begin
  if (Screen.Width >= 1920) and (Screen.Height >= 1024) then
  begin
    { normal HD screen }
    Left := 100;
    Top := 30;
    Width := 1700;
    Height := 960;
    ReportMemoWidth := 480;
  end
  else
  begin
    Left := 20;
    Top := 30;
    Width := 1336;
    Height := 800;
    ReportMemoWidth := 320;
  end;
end;

procedure TFormMain.InitScreenPos2;
begin
  if (Screen.Width >= FScale * 1920) and (Screen.Height >= FScale * 1024) then
  begin
    Left := 100;
    Top := 30;
    Width := 1700;
    Height := 960;
    ReportMemoWidth := 480;
  end
  else
  begin
    { Tested on Microsoft Surface Tablet with FScale = 2.0 }
    Left := 20;
    Top := 30;
    Width := 1336;
    Height := 800;
    ReportMemoWidth := 320;
  end;
end;

procedure TFormMain.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Parent := self; // needed for Alt-Tab
    FormMemo.Memo.Lines.Clear;
    Main.WriteTrimmItem;
    FormMemo.Memo.Text := Main.FLText;
    CheckFormBounds(FormMemo);
  end;
  FormMemo.Visible := True;
  FormMemo.Show; //needed on Mac
end;

procedure TFormMain.ActionsBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
    FormAction.Parent := self; // needed for Alt-Tab
    CheckFormBounds(FormAction);
  end;
  FormAction.Visible := True;
  FormAction.Show; //needed on Mac
end;

procedure TFormMain.DrawingsBtnClick(Sender: TObject);
begin
  if not Assigned(FormDrawing) then
  begin
    FormDrawing := TFormDrawing.Create(nil);
    FormDrawing.Parent := self; // needed for Alt-Tab
    CheckFormBounds(FormDrawing);
  end;
  FormDrawing.Visible := True;
  FormDrawing.Show; //needed on Mac
end;

procedure TFormMain.ConfigBtnClick(Sender: TObject);
begin
  if FormConfig = nil then
  begin
    FormConfig := TFormConfig.Create(Application);
    FormConfig.Parent := nil;
    FormConfig.Init(Rigg);
  end;

  { Istwerte in GSB aktualisieren für aktuelle Werte in Optionform }
  Rigg.UpdateGSB;
  FormConfig.ShowModal;
  if FormConfig.ModalResult = mrOK then
  begin
    Rigg.UpdateGlieder; { neue GSB Werte --> neue Integerwerte }
    Rigg.Reset; { neue Integerwerte --> neue Gleitkommawerte }
    Main.UpdateGetriebe;
    UpdateReport;
  end;
end;

procedure TFormMain.TrimmTabBtnClick(Sender: TObject);
begin
  if not Assigned(FormTrimmTab) then
  begin
    FormTrimmTab := TFormTrimmTab.Create(Application);
    FormTrimmTab.Parent := nil;
    FormTrimmTab.Init(Rigg);
  end;

  FormTrimmTab.ShowModal;
  if FormTrimmTab.ModalResult = mrOK then
  begin
//    Main.RggMain.UpdateGetriebe;
    UpdateReport;
  end;
end;

procedure TFormMain.DestroyForms;
begin
  if FormAction <> nil then
  begin
    FormAction.DisposeOf;
    FormAction := nil;
  end;
  if FormDrawing <> nil then
  begin
    FormDrawing.DisposeOf;
    FormDrawing := nil;
  end;
  if FormMemo <> nil then
  begin
    FormMemo.DisposeOf;
    FormMemo := nil;
  end;
end;

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel1 <> nil then
    SpeedPanel1.InitSpeedButtons;
  if SpeedPanel2 <> nil then
    SpeedPanel2.InitSpeedButtons;
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

  if ReportLabel <> nil then
    ReportLabel.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claReport;

  HintText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claHintText;
  ReportText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claReportText;
  HelpText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claHelpText;
  TrimmText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claTrimmText;

  SetupListboxItems(ParamListbox, SpeedPanel.SpeedColorScheme.claParamList);
  SetupListboxItems(ReportListbox, SpeedPanel.SpeedColorScheme.claReportList);

  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateControllerGraph;

  SalingGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateSalingGraph;

  RotaForm.DarkMode := SpeedPanel.DarkMode;
end;

procedure TFormMain.SuperSimpleBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := False;
  Main.GraphRadio := gSimple;
end;

procedure TFormMain.SuperNormalBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := False;
  Main.GraphRadio := gNormal;
end;

procedure TFormMain.SuperGrauBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gGrau;
end;

procedure TFormMain.SuperBlauBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gBlau;
end;

procedure TFormMain.SuperMultiBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := False;
  RotaForm.WantOverlayedRiggs := True;
  Main.GraphRadio := gMulti;
end;

procedure TFormMain.SuperDisplayBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := True;
  RotaForm.WantOverlayedRiggs := False;
  RotaForm.UseQuickSort := False;
  Main.GraphRadio := gDisplay;
end;

procedure TFormMain.SuperQuickBtnClick(Sender: TObject);
begin
  RotaForm.UseDisplayList := True;
  RotaForm.WantOverlayedRiggs := False;
  RotaForm.UseQuickSort := True;
  Main.GraphRadio := gQuick;
end;

procedure TFormMain.ShowZOrder;
var
  i: Integer;
  o: TFMXObject;
  c: TControl;
  ML: TStrings;
begin
  ML := TStringList.Create;
  for i := 0 to Self.ChildrenCount-1 do
  begin
    o := Self.Children.Items[i];
    if o is TControl then
    begin
      c := o as TControl;
      ML.Add(Format('%2d - %s: %s', [i, c.Name, c.ClassName]));
    end;
  end;
  HelpText.Text := ML.Text;
  ML.Free;

  HelpText.Visible := True;
  ReportText.Visible := False;
end;

procedure TFormMain.ShowHelp;
begin
  HelpText.Text := HL.Text;
  HelpText.Visible := True;
  ReportText.Visible := False;
end;

end.
