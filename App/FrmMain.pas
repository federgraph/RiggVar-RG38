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

{.$define DriverTestSupported}
{.$define ResizeEndSupported}

{.$define WantInitTimer}
{$define WantRotaForm1}
{$define WantRotaForm2}
{.$define WantRotaForm3}
{.$define WantDeviceCheck}
{.$define WantResizeEnd}
{.$define WantMenu}
{$define WantFormConfig}
{.$define WantCombos}
{$define WantListboxes}

uses
  RiggVar.App.Model,
  RiggVar.App.Strings,
  RiggVar.RG.View,
  RiggVar.FB.SpeedColor,
  RiggVar.FB.SpeedBar,
  RiggVar.RG.Report,
  RiggVar.RG.Rota,
  RiggVar.FD.Image,
{$ifdef WantMenu}
  RiggVar.FederModel.Menu,
{$endif}
  RiggVar.DT.Ctrls,
  RiggVar.Chart.Graph,
  RiggVar.RG.Types,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Messaging,
  FMX.Platform,
  FMX.Graphics,
  FMX.Types,
{$ifdef WantRotaForm3}
  FMX.Types3D,
  FMX.Viewport3D,
{$endif}
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
{$ifdef WantMenu}
  FMX.Menus,
{$endif}
  FMX.Listbox,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Surfaces,
  FMX.Layouts,
  FMX.Controls.Presentation;

type
  TFormMain = class(TForm, IFormMain)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    FScale: single;
    FWantResizeNormalizing: Boolean;
    DefaultCaption: string;
    FormShown: Boolean;
    InitTimerCalled: Boolean;
    DrawingNeeded: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure DoOrientationChanged(const Sender: TObject; const M: TMessage);
    procedure RegisterForAppEvents;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;

    procedure FormCreate2(Sender: TObject);
    procedure FormDestroy2(Sender: TObject);
    procedure HandleShowHint(Sender: TObject);
    procedure Flash(s: string);
    procedure Log(s: string);
    procedure Reset;
    procedure InitDebugInfo;
    procedure InitZOrderInfo;
    procedure ShowHelpText(fa: Integer);
    function GetCanShowMemo: Boolean;
    procedure ToggleLanguage;
{$ifdef WantListboxes}
    procedure UpdateParamListboxText;
    procedure UpdateReportListboxText;
{$endif}
    procedure InitScreenPos;
    procedure InitScreenPos1;
    procedure InitScreenPos2;
    procedure UpdateFormat(w, h: Integer);
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
  protected
    HL: TStringList;
    RL: TStrings;
    TL: TStrings;
{$ifdef WantListboxes}
    procedure InitParamListbox;
{$endif}
  public
    FWantButtonReport: Boolean;
    procedure ShowTrimm;
    procedure ShowTrimmData;
    procedure UpdateReport;
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
{$ifdef WantListboxes}
    ParamListbox: TListBox;
    ReportListbox: TListBox;
{$endif}
    ReportLabel: TText;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;
{$ifdef WantListboxes}
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure UpdateItemIndexParamsLB;
    procedure ParamListboxChange(Sender: TObject);
    procedure ReportListboxChange(Sender: TObject);
{$endif}
    procedure UpdateHintText(fa: Integer);
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
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
    procedure ListboxItemStyleLookup(Sender: TObject);
  public
    Raster: Integer;
    Margin: Integer;
    SpeedPanelMargin: Integer;
    ListboxWidth: Integer;
    ReportMemoWidth: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    SpeedPanel01: TActionSpeedBar;
    SpeedPanel02: TActionSpeedBar;
    SpeedPanel03: TActionSpeedBar;
    SpeedPanel04: TActionSpeedBar;
    SpeedPanel05: TActionSpeedBar;
    SpeedColorScheme: TSpeedColorScheme;
    procedure InitSpeedButtons;
    procedure LayoutSpeedPanel(SP: TActionSpeedBar);
    procedure ToggleSpeedPanel;
    procedure ToggleButtonSize;
    procedure SwapSpeedPanel(Value: Integer);
    procedure SwapRota(Value: Integer);
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
    procedure HullBtnClick(Sender: TObject);

    procedure SuperSimpleBtnClick(Sender: TObject);
    procedure SuperNormalBtnClick(Sender: TObject);
    procedure SuperGrauBtnClick(Sender: TObject);
    procedure SuperBlauBtnClick(Sender: TObject);
    procedure SuperMultiBtnClick(Sender: TObject);
    procedure SuperDisplayBtnClick(Sender: TObject);
    procedure SuperQuickBtnClick(Sender: TObject);
{$ifdef WantMenu}
  public
    MainMenu: TMainMenu;
    MenuItem: TMenuItem;
    FederMenu: TFederMenu;
    function GetMainMenuVisible: Boolean;
    procedure SetMainMenuVisible(const Value: Boolean);
    procedure PopulateMenu;
    property MainMenuVisible: Boolean read GetMainMenuVisible write SetMainMenuVisible;
{$endif}
  public
    NewControlSize: TControlSize;
    InitTimer: TTimer;
    ExitSizeMoveCounter: Integer;
    ClearStateCounter: Integer;

{$ifdef WantRotaForm3}
    Viewport: TViewport3D;
{$endif}

    procedure HandleClearStateException;
    procedure FormActivate(Sender: TObject);
    procedure FormResizeEnd(Sender: TObject);
    procedure DoOnResize;
    procedure DoOnResizeEnd;
    procedure InitTimerTimer(Sender: TObject);
    procedure InitWantOnResize;
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    function GetEnabled(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
    procedure RotaFormRotateZ(Delta: single);
    procedure RotaFormZoom(Delta: single);
  public
    RotaForm: TRotaForm;
    procedure HandleSegment(fa: Integer);
  public
    Rigg: IRigg;
    ReportManager: TRggReportManager;
    FViewPoint: TViewPoint;
    procedure UpdateOnParamValueChanged;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    procedure SetViewPoint(const Value: TViewPoint);
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property IsUp: Boolean read GetIsUp write SetIsUp;
    property CanShowMemo: Boolean read GetCanShowMemo;
  public
    Image: TOriginalImage;
    ImagePositionX: single;
    ImagePositionY: single;
    BitmapWidth: Integer;
    BitmapHeight: Integer;
    TextPositionX: single;
    TextPositionY: single;
    procedure UpdateFederText;
    procedure CenterRotaForm;
    procedure ToggleAllText;
  public
    SalingImage: TOriginalImage;
    SalingGraph: TSalingGraph;
    ControllerImage: TOriginalImage;
    ControllerGraph: TSalingGraph;
    ChartImage: TOriginalImage;
    ChartGraph: TChartGraph;
    procedure DoOnUpdateChart(Sender: TObject);
    procedure InitSalingGraph;
    procedure InitControllerGraph;
    procedure InitChartGraph;
    procedure UpdateSalingGraph;
    procedure UpdateControllerGraph;
    procedure UpdateChartGraph;
    procedure LayoutImages;
    procedure ConfigUpdatedOK;
  protected
    procedure DestroyForms;
    procedure ShowDiagramC;
    procedure ShowDiagramE;
    procedure ShowDiagramQ;
    procedure MemoBtnClick(Sender: TObject);
    procedure ActionsBtnClick(Sender: TObject);
    procedure DrawingsBtnClick(Sender: TObject);
    procedure ChartBtnClick(Sender: TObject);
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
{$ifdef WantDeviceCheck}
  RiggVar.FG.DriverTest,
{$endif}
  FrmDrawing,
  FrmConfig,
  FrmTrimmTab,
  FrmChart,
  FrmDiagramC,
  FrmDiagramE,
  FrmDiagramQ,
  RiggVar.Util.AppUtils,
  RiggVar.RG.Main,
  RiggVar.RG.Speed01,
  RiggVar.RG.Speed02,
  RiggVar.RG.Speed03,
  RiggVar.RG.Speed04,
  RiggVar.RG.Speed05,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

{$ifdef OSX}
const
  HelpCaptionText = 'Trimm420 - press ? for help';
  ApplicationTitleText = 'Trimm420';
{$else}
const
  HelpCaptionText = 'RG38 - press ? for help';
  ApplicationTitleText = 'RG38';
{$endif}

{ TFormMain }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
{$ifdef WantRotaForm3}
  if (E is EContext3DException) and (E.Message.StartsWith('ClearState')) then
    HandleClearStateException;
  if (E is EContext3DException) then
{$endif}
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if IsUp then
  begin
    if Main <> nil then
    begin
      if not MainVar.AppIsClosing then
      begin

        if IsUp then
        begin
{$ifdef WantRotaForm3}
          RotaForm.DoOnIdle;
{$endif}
        end;

        if DrawingNeeded then
        begin
          DrawingNeeded := False;
          Main.Draw;
        end;

      end;
    end;
  end;
  Done := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';

  MainVar.WantLocalizedText := True;
  MainVar.WantGermanText := True;
  MainVar.WantFederText := True;
  MainVar.ClientWidth := Round(ClientWidth);
  MainVar.ClientHeight := Round(ClientHeight);

  FormMain := self;

  SpeedColorScheme := TSpeedColorScheme.Create;
  SpeedColorScheme.InitDark;
  TActionSpeedBar.SpeedColorScheme := SpeedColorScheme;

  NewControlSize := TControlSize.Create(TSizeF.Create(0, 0));

  InitWantOnResize;

{$ifdef WantInitTimer}
  InitTimer := TTimer.Create(Self);
  InitTimer.Interval := 1100;
  InitTimer.OnTimer := InitTimerTimer; { will call FormCreate2 and FormShow }
  InitTimer.Enabled := True;
  Exit;
{$else}
  FormCreate2(Sender);
{$endif}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, DoOrientationChanged);

  MainVar.AppIsClosing := True;

  FormDestroy2(Sender);

{$ifdef WantDeviceCheck}
  if DeviceCheck <> nil then
  begin
    DeviceCheck.Free;
    DeviceCheck := nil;
  end;
{$endif}

  NewControlSize.Free;
  SpeedColorScheme.Free;
{$ifdef WantMenu}
  FederMenu.Free;
{$endif}

  TModelFactory.ReleaseIfAppropriate(Rigg);
end;

procedure TFormMain.FormCreate2(Sender: TObject);
begin
{$ifdef MSWINDOWS}
{$if defined(Debug) and not defined(WantRotaForm3) }
  ReportMemoryLeaksOnShutdown := True; // there is a small systematic Memory leak when using RotaForm3
{$endif}
{$endif}

  FScale := 1.0;
{$ifdef MSWINDOWS}
  FScale := Handle.Scale;
{$endif}
  MainVar.Scale := FScale;

  Application.OnException := ApplicationEventsException;
  Application.OnIdle := ApplicationEventsIdle;

  InitScreenPos;

  SpeedPanelMargin := 2;
  Margin := 10;
  Raster := MainVar.Raster;
  MainVar.ScaledRaster := Raster;

  SpeedPanelHeight := Raster;
  ListboxWidth := 200;

  BitmapWidth := Ceil(Screen.WorkAreaWidth);
  BitmapHeight := Ceil(Screen.WorkAreaHeight);

{$ifdef WantMenu}
  FederMenu := TFederMenu.Create;
  { Create MainMenu before setting ClientHeight ? }
  MainMenu := TMainMenu.Create(self);
  MainMenu.Parent := self;
  MenuItem := TMenuItem.Create(self);
  MainMenu.AddObject(MenuItem);
{$endif}

  CreateComponents;

{$ifdef WantListboxes}
  SetupListbox(ParamListbox);
  SetupListbox(ReportListbox);
{$endif}

  Rigg := TModelFactory.NewRigg;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg, Self, Self);
  Main.Logger.Verbose := True;

  RotaForm := TRotaForm.Create;
  RotaForm.Image := Image;
{$ifdef WantRotaForm3}
  RotaForm.Viewport := Viewport;
{$endif}

  RotaForm.Init;

  RotaForm.SwapRota(1);

{$ifdef WantListboxes}
  { Params }
  if ParamListbox <> nil then
  begin
    InitParamListbox;
    ParamListbox.OnChange := ParamListboxChange;
    ParamListbox.ItemIndex := ParamListbox.Items.IndexOf('Vorstag');
  end;
{$endif}

  { Reports }
  HL := TStringList.Create;
  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  if Main.IsPhone then
    ReportManager.CurrentReport := rgTrimmText
  else
    ReportManager.CurrentReport := rgDiffText;

{$ifdef WantListboxes}
  if ReportListbox <> nil then
  begin
    ReportManager.InitLB(ReportListbox.Items);
    ReportListbox.OnChange := ReportListboxChange;
    ReportListbox.ItemIndex := ReportListbox.Items.IndexOf(
    ReportManager.GetReportCaption(ReportManager.CurrentReport));
  end;
{$endif}

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
  end;

  Caption := HelpCaptionText;

  InitSalingGraph;
  InitControllerGraph;
  InitChartGraph;

  Main.IsUp := True;
  Main.Draw;
  Main.MemoryBtnClick;

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateColorScheme;

  SwapSpeedPanel(RotaForm.Current);

  Main.InitDefaultData;
  CenterRotaForm;
  Main.FixPoint := ooD0;
  Main.HullVisible := False;
  Main.OnUpdateChart := DoOnUpdateChart;
  Main.FederTextCheckState;

  Self.OnMouseWheel := FormMouseWheel;
  Self.OnResize := FormResize;
  Self.OnKeyUp := FormKeyUp;

  if not MainVar.IsSandBoxed then
    Main.ReadTrimmFile0;

{$ifdef WantRotaForm3}
  Self.OnActivate := FormActivate;
{$endif}

Application.OnIdle := ApplicationEventsIdle;

{$ifdef WantResizeEnd}
  Self.OnResizeEnd := FormResizeEnd;
{$endif}

{$if defined(MACOS) and (CompilerVersion < 34.0) }
  { OnKeyUp does not work well on Mac, RSP-2766 }
  { fixed in Sidney 10.4 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead, in Tokyo 10.2 and Rio 10.3 }
  OnKeyDown := FormKeyUp;
{$endif}

{$ifdef WantMenu}
  PopulateMenu;
{$endif}

  TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, DoOrientationChanged);
  RegisterForAppEvents;
end;

procedure TFormMain.FormDestroy2(Sender: TObject);
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
{$ifdef WantRotaForm3}
  if RotaForm.Current = 3 then
    Exit;

  if Viewport.IsFocused then
  begin
{$ifdef WantListboxes}
    ParamListbox.SetFocus;
{$endif}
    Exit;
  end;
{$endif}

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
  DoOnResizeEnd;
end;

procedure TFormMain.UpdateHintText(fa: Integer);
begin
  if (HintText <> nil) and HintText.Visible then
  begin
{$if defined(IOS) or defined(Android)}
  if fa <> faNoop then
    HintText.Text := Main.ActionHandler.GetCaption(fa) + #10 + Main.ActionHandler.GetShortcutString(fa)
  else
    HintText.Text := '';
{$endif}
  end;
end;

procedure TFormMain.UpdateItemIndexParams;
begin
{$ifdef WantListboxes}
  UpdateItemIndexParamsLB;
{$endif}
  ShowTrimm;
end;

{$ifdef WantListboxes}
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
{$endif}

procedure TFormMain.UpdateItemIndexReports;
{$ifdef WantListboxes}
var
  ii: Integer;
  ij: Integer;
{$endif}
begin
{$ifdef WantListboxes}
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
{$endif}
end;

procedure TFormMain.UpdateItemIndexTrimms;
begin
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
{$ifdef WantRotaForm3}
  { MouseWheel messages will be handled by TViewport3D }
  if RotaForm.Current = 3 then
    Exit;
{$endif}
  Main.DoMouseWheel(Shift, WheelDelta);
  Handled := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if Main = nil then
    Exit;

  Main.Logger.Info('in FormShow');
  DoOnResizeEnd;

  if (InitTimer <> nil) and not InitTimerCalled then
    Exit;

  if not FormShown then
  begin
    FormShown := True;

    Main.FederText1.UpdateToolSet(0);
    Main.FederText2.UpdateToolSet(0);
    ShowTrimm;

    { ClientHeight is now available }
    LayoutComponents;
    LayoutImages;

{$ifdef WantListboxes}
    SetupListboxItems(ParamListbox, claAqua);
    SetupListboxItems(ReportListbox, claAquamarine);
{$endif}

    Image.Align := TAlignLayout.Client;

    UpdateReport;

    RotaForm.IsUp := True;
    ViewPoint := vp3D;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if (Main <> nil) and Main.IsUp then
  begin
    MainVar.Scale := Handle.Scale;
    DoOnResize;
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    UpdateFederText;
  end;

  if FormShown then
  begin
    CheckSpaceForMemo;
    CheckSpaceForImages;
{$ifndef WantResizeEnd}
    if RotaForm.Current <> 3 then
    CenterRotaForm;
{$endif}
    UpdateReport;
    SpeedPanel.UpdateLayout;
  end;
end;

procedure TFormMain.CheckSpaceForListbox;
begin
  if not FormShown then
    Exit;
{$ifdef WantListboxes}
  ReportListbox.Visible := ClientHeight > 910;
{$endif}
end;

procedure TFormMain.CheckSpaceForMemo;
begin
  if not FormShown then
    Exit;
  if not ComponentsCreated then
    Exit;

  if not CanShowMemo then
  begin
    { Phone screen }
    if RotaForm.LegendItemChecked then
    begin
      RotaForm.LegendBtnClick(nil);
    end;

    SpeedPanel.Visible := False;

    TrimmText.Visible := False;

{$ifdef WantListboxes}
    ParamListbox.Visible := False;
    if ReportListbox <> nil then
      ReportListbox.Visible := False;
{$endif}

    HintText.Position.X := 2 * Raster + Margin;
    HintText.Position.Y := 1 * Raster + Margin;

    HelpText.Position.X := Raster + Margin;
    HelpText.Position.Y := 2 * Raster + Margin;

    ReportText.Position.X := Raster + Margin;
    ReportText.Position.Y := 2 * Raster + Margin;
  end
  else
  begin
    { Tablet screen }
    SpeedPanel.Visible := True;

    TrimmText.Visible := True;

{$ifdef WantListboxes}
    ParamListbox.Visible := True;
    ReportListbox.Visible := True;
{$endif}

    HintText.Position.X := TextPositionX;
    HintText.Position.Y := 2 * Raster + Margin;

    HelpText.Position.X := TextPositionX;
    HelpText.Position.Y := TextPositionY;

    ReportText.Position.X := TextPositionX;
    ReportText.Position.Y := TextPositionY;
  end;
end;

procedure TFormMain.CheckSpaceForImages;
begin
  if not ComponentsCreated then
    Exit;

  { At application startup FormResize is called several times,
    but always before FormShow is called. }

  { ClientWidth and ClientHeight are not yet available when starting up.
    ClientHeight is available when FormShow is called. }

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

  Main.FederTextCheckState;
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
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight * 4 / 3);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth * 3 / 4);
    Left := 0;
  end;
  Flash('Landscape');
  DoOnResizeEnd;
end;

procedure TFormMain.GotoPortrait;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight * 3 / 4);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth * 4 / 3);
    Left := 0;
    Top := 0;
  end;
  Flash('Portrait');
  DoOnResizeEnd;
end;

procedure TFormMain.GotoSquare;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight);
    ClientWidth := Round(ClientHeight);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth);
    ClientHeight := Round(ClientWidth);
    Left := 0
  end;
  Flash('Square');
  DoOnResizeEnd;
end;

procedure TFormMain.Flash(s: string);
begin
  Caption := s;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
{$if defined(MSWINDOWS) or defined(OSX)}
  HintText.Text := Application.Hint;
{$endif}
end;

procedure TFormMain.HandleAction(fa: Integer);
begin
  case fa of
    faToggleLanguage: ToggleLanguage;

    faToggleAllText: ToggleAllText;
    faToggleSpeedPanel: ToggleSpeedPanel;
    faToggleButtonSize: ToggleButtonSize;

    faToggleHelp:
    begin
      HelpText.Visible := not HelpText.Visible;
      ReportText.Visible := False;
      if HelpText.Visible then
        ShowHelpText(faShowHelpText);
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
      HelpText.Visible := False;
      ReportText.Visible := True;
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
    end;

    faToggleSortedRota: RotaForm.HandleAction(fa);
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
    faRggHull: HullBtnClick(nil);

    faSofortBtn: SofortBtnClick(nil);
    faGrauBtn: GrauBtnClick(nil);
    faBlauBtn: BlauBtnClick(nil);
    faMultiBtn: MultiBtnClick(nil);

    faSuperSimple: SuperSimpleBtnClick(nil);
    faSuperNormal: SuperNormalBtnClick(nil);
    faSuperGrau: SuperGrauBtnClick(nil);
    faSuperBlau: SuperBlauBtnClick(nil);
    faSuperMulti: SuperMultiBtnClick(nil);
    faSuperDisplay: SuperDisplayBtnClick(nil);
    faSuperQuick: SuperQuickBtnClick(nil);

    faShowMemo: MemoBtnClick(nil);
    faShowActions: ActionsBtnClick(nil);
    faShowDrawings: DrawingsBtnClick(nil);
    faShowConfig: ConfigBtnClick(nil);
    faShowTrimmTab: TrimmTabBtnClick(nil);

    faShowDiagC: ShowDiagramC;
    faShowDiagE: ShowDiagramE;
    faShowDiagQ: ShowDiagramQ;

    faToggleSandboxed: ;
    faToggleAllProps: MainVar.AllProps := not MainVar.AllProps;
    faToggleAllTags: MainVar.AllTags := not MainVar.AllTags;

    faRotaForm1: SwapRota(1);
    faRotaForm2: SwapRota(2);
    faRotaForm3: SwapRota(3);

    faReset,
    faResetPosition,
    faResetRotation,
    faResetZoom: RotaForm.HandleAction(fa);

    faSalingTypFest,
    faSalingTypDrehbar,
    faSalingTypOhne,
    faSalingTypOhneStarr: UpdateReport;

{$ifdef WantRotaForm3}
    faToggleViewType: RotaForm.HandleAction(fa);
{$endif}

    faPan:
    begin
      Main.SetParameter(faPan);
      ShowTrimm;
    end;

    faShowZOrderInfo,
    faShowNormalKeyInfo,
    faShowSpecialKeyInfo,
    faShowDebugInfo,
    faShowInfoText,
    faShowHelpText: ShowHelpText(fa);

    else
    begin
      { do nothing }
    end;

  end;
end;

function TFormMain.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkF12: result := faMemeGotoSquare;
//    vkC: result := faCopyTrimmItem;
//    vkV: result := faPasteTrimmItem;
    vkEscape:
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

    'b': fa := faBiegung;
    'B': fa := faFixpointB0;

    'c': fa := faMastfallF0C;
    'C': fa := faFixpointC0;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faMastfallF0F;
    'F': fa := faFixpointF0;

    'g': fa := faMastfallVorlauf;

    'h': fa := faSalingH;
    'H': fa := faToggleHelp;

    'i': fa := faWheelRight;
    'I': fa := faWheelLeft;

    'j': fa := faWheelUp;
    'J': fa := faWheelDown;

    'k': ;
    'K': fa := faRggKoppel;

    'l': fa := faToggleShowLegend;
    'L': fa := faMemeGotoLandscape;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': fa := faShowNormalKeyInfo;

    'o': fa := faWoben;

    'p': fa := faPan;
    'P': fa := faMemeGotoPortrait;

    'q': fa := faToggleAllText;

    'r': fa := faToggleReport;
    'R': fa := faReadTrimmFile;

    's': fa := faShowSpecialKeyInfo;
    'S': fa := faMemeGotoSquare;

    't': fa := faToggleDarkMode;
    'T': fa := faToggleSpeedPanel;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;

    'w': fa := faWante;

    'z': fa := faShowInfoText;
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

    '!': fa := faShowNormalKeyInfo;
    '"': fa := faShowSpecialKeyInfo;
    '§': fa := faShowInfoText;
    '$': fa := faShowDebugInfo;
    '=': fa := faShowZOrderInfo;
    '?': fa := faShowHelpText;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': fa := faActionPage4;

    ',': fa := faRotaForm1;
    '.': fa := faRotaForm2;
    '-': fa := faRotaForm3;

    ';': fa := faRotaForm1;
    ':': fa := faRotaForm2;
    '_': fa := faRotaForm3;

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

function TFormMain.GetOpenFileName(dn, fn: string): string;
begin
  Main.Logger.Info('in TFormMain.GetOpenFileName');

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
  begin
    result := '';
    Main.Logger.Info('  FileName is empty string');
  end;
end;

function TFormMain.GetSaveFileName(dn, fn: string): string;
begin
  Main.Logger.Info('in TFormMain.GetSaveFileName');
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
  begin
    result := '';
    Main.Logger.Info('  FileName is empty string');
  end;
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
  T.Font.Family := TAppUtils.GetMonspacedFontFamilyName;

  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
  if MM = nil then
    Exit;

  MM.ControlType := TControlType.Styled;
  MM.StyledSettings := [];
  MM.ShowScrollBars := True;
  MM.TextSettings.Font.Family := TAppUtils.GetMonspacedFontFamilyName;
  MM.TextSettings.Font.Size := 14;
  MM.TextSettings.FontColor := claBlue;
end;

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

{$ifdef WantRotaForm3}
  Viewport := TViewport3D.Create(self);
  Viewport.Name := 'Viewport';
  Viewport.Parent := self;
  Viewport.UsingDesignCamera := False;
  Viewport.Visible := False;
{$endif}

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

  Image := TOriginalImage.Create(Self, BitmapWidth, BitmapHeight);
  Image.WantWorkArea := True;
  Image.Name := 'Image';
  Image.Parent := Self;

  SpeedPanel01 := TActionSpeedBarRG01.Create(Self);
  SpeedPanel01.Name := 'SpeedPanel01';
  SpeedPanel01.Parent := Self;
  SpeedPanel01.ShowHint := True;
  SpeedPanel01.Visible := False;
  SpeedPanel01.Opacity := OpacityValue;

  SpeedPanel02 := TActionSpeedBarRG02.Create(Self);
  SpeedPanel02.Name := 'SpeedPanel02';
  SpeedPanel02.Parent := Self;
  SpeedPanel02.ShowHint := True;
  SpeedPanel02.Visible := False;
  SpeedPanel02.Opacity := OpacityValue;

  SpeedPanel03 := TActionSpeedBarRG03.Create(Self);
  SpeedPanel03.Name := 'SpeedPanel03';
  SpeedPanel03.Parent := Self;
  SpeedPanel03.ShowHint := True;
  SpeedPanel03.Visible := False;
  SpeedPanel03.Opacity := OpacityValue;

  SpeedPanel04 := TActionSpeedBarRG04.Create(Self);
  SpeedPanel04.Name := 'SpeedPanel04';
  SpeedPanel04.Parent := Self;
  SpeedPanel04.ShowHint := True;
  SpeedPanel04.Visible := False;
  SpeedPanel04.Opacity := OpacityValue;

  SpeedPanel05 := TActionSpeedBarRG05.Create(Self);
  SpeedPanel05.Name := 'SpeedPanel05';
  SpeedPanel05.Parent := Self;
  SpeedPanel05.ShowHint := True;
  SpeedPanel05.Visible := False;
  SpeedPanel05.Opacity := OpacityValue;

  SpeedPanel := SpeedPanel01;

{$ifdef WantListboxes}
  ParamListbox := TListbox.Create(Self);
  ParamListbox.Name := 'ParamListbox';
  ParamListbox.Parent := Self;
  ParamListbox.Opacity := OpacityValue;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Name := 'ReportListbox';
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;
{$endif}

  ComponentsCreated := True;
end;

procedure TFormMain.ToggleSpeedPanel;
begin
  if Main.IsPhone then
  begin
    SpeedPanel.Visible := False;
    Exit;
  end;

  if SpeedPanel = SpeedPanel01 then
    SwapSpeedPanel(RotaForm.Current)
  else
    SwapSpeedPanel(0);
end;

procedure TFormMain.SwapSpeedPanel(Value: Integer);
begin
  SpeedPanel.Visible := False;

  if Main.IsPhone then
    Exit;

  if Main.IsPortrait then
    Exit;

    case Value of
      1: SpeedPanel := SpeedPanel03;
      2: SpeedPanel := SpeedPanel04;
      3: SpeedPanel := SpeedPanel05;
      else
        SpeedPanel := SpeedPanel01;
    end;

  SpeedPanel.Width := ClientWidth - 3 * Raster - SpeedPanelMargin;
  SpeedPanel.Visible := True;
  SpeedPanel.UpdateLayout;;
  SpeedPanel.DarkMode := MainVar.ColorScheme.IsDark;
  SpeedPanel.UpdateColor;
end;

procedure TFormMain.LayoutSpeedPanel(SP: TActionSpeedBar);
begin
  SP.Anchors := [];
  SP.Position.X := 2 * Raster + SpeedPanelMargin;
  SP.Position.Y := Raster + Margin;
  SP.Width := ClientWidth - 3 * Raster - 2 * SpeedPanelMargin;
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

  LayoutSpeedPanel(SpeedPanel01);
  LayoutSpeedPanel(SpeedPanel02);
  LayoutSpeedPanel(SpeedPanel03);
  LayoutSpeedPanel(SpeedPanel04);
  LayoutSpeedPanel(SpeedPanel05);

  TrimmText.Position.X := Raster + Margin;
  TrimmText.Position.Y := 2 * Raster + Margin;
  TrimmText.Width := ListboxWidth;
  TrimmText.Height := 150;

{$ifdef WantListboxes}
  ParamListbox.Position.X := TrimmText.Position.X;
  ParamListbox.Position.Y := TrimmText.Position.Y + TrimmText.Height + Margin;
  ParamListbox.Width := ListboxWidth;
  ParamListbox.Height := 270;

  ReportListbox.Position.X := ParamListbox.Position.X;
  ReportListbox.Position.Y := ParamListbox.Position.Y + ParamListbox.Height + Margin;
  ReportListbox.Width := ParamListbox.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Raster - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];
{$endif}

  ImagePositionX := TrimmText.Position.X + ListboxWidth + Margin;
  ImagePositionY := TrimmText.Position.Y;

  HintText.Position.X := Raster + ListboxWidth + 2 * Margin;
  HintText.Position.Y := 2 * Raster + Margin;

  HelpText.Position.X := HintText.Position.X;
  HelpText.Position.Y := HintText.Position.Y + 60;

  ReportText.Position.X := HelpText.Position.X;
  ReportText.Position.Y := HelpText.Position.Y;

  { remember text postions for tablet }
  TextPositionX := ReportText.Position.X;
  TextPositionY := ReportText.Position.Y;

  SpeedPanel.Width := ClientWidth - 3 * Raster - SpeedPanelMargin;
end;

procedure TFormMain.LineColorBtnClick(Sender: TObject);
begin
  RotaForm.WantLineColors := not RotaForm.WantLineColors;
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
    ControllerGraph.ParamXE0 := Round(Rigg.GetPoint3D(ooE0).X - Rigg.GetPoint3D(ooD0).X);
    ControllerGraph.EdgePos := Round(Rigg.RggFA.Find(fpController).Min);

    ControllerGraph.Draw(TFigure.dtController);
  end;
end;

procedure TFormMain.InitChartGraph;
begin
  ChartImage := TOriginalImage.Create(Self, 650, 400);
  ChartImage.Name := 'ChartImage';
  ChartImage.Parent := Self;
  ChartImage.HitTest := False;
  ChartImage.Visible := False;

  ChartGraph := TChartGraph.Create(Rigg);
  ChartGraph.Image := ChartImage;

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

{$if defined(pfAndroid) or defined(iOS)}
  ChartImage.Position.X := ImagePositionX + 200;
{$else}
  ChartImage.Position.X := ImagePositionX + 400;
{$endif}
  ChartImage.Position.Y := ImagePositionY + 0;
end;

procedure TFormMain.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  RotaForm.ViewPoint := Value;
end;

{$ifdef WantListboxes}
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
    Main.FederTextCheckState;
  end;
end;

procedure TFormMain.ParamListboxChange(Sender: TObject);
begin
  if ParamListbox.ItemIndex <> -1 then
    Main.Param := Main.Text2Param(ParamListbox.Items[ParamListbox.ItemIndex]);
  ShowTrimm;
  UpdateControllerGraph;
  Main.FederTextCheckState;
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
{$endif}

procedure TFormMain.ShowTrimmData;
begin
  RL.BeginUpdate;
  try
    RL.Clear;
    Main.CurrentTrimm.WantAll := MainVar.AllProps;
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
    UpdateFederText;
  end;
  UpdateReport;
end;

procedure TFormMain.SetupListbox(LB: TListBox);
begin
  if LB = nil then
    Exit;

  LB.ShowScrollBars := False;
  LB.StyleLookup := 'transparentlistboxstyle';

{$if defined(IOS) or defined(Android)}
  LB.ItemHeight := 24;
{$endif}
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
{$ifdef MSWINDOWS}
    T.Font.Size := 14;
{$endif}
{$ifdef OSX}
    T.Font.Size := 14;
{$endif}
{$ifdef Android}
    T.Font.Size := 16;
{$endif}
{$ifdef IOS}
    T.Font.Size := 16;
{$endif}

    T.TextSettings.FontColor := Cardinal(cr.Tag);
  end;
end;

procedure TFormMain.SofortBtnClick(Sender: TObject);
begin
  Main.SofortBerechnen := not Main.SofortBerechnen;
  if Sender <> nil then
    Main.FederTextCheckState;
  UpdateReport;
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
    Main.FederTextCheckState;
end;

procedure TFormMain.KoppelBtnClick(Sender: TObject);
begin
  Main.Koppel := not Main.Koppel;
  if Sender <> nil then
    Main.FederTextCheckState;
end;

procedure TFormMain.HullBtnClick(Sender: TObject);
begin
  Main.HullVisible := not Main.HullVisible;
  if Sender <> nil then
    Main.FederTextCheckState;
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleSandboxed: result := MainVar.IsSandboxed;
    faToggleAllProps: result := MainVar.AllProps;
    faToggleAllTags: result := MainVar.AllTags;
    faToggleButtonSize: result := SpeedPanel.BigMode;

    faToggleHelp: result := HelpText.Visible;
    faToggleReport: result := ReportText.Visible;
    faToggleButtonReport: result := WantButtonReport;
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);
    faToggleSegmentF..faToggleSegmentA: result := RotaForm.GetChecked(fa);

    faToggleLineColor: result := RotaForm.WantLineColors;
    faToggleShowLegend: result := RotaForm.LegendItemChecked;

    faToggleUseDisplayList: result := RotaForm.UseDisplayList;
    faToggleUseQuickSort: result := RotaForm.UseQuickSort;
    faToggleSortedRota:result := RotaForm.GetChecked(fa);

    faRggBogen: result := Main.Bogen;
    faRggKoppel: result := Main.Koppel;

    faSofortBtn: result := Main.SofortBerechnen;
    faGrauBtn: result := Main.BtnGrauDown;
    faBlauBtn: result := Main.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;

    faToggleDataText: result := ShowDataText;
    faToggleDiffText: result := ShowDiffText;
    faToggleTrimmText: result := ShowTrimmText;

    faChartRect..faChartReset: result := ChartGraph.GetChecked(fa);
    faToggleChartGraph: result := ChartImage.IsVisible;
    faToggleSalingGraph: result := SalingImage.IsVisible;
    faToggleControllerGraph: result := ControllerImage.IsVisible;
    faToggleMatrixText: result := RotaForm.MatrixItemChecked;

    faRotaForm1: result := RotaForm.Current = 1;
    faRotaForm2: result := RotaForm.Current = 2;
    faRotaForm3: result := RotaForm.Current = 3;
  end;
end;

function TFormMain.GetEnabled(fa: TFederAction): Boolean;
begin
  { ToDo: Make sure that RotaFormX  is enabled, when implemented }
  case fa of
    faRotaForm1: result := True;
    faRotaForm2: result := True;
    faRotaForm3: result := False;

    faToggleSpeedPanel: result := not Main.IsPhone;

    faSalingTypDrehbar,
    faSalingTypOhne,
    faSalingTypOhneStarr: result := not Main.Demo;

    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderS: result := (RotaForm.Current = 3) or ((RotaForm.Current = 1) and RotaForm.UseDisplayList);

    faWantRenderE: result := RotaForm.Current = 3;

    faToggleUseDisplayList,
    faToggleUseQuickSort,
    faToggleShowLegend: result := RotaForm.Current = 2;

    faToggleMatrixText: result := RotaForm.Current = 1;

    faToggleSegmentF,
    faToggleSegmentR,
    faToggleSegmentS,
    faToggleSegmentM,
    faToggleSegmentV,
    faToggleSegmentW,
    faToggleSegmentA: result := RotaForm.Current = 2;

    faToggleSegmentC: result := (RotaForm.Current = 2) and (Main.Param = fpController);

    faGrauBtn,
    faBlauBtn,
    faMultiBtn: result := RotaForm.Current = 1;

    faRggKoppel: result := RotaForm.Current <> 3;
    faRggHull: result := RotaForm.Current <> 2;

    faToggleSandboxed: result := False;

    else
      result := True;
  end;
end;

procedure TFormMain.CheckFormBounds(AForm: TForm);
begin
  if Screen.Height <= 768 then
    AForm.Top := 0;
  if Screen.Width <= 768 then
    AForm.Left := 0;
  if AForm.Left + AForm.Width > Screen.Width then
    AForm.Width := Round(Screen.Width) - AForm.Left - 20;
  if AForm.Top + AForm.Height > Round(Screen.Height) then
    AForm.Height := Round(Screen.Width) - AForm.Top - 20;
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
    { Tested on Microsoft Surface Tablet }
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

procedure TFormMain.ChartBtnClick(Sender: TObject);
begin
  if not Assigned(FormDrawing) then
  begin
    FormChart := TFormChart.Create(nil);
    FormChart.Parent := self; // needed for Alt-Tab
    CheckFormBounds(FormChart);
  end;
  FormChart.Visible := True;
  FormChart.Show; // needed on Mac
end;

procedure TFormMain.ShowDiagramC;
begin
  if not Assigned(FormDiagramC) then
  begin
    FormDiagramC := TFormDiagramC.Create(nil);
    FormDiagramC.Parent := self; // needed for Alt-Tab
    FormDiagramC.ChartModel := ChartGraph;
    ChartGraph.OnActionHandled := FormDiagramC.UpdateUI;

    if not ChartImage.Visible then
    begin
//      Main.FederText.ActionPage := 9;
      ChartImageBtnClick(nil);
    end;
  end;

  FormDiagramC.Visible := True;
  FormDiagramC.Show; // needed on Mac
end;

procedure TFormMain.ShowDiagramE;
begin
  if not Assigned(FormDiagramE) then
  begin
    FormDiagramE := TFormDiagramE.Create(nil);
    FormDiagramE.Parent := self; // needed for Alt-Tab
  end;
  FormDiagramE.Visible := True;
  FormDiagramE.Show; // needed on Mac
end;

procedure TFormMain.ShowDiagramQ;
begin
  if not Assigned(FormDiagramQ) then
  begin
    FormDiagramQ := TFormDiagramQ.Create(nil);
    FormDiagramQ.Parent := self; // needed for Alt-Tab
  end;
  FormDiagramQ.Visible := True;
  FormDiagramQ.Show; // needed on Mac
end;

procedure TFormMain.ConfigBtnClick(Sender: TObject);
begin
{$if defined(MSWINDOWS) or defined(OSX)}
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
    ConfigUpdatedOK;
  end;
{$endif}

{$if defined(IOS) or defined(Android)}
  if FormConfig = nil then
  begin
    FormConfig := TFormConfig.Create(nil);
    FormConfig.Parent := self;
    FormConfig.Init(Rigg);
  end;

  { Istwerte in GSB aktualisieren für aktuelle Werte in Optionform }
  Rigg.UpdateGSB;
  FormConfig.Show;
{$endif}
end;

procedure TFormMain.ConfigUpdatedOK;
begin
  { when Form is not shown modally }

//  if FormConfig.ModalResult = mrOK then
//  begin
    Rigg.UpdateGlieder; { neue GSB Werte --> neue Integerwerte }
    Rigg.Reset; { neue Integerwerte --> neue Gleitkommawerte }
    Main.UpdateGraph(False);
    UpdateReport;
//  end;
end;

procedure TFormMain.TrimmTabBtnClick(Sender: TObject);
begin
{$if defined(MSWINDOWS) or defined(OSX)}
  if not Assigned(FormTrimmTab) then
  begin
    FormTrimmTab := TFormTrimmTab.Create(Application);
    FormTrimmTab.Parent := nil;
    FormTrimmTab.Init(Rigg);
  end;

  FormTrimmTab.ShowModal;
  if FormTrimmTab.ModalResult = mrOK then
  begin
    UpdateReport;
  end;
{$endif}

{$if defined(IOS) or defined(Android)}
  if not Assigned(FormTrimmTab) then
  begin
    FormTrimmTab := TFormTrimmTab.Create(nil);
    FormTrimmTab.Parent := self;
    FormTrimmTab.Init(Rigg);
  end;

  FormTrimmTab.Show;
//  if FormTrimmTab.ModalResult = mrOK then
//  begin
//    UpdateReport; // done from within FormTrimmTab
//  end;
{$endif}
end;

procedure TFormMain.DestroyForms;
begin
  { nothing here because Parent of forms was set to this one. }
end;

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel01 <> nil then
    SpeedPanel01.InitSpeedButtons;

  if SpeedPanel02 <> nil then
    SpeedPanel02.InitSpeedButtons;

  if SpeedPanel03 <> nil then
    SpeedPanel03.InitSpeedButtons;

  if SpeedPanel04 <> nil then
    SpeedPanel04.InitSpeedButtons;

  if SpeedPanel05 <> nil then
    SpeedPanel05.InitSpeedButtons;
end;

procedure TFormMain.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  Self.Fill.Color := MainVar.ColorScheme.claBackground;
  RotaForm.BackgroundColor := MainVar.ColorScheme.claBackground;

  if MainVar.ColorScheme.IsDark then
    SpeedColorScheme.InitDark
  else
    SpeedColorScheme.InitLight;

  SpeedPanel.DarkMode := MainVar.ColorScheme.IsDark;
  SpeedPanel.UpdateColor;

  if ReportLabel <> nil then
    ReportLabel.TextSettings.FontColor := SpeedColorScheme.claReport;

  HintText.TextSettings.FontColor := MainVar.ColorScheme.claHint;
  ReportText.TextSettings.FontColor := SpeedColorScheme.claReportText;
  HelpText.TextSettings.FontColor := SpeedColorScheme.claHelpText;
  TrimmText.TextSettings.FontColor := SpeedColorScheme.claTrimmText;

{$ifdef WantListboxes}
  SetupListboxItems(ParamListbox, SpeedColorScheme.claParamList);
  SetupListboxItems(ReportListbox, SpeedColorScheme.claReportList);
{$endif}

  ControllerGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateControllerGraph;

  SalingGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  UpdateSalingGraph;

  RotaForm.DarkMode := SpeedColorScheme.IsDark;
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

procedure TFormMain.InitZOrderInfo;
var
  i: Integer;
  o: TFMXObject;
  c: TControl;
begin
  for i := 0 to Self.ChildrenCount-1 do
  begin
    o := Self.Children.Items[i];
    if o is TControl then
    begin
      c := o as TControl;
      HL.Add(Format('%2d - %s: %s', [i, c.Name, c.ClassName]));
    end;
  end;
end;

procedure TFormMain.InitDebugInfo;
begin
  HL.Add('Window-Info:');
  HL.Add(Format('  Initial-Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  HL.Add(Format('  Handle.Scale = %.1f', [Handle.Scale]));
end;

procedure TFormMain.ShowHelpText(fa: Integer);
begin
  HL.Clear;

  case fa of
    faShowHelpText: Main.FederBinding.InitSplashText(HL);
    faShowInfoText: Main.FederBinding.InitInfoText(HL);
    faShowNormalKeyInfo: Main.FederBinding.InitNormalKeyInfo(HL);
    faShowSpecialKeyInfo: Main.FederBinding.InitSpecialKeyInfo(HL);
    faShowDebugInfo: InitDebugInfo;
    faShowZOrderInfo: InitZOrderInfo;
  end;

  HelpText.Text := HL.Text;
  HelpText.Visible := True;
  ReportText.Visible := False;
end;

procedure TFormMain.SwapRota(Value: Integer);
begin
  RotaForm.SwapRota(Value);
  RotaForm.BackgroundColor := MainVar.ColorScheme.claBackground;
  RotaForm.DarkMode := MainVar.ColorScheme.IsDark;
  SwapSpeedPanel(RotaForm.Current);
end;

procedure TFormMain.DoOnUpdateChart(Sender: TObject);
begin
  if (ChartGraph <> nil) and (Main.Param = fpAPW)  then
  begin
    ChartGraph.APWidth := Round(Main.CurrentValue);
    ChartGraph.UpdateXMinMax;
  end;
end;

procedure TFormMain.UpdateFederText;
begin
  Main.FederTextUpdateCaption;
end;

procedure TFormMain.CenterRotaForm;
begin
  RotaForm.InitPosition(Width, Height, 0, 0);
  if FormShown then
    RotaForm.Draw;
end;

procedure TFormMain.ToggleButtonSize;
begin
  SpeedPanel.ToggleBigMode;
  LayoutComponents;
  CheckSpaceForMemo;
  CheckSpaceForImages;
end;

procedure TFormMain.ToggleAllText;
var
  b: Boolean;
  c: Boolean;
begin
  if not Main.FederText.Visible then
    Exit;

  if Main.FederText = Main.FederTextPhone then
    Exit;

  { determine the current situation }
  b := ReportText.Visible or HelpText.Visible;

  { deal with ReportText and HelpText,
    toggle text visibility
    make sure that ReportText is shown when switching AllText on }
  ReportText.Visible := not b;
  HelpText.Visible := False;

  { now deal with SpeedPanel, Listboxes and TrimmText, if any }
  c := ReportText.Visible; // go in sync with text visibility
  if not CanShowMemo then
  begin
    { force off
        when on phone target
        or when not enough space on tablet }
    c := False;
  end;
  SpeedPanel.Visible := c;
  TrimmText.Visible := c;
{$ifdef WantListboxes}
  ParamListbox.Visible := c;
  ReportListbox.Visible := c;
{$endif}

  { test this out on the desktop with keyboard shortcuts for
    faToggleAllText (q),
    faToggleReport (r),
    faToggleHelp (H)
  }
end;

function TFormMain.GetCanShowMemo: Boolean;
begin
  result := True;

  if (ClientWidth < 900) then
    result := False;

  if (ClientHeight < 700) then
    result := False;

  if Main.IsPhone then
    result := False;
end;

procedure TFormMain.InitWantOnResize;
begin
  MainVar.WantOnResize := True;

{$ifdef MSWINDOWS}
  { see RSP-18851 }
{$ifdef ResizeEndSupported}
{$ifdef WantResizeEnd}
  MainVar.WantOnResize := False;
{$endif}
{$endif}
{$endif}
end;

procedure TFormMain.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled := False;
  InitTimerCalled := True;
  if not IsUp then
  begin
    FormCreate2(nil);
    FormShow(nil);
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
{$ifdef WantRotaForm3}
  if IsUp then
  begin
    Viewport.SetFocus;
  end;
{$endif}
{$ifdef MSWINDOWS}
{$ifdef WantDeviceCheck}
  if DeviceCheck <> nil then
    DeviceCheck.Viewport := Viewport;
{$endif}
{$endif}
end;

procedure TFormMain.DoOnResize;
begin
  if MainVar.WantOnResize then
    DoOnResizeEnd;
end;

procedure TFormMain.FormResizeEnd(Sender: TObject);
begin
  Inc(ExitSizeMoveCounter);
  DoOnResizeEnd;
end;

procedure TFormMain.DoOnResizeEnd;
begin
  if IsUp then
  begin
    NewControlSize.Width := ClientWidth;
    NewControlSize.Height := ClientHeight;
{$ifdef WantRotaForm3}
    if not (Viewport.Align = TAlignLayout.Client) then
      Viewport.Size := NewControlSize;
{$endif}
    if not (Image.Align = TAlignLayout.Client) then
      Image.Size := NewControlSize;

    MainVar.ClientWidth := ClientWidth;
    MainVar.ClientHeight := ClientHeight;
    Main.UpdateTouch;
    CenterRotaForm;
    RotaForm.DoOnResizeEnd;
  end;
end;

procedure TFormMain.HandleClearStateException;
begin
{$ifdef WantRotaForm3}
  Inc(ClearStateCounter);
  Caption := Format('%d - %d', [ClearStateCounter, Main.ResizeCounter]);
{$endif}
end;

{$ifdef WantMenu}
procedure TFormMain.PopulateMenu;
begin
  { need dummy MenuItem to set correct client height from beginning }
  if MenuItem.Parent = MainMenu then
  begin
    MainMenu.RemoveObject(MenuItem);
  end;

  if Assigned(MainMenu) and Assigned(Main) then
  begin
    FederMenu.InitMainMenu(MainMenu);
  end;
end;

function TFormMain.GetMainMenuVisible: Boolean;
begin
  result := MainMenu <> nil;
end;

procedure TFormMain.SetMainMenuVisible(const Value: Boolean);
begin
  if Value and not Assigned(MainMenu) then
  begin
    MainMenu := TMainMenu.Create(self);
    MainMenu.Tag := -1;
    MainMenu.Parent := self;
    PopulateMenu;
    MainMenu.Tag := 1;
    MainMenu.RecreateOSMenu;
  end
  else if Assigned(MainMenu) and not Value then
  begin
    MainMenu.Free;
    MainMenu := nil;
    Main.UpdateTouch;
  end;
end;
{$endif}

procedure TFormMain.RotaFormRotateZ(Delta: single);
begin
  RotaForm.RotateZ(Delta);
end;

procedure TFormMain.RotaFormZoom(Delta: single);
begin
  RotaForm.Zoom(Delta);
end;

procedure TFormMain.ToggleLanguage;
begin
  MainVar.WantGermanText := not MainVar.WantGermanText;
  RggLocalizedStrings.UpdateText;
{$ifdef WantMenu}
  FederMenu.UpdateText(MainMenu);
{$endif}
  SpeedPanel.UpdateText;
  Main.CycleToolSet(0);
{$ifdef WantListboxes}
  UpdateReportListboxText;
  UpdateParamListboxText;
{$endif}
  Main.ParamCaption := Main.Param2Text(Main.Param);
  ShowTrimm;

{$ifdef WantFormConfig}
  if (FormConfig <> nil) and not (FormConfig.Visible) then
    FreeAndNil(FormConfig);
{$endif}
end;

{$ifdef WantListboxes}
procedure TFormMain.UpdateParamListboxText;
var
  ii: Integer;
begin
  if ParamListbox <> nil then
  begin
    ii := ParamListbox.ItemIndex;
    ParamListbox.OnChange := nil;
    InitParamListbox;
    ParamListbox.ItemIndex := ii;
    ParamListbox.OnChange := ParamListboxChange;
    SetupListboxItems(ParamListbox, claAqua);
  end;
end;

procedure TFormMain.UpdateReportListboxText;
var
  ii: Integer;
begin
  if ReportListbox <> nil then
  begin
    ii := ReportListbox.ItemIndex;
    ReportListbox.OnChange := nil;
    ReportListbox.Clear;
    ReportManager.InitLB(ReportListbox.Items);
    ReportListbox.ItemIndex := ii;
    ReportListbox.OnChange := ReportListboxChange;
    SetupListboxItems(ReportListbox, claAquamarine);
  end;
end;
{$endif}

procedure TFormMain.DoOrientationChanged(const Sender: TObject; const M: TMessage);
var
  screenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, screenService) then
  begin
    DrawingNeeded := True;
  end;
end;

procedure TFormMain.RegisterForAppEvents;
var aFMXApplicationEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent)
  else
    Log('Application Event Service is not supported.');
end;

function TFormMain.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching:
    begin
      Log('Finished Launching');
      DrawingNeeded := True;
    end;
    TApplicationEvent.BecameActive:
    begin
//      if Assigned(Main) then
//        Main.UpdateLED;
      Log('Became Active');
      DrawingNeeded := True;
    end;
    TApplicationEvent.WillBecomeInactive:
    begin
//      MainVar.ShouldRecycleSocket := True;
      Log('Will Become Inactive');
    end;
    TApplicationEvent.EnteredBackground:
    begin
//      MainVar.ShouldRecycleSocket := True;
      Log('Entered Background');
    end;
    TApplicationEvent.WillBecomeForeground:
    begin
      Log('Will Become Foreground');
    end;
    TApplicationEvent.WillTerminate:
    begin
      Log('Will Terminate');
    end;
    TApplicationEvent.LowMemory:
    begin
      Log('Low Memory');
    end;
    TApplicationEvent.TimeChange:
    begin
      Log('Time Change');
    end;
    TApplicationEvent.OpenURL:
    begin
      Log('Open URL');
    end;
  end;
  result := True;
end;

procedure TFormMain.Log(s: string);
begin
  if IsUp then
  begin
    Main.Logger.Info(s);
  end;
end;

end.
