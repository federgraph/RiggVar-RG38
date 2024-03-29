﻿unit RiggVar.RG.Main;

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

{$ifndef MinimalRG}
{$define WantColorScheme}
{$define WantFederText}
{$define WantFederAction}
{$define WantFederKeyboard}
{$define WantOnUpdateChart}
{$define WantOnUpdateGraph}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  System.UIConsts,
  FMX.Types,
  RiggVar.App.Model,
  RiggVar.App.Strings,
  RiggVar.RG.Inter,
  RiggVar.RG.View,
  RiggVar.RG.Data,
  RiggVar.RG.Def,
  RiggVar.RG.Track,
  RiggVar.RG.Graph,
  RiggVar.RG.Scroll,
  RiggVar.RG.Types,
  RiggVar.RG.Calc,
  RiggVar.RG.Doc,
  RiggVar.FB.Classes,
  RiggVar.FB.ActionConst,
{$ifdef WantFederAction}
  RiggVar.FB.Action,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionTable,
  RiggVar.FB.ActionKeys,
  RiggVar.FB.ActionTest,
  RiggVar.FederModel.Action,
  RiggVar.FederModel.ActionList,
  RiggVar.FederModel.Binding,
{$endif}
{$ifdef WantFederText}
  RiggVar.FB.ActionMap,
  RiggVar.FB.TextBase,
  RiggVar.FederModel.TouchBase,
  RiggVar.FederModel.Touch,
  RiggVar.FederModel.TouchPhone,
{$endif}
  RiggVar.Util.Logger;

type
  TFederAction = Integer;

  TRggMain = class
  private
    InitDataOK: Boolean;

    FParam: TFederParam;
    FAction: TFederAction;
    FTrimm: Integer;
    FFixPoint: TRiggPoint;
    FViewPoint: TViewPoint;

    BiegungGF: single;
    BiegungGFDiff: single;

    TML: TStrings;

    FSofortBerechnen: Boolean;
    FBtnGrauDown: Boolean;
    FBtnBlauDown: Boolean;

    FGraphRadio: TGraphRadio;

    FKorrigiert: Boolean;
    FBogen: Boolean;
    FKoppel: Boolean;
    FHullVisible: Boolean;
    FDemo: Boolean;

    FTouch: Integer;

{$ifdef WantOnUpdateGraph}
    FOnUpdateGraph: TNotifyEvent;
{$endif}

{$ifdef WantOnUpdateChart}
    FOnUpdateChart: TNotifyEvent;
{$endif}

    ModelInput: TRggModelInput;
    ModelOutput: TRggModelOutput;
    ModelError: TRggModelError;

    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);
    procedure DoReadTrimmFile(fn: string);
    function GetTrimmFilePath: string;
    function GetIsRggParam: Boolean;
    function LogFileNameToInfoFormatted(t1, t2, fn: string): boolean;
    procedure PasteTrimm;

    procedure InitRaster;
    function GetIsDesktop: Boolean;
    function GetIsLandscape: Boolean;
    function GetIsPortrait: Boolean;
    function GetColorScheme: Integer;
    procedure SetColorScheme(const Value: Integer);
    function GetIsPhone: Boolean;
    procedure SetTouch(const Value: Integer);

{$ifdef WantFederText}
    function GetFederText: TFederTouchBase;
    procedure InitFederText(ft: TFederTouch0);
{$endif}

    function FormatValue(Value: single): string;
    procedure DoBiegungGF;

    procedure SetParamValue(idx: TFederParam; const Value: single);
    function GetParamValue(idx: TFederParam): single;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);
    function GetCurrentValue: single;
    procedure SetCurrentValue(const Value: single);
    function GetParamValueString(fp: TFederParam): string;
    function GetParamValueStringDiff(fp: TFederParam): string;
    function GetMastfall: string;
    procedure SetupTrackBarForRgg;
    procedure AL(A: string; fp: TFederParam);
    procedure BL(A: string; C: string);
    function GetHullVisible: Boolean;
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure UpdateEAR(Value: single);
    procedure UpdateEAH(Value: single);
    procedure SetSuperRadio(const Value: TGraphRadio);
    procedure SetKorrigiert(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetKoppel(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetDemo(const Value: Boolean);

    function GetCurrentTrimm: TRggData;
    procedure SetTrimm(const Value: Integer);
    procedure SetTrimmNoChange(const Value: Integer);
    procedure InitTrimmData;
    function GetBigStep: single;
    function GetSmallStep: single;
    procedure TrackBarChange(Sender: TObject);
    procedure RggSpecialDoOnTrackBarChange;
    procedure SetParam(const Value: TFederParam);

    procedure InternalDraw;
  public
    IsUp: Boolean;
    Rigg: IRigg; // injected via constructor
    MainParent: TFmxObject;
    MainView: IFormMain;
    StrokeRigg: IStrokeRigg; // injected
    FL: TStringList;
    Logger: TLogger;
    RggTrackbar: TFederTrackbar;

    RggData: TRggData; // temp object for data transfer

    { slot used as reference for diffing }
    Trimm0: TRggData;

    { user data slots }
    Trimm1: TRggData; // selected with button T1
    Trimm2: TRggData; // selected with button T2
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;

    { example data slots }
    Trimm7: TRggData; // 420
    Trimm8: TRggData; // Logo

    FixPunkt: TPoint3D;

    MinValCaption: string;
    MaxValCaption: string;
    IstValCaption: string;
    ParamCaption: string;

    RefCtrl: TTrimmControls;
    RefModelInput: TRggModelInput;

    InitialFixPoint: TRiggPoint;

{$ifdef WantFederAction}
    ActionHandler: IFederActionHandler;
    ActionHelper: TActionHelper;
    ActionList: TRggActionList;
    ActionGroupList: TActionGroupList;
    ActionTest: TActionTest;
{$endif}

{$ifdef WantFederText}
    ActionMap1: TActionMap;
    ActionMap2: TActionMap;
    FederText1: TFederTouch;
    FederText2: TFederTouchPhone;
{$endif}

{$ifdef WantFederKeyboard}
    FederKeyboard: TFederKeyboard;
    FederBinding: TFederBinding;
{$endif}
    BackgroundLock: Boolean;

    CurrentRotaForm: Integer;

    ReportCounter: Integer;
    ResizeCounter: Integer;

    UseTimedDrawing: Boolean;

    constructor Create(ARigg: IRigg; AMainView: IFormMain; AMainParent: TFMXObject);
    destructor Destroy; override;

    function GetFLText: string;
    procedure CopyText;

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    function GetTrimmItem(i: Integer): TRggData;
    function GetTrimmItemReport(ReportID: Integer): string;
    function GetTrimmItemReportData: string;
    function GetTrimmItemReportJson: string;
    function GetTrimmItemReportShort: string;
    function GetTrimmItemReportLong: string;

    procedure Reset;
    procedure UpdateGraph(CurrentChanged: Boolean);
    procedure DoOnModelEvent(Sender: TObject; Data: TRggModelOutput; Error: TRggModelError);
    procedure UpdateStrokeRigg;

    procedure InitDefaultData;
    procedure Init420;
    procedure InitLogo;
    procedure InitWithDefaultDoc(AWantLogoData: Boolean; TargetSlot: Integer);
    procedure DoAfterInitDefault(ATrimmSlot: Integer);
    procedure InitSalingTyp(fa: Integer);

    procedure MemoryBtnClick;
    procedure MemoryRecallBtnClick;

    procedure Draw;

    procedure DebugBiegungGF(ML: TStrings);
    procedure UpdateColumnC(ML: TStrings);
    procedure UpdateColumnD(ML: TStrings);
    procedure UpdateTrimmText(ML: TStrings);
    procedure UpdateJsonText(ML: TStrings);
    procedure UpdateDataText(ML: TStrings);
    procedure UpdateDiffText(ML: TStrings);

    function Param2Text(P: TFederParam): string;
    function Text2Param(T: string): TFederParam;

    procedure ToggleRenderOption(fa: TFederAction);
    procedure SetParameter(fa: TFederAction);

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: TFederAction): Boolean;
    function GetEnabled(fa: TFederAction): Boolean;
    procedure FederTextCheckState;
    procedure FederTextUpdateParent;
    procedure FederTextUpdateCaption;
    procedure CollectShortcuts(fa: Integer; ML: TStrings);

    procedure InitText;
{$ifdef WantFederText}
    procedure CycleToolSet(i: Integer);
    function IsTouchBtnEnabled(ABtn: TTouchBtn): Boolean;
{$endif}
    procedure FrameVisibilityChanged;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);
    procedure DoTouchbarTop(Delta: single);

    procedure CycleColorSchemeM;
    procedure CycleColorSchemeP;
    procedure ToggleDarkMode;
    procedure ToggleButtonSize;

    procedure InitTouch;
    procedure UpdateTouch;

    procedure UpdateOnParamValueChanged;

    procedure PlusOne;
    procedure PlusTen;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
    procedure DoWheel(Delta: single);
    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure WriteTrimmItem;
    procedure WriteTrimmFile;

    procedure CopyAndPaste;
    procedure CopyTrimmFile;
    procedure CopyTrimmItem;
    procedure PasteTrimmItem;

    procedure UpdateTrimm0;
    procedure ReadTrimmFile0;
    procedure ReadTrimmFile;
    procedure SaveTrimmFile;
    procedure ReadTrimmFileAuto;
    procedure SaveTrimmFileAuto;

    procedure ReadText(ML: TStrings);

    procedure DropTargetDropped(fn: string);
    procedure DoReport;
    procedure DoCleanReport;
    procedure ShowDebugData;
    procedure LogFileNameToInfo(fn: string);

    property FLText: string read GetFLText;
    property Param: TFederParam read FParam write SetParam;

    property CurrentTrimm: TRggData read GetCurrentTrimm;
    property TrimmNoChange: Integer read FTrimm write SetTrimmNoChange;
    property Trimm: Integer read FTrimm write SetTrimm;

    property TrimmData: string read GetTrimmItemReportData;
    property TrimmJson: string read GetTrimmItemReportJson;
    property TrimmShort: string read GetTrimmItemReportShort;
    property TrimmLong: string read GetTrimmItemReportLong;

    property Action: TFederAction read FAction;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property ParamValue[index: TFederParam]: single read GetParamValue write SetParamValue;
    property ParamValueString[index: TFederParam]: string read GetParamValueString;
    property ParamValueStringDiff[index: TFederParam]: string read GetParamValueStringDiff;
    property CurrentValue: single read GetCurrentValue write SetCurrentValue;
    property Mastfall: string read GetMastfall;

    property Bogen: Boolean read FBogen write SetBogen;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property HullVisible: Boolean read GetHullVisible write SetHullVisible;
    property Demo: Boolean read FDemo write SetDemo;

    property Korrigiert: Boolean read FKorrigiert write SetKorrigiert;
    property SofortBerechnenNoChange: Boolean read FSofortBerechnen write FSofortBerechnen;
    property SofortBerechnen: Boolean read FSofortBerechnen write SetSofortBerechnen;
    property BtnGrauDown: Boolean read FBtnGrauDown write SetBtnGrauDown;
    property BtnBlauDown: Boolean read FBtnBlauDown write SetBtnBlauDown;

    property GraphRadio: TGraphRadio read FGraphRadio write SetSuperRadio;

{$ifdef WantOnUpdateGraph}
    property OnUpdateGraph: TNotifyEvent read FOnUpdateGraph write FOnUpdateGraph;
{$endif}
{$ifdef WantOnUpdateChart}
    property OnUpdateChart: TNotifyEvent read FOnUpdateChart write FOnUpdateChart;
{$endif}

    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;

    property IsRggParam: Boolean read GetIsRggParam;

    property IsDesktop: Boolean read GetIsDesktop;
    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;

    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
    property Touch: Integer read FTouch write SetTouch;

{$ifdef WantFederKeyboard}
    property Keyboard: TFederKeyboard read FederKeyboard;
{$endif}
{$ifdef WantFederText}
    property FederText: TFederTouchBase read GetFederText;
    property FederTextTablet: TFederTouch read FederText1;
    property FederTextPhone: TFederTouchPhone read FederText2;
    property ActionMapTablet: TActionMap read ActionMap1;
    property ActionMapPhone: TActionMap read ActionMap2;
{$endif}
  end;

implementation

uses
  System.Rtti,
  FMX.PlatForm,
  FMX.Layouts,
  RiggVar.App.Main,
{$ifdef WantFederText}
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet,
{$endif}
{$ifdef WantFederKeyboard}
  RiggVar.FederModel.Keyboard01,
{$endif}
  RiggVar.Util.AppUtils;

const
  tfs = '%-3s %s %8s %6s';

  ParamsRange = [
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

  ReportsRange = [faReportNone .. faReportReadme];
  TrimmsRange = [faTrimm0 .. faLogo];

{ TRggMain }

constructor TRggMain.Create(ARigg: IRigg; AMainView: IFormMain; AMainParent: TFmxObject);
begin
  MainParent := AMainParent;
  MainView := AMainView;
  Rigg := ARigg;
  Rigg.ControllerTyp := ctOhne;
  Rigg.ModelEvent := DoOnModelEvent;

  Main := self;

  { this should not be necessary, beause it will be injected in a moment }
  StrokeRigg := TDummyStrokeRigg.Create(Rigg);

  FDemo := False;
  FParam := fpVorstag;
  FFixPoint := ooD;
  FKorrigiert := True;
  FSofortBerechnen := False;
  FBtnGrauDown := True;
  FBtnBlauDown := False;
  FBogen := True;
  FKoppel := False;
  FGraphRadio := gSimple;

  InitialFixPoint := ooD;

  FL := TStringList.Create;
  Logger := TLogger.Create;

  RggData := TRggData.Create;
  RggData.Name := 'fd';

  RggTrackbar := TFederTrackbar.Create;
  RggTrackbar.OnChange := TrackBarChange;

  Rigg.InitFactArray;

  Trimm0 := TRggData.Create;
  Trimm0.Name := 'T0';
  Trimm7 := TRggData.Create;
  Trimm7.Name := '420';
  Trimm8 := TRggData.Create;
  Trimm8.Name := 'Logo';

  Trimm1 := TRggData.Create;
  Trimm2 := TRggData.Create;
  Trimm3 := TRggData.Create;
  Trimm4 := TRggData.Create;
  Trimm5 := TRggData.Create;
  Trimm6 := TRggData.Create;

  InitTrimmData;

{$ifdef WantFederAction}
  ActionList := TRggActionList.Create(nil);
  ActionGroupList := TActionGroupList.Create;
  ActionTest := TActionTest.Create;
{$endif}

  InitRaster;

{$ifdef WantFederText}
  ActionMap1 := TActionMapTablet.Create;
  ActionMap2 := TActionMapPhone.Create;

  TTouchBtn.WantHint := True;
  FederText1 := TFederTouch.Create(nil);
  FederText1.Name := 'FederText1';
  FederText2 := TFederTouchPhone.Create(nil);
  FederText2.Name := 'FederText2';
{$endif}

{$ifdef WantFederKeyboard}
  FederKeyboard := TFederKeyboard01.Create;
  FederBinding := TFederBinding.Create;
{$endif}

{$ifdef WantFederAction}
  ActionHandler := TFederActionHandler.Create;
  ActionHelper := TActionHelper.Create(ActionHandler);
{$endif}

  InitText;
end;

destructor TRggMain.Destroy;
begin
  MainVar.AppIsClosing := True;

{$ifdef WantFederAction}
  ActionHelper.Free;
{$endif}

{$ifdef WantFederText}
  ActionMap1.Free;
  ActionMap2.Free;
  FederText1.Free;
  FederText2.Free;
{$endif}

{$ifdef WantFederAction}
  ActionGroupList.Free;
  ActionTest.Free;
  ActionList.Free;
{$endif}

{$ifdef WantFederKeyboard}
  FederKeyboard.Free;
  FederBinding.Free;
{$endif}

  RggData.Free;
  Trimm0.Free;
  Trimm1.Free;
  Trimm2.Free;
  Trimm3.Free;
  Trimm4.Free;
  Trimm5.Free;
  Trimm6.Free;
  Trimm7.Free;
  Trimm8.Free;

  RggTrackbar.Free;

  Logger.Free;
  FL.Free;

  inherited;
end;

procedure TRggMain.UpdateStrokeRigg;
begin
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  StrokeRigg.ControllerTyp := Rigg.ControllerTyp;
  StrokeRigg.SofortBerechnen := SofortBerechnen;
  StrokeRigg.GrauZeichnen := ModelOutput.GrauZeichnen;
  StrokeRigg.BtnGrauDown := BtnGrauDown;
  StrokeRigg.BtnBlauDown := BtnBlauDown;
  StrokeRigg.RiggLED := ModelOutput.RiggLED;

  if Rigg.SalingTyp > stDrehbar then
    StrokeRigg.Koppel := False
  else
    StrokeRigg.Koppel := Koppel;

  StrokeRigg.Bogen := Bogen;
  StrokeRigg.WanteGestrichelt := not ModelError.GetriebeOK;
  StrokeRigg.Koordinaten := ModelOutput.Koordinaten;
  StrokeRigg.KoordinatenE := ModelOutput.KoordinatenE;
  StrokeRigg.SetKoppelKurve(ModelOutput.KoppelKurve);
  StrokeRigg.SetMastKurve(ModelOutput.MastKurve);

  StrokeRigg.DoOnUpdateStrokeRigg;
end;

procedure TRggMain.SetParameter(fa: TFederAction);
begin
  if FAction = faPan then
    FAction := faNoop
  else
  FAction := fa;

  case fa of
    faNoop: ;
    faPan: ;
    faController: Param := fpController;
    faWinkel: Param := fpWinkel;
    faVorstag: Param := fpVorstag;
    faWante: Param := fpWante;
    faWoben: Param := fpWoben;
    faSalingH: Param := fpSalingH;
    faSalingA: Param := fpSalingA;
    faSalingL: Param := fpSalingL;
    faSalingW: Param := fpSalingW;
    faMastfallF0C: Param := fpMastfallF0C;
    faMastfallF0F: Param := fpMastfallF0F;
    faMastfallVorlauf: Param := fpMastfallVorlauf;
    faBiegung: Param := fpBiegung;
    faMastfussD0X: Param := fpD0X;

    faParamAPW: Param := fpAPW;
    faParamEAH: Param := fpEAH;
    faParamEAR: Param := fpEAR;
    faParamEI: Param := fpEI;
  end;
end;

procedure TRggMain.SetSofortBerechnen(const Value: Boolean);
begin
  if FSofortBerechnen <> Value then
  begin
    FSofortBerechnen := Value;
    if Value then
      UpdateGraph(False)
    else
      InternalDraw;
  end;
end;

procedure TRggMain.SetBtnBlauDown(const Value: Boolean);
begin
  if FBtnBlauDown <> Value then
  begin
    FBtnBlauDown := Value;
    InternalDraw;
  end;
end;

procedure TRggMain.SetBtnGrauDown(const Value: Boolean);
begin
  if FBtnGrauDown <> Value then
  begin
    FBtnGrauDown := Value;
    if Value then
      InternalDraw
    else
      UpdateGraph(False);
  end;
end;

procedure TRggMain.SetCurrentValue(const Value: single);
begin
  Rigg.RggFA.Find(FParam).Ist := Value;
end;

procedure TRggMain.SetDemo(const Value: Boolean);
begin
  FDemo := Value;
  if FDemo then
  begin
    Rigg.SetDefaultDocument;
    Rigg.InitFactArray;
    SetParam(FParam);
  end;
  MainView.ShowTrimm;
end;

procedure TRggMain.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  StrokeRigg.ViewPoint := Value;
end;

procedure TRggMain.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  FixPunkt := ModelOutput.Koordinaten.V[Value];
  StrokeRigg.FixPoint := Value;
end;

procedure TRggMain.SetHullVisible(const Value: Boolean);
begin
  if Value <> FHullVisible then
  begin
    FHullVisible := Value;
    StrokeRigg.HullVisible := Value;
    InternalDraw;
  end;
end;

procedure TRggMain.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  InternalDraw;
end;

procedure TRggMain.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  InternalDraw;
end;

procedure TRggMain.SetKorrigiert(const Value: Boolean);
begin
  if (Value <> FKorrigiert) then
  begin
    FKorrigiert := Value;
    Rigg.Korrigiert := Value;
    UpdateGraph(False);
  end;
end;

procedure TRggMain.UpdateGraph(CurrentChanged: Boolean);
begin
{$ifdef WantOnUpdateChart}
  if FParam  = fpAPW then
  begin
    if Assigned(OnUpdateChart) then
      FOnUpdateChart(Self);
    Exit;
  end;
{$endif}

  case FParam of
    fpEAH: UpdateEAH(CurrentValue);
    fpEAR: UpdateEAR(CurrentValue);
    fpEI: Rigg.MastEI := Round(CurrentValue);
  end;

  ModelInput.CurrentChanged := CurrentChanged;
  ModelInput.CurrentParam := Param;
  ModelInput.CurrentParamValue := CurrentValue;

  ModelInput.SofortBerechnen := SofortBerechnen;

  Rigg.Process(ModelInput); // --> Draw via ModelEvent

{$ifdef WantOnUpdateGraph}
  if Assigned(OnUpdateGraph) then
    OnUpdateGraph(nil);
{$endif}
end;

procedure TRggMain.SetParamValue(idx: TFederParam; const Value: single);
var
  sb: TRggSB;
begin
  sb := Rigg.RggFA.Find(idx);
  if sb <> nil then
  begin
    if Value = CurrentValue then
      { do nothing }
    else if Value >= sb.Max then
      sb.Ist := sb.Max
    else if Value <= sb.Min then
      sb.Ist := sb.Min
    else
      sb.Ist := Value;

    UpdateGraph(True);
  end;
end;

function TRggMain.GetCurrentValue: single;
begin
  result := Rigg.RggFA.Find(FParam).Ist;
end;

function TRggMain.GetHullVisible: Boolean;
begin
  result := StrokeRigg.QueryRenderOption(faRggHull);
  if result <> FHullVisible then
    FHullVisible := result;
end;

function TRggMain.GetMastfall: string;
begin
  result := Format('%.0f', [Rigg.RggFA.MastfallF0F.Ist - Rigg.RggFA.MastfallVorlauf.Ist]);
end;

function TRggMain.GetParamValue(idx: TFederParam): single;
begin
  result := Rigg.RggFA.Find(idx).Ist;
end;

function TRggMain.GetParamValueString(fp: TFederParam): string;
begin
  result := Format('%.0f', [Rigg.RggFA.Find(fp).Ist]);
end;

function TRggMain.GetParamValueStringDiff(fp: TFederParam): string;
var
  tv: single;
  fd: TRggData;
begin
  fd := Trimm0;
  tv := Rigg.RggFA.Find(fp).Ist;
  case fp of
    fpD0X: tv := tv - fd.D0X;
    fpController: tv := tv - fd.CPPos;
    fpWinkel: tv := tv - fd.WIPos;
    fpVorstag: tv := tv - fd.VOPos;
    fpWante: tv := tv - fd.WLPos;
    fpWoben: tv := tv - fd.WOPos;
    fpSalingH: tv := tv - fd.SHPos;
    fpSalingA: tv := tv - fd.SAPos;
    fpSalingL: tv := tv - fd.SLPos;
    fpSalingW: tv := tv - fd.SWPos;
    fpMastfallVorlauf: tv := tv - fd.MV;
    fpMastfallF0C: tv := tv - fd.F0C;
    fpMastfallF0F: tv := tv - fd.F0F;
    fpBiegung: tv := tv - fd.Bie;
  end;
  result := Format('%.0f', [tv]);
end;

procedure TRggMain.SetParam(const Value: TFederParam);
begin
  { make sure the 'pseudo-param' faPan gets cancelled }
  FAction := faNoop;

  Rigg.SetParam(Value, Demo);

  StrokeRigg.ControllerTyp := Rigg.ControllerTyp;

  FParam := Value;
  CurrentValue := Rigg.RggFA.Find(FParam).Ist;

  UpdateGraph(True);
  SetupTrackBarForRgg;
end;

function TRggMain.Text2Param(T: string): TFederParam;
begin
  result := fpVorstag;
  if T = RggLocalizedStrings.ControllerString then
    result := fpController
  else if T = RggLocalizedStrings.WinkelString then
    result := fpWinkel
  else if T = RggLocalizedStrings.VorstagString then
    result := fpVorstag
  else if T = RggLocalizedStrings.WanteString then
    result := fpWante
  else if (T = RggLocalizedStrings.WanteObenString) or (T = 'Woben') then
    result := fpWoben
  else if (T = RggLocalizedStrings.SalingHString) or (T = 'SalingH') then
    result := fpSalingH
  else if (T = RggLocalizedStrings.SalingAString) or (T = 'SalingA') then
    result := fpSalingA
  else if (T = RggLocalizedStrings.SalingLString) or (T = 'SalingL') then
    result := fpSalingL
  else if (T = RggLocalizedStrings.SalingWString) or (T = 'SalingW') then
    result := fpSalingW
  else if T = RggLocalizedStrings.MastfallF0CString then
    result := fpMastfallF0C
  else if T = RggLocalizedStrings.MastfallF0FString then
    result := fpMastfallF0F
  else if T = RggLocalizedStrings.MastfallVorlaufString then
    result := fpMastfallVorlauf
  else if T = RggLocalizedStrings.BiegungString then
    result := fpBiegung
  else if T = RggLocalizedStrings.MastFootD0XString then
    result := fpD0X
  else if T = RggLocalizedStrings.APWidthString then
    result := fpAPW
  else if T = RggLocalizedStrings.EAHullString then
    result := fpEAH
  else if T = RggLocalizedStrings.EARiggString then
    result := fpEAR
  else if T = RggLocalizedStrings.EIMastString then
    result := fpEI
    ;
end;

function TRggMain.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := RggLocalizedStrings.ControllerString
  else if P = fpWinkel then
    result := RggLocalizedStrings.WinkelString
  else if P = fpVorstag then
    result := RggLocalizedStrings.VorstagString
  else if P = fpWante then
    result := RggLocalizedStrings.WanteString
  else if P = fpWoben then
    result := RggLocalizedStrings.WanteObenString
  else if P = fpSalingH then
    result := RggLocalizedStrings.SalingHString
  else if P = fpSalingA then
    result := RggLocalizedStrings.SalingAString
  else if P = fpSalingL then
    result := RggLocalizedStrings.SalingLString
  else if P = fpSalingW then
    result := RggLocalizedStrings.SalingWString
  else if P = fpMastfallF0C then
    result := RggLocalizedStrings.MastfallF0CString
  else if P = fpMastfallF0F then
    result := RggLocalizedStrings.MastfallF0FString
  else if P = fpMastfallVorlauf then
    result := RggLocalizedStrings.MastfallVorlaufString
  else if P = fpBiegung then
    result := RggLocalizedStrings.BiegungString
  else if P = fpD0X then
    result := RggLocalizedStrings.MastfootD0XString
  else if P = fpAPW then
    result := RggLocalizedStrings.APWidthString
  else if P = fpEAH then
    result := RggLocalizedStrings.EAHullString
  else if P = fpEAR then
    result := RggLocalizedStrings.EARiggString
  else if P = fpEI then
    result := RggLocalizedStrings.EIMastString
    ;
end;

procedure TRggMain.Reset;
begin
  Rigg.SetDefaultDocument;
  Rigg.ControllerTyp := ctOhne;
  Rigg.InitFactArray;
  UpdateGraph(True);
end;

procedure TRggMain.LoadTrimm(fd: TRggData);
begin
  Rigg.LoadTrimm(fd);
  SetParam(FParam);
end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin
  Rigg.SaveTrimm(fd);
end;

procedure TRggMain.Init420;
begin
  InitWithDefaultDoc(False, 7);
end;

procedure TRggMain.InitLogo;
begin
  InitWithDefaultDoc(True, 8);
end;

procedure TRggMain.InitWithDefaultDoc(AWantLogoData: Boolean; TargetSlot: Integer);
var
  doc: TRggDocument;
begin
  doc := TRggDocument.Create;
  WantLogoData := AWantLogoData;
  doc.GetDefaultDocument;
  Rigg.SetDocument(doc);
  doc.Free;
  DoAfterInitDefault(TargetSlot);
end;

procedure TRggMain.DoAfterInitDefault(ATrimmSlot: Integer);
begin
  Rigg.ControllerTyp := TControllerTyp.ctOhne;
  Rigg.InitFactArray;
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);

  case ATrimmSlot of
    7:
    begin
      SaveTrimm(Trimm7);
      TrimmNoChange := 7;
    end;
    8:
    begin
      SaveTrimm(Trimm8);
      TrimmNoChange := 8;
    end;
  end;

  ParamValue[Param] := ParamValue[Param];
  UpdateOnParamValueChanged;
end;

procedure TRggMain.InitSalingTyp(fa: Integer);
begin
  case fa of
    faSalingTypFest: Rigg.SalingTyp := stFest;
    faSalingTypDrehbar: Rigg.SalingTyp := stDrehbar;
    faSalingTypOhne: Rigg.SalingTyp := stOhneBiegt;
    faSalingTypOhneStarr: Rigg.SalingTyp := stOhneStarr;
  end;
  StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
  MainView.HandleAction(fa);
end;

procedure TRggMain.UpdateColumnC(ML: TStrings);
begin
  DoBiegungGF;
  ML.Add(ParamValueString[fpVorstag]);
  ML.Add(ParamValueString[fpWante]);
  ML.Add(ParamValueString[fpWoben]);
  ML.Add(ParamValueString[fpSalingH]);
  ML.Add(ParamValueString[fpSalingA]);
  ML.Add(ParamValueString[fpSalingL]);
  ML.Add(ParamValueString[fpSalingW]);
  ML.Add(ParamValueString[fpMastfallVorlauf]);
  ML.Add(Mastfall);
  ML.Add(ParamValueString[fpMastfallF0F]);
  ML.Add(ParamValueString[fpMastfallF0C]);
  ML.Add(ParamValueString[fpBiegung]);
  ML.Add(FormatValue(BiegungGF));
end;

procedure TRggMain.UpdateColumnD(ML: TStrings);
begin
  ML.Add(ParamValueStringDiff[fpVorstag]);
  ML.Add(ParamValueStringDiff[fpWante]);
  ML.Add(ParamValueStringDiff[fpWoben]);
  ML.Add(ParamValueStringDiff[fpSalingH]);
  ML.Add(ParamValueStringDiff[fpSalingA]);
  ML.Add(ParamValueStringDiff[fpSalingL]);
  ML.Add(ParamValueStringDiff[fpSalingW]);
  ML.Add(ParamValueStringDiff[fpMastfallVorlauf]);
  ML.Add('');
  ML.Add(ParamValueStringDiff[fpMastfallF0F]);
  ML.Add(ParamValueStringDiff[fpMastfallF0C]);
  ML.Add(ParamValueStringDiff[fpBiegung]);
  ML.Add(''); //FormatValue(BiegungGFDiff));
end;

procedure TRggMain.AL(A: string; fp: TFederParam);
var
  B, C, D: string;
begin
  B := ':';
  C := ParamValueString[fp];
  D := ParamValueStringDiff[fp];
  TML.Add(Format(tfs, [A, B, C, D]));
end;

procedure TRggMain.BL(A: string; C: string);
var
  B, D: string;
begin
  B := ':';
  D := '';
  TML.Add(Format(tfs, [A, B, C, D]));
end;

procedure TRggMain.UpdateDiffText(ML: TStrings);
begin
  DoBiegungGF;

  ML.Clear;
  TML := ML;

  AL(RggStrings.VOString, fpVorstag);
  AL(RggStrings.WAString, fpWante);
  AL(RggStrings.WOString, fpWoben);
  AL(RggStrings.SHString, fpSalingH);
  AL(RggStrings.SAString, fpSalingA);
  AL(RggStrings.SLString, fpSalingL);
  AL(RggStrings.SWString, fpSalingW);
  AL(RggStrings.MVString, fpMastfallVorlauf);

  BL(RggStrings.MFString, Mastfall);
  AL(RggStrings.F0FString, fpMastfallF0F);
  AL(RggStrings.F0CString, fpMastfallF0C);
  AL(RggStrings.BieString, fpBiegung);
  BL(RggStrings.BGFString, FormatValue(BiegungGF));
end;

procedure TRggMain.SetupTrackbarForRgg;
var
  temp: single;
  us: string;
  sb: TRggSB;
  tb: TFederTrackbar;
  fs: string;
begin
  tb := RggTrackbar;
  temp := ParamValue[Param];
  sb := Rigg.RggFA.Find(Param);
  tb.Min := -200; // smaller than the smallest Value
  tb.Max := sb.Max;
  tb.ValueNoChange := temp;
  tb.Min := sb.Min;
  tb.Frequency := sb.SmallStep;
  tb.WantMultiDelta := sb.IsMulti;

  { get unit string }
  case Param of
    fpWinkel, fpSalingW: us := RggStrings.GradString;
    fpEAH, fpEAR: us := RggStrings.KiloNewtonString;
    fpEI: us := RggStrings.NewtonMeterSquareString;
  else
    us := RggStrings.MilimeterString;
  end;

  { get format string }
  fs := RggStrings.NoDigitRealPlusUnitFormatString;

  MinValCaption := Format(fs, [sb.Min, us]);
  MaxValCaption := Format(fs, [sb.Max, us]);
  IstValCaption := Format(fs, [temp, us]);
  ParamCaption := Param2Text(Param);
end;

procedure TRggMain.RggSpecialDoOnTrackBarChange;
var
  fs: string;
  us: string;
begin
  case Param of
    fpWinkel, fpSalingW: us := RggStrings.GradString;
    fpEAH, fpEAR: us := RggStrings.KiloNewtonString;
    fpEI: us := RggStrings.NewtonMeterSquareString;
  else
    us := RggStrings.MilimeterString;
  end;

  fs := RggStrings.NoDigitRealPlusUnitFormatString;
  IstValCaption := Format(fs, [RggTrackbar.Value, us]);

  ParamValue[FParam] := Round(RggTrackbar.Value);
end;

procedure TRggMain.InternalDraw;
begin
  if not UseTimedDrawing then
    Draw;
  Rigg.UpdateFactArray(Param);
end;

procedure TRggMain.Draw;
begin
  UpdateStrokeRigg;
  StrokeRigg.Draw;
end;

procedure TRggMain.ToggleRenderOption(fa: TFederAction);
begin
  StrokeRigg.ToggleRenderOption(fa);
  InternalDraw;
end;

procedure TRggMain.TrackBarChange(Sender: TObject);
begin
  RggSpecialDoOnTrackBarChange;
end;

procedure TRggMain.DoSmallWheel(Delta: single);
var
  f: single;
begin
  if FAction = faPan then
  begin
    StrokeRigg.UpdateCameraX(Delta);
  end
  else
  begin
    f := GetSmallStep;
    if Delta > 0 then
      DoWheel(f)
    else
      DoWheel(-f);
  end;
end;

procedure TRggMain.DoBigWheel(Delta: single);
var
  f: single;
begin
  if FAction = faPan then
  begin
    StrokeRigg.UpdateCameraY(Delta);
  end
  else
  begin
    f := GetBigStep;
    if Delta > 0 then
      DoWheel(f)
    else
      DoWheel(-f);
  end;
end;

procedure TRggMain.DoWheel(Delta: single);
begin
  RggTrackbar.Delta := Delta;
  UpdateOnParamValueChanged;
end;

function TRggMain.GetBigStep: single;
begin
  case FParam of
    fpWante: result := 2;
    fpWinkel: result := 1;
    fpSalingW: result := 1;
    else
      result := 10;
  end;
end;

function TRggMain.GetSmallStep: single;
begin
  result := 1;
end;

function TRggMain.FormatValue(Value: single): string;
begin
  result := Format('%.0f', [Value]);
end;

procedure TRggMain.DoBiegungGF;
var
  a, b, c, k, h: single;
  pf: TPoint3D;
  kg, kh: TPoint3D;
  IndexG: Integer;
  IndexH: Integer;
begin
  if StrokeRigg <> nil then
  begin
    pf := ModelOutput.Koordinaten.F;

    IndexG := 13;
    IndexH := 40;

    kg := StrokeRigg.GetMastKurvePoint(IndexG);
    kh := StrokeRigg.GetMastKurvePoint(IndexH);

    a := kg.Distance(pf);
    b := pf.Distance(kh);
    c := kh.Distance(kg);

    h := TRggCalc.Hoehe(a - 0.00001, b, c, k);

    BiegungGFDiff := BiegungGF - h;
    BiegungGF := h;
  end
  else
  begin
    BiegungGFDiff := 0;
    BiegungGF := 0;
  end;
end;

procedure TRggMain.DebugBiegungGF(ML: TStrings);
var
  a, b, c, k, h: single;
  pf: TPoint3D;
  kg: TPoint3D;
  kh: TPoint3D;
  bm, l: single;
  t: single;
  IndexG, IndexD, IndexH, IndexC: single;
begin
  ML.Clear;
  if StrokeRigg <> nil then
  begin
    pf := Rigg.RiggPoints.F;

    bm := BogenMax;
    l := Rigg.GetRiggDistance(DC) + Rigg.GetRiggDistance(D0D);

    ML.Add('');
    t := 1160;
    ML.Add('G = 1160');
    IndexG := bm * t / l;
    ML.Add(Format('iG = %.2f', [IndexG]));
    ML.Add('IndexG := 13;');

    ML.Add('');
    t := Rigg.GetRiggDistance(D0D);
    ML.Add(Format('D = Rigg.rL[16] = %.2f', [t]));
    IndexD := bm * t / l;
    ML.Add(Format('iD = %.2f', [IndexD]));
    ML.Add('IndexD := 28;');

    ML.Add('');
    ML.Add('H = 1160 + (4900 / 2)');
    t := 1160 + (4900 / 2);
    ML.Add(Format('H = %.2f', [t]));
    IndexH := bm * t / l;
    ML.Add(Format('iH = %.2f', [IndexH]));
    ML.Add('IndexH := 40;');

    ML.Add('');
    t := l;
    ML.Add(Format('C = D0D + DC = %.2f', [t]));
    IndexC := bm * t / l;
    ML.Add(Format('iC = %.2f', [IndexC]));
    ML.Add('IndexC := 50;');

    kg := StrokeRigg.GetMastKurvePoint(Round(IndexG));
    kh := StrokeRigg.GetMastKurvePoint(Round(IndexH));

    a := kg.Distance(pf);
    b := pf.Distance(kh);
    c := kh.Distance(kg);

    ML.Add('');
    h := TRggCalc.Hoehe(a-0.00001, b, c, k);
    ML.Add('Triangle abc = (GF, FH, HG)');
    ML.Add(Format('(%.2f, %.2f, %.2f)', [a, b, c]));
    ML.Add(Format('Hoehe BGF at H = %.2f', [h]));
  end;
end;

procedure TRggMain.UpdateTrimmText(ML: TStrings);
begin
  ML.Clear;
  ML.Add('Trimm = ' + IntToStr(Trimm));
  ML.Add('Name = ' + CurrentTrimm.Name);
  if Action = faPan then
  begin
    ML.Add('Param = Pan');
    ML.Add('Min = ');
    ML.Add('Pos = ');
    ML.Add('Max = ');
  end
  else
  begin
    ML.Add('Param = ' + ParamCaption);
    ML.Add('Min = ' + MinValCaption);
    ML.Add('Pos = ' + IstValCaption);
    ML.Add('Max = ' + MaxValCaption);
  end;

  if Demo then
    ML.Add('Modus = Demo')
  else
    ML.Add('Modus = Pro');

{$ifdef MSWINDOWS}
  ML.Add('CounterG = ' + IntToStr(Rigg.GetCounterValue(0)));
{$endif}
end;

procedure TRggMain.UpdateJsonText(ML: TStrings);
begin
  ML.Text := TrimmJson;
end;

procedure TRggMain.UpdateDataText(ML: TStrings);
begin
  ML.Text := TrimmData;
end;

procedure TRggMain.MemoryBtnClick;
begin
{$ifdef debug}
  Logger.Info('in MemoryBtnClick');
{$endif}
  RefCtrl := Rigg.Glieder;
  RefModelInput := ModelInput;
  StrokeRigg.KoordinatenR := Rigg.RiggPoints;
  InternalDraw;
end;

procedure TRggMain.MemoryRecallBtnClick;
begin
{$ifdef debug}
  Logger.Info('in MemoryRecallBtnClick');
{$endif}
  Rigg.Glieder := RefCtrl;
  Rigg.UpdateFactArray(RefModelInput.CurrentParam);
  UpdateGraph(False);

  RggTrackbar.ValueNoChange := CurrentValue;
  RggSpecialDoOnTrackBarChange;
  UpdateOnParamValueChanged;
end;

procedure TRggMain.UpdateEAR(Value: single);
var
  EA: TRiggRods; { EA in KN }
  c: Integer;
begin
  EA := Rigg.EA;

  c := Round(Value);

  { Wanten }
  EA.B0B := c;
  EA.A0A := c;
  EA.BC := c;
  EA.AC := c;

  { Vorstag }
  EA.C0C := c;

  { Saling }
  EA.BD := c;
  EA.AD := c;

  { Saling-Verbindung }
  EA.AB := c;

  Rigg.EA := EA;
end;

procedure TRggMain.UpdateEAH(Value: single);
var
  EA: TRiggRods; { EA in KN }
  c: Integer;
begin
  EA := Rigg.EA;

  c := Round(Value);

  { Rumpflängen }
  EA.C0D0 := c;
  EA.B0C0 := c;
  EA.A0C0 := c;
  EA.B0D0 := c;
  EA.A0D0 := c;
  EA.A0B0 := c;

  Rigg.EA := EA;
end;

procedure TRggMain.SetTrimmNoChange(const Value: Integer);
begin
  Logger.Info('SetTrimmNoChange: ' + IntToStr(Value));
  FTrimm := Value;
end;

procedure TRggMain.SetTrimm(const Value: Integer);
begin
  Logger.Info('SetTrimm: ' + IntToStr(Value));
  FTrimm := Value;
  LoadTrimm(CurrentTrimm);
  InitSalingTyp(faSalingTypFest);
  UpdateOnParamValueChanged;
end;

function TRggMain.GetCurrentTrimm: TRggData;
begin
  result := GetTrimmItem(FTrimm);
end;

function TRggMain.GetTrimmItem(i: Integer): TRggData;
begin
  case i of
    1: result := Trimm1;
    2: result := Trimm2;
    3: result := Trimm3;
    4: result := Trimm4;
    5: result := Trimm5;
    6: result := Trimm6;
    7: result := Trimm7;
    8: result := Trimm8;
    else
      result := Trimm0;
  end;
end;

function TRggMain.GetTrimmItemReport(ReportID: Integer): string;
begin
  if (Rigg <> nil) and (RggData <> nil) and (FL <> nil) then
  begin
    Rigg.SaveToFederData(RggData);
    FL.Clear;
    case ReportID of
      0: RggData.WriteReport(FL);
      1: RggData.WriteJSon(FL);
      2:
      begin
        RggData.WantAll := False;
        RggData.SaveTrimmItem(FL);
      end;
      3:
      begin
        RggData.WantAll := True;
        RggData.SaveTrimmItem(FL);
      end;

      else
        RggData.WriteReport(FL);
    end;
    result := FL.Text;
    FL.Clear;
  end;
end;

function TRggMain.GetTrimmItemReportLong: string;
begin
  result := GetTrimmItemReport(3);
end;

function TRggMain.GetTrimmItemReportShort: string;
begin
  result := GetTrimmItemReport(2);
end;

function TRggMain.GetTrimmItemReportJson: string;
begin
  result := GetTrimmItemReport(1);
end;

function TRggMain.GetTrimmItemReportData: string;
begin
  result := GetTrimmItemReport(0);
end;

procedure TRggMain.InitTrimmData;
var
  fd: TRggData;
begin
  Logger.Info('in InitTrimmData ( default data )');
  fd := Trimm0;
  fd.Reset;

  fd := Trimm1;
  fd.Assign(Trimm0);
  fd.Name := 'T1';
  fd.MV := fd.MV + 10;

  fd := Trimm2;
  fd.Assign(Trimm0);
  fd.Name := 'T2';
  fd.WLPos := fd.WLPos + 20;

  fd := Trimm3;
  fd.Assign(Trimm0);
  fd.Name := 'T3';
  fd.VOPos := fd.VOPos + 30;

  fd := Trimm4;
  fd.Assign(Trimm0);
  fd.Name := 'T4';
  fd.SHPos := fd.SHPos + 40;

  fd := Trimm5;
  fd.Assign(Trimm0);
  fd.Name := 'T5';
  fd.SAPos := fd.SAPos + 50;

  fd := Trimm6;
  fd.Assign(Trimm0);
  fd.Name := 'T6';
  fd.VOPos := fd.VOPos + 60;
  fd.WLPos := fd.WLPos - 20;
end;

function TRggMain.GetFLText: string;
begin
  result := FL.Text;
end;

procedure TRggMain.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

procedure TRggMain.SetSuperRadio(const Value: TGraphRadio);
begin
  FGraphRadio := Value;
  case Value of
    gSimple:
    begin
      FSofortBerechnen := False;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gNormal:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gBlau:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := True;
    end;

    gGrau:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := True;
      FBtnBlauDown := False;
    end;

    gMulti:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := True;
      FBtnBlauDown := True;
    end;

    gDisplay:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

    gQuick:
    begin
      FSofortBerechnen := True;
      FBtnGrauDown := False;
      FBtnBlauDown := False;
    end;

  end;

  UpdateGraph(False);
  MainView.ShowTrimm;
  FederTextCheckState;
end;

{$ifdef WantFederText}
procedure TRggMain.InitFederText(ft: TFederTouch0);
begin
  if ft is TLayout then
  begin
    ft.Parent := MainParent;
    TFederTouchBase.OwnerComponent := ft;
    TFederTouchBase.ParentObject := ft;
  end
  else
  begin
    TFederTouchBase.OwnerComponent := MainParent;
    TFederTouchBase.ParentObject := MainParent;
  end;

  ft.Position.X := 0;
  ft.Position.Y := 0;
  ft.Width := MainVar.ClientWidth;
  ft.Height := MainVar.ClientHeight;
  ft.Init;
end;
{$endif}

procedure TRggMain.InitRaster;
begin
  MainVar.ClientWidth := MainView.ClientWidth;
  MainVar.ClientHeight := MainView.ClientHeight - MainVar.StatusBarHeight;
end;

procedure TRggMain.InitText;
begin
{$ifdef WantFederText}
  InitRaster;
  InitFederText(FederText1);
  InitFederText(FederText2);
  Touch := faTouchDesk;
  FederText1.UpdatePageBtnText;
  FederText2.UpdatePageBtnText;
{$else}
  InitRaster;
  FTouch := faTouchDesk;
{$endif}
end;

procedure TRggMain.InitTouch;
begin
  InitRaster;
{$ifdef WantFederText}
  if MainVar.WantFederText then
  begin
    FederText2.Visible := IsPhone;
    FederText1.Visible := not FederText2.Visible;
  end
  else
  begin
    FederText1.Visible := False;
    FederText2.Visible := False;
  end;
{$endif}
end;

procedure TRggMain.UpdateTouch;
begin
  InitRaster;
{$ifdef WantFederText}
  if FederText.InitOK then
  begin
    InitTouch;
    FederText.UpdateShape;
  end;
{$endif}
end;

function TRggMain.GetIsDesktop: Boolean;
begin
  result := not IsPhone;
end;

function TRggMain.GetIsLandscape: Boolean;
begin
  result := MainView.ClientWidth >= MainView.ClientHeight;
end;

function TRggMain.GetIsPhone: Boolean;
var
  MinCount, MaxCount: Integer;
begin
  case FTouch of
    faTouchPhone: result := True;
    faTouchTablet: result := False;
    else
    begin
      result := False;
      if MainVar.Raster > 1 then
      begin
        MinCount := Min(MainView.ClientHeight, MainView.ClientWidth) div MainVar.Raster;
        MaxCount := Max(MainView.ClientHeight, MainView.ClientWidth) div MainVar.Raster;
        result  := (MinCount < 8) or (MaxCount < 12);
      end;
    end;
  end;
end;

function TRggMain.GetIsPortrait: Boolean;
begin
  result := MainView.ClientWidth < MainView.ClientHeight;
end;

procedure TRggMain.SetColorScheme(const Value: Integer);
begin
{$ifdef WantColorScheme}
  if not BackgroundLock then
  begin
    MainVar.ColorScheme.Scheme := Value;
    MainVar.ColorScheme.Init(Value);
  end;
{$endif}

  if IsUp then
  begin
{$ifdef WantFederText}
    FederText.UpdateColorScheme;
{$endif}
    MainView.UpdateColorScheme;
  end;
end;

procedure TRggMain.ToggleButtonSize;
begin
  MainView.ToggleButtonSize;
end;

procedure TRggMain.ToggleDarkMode;
begin
{$ifdef WantColorScheme}
  if MainVar.ColorScheme.IsDark then
    ColorScheme := MainVar.ColorScheme.Light
  else
    ColorScheme := MainVar.ColorScheme.Dark;
{$endif}
end;

function TRggMain.GetColorScheme: Integer;
begin
{$ifdef WantColorScheme}
  result := MainVar.ColorScheme.Scheme;
{$else}
  result := 0;
{$endif}
end;

procedure TRggMain.SetTouch(const Value: Integer);
begin
  FTouch := Value;
{$ifdef WantFederText}
  if IsPhone then
    FederText1.Visible := False
  else case FTouch of
    faTouchTablet: FederText1.Visible := True;
    faTouchPhone: FederText1.Visible := False;
    else
      FederText1.Visible := not IsPhone;
  end;
  FederText2.Visible := not FederText1.Visible;
  FederText.UpdateShape;
{$endif}
end;

procedure TRggMain.CycleColorSchemeM;
{$ifdef WantColorScheme}
var
  i: Integer;
  l: Boolean;
{$endif}
begin
{$ifdef WantColorScheme}
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Dec(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.SchemeDefault := i;
  ColorScheme := i;
  BackgroundLock := l;
{$endif}
end;

procedure TRggMain.CycleColorSchemeP;
{$ifdef WantColorScheme}
var
  i: Integer;
  l: Boolean;
{$endif}
begin
{$ifdef WantColorScheme}
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Inc(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.SchemeDefault := i;
  ColorScheme := i;
  BackgroundLock := l;
{$endif}
end;

{$ifdef WantFederText}
procedure TRggMain.CycleToolSet(i: Integer);
begin
  FederText.UpdateToolSet(i);
  MainView.ShowTrimm;
end;

function TRggMain.GetFederText: TFederTouchBase;
begin
  case FTouch of
    faTouchTablet: result := FederText1;
    faTouchPhone: result := FederText2;
    faTouchDesk:
    begin
      if IsPhone then
        result := FederText2
      else
        result := FederText1;
    end;
    else
      result := FederText1;
  end;
end;
{$endif}

procedure TRggMain.DoTouchbarLeft(Delta: single);
begin
  DoMouseWheel([ssShift], Round(Delta));
end;

procedure TRggMain.DoTouchbarTop(Delta: single);
begin
  MainView.RotaFormRotateZ(Delta);
end;

procedure TRggMain.DoTouchbarRight(Delta: single);
begin
  DoMouseWheel([], Round(Delta));
end;

procedure TRggMain.DoTouchbarBottom(Delta: single);
begin
  MainView.RotaFormZoom(Delta);
end;

procedure TRggMain.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
var
  wd: Integer;
begin
  wd := WheelDelta; // small values may come from touchpad

  { normal mouse }
  if Abs(WheelDelta) > 100 then
    wd := WheelDelta div 120;;

  if ssCtrl in Shift then
  begin
    MainView.RotaFormZoom(wd);
  end
  else if ssShift in Shift then
  begin
    DoBigWheel(wd);
  end
  else if Shift = [] then
  begin
    DoSmallWheel(wd);
  end;
end;

procedure TRggMain.PlusOne;
begin
  DoMouseWheel([ssShift], 1);
end;

procedure TRggMain.PlusTen;
begin
  DoMouseWheel([ssCtrl], 1);
end;

procedure TRggMain.UpdateOnParamValueChanged;
begin
  MainView.UpdateOnParamValueChanged;
end;

procedure TRggMain.SetShowTrimmText(const Value: Boolean);
begin
  MainView.ShowTrimmText := Value;
end;

procedure TRggMain.SetShowDiffText(const Value: Boolean);
begin
  MainView.ShowDiffText := Value;
end;

procedure TRggMain.SetShowDataText(const Value: Boolean);
begin
  MainView.ShowDataText := Value;
end;

function TRggMain.GetShowTrimmText: Boolean;
begin
  result := MainView.ShowTrimmText;
end;

function TRggMain.GetShowDiffText: Boolean;
begin
  result := MainView.ShowDiffText;
end;

function TRggMain.GetShowDataText: Boolean;
begin
  result := MainView.ShowDataText;
end;

procedure TRggMain.WriteTrimmItem;
var
  fd: TRggData;
begin
  FL.Clear;
  fd := RggData;
  SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);
end;

procedure TRggMain.CopyTrimmItem;
begin
  Logger.Info('in CopyTrimmItem');
  WriteTrimmItem;
  CopyText;
  FL.Clear;
  MainView.ShowTrimm;
end;

procedure TRggMain.WriteTrimmFile;
begin
  FL.Clear;
  Trimm0.SaveTrimmFile(FL);
end;

procedure TRggMain.CopyTrimmFile;
begin
  WriteTrimmFile;
  CopyText;
  FL.Clear;
  MainView.ShowTrimm;
end;

procedure TRggMain.CopyAndPaste;
var
  fd: TRggData;
begin
  { copy }
  fd := RggData;
  SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);

  { paste }
  ReadText(FL);
  FL.Clear;
  MainView.ShowTrimm;
end;

procedure TRggMain.PasteTrimmItem;
begin
  Logger.Info('in PasteTrimmItem');
  PasteTrimm;
  { Note: There is just one paste button (pti), named after the item, }
  { but you can paste a Trimm-Item OR a Trimm-File. }
  MainView.ShowTrimm;
end;

procedure TRggMain.PasteTrimm;
var
  v: TValue;
  s: string;
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    FL.Clear;
    try
      v := cbs.GetClipboard;
      if not v.IsEmpty then
      begin
        s := v.AsString;
        if (s <> '') and (Length(s) < 10000) then
        begin
          FL.Text := s;
          ReadText(FL);
          FL.Clear;
        end
        else
        begin
          Logger.Error('there is no ''data'' string on the clipboard');
        end;
      end;
    except
      Logger.Error('no usable data on clipboard');
    end;
  end;
end;

procedure TRggMain.ReadText(ML: TStrings);
var
  i: Integer;
  s: string;
  IsTrimmItem: Boolean;
  IsTrimmFile: Boolean;
begin
  try
    IsTrimmItem := False;
    IsTrimmFile := False;
    for i := 0 to ML.Count-1 do
    begin
      if i > 4 then
        break;
      s := ML[i];
      if Pos('DOCTYPE', s) > 0 then
      begin
        if s.Contains('Trimm-Item' )then
          IsTrimmItem := True;
        if s.Contains('Trimm-File' )then
          IsTrimmFile := True;
      end;
      if IsTrimmItem then
      begin
        RggData.LoadTrimmItem(ML);
        CurrentTrimm.Assign(RggData);
        Trimm := FTrimm;
        Logger.Info(Format('  Trimm %d assigned', [Trimm]));
        break;
      end
      else if IsTrimmFile then
      begin
        RggData.ReadTrimmFile(ML);
        Trimm := FTrimm;
        Logger.Info(Format('  TrimmFile read, Current Trimm is %d.', [Trimm]));
        break;
      end;
    end;
  except
    Trimm := 1;
  end;
end;

function TRggMain.GetTrimmFilePath: string;
begin
  result := '';
{$ifdef MacOS}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef Win32}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Win64}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Android}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef IOS}
  result  := TAppUtils.GetIOSDataDirectory;
{$endif}
end;

procedure TRggMain.ReadTrimmFile0;
var
  fp, sTrimmFileAuto, sTrimmFile: string;
begin
  if not MainVar.IsSandBoxed then
  begin
    Logger.Info('in ReadTrimmFile0');
    fp := GetTrimmFilePath;
    if fp <> '' then
    begin
      sTrimmFile := fp + MainConst.TrimmFileName;
      sTrimmFileAuto := fp + MainConst.TrimmFileNameAuto;
      if FileExists(sTrimmFile) then
      begin
        DoReadTrimmFile(sTrimmFile);
      end
      else if FileExists(sTrimmFileAuto) then
      begin
        DoReadTrimmFile(sTrimmFileAuto);
      end
      else
      begin
        Logger.Info('Trimm-File.txt or Trimm-File-Auto.txt does not exits at path');
        LogFileNameToInfo(fp);
      end;
    end
    else
    begin
      Logger.Info('GetTrimmFilePath is empty');
    end;
  end;
end;

procedure TRggMain.ReadTrimmFile;
var
  fp, fn, s: string;
begin
  Logger.Info('in ReadTrimmFile');
  fp := GetTrimmFilePath;

{ By default you try and load the 'manually edited' Trimm-File.txt; }
{ this should make sense on the Desktop, }
{ or on any device where you have access to the Documents folder. }
  fn := MainConst.TrimmFileName;

{ Maybe you want to have the same behaviour on Windows and iOS }
{ for debugging purpose only... }
{$ifdef MSWINDOWS}
//  fn := TrimmFileNameAuto;
{$endif}

{ On Android and iOS the Trimm-File in the known location cannot be edited, }
{ so it does not make sense to read a 'manually edited' Trimm-File.txt, }
{ but you can manually read a Trimm-File-Auto.txt if already saved, }
{ e.g. by clicking on a button. }
{$ifdef IOS}
  fn := MainConst.TrimmFileNameAuto;
{$endif}
{$ifdef Android}
  fn := MainConst.TrimmFileNameAuto;
{$endif}

  s := fp + fn;
  if MainVar.IsSandboxed then
  begin
    s := MainView.GetOpenFileName(fp, fn);
  end;

  if s <> '' then
  begin
    DoReadTrimmFile(s);
  end;

  MainView.ShowTrimm;
end;

procedure TRggMain.ReadTrimmFileAuto;
var
  fp, fn: string;
begin
  if not MainVar.IsSandboxed then
  begin
    Logger.Info('in ReadTrimmFileAuto');
    fp := GetTrimmFilePath;
    fn := MainConst.TrimmFileNameAuto;
    if (fp <> '') and (fn <> '') then
      DoReadTrimmFile(fp + fn);
  end;
end;

procedure TRggMain.DoReadTrimmFile(fn: string);
begin
  Logger.Info('in DoReadTrimmFile');
  if (fn <> '') and FileExists(fn) then
  begin
    try
      try
        LogFileNameToInfo(fn);
        FL.LoadFromFile(fn, TEncoding.UTF8);
        RggData.ReadTrimmFile(FL);
        Trimm := FTrimm;
      except
        Logger.Info('  in exeption handler');
        InitTrimmData;
      end;
    finally
      FL.Clear;
    end;
  end
  else
  begin
    Logger.Info('TrimmFile does not exist');
    LogFileNameToInfo(fn);
    Logger.Info('Nothing read.');
    InitTrimmData;
  end;
end;

procedure TRggMain.SaveTrimmFile;
begin
  Logger.Info('in SaveTrimmFile');
  SaveTrimmFileAuto;
  MainView.ShowTrimm;
end;

procedure TRggMain.SaveTrimmFileAuto;
var
  fp, fn, s: string;
begin
  Logger.Info('in SaveTrimmFileAuto');
  fp := GetTrimmFilePath;
  fn := MainConst.TrimmFileNameAuto;

  s := fp + fn;
  if MainVar.IsSandboxed then
  begin
    s := MainView.GetSaveFileName(fp, fn);
  end;

  if s <> '' then
  begin
    Trimm0.SaveTrimmFile(FL);
    LogFileNameToInfo(s);
    FL.SaveToFile(s);
    Logger.Info('TrimmFileAuto saved.');
  end
  else
  begin
    Logger.Info('Nothing saved.');
  end;
end;

procedure TRggMain.UpdateTrimm0;
begin
  Logger.Info('in UpdateTrimm0');
  SaveTrimm(Trimm0);
  MainView.ShowTrimm;
end;

function TRggMain.GetIsRggParam: Boolean;
begin
  result := True;
end;

procedure TRggMain.DropTargetDropped(fn: string);
var
 ext: string;
begin
  Logger.Info('in DropTargetDropped');
  ext := ExtractFileExt(fn);
  if ext = '.txt' then
  begin
    DoReadTrimmFile(fn);
  end
  else
  begin
    Logger.Info('  .txt file expected');
  end;
end;

procedure TRggMain.DoReport;
var
  ML: TStrings;
begin
  Inc(ReportCounter);
  ML := Logger.TL;

  while ML.Count > 24 - 8 do
    ML.Delete(0);

  ML.Add('Report:');
  ML.Add('  ReportCounter = ' + IntToStr(ReportCounter));
{$ifdef WantColorScheme}
  ML.Add('  ColorScheme = ' + IntToStr(MainVar.ColorScheme.Scheme));
{$endif}
  ML.Add('  Scale = ' + FloatToStr(MainVar.Scale));
  ML.Add('  Sandboxed = ' + BoolStr[MainVar.IsSandboxed]);
  ML.Add('  WantOnResize = ' + BoolStr[MainVar.WantOnResize]);
  ML.Add('  ResizeCounter = ' + IntToStr(ResizeCounter));
  ML.Add(Format('  ClientSize = (%d, %d)', [MainVar.ClientWidth, MainVar.ClientHeight]));
  ML.Add('---');
  ML.Add(Format('  A = %6.2f', [Rigg.GetTempValue(1)]));
  ML.Add(Format('  B = %6.2f', [Rigg.GetTempValue(2)]));
  ML.Add(Format('  C = %6.2f', [Rigg.GetTempValue(3)]));
end;

procedure TRggMain.DoCleanReport;
begin
  MainVar.ShowDebugData := True;
  Logger.TL.Clear;
  DoReport;
end;

procedure TRggMain.ShowDebugData;
begin
  if not ShowDataText then
  begin
    ShowDataText := true;
    MainVar.ShowDebugData := True;
  end
  else
  begin
    MainVar.ShowDebugData := not MainVar.ShowDebugData;
  end;
end;

procedure TRggMain.LogFileNameToInfo(fn: string);
var
  t1: string;
  t2: string;
  r: boolean;
begin
  t1 := 'C:\Users\';
  t2 := 'Trimm';
  r := LogFileNameToInfoFormatted(t1, t2, fn);

  if not r then
  begin
    t1 := '/var/mobile/Containers/Data/Application/';
    t2 := 'Documents/';
    r := LogFileNameToInfoFormatted(t1, t2, fn);
  end;

  if not r then
  begin
    Logger.Info(fn);
  end;
end;

function TRggMain.LogFileNameToInfoFormatted(t1: string; t2: string; fn: string): boolean;
var
  p2: Integer;
begin
  result := false;
  if fn.StartsWith(t1) then
  begin
    fn := fn.Substring(t1.Length);
    p2 := fn.LastIndexOf(t2);
    if p2 > -1 then
    begin
      Logger.Info(t1);
      Logger.Info('  ' + fn.Substring(0, p2));
      Logger.Info('  ' + fn.Substring(p2));
      result := True;
    end;
  end;
end;

procedure TRggMain.HandleAction(fa: Integer);
begin
  if IsUp then
  case fa of
    faUpdateReportText: DoCleanReport;
    faToggleDebugText: ShowDebugData;

    faParamValueMinus1, faWheelLeft: DoMouseWheel([], -1);
    faParamValuePlus1, faWheelRight: DoMouseWheel([], 1);
    faParamValuePlus10, faWheelUp: DoMouseWheel([ssShift], 1);
    faParamValueMinus10, faWheelDown: DoMouseWheel([ssShift], -1);

    faController: SetParameter(faController);
    faWinkel: SetParameter(faWinkel);
    faVorstag: SetParameter(faVorstag);
    faWante: SetParameter(faWante);
    faWoben: SetParameter(faWoben);
    faSalingH: SetParameter(faSalingH);
    faSalingA: SetParameter(faSalingA);
    faSalingL: SetParameter(faSalingL);
    faSalingW: SetParameter(faSalingW);
    faMastfallF0C: SetParameter(faMastfallF0C);
    faMastfallF0F: SetParameter(faMastfallF0F);
    faMastfallVorlauf: SetParameter(faMastfallVorlauf);
    faBiegung: SetParameter(faBiegung);
    faMastfussD0X: SetParameter(faMastfussD0X);

    faParamAPW: SetParameter(faParamAPW);
    faParamEAH: SetParameter(faParamEAH);
    faParamEAR: SetParameter(faParamEAR);
    faParamEI: SetParameter(faParamEI);

    faFixpointA0: FixPoint := ooA0;
    faFixpointA: FixPoint := ooA;
    faFixpointB0: FixPoint := ooB0;
    faFixpointB: FixPoint := ooB;
    faFixpointC0: FixPoint := ooC0;
    faFixpointC: FixPoint := ooC;
    faFixpointD0: FixPoint := ooD0;
    faFixpointD: FixPoint := ooD;
    faFixpointE0: FixPoint := ooE0;
    faFixpointE: FixPoint := ooE;
    faFixpointF0: FixPoint := ooF0;
    faFixpointF: FixPoint := ooF;

    faSalingTypOhneStarr,
    faSalingTypOhne,
    faSalingTypDrehbar,
    faSalingTypFest: InitSalingTyp(fa);

    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderE,
    faWantRenderS: ToggleRenderOption(fa);

    faViewpointS: ViewPoint := vpSeite;
    faViewpointA: ViewPoint := vpAchtern;
    faViewpointT: ViewPoint := vpTop;
    faViewpoint3: ViewPoint := vp3D;

    faDemo: Demo := not Demo;

    faTrimm0: Trimm := 0;
    faTrimm1: Trimm := 1;
    faTrimm2: Trimm := 2;
    faTrimm3: Trimm := 3;
    faTrimm4: Trimm := 4;
    faTrimm5: Trimm := 5;
    faTrimm6: Trimm := 6;

    fa420: Init420;
    faLogo: InitLogo;

    faUpdateTrimm0: UpdateTrimm0;
    faCopyAndPaste: CopyAndPaste;
    faCopyTrimmItem: CopyTrimmItem;
    faPasteTrimmItem: PasteTrimmItem;

    faReadTrimmFile: ReadTrimmFile;
    faCopyTrimmFile: CopyTrimmFile;
    faSaveTrimmFile: SaveTrimmFile;

    faToggleTrimmText: ShowTrimmText := not ShowTrimmText;
    faToggleDiffText: ShowDiffText := not ShowDiffText;
    faToggleDataText:
    begin
      MainVar.ShowDebugData := False;
      ShowDataText := not ShowDataText;
    end;

    faNoop: ;

{$ifdef WantFederText}
    faToggleTouchFrame: FederText.ToggleTouchFrame;
    faActionPageM: CycleToolSet(-1);
    faActionPageP: CycleToolSet(1);
    faActionPage1: FederText.ActionPage := 1;
    faActionPage2: FederText.ActionPage := 2;
    faActionPage3: FederText.ActionPage := 3;
    faActionPage4: FederText.ActionPage := 4;
    faActionPage5: FederText.ActionPage := 5;
    faActionPageE: FederText.ActionPage := -1;
    faActionPageS: FederText.ActionPage := -3;
    faActionPageX: FederText.ActionPage := -2;
{$else}
    faToggleTouchFrame: ;
    faActionPageM: ;
    faActionPageP: ;
    faActionPage1: ;
    faActionPage2: ;
    faActionPage3: ;
    faActionPage4: ;
    faActionPage5: ;
    faActionPageE: ;
    faActionPageS: ;
    faActionPageX: ;
{$endif}

    faCycleColorSchemeM: CycleColorSchemeM;
    faCycleColorSchemeP: CycleColorSchemeP;

    faToggleDarkMode: ToggleDarkMode;

    else
    begin
      MainView.HandleAction(fa);
    end;
  end;

  if IsUp then
  begin
    if (fa in ParamsRange) then
      MainView.UpdateItemIndexParams
    else if (fa in ReportsRange) then
      MainView.UpdateItemIndexReports
    else if (fa in TrimmsRange) then
      MainView.UpdateItemIndexTrimms;

    MainView.UpdateHintText(fa);
    FederTextCheckState;
  end;
end;

function TRggMain.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleLanguage: result := MainVar.WantGermanText;
    faController: result := Param = fpController;
    faWinkel: result := Param = fpWinkel;
    faVorstag: result := Param = fpVorstag;
    faWante: result := Param = fpWante;
    faWoben: result := Param = fpWoben;
    faSalingH: result := Param = fpSalingH;
    faSalingA: result := Param = fpSalingA;
    faSalingL: result := Param = fpSalingL;
    faSalingW: result := Param = fpSalingW;
    faMastfallF0C: result := Param = fpMastfallF0C;
    faMastfallF0F: result := Param = fpMastfallF0F;
    faMastfallVorlauf: result := Param = fpMastfallVorlauf;
    faBiegung: result := Param = fpBiegung;
    faMastfussD0X: result := Param = fpD0X;

    faParamAPW: result := Param = fpAPW;
    faParamEAH: result := Param = fpEAH;
    faParamEAR: result := Param = fpEAR;
    faParamEI: result := Param = fpEI;

    faPan: result := Action = faPan;

    faFixpointA0: result := FixPoint = ooA0;
    faFixpointA: result := FixPoint = ooA;
    faFixpointB0: result := FixPoint = ooB0;
    faFixpointB: result := FixPoint = ooB;
    faFixpointC0: result := FixPoint = ooC0;
    faFixpointC: result := FixPoint = ooC;
    faFixpointD0: result := FixPoint = ooD0;
    faFixpointD: result := FixPoint = ooD;
    faFixpointE0: result := FixPoint = ooE0;
    faFixpointE: result := FixPoint = ooE;
    faFixpointF0: result := FixPoint = ooF0;
    faFixpointF: result := FixPoint = ooF;

    faSalingTypFest: result := Rigg.SalingTyp = stFest;
    faSalingTypDrehbar: result := Rigg.SalingTyp = stDrehbar;
    faSalingTypOhne: result := Rigg.SalingTyp = stOhneBiegt;
    faSalingTypOhneStarr: result := Rigg.SalingTyp = stOhneStarr;

    faTrimm0: result := Trimm = 0;
    faTrimm1: result := Trimm = 1;
    faTrimm2: result := Trimm = 2;
    faTrimm3: result := Trimm = 3;
    faTrimm4: result := Trimm = 4;
    faTrimm5: result := Trimm = 5;
    faTrimm6: result := Trimm = 6;
    fa420: result := Trimm = 7;
    faLogo: result := Trimm = 8;

    faToggleUseDisplayList,
    faToggleUseQuickSort,
    faRggBogen,
    faRggKoppel,
    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderE,
    faWantRenderS: result := StrokeRigg.QueryRenderOption(fa);

    faRggHull: result := HullVisible;
    faDemo: result := Demo;

    faSofortBtn: result := SofortBerechnen;
    faGrauBtn: result := BtnGrauDown;
    faBlauBtn: result := BtnBlauDown;
    faMemoryBtn: result := False;

    faSuperSimple: result := GraphRadio = gSimple;
    faSuperNormal: result := GraphRadio = gNormal;
    faSuperGrau: result := GraphRadio = gGrau;
    faSuperBlau: result := GraphRadio = gBlau;
    faSuperMulti: result := GraphRadio = gMulti;
    faSuperDisplay: result := GraphRadio = gDisplay;
    faSuperQuick: result := GraphRadio = gQuick;

    faToggleDataText: result := ShowDataText;
    faToggleDiffText: result := ShowDiffText;
    faToggleTrimmText: result := ShowTrimmText;

{$ifdef WantColorScheme}
    faToggleDarkMode: result := MainVar.ColorScheme.IsDark;
{$endif}

    else
      result := MainView.GetChecked(fa);
  end;
end;

function TRggMain.GetEnabled(fa: TFederAction): Boolean;
begin
{$ifdef WantFederAction}
  result := ActionHandler.GetEnabled(fa);
{$else}
  result := True;
{$endif}
end;

procedure TRggMain.InitDefaultData;
begin
  if not InitDataOK then
  begin
    InitDataOK := True;
    InitLogo; // sets WantLogoData to true
    Init420; // resets WantLogoData to false
    Trimm := 1;
    FixPoint := InitialFixPoint;
  end;
end;

procedure TRggMain.FederTextCheckState;
begin
{$ifdef WantFederText}
  FederText.CheckState;
{$endif}
end;

procedure TRggMain.FederTextUpdateParent;
begin
{$ifdef WantFederText}
//  if FederText.Parent = nil then
//  begin
    FederText1.Parent := MainParent;
    FederText2.Parent := MainParent;
//  end;
{$endif}
end;

procedure TRggMain.FederTextUpdateCaption;
begin
{$ifdef WantFederText}
  if Action = faPan then
  begin
    { use CurrentPageCaption on Tablet and Desktop,
        see RiggVar.FederModel.TouchBase - UpdatePageBtnText }
    if IsPhone then
      FederText.ST00.Text.Text := 'Pan';

    FederText.SB00.Text.Text := '';
  end
  else
  begin
    { use CurrentPageCaption on Tablet and Desktop }
    if IsPhone then
      FederText.ST00.Text.Text := ParamCaption;

    FederText.SB00.Text.Text := ParamValueString[Param];
  end;
{$endif}
end;

procedure TRggMain.CollectShortcuts(fa: Integer; ML: TStrings);
begin
{$ifdef WantFederKeyboard}
  Keyboard.GetShortcuts(fa, ML);
{$endif}
//  Main.ActionMap0.CollectOne(fa, ML);
{$ifdef WantFederText}
  ActionMapTablet.CollectOne(fa, ML);
  ActionMapPhone.CollectOne(fa, ML);
{$endif}
//  FederMenu.CollectOne(fa, ML);
end;

procedure TRggMain.DoOnModelEvent(Sender: TObject; Data: TRggModelOutput; Error: TRggModelError);
begin
  ModelOutput := Data;
  ModelError := Error;
  InternalDraw;
end;

{$ifdef WantFederText}
function TRggMain.IsTouchBtnEnabled(ABtn: TTouchBtn): Boolean;
begin
  result := True;
end;
{$endif}

procedure TRggMain.FrameVisibilityChanged;
begin
  MainView.UpdateHintText(faNoop);
end;

end.
