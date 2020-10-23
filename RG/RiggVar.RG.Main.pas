unit RiggVar.RG.Main;

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
  System.SysUtils,
  System.Classes,
  RiggVar.RG.Data,
  RiggVar.RG.Def,
  RiggVar.RG.Track,
  RiggVar.RG.Graph,
  RggStrings,
  RggScroll,
  RggTypes,
  RggUnit4,
  RggCalc,
  RggChart,
  RggDoc,
  RiggVar.FB.ActionConst,
  RiggVar.Util.Logger,
  System.Math,
  System.Math.Vectors;

type
  TFederAction = Integer;

  TRggRigg = class
  public
    IsUp: Boolean;
    Rigg: TRigg;
    StrokeRigg: IStrokeRigg; // injected
  end;

  TRggText = class(TRggRigg)
  protected
    FL: TStringList;
    function GetFLText: string;
    procedure CopyText;
  public
    Logger: TLogger;
    constructor Create;
    destructor Destroy; override;
    procedure UpdateText(ClearFlash: Boolean = False); virtual;
    procedure UpdateOnParamValueChanged; virtual;
    property FLText: string read GetFLText;
  end;

  TRggParam = class(TRggText)
  private
    FParam: TFederParam;
    procedure SetParam(Value: TFederParam); virtual; abstract;
  public
    property Param: TFederParam read FParam write SetParam;
  end;

  TRggWheel = class(TRggParam)
  protected
    FAction: TFederAction;
    FactArray: TRggFA; // not owned, cached from Rigg
    function GetBigStep: single;
    function GetSmallStep: single;
    procedure TrackBarChange(Sender: TObject);
    procedure RggSpecialDoOnTrackBarChange; virtual;
  public
    RggTrackbar: TFederTrackbar;

    constructor Create;
    destructor Destroy; override;

    procedure DoWheel(Delta: single);

    procedure DoBigWheelRG(Delta: single);
    procedure DoSmallWheelRG(Delta: single);
    procedure DoRasterWheelRG(Delta: single);
  end;

  TRggTrimm = class(TRggWheel)
  protected
    FTrimm: Integer;
    function GetCurrentTrimm: TRggData;
    procedure SetTrimm(const Value: Integer);
    procedure SetTrimmNoChange(const Value: Integer);
    procedure InitTrimmData;
  public
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

    constructor Create;
    destructor Destroy; override;

    procedure InitSalingTyp(fa: Integer); virtual; abstract;

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    function GetTrimmItem(i: Integer): TRggData;
    function GetTrimmItemReport(ReportID: Integer): string;
    function GetTrimmItemReportData: string;
    function GetTrimmItemReportJson: string;
    function GetTrimmItemReportShort: string;
    function GetTrimmItemReportLong: string;

    property CurrentTrimm: TRggData read GetCurrentTrimm;
    property TrimmNoChange: Integer read FTrimm write SetTrimmNoChange;
    property Trimm: Integer read FTrimm write SetTrimm;

    property TrimmData: string read GetTrimmItemReportData;
    property TrimmJson: string read GetTrimmItemReportJson;
    property TrimmShort: string read GetTrimmItemReportShort;
    property TrimmLong: string read GetTrimmItemReportLong;
  end;

  TRggMain = class(TRggTrimm)
  private
    FFixPoint: TRiggPoint;
    FViewPoint: TViewPoint;
    FVisible: Boolean;

    BiegungGF: single;
    BiegungGFDiff: single;

    TML: TStrings;

    FSofortBerechnen: Boolean;
    FBtnGrauDown: Boolean;
    FBtnBlauDown: Boolean;

    FGraphRadio: TGraphRadio;

    FOnUpdateGraph: TNotifyEvent;
    FKorrigiert: Boolean;
    FBogen: Boolean;
    FKoppel: Boolean;
    FHullVisible: Boolean;
    FDemo: Boolean;

    function FormatValue(Value: single): string;
    procedure DoBiegungGF;

    procedure ChangeRigg(Value: single);

    procedure SetParam(Value: TFederParam); override;
    procedure SetParamValue(idx: TFederParam; Value: single);
    function GetParamValue(idx: TFederParam): single;
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);
    function GetCurrentValue: single;
    procedure SetCurrentValue(const Value: single);
    function GetParamValueString(fp: TFederParam): string;
    function GetParamValueStringDiff(fp: TFederParam): string;
    function GetMastfall: string;
    procedure SetupTrackBarForRgg;
    procedure SetVisible(const Value: Boolean);
    procedure AL(A: string; fp: TFederParam);
    procedure BL(A: string; C: string);
    function GetHullVisible: Boolean;
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure SetOnUpdateGraph(const Value: TNotifyEvent);
    procedure UpdateEAR(Value: single);
    procedure UpdateEAH(Value: single);
    procedure SetSuperRadio(const Value: TGraphRadio);
    procedure SetKorrigiert(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetKoppel(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetDemo(const Value: Boolean);
  protected
    procedure InitFactArray;
    procedure RggSpecialDoOnTrackBarChange; override;
  public
    FixPunkt: TPoint3D;

    MinValCaption: string;
    MaxValCaption: string;
    IstValCaption: string;
    ParamCaption: string;

    RefCtrl: TTrimmControls;

    ChartGraph: TChartModel;

    RiggLED: Boolean;
    StatusText: string;
    GrauZeichnen: Boolean;

    InitialFixPoint: TRiggPoint;

    UpdateTextCounter: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure UpdateGetriebe;
    procedure UpdateGraph;

    procedure Init;
    procedure InitStrokeRigg;
    procedure UpdateStrokeRigg;

    procedure Init420;
    procedure InitLogo;
    procedure InitWithDefaultDoc(AWantLogoData: Boolean; TargetSlot: Integer);
    procedure DoAfterInitDefault(ATrimmSlot: Integer);
    procedure InitSalingTyp(fa: Integer); override;

    procedure MemoryBtnClick;
    procedure MemoryRecallBtnClick;

    procedure Draw;

    function GetPlotValue(PlotID: Integer; x, y: single): single;

    procedure DebugBiegungGF(ML: TStrings);
    procedure UpdateColumnC(ML: TStrings);
    procedure UpdateColumnD(ML: TStrings);
    procedure UpdateTrimmText(ML: TStrings);
    procedure UpdateJsonText(ML: TStrings);
    procedure UpdateDataText(ML: TStrings);
    procedure UpdateDiffText(ML: TStrings);
    procedure UpdateFactArrayFromRigg;

    function Param2Text(P: TFederParam): string;
    function Text2Param(T: string): TFederParam;

    procedure ToggleRenderOption(fa: TFederAction);
    procedure SetParameter(fa: TFederAction);

    procedure ViewportChanged(Sender: TObject);

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
    property Visible: Boolean read FVisible write SetVisible;
    property Demo: Boolean read FDemo write SetDemo;

    property Korrigiert: Boolean read FKorrigiert write SetKorrigiert;
    property SofortBerechnenNoChange: Boolean read FSofortBerechnen write FSofortBerechnen;
    property SofortBerechnen: Boolean read FSofortBerechnen write SetSofortBerechnen;
    property BtnGrauDown: Boolean read FBtnGrauDown write SetBtnGrauDown;
    property BtnBlauDown: Boolean read FBtnBlauDown write SetBtnBlauDown;

    property GraphRadio: TGraphRadio read FGraphRadio write SetSuperRadio;

    property OnUpdateGraph: TNotifyEvent read FOnUpdateGraph write SetOnUpdateGraph;
  end;

implementation

uses
  FMX.PlatForm;

const
  tfs = '%-3s %s %8s %6s';

{ TRggMain }

constructor TRggMain.Create;
begin
  inherited;

  InitialFixPoint := ooD;

  Demo := False;

  FParam := fpVorstag;
  FKorrigiert := True;
  FSofortBerechnen := False;
  FBtnGrauDown := True;
  FBtnBlauDown := False;
  FBogen := True;
  FKoppel := False;

  FGraphRadio := gSimple;

  FactArray := Rigg.GSB;
  Rigg.ControllerTyp := ctOhne;
  Init;
end;

destructor TRggMain.Destroy;
begin
  Rigg.Free;
  inherited;
end;

procedure TRggMain.Init;
begin
  if not Assigned(RggTrackbar) then
    Exit;

  RggTrackbar.OnChange := TrackBarChange;

  InitFactArray;

  Param := fpVorstag;
  FixPoint := InitialFixPoint;

  InitStrokeRigg;
end;

procedure TRggMain.InitStrokeRigg;
begin
  if StrokeRigg <> nil then
  begin
    StrokeRigg.SalingTyp := Rigg.SalingTyp;
    StrokeRigg.ControllerTyp := Rigg.ControllerTyp;
    StrokeRigg.Koordinaten := Rigg.rP;
    StrokeRigg.KoordinatenE := Rigg.rPE;
    StrokeRigg.KoordinatenR := Rigg.rP;
    StrokeRigg.SetMastLineData(Rigg.MastLinie, Rigg.lc, Rigg.beta);
    StrokeRigg.WanteGestrichelt := not Rigg.GetriebeOK;
  end;
end;

procedure TRggMain.UpdateStrokeRigg;
begin
  if StrokeRigg <> nil then
  begin
    StrokeRigg.SofortBerechnen := SofortBerechnen;
    StrokeRigg.GrauZeichnen := GrauZeichnen;
    StrokeRigg.BtnGrauDown := BtnGrauDown;
    StrokeRigg.BtnBlauDown := BtnBlauDown;
    StrokeRigg.RiggLED := RiggLED;

    if Rigg.SalingTyp > stDrehbar then
      StrokeRigg.Koppel := False
    else
      StrokeRigg.Koppel := Koppel;

    StrokeRigg.Bogen := Bogen;
    // if (FParam <> fpWinkel) then StrokeRigg.Bogen := False;

    StrokeRigg.WanteGestrichelt := not Rigg.GetriebeOK;

    StrokeRigg.Koordinaten := Rigg.rP;
    StrokeRigg.KoordinatenE := Rigg.rPe;
    StrokeRigg.SetKoppelKurve(Rigg.KoppelKurve);
    StrokeRigg.SetMastLineData(Rigg.MastLinie, Rigg.lc, Rigg.beta);

    StrokeRigg.DoOnUpdateStrokeRigg;
  end;
end;

procedure TRggMain.SetParameter(fa: TFederAction);
begin
  FAction := fa;
  case fa of
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

    faParamT1: Param := fpT1;
    faParamT2: Param := fpT2;

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
      UpdateGetriebe
    else
      Draw;
  end;
end;

procedure TRggMain.SetBtnBlauDown(const Value: Boolean);
begin
  if FBtnBlauDown <> Value then
  begin
    FBtnBlauDown := Value;
    Draw;
  end;
end;

procedure TRggMain.SetBtnGrauDown(const Value: Boolean);
begin
  if FBtnGrauDown <> Value then
  begin
    FBtnGrauDown := Value;
    if Value then
      Draw
    else
      UpdateGetriebe;
  end;
end;

procedure TRggMain.SetCurrentValue(const Value: single);
begin
  FactArray.Find(FParam).Ist := Value;
end;

procedure TRggMain.SetDemo(const Value: Boolean);
begin
  FDemo := Value;
  if FDemo then
  begin
    Rigg.SetDefaultDocument;
    InitFactArray;
    SetParam(FParam);
  end;
  UpdateText;
end;

procedure TRggMain.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  if StrokeRigg <> nil then
    StrokeRigg.ViewPoint := Value;
end;

procedure TRggMain.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
  FixPunkt := Rigg.rP.V[Value];
  if StrokeRigg <> nil then
    StrokeRigg.FixPoint := Value;
end;

procedure TRggMain.SetHullVisible(const Value: Boolean);
begin
  if (Value <> FHullVisible) and (StrokeRigg <> nil) then
  begin
    FHullVisible := Value;
    StrokeRigg.HullVisible := Value;
    Draw;
  end;
end;

procedure TRggMain.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  Draw;
end;

procedure TRggMain.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  Draw;
end;

procedure TRggMain.SetKorrigiert(const Value: Boolean);
begin
  if (Value <> FKorrigiert) then
  begin
    FKorrigiert := Value;
    Rigg.Korrigiert := Value;
    UpdateGetriebe;
  end;
end;

procedure TRggMain.SetOnUpdateGraph(const Value: TNotifyEvent);
begin
  FOnUpdateGraph := Value;
end;

procedure TRggMain.UpdateGraph;
begin
  ChangeRigg(CurrentValue);
  case FParam of
    fpController,
    fpWinkel,
    fpVorstag,
    fpWante,
    fpWoben,
    fpSalingH,
    fpSalingA,
    fpSalingL,
    fpSalingW,
    fpVorstagOS,
    fpWPowerOS,
    fpMastfallVorlauf:
    begin
      UpdateGetriebe;
    end;

    fpMastfallF0C,
    fpMastfallF0F,
    fpBiegung:
    begin
      UpdateGetriebe;
      Rigg.Schnittkraefte;
    end;

    fpD0X:
    begin
      Rigg.Reset;
      UpdateGetriebe;
    end;

    fpT1, fpT2:
    begin
      Draw;
    end;

    fpAPW:
    begin
      if ChartGraph <> nil then
      begin
        ChartGraph.APWidth := Round(CurrentValue);
        ChartGraph.UpdateXMinMax;
      end;
    end;

    fpEAH:
    begin
      UpdateEAH(CurrentValue);
      UpdateGetriebe;
    end;

    fpEAR:
    begin
      UpdateEAR(CurrentValue);
      UpdateGetriebe;
    end;

    fpEI:
    begin
      Rigg.MastEI := Round(CurrentValue);
      UpdateGetriebe;
    end;

  end;
end;

procedure TRggMain.ChangeRigg(Value: single);
var
  tempH, tempA, tempL, tempW: single;
begin
  case FParam of
    fpController: Rigg.RealGlied[fpController] := Value;

    fpWinkel: Rigg.RealGlied[fpWinkel] := Value * pi / 180;

    fpVorstag: Rigg.RealGlied[fpVorstag] := Value;
    fpWante: Rigg.RealGlied[fpWante] := Value;
    fpWoben: Rigg.RealGlied[fpWoben] := Value;

    fpSalingH:
    begin
      tempH := FactArray.SalingH.Ist;
      tempA := FactArray.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := arctan2(tempH * 2, tempA) * 180 / pi;

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      // SalingH no change (just changed)
      // SalingA no change (kept unchanged)
      FactArray.SalingL.Ist := tempL;
      FactArray.SalingW.Ist := tempW;
    end;

    fpSalingA:
    begin
      tempH := FactArray.SalingH.Ist;
      tempA := FactArray.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := arctan2(tempH * 2, tempA) * 180 / pi;

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      // SalingH no change (kept unchanged)
      // SalingA no change (just changed)
      FactArray.SalingL.Ist := tempL;
      FactArray.SalingW.Ist := tempW;
    end;

    fpSalingL:
    begin
      tempW := FactArray.SalingW.Ist;
      tempL := FactArray.SalingL.Ist;
      tempH := tempL * sin(tempW * pi / 180);
      tempA := 2 * tempL * cos(tempW * pi / 180);

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      FactArray.SalingH.Ist := tempH;
      FactArray.SalingA.Ist := tempA;
      // SalingL no change (just changed)
      // SalingW no change (kept unchanged)
    end;

    fpSalingW:
    begin
      tempW := FactArray.SalingW.Ist;
      tempL := FactArray.SalingL.Ist;
      tempH := tempL * sin(tempW * pi / 180);
      tempA := 2 * tempL * cos(tempW * pi / 180);

      Rigg.RealGlied[fpSalingH] := tempH;
      Rigg.RealGlied[fpSalingA] := tempA;
      Rigg.RealGlied[fpSalingL] := tempL;

      FactArray.SalingH.Ist := tempH;
      FactArray.SalingA.Ist := tempA;
      // SalingL no change
      // SalingW no change
    end;

    fpMastfallF0F:
      Rigg.NeigeF(Value - FactArray.MastfallVorlauf.Ist);

    fpMastfallF0C:
      Rigg.BiegeUndNeigeC(Value, FactArray.Biegung.Ist);

    fpMastfallVorlauf:
      Rigg.MastfallVorlauf := Value;

    fpBiegung:
      Rigg.BiegeUndNeigeC(FactArray.MastfallF0C.Ist, Value);

    fpD0X:
      Rigg.rP.D0.X := Round(Value);
  end;
end;

procedure TRggMain.SetParamValue(idx: TFederParam; Value: single);
var
  sb: TRggSB;
begin
  sb := FactArray.Find(idx);
  if Assigned(sb) then
  begin
    if Value = CurrentValue then
      { do nothing }
    else if Value >= sb.Max then
      sb.Ist := sb.Max
    else if Value <= sb.Min then
      sb.Ist := sb.Min
    else
      sb.Ist := Value;

    UpdateGraph;
  end;
end;

function TRggMain.GetCurrentValue: single;
begin
  result := FactArray.Find(FParam).Ist;
end;

function TRggMain.GetHullVisible: Boolean;
begin
  result := FHullVisible;
  if StrokeRigg <> nil then
  begin
    result := StrokeRigg.QueryRenderOption(faRggHull);
    if result <> FHullVisible then
      FHullVisible := result;
  end;
end;

function TRggMain.GetMastfall: string;
begin
  result := Format('%.0f', [FactArray.MastfallF0F.Ist - FactArray.MastfallVorlauf.Ist]);
end;

function TRggMain.GetParamValue(idx: TFederParam): single;
begin
  result := FactArray.Find(idx).Ist;
end;

function TRggMain.GetParamValueString(fp: TFederParam): string;
begin
  result := Format('%.0f', [FactArray.Find(fp).Ist]);
end;

function TRggMain.GetParamValueStringDiff(fp: TFederParam): string;
var
  tv: single;
  fd: TRggData;
begin
  fd := Trimm0;
  tv := FactArray.Find(fp).Ist;
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

procedure TRggMain.SetParam(Value: TFederParam);
begin
  { make sure the 'pseudo-param' faPan gets cancelled }
  FAction := faNoop;
  if Demo then
  begin
    InitFactArray;
    ChangeRigg(FactArray.Find(FParam).Ist); // Istwert zurücksetzen
    Rigg.RealGlied[fpVorstag] := FactArray.Vorstag.Ist;
  end;

  if Value = fpWinkel then
  begin
    { Wanten straff ziehen }
    case Rigg.SalingTyp of
      stFest:
        Rigg.MakeSalingHBiggerFS(FactArray.SalingH.Ist);
      stDrehbar:
        Rigg.MakeSalingLBiggerDS(FactArray.SalingL.Ist);
    end;
  end;

  if Value = fpController then
    Rigg.ControllerTyp := ctDruck
  else
    Rigg.ControllerTyp := ctOhne;

  if Assigned(StrokeRigg) then
  begin
    StrokeRigg.ControllerTyp := Rigg.ControllerTyp;
  end;

  Rigg.ManipulatorMode := (Value = fpWinkel);
  FParam := Value;
  CurrentValue := FactArray.Find(FParam).Ist;
  SetupTrackBarForRgg;
  UpdateGraph;
end;

function TRggMain.Text2Param(T: string): TFederParam;
begin
  result := fpT1;
  if T = ControllerString then
    result := fpController
  else if T = WinkelString then
    result := fpWinkel
  else if T = VorstagString then
    result := fpVorstag
  else if T = WanteString then
    result := fpWante
  else if (T = WanteObenString) or (T = 'Woben') then
    result := fpWoben
  else if (T = SalingHString) or (T = 'SalingH') then
    result := fpSalingH
  else if (T = SalingAString) or (T = 'SalingA') then
    result := fpSalingA
  else if (T = SalingLString) or (T = 'SalingL') then
    result := fpSalingL
  else if (T = SalingWString) or (T = 'SalingW') then
    result := fpSalingW
  else if T = MastfallF0CString then
    result := fpMastfallF0C
  else if T = MastfallF0FString then
    result := fpMastfallF0F
  else if T = MastfallVorlaufString then
    result := fpMastfallVorlauf
  else if T = BiegungString then
    result := fpBiegung
  else if T = MastFootD0XString then
    result := fpD0X
  else if T = T1String then
    result := fpT1
  else if T = T2String then
    result := fpT2
  else if T = APWidthString then
    result := fpAPW
  else if T = EAHullString then
    result := fpEAH
  else if T = EARiggString then
    result := fpEAR
  else if T = EIMastString then
    result := fpEI
    ;
end;

function TRggMain.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := ControllerString
  else if P = fpWinkel then
    result := WinkelString
  else if P = fpVorstag then
    result := VorstagString
  else if P = fpWante then
    result := WanteString
  else if P = fpWoben then
    result := WanteObenString
  else if P = fpSalingH then
    result := SalingHString
  else if P = fpSalingA then
    result := SalingAString
  else if P = fpSalingL then
    result := SalingLString
  else if P = fpSalingW then
    result := SalingWString
  else if P = fpMastfallF0C then
    result := MastfallF0CString
  else if P = fpMastfallF0F then
    result := MastfallF0FString
  else if P = fpMastfallVorlauf then
    result := MastfallVorlaufString
  else if P = fpBiegung then
    result := BiegungString
  else if P = fpD0X then
    result := MastfootD0XString
  else if P = fpT1 then
    result := T1String
  else if P = fpT2 then
    result := T2String
  else if P = fpAPW then
    result := APWidthString
  else if P = fpEAH then
    result := EAHullString
  else if P = fpEAR then
    result := EARiggString
  else if P = fpEI then
    result := EIMastString
    ;
end;

procedure TRggMain.Reset;
begin
  Rigg.SetDefaultDocument;
  Rigg.ControllerTyp := ctOhne;
  InitFactArray;
  UpdateGraph;
end;

procedure TRggMain.InitFactArray;
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := Rigg.GSB.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := Rigg.GSB.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  FactArray.SalingW.Ist := Round(180 / pi * arctan2(tempH * 2, tempA));

  FactArray.MastfallF0C.Ist := Rigg.RealTrimm[tiMastfallF0C];
  FactArray.MastfallF0F.Ist := Rigg.RealTrimm[tiMastfallF0F];
  FactArray.Biegung.Ist := Rigg.RealTrimm[tiBiegungS];
  FactArray.D0X.Ist := Rigg.rP.D0.X;

  FactArray.T1.Ist := 650;
  FactArray.T2.Ist := 150;

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  { speziell überschreiben }
  if WantLogoData then
  begin
    FactArray.Controller.Min := 50;
    FactArray.Winkel.Min := 70;
    FactArray.Winkel.Max := 120;
    // FactArray.Woben.Min := 2000;
    // FactArray.Woben.Max := 2100;
    FactArray.SalingW.Min := 40;
    FactArray.SalingW.Max := 60;
    // FactArray.MastfallF0F.Max := 6400;
    FactArray.Biegung.Min := 0;
    FactArray.Biegung.Max := 120;
  end
  else
  begin
    FactArray.Controller.Min := 50;

    FactArray.Wante.Min := 4020;
    FactArray.Wante.Max := 4220;

    FactArray.Vorstag.Min := 4200;
    FactArray.Vorstag.Max := 5000;

    FactArray.Winkel.Min := 80;
    FactArray.Winkel.Max := 115;

    FactArray.Woben.Min := 2000;
    FactArray.Woben.Max := 2100;

    FactArray.SalingH.Min := 170;
    FactArray.SalingH.Max := 1020;

    FactArray.SalingA.Min := 250;
    FactArray.SalingA.Max := 1550;

    FactArray.SalingL.Ist := 480;
    FactArray.SalingL.Min := 240;
    FactArray.SalingL.Max := 1200;

    FactArray.SalingW.Min := 15;
    FactArray.SalingW.Max := 87;

    FactArray.D0X.Min := 2600;
    FactArray.D0X.Ist := 2870;
    FactArray.D0X.Max := 3300;

    FactArray.MastfallF0C.Min := 4000;
    FactArray.MastfallF0C.Ist := 4800;
    FactArray.MastfallF0C.Max := 5100;

    FactArray.MastfallF0F.Min := 5370;
    FactArray.MastfallF0F.Ist := 6070;
    FactArray.MastfallF0F.Max := 6570;

    FactArray.MastfallVorlauf.Min := 4950;
    FactArray.MastfallVorlauf.Ist := 5000;
    FactArray.MastfallVorlauf.Max := 5150;

    FactArray.Biegung.Min := 0;
    FactArray.Biegung.Max := 500;

    FactArray.T1.Save;
    FactArray.T2.Save;
    FactArray.ResetVolatile;
  end;
end;

procedure TRggMain.UpdateFactArrayFromRigg;
var
  i: TFederParam;
  sb: TRggSB;
begin
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    case i of
      fpController:
        sb.Ist := Rigg.RealGlied[fpController];
      fpWinkel:
        sb.Ist := Rigg.RealGlied[fpWinkel] * 180 / pi;
      fpVorstag:
        sb.Ist := Rigg.RealGlied[fpVorstag];
      fpWante:
        sb.Ist := Rigg.RealGlied[fpWante];
      fpWoben:
        sb.Ist := Rigg.RealGlied[fpWoben];
      fpSalingH:
        sb.Ist := Rigg.RealGlied[fpSalingH];
      fpSalingA:
        sb.Ist := Rigg.RealGlied[fpSalingA];
      fpSalingL:
        sb.Ist := Rigg.RealGlied[fpSalingL];
      fpSalingW:
        sb.Ist := arctan2(Rigg.RealGlied[fpSalingH] * 2, Rigg.RealGlied[fpSalingA]) * 180 / pi;
      fpMastfallF0C:
        sb.Ist := Rigg.rP.F0.Distance(Rigg.rP.C);
      fpMastfallF0F:
        sb.Ist := Rigg.rP.F0.Distance(Rigg.rP.F);
      fpBiegung:
        sb.Ist := Rigg.hd;
      fpD0X:
        sb.Ist := Rigg.rP.D0.X;
    end;
  end;

  if Param <> fpWinkel then
  begin
    sb := FactArray.Find(fpWinkel);
    sb.Ist := Rigg.RealGlied[fpWinkel] * 180 / pi;
  end;
end;

function TRggMain.GetPlotValue(PlotID: Integer; x, y: single): single;
var
  tx, ty: single;
begin
  case PlotID of
    1..12:
    begin
      tx := FactArray.Vorstag.Ist;
      ty := FactArray.SalingL.Ist;
      Rigg.RealGlied[fpVorstag] := tx + x;
      Rigg.RealGlied[fpSalingA] := ty + y / 10;
      Rigg.UpdateGetriebe;
      if Rigg.GetriebeOK then
      begin
        result := Rigg.rP.F0.Distance(Rigg.rP.F);
        UpdateFactArrayFromRigg;
      end
      else
        result := 0;
    end;
    else
      result := 0;
  end;
end;

procedure TRggTrimm.LoadTrimm(fd: TRggData);
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
  Rigg.SetDefaultDocument;
  Rigg.LoadFromFederData(fd);

//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := Rigg.GSB.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := Rigg.GSB.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  FactArray.SalingW.Ist := Round(180 / pi * arctan2(tempH * 2, tempA));

  FactArray.MastfallF0C.Ist := Rigg.RealTrimm[tiMastfallF0C];
  FactArray.MastfallF0F.Ist := Rigg.RealTrimm[tiMastfallF0F];
  FactArray.Biegung.Ist := Rigg.RealTrimm[tiBiegungS];
  FactArray.D0X.Ist := Rigg.rP.D0.X;

  fd.F0C := Round(FactArray.MastfallF0C.Ist);
  fd.F0F := Round(FactArray.MastfallF0F.Ist);
  fd.Bie := Round(FactArray.Biegung.Ist);

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := FactArray.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  FactArray.Controller.Min := fd.CPMin;
  FactArray.Controller.Max := fd.CPMax;

  FactArray.Wante.Min := fd.WLMin;
  FactArray.Wante.Max := fd.WLMax;

  FactArray.Vorstag.Min := fd.VOMin;
  FactArray.Vorstag.Max := fd.VOMax;

  FactArray.Winkel.Min := fd.WIMin;
  FactArray.Winkel.Max := fd.WIMax;

  FactArray.Woben.Min := fd.WOMin;
  FactArray.Woben.Max := fd.WOMax;

  FactArray.SalingH.Min := fd.SHMin;
  FactArray.SalingH.Max := fd.SHMax;

  FactArray.SalingA.Min := fd.SAMin;
  FactArray.SalingA.Max := fd.SAMax;

  FactArray.SalingL.Min := fd.SLMin;
  FactArray.SalingL.Max := fd.SLMax;

  FactArray.SalingW.Min := fd.SWMin;
  FactArray.SalingW.Max := fd.SWMax;

  FactArray.D0X.Min := fd.D0X - 100;
  FactArray.D0X.Max := fd.D0X + 100;

  FactArray.MastfallVorlauf.Ist := fd.MV;
  FactArray.MastfallVorlauf.Min := fd.MV - 100;
  FactArray.MastfallVorlauf.Max := fd.MV + 100;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
//  tempH := FactArray.MastfallF0C.Ist;
//  temp := tempA - tempH; // = 0
  temp := FactArray.MastfallF0C.Ist;
  FactArray.MastfallF0C.Min := temp - 700;
  FactArray.MastfallF0C.Max := temp + 500;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
//  tempH := FactArray.MastfallF0F.Ist;
//  temp := tempA - tempH; // = 0
  temp := FactArray.MastfallF0F.Ist;
  FactArray.MastfallF0F.Min := temp - 700;
  FactArray.MastfallF0F.Max := temp + 500;

  FactArray.Biegung.Min := 0;
  FactArray.Biegung.Max := 500;

  FactArray.T1.Save;
  FactArray.T2.Save;
  FactArray.ResetVolatile;

  SetParam(FParam);
end;

procedure TRggTrimm.SaveTrimm(fd: TRggData);
begin
  Rigg.SaveToFederData(fd);

  fd.CPMin := Round(FactArray.Controller.Min);
  fd.CPPos := Round(FactArray.Controller.Ist);
  fd.CPMax := Round(FactArray.Controller.Max);

  fd.SHMin := Round(FactArray.SalingH.Min);
  fd.SHPos := Round(FactArray.SalingH.Ist);
  fd.SHMax := Round(FactArray.SalingH.Max);

  fd.SAMin := Round(FactArray.SalingA.Min);
  fd.SAPos := Round(FactArray.SalingA.Ist);
  fd.SaMax := Round(FactArray.SalingA.Max);

  fd.SLMin := Round(FactArray.SalingL.Min);
  fd.SLPos := Round(FactArray.SalingL.Ist);
  fd.SLMax := Round(FactArray.SalingL.Max);

  fd.SWMin := Round(FactArray.SalingW.Min);
  fd.SWPos := Round(FactArray.SalingW.Ist);
  fd.SWMax := Round(FactArray.SalingW.Max);

  fd.VOMin := Round(FactArray.Vorstag.Min);
  fd.VOPos := Round(FactArray.Vorstag.Ist);
  fd.VOMax := Round(FactArray.Vorstag.Max);

  fd.WIMin := Round(FactArray.Winkel.Min);
  fd.WIPos := Round(FactArray.Winkel.Ist);
  fd.WIMax := Round(FactArray.Winkel.Max);

  fd.WLMin := Round(FactArray.Wante.Min);
  fd.WLPos := Round(FactArray.Wante.Ist);
  fd.WLMax := Round(FactArray.Wante.Max);

  fd.WOMin := Round(FactArray.Woben.Min);
  fd.WOPos := Round(FactArray.Woben.Ist);
  fd.WOMax := Round(FactArray.Woben.Max);
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
  InitFactArray();
  if StrokeRigg <> nil then
    StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
//  FixPoint := ooD;

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
  if StrokeRigg <> nil then
    StrokeRigg.SalingTyp := Rigg.SalingTyp;
  SetParam(FParam);
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

  AL(VOString, fpVorstag);
  AL(WAString, fpWante);
  AL(WOString, fpWoben);
  AL(SHString, fpSalingH);
  AL(SAString, fpSalingA);
  AL(SLString, fpSalingL);
  AL(SWString, fpSalingW);
  AL(MVString, fpMastfallVorlauf);

  BL(MFString, Mastfall);
  AL(F0FString, fpMastfallF0F);
  AL(F0CString, fpMastfallF0C);
  AL(BieString, fpBiegung);
  BL(BGFString, FormatValue(BiegungGF));
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
  sb := FactArray.Find(Param);
  tb.Min := -200; // smaller than the smallest Value
  tb.Max := sb.Max;
  tb.ValueNoChange := temp;
  tb.Min := sb.Min;
  tb.Frequency := sb.SmallStep;
  tb.WantMultiDelta := sb.IsMulti;

  { get unit string }
  case Param of
    fpWinkel, fpSalingW: us := GradString;
    fpEAH, fpEAR: us := KiloNewtonString;
    fpEI: us := NewtonMeterSquareString;
    fpT1, fpT2: us := '';
  else
    us := MilimeterString;
  end;

  { get format string }
  fs := NoDigitRealPlusUnitFormatString;

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
    fpWinkel, fpSalingW: us := GradString;
    fpEAH, fpEAR: us := KiloNewtonString;
    fpEI: us := NewtonMeterSquareString;
    fpT1, fpT2: us := '';
  else
    us := MilimeterString;
  end;

  fs := NoDigitRealPlusUnitFormatString;
  IstValCaption := Format(fs, [RggTrackbar.Value, us]);

  ParamValue[FParam] := Round(RggTrackbar.Value);
end;

procedure TRggMain.Draw;
begin
  if StrokeRigg <> nil then
  begin
    UpdateStrokeRigg;
    StrokeRigg.Draw;
  end;
  UpdateFactArrayFromRigg;
  UpdateText;
end;

procedure TRggMain.ToggleRenderOption(fa: TFederAction);
begin
  if StrokeRigg <> nil then
    StrokeRigg.ToggleRenderOption(fa);
  Draw;
end;

procedure TRggMain.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TRggMain0 }

constructor TRggWheel.Create;
begin
  inherited;
  RggTrackbar := TFederTrackbar.Create;
end;

destructor TRggWheel.Destroy;
begin
  RggTrackbar.Free;
  inherited;
end;

procedure TRggWheel.TrackBarChange(Sender: TObject);
begin
  if FParam = fpT1 then
  begin
    FactArray.T1.Ist := Round(RggTrackbar.Value);
    StrokeRigg.UpdateHullTexture;
  end
  else if FParam = fpT2 then
  begin
    FactArray.T2.Ist := Round(RggTrackbar.Value);
    StrokeRigg.UpdateHullTexture;
  end;
  RggSpecialDoOnTrackBarChange;
end;

procedure TRggWheel.RggSpecialDoOnTrackBarChange;
begin
end;

procedure TRggWheel.DoRasterWheelRG(Delta: single);
begin
end;

procedure TRggWheel.DoSmallWheelRG(Delta: single);
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

procedure TRggWheel.DoBigWheelRG(Delta: single);
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

procedure TRggWheel.DoWheel(Delta: single);
begin
  RggTrackbar.Delta := Delta;
  UpdateOnParamValueChanged;
end;

procedure TRggMain.ViewportChanged(Sender: TObject);
begin
  if IsUp then
    UpdateText;
end;

function TRggWheel.GetBigStep: single;
begin
  case FParam of
    fpWante: result := 2;
    fpWinkel: result := 1;
    fpSalingW: result := 1;
    else
      result := 10;
  end;
end;

function TRggWheel.GetSmallStep: single;
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
    pf := Rigg.rP.F;

    IndexG := 13;
    IndexH := 40;

    kg := StrokeRigg.GetMastKurvePoint(IndexG);
    kh := StrokeRigg.GetMastKurvePoint(IndexH);

    a := kg.Distance(pf);
    b := pf.Distance(kh);
    c := kh.Distance(kg);

    h := Hoehe(a-0.00001, b, c, k);

    BiegungGFDiff := BiegungGF-h;
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
    pf := Rigg.rP.F;

    bm := BogenMax;
    l := Rigg.rL.DC + Rigg.rL.D0D;

    ML.Add('');
    t := 1160;
    ML.Add('G = 1160');
    IndexG := bm * t / l;
    ML.Add(Format('iG = %.2f', [IndexG]));
    ML.Add('IndexG := 13;');

    ML.Add('');
    t := Rigg.rL.D0D;
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
    h := Hoehe(a-0.00001, b, c, k);
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

  ML.Add('CounterG = ' + IntToStr(Rigg.UpdateGetriebeCounter));
end;

procedure TRggMain.UpdateJsonText(ML: TStrings);
begin
  ML.Text := TrimmJson;
end;

procedure TRggMain.UpdateDataText(ML: TStrings);
begin
  ML.Text := TrimmData;
end;

procedure TRggMain.UpdateGetriebe;
var
  temp: Boolean;
begin
  GrauZeichnen := False;
  RiggLED := False;
  StatusText := '';

  Rigg.UpdateGetriebe;

  temp := (SofortBerechnen and Rigg.GetriebeOK and Rigg.MastOK);

  if temp then
  begin
    { continue to do Rigg }
    Rigg.UpdateRigg;

    RiggLED := Rigg.RiggOK;
    StatusText := Rigg.RiggStatusText;
    Grauzeichnen := RiggLED;
  end
  else
  begin
    { be done with Getriebe only }
    RiggLED := Rigg.GetriebeOK;
    StatusText := Rigg.GetriebeStatusText;
    if Rigg.GetriebeOK and not Rigg.MastOK then
    begin
      RiggLED := False;
      StatusText := Rigg.MastStatusText;
    end;
  end;

  Draw;
end;

procedure TRggMain.MemoryBtnClick;
begin
{$ifdef debug}
  Logger.Info('in MemoryBtnClick');
{$endif}
  RefCtrl := Rigg.Glieder;
  StrokeRigg.KoordinatenR := Rigg.rP;
  Draw;
end;

procedure TRggMain.MemoryRecallBtnClick;
begin
{$ifdef debug}
  Logger.Info('in MemoryRecallBtnClick');
{$endif}
  Rigg.Glieder := RefCtrl;
  UpdateGetriebe;
end;

procedure TRggMain.UpdateEAR(Value: single);
var
  EA: TRiggLvektor; { EA in KN }
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
  EA: TRiggLvektor; { EA in KN }
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

{ TRggTrimm }

procedure TRggTrimm.SetTrimmNoChange(const Value: Integer);
begin
  Logger.Info('SetTrimmNoChange: ' + IntToStr(Value));
  FTrimm := Value;
end;

procedure TRggTrimm.SetTrimm(const Value: Integer);
begin
  Logger.Info('SetTrimm: ' + IntToStr(Value));
  FTrimm := Value;
  LoadTrimm(CurrentTrimm);
  InitSalingTyp(faSalingTypFest);
  UpdateOnParamValueChanged;
end;

constructor TRggTrimm.Create;
begin
  inherited;
  RggData := TRggData.Create;
  RggData.Name := 'fd';

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

  { this should be done after or when calling Init }
//  InitLogo; // sets WantLogoData to true
//  Init420; // resets WantLogo to false
//  WantLogoData := False;
end;

destructor TRggTrimm.Destroy;
begin
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
  inherited;
end;

function TRggTrimm.GetCurrentTrimm: TRggData;
begin
  result := GetTrimmItem(FTrimm);
end;

function TRggTrimm.GetTrimmItem(i: Integer): TRggData;
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

function TRggTrimm.GetTrimmItemReport(ReportID: Integer): string;
begin
  if Assigned(Rigg) and Assigned(RggData) and Assigned(FL) then
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

function TRggTrimm.GetTrimmItemReportLong: string;
begin
  result := GetTrimmItemReport(3);
end;

function TRggTrimm.GetTrimmItemReportShort: string;
begin
  result := GetTrimmItemReport(2);
end;

function TRggTrimm.GetTrimmItemReportJson: string;
begin
  result := GetTrimmItemReport(1);
end;

function TRggTrimm.GetTrimmItemReportData: string;
begin
  result := GetTrimmItemReport(0);
end;

procedure TRggTrimm.InitTrimmData;
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

{ TRggText }

constructor TRggText.Create;
begin
  inherited;
  FL := TStringList.Create;
  Logger := TLogger.Create;
end;

destructor TRggText.Destroy;
begin
  Logger.Free;
  FL.Free;
  inherited;
end;

function TRggText.GetFLText: string;
begin
  result := FL.Text;
end;

procedure TRggText.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

procedure TRggText.UpdateOnParamValueChanged;
begin

end;

procedure TRggText.UpdateText(ClearFlash: Boolean);
begin

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

  UpdateGetriebe;
end;

end.
