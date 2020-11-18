unit RggChartModel;

interface

uses
  RiggVar.FB.ActionConst,
  RggStrings,
  RggUnit4,
  RggTypes,
  RggDoc,
  RggSaling3Eck,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors;

const
  ANr = 6; { MaxCurves, maximale Anzahl Kurven, d.h. berechneter Y Werte }
  PNr = 5; { MaxParamCount, maximale Anzahl der Werte des Parameters }
  VNr = 14; { CountOfAvailableCurves, Anzahl der zur Auswahl stehenden Y Werte }
  LNr = 50; { CountOfPointsInPolyLine - 1, Anzahl der Punkte im Diagramm - 1 }
  ErrorIndex = 999;
  D180 = 180 / PI;
  P180 = PI / 180;

type
  TLineDataR = TLineDataR50; { needs to match LNr }

  TxpName = (
    xpController,
    xpWinkel,
    xpVorstag,
    xpWante,
    xpWoben,
    xpSalingH,
    xpSalingA,
    xpSalingL,
    xpVorstagOS,
    xpWPowerOS,
    xpSalingW);

  TxpNameSet = set of TXpName;

  TChartStatus = (csBerechnet, csGeladen);
  TYLineArray = array[0..ANr-1] of TLineDataR;
  TYAchseSortedList = array[0..VNr-1] of TYAchseValue;
  TYAchseSet = set of TYAchseValue; { die berechneten Kurven }
  TYAchseStringArray = array[0..PNr-1] of string;

  TChartModel = class
  public
    FSalingTyp: TSalingTyp;

    Rigg: TRigg;
    RggDocument: TRggDocument;
    SalingDreieck: TSalingDreieck;

    procedure InitRigg;
    procedure UpdateGetriebe;
  public
    ProgressPosition: Integer;
    ProgressCaption: string;
    MemoLines: TStringList;
    MemoCounter: Integer;
    procedure GetMemoText;
  public
    XSet: TxpNameSet;
    PSet: TxpNameSet;

    XComboItems: TStrings;
    PComboItems: TStrings;
    YComboItems: TStrings;

    XComboItemIndex: Integer;
    PComboItemIndex: Integer;
    YComboItemIndex: Integer;

    procedure InitXComboItems;
    procedure InitYComboItems;
    procedure InitPComboItems;

    procedure UpdateXCombo(SalingTyp: TSalingTyp);
    procedure UpdatePCombo(SalingTyp: TSalingTyp);

    procedure XComboChange(Sender: TObject);
    procedure PComboChange(Sender: TObject);
    procedure YComboChange(Sender: TObject);

    function XComboSelectedText: string;
    function PComboSelectedText: string;
    function YComboSelectedText: string;

    function GetXText(Text: string): string;
    function GetPText(Text: string): string;
  public
    YLEDFillColor: TAlphaColor;
    PLEDFillColor: TAlphaColor;
    XLEDFillColor: TAlphaColor;

    XminEditText: string;
    XmaxEditText: string;
    PMinEditText: string;
    PMaxEditText: string;
    YMaxEditText: string;
    YMinEditText: string;

    PMinEditValue: Integer;
    PMaxEditValue: Integer;

    FXTextClicked: string;
    FPTextClicked: string;

    procedure LookForYMinMax;

    procedure UpdateXMinMax; virtual;
    procedure UpdatePMinMax; virtual;
    procedure UpdateYMinMax; virtual;
  public
    APWidth: Integer;
    FAP: Boolean;
    procedure SetAP(const Value: Boolean);
    property AP: Boolean read FAP write SetAP;
  public
    APSpinnerValue: Integer;
    APSpinnerMax: Integer;

    PSpinnerValue: Integer;
    PSpinnerMax: Integer;

    KurvenZahlSpinnerValue: Integer;
    KurvenZahlSpinnerMax: Integer;
    DefaultKurvenZahl: Integer;
    UserSelectedKurvenZahl: Integer;

    procedure InitSpinner;
  public
    GroupKurvenZahl: Integer;
    ParamCount: Integer;
    FShowGroup: Boolean;
    GroupText: TYAchseStringArray;
    property ShowGroup: Boolean read FShowGroup write FShowGroup;
  protected
    YAchseSet: TYAchseSet;
    YAchseRecordList: TYAchseRecordList;
    YAchseSortedList: TYAchseSortedList;
    function GetYText(Text: string): string;
    procedure UpdateYAchseList;
    procedure UpdateYAchseSet;
    function ComboIndexToCurve(ComboIndex: Integer): Integer;
  protected
    FDarkColors: Boolean;
    procedure SetDarkColors(const Value: Boolean);
    property DarkColors: Boolean read FDarkColors write SetDarkColors;
  public
    TopTitle: string;
    YTitle: string;
    XTitle: string;
    PTitle: string;

    Xmin: single;
    Xmax: single;
    Ymin: single;
    Ymax: single;

    XAchseMin: Integer;
    XAchseMax: Integer;
    ParamMin: Integer;
    ParamMax: Integer;

    XComboText: string;
    PComboText: string;
    XAchseText: string;
    ParamText: string;

    PText: TYAchseStringArray;
    PColorText: TYAchseStringArray;
  public
    TempF: TLineDataR;
    TestF: TLineDataR;

    {
      af is a fixed size array
      af holds the computed data
      af = array of array of TLineDataR
      af = array of array of (array of single)

      af = [0..PNr-1, 0..ANr-1, 0..LNr] or
      af = [0..4,     0..5,     0..50] or
      af = [Index of Parameter, Index of Curve, Index of Point in Curve]

      The loop to compute new values:
      for p := 0 to ParamCount-1 do
        for i := 0 to LNr-1 do
        begin
          j := Index of curve to be plotted
          af[p, j, i] := computed value from model;
        end;

      ParamCount can be PNr or less, but at least one.

      af = (z, y, x) // logical indices
      af = (p, j, i) // actual indices used
        The parameter planes are stacked vertical in z direction.
        The curve to be displayed in the chart is in horizontal xy plane.

      bf holds a copy of a plane in af for display (horizontal or vertical)
        vertical: plane xz or ip (standard)
          Set of curves of same type (selected) for different values of the param.
        horizontal: plane xy or ij
          Group of different curves for the same value of the selected param.
    }

    af: array[0..PNr-1] of TYLineArray; { computed data }
    bf: array[0..PNr-1] of TLineDataR; { copy of one plane in af, for display }
    cf: array[0..PNr-1] of TAlphaColor;
    ChartPunktX: double;
    procedure InitStraightLine;
    procedure GetCurves;
    procedure DrawInternal;
    procedure ShowTogether(ParamNo: Integer);
  private
    FOnActionHandled: TNotifyEvent;
    FOnUpdateAvailable: TNotifyEvent;
    procedure SetOnActionHandled(const Value: TNotifyEvent);
    procedure SetOnUpdateAvailable(const Value: TNotifyEvent);
  protected
    FValid: Boolean;
    FBusy: Boolean;
    FStatus: set of TChartStatus;
    function GetTsbName(Value: string): TxpName;
  public
    CalcCounter: Integer;
    procedure Reset;
    procedure Calc;

    procedure LoadNormal;
    procedure DrawNormal;
    procedure DrawTogether;
    procedure DrawGroup;

    function CheckBeforeCalc: Boolean; virtual;
    procedure DoAfterCalc;
  public
    WantRectangles: Boolean;
    WantTextRect: Boolean;
    WantLegend: Boolean;

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
  public
    IsUp: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure SuperInit;
    procedure SuperCalc;

    procedure Draw; virtual;
    procedure DoOnAction; virtual;

    property Valid: Boolean read FValid write FValid;
    property OnActionHandled: TNotifyEvent read FOnActionHandled write SetOnActionHandled;
    property OnUpdateAvailable: TNotifyEvent read FOnUpdateAvailable write SetOnUpdateAvailable;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.Classes,
  RiggVar.RG.Def,
  RggScroll;

procedure TChartModel.InitXComboItems;
var
  ML: TStrings;
begin
  ML := XComboItems;
  ML.Add(ControllerString);
  ML.Add(WinkelString);
  ML.Add(VorstagString);
  ML.Add(WanteString);
  ML.Add(WanteObenString);
  ML.Add(SalingHString);
  ML.Add(SalingAString);
end;

procedure TChartModel.InitPComboItems;
var
  ML: TStrings;
begin
  ML := PComboItems;
  ML.Add(SalingAString);
  ML.Add(SalingHString);
end;

procedure TChartModel.InitYComboItems;
var
  ML: TStrings;
begin
  ML := YComboItems;
  ML.Add(DurchbiegungHDString);
  ML.Add(ElasticityPointCString);
  ML.Add(MastfallF0CString);
  ML.Add(MastfallF0FString);
  ML.Add(VorstagSpannungString);
  ML.Add(WantenSpannungString);
end;

constructor TChartModel.Create;
begin
  UserSelectedKurvenZahl := 3;
  ParamCount := 3;
  APWidth := 30;
  FSalingTyp := stFest;

  Include(XSet, xpController);
  Include(XSet, xpWinkel);
  Include(XSet, xpVorstag);
  Include(XSet, xpWante);
  Include(XSet, xpWoben);
  Include(XSet, xpSalingH);
  Include(XSet, xpSalingA);
  Include(XSet, xpSalingL);
  Include(XSet, xpSalingW);

  PSet := XSet;

  DarkColors := False;

  PColorText[0] := BlueString;
  PColorText[1] := RedString;
  PColorText[2] := GreenString;
  PColorText[3] := WhiteString;
  PColorText[4] := YellowString;

  RggDocument := TRggDocument.Create;
  SalingDreieck := TSalingDreieck.Create;

  MemoLines := TStringList.Create;
  MemoLines.Add(AnfangsZustandString);

  XComboItems := TStringList.Create;
  YComboItems := TStringList.Create;
  PComboItems := TStringList.Create;

  InitSpinner;

  InitRigg;

  FXTextClicked := VorstagString;
  FPTextClicked := SalingHString;
  UpdateXCombo(FSalingTyp);
  InitYComboItems;
  YComboItemIndex := 3;
  InitYAchseRecordList(YAchseRecordList);
  UpdateYAchseList;

  AP := True;

  Reset;
end;

destructor TChartModel.Destroy;
begin
  RggDocument.Free;
  MemoLines.Free;
  SalingDreieck.Free;
  XComboItems.Free;
  YComboItems.Free;
  PComboItems.Free;
end;

procedure TChartModel.SetAP(const Value: Boolean);
begin
  FAP := Value;
  XLEDFillColor := claYellow;
  PLEDFillColor := claYellow;
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartModel.Reset;
begin
  FBusy := False;
  FValid := False;
  FStatus := [];
  YAchseSet := [];
  PText := PColorText;
  YLEDFillColor := claRed;
  XLEDFillColor := claRed;
  PLEDFillColor := claRed;
  DrawInternal;
  MemoLines.Clear;
  MemoLines.Add(ResetMessageString);
end;

procedure TChartModel.SetDarkColors(const Value: Boolean);
begin
  FDarkColors := Value;
  if FDarkColors then
  begin
    cf[0] := claBlue;
    cf[1] := claRed;
    cf[2] := claGreen;
    cf[3] := claWhite;
    cf[4] := claYellow;
  end
  else
  begin
    cf[0] := claAqua;
    cf[1] := claFuchsia;
    cf[2] := claLime;
    cf[3] := claWhite;
    cf[4] := claYellow;
  end;
end;

procedure TChartModel.SetOnActionHandled(const Value: TNotifyEvent);
begin
  FOnActionHandled := Value;
end;

procedure TChartModel.SetOnUpdateAvailable(const Value: TNotifyEvent);
begin
  FOnUpdateAvailable := Value;
end;

procedure TChartModel.InitStraightLine;
var
  i: Integer;
begin
  for i := 0 to LNr do
  begin
    TempF[i] := i;
    bf[0, i] := TempF[i];
    bf[1, i] := TempF[i] + 5;
    bf[2, i] := TempF[i] + 10;
    bf[3, i] := TempF[i] + 20;
    bf[4, i] := TempF[i] + 50;
  end;
  TestF := TempF;
end;

procedure TChartModel.UpdateYAchseList;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    YAchseRecordList[YAV].ComboIndex := -1;
  for i := 0 to YComboItems.Count-1 do
  begin
    s := YComboItems[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      if s = YAchseRecordList[YAV].ComboText then
      begin
        YAchseRecordList[YAV].ComboIndex := i;
        YAchseSortedList[i] := YAV;
        break;
      end;
  end;
end;

procedure TChartModel.UpdateYAchseSet;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  YAchseSet := [];
  for i := 0 to YComboItems.Count-1 do
  begin
    s := YComboItems[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      if s = YAchseRecordList[YAV].ComboText then
      begin
        YAchseRecordList[YAV].ArrayIndex := i;
        if i <= ANr-1 then
          Include(YAchseSet, YAV);
        break;
      end;
  end;
end;

function TChartModel.ComboIndexToCurve(ComboIndex: Integer): Integer;
var
  YAV: TYAchseValue;
begin
  if ComboIndex = -1 then
    YAV := YAchseSortedList[0]
  else
    YAV := YAchseSortedList[ComboIndex];

  if YAV in YAchseSet then
  begin
    if (csBerechnet in FStatus) or (csGeladen in FStatus) then
      FValid := True;
    result := YAchseRecordList[YAV].ArrayIndex;
  end
  else
  begin
    FValid := False;
    result := ErrorIndex;
  end;
end;

procedure TChartModel.Calc;
begin
  if not FBusy then
  begin
    Inc(CalcCounter);

    if not CheckBeforeCalc then
      Exit;

    FBusy := True;

    if PComboSelectedText = NoParamString then
    begin
      PMinEditValue := 0;
      PMaxEditValue := 0;
      KurvenZahlSpinnerValue := 1;
    end
    else if KurvenZahlSpinnerValue = 1 then
    begin
      KurvenZahlSpinnerValue := UserSelectedKurvenZahl;
    end;

    ParamCount := KurvenZahlSpinnerValue;

    if PSpinnerValue > ParamCount then
      PSpinnerValue := ParamCount;

    PSpinnerMax := ParamCount;
    if PSpinnerMax = 1 then
      PSpinnerMax := 2;

    if YComboItemIndex > ANr-1 then
      YComboItemIndex := 1;

    UpdateYAchseSet;
    GetCurves;

    DoAfterCalc;
    Rigg.GetDocument(RggDocument);
    GetMemoText;
    Include(FStatus, csBerechnet);
    YLEDFillColor := claLime;
    FBusy := False;

    LoadNormal;
    DrawInternal;
  end;
end;

procedure TChartModel.LoadNormal;
var
  j: Integer;
  p: Integer;
begin
  { Anzeigen }
  j := ComboIndexToCurve(YComboItemIndex);
  for p := 0 to ParamCount - 1 do
    bf[p] := af[p, j];
  UpdateYMinMax;
end;

procedure TChartModel.GetCurves;
var
  i, j, p: Integer;
  Antrieb, PAntrieb: double;
  Anfang, Ende, PAnfang, PEnde: double;
  InputRec: TTrimmControls;
  PunktOK: Boolean;
  temp, tempL, tempH, tempA: double;
  st: string;
  xn: TxpName;
  pn: TxpName;
begin
  xn := GetTsbName(XComboSelectedText);
  pn := GetTsbName(PComboSelectedText);

  { Getriebezustand sichern und verfügbar machen /
    record the model state and make it available, within this method }
  InputRec := Rigg.Glieder;

  case xn of
    xpController: ChartPunktX := InputRec.Controller;
    xpWinkel: ChartPunktX := InputRec.Winkel;
    xpVorstag: ChartPunktX := InputRec.Vorstag;
    xpWante: ChartPunktX := InputRec.Wanten;
    xpWoben: ChartPunktX := InputRec.Woben;
    xpSalingH: ChartPunktX := InputRec.SalingH;
    xpSalingA: ChartPunktX := InputRec.SalingA;
    xpSalingL: ChartPunktX := InputRec.SalingL;
    xpVorstagOS: ChartPunktX := InputRec.Vorstag;
  end;

  TopTitle := Format('Co%dVo%dWi%dWo%dWa%dSh%dSa%dSl%d',
    [InputRec.Controller,
     InputRec.Vorstag,
     InputRec.Winkel,
     InputRec.Woben,
     InputRec.Wanten,
     InputRec.SalingH,
     InputRec.SalingA,
     InputRec.SalingL]);

  case FSalingTyp of
    stFest: st := SalingFestString;
    stDrehbar: st := SalingDrehbarString;
    stOhneBiegt: st := OhneSalingString;
    stOhneStarr: st := OhneSalingStarrString;
  end;

  TopTitle := Format('(%s/%s)', [TopTitle, st]);
  TopTitle := TopTitleString + ' - ' + DateToStr(Date) + ' - ' + TopTitle;

  Rigg.ProofRequired := False;

  try
    { determine parameter range (P) and and start outer loop }
    PAnfang := StrToInt(PminEditText);
    PEnde := StrToInt(PmaxEditText);
    PAntrieb := (PEnde + PAnfang) / 2;
    for p := 0 to ParamCount - 1 do
    begin
      if ParamCount > 1 then
        ProgressCaption := Format(ProgressCaptionFormatString, [p + 1, ParamCount])
      else
        ProgressCaption := ProgressCaptionString;

      if ParamCount > 1 then
      begin
        PAntrieb := PAnfang + (PEnde - PAnfang) * p / (ParamCount - 1);
        PText[p] := Format('%6.2f', [PAntrieb]);
      end;

      { Parameter ansteuern / set the param to its new loop value }
      if ParamCount < 2 then
      begin
       { do nothing }
      end
      else
      case pn of
        xpController: Rigg.RealGlied[fpController] := PAntrieb;
        xpWinkel: Rigg.RealGlied[fpWinkel] := PAntrieb * P180;
        xpVorstag: Rigg.RealGlied[fpVorstag] := PAntrieb;
        xpWante: Rigg.RealGlied[fpWante] := PAntrieb;
        xpWoben: Rigg.RealGlied[fpWoben] := PAntrieb;
        xpSalingH: Rigg.RealGlied[fpSalingH] := PAntrieb;
        xpSalingA: Rigg.RealGlied[fpSalingA] := PAntrieb;
        xpSalingL:
        begin
          case FSalingTyp of
            stDrehbar: Rigg.RealGlied[fpSalingL] := PAntrieb;
            stFest:
            begin
              tempL := Rigg.RealGlied[fpSalingL];
              temp := PAntrieb/tempL;
              tempH := temp * Rigg.RealGlied[fpSalingH];
              tempA := temp * Rigg.RealGlied[fpSalingA];
              Rigg.RealGlied[fpSalingH] := tempH;
              Rigg.RealGlied[fpSalingA] := tempA;
            end;
          end;
        end;
        xpVorstagOS: ;
        xpWPowerOS: ;
        xpSalingW:
        begin
          temp := PAntrieb * P180;
          tempL := Rigg.RealGlied[fpSalingL];
          tempH := tempL * sin(temp);
          tempA := 2 * tempL * cos(temp);
          Rigg.RealGlied[fpSalingH] := tempH;
          Rigg.RealGlied[fpSalingA] := tempA;
        end;
      end;

      { determine definition range (X) and start inner loop }
      Anfang := StrToInt(XminEditText);
      Ende := StrToInt(XmaxEditText);
      for i := 0 to LNr do
      begin
        if i mod 5 = 0 then
          ProgressPosition := i;

        Antrieb := Anfang + (Ende - Anfang) * i / LNr;

        { Antrieb ansteuern / set the primary driving member to new value }
        case xn of
          xpController: Rigg.RealGlied[fpController] := Antrieb;
          xpWinkel: Rigg.RealGlied[fpWinkel] := Antrieb * P180;
          xpVorstag: Rigg.RealGlied[fpVorstag] := Antrieb;
          xpWante: Rigg.RealGlied[fpWante] := Antrieb;
          xpWOben: Rigg.RealGlied[fpWoben] := Antrieb;
          xpSalingH: Rigg.RealGlied[fpSalingH] := Antrieb;
          xpSalingA: Rigg.RealGlied[fpSalingA] := Antrieb;
          xpSalingL:
          begin
            case FSalingTyp of
              stDrehbar: Rigg.RealGlied[fpSalingL] := Antrieb;
              stFest:
              begin
                tempL := Rigg.RealGlied[fpSalingL];
                temp := Antrieb/tempL;
                tempH := temp * Rigg.RealGlied[fpSalingH];
                tempA := temp * Rigg.RealGlied[fpSalingA];
                Rigg.RealGlied[fpSalingH] := tempH;
                Rigg.RealGlied[fpSalingA] := tempA;
              end;
            end;
          end;
          xpSalingW:
          begin
            temp := Antrieb * P180;
            tempL := Rigg.RealGlied[fpSalingL];
            tempH := tempL * sin(temp);
            tempA := 2 * tempL * cos(temp);
            Rigg.RealGlied[fpSalingH] := tempH;
            Rigg.RealGlied[fpSalingA] := tempA;
          end;
        end;

        { Berechnen / do some number crunching within the model (Rigg) }
        if FSalingTyp = stFest then
        begin
          if (XComboSelectedText = WinkelString) or
             (PComboSelectedText = WinkelString) then
            Rigg.UpdateGetriebeFS
          else
            Rigg.BerechneWinkel;
          end
        else
        begin
          Rigg.UpdateGetriebe;
          end;
        Rigg.UpdateRigg;
        PunktOK := Rigg.GetriebeOK and Rigg.MastOK and Rigg.RiggOK;

        { Ergebnisse einspeichern / take the results }
        if yavVorstagSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavVorstagSpannung].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.C0C
          else
            af[p, j, i] := 0;
        end;
        if yavWantenSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavWantenSpannung].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.A0A
          else
            af[p, j, i] := 0;
        end;
        if yavMastfallF0F in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0F].ArrayIndex;
          af[p, j, i] := Rigg.rP.F0.Distance(Rigg.rP.F);
        end;
        if yavMastfallF0C in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0C].ArrayIndex;
          af[p, j, i] := Rigg.rP.F0.Distance(Rigg.rP.C);
        end;
        if yavAuslenkungC in YAchseSet then
        begin
          j := YAchseRecordList[yavAuslenkungC].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rP.C.Distance(Rigg.rPe.C)
          else
            af[p, j, i] := 0;
        end;
        if yavDurchbiegungHD in YAchseSet then
        begin
          j := YAchseRecordList[yavDurchbiegungHD].ArrayIndex;
          af[p, j, i] := Rigg.hd;
        end;
        if yavRF00 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF00].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.D0C
          else
            af[p, j, i] := 0;
        end;
        if yavRF01 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF01].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.C0D0
          else
            af[p, j, i] := 0;
        end;
        if yavRF03 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF03].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.A0C0
          else
            af[p, j, i] := 0;
        end;
        if yavRF05 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF05].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.A0D0
          else
            af[p, j, i] := 0;
        end;
        if yavRF10 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF10].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.AD
          else
            af[p, j, i] := 0;
        end;
        if yavRF11 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF11].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.AB
          else
            af[p, j, i] := 0;
        end;
        if yavRF13 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF13].ArrayIndex;
          if PunktOK then
            af[p, j, i] := Rigg.rF.AC
          else
            af[p, j, i] := 0;
        end;
      end;
    end;

  finally
    { Getriebe wiederherstellen / restore the model state }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
  end;
end;

procedure TChartModel.DrawNormal;
begin
  FShowGroup := False;
  LoadNormal;
  DrawInternal;
end;

procedure TChartModel.DrawInternal;
begin
  if FValid then
  begin
    YTitle := GetYText(YComboSelectedText);
    XTitle := XAchseText;
    PTitle := Format('%s', [ParamText]);
    Xmin := XAchseMin;
    Xmax := XAchseMax;
    LookForYMinMax;
  end
  else
  begin
    TopTitle := TopTitleTestString;
    XTitle := BottomTitleTestString;
    PTitle := ParamTitleTestString;
    if FStatus = [] then
      YTitle := StatusResetString // 'Diagramm wurde zurückgesetzt'
    else if csBerechnet in FStatus then
      YTitle := StatusNotComputedString // 'Kurve wurde nicht berechnet!'
    else if csGeladen in FStatus then
      YTitle := StatusNotLoadedString; // 'Kurve wurde nicht geladen!';

    Xmin := 0;
    Xmax := 100;
    Ymin := 0;
    Ymax := 100;

    InitStraightLine;
  end;

  if (Ymax - Ymin < 0.1) then
  begin
    Ymin := Ymin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  if (Xmax - Xmin < 0.1) then
  begin
    Xmin := Xmin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  if FShowGroup then
    DrawTogether
  else
    Draw;
end;

procedure TChartModel.YComboChange(Sender: TObject);
var
  j, p: Integer;
begin
  FShowGroup := False;
  if (YComboItemIndex < 0) or (YComboItemIndex > VNr-1) then
  begin
    FValid := False;
    YMinEditText := YMinEditString;
    YMaxEditText := YMaxEditString;
    DrawInternal;
    Exit;
  end;
  j := ComboIndexToCurve(YComboItemIndex);
  if not Valid then
  begin
    YMinEditText := YMinString;
    YMaxEditText := YMaxString;
    DrawInternal;
    Exit;
  end;
  for p := 0 to ParamCount - 1 do
    bf[p] := af[p, j];
  UpdateYMinMax;
  DrawInternal;
end;

procedure TChartModel.DrawGroup;
begin
  if FShowGroup then
    DrawTogether
  else
    DrawNormal;
end;

procedure TChartModel.DrawTogether;
begin
  if FStatus = [] then
    Exit;
  if KurvenzahlSpinnerValue > ParamCount then
    KurvenzahlSpinnerValue := ParamCount;
  ShowTogether(KurvenZahlSpinnerValue);
  FShowGroup := True;
end;

procedure TChartModel.ShowTogether(ParamNo: Integer);
var
  i, j, param: Integer;
  YAV: TYAchseValue;
  min, max, diff, temp: double;
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ParamNo > ParamCount then
    ParamNo := ParamCount;

  { bf füllen }
  param := 0; { p steht hier für die Anzahl der Kurven in YAchseSet }
  for i := 0 to YComboItems.Count - 1 do
  begin
    YAV := YAchseSortedList[i];
    if YAV in YAchseSet then
    begin
      j := YAchseRecordList[YAV].ArrayIndex;
      if param = PNr then
        break;
      bf[param] := af[ParamNo - 1, j];
      GroupText[param] := YAchseRecordList[YAV].ComboText;
      param := param + 1;
    end;
  end;

  GroupKurvenZahl := param;
  for param := 0 to GroupKurvenZahl - 1 do
  begin
    { Maximum und Minimum ermitteln }
    max := bf[param, 0];
    min := max;
    for i := 0 to LNr do
    begin
      if bf[param, i] > max then
        max := bf[param, i];
      if bf[param, i] < min then
        min := bf[param, i];
    end;

    { Normieren }
    diff := max - min;
    temp := param * 100 / GroupKurvenZahl;
    if max-min = 0 then
      for i := 0 to LNr do
        bf[param, i] := temp
    else
    begin
      for i := 0 to LNr do
      try
        bf[param, i] := (bf[param, i] - min) * 100 / diff;
      except on EMathError do
        bf[param, i] := 0;
      end;
    end;
  end;

  FValid := True;
  YMinEditText := YMinString;
  YMaxEditText := YMaxString;

  TopTitle := '';
  YTitle := AllCurvesNormalizedString; // 'Alle Kurven normiert [%]';
  XTitle := XAchseText;
  PTitle := Format('%s%d', [PIdentString, ParamNo]); //Nr.1

  Xmin := XAchseMin;
  Xmax := XAchseMax;
  Ymin := 0;
  Ymax := 100;

  if (Ymax-Ymin < 0.1) then
  begin
    Ymin := Ymin - 0.1;
    Ymax := Ymax + 0.1;
  end;
  if (Xmax-Xmin < 0.1) then
  begin
    Xmin := Xmin - 0.1;
    Ymax := Ymax + 0.1;
  end;

  tempParamCount := ParamCount;
  tempPText := PText;

  ParamCount := GroupKurvenZahl;
  PText := GroupText;

  Draw;

  ParamCount := tempParamCount;
  PText := tempPText;
end;

procedure TChartModel.UpdateYMinMax;
var
  min, max: double;
  i, j, p: Integer;
begin
  { Maximum und Minimum suchen für eine einzelne Kurve. }
  p := PSpinnerValue - 1;
  j := ComboIndexToCurve(YComboItemIndex);
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEditText := YMinString;
    YMaxEditText := YMaxString;
    Exit;
  end;
  TempF := af[p, j];
  max := TempF[0];
  min := max;
  for i := 0 to LNr do
  begin
    if TempF[i] > max then
      max := TempF[i];
    if TempF[i] < min then
      min := TempF[i];
  end;
  YMinEditText := Format('%6.2f', [min]);
  YMaxEditText := Format('%6.2f', [max]);
end;

procedure TChartModel.LookForYMinMax;
var
  i, j, p: Integer;
begin
  { Maximum und Minimum suchen über alle Parameter hinweg }
  if RggDocument.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YComboSelectedText = VorstagSpannungString) or
       (YComboSelectedText = WantenSpannungString) then
    begin
      Ymax := 5000; { 5000 N }
      Ymin := -1000; { -1000 N }
      Exit;
    end;
    if (YComboSelectedText = ElasticityPointCString) then
    begin
      YMax := 1000; { 1000 mm }
      YMin := 0;
      Exit;
    end;
  end;

  p := PSpinnerValue - 1; { Index für Parameter }
  j := ComboIndexToCurve(YComboItemIndex); { Index für Kurve }
  if not Valid then
  begin
    { MessageBeep(MB_ICONEXCLAMATION); }
    YMinEditText := YMinString;
    YMaxEditText := YMaxString;
    Exit;
  end;
  Ymax := af[p, j, 0];
  Ymin := Ymax;
  for p := 0 to ParamCount - 1 do
  begin
    TempF := af[p, j];
    for i := 0 to LNr do
    begin
      if TempF[i] > Ymax then
        Ymax := TempF[i];
      if TempF[i] < Ymin then
        Ymin := TempF[i];
    end;
  end;
end;

function TChartModel.GetYText(Text: string): string;
var
  YAV: TYAchseValue;
begin
  result := '';
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAchseRecordList[YAV].ComboText = Text then
    begin
      result := YAchseRecordList[YAV].Text;
      break;
    end;
end;

procedure TChartModel.GetMemoText;
var
  p: Integer;
  YAV: TYAchseValue;
  xpName: TxpName;
  T: TTrimmTabDaten;
begin
  Inc(MemoCounter);
  with MemoLines do
  begin
    Clear;
    { SalingTyp }
    case FSalingTyp of
      stFest: Add('SalingTyp: Feste Salinge');
      stDrehbar: Add('SalingTyp: Drehbare Salinge');
      stOhneBiegt: Add('SalingTyp: Ohne Salinge (Mast biegsam)');
      stOhneStarr: Add('SalingTyp: Ohne Salinge (Mast starr)');
    end;
    { ControllerTyp }
    case Rigg.ControllerTyp of
      ctDruck: Add('ControllerTyp: Controller überträgt Druck');
      ctOhne: Add('ControllerTyp: kein Controller');
    end;
    { CalcTyp }
    if FSalingTyp = stOhneStarr then
      Add('BerechnungsTyp: Wantenkraft vorgegeben');
    if FSalingTyp <> stOhneStarr then
    case Rigg.CalcTyp of
      ctQuerKraftBiegung: Add('BerechnungsTyp: nur Quekraftbiegung');
      ctBiegeKnicken: Add('BerechnungsTyp: Biegeknicken');
      ctKraftGemessen: begin
        Add('BerechnungsTyp: Trimmtabelle verwendet');
        Add('');
        Add('Trimmtabelle:');
        T := Rigg.TrimmTab.TrimmTabDaten;
        case T.TabellenTyp of
          itKonstante:
          begin
            Add('  KurvenTyp: Konstante');
            Add(Format('  Konstante Kraft: %g N',[T.a1]));
            Add(Format('  Maximale Kraft: %g N',[T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm',[T.x1]));
          end;
          itGerade:
          begin
            Add('  KurvenTyp: Gerade');
            Add(Format('  Steigung: %g N/mm',[1/T.a1]));
            Add(Format('  Maximale Kraft: %g N',[T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm',[T.a1*T.x2]));
          end;
          itParabel:
          begin
            Add('  KurvenTyp: Parabel');
            Add('  Weg/mm = a2*(Kraft/N)^2 + a1*Kraft/N');
            Add(Format('  a1: %g', [T.a1]));
            Add(Format('  a2: %g', [T.a2]));
            Add(Format('  Maximale Kraft: %g N', [T.x2]));
            Add(Format('  Maximale Auslenkung: %g mm', [T.a2*T.x2*T.x2 + T.a1*T.x2]));
          end;
          itBezier:
          begin
            Add('  KurvenTyp: Bezier');
            Add(Format('  KontrollPunkt bei (%g mm, %g N)', [T.a1, T.a2]));
            Add(Format('  Endpunkt bei (%g mm, %g N)', [T.x1, T.x2]));
          end;
        end;
      end;
    end;
    { X }
    Add('');
    Add('XAchse: ' + XAchseText);
    Add(Format('  Use AP: %s', [BoolStr[AP]]));
    Add(Format('  AP Width: %d', [APWidth]));
    Add(Format('  %d ... %d', [XAchseMin, XAchseMax]));
    { P }
    if ParamCount > 1 then
    begin
      Add('');
      Add('Parameter: ' + ParamText);
      with MemoLines do
      begin
        for p := 0 to ParamCount - 1 do
          Add(Format('  #%d: %s (%s) ', [p + 1, PText[p], PColorText[p]]));
      end;
    end;
    { Y }
    if YAchseSet <> [] then
    begin
      Add('');
      Add('YAchse: Berechnete Kurven');
      for YAV := Low(TYAchseValue) to High(TYAchseValue) do
        if YAV in YAchseSet then
          Add('  ' + YAchseRecordList[YAV].ComboText);
    end;
    { Längen }
    xpName := GetTsbName(PComboText);
    Add('');
    Add('Rigg: Einstellwerte');
    with Rigg do
    begin
      if (ControllerTyp = ctDruck) and (xpName <> xpController) then
        Add(Format('  Controller: %g mm', [RealGlied[fpController]]));
      if (SalingTyp <> stOhneStarr) and ManipulatorMode and (xpName <> xpWinkel) then
        Add(Format(  '  Winkel: %g Grad', [RealGlied[fpWinkel] * D180]));
      if (SalingTyp <> stOhneStarr) and not ManipulatorMode and (xpName <> xpVorstag) then
        Add(Format('  Vorstag: %g mm', [RealGlied[fpVorstag]]));
      if xpName <> xpWante then
        Add(Format(  '  Wante: %g mm', [RealGlied[fpWante]]));
      case SalingTyp of stFest, stDrehbar:
        if xpName <> xpWoben then
          Add(Format('  WanteOben: %g mm', [RealGlied[fpWoben]]));
      end;

      if (SalingTyp = stFest) and not (xpName in [xpSalingH, xpSalingL, xpSalingW]) then
        Add(Format('  SalingHöhe: %g mm', [RealGlied[fpSalingH]]));
      if (SalingTyp = stFest) and not (xpName in [xpSalingA, xpSalingL, xpSalingW]) then
        Add(Format('  SalingAbstand: %g mm', [RealGlied[fpSalingA]]));
      if (SalingTyp = stFest) and (xpName = xpSalingL) then
        { SalingWinkel ausgeben }
        if RealGlied[fpSalingA] <> 0 then
          Add(Format('  SalingWinkel: %g Grad',[
            arctan2(RealGlied[fpSalingH], RealGlied[fpSalingA]) * D180]));
      if (SalingTyp = stFest) and (xpName = xpSalingW) then
        Add(Format('  SalingLänge: %6.2f mm', [RealGlied[fpSalingL]]));

      if (SalingTyp = stDrehbar) and (xpName <> xpSalingL) then
        Add(Format('  SalingLänge: %6.2f mm', [RealGlied[fpSalingL]]));
      if (SalingTyp = stOhneStarr) and (xpName <> xpVorstag) then { nicht VorstagOS - ok }
        Add(Format('  Vorstag: %g mm', [RealGlied[fpVorstagOS]]));
      if (SalingTyp = stOhneStarr) and (xpName <> xpWPowerOS) then
        Add(Format('  Wantenspannung: %g N', [RealGlied[fpWPowerOS]]));
    end;
    { Koordinaten }
    Add('');
    Add('Rumpf: Koordinaten (x,y,z) [mm]');
    with Rigg do
    begin
      Add(Format('  A0(%g,%g,%g)', [rP.A0.X, rP.A0.Y, rP.A0.Z]));
      Add(Format('  B0(%g,%g,%g)', [rP.B0.X, rP.B0.Y, rP.B0.Z]));
      Add(Format('  C0(%g,%g,%g)', [rP.C0.X, rP.C0.Y, rP.C0.Z]));
      Add(Format('  D0(%g,%g,%g)', [rP.D0.X, rP.D0.Y, rP.D0.Z]));
      Add(Format('  E0(%g,%g,%g)', [rP.E0.X, rP.E0.Y, rP.E0.Z]));
      Add(Format('  F0(%g,%g,%g)', [rP.F0.X, rP.F0.Y, rP.F0.Z]));
    end;
    { Mast }
    Add('');
    Add('Mast:');
    with Rigg do
    begin
      Add(Format('  D0D: %d mm (Saling)', [Round(MastUnten)]));
      Add(Format('  D0C: %d mm (Vorstag)', [Round(MastUnten + MastOben)]));
      Add(Format('  D0F: %d mm (Top)', [Round(MastLaenge)]));
      Add(Format('  Biegesteifigkeit EI: %d Nm^2', [MastEI]));
    end;
    { Exit Counters }
    Add('');
    with Rigg do
    begin
      if ExitCounter1 > 0 then Add(Format('  EC 1: %d ', [ExitCounter1]));
      if ExitCounter2 > 0 then Add(Format('  EC 2: %d ', [ExitCounter2]));
      if ExitCounter3 > 0 then Add(Format('  EC 3: %d ', [ExitCounter3]));
      if ExitCounter4 > 0 then Add(Format('  EC 4: %d ', [ExitCounter4]));
      if ExitCounter5 > 0 then Add(Format('  EC 5: %d ', [ExitCounter5]));
      if ExitCounter6 > 0 then Add(Format('  EC 6: %d ', [ExitCounter6]));
      if ExitCounter7 > 0 then Add(Format('  EC 7: %d ', [ExitCounter6]));
    end;
    Add(Format('Memo Counter: %d', [MemoCounter]));
    Add(Format('Calc Counter: %d', [CalcCounter]));
  end;
end;

procedure TChartModel.XComboChange(Sender: TObject);
begin
  UpdateXMinMax;
  FXTextClicked := XComboSelectedText;
  if (XComboSelectedText = XComboText) and (csBerechnet in FStatus) then
    XLEDFillColor := claLime
  else
    XLEDFillColor := claRed;
  UpdatePCombo(FSalingTyp);
end;

procedure TChartModel.PComboChange(Sender: TObject);
begin
  UpdatePMinMax;
  FPTextClicked := PComboSelectedText;
  if (PComboSelectedText = PComboText) and (csBerechnet in FStatus) then
    PLEDFillColor := claLime
  else
    PLEDFillColor := claRed;
end;

procedure TChartModel.UpdateXCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with XComboItems do
  begin
    Clear;
    if SalingTyp = stFest then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WinkelString);
      Add(WanteString);
      Add(WanteObenString);
      Add(SalingHString);
      Add(SalingAString);
      Add(SalingLString);
      Add(SalingWString);
    end;
    if SalingTyp = stDrehbar then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WanteString);
      Add(WanteObenString);
      Add(SalingLString);
    end;
    if SalingTyp = stOhneBiegt then
    begin
      Add(ControllerString);
      Add(VorstagString);
      Add(WanteString);
    end;
    if SalingTyp = stOhneStarr then
    begin
      Add(VorstagString);
    end;
  end;
  XComboItemIndex := XComboItems.IndexOf(VorstagString);
  for i := 0 to XComboItems.Count-1 do
    if (XComboItems[i] = FXTextClicked) then
    begin
      XComboItemIndex := i;
      break;
    end;
  UpdatePCombo(SalingTyp);
end;

procedure TChartModel.UpdatePCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with PComboItems do
  begin
    Clear;
    Add(NoParamString);
    if XComboSelectedText = ControllerString then
    begin
      if SalingTyp = stFest then
      begin
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(VorstagString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhneBiegt then
      begin
        Add(VorstagString);
        Add(WanteString);
      end;
    end
    else if XComboSelectedText = VorstagString then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhneBiegt then
      begin
        Add(ControllerString);
        Add(WanteString);
      end;
    end
    else if XComboSelectedText = WinkelString then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if XComboSelectedText = WanteString then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteObenString);
        Add(SalingLString);
      end;
      if SalingTyp = stOhneBiegt then
      begin
        Add(ControllerString);
        Add(VorstagString);
      end;
    end
    else if (XComboSelectedText = WanteObenString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteString);
        Add(SalingLString);
      end;
    end
    else if (XComboSelectedText = SalingHString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingAString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if (XComboSelectedText = SalingAString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingLString);
        Add(SalingWString);
      end;
    end
    else if (XComboSelectedText = SalingLString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
        Add(SalingHString);
        Add(SalingAString);
        Add(SalingWString);
      end;
      if SalingTyp = stDrehbar then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WanteString);
        Add(WanteObenString);
      end;
    end
    else if (XComboSelectedText = SalingWString) then
    begin
      if SalingTyp = stFest then
      begin
        Add(ControllerString);
        Add(VorstagString);
        Add(WinkelString);
        Add(WanteString);
        Add(WanteObenString);
      end;
    end;
  end;
  PComboItemIndex := 0;
  for i := 0 to PComboItems.Count-1 do
    if (PComboItems[i] = FPTextClicked) then
    begin
      PComboItemIndex := i;
      break;
    end;

  UpdatePMinMax;
end;

function TChartModel.GetXText(Text: string): string;
var
  s: string;
begin
  if Text = ControllerString then s := ControllerText
  else if Text = WinkelString then s := WinkelText
  else if Text = VorstagString then s := VorstagText
  else if Text = WanteString then s := WanteText
  else if Text = WanteObenString then s := WanteObenText
  else if Text = SalingHString then s := SalingHText
  else if Text = SalingAString then s := SalingAText
  else if Text = SalingLString then s := SalingLText
  else if Text = SalingWString then s := SalingWText;
  result := s;
end;

function TChartModel.GetPText(Text: string): string;
var
  s: string;
begin
  if Text = ControllerString then s := ControllerText
  else if Text = WinkelString then s := WinkelText
  else if Text = VorstagString then s := VorstagText
  else if Text = WanteString then s := WanteText
  else if Text = WanteObenString then s := WanteObenText
  else if Text = SalingHString then s := SalingHText
  else if Text = SalingAString then s := SalingAText
  else if Text = SalingLString then s := SalingLText
  else if Text = SalingWString then s := SalingWText;
  result := s;
end;

function TChartModel.GetTsbName(Value: string): TxpName;
var
  xp: TxpName;
begin
  xp := xpController;
  if Value = WinkelString then xp := xpWinkel
  else if Value = VorstagString then xp := xpVorstag
  else if Value = WanteString then xp := xpWante
  else if Value = WanteObenString then xp := xpWoben
  else if Value = SalingHString then xp := xpSalingH
  else if Value = SalingAString then xp := xpSalingA
  else if Value = SalingLString then xp := xpSalingL
  else if Value = SalingWString then xp := xpSalingW
  else if Value = VorstagOhneSalingString then xp := xpVorstagOS
  else if Value = WantenkraftOhneSalingString then xp := xpWPowerOS;
  result := xp;
end;

procedure TChartModel.UpdateXMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := XComboSelectedText;
  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (FSalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    Minimum := tempMin;
    Maximum := tempMax;

    if AP then
    begin
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end;

//    try
//      Minimum := StrToInt(XMinEditText);
//      Maximum := StrToInt(XMaxEditText);
//      if Minimum < tempMin then Minimum := tempMin;
//      if Maximum > tempMax then Maximum := tempMax;
//      if Minimum > Maximum then Minimum := tempMin;
//      if Maximum < Minimum then Maximum := tempMax;
//    except
//      on EConvertError do
//      begin
//        Minimum := tempMin;
//        Maximum := tempMax;
//      end;
//    end;

    XMinEditText := IntToStr(Minimum);
    XMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      XMinEditText := IntToStr(0);
      XMaxEditText := IntToStr(LNr);
      Valid := False;
      XLEDFillColor := claRed;
    end;
  end;
end;

procedure TChartModel.UpdatePMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := PComboSelectedText;
  if s = NoParamString then
  begin
    PMinEditText := IntToStr(0);
    PMaxEditText := IntToStr(0);
    KurvenZahlSpinnerValue := 1;
    Exit;
  end
  else if KurvenZahlSpinnerValue = 1 then
  begin
    KurvenzahlSpinnerValue := UserSelectedKurvenZahl;
  end;

  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (FSalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    Minimum := tempMin;
    Maximum := tempMax;

    if AP then
    begin
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end;

//    try
//      Minimum := StrToInt(PMinEditText);
//      Maximum := StrToInt(PMaxEditText);
//      if Minimum < tempMin then Minimum := tempMin;
//      if Maximum > tempMax then Maximum := tempMax;
//      if Minimum > Maximum then Minimum := tempMin;
//      if Maximum < Minimum then Maximum := tempMax;
//    except
//      on EConvertError do
//      begin
//        Minimum := tempMin;
//        Maximum := tempMax;
//      end;
//    end;

    PMinEditText := IntToStr(Minimum);
    PMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      PMinEditText := IntToStr(0);
      PMaxEditText := IntToStr(LNr);
      Valid := False;
      PLEDFillColor := claRed;
    end;
  end;
end;

function  TChartModel.CheckBeforeCalc: Boolean;
begin
  result := True;
  if (XMinEditText = XMinEditString) or (XMaxEditText = XMaxEditString) then
    UpdateXMinMax;
  if (PMinEditText = PMinEditString) or (PMaxEditText = PMaxEditString) then
    UpdatePMinMax;
end;

procedure TChartModel.DoAfterCalc;
begin
  { X }
  XLEDFillColor := claLime;
  XComboText := XComboSelectedText;
  XAchseText := GetXText(XComboSelectedText);
  XAchseMin := StrToInt(XMinEditText);
  XAchseMax := StrToInt(XMaxEditText);

  { Parameter }
  PLEDFillColor := claLime;
  PComboText := PComboSelectedText;
  ParamText := GetPText(PComboSelectedText);
  ParamMin := StrToInt(PMinEditText);
  ParamMax := StrToInt(PMaxEditText);
end;

procedure TChartModel.UpdateGetriebe;
begin
  Rigg.UpdateGetriebe;

  if (Rigg.GetriebeOK and Rigg.MastOK) then
    Rigg.UpdateRigg;
end;

procedure TChartModel.InitSpinner;
begin
  KurvenZahlSpinnerMax := ANr;
  KurvenZahlSpinnerValue := ParamCount;

  PSpinnerMax := PNr;
  PSpinnerValue := 1;

  APSpinnerMax := 100;
  APSpinnerValue := APWidth;
end;

procedure TChartModel.InitRigg;
begin
  Rigg := Main.Rigg;
  FSalingTyp := Rigg.SalingTyp;
end;

function TChartModel.XComboSelectedText: string;
begin
  if XComboItemIndex = -1 then
  begin
    XComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := XComboItems[XComboItemIndex];
end;

function TChartModel.YComboSelectedText: string;
begin
  if YComboItemIndex = -1 then
  begin
    YComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := YComboItems[YComboItemIndex];
end;

function TChartModel.PComboSelectedText: string;
begin
  if PComboItemIndex = -1 then
  begin
    PComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := PComboItems[PComboItemIndex];
end;

procedure TChartModel.SuperInit;
begin
  { Auswahl Anrieb (Definitionsbereich X): Wantenlänge }
  XComboItemIndex := XComboItems.IndexOf(WanteString);
  XComboChange(nil);

  { Auswahl Parameter: SalingHöhe }
  PComboItemIndex := PComboItems.IndexOf(SalingHString);
  PComboChange(nil);

  { Selektierer Y-Wert: Mastfall F0F }
  YComboItemIndex := YComboItems.IndexOf(MastfallF0FString);
  YComboChange(nil);
end;

procedure TChartModel.SuperCalc;
begin
  UpdateXMinMax;
  UpdatePMinMax;
  Calc;
end;

function TChartModel.GetChecked(fa: Integer): Boolean;
begin
  case fa of
    faChartRect: result := WantRectangles;
    faChartTextRect: result := WantTextRect;
    faChartLegend: result := WantLegend;
    faChartAP: result := AP;
    faChartBP: result := not AP;
    faChartGroup: result := FShowGroup;
    else
      result := False;
  end;
end;

procedure TChartModel.HandleAction(fa: Integer);
begin
  case fa of
    faChartRect:
    begin
      WantRectangles := not WantRectangles;
      Draw;
    end;

    faChartTextRect:
    begin
      WantTextRect := not WantTextRect;
      Draw;
    end;

    faChartLegend:
    begin
      WantLegend := not WantLegend;
      Draw;
    end;

    faChartAP:
    begin
      FAP := True;
      SuperCalc;
    end;

    faChartBP:
    begin
      FAP := False;
      SuperCalc;
    end;

    faChartGroup:
    begin
      FShowGroup := not FShowGroup;
      DrawGroup;
    end;

    faParamCountPlus:
    begin
      Inc(KurvenZahlSpinnerValue);
      if KurvenZahlSpinnerValue > PNr then
        KurvenZahlSpinnerValue := PNr;

      UserSelectedKurvenZahl := KurvenZahlSpinnerValue;

      if FShowGroup then
        DrawGroup
      else
        Calc;
    end;

    faParamCountMinus:
    begin
      Dec(KurvenZahlSpinnerValue);
      if KurvenZahlSpinnerValue < 1 then
        KurvenZahlSpinnerValue := 1;

      UserSelectedKurvenZahl := KurvenZahlSpinnerValue;

      if FShowGroup then
        DrawGroup
      else
        Calc;
    end;

    faPComboPlus:
    begin
      Inc(PComboItemIndex);
      if PComboItemIndex >= PComboItems.Count then
      begin
        { Wrap to Index 1, skip over Indedex 0 = kein Parameter }
        PComboItemIndex := 1;
      end;
      SuperCalc;
    end;

    faPComboMinus:
    begin
      Dec(PComboItemIndex);
      if PComboItemIndex < 1 then
        PComboItemIndex := PComboItems.Count - 1;
      SuperCalc;
    end;

    faXComboPlus:
    begin
      Inc(XComboItemIndex);
      if XComboItemIndex >= XComboItems.Count then
        XComboItemIndex := 0;
      UpdatePCombo(Rigg.SalingTyp);
      SuperCalc;
    end;

    faXComboMinus:
    begin
      Dec(XComboItemIndex);
      if XComboItemIndex < 0 then
        XComboItemIndex := XComboItems.Count - 1;
      UpdatePCombo(Rigg.SalingTyp);
      SuperCalc;
    end;

    faYComboPlus:
    begin
      Inc(YComboItemIndex);
      if YComboItemIndex >= YComboItems.Count then
        YComboItemIndex := 0;
      Calc;
    end;

    faYComboMinus:
    begin
      Dec(YComboItemIndex);
      if YComboItemIndex < 0 then
        YComboItemIndex := YComboItems.Count - 1;
      Calc;
    end;

    faChartReset: Reset;

  end;

  DoOnAction;
end;

procedure TChartModel.DoOnAction;
begin
  if IsUp then
    if Assigned(FOnActionHandled) then
      OnActionHandled(Self);
end;

procedure TChartModel.Draw;
begin
  if IsUp then
    if Assigned(FOnUpdateAvailable) then
      OnUpdateAvailable(self);
end;

end.
