unit RggChart;

interface

uses
  RiggVar.FB.ActionConst,
  RggUnit4,
  RggTypes,
  RggDoc,
  RggSaling3Eck,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math;

const
  ANr = 6; { maximale Anzahl Kurven, d.h. berechneter Y Werte }
  PNr = 5; { maximale Anzahl der Werte des Parameters }
  VNr = 14; { Anzahl der zur Auswahl stehenden Y Werte }
  LNr = 50; { Anzahl der Punkte im Diagramm - 1 }
  ErrorIndex = 999;
  D180 = 180 / PI;
  P180 = PI / 180;

type
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

  TChartStatus = (csBerechnet, csGeladen);
  TYLineArray = array[0..ANr-1] of TLineDataR50;
  TYAchseSortedList = array[0..VNr-1] of TYAchseValue;
  TYAchseSet = set of TYAchseValue; { die berechneten Kurven }
  TYAchseStringArray = array[0..PNr-1] of string;

  TChartForm = class
  private
    FSalingTyp: TSalingTyp;

    Rigg: TRigg;
    RggDocument: TRggDocument;
    SalingDreieck: TSalingDreieck;

    procedure InitRigg;
    procedure UpdateGetriebe;

    property SalingTyp: TSalingTyp read FSalingTyp;
  private
    ProgressPosition: Integer;
    ProgressCaption: string;
  private
    MemoLines: TStringList;
    procedure GetMemoText;
  private
    XComboItems: TStrings;
    YComboItems: TStrings;
    PComboItems: TStrings;

    XComboItemIndex: Integer;
    YComboItemIndex: Integer;
    PComboItemIndex: Integer;

    procedure InitXComboItems;
    procedure InitYComboItems;
    procedure InitPComboItems;

    procedure UpdateXCombo(SalingTyp: TSalingTyp);
    procedure UpdatePCombo(SalingTyp: TSalingTyp);

    procedure XComboChange(Sender: TObject);
    procedure YComboChange(Sender: TObject);
    procedure PComboChange(Sender: TObject);

    function XComboSelectedText: string;
    function YComboSelectedText: string;
    function PComboSelectedText: string;

    function GetXText(Text: string): string;
    function GetPText(Text: string): string;
  private
    YLEDFillColor: TAlphaColor;
    PLEDFillColor: TAlphaColor;
    XLEDFillColor: TAlphaColor;
  private
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

    procedure UpdateXMinMax;
    procedure UpdateYMinMax;
    procedure UpdatePMinMax;
  private
    APWidth: Integer;

    BPBtnIsPressed: Boolean;
    APBtnIsPressed: Boolean;

    procedure APItemClick(Sender: TObject);
    procedure BPItemClick(Sender: TObject);
  private
    APSpinnerValue: Integer;
    APSpinnerMax: Integer;

    PSpinnerValue: Integer;
    PSpinnerMax: Integer;

    KurvenZahlSpinnerValue: Integer;
    KurvenZahlSpinnerMax: Integer;

    procedure InitSpinner;
  protected
    GroupKurvenZahl: Integer;
    ParamCount: Integer;
    FShowGroup: Boolean;
    GroupText: TYAchseStringArray;
  private
    YAchseSet: TYAchseSet;
    YAchseRecordList: TYAchseRecordList;
    YAchseSortedList: TYAchseSortedList;
    function GetYText(Text: string): string;
    procedure UpdateYAchseList;
    procedure UpdateYAchseSet;
    function ComboIndexToCurve(ComboIndex: Integer): Integer;
  private
    FDarkColors: Boolean;
    procedure SetDarkColors(const Value: Boolean);
    property DarkColors: Boolean read FDarkColors write SetDarkColors;
  protected
    TopTitle: string;
    LeftTitle: string;
    BottomTitle: string;
    RightTitle: string;

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
  protected
    TempF: TLineDataR50;
    TestF: TLineDataR50;
    af: array[0..PNr-1] of TYLineArray;
    bf: array[0..PNr-1] of TLineDataR50;
    cf: array[0..PNr-1] of TAlphaColor;
    procedure InitStraightLine;
    procedure GetCurves;
    procedure DrawInternal;
    procedure ShowTogether(ParamNo: Integer);
  private
    FBuissy: Boolean;
    FStatus: set of TChartStatus;
    function GetTsbName(Text: string): TxpName;
  protected
    FValid: Boolean;
    property Valid: Boolean read FValid write FValid;
  private
    procedure Reset;
    procedure Calc;

    procedure LoadNormal;
    procedure DrawNormal;
    procedure DrawTogether;
    procedure DrawGroup; // param group or curve group

    function CheckBeforeCalc: Boolean;
    procedure DoAfterCalc;
  public
    WantRectangles: Boolean;
    WantTextRect: Boolean;
    WantLegend: Boolean;

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: TFederAction): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SuperInit;
    procedure SuperCalc;

    procedure Draw; virtual;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.RG.Def,
  RggCalc,
  RggScroll;

procedure TChartForm.InitXComboItems;
var
  ML: TStrings;
begin
  ML := XComboItems;
  ML.Add('Controller');
  ML.Add('Winkel');
  ML.Add('Vorstag');
  ML.Add('Wante');
  ML.Add('Wante oben');
  ML.Add('Saling Höhe');
  ML.Add('Saling Abstand');
end;

procedure TChartForm.InitPComboItems;
var
  ML: TStrings;
begin
  ML := PComboItems;
  ML.Add('Saling Abstand');
  ML.Add('Saling Höhe');
end;

procedure TChartForm.InitYComboItems;
var
  ML: TStrings;
begin
  ML := YComboItems;
  ML.Add('Durchbiegung hd');
  ML.Add('Elastizität Punkt C');
  ML.Add('Mastfall F0C');
  ML.Add('Mastfall F0F');
  ML.Add('Vorstag-Spannung');
  ML.Add('Wanten-Spannung');
end;

constructor TChartForm.Create;
begin
  FSalingTyp := stFest;

  ParamCount := 3;
  APWidth := 30;

  YLEDFillColor := claRed;
  XLEDFillColor := claRed;
  PLEDFillColor := claRed;

  XComboItems := TStringList.Create;
  YComboItems := TStringList.Create;
  PComboItems := TStringList.Create;

  InitXComboItems;
  InitYComboItems;
  InitPComboItems;

  XComboItemIndex := 3;
  PComboItemIndex := 1;
  YComboItemIndex := 3;

  FXTextClicked := 'Vorstag';
  FPTextClicked := 'Kein Parameter';

  InitSpinner;

  InitRigg;

  RggDocument := TRggDocument.Create;
  SalingDreieck := TSalingDreieck.Create;

  MemoLines := TStringList.Create;
  MemoLines.Add('Diagramm befindet sich im Anfangszustand.');

  UpdateXCombo(SalingTyp);
  UpdatePCombo(SalingTyp);

  XComboItemIndex := XComboItems.IndexOf(FXTextClicked);
  PComboItemIndex := PComboItems.IndexOf(FPTextClicked);

  InitYAchseRecordList(YAchseRecordList);
  { Hiermit werden die Felder ComboText und Text initialisiert.
    ComboIndex wird in UpdateYAchseList weiter unten bestimmt.
    ArrayIndex wird beim Berechnen oder Einlesen neu bestimmt.
    YAchseSet = [] zeigt an, daß ArrayIndex nicht gültig ist.
  }

  DarkColors := False;

  PColorText[0] := 'Blau';
  PColorText[1] := 'Rot';
  PColorText[2] := 'Grün';
  PColorText[3] := 'Weiß';
  PColorText[4] := 'Gelb';

  PText := PColorText;

  APItemClick(nil);
//  BereichItemClick(nil);

  UpdateXMinMax;
  UpdatePMinMax;

  InitStraightLine;

  Reset;
end;

destructor TChartForm.Destroy;
begin
  RggDocument.Free;
  MemoLines.Free;
  SalingDreieck.Free;
  XComboItems.Free;
  YComboItems.Free;
  PComboItems.Free;
end;

procedure TChartForm.APItemClick(Sender: TObject);
begin
  APBtnIsPressed := not APBtnIsPressed;
  BPBtnIsPressed := not APBtnIsPressed;
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartForm.BPItemClick(Sender: TObject);
begin
  BPBtnIsPressed := not BPBtnIsPressed;
  APBtnIsPressed := not BPBtnIsPressed;
  UpdateXMinMax;
  UpdatePMinMax;
end;

procedure TChartForm.Reset;
begin
  FBuissy := False;
  FValid := False;
  FStatus := [];
  YAchseSet := [];
  PText := PColorText;
  YLEDFillColor := claRed;
  XLEDFillColor := claRed;
  PLEDFillColor := claRed;
  DrawInternal;
  MemoLines.Clear;
  MemoLines.Add('Diagramm wurde in den Anfangszustand versetzt.');
end;

procedure TChartForm.SetDarkColors(const Value: Boolean);
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

procedure TChartForm.InitStraightLine;
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

procedure TChartForm.UpdateYAchseList;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  { wird von YComboClick aufgerufen und vom constructor }

  { ComboIndex zurücksetzen auf -1 }
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    YAchseRecordList[YAV].ComboIndex := -1;
  { ComboIndex neu bestimmen. Nicht ausgewählte Einträge bleiben auf -1 }
  for i := 0 to YComboItems.Count-1 do
  begin
    s := YComboItems[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      { Position j des Eintrag finden durch  Textvergleich }
      if s = YAchseRecordList[YAV].ComboText then
      begin
        { Position in der ComboBox festhalten }
        YAchseRecordList[YAV].ComboIndex := i;
        { Reihenfolge in Liste festhalten }
        YAchseSortedList[i] := YAV;
        break;
      end;
  end;
end;

procedure TChartForm.UpdateYAchseSet;
var
  i: Integer;
  s: string;
  YAV: TYAchseValue;
begin
  { Wird nur bei NeuBerechnung aufgerufen }

  YAchseSet := [];
  for i := 0 to YComboItems.Count-1 do
  begin
    s := YComboItems[i];
    for YAV := Low(TYAchseValue) to High(TYAchseValue) do
      { Position j des Eintrag finden durch  Textvergleich }
      if s = YAchseRecordList[YAV].ComboText then
      begin
        { Position in der ComboBox festhalten }
        YAchseRecordList[YAV].ArrayIndex := i;
        { festhalten, welche Kurven existieren }
        if i <= ANr-1 then
          Include(YAchseSet, YAV);
        break;
      end;
  end;
end;

function TChartForm.ComboIndexToCurve(ComboIndex: Integer): Integer;
var
  YAV: TYAchseValue;
begin
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

procedure TChartForm.Calc;
begin
  if not FBuissy then
  begin
    { Berechnen }

    if not CheckBeforeCalc then
      Exit;

    FBuissy := True;

    { Parameterzahl bearbeiten }
    if PComboSelectedText = 'kein Parameter' then
    begin
      PMinEditValue := 0;
      PMaxEditValue := 0;
      KurvenZahlSpinnerValue := 1;
    end;

    ParamCount := KurvenZahlSpinnerValue;

    if PSpinnerValue > ParamCount then
      PSpinnerValue := ParamCount;

    PSpinnerMax := ParamCount;
    { MaxValue muß größer MinValue sein }
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
    FBuissy := False;

    LoadNormal;
    DrawInternal;
  end;
end;

procedure TChartForm.LoadNormal;
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

procedure TChartForm.GetCurves;
var
  i, j, p: Integer;
  Antrieb, PAntrieb: double;
  Anfang, Ende, PAnfang, PEnde: double;
  InputRec: TTrimmControls;
  PunktOK: Boolean;
  s: string;
  temp, tempL, tempH, tempA: double;
begin
  { Getriebezustand sichern und verfügbar machen }
  InputRec := Rigg.Glieder;

  TopTitle := Format(
    '(Co%dVo%dWi%dWo%dWa%dSh%dSa%dSl%d',
    [InputRec.Controller,
     InputRec.Vorstag,
     InputRec.Winkel,
     InputRec.Woben,
     InputRec.Wanten,
     InputRec.SalingH,
     InputRec.SalingA,
     InputRec.SalingL]);

  case SalingTyp of
    stFest: TopTitle := TopTitle + '/fest)';
    stDrehbar: TopTitle := TopTitle + '/drehbar)';
    stOhne: TopTitle := TopTitle + '/ohne Saling)';
    stOhne_2: TopTitle := TopTitle + '/ohne Saling (BK))';
  end;

  TopTitle := 'Riggchart - ' + DateToStr(Date) + ' - ' + TopTitle;

  Rigg.ProofRequired := False;

  try
    { Parameterbereich bestimmen und Schleife starten }
    PAnfang := StrToInt(PminEditText);
    PEnde := StrToInt(PmaxEditText);
    PAntrieb := (PEnde + PAnfang) / 2;
    for p := 0 to ParamCount - 1 do
    begin
      if ParamCount > 1 then
        ProgressCaption := Format('Parameter %d von %d', [p+1, ParamCount])
      else
        ProgressCaption := 'Kurve wird berechnet';

      if ParamCount > 1 then
      begin
        PAntrieb := PAnfang + (PEnde - PAnfang) * p / (ParamCount - 1);
        PText[p] := Format('%6.2f', [PAntrieb]);
      end;

      { Parameter ansteuern }
      s := PComboSelectedText;
      if ParamCount < 2 then
      begin
       { do nothing }
      end
      else if (s = 'Controller') then
          Rigg.RealGlied[fpController] := PAntrieb
      else if (s = 'Winkel') then
        Rigg.RealGlied[fpWinkel] := PAntrieb * P180
      else if (s = 'Vorstag') then
        Rigg.RealGlied[fpVorstag] := PAntrieb
      else if (s = 'Wante') then
        Rigg.RealGlied[fpWante] := PAntrieb
      else if (s = 'Wante oben') then
        Rigg.RealGlied[fpWoben] := PAntrieb
      else if (s = 'Saling Höhe') then
        Rigg.RealGlied[fpSalingH] := PAntrieb
      else if (s = 'Saling Abstand') then
        Rigg.RealGlied[fpSalingA] := PAntrieb
      else if (s = 'Saling Länge') and (SalingTyp = stDrehbar) then
        Rigg.RealGlied[fpSalingL] := PAntrieb
      else if (s = 'Saling Länge') and (SalingTyp = stFest) then
      begin
        tempL := Rigg.RealGlied[fpSalingL];
        temp := PAntrieb/tempL;
        tempH := temp * Rigg.RealGlied[fpSalingH];
        tempA := temp * Rigg.RealGlied[fpSalingA];
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
      end
      else if (s = 'Saling Winkel') then
      begin
        temp := PAntrieb * P180;
        tempL := Rigg.RealGlied[fpSalingL];
        tempH := tempL * sin(temp);
        tempA := 2 * tempL * cos(temp);
        Rigg.RealGlied[fpSalingH] := tempH;
        Rigg.RealGlied[fpSalingA] := tempA;
      end;

      { Definitionsbereich bestimmen und Berechnungsschleife starten }
      Anfang := StrToInt(XminEditText);
      Ende := StrToInt(XmaxEditText);
      for i := 0 to LNr do
      begin
        if i mod 5 = 0 then
          ProgressPosition := i;

        Antrieb := Anfang + (Ende - Anfang) * i / LNr;

        { Antrieb ansteuern }
        s := XComboSelectedText;
        if (s = 'Controller') then
          Rigg.RealGlied[fpController] := Antrieb
        else if (s = 'Winkel') then
          Rigg.RealGlied[fpWinkel] := Antrieb * P180
        else if (s = 'Vorstag') then
          Rigg.RealGlied[fpVorstag] := Antrieb
        else if (s = 'Wante') then
          Rigg.RealGlied[fpWante] := Antrieb
        else if (s = 'Wante oben') then
          Rigg.RealGlied[fpWoben] := Antrieb
        else if (s = 'Saling Höhe') then
          Rigg.RealGlied[fpSalingH] := Antrieb
        else if (s = 'Saling Abstand') then
          Rigg.RealGlied[fpSalingA] := Antrieb
        else if (s = 'Saling Länge') and (SalingTyp = stDrehbar) then
          Rigg.RealGlied[fpSalingL] := Antrieb
        else if (s = 'Saling Länge') and (SalingTyp = stFest) then
        begin
          tempL := Rigg.RealGlied[fpSalingL];
          temp := Antrieb/tempL;
          tempH := temp * Rigg.RealGlied[fpSalingH];
          tempA := temp * Rigg.RealGlied[fpSalingA];
          Rigg.RealGlied[fpSalingH] := tempH;
          Rigg.RealGlied[fpSalingA] := tempA;
        end
        else if (s = 'Saling Winkel') then
        begin
          temp := Antrieb * P180;
          tempL := Rigg.RealGlied[fpSalingL];
          tempH := tempL * sin(temp);
          tempA := 2 * tempL * cos(temp);
          Rigg.RealGlied[fpSalingH] := tempH;
          Rigg.RealGlied[fpSalingA] := tempA;
        end;

        { Berechnen }
        if SalingTyp = stFest then
        begin
          if (XComboSelectedText = 'Winkel') or
             (PComboSelectedText = 'Winkel') then
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

        { Ergebnisse einspeichern }
        if yavVorstagSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavVorstagSpannung].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[14]
          else
            af[p,j,i] := 0;
        end;
        if yavWantenSpannung in YAchseSet then
        begin
          j := YAchseRecordList[yavWantenSpannung].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[8]
          else
            af[p,j,i] := 0;
        end;
        if yavMastfallF0F in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0F].ArrayIndex;
          af[p,j,i] := Abstand(Rigg.rP[ooF0],Rigg.rP[ooF]);
        end;
        if yavMastfallF0C in YAchseSet then
        begin
          j := YAchseRecordList[yavMastfallF0C].ArrayIndex;
          af[p,j,i] := Abstand(Rigg.rP[ooF0],Rigg.rP[ooC]);
        end;
        if yavAuslenkungC in YAchseSet then
        begin
          j := YAchseRecordList[yavAuslenkungC].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Abstand(Rigg.rP[ooC],Rigg.rPe[ooC])
          else
            af[p,j,i] := 0;
        end;
        if yavDurchbiegungHD in YAchseSet then
        begin
          j := YAchseRecordList[yavDurchbiegungHD].ArrayIndex;
          af[p,j,i] := Rigg.hd;
        end;
        if yavRF00 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF00].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[0]
          else
            af[p,j,i] := 0;
        end;
        if yavRF01 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF01].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[1]
          else
            af[p,j,i] := 0;
        end;
        if yavRF03 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF03].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[3]
          else
            af[p,j,i] := 0;
        end;
        if yavRF05 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF05].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[5]
          else
            af[p,j,i] := 0;
        end;
        if yavRF10 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF10].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[10]
          else
            af[p,j,i] := 0;
        end;
        if yavRF11 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF11].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[11]
          else
            af[p,j,i] := 0;
        end;
        if yavRF13 in YAchseSet then
        begin
          j := YAchseRecordList[yavRF13].ArrayIndex;
          if PunktOK then
            af[p,j,i] := Rigg.rF[13]
          else
            af[p,j,i] := 0;
        end;
      end;
    end;

  finally
    { Getriebe wiederherstellen }
    Rigg.ProofRequired := True;
    Rigg.Glieder := InputRec;
    UpdateGetriebe;
  end;
end;

procedure TChartForm.Draw;
begin

end;

procedure TChartForm.DrawNormal;
begin
  FShowGroup := False;
  LoadNormal;
  DrawInternal;
end;

procedure TChartForm.DrawInternal;
begin
  if FValid then
  begin
    LeftTitle := GetYText(YComboSelectedText);
    BottomTitle := XAchseText;
    RightTitle := Format('Parameter %s', [ParamText]);
    Xmin := XAchseMin;
    Xmax := XAchseMax;
    LookForYMinMax;
  end
  else
  begin
    TopTitle := 'Top Title';
    if FStatus = [] then
      LeftTitle := 'Diagramm wurde zurückgesetzt'
    else if csBerechnet in FStatus then
      LeftTitle := 'Kurve wurde nicht berechnet!'
    else if csGeladen in FStatus then
      LeftTitle := 'Kurve wurde nicht geladen!';
    BottomTitle := 'Bottom Title';
    RightTitle := 'Right Title';

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

procedure TChartForm.YComboChange(Sender: TObject);
var
  j, p: Integer;
begin
  if (YComboItemIndex < 0) or (YComboItemIndex > VNr-1) then
  begin
    FValid := False;
    YMinEditText := 'YMin';
    YMaxEditText := 'YMax';
    Draw;
    Exit;
  end;
  j := ComboIndexToCurve(YComboItemIndex);
  if not Valid then
  begin
    YMinEditText := 'YMin';
    YMaxEditText := 'YMax';
    Draw;
    Exit;
  end;
  for p := 0 to ParamCount - 1 do
    bf[p] := af[p, j];
  UpdateYMinMax;
  Draw; { auch TestF zeichnen }
end;

procedure TChartForm.DrawGroup;
begin
  if FShowGroup then
  begin
    DrawTogether
  end
  else
  begin
    DrawNormal;
  end;
end;

procedure TChartForm.DrawTogether;
begin
  if FStatus = [] then
    Exit;
  if KurvenzahlSpinnerValue > ParamCount then
    KurvenzahlSpinnerValue := ParamCount;
  ShowTogether(KurvenZahlSpinnerValue);
  FShowGroup := True;
end;

procedure TChartForm.ShowTogether(ParamNo: Integer);
var
  i, j, p: Integer;
  YAV: TYAchseValue;
  min, max, diff, temp: double;
  tempParamCount: Integer;
  tempPText: TYAchseStringArray;
begin
  if ParamNo > ParamCount then
    ParamNo := ParamCount;

  { bf füllen }
  p := 0; { p steht hier für die Anzahl der Kurven in YAchseSet }
  for i := 0 to YComboItems.Count - 1 do
  begin
    YAV := YAchseSortedList[i];
    if YAV in YAchseSet then
    begin
      j := YAchseRecordList[YAV].ArrayIndex;
      if p = PNr then
        break;
      bf[p] := af[ParamNo - 1, j];
      GroupText[p] := YAchseRecordList[YAV].ComboText;
      p := p + 1;
    end;
  end;

  GroupKurvenZahl := p;
  for p := 0 to GroupKurvenZahl - 1 do
  begin
    { Maximum und Minimum ermitteln }
    max := bf[p, 0];
    min := max;
    for i := 0 to LNr do
    begin
      if bf[p, i] > max then
        max := bf[p, i];
      if bf[p, i] < min then
        min := bf[p, i];
    end;

    { Normieren }
    diff := max - min;
    temp := p * 100 / GroupKurvenZahl;
    if max-min = 0 then
      for i := 0 to LNr do
        bf[p, i] := temp
    else
    begin
      for i := 0 to LNr do
      try
        bf[p, i] := (bf[p, i] - min) * 100 / diff;
      except on EMathError do
        bf[p, i] := 0;
      end;
    end;
  end;

  FValid := True;
  YMinEditText := 'YMin';
  YMaxEditText := 'YMax';

  TopTitle := '';
  LeftTitle := 'Alle Kurven normiert [%]';
  BottomTitle := XAchseText;
  RightTitle := Format('Parameter Nr.%d', [ParamNo]);

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

procedure TChartForm.UpdateYMinMax;
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
    YMinEditText := 'YMin';
    YMaxEditText := 'YMax';
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

procedure TChartForm.LookForYMinMax;
var
  i, j, p: Integer;
begin
  { Maximum und Minimum suchen über alle Parameter hinweg }
  if RggDocument.CalcTyp = ctQuerKraftBiegung then
  begin
    if (YComboSelectedText = 'Vorstag-Spannung') or
       (YComboSelectedText = 'Wanten-Spannung') then
    begin
      Ymax := 5000; { 5000 N }
      Ymin := -1000; { -1000 N }
      Exit;
    end;
    if (YComboSelectedText = 'Elastizität Punkt C') then
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
    YMinEditText := 'YMin';
    YMaxEditText := 'YMax';
    Exit;
  end;
  Ymax := af[p, j, 0];
  Ymin := Ymax;
  for p := 0 to ParamCount-1 do
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

function TChartForm.GetYText(Text: string): string;
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

procedure TChartForm.GetMemoText;
var
  p: Integer;
  YAV: TYAchseValue;
  xpName: TxpName;
  T: TTrimmTabDaten;
begin
  with MemoLines do
  begin
    Clear;
    { SalingTyp }
    case SalingTyp of
      stFest: Add('SalingTyp: Feste Salinge');
      stDrehbar: Add('SalingTyp: Drehbare Salinge');
      stOhne_2: Add('SalingTyp: Ohne Salinge (Mast biegsam)');
      stOhne: Add('SalingTyp: Ohne Salinge (Mast starr)');
    end;
    { ControllerTyp }
    case Rigg.ControllerTyp of
      ctDruck: Add('ControllerTyp: Controller überträgt Druck');
      ctOhne: Add('ControllerTyp: kein Controller');
    end;
    { CalcTyp }
    if SalingTyp = stOhne then
      Add('BerechnungsTyp: Wantenkraft vorgegeben');
    if SalingTyp <> stOhne then
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
    Add(Format('  %d ... %d', [XAchseMin, XAchseMax]));
    { P }
    if ParamCount > 1 then
    begin
      Add('');
      Add('Parameter: ' + ParamText);
      with MemoLines do
      begin
        for p := 0 to ParamCount-1 do
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
      if (SalingTyp <> stOhne) and ManipulatorMode and (xpName <> xpWinkel) then
        Add(Format(  '  Winkel: %g Grad', [RealGlied[fpWinkel] * D180]));
      if (SalingTyp <> stOhne) and not ManipulatorMode and (xpName <> xpVorstag) then
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
        Add(Format('  SalingLänge: %g mm', [RealGlied[fpSalingL]]));

      if (SalingTyp = stDrehbar) and (xpName <> xpSalingL) then
        Add(Format('  SalingLänge: %g mm', [RealGlied[fpSalingL]]));
      if (SalingTyp = stOhne) and (xpName <> xpVorstag) then { nicht VorstagOS - ok }
        Add(Format('  Vorstag: %g mm', [RealGlied[fpVorstagOS]]));
      if (SalingTyp = stOhne) and (xpName <> xpWPowerOS) then
        Add(Format('  Wantenspannung: %g N', [RealGlied[fpWPowerOS]]));
    end;
    { Koordinaten }
    Add('');
    Add('Rumpf: Koordinaten (x,y,z) [mm]');
    with Rigg do
    begin
      Add(Format('  A0(%g,%g,%g)', [rP[ooA0,x],rP[ooA0,y],rP[ooA0,z]]));
      Add(Format('  B0(%g,%g,%g)', [rP[ooB0,x],rP[ooB0,y],rP[ooB0,z]]));
      Add(Format('  C0(%g,%g,%g)', [rP[ooC0,x],rP[ooC0,y],rP[ooC0,z]]));
      Add(Format('  D0(%g,%g,%g)', [rP[ooD0,x],rP[ooD0,y],rP[ooD0,z]]));
      Add(Format('  E0(%g,%g,%g)', [rP[ooE0,x],rP[ooE0,y],rP[ooE0,z]]));
      Add(Format('  F0(%g,%g,%g)', [rP[ooF0,x],rP[ooF0,y],rP[ooF0,z]]));
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
  end;
end;

procedure TChartForm.XComboChange(Sender: TObject);
begin
  UpdateXMinMax;
  FXTextClicked := XComboSelectedText;
  if (XComboSelectedText = XComboText) and (csBerechnet in FStatus) then
    XLEDFillColor := claLime
  else
    XLEDFillColor := claRed;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.PComboChange(Sender: TObject);
begin
  UpdatePMinMax;
  FPTextClicked := PComboSelectedText;
  if (PComboSelectedText = PComboText) and (csBerechnet in FStatus) then
    PLEDFillColor := claLime
  else
    PLEDFillColor := claRed;
end;

procedure TChartForm.UpdateXCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with XComboItems do
  begin
    Clear;
    if SalingTyp = stFest then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Winkel');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Höhe');
      Add('Saling Abstand');
      Add('Saling Länge');
      Add('Saling Winkel');
    end;
    if SalingTyp = stDrehbar then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Wante');
      Add('Wante oben');
      Add('Saling Länge');
    end;
    if SalingTyp = stOhne_2 then
    begin
      Add('Controller');
      Add('Vorstag');
      Add('Wante');
    end;
    if SalingTyp = stOhne then
    begin
      Add('Vorstag');
    end;
  end;
  XComboItemIndex := XComboItems.IndexOf('Vorstag');
  for i := 0 to XComboItems.Count-1 do
    if (XComboItems[i] = FXTextClicked) then
    begin
      XComboItemIndex := i;
      break;
    end;
  UpdatePCombo(SalingTyp);
end;

procedure TChartForm.UpdatePCombo(SalingTyp: TSalingTyp);
var
  i: Integer;
begin
  with PComboItems do
  begin
    Clear;
    Add('kein Parameter');
    if XComboSelectedText = 'Controller' then
    begin
      if SalingTyp = stFest then
      begin
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Vorstag');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Vorstag');
        Add('Wante');
      end;
    end
    else if (XComboSelectedText = 'Vorstag') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Controller');
        Add('Wante');
      end;
    end
    else if (XComboSelectedText = 'Winkel') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboSelectedText = 'Wante') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante oben');
        Add('Saling Länge');
      end;
      if SalingTyp = stOhne_2 then
      begin
        Add('Controller');
        Add('Vorstag');
      end;
    end
    else if (XComboSelectedText = 'Wante oben') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante');
        Add('Saling Länge');
      end;
    end
    else if (XComboSelectedText = 'Saling Höhe') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Abstand');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboSelectedText = 'Saling Abstand') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Länge');
        Add('Saling Winkel');
      end;
    end
    else if (XComboSelectedText = 'Saling Länge') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
        Add('Saling Höhe');
        Add('Saling Abstand');
        Add('Saling Winkel');
      end;
      if SalingTyp = stDrehbar then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Wante');
        Add('Wante oben');
      end;
    end
    else if (XComboSelectedText = 'Saling Winkel') then
    begin
      if SalingTyp = stFest then
      begin
        Add('Controller');
        Add('Vorstag');
        Add('Winkel');
        Add('Wante');
        Add('Wante oben');
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

function TChartForm.GetXText(Text: string): string;
var
  s: string;
begin
  if Text = 'Controller' then s := 'Zustellung Mast-Controller [mm]'
  else if Text = 'Winkel' then s := 'Winkel [1E-1 Grad]'
  else if Text = 'Vorstag' then s := 'Vorstaglänge [mm]'
  else if Text = 'Wante' then s := 'Wantenlänge [mm]'
  else if Text = 'Wante oben' then s := 'Länge des oberen Wantenabschnitts [mm]'
  else if Text = 'Saling Höhe' then s := 'Höhe des Salingdreiecks [mm]'
  else if Text = 'Saling Abstand' then s := 'Saling-Abstand [mm]'
  else if Text = 'Saling Länge' then s := 'Saling-Länge [mm]'
  else if Text = 'Saling Winkel' then s := 'Saling-Winkel [Grad]';
  result := s;
end;

function TChartForm.GetPText(Text: string): string;
var
  s: string;
begin
  if Text = 'Controller' then s := 'Zustellung Mast-Controller [mm]'
  else if Text = 'Winkel' then s := 'Winkel [1E-1 Grad]'
  else if Text = 'Vorstag' then s := 'Vorstaglänge [mm]'
  else if Text = 'Wante' then s := 'Wantenlänge [mm]'
  else if Text = 'Wante oben' then s := 'Länge des oberen Wantenabschnitts [mm]'
  else if Text = 'Saling Höhe' then s := 'Höhe des Salingdreiecks [mm]'
  else if Text = 'Saling Abstand' then s := 'Saling-Abstand [mm]'
  else if Text = 'Saling Länge' then s := 'Saling-Länge [mm]'
  else if Text = 'Saling Winkel' then s := 'Saling-Winkel [Grad]';
  result := s;
end;

function TChartForm.GetTsbName(Text: string): TxpName;
var
  xp: TxpName;
begin
  xp := xpController;
  if Text = 'Winkel' then xp := xpWinkel
  else if Text = 'Vorstag' then xp := xpVorstag
  else if Text = 'Wante' then xp := xpWante
  else if Text = 'Wante oben' then xp := xpWoben
  else if Text = 'Saling Höhe' then xp := xpSalingH
  else if Text = 'Saling Abstand' then xp := xpSalingA
  else if Text = 'Saling Länge' then xp := xpSalingL
  else if Text = 'Saling Winkel' then xp := xpSalingW;
  { else if Text = 'Vorstag OS' then xp := xpVorstagOS }
  { else if Text = 'Wantenkraft OS' then xp := xpWPowerOS }
  result := xp;
end;

procedure TChartForm.UpdateXMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := XComboSelectedText;

  if s = 'Controller' then xp := xpController
  else if s = 'Winkel' then xp := xpWinkel
  else if s = 'Vorstag' then xp := xpVorstag
  else if s = 'Wante' then xp := xpWante
  else if s = 'Wante oben' then xp := xpWoben
  else if s = 'Saling Höhe' then xp := xpSalingH
  else if s = 'Saling Abstand' then xp := xpSalingA
  else if s = 'Saling Länge' then xp := xpSalingL
  else if s = 'Saling Winkel' then xp := xpSalingW
  else
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
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
    if BPBtnIsPressed then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnIsPressed then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(XMinEditText);
        Maximum := StrToInt(XMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    XMinEditText := IntToStr(Minimum);
    XMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      XMinEditText := '0';
      XMaxEditText := '100';
      Valid := False;
      XLEDFillColor := claRed;
    end;
  end;
end;

procedure TChartForm.UpdatePMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := PComboSelectedText;
  if s = ('kein Parameter') then
  begin
    PMinEditText := '0';
    PMaxEditText := '0';
    KurvenZahlSpinnerValue := 1;
    Exit;
  end;

  if s = 'Controller' then xp := xpController
  else if s = 'Winkel' then xp := xpWinkel
  else if s = 'Vorstag' then xp := xpVorstag
  else if s = 'Wante' then xp := xpWante
  else if s = 'Wante oben' then xp := xpWoben
  else if s = 'Saling Höhe' then xp := xpSalingH
  else if s = 'Saling Abstand' then xp := xpSalingA
  else if s = 'Saling Länge' then xp := xpSalingL
  else if s = 'Saling Winkel' then xp := xpSalingW
  else
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
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
    if BPBtnIsPressed then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnIsPressed then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(PMinEditText);
        Maximum := StrToInt(PMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    PMinEditText := IntToStr(Minimum);
    PMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      PMinEditText := '0';
      PMaxEditText := '100';
      Valid := False;
      PLEDFillColor := claRed;
    end;
  end;
end;

function  TChartForm.CheckBeforeCalc: Boolean;
begin
  result := True;
  if (XMinEditText = 'XMinEdit') or (XMaxEditText = 'XMaxEdit') then
    UpdateXMinMax;
  if (PMinEditText = 'PMinEdit') or (PMaxEditText = 'PMaxEdit') then
    UpdatePMinMax;
//  if not ValidateInput(XMinEdit) then result := False;
//  if not ValidateInput(XMaxEdit) then result := False;
//  if not ValidateInput(PMinEdit) then result := False;
//  if not ValidateInput(PMaxEdit) then result := False;
end;

procedure TChartForm.DoAfterCalc;
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

procedure TChartForm.UpdateGetriebe;
begin
  Rigg.UpdateGetriebe;

  if (Rigg.GetriebeOK and Rigg.MastOK) then
    Rigg.UpdateRigg;
end;

procedure TChartForm.InitSpinner;
begin
//  KurvenZahlSpinner.Increment := 1;
//  KurvenZahlSpinner.Min := 1;
  KurvenZahlSpinnerMax := ANr;
  KurvenZahlSpinnerValue := 1;

//  PSpinner.Increment := 1;
//  PSpinner.Min := 1;
  PSpinnerMax := PNr;
  PSpinnerValue := 1;

//  APSpinner.Increment := 1;
//  APSpinner.Min := 1;
  APSpinnerMax := 100;
  APSpinnerValue := APWidth;
end;

procedure TChartForm.InitRigg;
begin
  Rigg := Main.RggMain.Rigg;
  FSalingTyp := Rigg.SalingTyp;
end;

function TChartForm.XComboSelectedText: string;
begin
  if XComboItemIndex = -1 then
  begin
    XComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := XComboItems[XComboItemIndex];
end;

function TChartForm.YComboSelectedText: string;
begin
  if YComboItemIndex = -1 then
  begin
    YComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := YComboItems[YComboItemIndex];
end;

function TChartForm.PComboSelectedText: string;
begin
  if PComboItemIndex = -1 then
  begin
    PComboItemIndex := 0;
  end;

  { result := XComboBox.Selected.Text; }
  result := PComboItems[PComboItemIndex];
end;

procedure TChartForm.SuperInit;
begin
  XComboItemIndex := XComboItems.IndexOf('Wante');
  XComboChange(nil);

  { SalingHöhe }
  PComboItemIndex := PComboItems.IndexOf('Saling Höhe');
  PComboChange(nil);

  { Mastfall F0F }
  YComboItemIndex := YComboItems.IndexOf('Mastfall F0F');
  YComboChange(nil);

  UpdateYAchseList;

  KurvenZahlSpinnerValue := 3;
end;

procedure TChartForm.SuperCalc;
begin
  Calc;
end;

function TChartForm.GetChecked(fa: TFederAction): Boolean;
begin
  case fa of
    faChartRect: result := WantRectangles;
    faChartTextRect: result := WantTextRect;
    faChartLegend: result := WantLegend;
    faChartAP: result := APBtnIsPressed;
    faChartBP: result := BPBtnIsPressed;
    faChartGroup: result := FShowGroup;
    else
      result := False;
  end;
end;

procedure TChartForm.HandleAction(fa: Integer);
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
      APItemClick(nil);
      SuperCalc;
//      Draw;
    end;

    faChartBP:
    begin
      BPItemClick(nil);
      SuperCalc;
//      Draw;
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
      Calc;
    end;

    faPComboMinus:
    begin
      Dec(PComboItemIndex);
      if PComboItemIndex < 0 then
        PComboItemIndex := PComboItems.Count - 1;
      Calc;
    end;

    faXComboPlus:
    begin
      Inc(XComboItemIndex);
      if XComboItemIndex >= XComboItems.Count then
        XComboItemIndex := 0;
      Calc;
    end;

    faXComboMinus:
    begin
      Dec(XComboItemIndex);
      if XComboItemIndex < 0 then
        XComboItemIndex := XComboItems.Count - 1;
      Calc;
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

end;

end.
