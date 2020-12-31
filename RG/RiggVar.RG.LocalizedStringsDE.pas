unit RiggVar.RG.LocalizedStringsDE;

interface

uses
  RiggVar.RG.LocalizedStrings;

type
  TRggLocalizedStringsDE = class(TRggLocalizedStrings)
  public
    class procedure InitDE;
  end;

implementation

class procedure TRggLocalizedStringsDE.InitDE;
begin
  { from RggUnit0 }
  Ebenen_senkrecht_in_GetSalingDaten_String := 'Ebenen senkrecht in GetSalingDaten!';

  Status_String_Getriebe := '  Getriebe:';
  Status_String_OK := ' O.K.';
  Status_String_WanteZuKurz := ' Wante zu kurz.';
  Status_Format_String_WanteZuLang := ' Wante um %5.2f mm zu lang!';

  { from RggUnit1 }
  LogList_String_InUpdateGetriebeFS := 'TGetriebeFS.UpdateGetriebeFS:';
  LogList_String_FalseInPsiVonPhi := '  svar False in PsivonPhi';
  LogList_String_InBerechneWinkel := 'TGetriebeFS.BerechneWinkel:';
  LogList_Format_String_GetVorstagNullFest := 'GetVorstagNull, stFest/%d: %s';
  LogList_Format_String_GetVorstagNullDrehbar := 'GetVorstagNull, stDrehbar/%d: %s';
  LogList_Format_String_GetVorstagNullOhne := 'GetVorstagOhne, stDrehbar/%d: %s';
  LogList_Format_String_GetVorstagNullException := 'GetVorstagNull: %s';

  { from RggUnit2 }
  Status_String_Mast := '  Mast:';
  Status_String_MastBiegungNegativ := ' Mastbiegung negativ';
  Status_String_MastControllerBeyondMiddle := ' Controller set beyond middle';
  Status_String_MastControllerTooFarBack := ' Controller too far back';
  Status_String_MastControllerTooFar := ' Controller too far';

  LogString_SolveKG21_Except := 'SolveKG21: EZeroDivide;';
  LogString_W1 := ' W1';
  LogString_W2 := ' W2';
  LogString_W3 := ' W3';
  LogString_D := ' D';
  LogString_AreNull := ' sind Null!';

  LogString_ZeroDivideAlpha := 'FanOut: EZeroDivide; cos(alpha?) = 0';

  { from RggUnit3 }
  EA_IniString := 'EA';
  Status_String_Rigg := '  Rigg:';
  Status_String_RiggLetzteRechnungOK := '    Letzte Rechnung O.K.';
  Status_String_RiggNichtEntspannbar := '    Nicht entspannbar';
  Status_String_RiggWanteAufDruck := '    Wante auf Druck';
  Status_String_RiggForceTooBig := '    Force too big';

  LogList_String_WanteAufDruck := 'TRiggFS.Kraefte: Wante auf Druck';
  LogList_String_LengthDeviation := 'Rigg.Split: Längenabweichung';
  LogList_Format_String_BetragTooBig := 'TRiggFS.Split: Betrag rF[%d] > 32000 N';
  LogList_Format_String_ProbeOfPoint := 'TRiggFS.Probe: Probe %s = %6.2f';

  LogList_String_ProbeFalsch := 'TRigg.Probe: Probe falsch';
  LogList_String_ProbeOK := 'TRigg.Probe: Probe O.K.';

  LogList_String_MakeRumpfKoordExcept := 'TRiggFS.MakeRumpfKoord:  ';
  LogList_Format_String_MakeKoord := 'TRiggFS.MakeKoord, %d. Aufruf: %s';
  LogList_String_MakeKoordExept := 'TRiggFS.MakeKoord:  ';

  LogList_FormatString_MakeKoordDS := 'TRiggFS.MakeKoordDS/%d: %s';
  LogList_String_MakeKoordDSExept := 'TRiggFS.MakeKoordDS:  ';

  LogList_Format_String_MakeKoordOS := 'TRiggFS.MakeKoordOS, %d. Aufruf: %s';
  LogList_String_MakeKoordExeptOS := 'TRiggFS.MakeKoordOS:  ';

  VOString := 'V0';
  WAString := 'WA';
  WOString := 'WO';
  SHString := 'SH';
  SAString := 'SA';
  SLString := 'SL';
  SWString := 'SW';
  MVString := 'MV';

  MFString := 'MF';
  F0FString := 'F0F';
  F0CString := 'F0C';
  BieString := 'Bie';
  BGFString := 'BGF';

  NoParamString := 'kein Parameter';

  ControllerString := 'Controller';
  WinkelString := 'Winkel';
  VorstagString := 'Vorstag';
  WanteString := 'Wante';
  WanteObenString := 'Wante Oben';

  SalingAString := 'Saling Abstand';
  SalingHString := 'Saling Höhe';
  SalingLString := 'Saling Länge';
  SalingWString := 'Saling Winkel';

  MastFootD0XString := 'Mastfuß D0x';

  VorstagOhneSalingString := 'Vorstag OS';
  WantenkraftOhneSalingString := 'Wantenkraft OS';

  BiegungString := 'Biegung';
  MastfallVorlaufString := 'Mastfall Vorlauf';
  APWidthString := 'AP Width';
  EAHullString := 'EA Hull';
  EARiggString := 'EA Rigg';
  EIMastString := 'EI Mast';

  SalingHTooSmallString := ' Salinghöhe zu klein!';

  MastfallF0CString := 'Mastfall F0C';
  MastfallF0FString := 'Mastfall F0F';
  VorstagSpannungString := 'Vorstag-Spannung';
  WantenSpannungString := 'Wanten-Spannung';
  DurchbiegungHDString := 'Durchbiegung hd';
  ElasticityPointCString := 'Elastizität Punkt C';

  AnfangsZustandString := 'Diagramm befindet sich im Anfangszustand.';
  ResetMessageString := 'Diagramm wurde in den Anfangszustand versetzt.';

  BlueString := 'Blau';
  RedString := 'Rot';
  GreenString := 'Grün';
  WhiteString := 'Weiß';
  YellowString := 'Gelb';

  ProgressCaptionString := 'Kurve wird berechnet';
  ProgressCaptionFormatString := 'Parameter %d von %d';

  TopTitleString := 'RiggChart';

  SalingFestString := 'fest';
  SalingDrehbarString := 'drehbar';
  OhneSalingString := 'ohne Saling biegt';
  OhneSalingStarrString := 'ohne Saling starr';

  TopTitleTestString := 'Top Title';
  BottomTitleTestString := 'Bottom Title';
  ParamTitleTestString := 'Param Title';

  StatusResetString := 'Diagramm wurde zurückgesetzt';
  StatusNotComputedString := 'Kurve wurde nicht berechnet!';
  StatusNotLoadedString := 'Kurve wurde nicht geladen!';

  AllCurvesNormalizedString := 'Alle Kurven normiert [%]';

  YMinString := 'YMin';
  YMaxString := 'YMax';

  ControllerText := 'Zustellung Mast-Controller [mm]';
  WinkelText := 'Winkel [1E-1 Grad]';
  VorstagText := 'Vorstaglänge [mm]';
  WanteText := 'Wantenlänge [mm]';
  WanteObenText := 'Länge des oberen Wantenabschnitts [mm]';
  SalingHText := 'Höhe des Salingdreiecks [mm]';
  SalingAText := 'Saling-Abstand [mm]';
  SalingLText := 'Saling-Länge [mm]';
  SalingWText := 'Saling-Winkel [Grad]';

  XMinEditString := 'XMinEdit';
  XMaxEditString := 'XMaxEdit';

  PMinEditString := 'PMinEdit';
  PMaxEditString := 'PMaxEdit';

  YMinEditString := 'YMinEdit';
  YMaxEditString := 'YMaxEdit';

  PIdentString := 'Nr.';

  { from FormConfig }

  MsgStr_NotFound := 'nicht gefunden';

  ComboTextHullRods := 'Rumpflängen';
  ComboTextWanten := 'Wanten';
  ComboTextVorstag := 'Vorstag';
  ComboTextSpreader := 'Salinge';
  ComboTextSpreaderConnection := 'Saling-Verbindung';
  ComboTextMast := 'Mast';

  LabelText_WinkelInGrad := 'Winkel in Grad';
  LabelText_DistanceInMM := 'Abmessungen in mm';

  MastComboTextController := 'Controller';
  MastComboTextSpreader := 'Saling';
  MastComboTextShroud := 'Wante';
  MastComboTextTop := 'Top';

  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Abbrechen';

  TrimmPageCaption := 'Trimm';

  TrimmGroupBoxCaption := 'Längen';
  PosLabelCaption := 'Pos';
  MinLabelCaption := 'Min';
  MaxLabelCaption := 'Max';

  LengthEditLabelCaption := 'Abmessungen in mm';
  TrimmComboLabelCaption := 'Trimmvariable';

  GroupBoxMaterialCaption := 'Material';

  FachwerkPageCaption := 'Fachwerk';
  ElementLabelCaption := 'Fachwerkstäbe';
  EAEditText := 'EAEdit';
  EAEditLabelCaption := 'EA in KN';
  TakeOverBtnCaption := 'Auswahl übernehmen';

  MaterialComboLabelCaption := 'Material';
  QuerschnittComboLabelCaption := 'Querschnitt';
  ALabelCaption := 'A';
  ELabelCaption := 'E';
  AEditText := 'AEdit';
  EEditText := 'EEdit';
  EEditLabelCaption := 'E-Modul in KN/mm^2';
  AEditLabelCaption := 'Querschnitt in mm^2';

  MastPageCaption := 'Mast';
  GroupBoxMastCaption := 'Mast';
  MastTypeComboLabelCaption := 'Profil';
  EIEditText := 'EIEdit';
  EILabelCaption := 'Biegesteifigkeit EI in Nm^2';
  MastMassComboLabelCaption := 'Abmessungen';
  MassMassEditLabelCaption := 'Abstand vom Mastfuß in mm';

  HullPageCaption := 'Rumpf';
  GroupBoxHullCaption := 'Feld Editieren';
  FieldString := 'Feld';
  RumpfBtnCaption := 'Übernehmen';

  IniMemoPageCaption := 'Rigg.ini';

  SaveIniBtnCaption := 'Speichern';
  LoadIniBtnCaption := 'Laden';

  EA_S_Key := 'EA Small';
  EA_M_Key := 'EA Medium';
  EA_L_Key := 'EA Large';

  SWarningText := 'Änderungen in %s sichern?';
end;

end.
