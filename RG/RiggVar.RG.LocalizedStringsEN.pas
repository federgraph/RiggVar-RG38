unit RiggVar.RG.LocalizedStringsEN;

interface

uses
  RiggVar.RG.LocalizedStrings;

type
  TRggLocalizedStringsEN = class(TRggLocalizedStrings)
  public
    class procedure InitEN;
  end;

implementation

class procedure TRggLocalizedStringsEN.InitEN;
begin
  { from RggUnit0 }
  Ebenen_senkrecht_in_GetSalingDaten_String := 'Planes perpendicular in GetSalingDaten!';

  Status_String_Getriebe := '  Getriebe:';
  Status_String_OK := ' O.K.';
  Status_String_WanteZuKurz := ' Shroud is too short.';
  Status_Format_String_WanteZuLang := ' Shroud is %5.2f mm too long!';

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
  LogString_AreNull := ' are Zero!';

  LogString_ZeroDivideAlpha := 'Fan out: EZeroDivide; cos(alpha?) = 0';

  { from RggUnit3 }
  EA_IniString := 'EA';
  Status_String_Rigg := '  Rigg:';
  Status_String_RiggLetzteRechnungOK := '    Last computation O.K.';
  Status_String_RiggNichtEntspannbar := '    Not relaxable';
  Status_String_RiggWanteAufDruck := '    Shroud has pressure';
  Status_String_RiggForceTooBig := '    Force too big';

  LogList_String_WanteAufDruck := 'TRiggFS.Kraefte: shroud has pressure';
  LogList_String_LengthDeviation := 'Rigg.Split: length deviation';
  LogList_Format_String_BetragTooBig := 'TRiggFS.Split: magnitude of rF[%d] > 32000 N';
  LogList_Format_String_ProbeOfPoint := 'TRiggFS.Probe: proof %s = %6.2f';

  LogList_String_ProbeFalsch := 'TRigg.Probe: proof failed';
  LogList_String_ProbeOK := 'TRigg.Probe: proof is O.K.';

  LogList_String_MakeRumpfKoordExcept := 'TRiggFS.MakeRumpfKoord:  ';
  LogList_Format_String_MakeKoord := 'TRiggFS.MakeKoord, %d. call: %s';
  LogList_String_MakeKoordExept := 'TRiggFS.MakeKoord:  ';

  LogList_FormatString_MakeKoordDS := 'TRiggFS.MakeKoordDS/%d: %s';
  LogList_String_MakeKoordDSExept := 'TRiggFS.MakeKoordDS:  ';

  LogList_Format_String_MakeKoordOS := 'TRiggFS.MakeKoordOS, %d. call: %s';
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

  NoParamString := 'no parameter';

  ControllerString := 'Controller';
  WinkelString := 'Angle';
  VorstagString := 'Headstay';
  WanteString := 'Shroud';
  WanteObenString := 'Shroud Upper';

  SalingAString := 'Spreader Distance';
  SalingHString := 'Spreader Height';
  SalingLString := 'Spreader Length';
  SalingWString := 'Spreader Angle';

  MastFootD0XString := 'Mastfoot D0x';

  VorstagOhneSalingString := 'Vorstag OS';
  WantenkraftOhneSalingString := 'Wantenkraft OS';

  BiegungString := 'Bending';
  MastfallVorlaufString := 'Mastfall Vorlauf';
  APWidthString := 'AP Width';
  EAHullString := 'EA Hull';
  EARiggString := 'EA Rigg';
  EIMastString := 'EI Mast';

  SalingHTooSmallString := ' Spreader height too small!';

  MastfallF0CString := 'Mastfall F0C';
  MastfallF0FString := 'Mastfall F0F';
  VorstagSpannungString := 'Headstay-Tension';
  WantenSpannungString := 'Shroud-Tension';
  DurchbiegungHDString := 'Displacement hd';
  ElasticityPointCString := 'Elasticity at Point C';

  AnfangsZustandString := 'Diagram is in initial state.';
  ResetMessageString := 'Diagram has been reset to initial state.';

  BlueString := 'Blue';
  RedString := 'Red';
  GreenString := 'Green';
  WhiteString := 'White';
  YellowString := 'Yellow';

  ProgressCaptionString := 'Curve is beeing computed';
  ProgressCaptionFormatString := 'Parameter %d of %d';

  TopTitleString := 'RiggChart';

  SalingFestString := 'fixed';
  SalingDrehbarString := 'pivotable';
  OhneSalingString := 'w/o Spreader bending';
  OhneSalingStarrString := 'w/o Spreaders stiff';

  TopTitleTestString := 'Top Title';
  BottomTitleTestString := 'Bottom Title';
  ParamTitleTestString := 'Param Title';

  StatusResetString := 'Diagram has been reset';
  StatusNotComputedString := 'Curve not computed!';
  StatusNotLoadedString := 'Curve not loaded!';

  AllCurvesNormalizedString := 'All Curves normalized [%]';

  YMinString := 'YMin';
  YMaxString := 'YMax';

  ControllerText := 'Position Mast-Controller [mm]';
  WinkelText := 'Angle [1E-1 Degrees]';
  VorstagText := 'Headstay length [mm]';
  WanteText := 'Shroud length [mm]';
  WanteObenText := 'Length of upper part of shroud [mm]';
  SalingHText := 'Height of spreader triangle [mm]';
  SalingAText := 'Spreader-Distance [mm]';
  SalingLText := 'Spreader-Length [mm]';
  SalingWText := 'Spreader-Angle [Degrees]';

  XMinEditString := 'XMinEdit';
  XMaxEditString := 'XMaxEdit';

  PMinEditString := 'PMinEdit';
  PMaxEditString := 'PMaxEdit';

  YMinEditString := 'YMinEdit';
  YMaxEditString := 'YMaxEdit';

  PIdentString := 'No.';

  { from FormConfig }

  MsgStr_NotFound := 'not found';

  ComboTextHullRods := 'Hull lengths';
  ComboTextWanten := 'Shroud';
  ComboTextVorstag := 'Headstay';
  ComboTextSpreader := 'Spreaders';
  ComboTextSpreaderConnection := 'Spreader-Fixture';
  ComboTextMast := 'Mast';

  LabelText_WinkelInGrad := 'Angle in Degrees';
  LabelText_DistanceInMM := 'Distance in mm';

  MastComboTextController := 'Controller';
  MastComboTextSpreader := 'Saling';
  MastComboTextShroud := 'Wante';
  MastComboTextTop := 'Top';

  OKBtnCaption := 'OK';
  CancelBtnCaption := 'Cancel';

  TrimmPageCaption := 'Trim';

  TrimmGroupBoxCaption := 'Lengths';
  PosLabelCaption := 'Pos';
  MinLabelCaption := 'Min';
  MaxLabelCaption := 'Max';

  LengthEditLabelCaption := 'Lengths in mm';
  TrimmComboLabelCaption := 'Trim variable';

  GroupBoxMaterialCaption := 'Material';

  FachwerkPageCaption := 'Framework';
  ElementLabelCaption := 'Framework rods';
  EAEditText := 'EAEdit';
  EAEditLabelCaption := 'EA in KN';
  TakeOverBtnCaption := 'Commit selection';

  MaterialComboLabelCaption := 'Material';
  QuerschnittComboLabelCaption := 'Cross section';
  ALabelCaption := 'A';
  ELabelCaption := 'E';
  AEditText := 'AEdit';
  EEditText := 'EEdit';
  EEditLabelCaption := 'E-Modul in KN/mm^2';
  AEditLabelCaption := 'Cross section area in mm^2';

  MastPageCaption := 'Mast';
  GroupBoxMastCaption := 'Mast';
  MastTypeComboLabelCaption := 'Profil';
  EIEditText := 'EIEdit';
  EILabelCaption := 'EI in Nm^2';
  MastMassComboLabelCaption := 'Measurements';
  MassMassEditLabelCaption := 'Distance from Mast foot in mm';

  HullPageCaption := 'Hull';
  GroupBoxHullCaption := 'Edit Field';
  FieldString := 'Field';
  RumpfBtnCaption := 'Commit';

  IniMemoPageCaption := 'Rigg.ini';

  SaveIniBtnCaption := 'Save';
  LoadIniBtnCaption := 'Load';

  EA_S_Key := 'EA Small';
  EA_M_Key := 'EA Medium';
  EA_L_Key := 'EA Large';

  SWarningText := 'Save changes in %s?';

  { Ausgabe }
  AusgabeRLHeading := 'Length values under load in mm (vector rL)';
  AusgabeRLEHeading := 'Length values relaxed in mm (vector rLe)';
  AusgabeDiffLHeading := 'Length value diff in mm  (rLe[i]-rL[i])';
  AusgabeRPHeading := 'Coordinates under load in mm (vector rP)';
  AusgabeRPEHeading := 'Coordinates of relaxed position in mm (vector rPe)';
  AusgabeDiffPHeading := 'Point displacement in mm (rPe[i]-rP[i])';
  AusgabeRFHeading := 'Force in N (vector rF)';
  AusgabeWinkelHeading := 'Angles';
  AusgabeTrimmControlsHeading := 'Settings (TTrimmControls)';
  AusgabeSalingDatenHeading := 'Spreader settings (TSalingDaten)';
  AusgabeLogHeading := 'Log';
  AusgabeVerschiebungenHeading := 'Displacement of points';

  AusgabeTokenGrad := 'Degrees';
  AusgabeTokenUpdatedOnly := 'updated only in mode SofortBerechnen';
  AusgabeTokenAuflagerKraftN := 'Point force in N';
  AusgabeTokenBelastungN := 'Load in N';
  AusgabeTokenKoordinatenMM := 'Point coordinates in mm';
  AusgabeTokenStabKraftN := 'Rod force in N';
  AusgabeTokenEAN := 'EA value in KN';
  AusgabeTokenStabElast := 'Elasticity in µm/N';
end;

end.
