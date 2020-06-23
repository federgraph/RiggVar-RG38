unit RggStrings;

interface

const
  { from RggUnit0 }
  Ebenen_senkrecht_in_GetSalingDaten_String: string = 'Ebenen senkrecht in GetSalingDaten!';

  Status_String_Getriebe: string = '  Getriebe:';
  Status_String_OK: string = ' O.K.';
  Status_String_WanteZuKurz: string = ' Wante zu kurz.';
  Status_Format_String_WanteZuLang: string = ' Wante um %5.2f mm zu lang!';

  Rigg_IniSectionString: string = 'Rigg';
  Mast_IniSectionString: string = 'Mast';
  Ist_IniSectionString: string = 'Ist';
  Min_IniSectionString: string = 'Min';
  Max_IniSectionString: string = 'Max';
  Koordinaten_Rumpf_IniSectionString: string = 'Koordinaten Rumpf';
  Koordinaten_Rigg_IniSectionString: string = 'Koordinaten Rigg';

  SalingTyp_IniString: string = 'SalingTyp';
  MastL_IniString: string = 'MastL';
  Mastunten_IniString: string = 'Mastunten';
  Mastoben_IniString: string = 'Mastoben';
  MastfallVorlauf_IniString: string = 'MastfallVorlauf';

  Controller_IniString: string = 'Controller';
  Winkel_IniString: string = 'Winkel';
  Vorstag_IniString: string = 'Vorstag';
  Wante_IniString: string = 'Wante';
  Woben_IniString: string = 'Woben';
  SalingH_IniString: string = 'SalingH';
  SalingA_IniString: string = 'SalingA';
  SalingL_IniString: string = 'SalingL';
  VorstagOS_IniString: string = 'VorstagOS';
  WPowerOS_IniString: string = 'WPowerOS';

  A0x_IniString: string = 'A0x';
  A0y_IniString: string = 'A0y';
  A0z_IniString: string = 'A0z';
  B0x_IniString: string = 'B0x';
  B0y_IniString: string = 'B0y';
  B0z_IniString: string = 'B0z';
  C0x_IniString: string = 'C0x';
  C0y_IniString: string = 'C0y';
  C0z_IniString: string = 'C0z';
  D0x_IniString: string = 'D0x';
  D0y_IniString: string = 'D0y';
  D0z_IniString: string = 'D0z';
  E0x_IniString: string = 'E0x';
  E0y_IniString: string = 'E0y';
  E0z_IniString: string = 'E0z';
  F0x_IniString: string = 'F0x';
  F0y_IniString: string = 'F0y';
  F0z_IniString: string = 'F0z';

  Ax_IniString: string = 'Ax';
  Ay_IniString: string = 'Ay';
  Az_IniString: string = 'Az';
  Bx_IniString: string = 'Bx';
  By_IniString: string = 'By';
  Bz_IniString: string = 'Bz';
  Cx_IniString: string = 'Cx';
  Cy_IniString: string = 'Cy';
  Cz_IniString: string = 'Cz';
  Dx_IniString: string = 'Dx';
  Dy_IniString: string = 'Dy';
  Dz_IniString: string = 'Dz';
  Ex_IniString: string = 'Ex';
  Ey_IniString: string = 'Ey';
  Ez_IniString: string = 'Ez';
  Fx_IniString: string = 'Fx';
  Fy_IniString: string = 'Fy';
  Fz_IniString: string = 'Fz';

  { from RggUnit1 }
  LogList_String_InUpdateGetriebeFS: string = 'TGetriebeFS.UpdateGetriebeFS:';
  LogList_String_FalseInPsiVonPhi: string = '  svar False in PsivonPhi';
  LogList_String_InBerechneWinkel: string = 'TGetriebeFS.BerechneWinkel:';
  LogList_Format_String_GetVorstagNullFest: string = 'GetVorstagNull, stFest/%d: %s';
  LogList_Format_String_GetVorstagNullDrehbar: string = 'GetVorstagNull, stDrehbar/%d: %s';
  LogList_Format_String_GetVorstagNullOhne: string = 'GetVorstagOhne, stDrehbar/%d: %s';
  LogList_Format_String_GetVorstagNullException: string = 'GetVorstagNull: %s';

  {from RggUnit2 }
  Status_String_Mast: string = '  Mast:';
  Status_String_MastBiegungNegativ: string = ' Mastbiegung negativ';
  Status_String_MastControllerBeyondMiddle: string = ' Controller set beyond middle';
  Status_String_MastControllerTooFarBack: string = ' Controller too far back';
  Status_String_MastControllerTooFar: string = ' Controller too far';

  ControllerTyp_IniString: string = 'ControllerTyp';
  CalcTyp_IniString: string = 'CalcTyp';
  EI_IniString: string = 'EI';

  LogString_SolveKG21_Except: string = 'SolveKG21: EZeroDivide;';
  LogString_W1: string = ' W1';
  LogString_W2: string = ' W2';
  LogString_W3: string = ' W3';
  LogString_D: string = ' D';
  LogString_AreNull: string = ' sind Null!';

  LogString_ZeroDivideAlpha: string = 'FanOut: EZeroDivide; cos(alpha?) = 0';

  {from RggUnit3}
  EA_IniString: string = 'EA';
  Status_String_Rigg: string = '  Rigg:';
  Status_String_RiggLetzteRechnungOK: string = '    Letzte Rechnung O.K.';
  Status_String_RiggNichtEntspannbar: string = '    Nicht entspannbar';
  Status_String_RiggWanteAufDruck: string = '    Wante auf Druck';
  Status_String_RiggForceTooBig: string = '    Force too big';

  LogList_String_WanteAufDruck: string = 'TRiggFS.Kraefte: Wante auf Druck';
  LogList_String_LengthDeviation: string = 'Rigg.Split: Längenabweichung';
  LogList_Format_String_BetragTooBig: string = 'TRiggFS.Split: Betrag rF[%d] > 32000 N';
  LogList_Format_String_ProbeOfPoint: string = 'TRiggFS.Probe: Probe %s = %6.2f';

  LogList_String_ProbeFalsch: string = 'TRigg.Probe: Probe falsch';
  LogList_String_ProbeOK: string = 'TRigg.Probe: Probe O.K.';

  LogList_String_MakeRumpfKoordExcept: string = 'TRiggFS.MakeRumpfKoord:  ';
  LogList_Format_String_MakeKoord: string = 'TRiggFS.MakeKoord, %d. Aufruf: %s';
  LogList_String_MakeKoordExept: string = 'TRiggFS.MakeKoord:  ';

  LogList_FormatString_MakeKoordDS: string = 'TRiggFS.MakeKoordDS/1: %s';
  LogList_String_MakeKoordDSExept: string = 'TRiggFS.MakeKoordDS:  ';

  LogList_Format_String_MakeKoordOS: string = 'TRiggFS.MakeKoordOS, %d. Aufruf: %s';
  LogList_String_MakeKoordExeptOS: string = 'TRiggFS.MakeKoordOS:  ';

  { from RggUnit4 }
  RGI_File_Extension: string = '.rgi';
  RGG_File_Extension: string = '.rgg';

  { from Chart }
  NoParamString: string = 'kein Parameter';

  ControllerString: string = 'Controller';
  WinkelString: string = 'Winkel';
  VorstagString: string = 'Vorstag';
  WanteString: string = 'Wante';
  WanteObenString: string = 'WanteOben';

  SalingAString: string = 'Saling Abstand';
  SalingHString: string = 'Saling Höhe';
  SalingLString: string = 'Saling Länge';
  SalingWString: string = 'Saling Winkel';

  MastFootD0XString: string = 'Mastfuß D0x';

  VorstagOhneSalingString: string = 'Vorstag OS';
  WantenkraftOhneSalingString: string = 'Wantenkraft OS';

  SalingHTooSmallString: string = ' Salinghöhe zu klein!';

  MastfallF0CString: string = 'Mastfall F0C';
  MastfallF0FString: string = 'Mastfall F0F';
  VorstagSpannungString: string = 'Vorstag-Spannung';
  WantenSpannungString: string = 'Wanten-Spannung';
  DurchbiegungHDString: string = 'Durchbiegung hd';
  ElasticityPointCString: string = 'Elastizität Punkt C';

  AnfangsZustandString: string = 'Diagramm befindet sich im Anfangszustand.';
  ResetMessageString: string = 'Diagramm wurde in den Anfangszustand versetzt.';

  BlueString: string = 'Blau';
  RedString: string = 'Rot';
  GreenString: string = 'Grün';
  WhiteString: string = 'Weiß';
  YellowString: string = 'Gelb';

  ProgressCaptionString: string = 'Kurve wird berechnet';
  ProgressCaptionFormatString: string = 'Parameter %d von %d';

  TopTitleString = 'RiggChart';

  SalingFestString: string = 'fest';
  SalingDrehbarString: string = 'drehbar';
  OhneSalingString: string = 'ohne Saling';
  OhneSalingStarrString: string = 'ohne Saling (BK)';

  TopTitleTestString: string = 'Top Title';
  BottomTitleTestString: string = 'Bottom Title';
  ParamTitleTestString: string = 'Param Title';

  StatusResetString: string = 'Diagramm wurde zurückgesetzt';
  StatusNotComputedString: string = 'Kurve wurde nicht berechnet!';
  StatusNotLoadedString: string = 'Kurve wurde nicht geladen!';

  AllCurvesNormalizedString: string = 'Alle Kurven normiert [%]';

  YMinString: string = 'YMin';
  YMaxString: string = 'YMax';

  ControllerText: string = 'Zustellung Mast-Controller [mm]';
  WinkelText: string = 'Winkel [1E-1 Grad]';
  VorstagText: string = 'Vorstaglänge [mm]';
  WanteText: string = 'Wantenlänge [mm]';
  WanteObenText: string = 'Länge des oberen Wantenabschnitts [mm]';
  SalingHText: string = 'Höhe des Salingdreiecks [mm]';
  SalingAText: string = 'Saling-Abstand [mm]';
  SalingLText: string = 'Saling-Länge [mm]';
  SalingWText: string = 'Saling-Winkel [Grad]';

  XMinEditString: string = 'XMinEdit';
  XMaxEditString: string = 'XMaxEdit';

  PMinEditString: string = 'PMinEdit';
  PMaxEditString: string = 'PMaxEdit';

  YMinEditString: string = 'YMinEdit';
  YMaxEditString: string = 'YMaxEdit';

  PIdentString: string = 'Nr.';

implementation

end.
