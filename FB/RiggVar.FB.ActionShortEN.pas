unit RiggVar.FB.ActionShortEN;

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
  RiggVar.FB.ActionConst;

function GetFederActionShortEN(fa: TFederAction): string;

implementation

function GetFederActionShortEN(fa: TFederAction): string;
begin
  result := '??';
  case fa of

    { EmptyAction }
    faNoop: result := '';

    { Pages }
    faActionPageM: result := 'P-';
    faActionPageP: result := 'P+';
    faActionPageE: result := 'PE';
    faActionPageS: result := 'PS';
    faActionPageX: result := 'LP';
    faActionPage1: result := 'HP';
    faActionPage2: result := 'SP';
    faActionPage3: result := 'ap3';
    faActionPage4: result := 'ap4';
    faActionPage5: result := 'ap5';
    faActionPage6: result := 'ap6';

    { Forms }
     faRotaForm1: result := 'RF1';
    faRotaForm2: result := 'RF2';
    faRotaForm3: result := 'RF3';
    faShowMemo: result := 'FM';
    faShowActions: result := 'FA';
    faShowOptions: result := 'FO';
    faShowDrawings: result := 'FD';
    faShowConfig: result := 'FC';
    faShowKreis: result := 'FK';
    faShowInfo: result := 'FI';
    faShowSplash: result := 'FS';
    faShowForce: result := 'sF';
    faShowTabelle: result := 'sT';
    faShowDetail: result := 'sD';
    faShowSaling: result := 'sS';
    faShowController: result := 'sC';
    faShowText: result := 'TA';
    faShowTrimmTab: result := 'TT';
    faShowChart: result := 'CF';
    faShowDiagA: result := 'sDA';
    faShowDiagC: result := 'sDC';
    faShowDiagE: result := 'sDE';
    faShowDiagQ: result := 'sDQ';

    { TouchLayout }
    faTouchTablet: result := 'tab';
    faTouchPhone: result := 'pho';
    faTouchDesk: result := 'dsk';

    { Wheel }
     faParamValuePlus1: result := '+1';
    faParamValueMinus1: result := '-1';
    faParamValuePlus10: result := '+10';
    faParamValueMinus10: result := '-10';
    faWheelLeft: result := 'wl';
    faWheelRight: result := 'wr';
    faWheelDown: result := 'wd';
    faWheelUp: result := 'wu';

    { ColorScheme }
    faCycleColorSchemeM: result := 'c-';
    faCycleColorSchemeP: result := 'c+';

    { FederText }
    faToggleAllText: result := 'tat';
    faToggleTouchFrame: result := 'fra';

    { ViewParams }
    faPan: result := 'pan';
    faParamORX: result := 'orx';
    faParamORY: result := 'ory';
    faParamORZ: result := 'orz';
    faParamRX: result := 'rx';
    faParamRY: result := 'ry';
    faParamRZ: result := 'rz';
    faParamCZ: result := 'cz';

    { Help }
    faToggleHelp: result := 'th';
    faToggleReport: result := 'tr';
    faToggleButtonReport: result := 'bfr';

    { RggControls }
    faController: result := 'Co';
    faWinkel: result := 'Wi';
    faVorstag: result := 'Vo';
    faWante: result := 'Wa';
    faWoben: result := 'Wo';
    faSalingH: result := 'SH';
    faSalingA: result := 'SA';
    faSalingL: result := 'SL';
    faSalingW: result := 'SW';
    faMastfallF0C: result := 'F0C';
    faMastfallF0F: result := 'F0F';
    faMastfallVorlauf: result := 'MV';
    faBiegung: result := 'Bie';
    faMastfussD0X: result := 'D0X';
    faVorstagOS: result := 'vos';
    faWPowerOS: result := 'wos';
    faParamAPW: result := 'apw';
    faParamEAH: result := 'EAH';
    faParamEAR: result := 'EAR';
    faParamEI: result := 'EI';

    { RggFixPoints }
    faFixpointA0: result := 'oA0';
    faFixpointA: result := 'oA';
    faFixpointB0: result := 'oB0';
    faFixpointB: result := 'oB';
    faFixpointC0: result := 'oC0';
    faFixpointC: result := 'oC';
    faFixpointD0: result := 'oD0';
    faFixpointD: result := 'oD';
    faFixpointE0: result := 'oE0';
    faFixpointE: result := 'oE';
    faFixpointF0: result := 'oF0';
    faFixpointF: result := 'oF';

    { RggViewPoint }
    faViewpointS: result := 'vpS';
    faViewpointA: result := 'vpA';
    faViewpointT: result := 'vpT';
    faViewpoint3: result := 'vp3';

    { RggSalingType }
    faSalingTypFest: result := 'fs';
    faSalingTypDrehbar: result := 'ds';
    faSalingTypOhne: result := 'os';
    faSalingTypOhneStarr: result := 'oss';

    { RggCalcType }
    faCalcTypQuer: result := 'cQ';
    faCalcTypKnick: result := 'cK';
    faCalcTypGemessen: result := 'cM';

    { RggAppMode }
    faDemo: result := 'mod';
    faMemoryBtn: result := 'M';
    faMemoryRecallBtn: result := 'MR';
    faKorrigiertItem: result := 'KI';
    faSofortBtn: result := 'SB';
    faGrauBtn: result := 'GB';
    faBlauBtn: result := 'BB';
    faMultiBtn: result := 'MB';

    { RggSuper }
    faSuperSimple: result := 'gS';
    faSuperNormal: result := 'gN';
    faSuperGrau: result := 'gG';
    faSuperBlau: result := 'gB';
    faSuperMulti: result := 'gM';
    faSuperDisplay: result := 'gD';
    faSuperQuick: result := 'gQ';

    { RggReport }
    faReportNone: result := '~N';
    faReportLog: result := '~L';
    faReportJson: result := '~J';
    faReportData: result := '~D';
    faReportShort: result := '~SI';
    faReportLong: result := '~LI';
    faReportTrimmText: result := '~TT';
    faReportJsonText: result := '~JT';
    faReportDataText: result := '~DT';
    faReportDiffText: result := '~dt';
    faReportAusgabeDetail: result := 'RD';
    faReportAusgabeRL: result := 'RL';
    faReportAusgabeRP: result := 'RP';
    faReportAusgabeRLE: result := 'RLE';
    faReportAusgabeRPE: result := 'RPE';
    faReportAusgabeDiffL: result := 'RDL';
    faReportAusgabeDiffP: result := 'RDP';
    faReportXML: result := '~X';
    faReportDebugReport: result := '~';
    faReportReadme: result := '~R';

    { RggChart }
    faChartRect: result := 'c[]';
    faChartTextRect: result := 'cT';
    faChartLegend: result := 'cL';
    faChartAP: result := 'cA';
    faChartBP: result := 'cB';
    faChartGroup: result := 'cG';
    faParamCountPlus: result := 'pC+';
    faParamCountMinus: result := 'pC-';
    faPComboPlus: result := 'cP+';
    faPComboMinus: result := 'cP-';
    faXComboPlus: result := 'cX+';
    faXComboMinus: result := 'cX-';
    faYComboPlus: result := 'cY+';
    faYComboMinus: result := 'cY-';
    faChartReset: result := 'cR';

    { RggGraph }
    faToggleLineColor: result := 'LC';
    faToggleUseDisplayList: result := 'DL';
    faToggleUseQuickSort: result := 'QS';
    faToggleShowLegend: result := 'LG';
    faToggleSortedRota: result := 'S';
    faRggBogen: result := 'B';
    faRggKoppel: result := 'Ko';
    faRggHull: result := 'HL';
    faRggZoomIn: result := 'Z+';
    faRggZoomOut: result := 'Z-';
    faToggleSalingGraph: result := 'SG';
    faToggleControllerGraph: result := 'CG';
    faToggleChartGraph: result := 'DG';
    faToggleKraftGraph: result := 'KG';
    faToggleMatrixText: result := 'MT';

    { RggSegment }
    faToggleSegmentF: result := '-F';
    faToggleSegmentR: result := '-R';
    faToggleSegmentS: result := '-S';
    faToggleSegmentM: result := '-M';
    faToggleSegmentV: result := '-V';
    faToggleSegmentW: result := '-W';
    faToggleSegmentC: result := '-C';
    faToggleSegmentA: result := '-A';

    { RggRenderOptions }
    faWantRenderH: result := 'rH';
    faWantRenderP: result := 'rP';
    faWantRenderF: result := 'rF';
    faWantRenderE: result := 'rE';
    faWantRenderS: result := 'rS';

    { RggTrimms }
    faTrimm0: result := 'T0';
    faTrimm1: result := 'T1';
    faTrimm2: result := 'T2';
    faTrimm3: result := 'T3';
    faTrimm4: result := 'T4';
    faTrimm5: result := 'T5';
    faTrimm6: result := 'T6';
    fa420: result := '420';
    faLogo: result := 'logo';

    { RggTrimmFile }
    faCopyTrimmItem: result := 'cti';
    faPasteTrimmItem: result := 'pti';
    faCopyAndPaste: result := 'cap';
    faUpdateTrimm0: result := 'ct0';
    faReadTrimmFile: result := 'rtf';
    faSaveTrimmFile: result := 'stf';
    faCopyTrimmFile: result := 'ctf';

    { RggTrimmText }
    faToggleTrimmText: result := 'trim';
    faToggleDiffText: result := 'diff';
    faToggleDataText: result := 'data';
    faToggleDebugText: result := 'log';
    faUpdateReportText: result := 'rpt';

    { RggSonstiges }
    faToggleDarkMode: result := 'DM';
    faToggleButtonSize: result := 'BS';
    faToggleSandboxed: result := 'SX';
    faToggleSpeedPanel: result := 'tsp';
    faToggleAllProps: result := 'AP';
    faToggleAllTags: result := 'AT';

    { RggInfo }
    faShowHelpText: result := 'sh';
    faShowInfoText: result := 'si';
    faShowNormalKeyInfo: result := 'nki';
    faShowSpecialKeyInfo: result := 'ski';
    faShowDebugInfo: result := 'sdi';
    faShowZOrderInfo: result := 'zoi';

    { BtnLegendTablet }
    faTL01: result := '#1';
    faTL02: result := '#2';
    faTL03: result := '#3';
    faTL04: result := '#4';
    faTL05: result := '#5';
    faTL06: result := '#6';
    faTR01: result := '1#';
    faTR02: result := '2#';
    faTR03: result := '3#';
    faTR04: result := '4#';
    faTR05: result := '5#';
    faTR06: result := '6#';
    faTR07: result := '7#';
    faTR08: result := '8#';
    faBL01: result := '1*';
    faBL02: result := '2*';
    faBL03: result := '3*';
    faBL04: result := '4*';
    faBL05: result := '5*';
    faBL06: result := '6*';
    faBL07: result := '7*';
    faBL08: result := '8*';
    faBR01: result := '*1';
    faBR02: result := '*2';
    faBR03: result := '*3';
    faBR04: result := '*4';
    faBR05: result := '*5';
    faBR06: result := '*6';

    { BtnLegendPhone }
    faMB01: result := '_1';
    faMB02: result := '_2';
    faMB03: result := '_3';
    faMB04: result := '_4';
    faMB05: result := '_5';
    faMB06: result := '_6';
    faMB07: result := '_7';
    faMB08: result := '_8';

    { TouchBarLegend }
    faTouchBarTop: result := 'tbT';
    faTouchBarBottom: result := 'tbB';
    faTouchBarLeft: result := 'tbL';
    faTouchBarRight: result := 'tbR';

    { Circles }
    faCirclesSelectC0: result := 'C0';
    faCirclesSelectC1: result := 'C1';
    faCirclesSelectC2: result := 'C2';
    faCircleParamR1: result := 'R1';
    faCircleParamR2: result := 'R2';
    faCircleParamM1X: result := '1.X';
    faCircleParamM1Y: result := '1.Y';
    faCircleParamM2X: result := '2.X';
    faCircleParamM2Y: result := '2.Y';
    faLineParamA1: result := 'A1';
    faLineParamA2: result := 'A2';
    faLineParamE1: result := 'E1';
    faLineParamE2: result := 'E2';
    faCircleParamM1Z: result := '1.Z';
    faCircleParamM2Z: result := '2.Z';
    faCirclesReset: result := 'R';

    { MemeFormat }
    faMemeGotoLandscape: result := '[L]';
    faMemeGotoSquare: result := '[S]';
    faMemeGotoPortrait: result := '[P]';
    faMemeFormat0: result := '[0]';
    faMemeFormat1: result := '[1]';
    faMemeFormat2: result := '[2]';
    faMemeFormat3: result := '[3]';
    faMemeFormat4: result := '[4]';
    faMemeFormat5: result := '[5]';
    faMemeFormat6: result := '[6]';
    faMemeFormat7: result := '[7]';
    faMemeFormat8: result := '[8]';
    faMemeFormat9: result := '[9]';

    { Reset }
    faReset: result := 'res';
    faResetPosition: result := 'rpo';
    faResetRotation: result := 'rro';
    faResetZoom: result := 'rzo';

    { Language }
    faToggleLanguage: result := 'lan';

    { CopyPaste }
    faSave: result := 'sav';
    faLoad: result := 'loa';
    faOpen: result := 'ope';
    faCopy: result := '^c';
    faPaste: result := '^v';
    faShare: result := 'sha';

  end;
end;

end.
