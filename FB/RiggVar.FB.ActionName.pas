unit RiggVar.FB.ActionName;

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

{.$define WantAll}

uses
  RiggVar.FB.ActionConst;

function GetFederActionName(fa: TFederAction): string;

implementation

function GetFederActionName(fa: TFederAction): string;
begin
  case fa of
// --- generated code snippet ---

{ EmptyAction }
faNoop: result := 'faNoop';

{ Pages }
faActionPageM: result := 'faActionPageM';
faActionPageP: result := 'faActionPageP';
faActionPageE: result := 'faActionPageE';
faActionPageS: result := 'faActionPageS';
faActionPageX: result := 'faActionPageX';
faActionPage1: result := 'faActionPage1';
faActionPage2: result := 'faActionPage2';
faActionPage3: result := 'faActionPage3';
faActionPage4: result := 'faActionPage4';
faActionPage5: result := 'faActionPage5';
faActionPage6: result := 'faActionPage6';

{ Forms }
faShowMemo: result := 'faShowMemo';
faShowActions: result := 'faShowActions';
faShowOptions: result := 'faShowOptions';
faShowDrawings: result := 'faShowDrawings';
faShowConfig: result := 'faShowConfig';
faShowTrimmTab: result := 'faShowTrimmTab';
faShowKreis: result := 'faShowKreis';
faShowInfo: result := 'faShowInfo';
faShowSplash: result := 'faShowSplash';
faShowText: result := 'faShowText';
faShowChart: result := 'faShowChart';
faShowDiagA: result := 'faShowDiagA';
faShowDiagC: result := 'faShowDiagC';
faShowDiagE: result := 'faShowDiagE';
faShowDiagQ: result := 'faShowDiagQ';
faShowForce: result := 'faShowForce';
faShowDetail: result := 'faShowDetail';
faShowTabelle: result := 'faShowTabelle';
faShowSaling: result := 'faShowSaling';
faShowController: result := 'faShowController';

{ TouchLayout }
faTouchTablet: result := 'faTouchTablet';
faTouchPhone: result := 'faTouchPhone';
faTouchDesk: result := 'faTouchDesk';

{ Wheel }
faPlusOne: result := 'faPlusOne';
faPlusTen: result := 'faPlusTen';
faWheelLeft: result := 'faWheelLeft';
faWheelRight: result := 'faWheelRight';
faWheelDown: result := 'faWheelDown';
faWheelUp: result := 'faWheelUp';
faParamValuePlus1: result := 'faParamValuePlus1';
faParamValueMinus1: result := 'faParamValueMinus1';
faParamValuePlus10: result := 'faParamValuePlus10';
faParamValueMinus10: result := 'faParamValueMinus10';

{ ColorScheme }
faCycleColorSchemeM: result := 'faCycleColorSchemeM';
faCycleColorSchemeP: result := 'faCycleColorSchemeP';

{ FederText }
faToggleAllText: result := 'faToggleAllText';
faToggleTouchFrame: result := 'faToggleTouchFrame';

{ ViewParams }
faPan: result := 'faPan';
faParamORX: result := 'faParamORX';
faParamORY: result := 'faParamORY';
faParamORZ: result := 'faParamORZ';
faParamRX: result := 'faParamRX';
faParamRY: result := 'faParamRY';
faParamRZ: result := 'faParamRZ';
faParamCZ: result := 'faParamCZ';

{ RggControls }
faController: result := 'faController';
faWinkel: result := 'faWinkel';
faVorstag: result := 'faVorstag';
faWante: result := 'faWante';
faWoben: result := 'faWoben';
faSalingH: result := 'faSalingH';
faSalingA: result := 'faSalingA';
faSalingL: result := 'faSalingL';
faSalingW: result := 'faSalingW';
faMastfallF0C: result := 'faMastfallF0C';
faMastfallF0F: result := 'faMastfallF0F';
faMastfallVorlauf: result := 'faMastfallVorlauf';
faBiegung: result := 'faBiegung';
faMastfussD0X: result := 'faMastfussD0X';
faVorstagOS: result := 'faVorstagOS';
faWPowerOS: result := 'faWPowerOS';
faParamAPW: result := 'faParamAPW';
faParamEAH: result := 'faParamEAH';
faParamEAR: result := 'faParamEAR';
faParamEI: result := 'faParamEI';

{ RggFixPoints }
faFixpointA0: result := 'faFixpointA0';
faFixpointA: result := 'faFixpointA';
faFixpointB0: result := 'faFixpointB0';
faFixpointB: result := 'faFixpointB';
faFixpointC0: result := 'faFixpointC0';
faFixpointC: result := 'faFixpointC';
faFixpointD0: result := 'faFixpointD0';
faFixpointD: result := 'faFixpointD';
faFixpointE0: result := 'faFixpointE0';
faFixpointE: result := 'faFixpointE';
faFixpointF0: result := 'faFixpointF0';
faFixpointF: result := 'faFixpointF';

{ RggViewPoint }
faViewpointS: result := 'faViewpointS';
faViewpointA: result := 'faViewpointA';
faViewpointT: result := 'faViewpointT';
faViewpoint3: result := 'faViewpoint3';

{ RggSalingType }
faSalingTypOhne: result := 'faSalingTypOhne';
faSalingTypDrehbar: result := 'faSalingTypDrehbar';
faSalingTypFest: result := 'faSalingTypFest';
faSalingTypOhneStarr: result := 'faSalingTypOhneStarr';

{ RggCalcType }
faCalcTypQuer: result := 'faCalcTypQuer';
faCalcTypKnick: result := 'faCalcTypKnick';
faCalcTypGemessen: result := 'faCalcTypGemessen';

{ RggAppMode }
faDemo: result := 'faDemo';
faMemoryBtn: result := 'faMemoryBtn';
faMemoryRecallBtn: result := 'faMemoryRecallBtn';
faKorrigiertItem: result := 'faKorrigiertItem';
faSofortBtn: result := 'faSofortBtn';
faGrauBtn: result := 'faGrauBtn';
faBlauBtn: result := 'faBlauBtn';
faMultiBtn: result := 'faMultiBtn';

{ RggSuper }
faSuperSimple: result := 'faSuperSimple';
faSuperNormal: result := 'faSuperNormal';
faSuperGrau: result := 'faSuperGrau';
faSuperBlau: result := 'faSuperBlau';
faSuperMulti: result := 'faSuperMulti';
faSuperDisplay: result := 'faSuperDisplay';
faSuperQuick: result := 'faSuperQuick';

{ RggReport }
faReportNone: result := 'faReportNone';
faReportLog: result := 'faReportLog';
faReportJson: result := 'faReportJson';
faReportData: result := 'faReportData';
faReportShort: result := 'faReportShort';
faReportLong: result := 'faReportLong';
faReportTrimmText: result := 'faReportTrimmText';
faReportJsonText: result := 'faReportJsonText';
faReportDataText: result := 'faReportDataText';
faReportDiffText: result := 'faReportDiffText';
faReportAusgabeDetail: result := 'faReportAusgabeDetail';
faReportAusgabeRL: result := 'faReportAusgabeRL';
faReportAusgabeRP: result := 'faReportAusgabeRP';
faReportAusgabeRLE: result := 'faReportAusgabeRLE';
faReportAusgabeRPE: result := 'faReportAusgabeRPE';
faReportAusgabeDiffL: result := 'faReportAusgabeDiffL';
faReportAusgabeDiffP: result := 'faReportAusgabeDiffP';
faReportXML: result := 'faReportXML';
faReportDebugReport: result := 'faReportDebugReport';
faReportReadme: result := 'faReportReadme';

{ RggChart }
faChartRect: result := 'faChartRect';
faChartTextRect: result := 'faChartTextRect';
faChartLegend: result := 'faChartLegend';
faChartAP: result := 'faChartAP';
faChartBP: result := 'faChartBP';
faChartGroup: result := 'faChartGroup';
faParamCountPlus: result := 'faParamCountPlus';
faParamCountMinus: result := 'faParamCountMinus';
faPComboPlus: result := 'faPComboPlus';
faPComboMinus: result := 'faPComboMinus';
faXComboPlus: result := 'faXComboPlus';
faXComboMinus: result := 'faXComboMinus';
faYComboPlus: result := 'faYComboPlus';
faYComboMinus: result := 'faYComboMinus';
faChartReset: result := 'faChartReset';

{ RggGraph }
faToggleLineColor: result := 'faToggleLineColor';
faToggleUseDisplayList: result := 'faToggleUseDisplayList';
faToggleUseQuickSort: result := 'faToggleUseQuickSort';
faToggleShowLegend: result := 'faToggleShowLegend';
faRggBogen: result := 'faRggBogen';
faRggKoppel: result := 'faRggKoppel';
faRggHull: result := 'faRggHull';
faRggZoomIn: result := 'faRggZoomIn';
faRggZoomOut: result := 'faRggZoomOut';
faToggleSalingGraph: result := 'faToggleSalingGraph';
faToggleControllerGraph: result := 'faToggleControllerGraph';
faToggleChartGraph: result := 'faToggleChartGraph';
faToggleKraftGraph: result := 'faToggleKraftGraph';
faToggleMatrixText: result := 'faToggleMatrixText';

{ RggSegment }
faToggleSegmentF: result := 'faToggleSegmentF';
faToggleSegmentR: result := 'faToggleSegmentR';
faToggleSegmentS: result := 'faToggleSegmentS';
faToggleSegmentM: result := 'faToggleSegmentM';
faToggleSegmentV: result := 'faToggleSegmentV';
faToggleSegmentW: result := 'faToggleSegmentW';
faToggleSegmentC: result := 'faToggleSegmentC';
faToggleSegmentA: result := 'faToggleSegmentA';

{ RggRenderOptions }
faWantRenderH: result := 'faWantRenderH';
faWantRenderP: result := 'faWantRenderP';
faWantRenderF: result := 'faWantRenderF';
faWantRenderE: result := 'faWantRenderE';
faWantRenderS: result := 'faWantRenderS';

{ RggTrimms }
faTrimm0: result := 'faTrimm0';
faTrimm1: result := 'faTrimm1';
faTrimm2: result := 'faTrimm2';
faTrimm3: result := 'faTrimm3';
faTrimm4: result := 'faTrimm4';
faTrimm5: result := 'faTrimm5';
faTrimm6: result := 'faTrimm6';
fa420: result := 'fa420';
faLogo: result := 'faLogo';

{ RggTrimmFile }
faCopyTrimmItem: result := 'faCopyTrimmItem';
faPasteTrimmItem: result := 'faPasteTrimmItem';
faCopyAndPaste: result := 'faCopyAndPaste';
faUpdateTrimm0: result := 'faUpdateTrimm0';
faReadTrimmFile: result := 'faReadTrimmFile';
faSaveTrimmFile: result := 'faSaveTrimmFile';
faCopyTrimmFile: result := 'faCopyTrimmFile';

{ RggTrimmText }
faToggleTrimmText: result := 'faToggleTrimmText';
faToggleDiffText: result := 'faToggleDiffText';
faToggleDataText: result := 'faToggleDataText';
faToggleDebugText: result := 'faToggleDebugText';
faUpdateReportText: result := 'faUpdateReportText';

{ RggSonstiges }
faToggleHelp: result := 'faToggleHelp';
faToggleReport: result := 'faToggleReport';
faToggleButtonReport: result := 'faToggleButtonReport';
faToggleDarkMode: result := 'faToggleDarkMode';
faToggleButtonSize: result := 'faToggleButtonSize';
faToggleSandboxed: result := 'faToggleSandboxed';
faToggleSpeedPanel: result := 'faToggleSpeedPanel';
faToggleAllProps: result := 'faToggleAllProps';
faToggleAllTags: result := 'faToggleAllTags';

{ BtnLegendTablet }
faTL01: result := 'faTL01';
faTL02: result := 'faTL02';
faTL03: result := 'faTL03';
faTL04: result := 'faTL04';
faTL05: result := 'faTL05';
faTL06: result := 'faTL06';
faTR01: result := 'faTR01';
faTR02: result := 'faTR02';
faTR03: result := 'faTR03';
faTR04: result := 'faTR04';
faTR05: result := 'faTR05';
faTR06: result := 'faTR06';
faTR07: result := 'faTR07';
faTR08: result := 'faTR08';
faBL01: result := 'faBL01';
faBL02: result := 'faBL02';
faBL03: result := 'faBL03';
faBL04: result := 'faBL04';
faBL05: result := 'faBL05';
faBL06: result := 'faBL06';
faBL07: result := 'faBL07';
faBL08: result := 'faBL08';
faBR01: result := 'faBR01';
faBR02: result := 'faBR02';
faBR03: result := 'faBR03';
faBR04: result := 'faBR04';
faBR05: result := 'faBR05';
faBR06: result := 'faBR06';

{ BtnLegendPhone }
faMB01: result := 'faMB01';
faMB02: result := 'faMB02';
faMB03: result := 'faMB03';
faMB04: result := 'faMB04';
faMB05: result := 'faMB05';
faMB06: result := 'faMB06';
faMB07: result := 'faMB07';
faMB08: result := 'faMB08';

{ Circles }
faCirclesSelectC0: result := 'faCirclesSelectC0';
faCirclesSelectC1: result := 'faCirclesSelectC1';
faCirclesSelectC2: result := 'faCirclesSelectC2';
faCircleParamR1: result := 'faCircleParamR1';
faCircleParamR2: result := 'faCircleParamR2';
faCircleParamM1X: result := 'faCircleParamM1X';
faCircleParamM1Y: result := 'faCircleParamM1Y';
faCircleParamM2X: result := 'faCircleParamM2X';
faCircleParamM2Y: result := 'faCircleParamM2Y';
faLineParamA1: result := 'faLineParamA1';
faLineParamA2: result := 'faLineParamA2';
faLineParamE1: result := 'faLineParamE1';
faLineParamE2: result := 'faLineParamE2';
faCircleParamM1Z: result := 'faCircleParamM1Z';
faCircleParamM2Z: result := 'faCircleParamM2Z';
faCirclesReset: result := 'faCirclesReset';

{ MemeFormat }
faMemeGotoLandscape: result := 'faMemeGotoLandscape';
faMemeGotoSquare: result := 'faMemeGotoSquare';
faMemeGotoPortrait: result := 'faMemeGotoPortrait';
faMemeFormat0: result := 'faMemeFormat0';
faMemeFormat1: result := 'faMemeFormat1';
faMemeFormat2: result := 'faMemeFormat2';
faMemeFormat3: result := 'faMemeFormat3';
faMemeFormat4: result := 'faMemeFormat4';
faMemeFormat5: result := 'faMemeFormat5';
faMemeFormat6: result := 'faMemeFormat6';
faMemeFormat7: result := 'faMemeFormat7';
faMemeFormat8: result := 'faMemeFormat8';
faMemeFormat9: result := 'faMemeFormat9';

faRotaForm1: result := 'faRotaForm1';
faRotaForm2: result := 'faRotaForm2';
faRotaForm3: result := 'faRotaForm3';

{ Reset }
faReset: result := 'faReset';
faResetPosition: result := 'faResetPosition';
faResetRotation: result := 'faResetRotation';
faResetZoom: result := 'faResetZoom';

faShowHelpText: result := 'faShowHelpText';
faShowInfoText: result := 'faShowInfoText';
faShowNormalKeyInfo: result := 'faShowNormalKeyInfo';
faShowSpecialKeyInfo: result := 'faShowSpecialKeyInfo';
faShowZOrderInfo: result := 'faShowZOrderInfo';
faShowDebugInfo: result := 'faShowDebugInfo';

{ ParamT }
faParamT1: result := 'faParamT1';
faParamT2: result := 'faParamT2';
faParamT3: result := 'faParamT3';
faParamT4: result := 'faParamT4';

faTouchBarTop: result := 'faTouchBarTop';
faTouchBarBottom: result := 'faTouchBarBottom';
faTouchBarLeft: result := 'faTouchBarLeft';
faTouchBarRight: result := 'faTouchBarRight';

faToggleSortedRota: result := 'faToggleSortedRota';

{ ViewType }
faToggleViewType: result := 'faToggleViewType';
faViewTypeOrtho: result := 'faViewTypeOrtho';
faViewTypePerspective: result := 'faViewTypePerspective';

{$ifdef WantAll}

{ DropTarget }
faToggleDropTarget: result := 'faToggleDropTarget';

{ Language }
faToggleLanguage: result := 'faToggleLanguage';

{ CopyPaste }
faSave: result := 'faSave';
faLoad: result := 'faLoad';
faOpen: result := 'faOpen';
faCopy: result := 'faCopy';
faPaste: result := 'faPaste';
faShare: result := 'faShare';

{ ViewOptions }
faToggleMoveMode: result := 'faToggleMoveMode';
faLinearMove: result := 'faLinearMove';
faExpoMove: result := 'faExpoMove';

{ RggHullMesh }
faHullMesh: result := 'faHullMesh';
faHullMeshOn: result := 'faHullMeshOn';
faHullMeshOff: result := 'faHullMeshOff';

{ BitmapCycle }
faCycleBitmapM: result := 'faCycleBitmapM';
faCycleBitmapP: result := 'faCycleBitmapP';
faRandom: result := 'faRandom';
faRandomWhite: result := 'faRandomWhite';
faRandomBlack: result := 'faRandomBlack';
faBitmapEscape: result := 'faBitmapEscape';
faToggleContour: result := 'faToggleContour';
{$endif}

    else
      result := '??';
  end;
end;

end.
