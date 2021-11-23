unit RiggVar.FB.ActionGroup;

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

type
  TActionGroup = array of Integer;

const

ActionGroupEmptyAction: TActionGroup = [
  faNoop];

ActionGroupTouchLayout: TActionGroup = [
  faTouchTablet,
  faTouchPhone,
  faTouchDesk];

ActionGroupPages: TActionGroup = [
  faActionPageM,
  faActionPageP,
  faActionPageE,
  faActionPageS,
  faActionPageX,
  faActionPage1,
  faActionPage2,
  faActionPage3,
  faActionPage4,
  faActionPage5,
  faActionPage6];

ActionGroupColorScheme: TActionGroup = [
  faCycleColorSchemeM,
  faCycleColorSchemeP];

ActionGroupWheel: TActionGroup = [
  faPlusOne,
  faPlusTen,
  faWheelLeft,
  faWheelRight,
  faWheelDown,
  faWheelUp,
  faParamValuePlus1,
  faParamValueMinus1,
  faParamValuePlus10,
  faParamValueMinus10];

ActionGroupForms: TActionGroup = [
  faRotaForm1,
  faRotaForm2,
  faRotaForm3,
  faShowMemo,
  faShowActions,
  faShowOptions,
  faShowDrawings,
  faShowConfig,
  faShowKreis,
  faShowInfo,
  faShowSplash,
  faShowForce,
  faShowTabelle,
  faShowDetail,
  faShowSaling,
  faShowController,
  faShowText,
  faShowTrimmTab,
  faShowChart,
  faShowDiagA,
  faShowDiagC,
  faShowDiagE,
  faShowDiagQ];

ActionGroupViewParams: TActionGroup = [
  faPan,
  faParamORX,
  faParamORY,
  faParamORZ,
  faParamRX,
  faParamRY,
  faParamRZ,
  faParamCZ];

ActionGroupFederText: TActionGroup = [
  faToggleAllText,
  faToggleTouchFrame];

ActionGroupRggControls: TActionGroup = [
  faController,
  faWinkel,
  faVorstag,
  faWante,
  faWoben,
  faSalingH,
  faSalingA,
  faSalingL,
  faSalingW,
  faMastfallF0C,
  faMastfallF0F,
  faMastfallVorlauf,
  faBiegung,
  faMastfussD0X,
  faVorstagOS,
  faWPowerOS,
  faParamAPW,
  faParamEAH,
  faParamEAR,
  faParamEI];

ActionGroupRggFixPoints: TActionGroup = [
  faFixpointA0,
  faFixpointA,
  faFixpointB0,
  faFixpointB,
  faFixpointC0,
  faFixpointC,
  faFixpointD0,
  faFixpointD,
  faFixpointE0,
  faFixpointE,
  faFixpointF0,
  faFixpointF];

ActionGroupRggTrimms: TActionGroup = [
  faTrimm0,
  faTrimm1,
  faTrimm2,
  faTrimm3,
  faTrimm4,
  faTrimm5,
  faTrimm6,
  fa420,
  faLogo];

ActionGroupRggSalingType: TActionGroup = [
  faSalingTypOhne,
  faSalingTypDrehbar,
  faSalingTypFest,
  faSalingTypOhneStarr];

ActionGroupRggCalcType: TActionGroup = [
  faCalcTypQuer,
  faCalcTypKnick,
  faCalcTypGemessen];

ActionGroupRggAppMode: TActionGroup = [
  faDemo,
  faMemoryBtn,
  faMemoryRecallBtn,
  faKorrigiertItem,
  faSofortBtn,
  faGrauBtn,
  faBlauBtn,
  faMultiBtn];

ActionGroupRggSuper: TActionGroup = [
  faSuperSimple,
  faSuperNormal,
  faSuperGrau,
  faSuperBlau,
  faSuperMulti,
  faSuperDisplay,
  faSuperQuick];

ActionGroupRggTrimmFile: TActionGroup = [
  faCopyTrimmItem,
  faPasteTrimmItem,
  faCopyAndPaste,
  faUpdateTrimm0,
  faReadTrimmFile,
  faSaveTrimmFile,
  faCopyTrimmFile];

ActionGroupRggTrimmText: TActionGroup = [
  faToggleTrimmText,
  faToggleDiffText,
  faToggleDataText,
  faToggleDebugText,
  faUpdateReportText];

ActionGroupRggViewPoint: TActionGroup = [
  faViewpointS,
  faViewpointA,
  faViewpointT,
  faViewpoint3];

ActionGroupRggRenderOptions: TActionGroup = [
  faWantRenderH,
  faWantRenderP,
  faWantRenderF,
  faWantRenderE,
  faWantRenderS];

ActionGroupRggChart: TActionGroup = [
  faChartRect,
  faChartTextRect,
  faChartLegend,
  faChartAP,
  faChartBP,
  faChartGroup,
  faParamCountPlus,
  faParamCountMinus,
  faPComboPlus,
  faPComboMinus,
  faXComboPlus,
  faXComboMinus,
  faYComboPlus,
  faYComboMinus,
  faChartReset];

ActionGroupRggReport: TActionGroup = [
  faReportNone,
  faReportLog,
  faReportJson,
  faReportData,
  faReportShort,
  faReportLong,
  faReportTrimmText,
  faReportJsonText,
  faReportDataText,
  faReportDiffText,
  faReportAusgabeDetail,
  faReportAusgabeRL,
  faReportAusgabeRP,
  faReportAusgabeRLE,
  faReportAusgabeRPE,
  faReportAusgabeDiffL,
  faReportAusgabeDiffP,
  faReportXML,
  faReportDebugReport,
  faReportReadme];

ActionGroupRggSegment: TActionGroup = [
  faToggleSegmentF,
  faToggleSegmentR,
  faToggleSegmentS,
  faToggleSegmentM,
  faToggleSegmentV,
  faToggleSegmentW,
  faToggleSegmentC,
  faToggleSegmentA];

ActionGroupRggGraph: TActionGroup = [
  faToggleLineColor,
  faToggleUseDisplayList,
  faToggleUseQuickSort,
  faToggleShowLegend,
  faToggleSortedRota,
  faRggBogen,
  faRggKoppel,
  faRggHull,
  faRggZoomIn,
  faRggZoomOut,
  faToggleSalingGraph,
  faToggleControllerGraph,
  faToggleChartGraph,
  faToggleKraftGraph,
  faToggleMatrixText];

ActionGroupMemeFormat: TActionGroup = [
  faMemeGotoLandscape,
  faMemeGotoSquare,
  faMemeGotoPortrait,
  faMemeFormat0,
  faMemeFormat1,
  faMemeFormat2,
  faMemeFormat3,
  faMemeFormat4,
  faMemeFormat5,
  faMemeFormat6,
  faMemeFormat7,
  faMemeFormat8,
  faMemeFormat9];

ActionGroupRggInfo: TActionGroup = [
  faShowHelpText,
  faShowInfoText,
  faShowNormalKeyInfo,
  faShowSpecialKeyInfo,
  faShowDebugInfo,
  faShowZOrderInfo];

ActionGroupRggSonstiges: TActionGroup = [
  faToggleHelp,
  faToggleReport,
  faToggleButtonReport,
  faToggleDarkMode,
  faToggleButtonSize,
  faToggleSandboxed,
  faToggleSpeedPanel,
  faToggleAllProps,
  faToggleAllTags];

ActionGroupBtnLegendTablet: TActionGroup = [
  faTL01,
  faTL02,
  faTL03,
  faTL04,
  faTL05,
  faTL06,

  faTR01,
  faTR02,
  faTR03,
  faTR04,
  faTR05,
  faTR06,
  faTR07,
  faTR08,

  faBL01,
  faBL02,
  faBL03,
  faBL04,
  faBL05,
  faBL06,
  faBL07,
  faBL08,

  faBR01,
  faBR02,
  faBR03,
  faBR04,
  faBR05,
  faBR06];

ActionGroupBtnLegendPhone: TActionGroup = [
  faMB01,
  faMB02,
  faMB03,
  faMB04,
  faMB05,
  faMB06,
  faMB07,
  faMB08];

ActionGroupCircles: TActionGroup = [
  faCirclesSelectC0,
  faCirclesSelectC1,
  faCirclesSelectC2,
  faCircleParamR1,
  faCircleParamR2,
  faCircleParamM1X,
  faCircleParamM1Y,
  faCircleParamM2X,
  faCircleParamM2Y,
  faLineParamA1,
  faLineParamA2,
  faLineParamE1,
  faLineParamE2,
  faCircleParamM1Z,
  faCircleParamM2Z,
  faCirclesReset];

ActionGroupParamT: TActionGroup = [
  faParamT1,
  faParamT2,
  faParamT3,
  faParamT4];

ActionGroupReset: TActionGroup = [
  faReset,
  faResetPosition,
  faResetRotation,
  faResetZoom];

ActionGroupTouchBarLegend: TActionGroup = [
  faTouchBarTop,
  faTouchBarBottom,
  faTouchBarLeft,
  faTouchBarRight];

ActionGroupViewType: TActionGroup = [
  faToggleViewType,
  faViewTypeOrtho,
  faViewTypePerspective];

ActionGroupDropTarget: TActionGroup = [
  faToggleDropTarget];

ActionGroupLanguage: TActionGroup = [
  faToggleLanguage];

ActionGroupCopyPaste: TActionGroup = [
  faSave,
  faLoad,
  faOpen,
  faCopy,
  faPaste,
  faShare];

ActionGroupViewOptions: TActionGroup = [
  faToggleMoveMode,
  faLinearMove,
  faExpoMove];

ActionGroupHullMesh: TActionGroup = [
  faHullMesh,
  faHullMeshOn,
  faHullMeshOff];

ActionGroupBitmapCycle: TActionGroup = [
  faCycleBitmapM,
  faCycleBitmapP,
  faRandom,
  faRandomWhite,
  faRandomBlack,
  faBitmapEscape,
  faToggleContour];

implementation

end.
