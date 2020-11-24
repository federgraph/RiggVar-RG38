unit RiggVar.FB.ActionConst;

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

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  TFederAction = Integer;

// --- generated code snippet ---
// Note that some of the defined actions
//   may not be implemented in this version of the app.

const

{ EmptyAction }
faNoop = 0;

{ Pages }
faActionPageM = 1;
faActionPageP = 2;
faActionPageE = 3;
faActionPageS = 4;
faActionPageX = 5;
faActionPage1 = 6;
faActionPage2 = 7;
faActionPage3 = 8;
faActionPage4 = 9;
faActionPage5 = 10;
faActionPage6 = 11;

{ Forms }
faRotaForm1 = 12;
faRotaForm2 = 13;
faRotaForm3 = 14;
faShowMemo = 15;
faShowActions = 16;
faShowOptions = 17;
faShowDrawings = 18;
faShowConfig = 19;
faShowKreis = 20;
faShowInfo = 21;
faShowSplash = 22;
faShowForce = 23;
faShowTabelle = 24;
faShowDetail = 25;
faShowSaling = 26;
faShowController = 27;
faShowText = 28;
faShowTrimmTab = 29;
faShowChart = 30;
faShowDiagA = 31;
faShowDiagC = 32;
faShowDiagE = 33;
faShowDiagQ = 34;

{ TouchLayout }
faTouchTablet = 35;
faTouchPhone = 36;
faTouchDesk = 37;

{ Wheel }
faPlusOne = 38;
faPlusTen = 39;
faWheelLeft = 40;
faWheelRight = 41;
faWheelDown = 42;
faWheelUp = 43;
faParamValuePlus1 = 44;
faParamValueMinus1 = 45;
faParamValuePlus10 = 46;
faParamValueMinus10 = 47;

{ ColorScheme }
faCycleColorSchemeM = 48;
faCycleColorSchemeP = 49;

{ FederText }
faToggleAllText = 50;
faToggleTouchFrame = 51;

{ ViewParams }
faPan = 52;
faParamORX = 53;
faParamORY = 54;
faParamORZ = 55;
faParamRX = 56;
faParamRY = 57;
faParamRZ = 58;
faParamCZ = 59;

{ RggControls }
faController = 60;
faWinkel = 61;
faVorstag = 62;
faWante = 63;
faWoben = 64;
faSalingH = 65;
faSalingA = 66;
faSalingL = 67;
faSalingW = 68;
faMastfallF0C = 69;
faMastfallF0F = 70;
faMastfallVorlauf = 71;
faBiegung = 72;
faMastfussD0X = 73;
faVorstagOS = 74;
faWPowerOS = 75;
faParamAPW = 76;
faParamEAH = 77;
faParamEAR = 78;
faParamEI = 79;

{ RggFixPoints }
faFixpointA0 = 80;
faFixpointA = 81;
faFixpointB0 = 82;
faFixpointB = 83;
faFixpointC0 = 84;
faFixpointC = 85;
faFixpointD0 = 86;
faFixpointD = 87;
faFixpointE0 = 88;
faFixpointE = 89;
faFixpointF0 = 90;
faFixpointF = 91;

{ RggViewPoint }
faViewpointS = 92;
faViewpointA = 93;
faViewpointT = 94;
faViewpoint3 = 95;

{ RggSalingType }
faSalingTypOhne = 96;
faSalingTypDrehbar = 97;
faSalingTypFest = 98;
faSalingTypOhneStarr = 99;

{ RggCalcType }
faCalcTypQuer = 100;
faCalcTypKnick = 101;
faCalcTypGemessen = 102;

{ RggAppMode }
faDemo = 103;
faMemoryBtn = 104;
faMemoryRecallBtn = 105;
faKorrigiertItem = 106;
faSofortBtn = 107;
faGrauBtn = 108;
faBlauBtn = 109;
faMultiBtn = 110;

{ RggSuper }
faSuperSimple = 111;
faSuperNormal = 112;
faSuperGrau = 113;
faSuperBlau = 114;
faSuperMulti = 115;
faSuperDisplay = 116;
faSuperQuick = 117;

{ RggReport }
faReportNone = 118;
faReportLog = 119;
faReportJson = 120;
faReportData = 121;
faReportShort = 122;
faReportLong = 123;
faReportTrimmText = 124;
faReportJsonText = 125;
faReportDataText = 126;
faReportDiffText = 127;
faReportAusgabeDetail = 128;
faReportAusgabeRL = 129;
faReportAusgabeRP = 130;
faReportAusgabeRLE = 131;
faReportAusgabeRPE = 132;
faReportAusgabeDiffL = 133;
faReportAusgabeDiffP = 134;
faReportXML = 135;
faReportDebugReport = 136;
faReportReadme = 137;

{ RggChart }
faChartRect = 138;
faChartTextRect = 139;
faChartLegend = 140;
faChartAP = 141;
faChartBP = 142;
faChartGroup = 143;
faParamCountPlus = 144;
faParamCountMinus = 145;
faPComboPlus = 146;
faPComboMinus = 147;
faXComboPlus = 148;
faXComboMinus = 149;
faYComboPlus = 150;
faYComboMinus = 151;
faChartReset = 152;

{ RggGraph }
faToggleLineColor = 153;
faToggleUseDisplayList = 154;
faToggleUseQuickSort = 155;
faToggleShowLegend = 156;
faRggBogen = 157;
faRggKoppel = 158;
faRggHull = 159;
faRggZoomIn = 160;
faRggZoomOut = 161;
faToggleSalingGraph = 162;
faToggleControllerGraph = 163;
faToggleChartGraph = 164;
faToggleKraftGraph = 165;
faToggleMatrixText = 166;

{ RggSegment }
faToggleSegmentF = 167;
faToggleSegmentR = 168;
faToggleSegmentS = 169;
faToggleSegmentM = 170;
faToggleSegmentV = 171;
faToggleSegmentW = 172;
faToggleSegmentC = 173;
faToggleSegmentA = 174;

{ RggRenderOptions }
faWantRenderH = 175;
faWantRenderP = 176;
faWantRenderF = 177;
faWantRenderE = 178;
faWantRenderS = 179;

{ RggTrimms }
faTrimm0 = 180;
faTrimm1 = 181;
faTrimm2 = 182;
faTrimm3 = 183;
faTrimm4 = 184;
faTrimm5 = 185;
faTrimm6 = 186;
fa420 = 187;
faLogo = 188;

{ RggTrimmFile }
faCopyTrimmItem = 189;
faPasteTrimmItem = 190;
faCopyAndPaste = 191;
faUpdateTrimm0 = 192;
faReadTrimmFile = 193;
faSaveTrimmFile = 194;
faCopyTrimmFile = 195;

{ RggTrimmText }
faToggleTrimmText = 196;
faToggleDiffText = 197;
faToggleDataText = 198;
faToggleDebugText = 199;
faUpdateReportText = 200;

{ RggSonstiges }
faToggleHelp = 201;
faToggleReport = 202;
faToggleButtonReport = 203;
faToggleFontColor = 204;
faToggleSandboxed = 205;
faToggleSpeedPanel = 206;
faToggleAllProps = 207;
faToggleAllTags = 208;

{ RggInfo }
faShowHelpText = 209;
faShowInfoText = 210;
faShowNormalKeyInfo = 211;
faShowSpecialKeyInfo = 212;
faShowDebugInfo = 213;
faShowZOrderInfo = 214;

{ BtnLegendTablet }
faTL01 = 215;
faTL02 = 216;
faTL03 = 217;
faTL04 = 218;
faTL05 = 219;
faTL06 = 220;
faTR01 = 221;
faTR02 = 222;
faTR03 = 223;
faTR04 = 224;
faTR05 = 225;
faTR06 = 226;
faTR07 = 227;
faTR08 = 228;
faBL01 = 229;
faBL02 = 230;
faBL03 = 231;
faBL04 = 232;
faBL05 = 233;
faBL06 = 234;
faBL07 = 235;
faBL08 = 236;
faBR01 = 237;
faBR02 = 238;
faBR03 = 239;
faBR04 = 240;
faBR05 = 241;
faBR06 = 242;

{ BtnLegendPhone }
faMB01 = 243;
faMB02 = 244;
faMB03 = 245;
faMB04 = 246;
faMB05 = 247;
faMB06 = 248;
faMB07 = 249;
faMB08 = 250;

{ Circles }
faCirclesSelectC0 = 251;
faCirclesSelectC1 = 252;
faCirclesSelectC2 = 253;
faCircleParamR1 = 254;
faCircleParamR2 = 255;
faCircleParamM1X = 256;
faCircleParamM1Y = 257;
faCircleParamM2X = 258;
faCircleParamM2Y = 259;
faLineParamA1 = 260;
faLineParamA2 = 261;
faLineParamE1 = 262;
faLineParamE2 = 263;
faCircleParamM1Z = 264;
faCircleParamM2Z = 265;
faCirclesReset = 266;

{ MemeFormat }
faMemeGotoLandscape = 267;
faMemeGotoSquare = 268;
faMemeGotoPortrait = 269;
faMemeFormat0 = 270;
faMemeFormat1 = 271;
faMemeFormat2 = 272;
faMemeFormat3 = 273;
faMemeFormat4 = 274;
faMemeFormat5 = 275;
faMemeFormat6 = 276;
faMemeFormat7 = 277;
faMemeFormat8 = 278;
faMemeFormat9 = 279;

{ Reset }
faReset = 280;
faResetPosition = 281;
faResetRotation = 282;
faResetZoom = 283;

{ DropTarget }
faToggleDropTarget = 284;

{ Language }
faToggleLanguage = 285;

{ CopyPaste }
faSave = 286;
faLoad = 287;
faOpen = 288;
faCopy = 289;
faPaste = 290;
faShare = 291;

{ ViewType }
faToggleViewType = 292;
faViewTypeOrtho = 293;
faViewTypePerspective = 294;

{ ViewOptions }
faToggleMoveMode = 295;
faLinearMove = 296;
faExpoMove = 297;

{ HullMesh }
faHullMesh = 298;
faHullMeshOn = 299;
faHullMeshOff = 300;

faTouchBarTop = 301;
faTouchBarBottom = 302;
faTouchBarLeft = 303;
faTouchBarRight = 304;

faMax = 305;

ParamsRange = [
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
