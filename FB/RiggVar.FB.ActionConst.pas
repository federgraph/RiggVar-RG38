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

{ ParamT }
faParamT1 = 60;
faParamT2 = 61;
faParamT3 = 62;
faParamT4 = 63;

{ RggControls }
faController = 64;
faWinkel = 65;
faVorstag = 66;
faWante = 67;
faWoben = 68;
faSalingH = 69;
faSalingA = 70;
faSalingL = 71;
faSalingW = 72;
faMastfallF0C = 73;
faMastfallF0F = 74;
faMastfallVorlauf = 75;
faBiegung = 76;
faMastfussD0X = 77;
faVorstagOS = 78;
faWPowerOS = 79;
faParamAPW = 80;
faParamEAH = 81;
faParamEAR = 82;
faParamEI = 83;

{ RggFixPoints }
faFixpointA0 = 84;
faFixpointA = 85;
faFixpointB0 = 86;
faFixpointB = 87;
faFixpointC0 = 88;
faFixpointC = 89;
faFixpointD0 = 90;
faFixpointD = 91;
faFixpointE0 = 92;
faFixpointE = 93;
faFixpointF0 = 94;
faFixpointF = 95;

{ RggViewPoint }
faViewpointS = 96;
faViewpointA = 97;
faViewpointT = 98;
faViewpoint3 = 99;

{ RggSalingType }
faSalingTypOhne = 100;
faSalingTypDrehbar = 101;
faSalingTypFest = 102;
faSalingTypOhneStarr = 103;

{ RggCalcType }
faCalcTypQuer = 104;
faCalcTypKnick = 105;
faCalcTypGemessen = 106;

{ RggAppMode }
faDemo = 107;
faMemoryBtn = 108;
faMemoryRecallBtn = 109;
faKorrigiertItem = 110;
faSofortBtn = 111;
faGrauBtn = 112;
faBlauBtn = 113;
faMultiBtn = 114;

{ RggSuper }
faSuperSimple = 115;
faSuperNormal = 116;
faSuperGrau = 117;
faSuperBlau = 118;
faSuperMulti = 119;
faSuperDisplay = 120;
faSuperQuick = 121;

{ RggReport }
faReportNone = 122;
faReportLog = 123;
faReportJson = 124;
faReportData = 125;
faReportShort = 126;
faReportLong = 127;
faReportTrimmText = 128;
faReportJsonText = 129;
faReportDataText = 130;
faReportDiffText = 131;
faReportAusgabeDetail = 132;
faReportAusgabeRL = 133;
faReportAusgabeRP = 134;
faReportAusgabeRLE = 135;
faReportAusgabeRPE = 136;
faReportAusgabeDiffL = 137;
faReportAusgabeDiffP = 138;
faReportXML = 139;
faReportDebugReport = 140;
faReportReadme = 141;

{ RggChart }
faChartRect = 142;
faChartTextRect = 143;
faChartLegend = 144;
faChartAP = 145;
faChartBP = 146;
faChartGroup = 147;
faParamCountPlus = 148;
faParamCountMinus = 149;
faPComboPlus = 150;
faPComboMinus = 151;
faXComboPlus = 152;
faXComboMinus = 153;
faYComboPlus = 154;
faYComboMinus = 155;
faChartReset = 156;

{ RggGraph }
faToggleLineColor = 157;
faToggleUseDisplayList = 158;
faToggleUseQuickSort = 159;
faToggleShowLegend = 160;
faRggBogen = 161;
faRggKoppel = 162;
faRggHull = 163;
faRggZoomIn = 164;
faRggZoomOut = 165;
faToggleSalingGraph = 166;
faToggleControllerGraph = 167;
faToggleChartGraph = 168;
faToggleKraftGraph = 169;
faToggleMatrixText = 170;

{ RggSegment }
faToggleSegmentF = 171;
faToggleSegmentR = 172;
faToggleSegmentS = 173;
faToggleSegmentM = 174;
faToggleSegmentV = 175;
faToggleSegmentW = 176;
faToggleSegmentC = 177;
faToggleSegmentA = 178;

{ RggRenderOptions }
faWantRenderH = 179;
faWantRenderP = 180;
faWantRenderF = 181;
faWantRenderE = 182;
faWantRenderS = 183;

{ RggTrimms }
faTrimm0 = 184;
faTrimm1 = 185;
faTrimm2 = 186;
faTrimm3 = 187;
faTrimm4 = 188;
faTrimm5 = 189;
faTrimm6 = 190;
fa420 = 191;
faLogo = 192;

{ RggTrimmFile }
faCopyTrimmItem = 193;
faPasteTrimmItem = 194;
faCopyAndPaste = 195;
faUpdateTrimm0 = 196;
faReadTrimmFile = 197;
faSaveTrimmFile = 198;
faCopyTrimmFile = 199;

{ RggTrimmText }
faToggleTrimmText = 200;
faToggleDiffText = 201;
faToggleDataText = 202;
faToggleDebugText = 203;
faUpdateReportText = 204;

{ RggSonstiges }
faToggleHelp = 205;
faToggleReport = 206;
faToggleButtonReport = 207;
faToggleDarkMode = 208;
faToggleButtonSize = 209;
faToggleSandboxed = 210;
faToggleSpeedPanel = 211;
faToggleAllProps = 212;
faToggleAllTags = 213;

{ RggInfo }
faShowHelpText = 214;
faShowInfoText = 215;
faShowNormalKeyInfo = 216;
faShowSpecialKeyInfo = 217;
faShowDebugInfo = 218;
faShowZOrderInfo = 219;

{ BtnLegendTablet }
faTL01 = 220;
faTL02 = 221;
faTL03 = 222;
faTL04 = 223;
faTL05 = 224;
faTL06 = 225;
faTR01 = 226;
faTR02 = 227;
faTR03 = 228;
faTR04 = 229;
faTR05 = 230;
faTR06 = 231;
faTR07 = 232;
faTR08 = 233;
faBL01 = 234;
faBL02 = 235;
faBL03 = 236;
faBL04 = 237;
faBL05 = 238;
faBL06 = 239;
faBL07 = 240;
faBL08 = 241;
faBR01 = 242;
faBR02 = 243;
faBR03 = 244;
faBR04 = 245;
faBR05 = 246;
faBR06 = 247;

{ BtnLegendPhone }
faMB01 = 248;
faMB02 = 249;
faMB03 = 250;
faMB04 = 251;
faMB05 = 252;
faMB06 = 253;
faMB07 = 254;
faMB08 = 255;

{ TouchBarLegend }
faTouchBarTop = 256;
faTouchBarBottom = 257;
faTouchBarLeft = 258;
faTouchBarRight = 259;

{ Circles }
faCirclesSelectC0 = 260;
faCirclesSelectC1 = 261;
faCirclesSelectC2 = 262;
faCircleParamR1 = 263;
faCircleParamR2 = 264;
faCircleParamM1X = 265;
faCircleParamM1Y = 266;
faCircleParamM2X = 267;
faCircleParamM2Y = 268;
faLineParamA1 = 269;
faLineParamA2 = 270;
faLineParamE1 = 271;
faLineParamE2 = 272;
faCircleParamM1Z = 273;
faCircleParamM2Z = 274;
faCirclesReset = 275;

{ MemeFormat }
faMemeGotoLandscape = 276;
faMemeGotoSquare = 277;
faMemeGotoPortrait = 278;
faMemeFormat0 = 279;
faMemeFormat1 = 280;
faMemeFormat2 = 281;
faMemeFormat3 = 282;
faMemeFormat4 = 283;
faMemeFormat5 = 284;
faMemeFormat6 = 285;
faMemeFormat7 = 286;
faMemeFormat8 = 287;
faMemeFormat9 = 288;

{ Reset }
faReset = 289;
faResetPosition = 290;
faResetRotation = 291;
faResetZoom = 292;

{$ifdef WantAll}
{ DropTarget }
faToggleDropTarget = 293;

{ Language }
faToggleLanguage = 294;

{ CopyPaste }
faSave = 295;
faLoad = 296;
faOpen = 297;
faCopy = 298;
faPaste = 299;
faShare = 300;

{ ViewType }
faToggleViewType = 301;
faViewTypeOrtho = 302;
faViewTypePerspective = 303;

{ ViewOptions }
faToggleMoveMode = 304;
faLinearMove = 305;
faExpoMove = 306;

{ HullMesh }
faHullMesh = 307;
faHullMeshOn = 308;
faHullMeshOff = 309;

{ BitmapCycle }
faCycleBitmapM = 310;
faCycleBitmapP = 311;
faRandom = 312;
faRandomWhite = 313;
faRandomBlack = 314;
faBitmapEscape = 315;
faToggleContour = 316;
{$endif}

{$ifdef WantAll}
faMax = 317;
{$else}
faMax = 293;
{$endif}

ParamsRange = [
  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
