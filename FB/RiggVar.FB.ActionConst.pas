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
faToggleFontColor = 208;
faToggleSandboxed = 209;
faToggleSpeedPanel = 210;
faToggleAllProps = 211;
faToggleAllTags = 212;

{ BtnLegendTablet }
faTL01 = 213;
faTL02 = 214;
faTL03 = 215;
faTL04 = 216;
faTL05 = 217;
faTL06 = 218;
faTR01 = 219;
faTR02 = 220;
faTR03 = 221;
faTR04 = 222;
faTR05 = 223;
faTR06 = 224;
faTR07 = 225;
faTR08 = 226;
faBL01 = 227;
faBL02 = 228;
faBL03 = 229;
faBL04 = 230;
faBL05 = 231;
faBL06 = 232;
faBL07 = 233;
faBL08 = 234;
faBR01 = 235;
faBR02 = 236;
faBR03 = 237;
faBR04 = 238;
faBR05 = 239;
faBR06 = 240;

{ BtnLegendPhone }
faMB01 = 241;
faMB02 = 242;
faMB03 = 243;
faMB04 = 244;
faMB05 = 245;
faMB06 = 246;
faMB07 = 247;
faMB08 = 248;

{ Circles }
faCirclesSelectC0 = 249;
faCirclesSelectC1 = 250;
faCirclesSelectC2 = 251;
faCircleParamR1 = 252;
faCircleParamR2 = 253;
faCircleParamM1X = 254;
faCircleParamM1Y = 255;
faCircleParamM2X = 256;
faCircleParamM2Y = 257;
faLineParamA1 = 258;
faLineParamA2 = 259;
faLineParamE1 = 260;
faLineParamE2 = 261;
faCircleParamM1Z = 262;
faCircleParamM2Z = 263;
faCirclesReset = 264;

{ MemeFormat }
faMemeGotoLandscape = 265;
faMemeGotoSquare = 266;
faMemeGotoPortrait = 267;
faMemeFormat0 = 268;
faMemeFormat1 = 269;
faMemeFormat2 = 270;
faMemeFormat3 = 271;
faMemeFormat4 = 272;
faMemeFormat5 = 273;
faMemeFormat6 = 274;
faMemeFormat7 = 275;
faMemeFormat8 = 276;
faMemeFormat9 = 277;

{ Reset }
faReset = 278;
faResetPosition = 279;
faResetRotation = 280;
faResetZoom = 281;

{ DropTarget }
faToggleDropTarget = 282;

{ Language }
faToggleLanguage = 283;

{ CopyPaste }
faSave = 284;
faLoad = 285;
faOpen = 286;
faCopy = 287;
faPaste = 288;
faShare = 289;

{ ViewType }
faToggleViewType = 290;
faViewTypeOrtho = 291;
faViewTypePerspective = 292;

{ ViewOptions }
faToggleMoveMode = 293;
faLinearMove = 294;
faExpoMove = 295;

{ BitmapCycle }
faCycleBitmapM = 296;
faCycleBitmapP = 297;
faRandom = 298;
faRandomWhite = 299;
faRandomBlack = 300;
faBitmapEscape = 301;
faToggleContour = 302;

{ HullMesh }
faHullMesh = 303;
faHullMeshOn = 304;
faHullMeshOff = 305;

faMax = 306;

ParamsRange = [
  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
