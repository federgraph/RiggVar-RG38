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
faShowActions = 12;
faShowMemo = 13;
faShowOptions = 14;
faShowConfig = 15;
faShowForce = 16;
faShowKreis = 17;
faShowInfo = 18;
faShowSplash = 19;
faShowTabelle = 20;
faShowDetail = 21;
faShowSaling = 22;
faShowController = 23;
faShowText = 24;
faShowTrimmTab = 25;
faShowChart = 26;
faShowDiagA = 27;
faShowDiagC = 28;
faShowDiagE = 29;
faShowDiagQ = 30;

{ TouchLayout }
faTouchTablet = 31;
faTouchPhone = 32;
faTouchDesk = 33;

{ Wheel }
faPlusOne = 34;
faPlusTen = 35;
faWheelLeft = 36;
faWheelRight = 37;
faWheelDown = 38;
faWheelUp = 39;
faParamValuePlus1 = 40;
faParamValueMinus1 = 41;
faParamValuePlus10 = 42;
faParamValueMinus10 = 43;

{ ColorScheme }
faCycleColorSchemeM = 44;
faCycleColorSchemeP = 45;
faBlackText = 46;
faGrayText = 47;
faWhiteText = 48;

{ FederText }
faToggleAllText = 49;
faToggleTouchFrame = 50;

{ ViewParams }
faPan = 51;
faParamORX = 52;
faParamORY = 53;
faParamORZ = 54;
faParamRX = 55;
faParamRY = 56;
faParamRZ = 57;
faParamCZ = 58;

{ ParamT }
faParamT1 = 59;
faParamT2 = 60;
faParamT3 = 61;
faParamT4 = 62;

{ RggControls }
faController = 63;
faWinkel = 64;
faVorstag = 65;
faWante = 66;
faWoben = 67;
faSalingH = 68;
faSalingA = 69;
faSalingL = 70;
faSalingW = 71;
faMastfallF0C = 72;
faMastfallF0F = 73;
faMastfallVorlauf = 74;
faBiegung = 75;
faMastfussD0X = 76;
faVorstagOS = 77;
faWPowerOS = 78;
faParamAPW = 79;
faParamEAH = 80;
faParamEAR = 81;
faParamEI = 82;

{ RggFixPoints }
faFixpointA0 = 83;
faFixpointA = 84;
faFixpointB0 = 85;
faFixpointB = 86;
faFixpointC0 = 87;
faFixpointC = 88;
faFixpointD0 = 89;
faFixpointD = 90;
faFixpointE0 = 91;
faFixpointE = 92;
faFixpointF0 = 93;
faFixpointF = 94;

{ RggViewPoint }
faViewpointS = 95;
faViewpointA = 96;
faViewpointT = 97;
faViewpoint3 = 98;

{ RggSalingType }
faSalingTypOhne = 99;
faSalingTypDrehbar = 100;
faSalingTypFest = 101;
faSalingTypOhneStarr = 102;

{ RggCalcType }
faCalcTypQuer = 103;
faCalcTypKnick = 104;
faCalcTypGemessen = 105;

{ RggAppMode }
faDemo = 106;
faMemoryBtn = 107;
faMemoryRecallBtn = 108;
faKorrigiertItem = 109;
faSofortBtn = 110;
faGrauBtn = 111;
faBlauBtn = 112;
faMultiBtn = 113;

{ RggSuper }
faSuperSimple = 114;
faSuperNormal = 115;
faSuperGrau = 116;
faSuperBlau = 117;
faSuperMulti = 118;
faSuperDisplay = 119;
faSuperQuick = 120;

{ RggReport }
faReportNone = 121;
faReportLog = 122;
faReportJson = 123;
faReportData = 124;
faReportShort = 125;
faReportLong = 126;
faReportTrimmText = 127;
faReportJsonText = 128;
faReportDataText = 129;
faReportDiffText = 130;
faReportAusgabeDetail = 131;
faReportAusgabeRL = 132;
faReportAusgabeRP = 133;
faReportAusgabeRLE = 134;
faReportAusgabeRPE = 135;
faReportAusgabeDiffL = 136;
faReportAusgabeDiffP = 137;
faReportXML = 138;
faReportDebugReport = 139;
faReportReadme = 140;

{ RggChart }
faChartRect = 141;
faChartTextRect = 142;
faChartLegend = 143;
faChartAP = 144;
faChartBP = 145;
faChartGroup = 146;
faParamCountPlus = 147;
faParamCountMinus = 148;
faPComboPlus = 149;
faPComboMinus = 150;
faXComboPlus = 151;
faXComboMinus = 152;
faYComboPlus = 153;
faYComboMinus = 154;
faChartReset = 155;

{ RggGraph }
faToggleLineColor = 156;
faToggleUseDisplayList = 157;
faToggleUseQuickSort = 158;
faToggleShowLegend = 159;
faRggBogen = 160;
faRggKoppel = 161;
faRggHull = 162;
faRggZoomIn = 163;
faRggZoomOut = 164;
faToggleSalingGraph = 165;
faToggleControllerGraph = 166;
faToggleChartGraph = 167;
faToggleKraftGraph = 168;
faToggleMatrixText = 169;

{ RggSegment }
faToggleSegmentF = 170;
faToggleSegmentR = 171;
faToggleSegmentS = 172;
faToggleSegmentM = 173;
faToggleSegmentV = 174;
faToggleSegmentW = 175;
faToggleSegmentC = 176;
faToggleSegmentA = 177;

{ RggRenderOptions }
faWantRenderH = 178;
faWantRenderP = 179;
faWantRenderF = 180;
faWantRenderE = 181;
faWantRenderS = 182;

{ RggTrimms }
faTrimm0 = 183;
faTrimm1 = 184;
faTrimm2 = 185;
faTrimm3 = 186;
faTrimm4 = 187;
faTrimm5 = 188;
faTrimm6 = 189;
fa420 = 190;
faLogo = 191;

{ RggTrimmFile }
faCopyTrimmItem = 192;
faPasteTrimmItem = 193;
faCopyAndPaste = 194;
faUpdateTrimm0 = 195;
faReadTrimmFile = 196;
faSaveTrimmFile = 197;
faCopyTrimmFile = 198;

{ RggTrimmText }
faToggleTrimmText = 199;
faToggleDiffText = 200;
faToggleDataText = 201;
faToggleDebugText = 202;
faUpdateReportText = 203;

{ RggSonstiges }
faMemeToggleHelp = 204;
faMemeToggleReport = 205;
faButtonFrameReport = 206;
faToggleFontColor = 207;
faToggleSandboxed = 208;
faToggleSpeedPanel = 209;
faToggleAllProps = 210;
faToggleAllTags = 211;

{ BtnLegendTablet }
faTL01 = 212;
faTL02 = 213;
faTL03 = 214;
faTL04 = 215;
faTL05 = 216;
faTL06 = 217;
faTR01 = 218;
faTR02 = 219;
faTR03 = 220;
faTR04 = 221;
faTR05 = 222;
faTR06 = 223;
faTR07 = 224;
faTR08 = 225;
faBL01 = 226;
faBL02 = 227;
faBL03 = 228;
faBL04 = 229;
faBL05 = 230;
faBL06 = 231;
faBL07 = 232;
faBL08 = 233;
faBR01 = 234;
faBR02 = 235;
faBR03 = 236;
faBR04 = 237;
faBR05 = 238;
faBR06 = 239;

{ BtnLegendPhone }
faMB01 = 240;
faMB02 = 241;
faMB03 = 242;
faMB04 = 243;
faMB05 = 244;
faMB06 = 245;
faMB07 = 246;
faMB08 = 247;

{ Circles }
faCirclesSelectC0 = 248;
faCirclesSelectC1 = 249;
faCirclesSelectC2 = 250;
faCircleParamR1 = 251;
faCircleParamR2 = 252;
faCircleParamM1X = 253;
faCircleParamM1Y = 254;
faCircleParamM2X = 255;
faCircleParamM2Y = 256;
faLineParamA1 = 257;
faLineParamA2 = 258;
faLineParamE1 = 259;
faLineParamE2 = 260;
faCircleParamM1Z = 261;
faCircleParamM2Z = 262;
faCirclesReset = 263;

{ MemeFormat }
faMemeGotoLandscape = 264;
faMemeGotoSquare = 265;
faMemeGotoPortrait = 266;
faMemeFormat0 = 267;
faMemeFormat1 = 268;
faMemeFormat2 = 269;
faMemeFormat3 = 270;
faMemeFormat4 = 271;
faMemeFormat5 = 272;
faMemeFormat6 = 273;
faMemeFormat7 = 274;
faMemeFormat8 = 275;
faMemeFormat9 = 276;

faShowDrawings = 277;

faMax = 278;

ParamsRange = [
//  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH ..faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
