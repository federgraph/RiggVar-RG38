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
faShowActi = 12;
faShowMemo = 13;
faShowOpti = 14;
faShowConf = 15;
faShowText = 16;
faShowTrimmTab = 17;
faShowChart = 18;
faShowDiagA = 19;
faShowDiagC = 20;
faShowDiagE = 21;
faShowDiagQ = 22;

{ TouchLayout }
faTouchTablet = 23;
faTouchPhone = 24;
faTouchDesk = 25;

{ Wheel }
faPlusOne = 26;
faPlusTen = 27;
faWheelLeft = 28;
faWheelRight = 29;
faWheelDown = 30;
faWheelUp = 31;
faParamValuePlus1 = 32;
faParamValueMinus1 = 33;
faParamValuePlus10 = 34;
faParamValueMinus10 = 35;

{ ColorScheme }
faCycleColorSchemeM = 36;
faCycleColorSchemeP = 37;
faBlackText = 38;
faGrayText = 39;
faWhiteText = 40;

{ FederText }
faToggleAllText = 41;
faToggleTouchFrame = 42;

{ ViewParams }
faPan = 43;
faParamORX = 44;
faParamORY = 45;
faParamORZ = 46;
faParamRX = 47;
faParamRY = 48;
faParamRZ = 49;
faParamCZ = 50;

{ ParamT }
faParamT1 = 51;
faParamT2 = 52;
faParamT3 = 53;
faParamT4 = 54;

{ RggControls }
faController = 55;
faWinkel = 56;
faVorstag = 57;
faWante = 58;
faWoben = 59;
faSalingH = 60;
faSalingA = 61;
faSalingL = 62;
faSalingW = 63;
faMastfallF0C = 64;
faMastfallF0F = 65;
faMastfallVorlauf = 66;
faBiegung = 67;
faMastfussD0X = 68;
faVorstagOS = 69;
faWPowerOS = 70;
faParamAPW = 71;
faParamEAH = 72;
faParamEAR = 73;
faParamEI = 74;

{ RggFixPoints }
faFixpointA0 = 75;
faFixpointA = 76;
faFixpointB0 = 77;
faFixpointB = 78;
faFixpointC0 = 79;
faFixpointC = 80;
faFixpointD0 = 81;
faFixpointD = 82;
faFixpointE0 = 83;
faFixpointE = 84;
faFixpointF0 = 85;
faFixpointF = 86;

{ RggViewPoint }
faViewpointS = 87;
faViewpointA = 88;
faViewpointT = 89;
faViewpoint3 = 90;

{ RggSalingType }
faSalingTypOhne = 91;
faSalingTypDrehbar = 92;
faSalingTypFest = 93;
faSalingTypOhneStarr = 94;

{ RggCalcType }
faCalcTypQuer = 95;
faCalcTypKnick = 96;
faCalcTypGemessen = 97;

{ RggAppMode }
faDemo = 98;
faMemoryBtn = 99;
faMemoryRecallBtn = 100;
faKorrigiertItem = 101;
faSofortBtn = 102;
faGrauBtn = 103;
faBlauBtn = 104;
faMultiBtn = 105;

{ RggSuper }
faSuperSimple = 106;
faSuperNormal = 107;
faSuperGrau = 108;
faSuperBlau = 109;
faSuperMulti = 110;
faSuperDisplay = 111;
faSuperQuick = 112;

{ RggReport }
faReportNone = 113;
faReportLog = 114;
faReportJson = 115;
faReportData = 116;
faReportShort = 117;
faReportLong = 118;
faReportTrimmText = 119;
faReportJsonText = 120;
faReportDataText = 121;
faReportDiffText = 122;
faReportAusgabeRL = 123;
faReportAusgabeRP = 124;
faReportAusgabeRLE = 125;
faReportAusgabeRPE = 126;
faReportAusgabeDiffL = 127;
faReportAusgabeDiffP = 128;
faReportXML = 129;
faReportDebugReport = 130;
faReportReadme = 131;

{ RggChart }
faChartRect = 132;
faChartTextRect = 133;
faChartLegend = 134;
faChartAP = 135;
faChartBP = 136;
faChartGroup = 137;
faParamCountPlus = 138;
faParamCountMinus = 139;
faPComboPlus = 140;
faPComboMinus = 141;
faXComboPlus = 142;
faXComboMinus = 143;
faYComboPlus = 144;
faYComboMinus = 145;
faChartReset = 146;

{ RggGraph }
faToggleLineColor = 147;
faToggleUseDisplayList = 148;
faToggleUseQuickSort = 149;
faToggleShowLegend = 150;
faRggBogen = 151;
faRggKoppel = 152;
faRggHull = 153;
faRggZoomIn = 154;
faRggZoomOut = 155;
faToggleSalingGraph = 156;
faToggleControllerGraph = 157;
faToggleChartGraph = 158;
faToggleKraftGraph = 159;
faToggleMatrixText = 160;

{ RggSegment }
faToggleSegmentF = 161;
faToggleSegmentR = 162;
faToggleSegmentS = 163;
faToggleSegmentM = 164;
faToggleSegmentV = 165;
faToggleSegmentW = 166;
faToggleSegmentC = 167;
faToggleSegmentA = 168;

{ RggRenderOptions }
faWantRenderH = 169;
faWantRenderP = 170;
faWantRenderF = 171;
faWantRenderE = 172;
faWantRenderS = 173;

{ RggTrimms }
faTrimm0 = 174;
faTrimm1 = 175;
faTrimm2 = 176;
faTrimm3 = 177;
faTrimm4 = 178;
faTrimm5 = 179;
faTrimm6 = 180;
fa420 = 181;
faLogo = 182;

{ RggTrimmFile }
faCopyTrimmItem = 183;
faPasteTrimmItem = 184;
faCopyAndPaste = 185;
faUpdateTrimm0 = 186;
faReadTrimmFile = 187;
faSaveTrimmFile = 188;
faCopyTrimmFile = 189;

{ RggTrimmText }
faToggleTrimmText = 190;
faToggleDiffText = 191;
faToggleDataText = 192;
faToggleDebugText = 193;
faUpdateReportText = 194;

{ RggSonstiges }
faMemeToggleHelp = 195;
faMemeToggleReport = 196;
faButtonFrameReport = 197;
faToggleFontColor = 198;
faToggleSandboxed = 199;
faToggleSpeedPanel = 200;
faToggleAllProps = 201;
faToggleAllTags = 202;

{ BtnLegendTablet }
faTL01 = 203;
faTL02 = 204;
faTL03 = 205;
faTL04 = 206;
faTL05 = 207;
faTL06 = 208;
faTR01 = 209;
faTR02 = 210;
faTR03 = 211;
faTR04 = 212;
faTR05 = 213;
faTR06 = 214;
faTR07 = 215;
faTR08 = 216;
faBL01 = 217;
faBL02 = 218;
faBL03 = 219;
faBL04 = 220;
faBL05 = 221;
faBL06 = 222;
faBL07 = 223;
faBL08 = 224;
faBR01 = 225;
faBR02 = 226;
faBR03 = 227;
faBR04 = 228;
faBR05 = 229;
faBR06 = 230;

{ BtnLegendPhone }
faMB01 = 231;
faMB02 = 232;
faMB03 = 233;
faMB04 = 234;
faMB05 = 235;
faMB06 = 236;
faMB07 = 237;
faMB08 = 238;

{ Circles }
faCirclesSelectC0 = 239;
faCirclesSelectC1 = 240;
faCirclesSelectC2 = 241;
faCircleParamR1 = 242;
faCircleParamR2 = 243;
faCircleParamM1X = 244;
faCircleParamM1Y = 245;
faCircleParamM2X = 246;
faCircleParamM2Y = 247;
faLineParamA1 = 248;
faLineParamA2 = 249;
faLineParamE1 = 250;
faLineParamE2 = 251;
faCircleParamM1Z = 252;
faCircleParamM2Z = 253;
faCirclesReset = 254;

{ MemeFormat }
faMemeGotoLandscape = 255;
faMemeGotoSquare = 256;
faMemeGotoPortrait = 257;
faMemeFormat0 = 258;
faMemeFormat1 = 259;
faMemeFormat2 = 260;
faMemeFormat3 = 261;
faMemeFormat4 = 262;
faMemeFormat5 = 263;
faMemeFormat6 = 264;
faMemeFormat7 = 265;
faMemeFormat8 = 266;
faMemeFormat9 = 267;

faMax = 268;

ParamsRange = [
//  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH ..faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
