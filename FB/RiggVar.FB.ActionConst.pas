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
faShowTrimmTab = 16;
faShowChart = 17;
faShowDiagA = 18;
faShowDiagC = 19;
faShowDiagE = 20;
faShowDiagQ = 21;

{ TouchLayout }
faTouchTablet = 22;
faTouchPhone = 23;
faTouchDesk = 24;

{ Wheel }
faPlusOne = 25;
faPlusTen = 26;
faWheelLeft = 27;
faWheelRight = 28;
faWheelDown = 29;
faWheelUp = 30;
faParamValuePlus1 = 31;
faParamValueMinus1 = 32;
faParamValuePlus10 = 33;
faParamValueMinus10 = 34;

{ ColorScheme }
faCycleColorSchemeM = 35;
faCycleColorSchemeP = 36;
faBlackText = 37;
faGrayText = 38;
faWhiteText = 39;

{ FederText }
faToggleAllText = 40;
faToggleTouchFrame = 41;

{ ViewParams }
faPan = 42;
faParamORX = 43;
faParamORY = 44;
faParamORZ = 45;
faParamRX = 46;
faParamRY = 47;
faParamRZ = 48;
faParamCZ = 49;

{ RggControls }
faController = 50;
faWinkel = 51;
faVorstag = 52;
faWante = 53;
faWoben = 54;
faSalingH = 55;
faSalingA = 56;
faSalingL = 57;
faSalingW = 58;
faMastfallF0C = 59;
faMastfallF0F = 60;
faMastfallVorlauf = 61;
faBiegung = 62;
faMastfussD0X = 63;
faVorstagOS = 64;
faWPowerOS = 65;
faParamAPW = 66;
faParamEAH = 67;
faParamEAR = 68;
faParamEI = 69;

{ RggFixPoints }
faFixpointA0 = 70;
faFixpointA = 71;
faFixpointB0 = 72;
faFixpointB = 73;
faFixpointC0 = 74;
faFixpointC = 75;
faFixpointD0 = 76;
faFixpointD = 77;
faFixpointE0 = 78;
faFixpointE = 79;
faFixpointF0 = 80;
faFixpointF = 81;

{ RggViewPoint }
faViewpointS = 82;
faViewpointA = 83;
faViewpointT = 84;
faViewpoint3 = 85;

{ RggSalingType }
faSalingTypOhne = 86;
faSalingTypDrehbar = 87;
faSalingTypFest = 88;
faSalingTypOhneStarr = 89;

{ RggAppMode }
faDemo = 90;
faMemoryBtn = 91;
faMemoryRecallBtn = 92;
faSofortBtn = 93;
faGrauBtn = 94;
faBlauBtn = 95;
faMultiBtn = 96;

{ RggSuper }
faSuperSimple = 97;
faSuperNormal = 98;
faSuperGrau = 99;
faSuperBlau = 100;
faSuperMulti = 101;
faSuperDisplay = 102;
faSuperQuick = 103;

{ RggReport }
faReportNone = 104;
faReportLog = 105;
faReportJson = 106;
faReportData = 107;
faReportShort = 108;
faReportLong = 109;
faReportTrimmText = 110;
faReportJsonText = 111;
faReportDataText = 112;
faReportDiffText = 113;
faReportAusgabeRL = 114;
faReportAusgabeRP = 115;
faReportAusgabeRLE = 116;
faReportAusgabeRPE = 117;
faReportAusgabeDiffL = 118;
faReportAusgabeDiffP = 119;
faReportXML = 120;
faReportDebugReport = 121;
faReportReadme = 122;

{ RggChart }
faChartRect = 123;
faChartTextRect = 124;
faChartLegend = 125;
faChartAP = 126;
faChartBP = 127;
faChartGroup = 128;
faParamCountPlus = 129;
faParamCountMinus = 130;
faPComboPlus = 131;
faPComboMinus = 132;
faXComboPlus = 133;
faXComboMinus = 134;
faYComboPlus = 135;
faYComboMinus = 136;
faChartReset = 137;

{ RggGraph }
faToggleLineColor = 138;
faToggleUseDisplayList = 139;
faToggleUseQuickSort = 140;
faToggleShowLegend = 141;
faRggBogen = 142;
faRggKoppel = 143;
faRggHull = 144;
faRggZoomIn = 145;
faRggZoomOut = 146;
faToggleSalingGraph = 147;
faToggleControllerGraph = 148;
faToggleChartGraph = 149;
faToggleMatrixText = 150;

{ RggSegment }
faToggleSegmentF = 151;
faToggleSegmentR = 152;
faToggleSegmentS = 153;
faToggleSegmentM = 154;
faToggleSegmentV = 155;
faToggleSegmentW = 156;
faToggleSegmentC = 157;
faToggleSegmentA = 158;

{ RggRenderOptions }
faWantRenderH = 159;
faWantRenderP = 160;
faWantRenderF = 161;
faWantRenderE = 162;
faWantRenderS = 163;

{ RggTrimms }
faTrimm0 = 164;
faTrimm1 = 165;
faTrimm2 = 166;
faTrimm3 = 167;
faTrimm4 = 168;
faTrimm5 = 169;
faTrimm6 = 170;
fa420 = 171;
faLogo = 172;

{ RggTrimmFile }
faCopyTrimmItem = 173;
faPasteTrimmItem = 174;
faCopyAndPaste = 175;
faUpdateTrimm0 = 176;
faReadTrimmFile = 177;
faSaveTrimmFile = 178;
faCopyTrimmFile = 179;

{ RggTrimmText }
faToggleTrimmText = 180;
faToggleDiffText = 181;
faToggleDataText = 182;
faToggleDebugText = 183;
faUpdateReportText = 184;

{ RggSonstiges }
faMemeToggleHelp = 185;
faMemeToggleReport = 186;
faButtonFrameReport = 187;
faToggleFontColor = 188;
faToggleSandboxed = 189;
faToggleSpeedPanel = 190;
faToggleAllProps = 191;
faToggleAllTags = 192;

{ BtnLegendTablet }
faTL01 = 193;
faTL02 = 194;
faTL03 = 195;
faTL04 = 196;
faTL05 = 197;
faTL06 = 198;
faTR01 = 199;
faTR02 = 200;
faTR03 = 201;
faTR04 = 202;
faTR05 = 203;
faTR06 = 204;
faTR07 = 205;
faTR08 = 206;
faBL01 = 207;
faBL02 = 208;
faBL03 = 209;
faBL04 = 210;
faBL05 = 211;
faBL06 = 212;
faBL07 = 213;
faBL08 = 214;
faBR01 = 215;
faBR02 = 216;
faBR03 = 217;
faBR04 = 218;
faBR05 = 219;
faBR06 = 220;

{ BtnLegendPhone }
faMB01 = 221;
faMB02 = 222;
faMB03 = 223;
faMB04 = 224;
faMB05 = 225;
faMB06 = 226;
faMB07 = 227;
faMB08 = 228;

{ Circles }
faCirclesSelectC0 = 229;
faCirclesSelectC1 = 230;
faCirclesSelectC2 = 231;
faCircleParamR1 = 232;
faCircleParamR2 = 233;
faCircleParamM1X = 234;
faCircleParamM1Y = 235;
faCircleParamM2X = 236;
faCircleParamM2Y = 237;
faLineParamA1 = 238;
faLineParamA2 = 239;
faLineParamE1 = 240;
faLineParamE2 = 241;
faCircleParamM1Z = 242;
faCircleParamM2Z = 243;
faCirclesReset = 244;

{ MemeFormat }
faMemeGotoLandscape = 245;
faMemeGotoSquare = 246;
faMemeGotoPortrait = 247;
faMemeFormat0 = 248;
faMemeFormat1 = 249;
faMemeFormat2 = 250;
faMemeFormat3 = 251;
faMemeFormat4 = 252;
faMemeFormat5 = 253;
faMemeFormat6 = 254;
faMemeFormat7 = 255;
faMemeFormat8 = 256;
faMemeFormat9 = 257;

faMax = 258;

ParamsRange = [
  faController .. faParamAPW,
  faParamEAH ..faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
