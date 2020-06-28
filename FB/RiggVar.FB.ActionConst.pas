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

{ RggFixPoints }
faFixpointA0 = 67;
faFixpointA = 68;
faFixpointB0 = 69;
faFixpointB = 70;
faFixpointC0 = 71;
faFixpointC = 72;
faFixpointD0 = 73;
faFixpointD = 74;
faFixpointE0 = 75;
faFixpointE = 76;
faFixpointF0 = 77;
faFixpointF = 78;

{ RggViewPoint }
faViewpointS = 79;
faViewpointA = 80;
faViewpointT = 81;
faViewpoint3 = 82;

{ RggSalingType }
faSalingTypOhne = 83;
faSalingTypDrehbar = 84;
faSalingTypFest = 85;
faSalingTypOhneStarr = 86;

{ RggAppMode }
faDemo = 87;
faMemoryBtn = 88;
faMemoryRecallBtn = 89;
faSofortBtn = 90;
faGrauBtn = 91;
faBlauBtn = 92;
faMultiBtn = 93;
faKoppelBtn = 94;
faHull = 95;

{ RggReport }
faReportNone = 96;
faReportLog = 97;
faReportJson = 98;
faReportData = 99;
faReportShort = 100;
faReportLong = 101;
faReportTrimmText = 102;
faReportJsonText = 103;
faReportDataText = 104;
faReportDiffText = 105;
faReportAusgabeRL = 106;
faReportAusgabeRP = 107;
faReportAusgabeRLE = 108;
faReportAusgabeRPE = 109;
faReportAusgabeDiffL = 110;
faReportAusgabeDiffP = 111;
faReportXML = 112;
faReportDebugReport = 113;
faReportReadme = 114;

{ RggChart }
faChartRect = 115;
faChartTextRect = 116;
faChartLegend = 117;
faChartAP = 118;
faChartBP = 119;
faChartGroup = 120;
faParamCountPlus = 121;
faParamCountMinus = 122;
faPComboPlus = 123;
faPComboMinus = 124;
faXComboPlus = 125;
faXComboMinus = 126;
faYComboPlus = 127;
faYComboMinus = 128;
faChartReset = 129;

{ RggGraph }
faToggleLineColor = 130;
faToggleUseDisplayList = 131;
faToggleUseQuickSort = 132;
faToggleShowLegend = 133;
faRggBogen = 134;
faRggZoomIn = 135;
faRggZoomOut = 136;
faToggleSalingGraph = 137;
faToggleControllerGraph = 138;
faToggleChartGraph = 139;
faToggleMatrixText = 140;

{ RggSegment }
faToggleSegmentF = 141;
faToggleSegmentR = 142;
faToggleSegmentS = 143;
faToggleSegmentM = 144;
faToggleSegmentV = 145;
faToggleSegmentW = 146;
faToggleSegmentC = 147;
faToggleSegmentA = 148;

{ RggRenderOptions }
faWantRenderH = 149;
faWantRenderP = 150;
faWantRenderF = 151;
faWantRenderE = 152;
faWantRenderS = 153;

{ RggTrimms }
faTrimm0 = 154;
faTrimm1 = 155;
faTrimm2 = 156;
faTrimm3 = 157;
faTrimm4 = 158;
faTrimm5 = 159;
faTrimm6 = 160;
fa420 = 161;
faLogo = 162;

{ RggTrimmFile }
faCopyTrimmItem = 163;
faPasteTrimmItem = 164;
faCopyAndPaste = 165;
faUpdateTrimm0 = 166;
faReadTrimmFile = 167;
faSaveTrimmFile = 168;
faCopyTrimmFile = 169;

{ RggTrimmText }
faToggleTrimmText = 170;
faToggleDiffText = 171;
faToggleDataText = 172;
faToggleDebugText = 173;
faUpdateReportText = 174;

{ RggSonstiges }
faMemeToggleHelp = 175;
faMemeToggleReport = 176;
faButtonFrameReport = 177;
faToggleFontColor = 178;
faToggleSandboxed = 179;
faToggleSpeedPanel = 180;
faToggleAllProps = 181;
faToggleAllTags = 182;

{ BtnLegendTablet }
faTL01 = 183;
faTL02 = 184;
faTL03 = 185;
faTL04 = 186;
faTL05 = 187;
faTL06 = 188;
faTR01 = 189;
faTR02 = 190;
faTR03 = 191;
faTR04 = 192;
faTR05 = 193;
faTR06 = 194;
faTR07 = 195;
faTR08 = 196;
faBL01 = 197;
faBL02 = 198;
faBL03 = 199;
faBL04 = 200;
faBL05 = 201;
faBL06 = 202;
faBL07 = 203;
faBL08 = 204;
faBR01 = 205;
faBR02 = 206;
faBR03 = 207;
faBR04 = 208;
faBR05 = 209;
faBR06 = 210;

{ BtnLegendPhone }
faMB01 = 211;
faMB02 = 212;
faMB03 = 213;
faMB04 = 214;
faMB05 = 215;
faMB06 = 216;
faMB07 = 217;
faMB08 = 218;

{ Circles }
faCirclesSelectC0 = 219;
faCirclesSelectC1 = 220;
faCirclesSelectC2 = 221;
faCircleParamR1 = 222;
faCircleParamR2 = 223;
faCircleParamM1X = 224;
faCircleParamM1Y = 225;
faCircleParamM2X = 226;
faCircleParamM2Y = 227;
faLineParamA1 = 228;
faLineParamA2 = 229;
faLineParamE1 = 230;
faLineParamE2 = 231;
faCircleParamM1Z = 232;
faCircleParamM2Z = 233;
faCirclesReset = 234;

{ MemeFormat }
faMemeGotoLandscape = 235;
faMemeGotoSquare = 236;
faMemeGotoPortrait = 237;
faMemeFormat0 = 238;
faMemeFormat1 = 239;
faMemeFormat2 = 240;
faMemeFormat3 = 241;
faMemeFormat4 = 242;
faMemeFormat5 = 243;
faMemeFormat6 = 244;
faMemeFormat7 = 245;
faMemeFormat8 = 246;
faMemeFormat9 = 247;

faParamEAH = 248;
faParamEAR = 249;
faParamEI = 250;

faMax = 251;

ParamsRange = [
  faController .. faParamAPW,
  faParamEAH ..faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
