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

{ TouchLayout }
faTouchTablet = 14;
faTouchPhone = 15;
faTouchDesk = 16;

{ Wheel }
faPlusOne = 17;
faPlusTen = 18;
faWheelLeft = 19;
faWheelRight = 20;
faWheelDown = 21;
faWheelUp = 22;
faParamValuePlus1 = 23;
faParamValueMinus1 = 24;
faParamValuePlus10 = 25;
faParamValueMinus10 = 26;

{ ColorScheme }
faCycleColorSchemeM = 27;
faCycleColorSchemeP = 28;
faBlackText = 29;
faGrayText = 30;
faWhiteText = 31;

{ FederText }
faToggleAllText = 32;
faToggleTouchFrame = 33;

{ ViewParams }
faPan = 34;
faParamORX = 35;
faParamORY = 36;
faParamORZ = 37;
faParamRX = 38;
faParamRY = 39;
faParamRZ = 40;
faParamCZ = 41;

{ RggControls }
faController = 42;
faWinkel = 43;
faVorstag = 44;
faWante = 45;
faWoben = 46;
faSalingH = 47;
faSalingA = 48;
faSalingL = 49;
faSalingW = 50;
faMastfallF0C = 51;
faMastfallF0F = 52;
faMastfallVorlauf = 53;
faBiegung = 54;
faMastfussD0X = 55;
faVorstagOS = 56;
faWPowerOS = 57;

{ RggFixPoints }
faFixpointA0 = 58;
faFixpointA = 59;
faFixpointB0 = 60;
faFixpointB = 61;
faFixpointC0 = 62;
faFixpointC = 63;
faFixpointD0 = 64;
faFixpointD = 65;
faFixpointE0 = 66;
faFixpointE = 67;
faFixpointF0 = 68;
faFixpointF = 69;

{ RggViewPoint }
faViewpointS = 70;
faViewpointA = 71;
faViewpointT = 72;
faViewpoint3 = 73;

{ RggSalingType }
faSalingTypOhne = 74;
faSalingTypDrehbar = 75;
faSalingTypFest = 76;
faSalingTypOhneStarr = 77;

{ RggAppMode }
faDemo = 78;
faMemoryBtn = 79;
faMemoryRecallBtn = 80;
faSofortBtn = 81;
faGrauBtn = 82;
faBlauBtn = 83;
faMultiBtn = 84;
faKoppelBtn = 85;
faHull = 86;

{ RggReport }
faReportNone = 87;
faReportLog = 88;
faReportJson = 89;
faReportData = 90;
faReportShort = 91;
faReportLong = 92;
faReportTrimmText = 93;
faReportJsonText = 94;
faReportDataText = 95;
faReportDiffText = 96;
faReportAusgabeRL = 97;
faReportAusgabeRP = 98;
faReportAusgabeRLE = 99;
faReportAusgabeRPE = 100;
faReportAusgabeDiffL = 101;
faReportAusgabeDiffP = 102;
faReportXML = 103;
faReportDebugReport = 104;
faReportReadme = 105;

{ RggChart }
faChartRect = 106;
faChartTextRect = 107;
faChartLegend = 108;
faChartAP = 109;
faChartBP = 110;
faChartGroup = 111;
faParamCountPlus = 112;
faParamCountMinus = 113;
faPComboPlus = 114;
faPComboMinus = 115;
faXComboPlus = 116;
faXComboMinus = 117;
faYComboPlus = 118;
faYComboMinus = 119;
faChartReset = 120;

{ RggGraph }
faToggleLineColor = 121;
faToggleUseDisplayList = 122;
faToggleUseQuickSort = 123;
faToggleShowLegend = 124;
faRggBogen = 125;
faRggZoomIn = 126;
faRggZoomOut = 127;
faToggleSalingGraph = 128;
faToggleControllerGraph = 129;
faToggleChartGraph = 130;
faToggleMatrixText = 131;

{ RggSegment }
faToggleSegmentF = 132;
faToggleSegmentR = 133;
faToggleSegmentS = 134;
faToggleSegmentM = 135;
faToggleSegmentV = 136;
faToggleSegmentW = 137;
faToggleSegmentC = 138;
faToggleSegmentA = 139;

{ RggRenderOptions }
faWantRenderH = 140;
faWantRenderP = 141;
faWantRenderF = 142;
faWantRenderE = 143;
faWantRenderS = 144;

{ RggTrimms }
faTrimm0 = 145;
faTrimm1 = 146;
faTrimm2 = 147;
faTrimm3 = 148;
faTrimm4 = 149;
faTrimm5 = 150;
faTrimm6 = 151;
fa420 = 152;
faLogo = 153;

{ RggTrimmFile }
faCopyTrimmItem = 154;
faPasteTrimmItem = 155;
faCopyAndPaste = 156;
faUpdateTrimm0 = 157;
faReadTrimmFile = 158;
faSaveTrimmFile = 159;
faCopyTrimmFile = 160;

{ RggTrimmText }
faToggleTrimmText = 161;
faToggleDiffText = 162;
faToggleDataText = 163;
faToggleDebugText = 164;
faUpdateReportText = 165;

{ RggSonstiges }
faMemeToggleHelp = 166;
faMemeToggleReport = 167;
faButtonFrameReport = 168;
faToggleFontColor = 169;
faToggleSandboxed = 170;
faToggleAllProps = 171;
faToggleAllTags = 172;

{ BtnLegendTablet }
faTL01 = 173;
faTL02 = 174;
faTL03 = 175;
faTL04 = 176;
faTL05 = 177;
faTL06 = 178;
faTR01 = 179;
faTR02 = 180;
faTR03 = 181;
faTR04 = 182;
faTR05 = 183;
faTR06 = 184;
faTR07 = 185;
faTR08 = 186;
faBL01 = 187;
faBL02 = 188;
faBL03 = 189;
faBL04 = 190;
faBL05 = 191;
faBL06 = 192;
faBL07 = 193;
faBL08 = 194;
faBR01 = 195;
faBR02 = 196;
faBR03 = 197;
faBR04 = 198;
faBR05 = 199;
faBR06 = 200;

{ BtnLegendPhone }
faMB01 = 201;
faMB02 = 202;
faMB03 = 203;
faMB04 = 204;
faMB05 = 205;
faMB06 = 206;
faMB07 = 207;
faMB08 = 208;

{ Circles }
faCirclesSelectC0 = 209;
faCirclesSelectC1 = 210;
faCirclesSelectC2 = 211;
faCircleParamR1 = 212;
faCircleParamR2 = 213;
faCircleParamM1X = 214;
faCircleParamM1Y = 215;
faCircleParamM2X = 216;
faCircleParamM2Y = 217;
faLineParamA1 = 218;
faLineParamA2 = 219;
faLineParamE1 = 220;
faLineParamE2 = 221;
faCircleParamM1Z = 222;
faCircleParamM2Z = 223;
faCirclesReset = 224;

{ Format }
faMemeGotoLandscape = 225;
faMemeGotoSquare = 226;
faMemeGotoPortrait = 227;
faMemeFormat0 = 228;
faMemeFormat1 = 229;
faMemeFormat2 = 230;
faMemeFormat3 = 231;
faMemeFormat4 = 232;
faMemeFormat5 = 233;
faMemeFormat6 = 234;
faMemeFormat7 = 235;
faMemeFormat8 = 236;
faMemeFormat9 = 237;

faToggleSpeedPanel = 238;

faMax = 239;

ParamsRange = [faController .. faMastfussD0X];
ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
