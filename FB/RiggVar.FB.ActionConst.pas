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
faBlackText = 50;
faGrayText = 51;
faWhiteText = 52;

{ FederText }
faToggleAllText = 53;
faToggleTouchFrame = 54;

{ ViewParams }
faPan = 55;
faParamORX = 56;
faParamORY = 57;
faParamORZ = 58;
faParamRX = 59;
faParamRY = 60;
faParamRZ = 61;
faParamCZ = 62;

{ ParamT }
faParamT1 = 63;
faParamT2 = 64;
faParamT3 = 65;
faParamT4 = 66;

{ RggControls }
faController = 67;
faWinkel = 68;
faVorstag = 69;
faWante = 70;
faWoben = 71;
faSalingH = 72;
faSalingA = 73;
faSalingL = 74;
faSalingW = 75;
faMastfallF0C = 76;
faMastfallF0F = 77;
faMastfallVorlauf = 78;
faBiegung = 79;
faMastfussD0X = 80;
faVorstagOS = 81;
faWPowerOS = 82;
faParamAPW = 83;
faParamEAH = 84;
faParamEAR = 85;
faParamEI = 86;

{ RggFixPoints }
faFixpointA0 = 87;
faFixpointA = 88;
faFixpointB0 = 89;
faFixpointB = 90;
faFixpointC0 = 91;
faFixpointC = 92;
faFixpointD0 = 93;
faFixpointD = 94;
faFixpointE0 = 95;
faFixpointE = 96;
faFixpointF0 = 97;
faFixpointF = 98;

{ RggViewPoint }
faViewpointS = 99;
faViewpointA = 100;
faViewpointT = 101;
faViewpoint3 = 102;

{ RggSalingType }
faSalingTypOhne = 103;
faSalingTypDrehbar = 104;
faSalingTypFest = 105;
faSalingTypOhneStarr = 106;

{ RggCalcType }
faCalcTypQuer = 107;
faCalcTypKnick = 108;
faCalcTypGemessen = 109;

{ RggAppMode }
faDemo = 110;
faMemoryBtn = 111;
faMemoryRecallBtn = 112;
faKorrigiertItem = 113;
faSofortBtn = 114;
faGrauBtn = 115;
faBlauBtn = 116;
faMultiBtn = 117;

{ RggSuper }
faSuperSimple = 118;
faSuperNormal = 119;
faSuperGrau = 120;
faSuperBlau = 121;
faSuperMulti = 122;
faSuperDisplay = 123;
faSuperQuick = 124;

{ RggReport }
faReportNone = 125;
faReportLog = 126;
faReportJson = 127;
faReportData = 128;
faReportShort = 129;
faReportLong = 130;
faReportTrimmText = 131;
faReportJsonText = 132;
faReportDataText = 133;
faReportDiffText = 134;
faReportAusgabeDetail = 135;
faReportAusgabeRL = 136;
faReportAusgabeRP = 137;
faReportAusgabeRLE = 138;
faReportAusgabeRPE = 139;
faReportAusgabeDiffL = 140;
faReportAusgabeDiffP = 141;
faReportXML = 142;
faReportDebugReport = 143;
faReportReadme = 144;

{ RggChart }
faChartRect = 145;
faChartTextRect = 146;
faChartLegend = 147;
faChartAP = 148;
faChartBP = 149;
faChartGroup = 150;
faParamCountPlus = 151;
faParamCountMinus = 152;
faPComboPlus = 153;
faPComboMinus = 154;
faXComboPlus = 155;
faXComboMinus = 156;
faYComboPlus = 157;
faYComboMinus = 158;
faChartReset = 159;

{ RggGraph }
faToggleLineColor = 160;
faToggleUseDisplayList = 161;
faToggleUseQuickSort = 162;
faToggleShowLegend = 163;
faRggBogen = 164;
faRggKoppel = 165;
faRggHull = 166;
faRggZoomIn = 167;
faRggZoomOut = 168;
faToggleSalingGraph = 169;
faToggleControllerGraph = 170;
faToggleChartGraph = 171;
faToggleKraftGraph = 172;
faToggleMatrixText = 173;

{ RggSegment }
faToggleSegmentF = 174;
faToggleSegmentR = 175;
faToggleSegmentS = 176;
faToggleSegmentM = 177;
faToggleSegmentV = 178;
faToggleSegmentW = 179;
faToggleSegmentC = 180;
faToggleSegmentA = 181;

{ RggRenderOptions }
faWantRenderH = 182;
faWantRenderP = 183;
faWantRenderF = 184;
faWantRenderE = 185;
faWantRenderS = 186;

{ RggTrimms }
faTrimm0 = 187;
faTrimm1 = 188;
faTrimm2 = 189;
faTrimm3 = 190;
faTrimm4 = 191;
faTrimm5 = 192;
faTrimm6 = 193;
fa420 = 194;
faLogo = 195;

{ RggTrimmFile }
faCopyTrimmItem = 196;
faPasteTrimmItem = 197;
faCopyAndPaste = 198;
faUpdateTrimm0 = 199;
faReadTrimmFile = 200;
faSaveTrimmFile = 201;
faCopyTrimmFile = 202;

{ RggTrimmText }
faToggleTrimmText = 203;
faToggleDiffText = 204;
faToggleDataText = 205;
faToggleDebugText = 206;
faUpdateReportText = 207;

{ RggSonstiges }
faMemeToggleHelp = 208;
faMemeToggleReport = 209;
faButtonFrameReport = 210;
faToggleFontColor = 211;
faToggleSandboxed = 212;
faToggleSpeedPanel = 213;
faToggleAllProps = 214;
faToggleAllTags = 215;

{ BtnLegendTablet }
faTL01 = 216;
faTL02 = 217;
faTL03 = 218;
faTL04 = 219;
faTL05 = 220;
faTL06 = 221;
faTR01 = 222;
faTR02 = 223;
faTR03 = 224;
faTR04 = 225;
faTR05 = 226;
faTR06 = 227;
faTR07 = 228;
faTR08 = 229;
faBL01 = 230;
faBL02 = 231;
faBL03 = 232;
faBL04 = 233;
faBL05 = 234;
faBL06 = 235;
faBL07 = 236;
faBL08 = 237;
faBR01 = 238;
faBR02 = 239;
faBR03 = 240;
faBR04 = 241;
faBR05 = 242;
faBR06 = 243;

{ BtnLegendPhone }
faMB01 = 244;
faMB02 = 245;
faMB03 = 246;
faMB04 = 247;
faMB05 = 248;
faMB06 = 249;
faMB07 = 250;
faMB08 = 251;

{ Circles }
faCirclesSelectC0 = 252;
faCirclesSelectC1 = 253;
faCirclesSelectC2 = 254;
faCircleParamR1 = 255;
faCircleParamR2 = 256;
faCircleParamM1X = 257;
faCircleParamM1Y = 258;
faCircleParamM2X = 259;
faCircleParamM2Y = 260;
faLineParamA1 = 261;
faLineParamA2 = 262;
faLineParamE1 = 263;
faLineParamE2 = 264;
faCircleParamM1Z = 265;
faCircleParamM2Z = 266;
faCirclesReset = 267;

{ MemeFormat }
faMemeGotoLandscape = 268;
faMemeGotoSquare = 269;
faMemeGotoPortrait = 270;
faMemeFormat0 = 271;
faMemeFormat1 = 272;
faMemeFormat2 = 273;
faMemeFormat3 = 274;
faMemeFormat4 = 275;
faMemeFormat5 = 276;
faMemeFormat6 = 277;
faMemeFormat7 = 278;
faMemeFormat8 = 279;
faMemeFormat9 = 280;

faMax = 281;

ParamsRange = [
//  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
