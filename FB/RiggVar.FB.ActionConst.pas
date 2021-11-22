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

const
  // --- generated code snippet ---

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
  faToggleSortedRota = 161;
  faRggBogen = 162;
  faRggKoppel = 163;
  faRggHull = 164;
  faRggZoomIn = 165;
  faRggZoomOut = 166;
  faToggleSalingGraph = 167;
  faToggleControllerGraph = 168;
  faToggleChartGraph = 169;
  faToggleKraftGraph = 170;
  faToggleMatrixText = 171;

  { RggSegment }
  faToggleSegmentF = 172;
  faToggleSegmentR = 173;
  faToggleSegmentS = 174;
  faToggleSegmentM = 175;
  faToggleSegmentV = 176;
  faToggleSegmentW = 177;
  faToggleSegmentC = 178;
  faToggleSegmentA = 179;

  { RggRenderOptions }
  faWantRenderH = 180;
  faWantRenderP = 181;
  faWantRenderF = 182;
  faWantRenderE = 183;
  faWantRenderS = 184;

  { RggTrimms }
  faTrimm0 = 185;
  faTrimm1 = 186;
  faTrimm2 = 187;
  faTrimm3 = 188;
  faTrimm4 = 189;
  faTrimm5 = 190;
  faTrimm6 = 191;
  fa420 = 192;
  faLogo = 193;

  { RggTrimmFile }
  faCopyTrimmItem = 194;
  faPasteTrimmItem = 195;
  faCopyAndPaste = 196;
  faUpdateTrimm0 = 197;
  faReadTrimmFile = 198;
  faSaveTrimmFile = 199;
  faCopyTrimmFile = 200;

  { RggTrimmText }
  faToggleTrimmText = 201;
  faToggleDiffText = 202;
  faToggleDataText = 203;
  faToggleDebugText = 204;
  faUpdateReportText = 205;

  { RggSonstiges }
  faToggleHelp = 206;
  faToggleReport = 207;
  faToggleButtonReport = 208;
  faToggleDarkMode = 209;
  faToggleButtonSize = 210;
  faToggleSandboxed = 211;
  faToggleSpeedPanel = 212;
  faToggleAllProps = 213;
  faToggleAllTags = 214;

  { RggInfo }
  faShowHelpText = 215;
  faShowInfoText = 216;
  faShowNormalKeyInfo = 217;
  faShowSpecialKeyInfo = 218;
  faShowDebugInfo = 219;
  faShowZOrderInfo = 220;

  { BtnLegendTablet }
  faTL01 = 221;
  faTL02 = 222;
  faTL03 = 223;
  faTL04 = 224;
  faTL05 = 225;
  faTL06 = 226;
  faTR01 = 227;
  faTR02 = 228;
  faTR03 = 229;
  faTR04 = 230;
  faTR05 = 231;
  faTR06 = 232;
  faTR07 = 233;
  faTR08 = 234;
  faBL01 = 235;
  faBL02 = 236;
  faBL03 = 237;
  faBL04 = 238;
  faBL05 = 239;
  faBL06 = 240;
  faBL07 = 241;
  faBL08 = 242;
  faBR01 = 243;
  faBR02 = 244;
  faBR03 = 245;
  faBR04 = 246;
  faBR05 = 247;
  faBR06 = 248;

  { BtnLegendPhone }
  faMB01 = 249;
  faMB02 = 250;
  faMB03 = 251;
  faMB04 = 252;
  faMB05 = 253;
  faMB06 = 254;
  faMB07 = 255;
  faMB08 = 256;

  { TouchBarLegend }
  faTouchBarTop = 257;
  faTouchBarBottom = 258;
  faTouchBarLeft = 259;
  faTouchBarRight = 260;

  { Circles }
  faCirclesSelectC0 = 261;
  faCirclesSelectC1 = 262;
  faCirclesSelectC2 = 263;
  faCircleParamR1 = 264;
  faCircleParamR2 = 265;
  faCircleParamM1X = 266;
  faCircleParamM1Y = 267;
  faCircleParamM2X = 268;
  faCircleParamM2Y = 269;
  faLineParamA1 = 270;
  faLineParamA2 = 271;
  faLineParamE1 = 272;
  faLineParamE2 = 273;
  faCircleParamM1Z = 274;
  faCircleParamM2Z = 275;
  faCirclesReset = 276;

  { MemeFormat }
  faMemeGotoLandscape = 277;
  faMemeGotoSquare = 278;
  faMemeGotoPortrait = 279;
  faMemeFormat0 = 280;
  faMemeFormat1 = 281;
  faMemeFormat2 = 282;
  faMemeFormat3 = 283;
  faMemeFormat4 = 284;
  faMemeFormat5 = 285;
  faMemeFormat6 = 286;
  faMemeFormat7 = 287;
  faMemeFormat8 = 288;
  faMemeFormat9 = 289;

  { Reset }
  faReset = 290;
  faResetPosition = 291;
  faResetRotation = 292;
  faResetZoom = 293;

  { ViewType }
  faToggleViewType = 294;
  faViewTypeOrtho = 295;
  faViewTypePerspective = 296;

  { DropTarget }
  faToggleDropTarget = 297;

  { Language }
  faToggleLanguage = 298;

  { CopyPaste }
  faSave = 299;
  faLoad = 300;
  faOpen = 301;
  faCopy = 302;
  faPaste = 303;
  faShare = 304;

  { ViewOptions }
  faToggleMoveMode = 305;
  faLinearMove = 306;
  faExpoMove = 307;

  { HullMesh }
  faHullMesh = 308;
  faHullMeshOn = 309;
  faHullMeshOff = 310;

  { BitmapCycle }
  faCycleBitmapM = 311;
  faCycleBitmapP = 312;
  faRandom = 313;
  faRandomWhite = 314;
  faRandomBlack = 315;
  faBitmapEscape = 316;
  faToggleContour = 317;

  faMax = 318;

  ParamsRange = [
  faParamT1 .. faParamT2,
  faController .. faParamAPW,
  faParamEAH .. faParamEI];

  ReportsRange = [faReportNone .. faReportReadme];
  TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
