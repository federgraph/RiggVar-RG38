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
  faParamValuePlus1 = 38;
  faParamValueMinus1 = 39;
  faParamValuePlus10 = 40;
  faParamValueMinus10 = 41;
  faWheelLeft = 42;
  faWheelRight = 43;
  faWheelDown = 44;
  faWheelUp = 45;

  { ColorScheme }
  faCycleColorSchemeM = 46;
  faCycleColorSchemeP = 47;

  { FederText }
  faToggleAllText = 48;
  faToggleTouchFrame = 49;

  { ViewParams }
  faPan = 50;
  faParamORX = 51;
  faParamORY = 52;
  faParamORZ = 53;
  faParamRX = 54;
  faParamRY = 55;
  faParamRZ = 56;
  faParamCZ = 57;

  { ParamT }
  faParamT1 = 58;
  faParamT2 = 59;
  faParamT3 = 60;
  faParamT4 = 61;

  { Help }
  faToggleHelp = 62;
  faToggleReport = 63;
  faToggleButtonReport = 64;

  { RggControls }
  faController = 65;
  faWinkel = 66;
  faVorstag = 67;
  faWante = 68;
  faWoben = 69;
  faSalingH = 70;
  faSalingA = 71;
  faSalingL = 72;
  faSalingW = 73;
  faMastfallF0C = 74;
  faMastfallF0F = 75;
  faMastfallVorlauf = 76;
  faBiegung = 77;
  faMastfussD0X = 78;
  faVorstagOS = 79;
  faWPowerOS = 80;
  faParamAPW = 81;
  faParamEAH = 82;
  faParamEAR = 83;
  faParamEI = 84;

  { RggFixPoints }
  faFixpointA0 = 85;
  faFixpointA = 86;
  faFixpointB0 = 87;
  faFixpointB = 88;
  faFixpointC0 = 89;
  faFixpointC = 90;
  faFixpointD0 = 91;
  faFixpointD = 92;
  faFixpointE0 = 93;
  faFixpointE = 94;
  faFixpointF0 = 95;
  faFixpointF = 96;

  { RggViewPoint }
  faViewpointS = 97;
  faViewpointA = 98;
  faViewpointT = 99;
  faViewpoint3 = 100;

  { RggSalingType }
  faSalingTypFest = 101;
  faSalingTypDrehbar = 102;
  faSalingTypOhne = 103;
  faSalingTypOhneStarr = 104;

  { RggCalcType }
  faCalcTypQuer = 105;
  faCalcTypKnick = 106;
  faCalcTypGemessen = 107;

  { RggAppMode }
  faDemo = 108;
  faMemoryBtn = 109;
  faMemoryRecallBtn = 110;
  faKorrigiertItem = 111;
  faSofortBtn = 112;
  faGrauBtn = 113;
  faBlauBtn = 114;
  faMultiBtn = 115;

  { RggSuper }
  faSuperSimple = 116;
  faSuperNormal = 117;
  faSuperGrau = 118;
  faSuperBlau = 119;
  faSuperMulti = 120;
  faSuperDisplay = 121;
  faSuperQuick = 122;

  { RggReport }
  faReportNone = 123;
  faReportLog = 124;
  faReportJson = 125;
  faReportData = 126;
  faReportShort = 127;
  faReportLong = 128;
  faReportTrimmText = 129;
  faReportJsonText = 130;
  faReportDataText = 131;
  faReportDiffText = 132;
  faReportAusgabeDetail = 133;
  faReportAusgabeRL = 134;
  faReportAusgabeRP = 135;
  faReportAusgabeRLE = 136;
  faReportAusgabeRPE = 137;
  faReportAusgabeDiffL = 138;
  faReportAusgabeDiffP = 139;
  faReportXML = 140;
  faReportDebugReport = 141;
  faReportReadme = 142;

  { RggChart }
  faChartRect = 143;
  faChartTextRect = 144;
  faChartLegend = 145;
  faChartAP = 146;
  faChartBP = 147;
  faChartGroup = 148;
  faParamCountPlus = 149;
  faParamCountMinus = 150;
  faPComboPlus = 151;
  faPComboMinus = 152;
  faXComboPlus = 153;
  faXComboMinus = 154;
  faYComboPlus = 155;
  faYComboMinus = 156;
  faChartReset = 157;

  { RggGraph }
  faToggleLineColor = 158;
  faToggleUseDisplayList = 159;
  faToggleUseQuickSort = 160;
  faToggleShowLegend = 161;
  faToggleSortedRota = 162;
  faRggBogen = 163;
  faRggKoppel = 164;
  faRggHull = 165;
  faRggZoomIn = 166;
  faRggZoomOut = 167;
  faToggleSalingGraph = 168;
  faToggleControllerGraph = 169;
  faToggleChartGraph = 170;
  faToggleKraftGraph = 171;
  faToggleMatrixText = 172;

  { RggSegment }
  faToggleSegmentF = 173;
  faToggleSegmentR = 174;
  faToggleSegmentS = 175;
  faToggleSegmentM = 176;
  faToggleSegmentV = 177;
  faToggleSegmentW = 178;
  faToggleSegmentC = 179;
  faToggleSegmentA = 180;

  { RggRenderOptions }
  faWantRenderH = 181;
  faWantRenderP = 182;
  faWantRenderF = 183;
  faWantRenderE = 184;
  faWantRenderS = 185;

  { RggTrimms }
  faTrimm0 = 186;
  faTrimm1 = 187;
  faTrimm2 = 188;
  faTrimm3 = 189;
  faTrimm4 = 190;
  faTrimm5 = 191;
  faTrimm6 = 192;
  fa420 = 193;
  faLogo = 194;

  { RggTrimmFile }
  faCopyTrimmItem = 195;
  faPasteTrimmItem = 196;
  faCopyAndPaste = 197;
  faUpdateTrimm0 = 198;
  faReadTrimmFile = 199;
  faSaveTrimmFile = 200;
  faCopyTrimmFile = 201;

  { RggTrimmText }
  faToggleTrimmText = 202;
  faToggleDiffText = 203;
  faToggleDataText = 204;
  faToggleDebugText = 205;
  faUpdateReportText = 206;

  { RggSonstiges }
  faToggleDarkMode = 207;
  faToggleButtonSize = 208;
  faToggleSandboxed = 209;
  faToggleSpeedPanel = 210;
  faToggleAllProps = 211;
  faToggleAllTags = 212;

  { RggInfo }
  faShowHelpText = 213;
  faShowInfoText = 214;
  faShowNormalKeyInfo = 215;
  faShowSpecialKeyInfo = 216;
  faShowDebugInfo = 217;
  faShowZOrderInfo = 218;

  { BtnLegendTablet }
  faTL01 = 219;
  faTL02 = 220;
  faTL03 = 221;
  faTL04 = 222;
  faTL05 = 223;
  faTL06 = 224;
  faTR01 = 225;
  faTR02 = 226;
  faTR03 = 227;
  faTR04 = 228;
  faTR05 = 229;
  faTR06 = 230;
  faTR07 = 231;
  faTR08 = 232;
  faBL01 = 233;
  faBL02 = 234;
  faBL03 = 235;
  faBL04 = 236;
  faBL05 = 237;
  faBL06 = 238;
  faBL07 = 239;
  faBL08 = 240;
  faBR01 = 241;
  faBR02 = 242;
  faBR03 = 243;
  faBR04 = 244;
  faBR05 = 245;
  faBR06 = 246;

  { BtnLegendPhone }
  faMB01 = 247;
  faMB02 = 248;
  faMB03 = 249;
  faMB04 = 250;
  faMB05 = 251;
  faMB06 = 252;
  faMB07 = 253;
  faMB08 = 254;

  { TouchBarLegend }
  faTouchBarTop = 255;
  faTouchBarBottom = 256;
  faTouchBarLeft = 257;
  faTouchBarRight = 258;

  { Circles }
  faCirclesSelectC0 = 259;
  faCirclesSelectC1 = 260;
  faCirclesSelectC2 = 261;
  faCircleParamR1 = 262;
  faCircleParamR2 = 263;
  faCircleParamM1X = 264;
  faCircleParamM1Y = 265;
  faCircleParamM2X = 266;
  faCircleParamM2Y = 267;
  faLineParamA1 = 268;
  faLineParamA2 = 269;
  faLineParamE1 = 270;
  faLineParamE2 = 271;
  faCircleParamM1Z = 272;
  faCircleParamM2Z = 273;
  faCirclesReset = 274;

  { MemeFormat }
  faMemeGotoLandscape = 275;
  faMemeGotoSquare = 276;
  faMemeGotoPortrait = 277;
  faMemeFormat0 = 278;
  faMemeFormat1 = 279;
  faMemeFormat2 = 280;
  faMemeFormat3 = 281;
  faMemeFormat4 = 282;
  faMemeFormat5 = 283;
  faMemeFormat6 = 284;
  faMemeFormat7 = 285;
  faMemeFormat8 = 286;
  faMemeFormat9 = 287;

  { Reset }
  faReset = 288;
  faResetPosition = 289;
  faResetRotation = 290;
  faResetZoom = 291;

  { ViewType }
  faToggleViewType = 292;
  faViewTypeOrtho = 293;
  faViewTypePerspective = 294;

  { DropTarget }
  faToggleDropTarget = 295;

  { Language }
  faToggleLanguage = 296;

  { CopyPaste }
  faSave = 297;
  faLoad = 298;
  faOpen = 299;
  faCopy = 300;
  faPaste = 301;
  faShare = 302;

  { ViewOptions }
  faToggleMoveMode = 303;
  faLinearMove = 304;
  faExpoMove = 305;

  { HullMesh }
  faHullMesh = 306;
  faHullMeshOn = 307;
  faHullMeshOff = 308;

  { BitmapCycle }
  faCycleBitmapM = 309;
  faCycleBitmapP = 310;
  faRandom = 311;
  faRandomWhite = 312;
  faRandomBlack = 313;
  faBitmapEscape = 314;
  faToggleContour = 315;

  { Deprecated }
  faPlusOne = 316;
  faPlusTen = 317;

  faMax = 318;

implementation

end.
