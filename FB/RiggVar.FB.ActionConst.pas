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

  { Help }
  faToggleHelp = 58;
  faToggleReport = 59;
  faToggleButtonReport = 60;

  { RggControls }
  faController = 61;
  faWinkel = 62;
  faVorstag = 63;
  faWante = 64;
  faWoben = 65;
  faSalingH = 66;
  faSalingA = 67;
  faSalingL = 68;
  faSalingW = 69;
  faMastfallF0C = 70;
  faMastfallF0F = 71;
  faMastfallVorlauf = 72;
  faBiegung = 73;
  faMastfussD0X = 74;
  faVorstagOS = 75;
  faWPowerOS = 76;
  faParamAPW = 77;
  faParamEAH = 78;
  faParamEAR = 79;
  faParamEI = 80;

  { RggFixPoints }
  faFixpointA0 = 81;
  faFixpointA = 82;
  faFixpointB0 = 83;
  faFixpointB = 84;
  faFixpointC0 = 85;
  faFixpointC = 86;
  faFixpointD0 = 87;
  faFixpointD = 88;
  faFixpointE0 = 89;
  faFixpointE = 90;
  faFixpointF0 = 91;
  faFixpointF = 92;

  { RggViewPoint }
  faViewpointS = 93;
  faViewpointA = 94;
  faViewpointT = 95;
  faViewpoint3 = 96;

  { RggSalingType }
  faSalingTypFest = 97;
  faSalingTypDrehbar = 98;
  faSalingTypOhne = 99;
  faSalingTypOhneStarr = 100;

  { RggCalcType }
  faCalcTypQuer = 101;
  faCalcTypKnick = 102;
  faCalcTypGemessen = 103;

  { RggAppMode }
  faDemo = 104;
  faMemoryBtn = 105;
  faMemoryRecallBtn = 106;
  faKorrigiertItem = 107;
  faSofortBtn = 108;
  faGrauBtn = 109;
  faBlauBtn = 110;
  faMultiBtn = 111;

  { RggSuper }
  faSuperSimple = 112;
  faSuperNormal = 113;
  faSuperGrau = 114;
  faSuperBlau = 115;
  faSuperMulti = 116;
  faSuperDisplay = 117;
  faSuperQuick = 118;

  { RggReport }
  faReportNone = 119;
  faReportLog = 120;
  faReportJson = 121;
  faReportData = 122;
  faReportShort = 123;
  faReportLong = 124;
  faReportTrimmText = 125;
  faReportJsonText = 126;
  faReportDataText = 127;
  faReportDiffText = 128;
  faReportAusgabeDetail = 129;
  faReportAusgabeRL = 130;
  faReportAusgabeRP = 131;
  faReportAusgabeRLE = 132;
  faReportAusgabeRPE = 133;
  faReportAusgabeDiffL = 134;
  faReportAusgabeDiffP = 135;
  faReportXML = 136;
  faReportDebugReport = 137;
  faReportReadme = 138;

  { RggChart }
  faChartRect = 139;
  faChartTextRect = 140;
  faChartLegend = 141;
  faChartAP = 142;
  faChartBP = 143;
  faChartGroup = 144;
  faParamCountPlus = 145;
  faParamCountMinus = 146;
  faPComboPlus = 147;
  faPComboMinus = 148;
  faXComboPlus = 149;
  faXComboMinus = 150;
  faYComboPlus = 151;
  faYComboMinus = 152;
  faChartReset = 153;

  { RggGraph }
  faToggleLineColor = 154;
  faToggleUseDisplayList = 155;
  faToggleUseQuickSort = 156;
  faToggleShowLegend = 157;
  faToggleSortedRota = 158;
  faRggBogen = 159;
  faRggKoppel = 160;
  faRggHull = 161;
  faRggZoomIn = 162;
  faRggZoomOut = 163;
  faToggleSalingGraph = 164;
  faToggleControllerGraph = 165;
  faToggleChartGraph = 166;
  faToggleKraftGraph = 167;
  faToggleMatrixText = 168;

  { RggSegment }
  faToggleSegmentF = 169;
  faToggleSegmentR = 170;
  faToggleSegmentS = 171;
  faToggleSegmentM = 172;
  faToggleSegmentV = 173;
  faToggleSegmentW = 174;
  faToggleSegmentC = 175;
  faToggleSegmentA = 176;

  { RggRenderOptions }
  faWantRenderH = 177;
  faWantRenderP = 178;
  faWantRenderF = 179;
  faWantRenderE = 180;
  faWantRenderS = 181;

  { RggTrimms }
  faTrimm0 = 182;
  faTrimm1 = 183;
  faTrimm2 = 184;
  faTrimm3 = 185;
  faTrimm4 = 186;
  faTrimm5 = 187;
  faTrimm6 = 188;
  fa420 = 189;
  faLogo = 190;

  { RggTrimmFile }
  faCopyTrimmItem = 191;
  faPasteTrimmItem = 192;
  faCopyAndPaste = 193;
  faUpdateTrimm0 = 194;
  faReadTrimmFile = 195;
  faSaveTrimmFile = 196;
  faCopyTrimmFile = 197;

  { RggTrimmText }
  faToggleTrimmText = 198;
  faToggleDiffText = 199;
  faToggleDataText = 200;
  faToggleDebugText = 201;
  faUpdateReportText = 202;

  { RggSonstiges }
  faToggleDarkMode = 203;
  faToggleButtonSize = 204;
  faToggleSandboxed = 205;
  faToggleSpeedPanel = 206;
  faToggleAllProps = 207;
  faToggleAllTags = 208;

  { RggInfo }
  faShowHelpText = 209;
  faShowInfoText = 210;
  faShowNormalKeyInfo = 211;
  faShowSpecialKeyInfo = 212;
  faShowDebugInfo = 213;
  faShowZOrderInfo = 214;

  { BtnLegendTablet }
  faTL01 = 215;
  faTL02 = 216;
  faTL03 = 217;
  faTL04 = 218;
  faTL05 = 219;
  faTL06 = 220;
  faTR01 = 221;
  faTR02 = 222;
  faTR03 = 223;
  faTR04 = 224;
  faTR05 = 225;
  faTR06 = 226;
  faTR07 = 227;
  faTR08 = 228;
  faBL01 = 229;
  faBL02 = 230;
  faBL03 = 231;
  faBL04 = 232;
  faBL05 = 233;
  faBL06 = 234;
  faBL07 = 235;
  faBL08 = 236;
  faBR01 = 237;
  faBR02 = 238;
  faBR03 = 239;
  faBR04 = 240;
  faBR05 = 241;
  faBR06 = 242;

  { BtnLegendPhone }
  faMB01 = 243;
  faMB02 = 244;
  faMB03 = 245;
  faMB04 = 246;
  faMB05 = 247;
  faMB06 = 248;
  faMB07 = 249;
  faMB08 = 250;

  { TouchBarLegend }
  faTouchBarTop = 251;
  faTouchBarBottom = 252;
  faTouchBarLeft = 253;
  faTouchBarRight = 254;

  { Circles }
  faCirclesSelectC0 = 255;
  faCirclesSelectC1 = 256;
  faCirclesSelectC2 = 257;
  faCircleParamR1 = 258;
  faCircleParamR2 = 259;
  faCircleParamM1X = 260;
  faCircleParamM1Y = 261;
  faCircleParamM2X = 262;
  faCircleParamM2Y = 263;
  faLineParamA1 = 264;
  faLineParamA2 = 265;
  faLineParamE1 = 266;
  faLineParamE2 = 267;
  faCircleParamM1Z = 268;
  faCircleParamM2Z = 269;
  faCirclesReset = 270;

  { MemeFormat }
  faMemeGotoLandscape = 271;
  faMemeGotoSquare = 272;
  faMemeGotoPortrait = 273;
  faMemeFormat0 = 274;
  faMemeFormat1 = 275;
  faMemeFormat2 = 276;
  faMemeFormat3 = 277;
  faMemeFormat4 = 278;
  faMemeFormat5 = 279;
  faMemeFormat6 = 280;
  faMemeFormat7 = 281;
  faMemeFormat8 = 282;
  faMemeFormat9 = 283;

  { Reset }
  faReset = 284;
  faResetPosition = 285;
  faResetRotation = 286;
  faResetZoom = 287;

  { Language }
  faToggleLanguage = 288;

  { CopyPaste }
  faSave = 289;
  faLoad = 290;
  faOpen = 291;
  faCopy = 292;
  faPaste = 293;
  faShare = 294;

  faMax = 295;

implementation

end.
