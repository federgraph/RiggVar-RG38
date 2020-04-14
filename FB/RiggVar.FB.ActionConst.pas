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

{ Forms }
faShowActi = 3;
faShowMemo = 4;

{ TouchLayout }
faTouchTablet = 5;
faTouchPhone = 6;
faTouchDesk = 7;

{ Format }
faMemeGotoLandscape = 8;
faMemeGotoSquare = 9;
faMemeGotoPortrait = 10;
faMemeFormat0 = 11;
faMemeFormat1 = 12;
faMemeFormat2 = 13;
faMemeFormat3 = 14;
faMemeFormat4 = 15;
faMemeFormat5 = 16;
faMemeFormat6 = 17;
faMemeFormat7 = 18;
faMemeFormat8 = 19;
faMemeFormat9 = 20;

{ Wheel }
faWheelLeft = 21;
faWheelRight = 22;
faWheelDown = 23;
faWheelUp = 24;
faParamValuePlus1 = 25;
faParamValueMinus1 = 26;
faParamValuePlus10 = 27;
faParamValueMinus10 = 28;

{ ColorScheme }
faCycleColorSchemeM = 29;
faCycleColorSchemeP = 30;

{ ViewParams }
faPan = 31;

{ ViewFlags }
faToggleAllText = 32;
faToggleTouchFrame = 33;

{ ViewType }
faToggleViewType = 34;

{ Texture Param }
faParamT1 = 35;
faParamT2 = 36;

{ RggControls }
faController = 37;
faWinkel = 38;
faVorstag = 39;
faWante = 40;
faWoben = 41;
faSalingH = 42;
faSalingA = 43;
faSalingL = 44;
faSalingW = 45;
faMastfallF0C = 46;
faMastfallF0F = 47;
faMastfallVorlauf = 48;
faBiegung = 49;
faMastfussD0X = 50;
faVorstagOS = 51;
faWPowerOS = 52;

{ RggFixPoints }
faFixpointA0 = 53;
faFixpointA = 54;
faFixpointB0 = 55;
faFixpointB = 56;
faFixpointC0 = 57;
faFixpointC = 58;
faFixpointD0 = 59;
faFixpointD = 60;
faFixpointE0 = 61;
faFixpointE = 62;
faFixpointF0 = 63;
faFixpointF = 64;

{ RggViewPoint }
faViewpointS = 65;
faViewpointA = 66;
faViewpointT = 67;
faViewpoint3 = 68;

{ RggSalingType }
faSalingTypOhne = 69;
faSalingTypDrehbar = 70;
faSalingTypFest = 71;
faSalingTypOhneStarr = 72;

{ RggAppMode }
faDemo = 73;
faSofortBtn = 74;
faGrauBtn = 75;
faBlauBtn = 76;
faMultiBtn = 77;
faMemoryBtn = 78;
faKoppelBtn = 79;
faHull = 80;

{ RggReport }
faReportNone = 81;
faReportLog = 82;
faReportJson = 83;
faReportData = 84;
faReportTrimmText = 85;
faReportJsonText = 86;
faReportDataText = 87;
faReportDiffText = 88;
faReportAusgabeRL = 89;
faReportAusgabeRP = 90;
faReportAusgabeRLE = 91;
faReportAusgabeRPE = 92;
faReportAusgabeDiffL = 93;
faReportAusgabeDiffP = 94;
faReportXML = 95;
faReportDebugReport = 96;
faReportReadme = 97;

{ RggChart }
faChartRect = 98;
faChartTextRect = 99;
faChartLegend = 100;
faChartAP = 101;
faChartBP = 102;
faChartGroup = 103;
faParamCountPlus = 104;
faParamCountMinus = 105;
faPComboPlus = 106;
faPComboMinus = 107;
faXComboPlus = 108;
faXComboMinus = 109;
faYComboPlus = 110;
faYComboMinus = 111;
faChartReset = 112;

{ RggGraph }
faToggleLineColor = 113;
faToggleUseDisplayList = 114;
faToggleUseQuickSort = 115;
faToggleShowLegend = 116;
faRggBogen = 117;
faRggZoomIn = 118;
faRggZoomOut = 119;
faToggleSalingGraph = 120;
faToggleControllerGraph = 121;
faToggleChartGraph = 122;
faToggleMatrixText = 123;

{ RggSegment }
faToggleSegmentF = 124;
faToggleSegmentR = 125;
faToggleSegmentS = 126;
faToggleSegmentM = 127;
faToggleSegmentV = 128;
faToggleSegmentW = 129;
faToggleSegmentC = 130;
faToggleSegmentA = 131;

{ RggRenderOptions }
faWantRenderH = 132;
faWantRenderP = 133;
faWantRenderF = 134;
faWantRenderE = 135;
faWantRenderS = 136;

{ RggTrimms }
faTrimm0 = 137;
faTrimm1 = 138;
faTrimm2 = 139;
faTrimm3 = 140;
faTrimm4 = 141;
faTrimm5 = 142;
faTrimm6 = 143;
fa420 = 144;
faLogo = 145;

{ RggTrimmFile }
faCopyTrimmItem = 146;
faPasteTrimmItem = 147;
faCopyAndPaste = 148;
faUpdateTrimm0 = 149;
faReadTrimmFile = 150;
faSaveTrimmFile = 151;
faCopyTrimmFile = 152;

{ RggTrimmText }
faToggleTrimmText = 153;
faToggleDiffText = 154;
faToggleDataText = 155;
faToggleDebugText = 156;
faUpdateReportText = 157;

{ RggSonstiges }
faMemeToggleHelp = 158;
faMemeToggleReport = 159;
faButtonFrameReport = 160;
faToggleFontColor = 161;
faToggleSandboxed = 162;
faToggleAllProps = 163;
faToggleAllTags = 164;

{ BtnLegendTablet }
faTL01 = 165;
faTL02 = 166;
faTL03 = 167;
faTL04 = 168;
faTL05 = 169;
faTL06 = 170;
faTR01 = 171;
faTR02 = 172;
faTR03 = 173;
faTR04 = 174;
faTR05 = 175;
faTR06 = 176;
faTR07 = 177;
faTR08 = 178;
faBL01 = 179;
faBL02 = 180;
faBL03 = 181;
faBL04 = 182;
faBL05 = 183;
faBL06 = 184;
faBL07 = 185;
faBL08 = 186;
faBR01 = 187;
faBR02 = 188;
faBR03 = 189;
faBR04 = 190;
faBR05 = 191;
faBR06 = 192;

{ BtnLegendPhone }
faMB01 = 193;
faMB02 = 194;
faMB03 = 195;
faMB04 = 196;
faMB05 = 197;
faMB06 = 198;
faMB07 = 199;
faMB08 = 200;

{ Circles }
faCirclesSelectC0 = 201;
faCirclesSelectC1 = 202;
faCirclesSelectC2 = 203;
faCircleParamR1 = 204;
faCircleParamR2 = 205;
faCircleParamM1X = 206;
faCircleParamM1Y = 207;
faCircleParamM2X = 208;
faCircleParamM2Y = 209;
faLineParamA1 = 210;
faLineParamA2 = 211;
faLineParamE1 = 212;
faLineParamE2 = 213;
faCircleParamM1Z = 214;
faCircleParamM2Z = 215;
faCirclesReset = 216;

faMax = 217;

ParamsRange = [faController .. faMastfussD0X];
ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
