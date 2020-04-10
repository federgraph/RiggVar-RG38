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
// Note that some of the defined actions
//   may not be implemented in this version of the app.


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
faReportDataText = 86;
faReportDiffText = 87;
faReportAusgabeRL = 88;
faReportAusgabeRP = 89;
faReportAusgabeRLE = 90;
faReportAusgabeRPE = 91;
faReportAusgabeDiffL = 92;
faReportAusgabeDiffP = 93;
faReportXML = 94;
faReportDebugReport = 95;
faReportReadme = 96;

{ RggChart }
faChartRect = 97;
faChartTextRect = 98;
faChartLegend = 99;
faChartAP = 100;
faChartBP = 101;
faChartGroup = 102;
faParamCountPlus = 103;
faParamCountMinus = 104;
faPComboPlus = 105;
faPComboMinus = 106;
faXComboPlus = 107;
faXComboMinus = 108;
faYComboPlus = 109;
faYComboMinus = 110;
faChartReset = 111;

{ RggGraph }
faToggleLineColor = 112;
faToggleUseDisplayList = 113;
faToggleUseQuickSort = 114;
faToggleShowLegend = 115;
faRggBogen = 116;
faRggZoomIn = 117;
faRggZoomOut = 118;
faToggleSalingGraph = 119;
faToggleControllerGraph = 120;
faToggleChartGraph = 121;
faToggleMatrixText = 122;

{ RggSegment }
faToggleSegmentF = 123;
faToggleSegmentR = 124;
faToggleSegmentS = 125;
faToggleSegmentM = 126;
faToggleSegmentV = 127;
faToggleSegmentW = 128;
faToggleSegmentC = 129;
faToggleSegmentA = 130;

{ RggRenderOptions }
faWantRenderH = 131;
faWantRenderP = 132;
faWantRenderF = 133;
faWantRenderE = 134;
faWantRenderS = 135;

{ RggTrimms }
faTrimm0 = 136;
faTrimm1 = 137;
faTrimm2 = 138;
faTrimm3 = 139;
faTrimm4 = 140;
faTrimm5 = 141;
faTrimm6 = 142;
fa420 = 143;
faLogo = 144;

{ RggTrimmFile }
faCopyTrimmItem = 145;
faPasteTrimmItem = 146;
faCopyAndPaste = 147;
faUpdateTrimm0 = 148;
faReadTrimmFile = 149;
faSaveTrimmFile = 150;
faCopyTrimmFile = 151;

{ RggTrimmText }
faToggleTrimmText = 152;
faToggleDiffText = 153;
faToggleDataText = 154;
faToggleDebugText = 155;
faUpdateReportText = 156;

{ RggSonstiges }
faMemeToggleHelp = 157;
faMemeToggleReport = 158;
faButtonFrameReport = 159;
faToggleFontColor = 160;
faToggleSandboxed = 161;
faToggleAllTags = 162;

faTL01 = 163;
faTL02 = 164;
faTL03 = 165;
faTL04 = 166;
faTL05 = 167;
faTL06 = 168;

faTR01 = 169;
faTR02 = 170;
faTR03 = 171;
faTR04 = 172;
faTR05 = 173;
faTR06 = 174;
faTR07 = 175;
faTR08 = 176;

faBL01 = 177;
faBL02 = 178;
faBL03 = 179;
faBL04 = 180;
faBL05 = 181;
faBL06 = 182;
faBL07 = 183;
faBL08 = 184;

faBR01 = 185;
faBR02 = 186;
faBR03 = 187;
faBR04 = 188;
faBR05 = 189;
faBR06 = 190;

faMB01 = 191;
faMB02 = 192;
faMB03 = 193;
faMB04 = 194;
faMB05 = 195;
faMB06 = 196;
faMB07 = 197;
faMB08 = 198;

faMax = 199;

ParamsRange = [faController .. faMastfussD0X];
ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
