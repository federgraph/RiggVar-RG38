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
faSofortBtn = 79;
faGrauBtn = 80;
faBlauBtn = 81;
faMultiBtn = 82;
faMemoryBtn = 83;
faKoppelBtn = 84;
faHull = 85;

{ RggReport }
faReportNone = 86;
faReportLog = 87;
faReportJson = 88;
faReportData = 89;
faReportShort = 90;
faReportLong = 91;
faReportTrimmText = 92;
faReportJsonText = 93;
faReportDataText = 94;
faReportDiffText = 95;
faReportAusgabeRL = 96;
faReportAusgabeRP = 97;
faReportAusgabeRLE = 98;
faReportAusgabeRPE = 99;
faReportAusgabeDiffL = 100;
faReportAusgabeDiffP = 101;
faReportXML = 102;
faReportDebugReport = 103;
faReportReadme = 104;

{ RggChart }
faChartRect = 105;
faChartTextRect = 106;
faChartLegend = 107;
faChartAP = 108;
faChartBP = 109;
faChartGroup = 110;
faParamCountPlus = 111;
faParamCountMinus = 112;
faPComboPlus = 113;
faPComboMinus = 114;
faXComboPlus = 115;
faXComboMinus = 116;
faYComboPlus = 117;
faYComboMinus = 118;
faChartReset = 119;

{ RggGraph }
faToggleLineColor = 120;
faToggleUseDisplayList = 121;
faToggleUseQuickSort = 122;
faToggleShowLegend = 123;
faRggBogen = 124;
faRggZoomIn = 125;
faRggZoomOut = 126;
faToggleSalingGraph = 127;
faToggleControllerGraph = 128;
faToggleChartGraph = 129;
faToggleMatrixText = 130;

{ RggSegment }
faToggleSegmentF = 131;
faToggleSegmentR = 132;
faToggleSegmentS = 133;
faToggleSegmentM = 134;
faToggleSegmentV = 135;
faToggleSegmentW = 136;
faToggleSegmentC = 137;
faToggleSegmentA = 138;

{ RggRenderOptions }
faWantRenderH = 139;
faWantRenderP = 140;
faWantRenderF = 141;
faWantRenderE = 142;
faWantRenderS = 143;

{ RggTrimms }
faTrimm0 = 144;
faTrimm1 = 145;
faTrimm2 = 146;
faTrimm3 = 147;
faTrimm4 = 148;
faTrimm5 = 149;
faTrimm6 = 150;
fa420 = 151;
faLogo = 152;

{ RggTrimmFile }
faCopyTrimmItem = 153;
faPasteTrimmItem = 154;
faCopyAndPaste = 155;
faUpdateTrimm0 = 156;
faReadTrimmFile = 157;
faSaveTrimmFile = 158;
faCopyTrimmFile = 159;

{ RggTrimmText }
faToggleTrimmText = 160;
faToggleDiffText = 161;
faToggleDataText = 162;
faToggleDebugText = 163;
faUpdateReportText = 164;

{ RggSonstiges }
faMemeToggleHelp = 165;
faMemeToggleReport = 166;
faButtonFrameReport = 167;
faToggleFontColor = 168;
faToggleSandboxed = 169;
faToggleAllProps = 170;
faToggleAllTags = 171;

{ BtnLegendTablet }
faTL01 = 172;
faTL02 = 173;
faTL03 = 174;
faTL04 = 175;
faTL05 = 176;
faTL06 = 177;
faTR01 = 178;
faTR02 = 179;
faTR03 = 180;
faTR04 = 181;
faTR05 = 182;
faTR06 = 183;
faTR07 = 184;
faTR08 = 185;
faBL01 = 186;
faBL02 = 187;
faBL03 = 188;
faBL04 = 189;
faBL05 = 190;
faBL06 = 191;
faBL07 = 192;
faBL08 = 193;
faBR01 = 194;
faBR02 = 195;
faBR03 = 196;
faBR04 = 197;
faBR05 = 198;
faBR06 = 199;

{ BtnLegendPhone }
faMB01 = 200;
faMB02 = 201;
faMB03 = 202;
faMB04 = 203;
faMB05 = 204;
faMB06 = 205;
faMB07 = 206;
faMB08 = 207;

{ Circles }
faCirclesSelectC0 = 208;
faCirclesSelectC1 = 209;
faCirclesSelectC2 = 210;
faCircleParamR1 = 211;
faCircleParamR2 = 212;
faCircleParamM1X = 213;
faCircleParamM1Y = 214;
faCircleParamM2X = 215;
faCircleParamM2Y = 216;
faLineParamA1 = 217;
faLineParamA2 = 218;
faLineParamE1 = 219;
faLineParamE2 = 220;
faCircleParamM1Z = 221;
faCircleParamM2Z = 222;
faCirclesReset = 223;

{ Format }
faMemeGotoLandscape = 224;
faMemeGotoSquare = 225;
faMemeGotoPortrait = 226;
faMemeFormat0 = 227;
faMemeFormat1 = 228;
faMemeFormat2 = 229;
faMemeFormat3 = 230;
faMemeFormat4 = 231;
faMemeFormat5 = 232;
faMemeFormat6 = 233;
faMemeFormat7 = 234;
faMemeFormat8 = 235;
faMemeFormat9 = 236;

faMax = 237;

(*

{ ViewType }
faToggleViewType = 237;
faViewTypeOrtho = 238;
faViewTypePerspective = 239;

{ ViewFlags }
faToggleBMap = 240;
faToggleZoom = 241;
faToggleMapK = 242;
faMapKOn = 243;
faMapKOff = 244;
faToggleTouchMenu = 245;
faToggleEquationText = 246;
faTogglePrimeText = 247;
faToggleSecondText = 248;
faToggleLabelText = 249;
faLabelBatchM = 250;
faLabelBatchP = 251;
faLabelTextP = 252;
faLabelTextM = 253;

{ Texture Param }
faParamT1 = 254;
faParamT2 = 255;
faParamT3 = 256;
faParamT4 = 257;

{ LastLine }
faELLOn = 258;
faELLOff = 259;

{ Help }
faCycleHelpM = 260;
faCycleHelpP = 261;
faHelpCycle = 262;
faHelpList = 263;
faHelpHome = 264;
faToggleLanguage = 265;

{ Format }
faFormatLandscape = 266;
faFormatPortrait = 267;
faFormatIPhoneLandscape = 268;
faFormatIPhonePortrait = 269;

{ IconSize }
faIconSize016 = 270;
faIconSize032 = 271;
faIconSize048 = 272;
faIconSize064 = 273;
faIconSize096 = 274;
faIconSize128 = 275;
faIconSize256 = 276;
faIconSize512 = 277;
faIconSize640 = 278;
faIconSize960 = 279;
faIconSize01K = 280;

{ ViewOptions }
faToggleMoveMode = 281;
faLinearMove = 282;
faExpoMove = 283;

{ Reset }
faReset = 284;
faResetPosition = 285;
faResetRotation = 286;
faResetZoom = 287;

{ BitmapCycle }
faCycleBitmapM = 288;
faCycleBitmapP = 289;
faRandom = 290;
faRandomWhite = 291;
faRandomBlack = 292;
faBitmapEscape = 293;
faToggleContour = 294;

{ CopyPaste }
faSave = 295;
faLoad = 296;
faOpen = 297;
faCopy = 298;
faPaste = 299;
faShare = 300;

{ CopyImage }
faCopyScreenshot = 301;
faCopyBitmap = 302;
faCopyBitmap3D = 303;

{ CopyOptions }
faToggleHardCopy = 304;
faHardCopyOn = 305;
faHardCopyOff = 306;
faTogglePngCopy = 307;
faPngCopyOn = 308;
faPngCopyOff = 309;
faToggleNoCopy = 310;
faNoCopyOn = 311;
faNoCopyOff = 312;

{ Input }
faToggleDropTarget = 313;

{ RggHullMesh }
faHullMesh = 314;
faHullMeshOn = 315;
faHullMeshOff = 316;

faMax = 317;
*)


ParamsRange = [faController .. faMastfussD0X];
ReportsRange = [faReportNone .. faReportReadme];
TrimmsRange = [faTrimm0 .. faLogo];

implementation

end.
