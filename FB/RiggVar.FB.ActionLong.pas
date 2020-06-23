unit RiggVar.FB.ActionLong;

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

uses
  System.SysUtils,
  RiggVar.FB.ActionConst;

function GetFederActionLong(fa: TFederAction): string;

implementation

function GetFederActionLong(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    faNoop: result := 'Noop';

    faController: result := 'Controller';
    faWinkel: result := 'Winkel';
    faVorstag: result := 'Vorstag';
    faWante: result := 'Wante';
    faWoben: result := 'Wante oben';
    faSalingH: result := 'Saling Höhe';
    faSalingA: result := 'Saling Abstand';
    faSalingL: result := 'Saling Länge';
    faSalingW: result := 'Saling Winkel';
    faMastfallF0F: result := 'Mastfall F0F';
    faMastfallF0C: result := 'Mastfall F0C';
    faMastfallVorlauf: result := 'Mastfall Vorlauf';
    faBiegung: result := 'Biegung';
    faMastfussD0X: result := 'Mastfuss D0X';
    faVorstagOS: result := 'Vorstag OS';
    faWPowerOS: result := 'WP ower OS';

    faFixpointA0 : result := 'Fixpoint oA0';
    faFixpointA : result := 'Fixpoint oA';
    faFixpointB0 : result := 'Fixpoint oB0';
    faFixpointB : result := 'Fixpoint oB';
    faFixpointC0 : result := 'Fixpoint oC0';
    faFixpointC : result := 'Fixpoint oC';
    faFixpointD0 : result := 'Fixpoint oD0';
    faFixpointD : result := 'Fixpoint oD';
    faFixpointE0 : result := 'Fixpoint oE0';
    faFixpointE : result := 'Fixpoint oE';
    faFixpointF0 : result := 'Fixpoint oF0';
    faFixpointF : result := 'Fixpoint oF';

    faTrimm0: result := 'Trimm 0';
    faTrimm1: result := 'Trimm 1';
    faTrimm2: result := 'Trimm 2';
    faTrimm3: result := 'Trimm 3';
    faTrimm4: result := 'Trimm 4';
    faTrimm5: result := 'Trimm 5';
    faTrimm6: result := 'Trimm 6';
    fa420: result := 'Init 420'; //Trimm 7
    faLogo: result := 'Init Logo'; //Trimm 8

    faUpdateTrimm0: result := 'Update Trimm 0';
    faCopyAndPaste: result := 'Memory - Copy And Paste';
    faReadTrimmFile: result := 'Read Trimm File';
    faCopyTrimmFile: result := 'Copy Trimm File';
    faSaveTrimmFile: result := 'Save Trimm File';

    faSalingTypOhneStarr: result := 'Ohne Salinge Starr';
    faSalingTypOhne: result := 'Ohne Salinge';
    faSalingTypDrehbar: result := 'Drehbare Salinge';
    faSalingTypFest: result := 'Feste Salinge';

    faWantRenderH: result := 'Want render H (Hull-Tetraeder)';
    faWantRenderP: result := 'Want render P (Fachwerk)';
    faWantRenderF: result := 'Want render F (Mastfall)';
    faWantRenderE: result := 'Want render E (Kugeln E0-E)';
    faWantRenderS: result := 'Want render S (Stäbe)';

    faViewpointS: result := 'Viewpoint Seite';
    faViewpointA: result := 'Viewpoint Achtern';
    faViewpointT: result := 'Viewpoint Top';
    faViewpoint3: result := 'Viewpoint 3D';

    faToggleTrimmText: result := 'Toggle rgg trimm text';
    faToggleDataText: result := 'Toggle rgg data text';
    faToggleDiffText: result := 'Toggle rgg diff text';
    faToggleDebugText: result := 'Toggle debug text';
    faUpdateReportText: result := 'Update report text';

    faHull: result := 'Toggle visibility of hull';
    faDemo: result := 'Toggle Demo / Pro mode';
    faCopyTrimmItem: result := 'Copy Trimm-Item';
    faPasteTrimmItem: result := 'Paste Trimm-Item or Trimm-File';

    faShowMemo: result := 'Form Memo';
    faShowActi: result := 'Form Actions';
    faShowOpti: result := 'Form Options';
    faShowConf: result := 'Form Config';
    faShowChart: result := 'Form Chart';
    faShowDiag: result := 'Form Diagramm';
    faShowDiagC: result := 'Form Live Diagramm Controls';
    faShowDiagE: result := 'Form Diagramm Edits';
    faShowDiagQ: result := 'Form Diagramm Quick';
    faShowTrimmTab: result := 'Form Trimm Tab';

    faWheelLeft: result := 'Wheel -1';
    faWheelRight: result := 'Wheel +1';
    faWheelUp: result := 'Wheel -10';
    faWheelDown: result := 'Wheel +10';

    faActionPageM: result := 'Action Page Minus';
    faActionPageP: result := 'Action Page Plus';
    faActionPage1: result := 'Action Page 1';
    faActionPage2: result := 'Action Page 2';
    faActionPage3: result := 'Action Page 3';
    faActionPage4: result := 'Action Page 4';
    faActionPage5: result := 'Action Page 5';
    faActionPage6: result := 'Action Page 6';
    faActionPageE: result := 'Action Page E';
    faActionPageS: result := 'Action Page S';
    faActionPageX: result := 'Action Page X';

    faToggleAllText: result := 'Toggle all text';
    faToggleTouchFrame: result := 'Toggle Touch Frame';
    faToggleSpeedPanel: result := 'Toggle Speed Panel';

    faCycleColorSchemeM: result := 'cycle 3D color scheme -';
    faCycleColorSchemeP: result := 'cycle 3D color scheme +';

    faTouchDesk: result := 'Touch Desk';
    faTouchTablet: result := 'Touch Tablet';
    faTouchPhone: result := 'Touch Phone';

    faButtonFrameReport: result := 'Button Frame Report';

    faParamValuePlus1: result := 'Param Value + 1';
    faParamValueMinus1: result := 'Param Value - 1';
    faParamValuePlus10: result := 'Param Value + 10';
    faParamValueMinus10: result := 'Param Value - 10';

    faMemeToggleHelp: result := 'Toggle Help Text';
    faMemeToggleReport: result := 'Toggle Report';

    faMemeGotoLandscape: result := 'Goto Landscape';
    faMemeGotoSquare: result := 'Goto Square';
    faMemeGotoPortrait: result := 'Goto Portrait';

    faMemeFormat0: result := 'Format 0';
    faMemeFormat1: result := 'Format 1';
    faMemeFormat2: result := 'Format 2';
    faMemeFormat3: result := 'Format 3';
    faMemeFormat4: result := 'Format 4';
    faMemeFormat5: result := 'Format 5';
    faMemeFormat6: result := 'Format 6';
    faMemeFormat7: result := 'Format 7';
    faMemeFormat8: result := 'Format 8';
    faMemeFormat9: result := 'Format 9';

    faChartRect: result := 'Chart Show Rectangles';
    faChartTextRect: result := 'Chart Show Text border';
    faChartLegend: result := 'Chart Show Legend';
    faChartAP: result := 'Chart Range AP';
    faChartBP: result := 'Chart Range BP';
    faChartGroup: result := 'Chart Group';

    faParamCountPlus: result := 'Chart Param Count Plus';
    faParamCountMinus: result := 'Chart Param Count Minus';

    faPComboPlus: result := 'Chart P Combo Plus';
    faPComboMinus: result := 'Chart P Combo Minus';

    faXComboPlus: result := 'Chart X Combo Plus';
    faXComboMinus: result := 'Chart X Combo Minus';

    faYComboPlus: result := 'Chart Y Combo Plus';
    faYComboMinus: result := 'Chart Y Combo Minus';

    faChartReset: result := 'Chart Reset';

    faToggleFontColor: result := 'Toggle Font Color';

    faReportNone: result := 'Empty Report';
    faReportLog: result := 'Log Report';
    faReportJson: result := 'Json Report';
    faReportData: result := 'Data Report';
    faReportShort: result := 'Short Report';
    faReportLong: result := 'Long Report';
    faReportTrimmText: result := 'Trimm Text Report';
    faReportJsonText: result := 'Json Text Report';
    faReportDataText: result := 'Data Text Report';
    faReportDiffText: result := 'Diff Text Report';
    faReportAusgabeRL: result := 'Ausgabe Rigg Längen';
    faReportAusgabeRP: result := 'Ausgabe Rigg Koordinaten';
    faReportAusgabeRLE: result := 'Ausgabe Rigg Längen Entspannt';
    faReportAusgabeRPE: result := 'Ausabe Rigg Koordinaten Entspannt';
    faReportAusgabeDiffL: result := 'Ausgabe Diff Längen';
    faReportAusgabeDiffP: result := 'Ausgabe Diff Koordinaten';
    faReportXML: result := 'XML Report';
    faReportDebugReport: result := 'Debug Report';
    faReportReadme: result := 'Readme Report';

    faToggleSandboxed: result := 'Toggle Sandboxed';
    faToggleAllProps: result := 'Toggle All Trimm Props';
    faToggleAllTags: result := 'Toggle All Xml Tags';

    faToggleLineColor: result := 'Toggle Line Color Scheme';

    faToggleSegmentF: result := 'Toggle Segment F';
    faToggleSegmentR: result := 'Toggle Segment R';
    faToggleSegmentS: result := 'Toggle Segment S';
    faToggleSegmentM: result := 'Toggle Segment M';
    faToggleSegmentV: result := 'Toggle Segment V';
    faToggleSegmentW: result := 'Toggle Segment W';
    faToggleSegmentC: result := 'Toggle Segment C';
    faToggleSegmentA: result := 'Toggle Segment A';

    faRggZoomIn: result := 'Zoom In';
    faRggZoomOut: result := 'Zoom Out';

    faToggleUseDisplayList: result := 'Toggle Use DisplayList';
    faToggleUseQuickSort: result := 'Toggle Use Quicksort';
    faToggleShowLegend: result := 'Toggle Show DL Legend';
    faRggBogen: result := 'Show Mast Bogen';

    faToggleSalingGraph: result := 'Toggle Saling Graph';
    faToggleControllerGraph: result := 'Toggle Controller Graph';
    faToggleChartGraph: result := 'Toggle Chart Graph';
    faToggleMatrixText: result := 'Toggle Matrix Text';

    faMemoryBtn: result := 'Memory Btn';
    faMemoryRecallBtn: result := 'Memory Recall Btn';

    faSofortBtn: result := 'Sofort Btn';
    faGrauBtn: result := 'Grau Btn';
    faBlauBtn: result := 'Blau Btn';
    faMultiBtn: result := 'Multi Btn';
    faKoppelBtn: result := 'Koppel Btn';

    faTL01: result := 'Top Left 1';
    faTL02: result := 'Top Left 2';
    faTL03: result := 'Top Left 3';
    faTL04: result := 'Top Left 4';
    faTL05: result := 'Top Left 5';
    faTL06: result := 'Top Left 6';

    faTR01: result := 'Top Right 1';
    faTR02: result := 'Top Right 2';
    faTR03: result := 'Top Right 3';
    faTR04: result := 'Top Right 4';
    faTR05: result := 'Top Right 5';
    faTR06: result := 'Top Right 6';
    faTR07: result := 'Top Right 7';
    faTR08: result := 'Top Right 8';

    faBR01: result := 'Bottom Right 1';
    faBR02: result := 'Bottom Right 2';
    faBR03: result := 'Bottom Right 3';
    faBR04: result := 'Bottom Right 4';
    faBR05: result := 'Bottom Right 5';
    faBR06: result := 'Bottom Right 6';

    faBL01: result := 'Bottom Left 1';
    faBL02: result := 'Bottom Left 2';
    faBL03: result := 'Bottom Left 3';
    faBL04: result := 'Bottom Left 4';
    faBL05: result := 'Bottom Left 5';
    faBL06: result := 'Bottom Left 6';
    faBL07: result := 'Bottom Left 7';
    faBL08: result := 'Bottom Left 8';

    faMB01: result := 'Mobile Btn 1';
    faMB02: result := 'Mobile Btn 2';
    faMB03: result := 'Mobile Btn 3';
    faMB04: result := 'Mobile Btn 4';
    faMB05: result := 'Mobile Btn 5';
    faMB06: result := 'Mobile Btn 6';
    faMB07: result := 'Mobile Btn 7';
    faMB08: result := 'Mobile Btn 8';

    faCirclesReset: result := 'Reset Circle';
    faCirclesSelectC0: result := 'Unselect Circle';
    faCirclesSelectC1: result := 'Select Circle 1';
    faCirclesSelectC2: result := 'Select Circle 2';
    faCircleParamR1: result := 'Radius 1';
    faCircleParamR2: result := 'Radius 2';
    faCircleParamM1X: result := 'Mittelpunkt C1.X';
    faCircleParamM1Y: result := 'Mittelpunkt C1.Y';
    faCircleParamM2X: result := 'Mittelpunkt C2.X';
    faCircleParamM2Y: result := 'Mittelpunkt C2.Y';
    faLineParamA1: result := 'Line Segment 1 Angle';
    faLineParamA2: result := 'Line Segment 2 Angle';
    faLineParamE1: result := 'Line Segment 1 Elevation';
    faLineParamE2: result := 'Line Segment 2 Elevation';
    faCircleParamM1Z: result := 'Mittelpunkt C1.Z';
    faCircleParamM2Z: result := 'Mittelpunkt C2.Z';

    faPlusOne: result := 'Plus One';
    faPlusTen: result := 'Plus Ten';

    faBlackText: result := 'Black Text';
    faGrayText: result := 'Gray Text';
    faWhiteText: result := 'White Text';

    faPan: result := 'Pan';

    faParamORX: result := 'Param OrthoRot X';
    faParamORY: result := 'Param OrthoRot Y';
    faParamORZ: result := 'Param OrthoRot Z';
    faParamRX: result := 'Model Rotation X';
    faParamRY: result := 'Model Rotation Y';
    faParamRZ: result := 'Model Rotation Z';
    faParamCZ: result := 'Camera Position Z';

    faParamAPW: result := 'Param AP Width';
  end;
end;

end.
