unit RiggVar.FB.ActionLongEN;

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
  RiggVar.FB.ActionConst;

function GetFederActionLongEN(fa: TFederAction): string;

implementation

function GetFederActionLongEN(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    // --- generated code snippet ---

    { EmptyAction }
    faNoop: result := 'Noop';

    { Pages }
    faActionPageM: result := 'Action Page Minus';
    faActionPageP: result := 'Action Page Plus';
    faActionPageE: result := 'Action Page E';
    faActionPageS: result := 'Action Page S';
    faActionPageX: result := 'Action Page X';
    faActionPage1: result := 'Action Page 1';
    faActionPage2: result := 'Action Page 2';
    faActionPage3: result := 'Action Page 3';
    faActionPage4: result := 'Action Page 4';
    faActionPage5: result := 'Action Page 5';
    faActionPage6: result := 'Action Page 6';

    { Forms }
    faRotaForm1: result := 'Use RotaForm 1';
    faRotaForm2: result := 'Use RotaForm 2';
    faRotaForm3: result := 'Use RotaForm 3';
    faShowMemo: result := 'Form Memo';
    faShowActions: result := 'Form Actions';
    faShowOptions: result := 'Form Options';
    faShowDrawings: result := 'Form Drawings';
    faShowConfig: result := 'Form Config';
    faShowKreis: result := 'Form Kreis';
    faShowInfo: result := 'Form Info';
    faShowSplash: result := 'Form Splash';
    faShowForce: result := 'Form Force';
    faShowTabelle: result := 'Form Table';
    faShowDetail: result := 'Form Detail';
    faShowSaling: result := 'Form Spreader';
    faShowController: result := 'Form Controller';
    faShowText: result := 'Form Text-Output';
    faShowTrimmTab: result := 'Form Trim Tabble';
    faShowChart: result := 'Form Chart';
    faShowDiagA: result := 'Form Diagramm';
    faShowDiagC: result := 'Form Live Diagramm Controls';
    faShowDiagE: result := 'Form Diagramm Edits';
    faShowDiagQ: result := 'Form Diagramm Quick';

    { TouchLayout }
    faTouchTablet: result := 'Touch Tablet';
    faTouchPhone: result := 'Touch Phone';
    faTouchDesk: result := 'Touch Desk';

    { Wheel }
    faParamValuePlus1: result := 'Param Value + 1';
    faParamValueMinus1: result := 'Param Value - 1';
    faParamValuePlus10: result := 'Param Value + 10';
    faParamValueMinus10: result := 'Param Value - 10';
    faWheelLeft: result := 'Wheel -1';
    faWheelRight: result := 'Wheel +1';
    faWheelDown: result := 'Wheel +10';
    faWheelUp: result := 'Wheel -10';

    { ColorScheme }
    faCycleColorSchemeM: result := 'cycle color scheme -';
    faCycleColorSchemeP: result := 'cycle color scheme +';

    { FederText }
    faToggleAllText: result := 'Toggle All Text';
    faToggleTouchFrame: result := 'Toggle Touch Frame';

    { ViewParams }
    faPan: result := 'Pan';
    faParamORX: result := 'Param OrthoRot X';
    faParamORY: result := 'Param OrthoRot Y';
    faParamORZ: result := 'Param OrthoRot Z';
    faParamRX: result := 'Model Rotation X';
    faParamRY: result := 'Model Rotation Y';
    faParamRZ: result := 'Model Rotation Z';
    faParamCZ: result := 'Camera Position Z';

    { Help }
    faToggleHelp: result := 'Toggle Help Text';
    faToggleReport: result := 'Toggle Report';
    faToggleButtonReport: result := 'Button Frame Report';

    { RggControls }
    faController: result := 'Controller';
    faWinkel: result := 'Angle';
    faVorstag: result := 'Headstay';
    faWante: result := 'Shroud';
    faWoben: result := 'Shroud upper';
    faSalingH: result := 'Spreader Height';
    faSalingA: result := 'Spreader Distance';
    faSalingL: result := 'Spreader Length';
    faSalingW: result := 'Spreader Angle';
    faMastfallF0C: result := 'Mastfall F0C';
    faMastfallF0F: result := 'Mastfall F0F';
    faMastfallVorlauf: result := 'Mastfall Vorlauf';
    faBiegung: result := 'Bending';
    faMastfussD0X: result := 'Mastfoot D0X';
    faVorstagOS: result := 'Headstay WS';
    faWPowerOS: result := 'Shroud force WS';
    faParamAPW: result := 'Param AP Width';
    faParamEAH: result := 'Param EA Hull';
    faParamEAR: result := 'Param EA Rig';
    faParamEI: result := 'Param EI Mast';

    { RggFixPoints }
    faFixpointA0: result := 'Fixpoint oA0';
    faFixpointA: result := 'Fixpoint oA';
    faFixpointB0: result := 'Fixpoint oB0';
    faFixpointB: result := 'Fixpoint oB';
    faFixpointC0: result := 'Fixpoint oC0';
    faFixpointC: result := 'Fixpoint oC';
    faFixpointD0: result := 'Fixpoint oD0';
    faFixpointD: result := 'Fixpoint oD';
    faFixpointE0: result := 'Fixpoint oE0';
    faFixpointE: result := 'Fixpoint oE';
    faFixpointF0: result := 'Fixpoint oF0';
    faFixpointF: result := 'Fixpoint oF';

    { RggViewPoint }
    faViewpointS: result := 'Viewpoint Side';
    faViewpointA: result := 'Viewpoint Stern';
    faViewpointT: result := 'Viewpoint Top';
    faViewpoint3: result := 'Viewpoint 3D';

    { RggSalingType }
    faSalingTypFest: result := 'Fixed Spreader';
    faSalingTypDrehbar: result := 'Rotatable Spreader';
    faSalingTypOhne: result := 'Without Spreaders';
    faSalingTypOhneStarr: result := 'Witout Spreaders Stiff';

    { RggCalcType }
    faCalcTypQuer: result := 'Querkraftbiegung';
    faCalcTypKnick: result := 'Biegeknicken';
    faCalcTypGemessen: result := 'Force measured';

    { RggAppMode }
    faDemo: result := 'Toggle Demo / Pro mode';
    faMemoryBtn: result := 'Memory Btn';
    faMemoryRecallBtn: result := 'Memory Recall Btn';
    faKorrigiertItem: result := 'Corrected Item';
    faSofortBtn: result := 'Compute Immediately Btn';
    faGrauBtn: result := 'Gray Btn';
    faBlauBtn: result := 'Blue Btn';
    faMultiBtn: result := 'Multi Btn';

    { RggSuper }
    faSuperSimple: result := 'Super Simple';
    faSuperNormal: result := 'Super Normal';
    faSuperGrau: result := 'Super Gray';
    faSuperBlau: result := 'Super Blue';
    faSuperMulti: result := 'Super Multi';
    faSuperDisplay: result := 'Super Disp';
    faSuperQuick: result := 'Super Quick';

    { RggReport }
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
    faReportAusgabeDetail: result := 'Output Rigg Detail';
    faReportAusgabeRL: result := 'Output Rigg Length';
    faReportAusgabeRP: result := 'Output Rigg Coordinates';
    faReportAusgabeRLE: result := 'Output Rigg Lengths Relaxed';
    faReportAusgabeRPE: result := 'Output Rigg Coordinates Relaxed';
    faReportAusgabeDiffL: result := 'Output Diff Lengths';
    faReportAusgabeDiffP: result := 'Output Diff Coordinates';
    faReportXML: result := 'XML Report';
    faReportDebugReport: result := 'Debug Report';
    faReportReadme: result := 'Readme Report';

    { RggChart }
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

    { RggGraph }
    faToggleLineColor: result := 'Toggle Line Color Scheme';
    faToggleUseDisplayList: result := 'Toggle Use DisplayList';
    faToggleUseQuickSort: result := 'Toggle Use Quicksort';
    faToggleShowLegend: result := 'Toggle Show DL Legend';
    faToggleSortedRota: result := 'Toggle Sorted Rota';
    faRggBogen: result := 'Show Mast-Bending';
    faRggKoppel: result := 'Show Linkage-Curve';
    faRggHull: result := 'Toggle visibility of hull';
    faRggZoomIn: result := 'Zoom In';
    faRggZoomOut: result := 'Zoom Out';
    faToggleSalingGraph: result := 'Toggle Spreader Graph';
    faToggleControllerGraph: result := 'Toggle Controller Graph';
    faToggleChartGraph: result := 'Toggle Chart Graph';
    faToggleKraftGraph: result := 'Toggle Force Graph';
    faToggleMatrixText: result := 'Toggle Matrix Text';

    { RggSegment }
    faToggleSegmentF: result := 'Toggle Segment F';
    faToggleSegmentR: result := 'Toggle Segment R';
    faToggleSegmentS: result := 'Toggle Segment S';
    faToggleSegmentM: result := 'Toggle Segment M';
    faToggleSegmentV: result := 'Toggle Segment V';
    faToggleSegmentW: result := 'Toggle Segment W';
    faToggleSegmentC: result := 'Toggle Segment C';
    faToggleSegmentA: result := 'Toggle Segment A';

    { RggRenderOptions }
    faWantRenderH: result := 'Want render H (Hull-Tetrahedron)';
    faWantRenderP: result := 'Want render P (Framework)';
    faWantRenderF: result := 'Want render F (Mastfall)';
    faWantRenderE: result := 'Want render E (Sphere E0-E)';
    faWantRenderS: result := 'Want render S (Rods)';

    { RggTrimms }
    faTrimm0: result := 'Trim 0';
    faTrimm1: result := 'Trim 1';
    faTrimm2: result := 'Trim 2';
    faTrimm3: result := 'Trim 3';
    faTrimm4: result := 'Trim 4';
    faTrimm5: result := 'Trim 5';
    faTrimm6: result := 'Trim 6';
    fa420: result := 'Init 420';
    faLogo: result := 'Init Logo';

    { RggTrimmFile }
    faCopyTrimmItem: result := 'Copy Trim-Item';
    faPasteTrimmItem: result := 'Paste Trim-Item or Trim-File';
    faCopyAndPaste: result := 'Memory - Copy And Paste';
    faUpdateTrimm0: result := 'Update Reference Trim 0';
    faReadTrimmFile: result := 'Read Trim File';
    faSaveTrimmFile: result := 'Save Trim File';
    faCopyTrimmFile: result := 'Copy Trim File';

    { RggTrimmText }
    faToggleTrimmText: result := 'Toggle rgg trim text';
    faToggleDiffText: result := 'Toggle rgg diff text';
    faToggleDataText: result := 'Toggle rgg data text';
    faToggleDebugText: result := 'Toggle debug text';
    faUpdateReportText: result := 'Update report text';

    { RggSonstiges }
    faToggleDarkMode: result := 'Toggle Dark Mode';
    faToggleButtonSize: result := 'Toggle Button Size';
    faToggleSandboxed: result := 'Toggle Sandboxed';
    faToggleSpeedPanel: result := 'Toggle Speed Panel';
    faToggleAllProps: result := 'Toggle All Trim Props';
    faToggleAllTags: result := 'Toggle All Xml Tags';

    { RggInfo }
    faShowHelpText: result := 'Show Help Text';
    faShowInfoText: result := 'Show Info Text';
    faShowNormalKeyInfo: result := 'Show normal key info';
    faShowSpecialKeyInfo: result := 'Show special key info';
    faShowDebugInfo: result := 'Show Debug Info';
    faShowZOrderInfo: result := 'Show Z-Order';

    { BtnLegendTablet }
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
    faBL01: result := 'Bottom Left 1';
    faBL02: result := 'Bottom Left 2';
    faBL03: result := 'Bottom Left 3';
    faBL04: result := 'Bottom Left 4';
    faBL05: result := 'Bottom Left 5';
    faBL06: result := 'Bottom Left 6';
    faBL07: result := 'Bottom Left 7';
    faBL08: result := 'Bottom Left 8';
    faBR01: result := 'Bottom Right 1';
    faBR02: result := 'Bottom Right 2';
    faBR03: result := 'Bottom Right 3';
    faBR04: result := 'Bottom Right 4';
    faBR05: result := 'Bottom Right 5';
    faBR06: result := 'Bottom Right 6';

    { BtnLegendPhone }
    faMB01: result := 'Mobile Btn 1';
    faMB02: result := 'Mobile Btn 2';
    faMB03: result := 'Mobile Btn 3';
    faMB04: result := 'Mobile Btn 4';
    faMB05: result := 'Mobile Btn 5';
    faMB06: result := 'Mobile Btn 6';
    faMB07: result := 'Mobile Btn 7';
    faMB08: result := 'Mobile Btn 8';

    { TouchBarLegend }
    faTouchBarTop: result := 'TouchBar Top';
    faTouchBarBottom: result := 'TouchBar Bottom';
    faTouchBarLeft: result := 'TouchBar Left';
    faTouchBarRight: result := 'TouchBar Right';

    { Circles }
    faCirclesSelectC0: result := 'Unselect Circle';
    faCirclesSelectC1: result := 'Select Circle 1';
    faCirclesSelectC2: result := 'Select Circle 2';
    faCircleParamR1: result := 'Radius 1';
    faCircleParamR2: result := 'Radius 2';
    faCircleParamM1X: result := 'Center Point C1.X';
    faCircleParamM1Y: result := 'Center Point C1.Y';
    faCircleParamM2X: result := 'Center Point C2.X';
    faCircleParamM2Y: result := 'Center Point C2.Y';
    faLineParamA1: result := 'Line Segment 1 Angle';
    faLineParamA2: result := 'Line Segment 2 Angle';
    faLineParamE1: result := 'Line Segment 1 Elevation';
    faLineParamE2: result := 'Line Segment 2 Elevation';
    faCircleParamM1Z: result := 'Center Point C1.Z';
    faCircleParamM2Z: result := 'Center Point C2.Z';
    faCirclesReset: result := 'Reset Circle';

    { MemeFormat }
    faMemeGotoLandscape: result := 'Goto Landscape';
    faMemeGotoSquare: result := 'Goto Square';
    faMemeGotoPortrait: result := 'Goto Portrait';
    faMemeFormat0: result := 'Format 0 - 600 x 800';
    faMemeFormat1: result := 'Format 1 - 800 x 600';
    faMemeFormat2: result := 'Format 2 - 1440 x 900';
    faMemeFormat3: result := 'Format 3 - iPad Landscape Screen';
    faMemeFormat4: result := 'Format 4 - iPad Landscape';
    faMemeFormat5: result := 'Format 5 - iPad Portrait';
    faMemeFormat6: result := 'Format 6 - iPhone 14 Landscape';
    faMemeFormat7: result := 'Format 7 - iPhone 14 Portrait';
    faMemeFormat8: result := 'Format 8 - Pixel 8 Landscape';
    faMemeFormat9: result := 'Format 9 - Pixel 8 Portrait';

    { Reset }
    faReset: result := 'Reset';
    faResetPosition: result := 'Reset Position';
    faResetRotation: result := 'Reset Rotation';
    faResetZoom: result := 'Reset Zoom';

    { Language }
    faToggleLanguage: result := 'Toggle Language';

    { CopyPaste }
    faSave: result := 'Save';
    faLoad: result := 'Load';
    faOpen: result := 'Open';
    faCopy: result := 'Copy';
    faPaste: result := 'Paste';
    faShare: result := 'Share';

  end;
end;

end.
