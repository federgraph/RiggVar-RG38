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

{.$define WantAll}

uses
  System.SysUtils,
  RiggVar.FB.ActionConst;

function GetFederActionLongEN(fa: TFederAction): string;

implementation

function GetFederActionLongEN(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    faNoop: result := 'Noop';

    faController: result := 'Controller';
    faWinkel: result := 'Angle';
    faVorstag: result := 'Headstay';
    faWante: result := 'Shroud';
    faWoben: result := 'Shroud upper';
    faSalingH: result := 'Spreader Height';
    faSalingA: result := 'Spreader Distance';
    faSalingL: result := 'Spreader Length';
    faSalingW: result := 'Spreader Angle';
    faMastfallF0F: result := 'Mastfall F0F';
    faMastfallF0C: result := 'Mastfall F0C';
    faMastfallVorlauf: result := 'Mastfall Vorlauf';
    faBiegung: result := 'Bending';
    faMastfussD0X: result := 'Mastfoot D0X';
    faVorstagOS: result := 'Headstay WS';
    faWPowerOS: result := 'Shroud force WS';

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

    faTrimm0: result := 'Trim 0';
    faTrimm1: result := 'Trim 1';
    faTrimm2: result := 'Trim 2';
    faTrimm3: result := 'Trim 3';
    faTrimm4: result := 'Trim 4';
    faTrimm5: result := 'Trim 5';
    faTrimm6: result := 'Trim 6';
    fa420: result := 'Init 420'; //Trim 7
    faLogo: result := 'Init Logo'; //Trim 8

    faUpdateTrimm0: result := 'Update Trim 0';
    faCopyAndPaste: result := 'Memory - Copy And Paste';
    faReadTrimmFile: result := 'Read Trim File';
    faCopyTrimmFile: result := 'Copy Trim File';
    faSaveTrimmFile: result := 'Save Trim File';

    faSalingTypFest: result := 'Fixed Spreader';
    faSalingTypDrehbar: result := 'Rotatable Spreader';
    faSalingTypOhne: result := 'Without Spreaders';
    faSalingTypOhneStarr: result := 'Witout Spreaders Stiff';

    faCalcTypQuer: result := 'Querkraftbiegung';
    faCalcTypKnick: result := 'Biegeknicken';
    faCalcTypGemessen: result := 'Force measured';

    faWantRenderH: result := 'Want render H (Hull-Tetrahedron)';
    faWantRenderP: result := 'Want render P (Framework)';
    faWantRenderF: result := 'Want render F (Mastfall)';
    faWantRenderE: result := 'Want render E (Sphere E0-E)';
    faWantRenderS: result := 'Want render S (Rods)';

    faViewpointS: result := 'Viewpoint Side';
    faViewpointA: result := 'Viewpoint Stern';
    faViewpointT: result := 'Viewpoint Top';
    faViewpoint3: result := 'Viewpoint 3D';

    faToggleTrimmText: result := 'Toggle rgg trim text';
    faToggleDataText: result := 'Toggle rgg data text';
    faToggleDiffText: result := 'Toggle rgg diff text';
    faToggleDebugText: result := 'Toggle debug text';
    faUpdateReportText: result := 'Update report text';

    faRggHull: result := 'Toggle visibility of hull';
    faDemo: result := 'Toggle Demo / Pro mode';
    faCopyTrimmItem: result := 'Copy Trim-Item';
    faPasteTrimmItem: result := 'Paste Trim-Item or Trim-File';

    faShowMemo: result := 'Form Memo';
    faShowActions: result := 'Form Actions';
    faShowOptions: result := 'Form Options';
    faShowDrawings: result := 'Form Drawings';
    faShowConfig: result := 'Form Config';
    faShowTrimmTab: result := 'Form Trim Tabble';
    faShowKreis: result := 'Form Kreis';
    faShowInfo: result := 'Form Info';
    faShowSplash: result := 'Form Splash';

    faShowDiagA: result := 'Form Diagramm';
    faShowDiagC: result := 'Form Live Diagramm Controls';
    faShowDiagE: result := 'Form Diagramm Edits';
    faShowDiagQ: result := 'Form Diagramm Quick';

    faShowChart: result := 'Form Chart';
    faShowText: result := 'Form Text-Output';

    faShowForce: result := 'Form Force';
    faShowDetail: result := 'Form Detail';
    faShowTabelle: result := 'Form Table';
    faShowSaling: result := 'Form Spreader';
    faShowController: result := 'Form Controller';

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

    faToggleAllText: result := 'Toggle All Text';
    faToggleTouchFrame: result := 'Toggle Touch Frame';
    faToggleSpeedPanel: result := 'Toggle Speed Panel';

    faCycleColorSchemeM: result := 'cycle color scheme -';
    faCycleColorSchemeP: result := 'cycle color scheme +';

    faTouchDesk: result := 'Touch Desk';
    faTouchTablet: result := 'Touch Tablet';
    faTouchPhone: result := 'Touch Phone';

    faToggleButtonReport: result := 'Button Frame Report';

    faParamValuePlus1: result := 'Param Value + 1';
    faParamValueMinus1: result := 'Param Value - 1';
    faParamValuePlus10: result := 'Param Value + 10';
    faParamValueMinus10: result := 'Param Value - 10';

    faToggleHelp: result := 'Toggle Help Text';
    faToggleReport: result := 'Toggle Report';

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

    faToggleDarkMode: result := 'Toggle Dark Mode';
    faToggleButtonSize: result := 'Toggle Button Size';

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

    faToggleSandboxed: result := 'Toggle Sandboxed';
    faToggleAllProps: result := 'Toggle All Trim Props';
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

    faRggBogen: result := 'Show Mast-Bending';
    faRggKoppel: result := 'Show Linkage-Curve';

    faToggleSalingGraph: result := 'Toggle Spreader Graph';
    faToggleControllerGraph: result := 'Toggle Controller Graph';
    faToggleChartGraph: result := 'Toggle Chart Graph';
    faToggleKraftGraph: result := 'Toggle Force Graph';
    faToggleMatrixText: result := 'Toggle Matrix Text';

    faMemoryBtn: result := 'Memory Btn';
    faMemoryRecallBtn: result := 'Memory Recall Btn';

    faKorrigiertItem: result := 'Corrected Item';
    faSofortBtn: result := 'Compute Immediately Btn';
    faGrauBtn: result := 'Gray Btn';
    faBlauBtn: result := 'Blue Btn';
    faMultiBtn: result := 'Multi Btn';

    faSuperSimple: result := 'Super Simple';
    faSuperNormal: result := 'Super Normal';
    faSuperGrau: result := 'Super Gray';
    faSuperBlau: result := 'Super Blue';
    faSuperMulti: result := 'Super Multi';
    faSuperDisplay: result := 'Super Disp';
    faSuperQuick: result := 'Super Quick';

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

    faPlusOne: result := 'Plus One';
    faPlusTen: result := 'Plus Ten';

    faPan: result := 'Pan';

    faParamORX: result := 'Param OrthoRot X';
    faParamORY: result := 'Param OrthoRot Y';
    faParamORZ: result := 'Param OrthoRot Z';
    faParamRX: result := 'Model Rotation X';
    faParamRY: result := 'Model Rotation Y';
    faParamRZ: result := 'Model Rotation Z';
    faParamCZ: result := 'Camera Position Z';

    faParamAPW: result := 'Param AP Width';
    faParamEAH: result := 'Param EA Hull';
    faParamEAR: result := 'Param EA Rig';
    faParamEI: result := 'Param EI Mast';

    faRotaForm1: result := 'Use RotaForm 1';
    faRotaForm2: result := 'Use RotaForm 2';
    faRotaForm3: result := 'Use RotaForm 3';

    faReset: result := 'Reset';
    faResetPosition: result := 'Reset Position';
    faResetRotation: result := 'Reset Rotation';
    faResetZoom: result := 'Reset Zoom';

    faShowHelpText: result := 'Show Help Text';
    faShowInfoText: result := 'Show Info Text';
    faShowNormalKeyInfo: result := 'Show normal key info';
    faShowSpecialKeyInfo: result := 'Show special key info';
    faShowDebugInfo: result := 'Show Debug Info';
    faShowZOrderInfo: result := 'Show Z-Order';

    faParamT1: result := 'Param T1';
    faParamT2: result := 'Param T2';
    faParamT3: result := 'Param T3';
    faParamT4: result := 'Param T4';

    faTouchBarTop: result := 'TouchBar Top: Rotation Z';
    faTouchBarBottom: result := 'TouchBar Bottom: Zoom';
    faTouchBarLeft: result := 'TouchBar Left: Big Step';
    faTouchBarRight: result := 'TouchBar Right: Small Step ';

    faToggleSortedRota: result := 'Toggle Sorted Rota';

    faToggleViewType: result := 'Toggle view type';
    faViewTypeOrtho: result := 'Set view type to orthographic';
    faViewTypePerspective: result := 'Set view type to perspective';

    faToggleLanguage: result := 'Toggle Language'; //'Use Localized Language';

    faToggleDropTarget: result := 'Drop target';

    faSave: result := 'Save';
    faLoad: result := 'Load';
    faOpen: result := 'Open';
    faCopy: result := 'Copy';
    faPaste: result := 'Paste';
    faShare: result := 'Share';

{$ifdef WantAll}
    faHullMesh :result := 'toggle hull mesh';
    faHullMeshOn :result := 'hull mesh on';
    faHullMeshOff :result := 'hull mesh off';

    faToggleMoveMode: result := 'Toggle move mode';
    faLinearMove: result := 'Linear move';
    faExpoMove: result := 'Exponential move';

    faCycleBitmapP: result := 'cycle bitmap +';
    faCycleBitmapM: result := 'cycle bitmap -';

    faRandom: result := 'Random Param Values';
    faRandomWhite: result := 'random colors white rings';
    faRandomBlack: result := 'random colors black rings';
    faBitmapEscape: result := 'Enter outer cycle';

    faToggleContour: result := 'Toggle contour rings';
{$endif}

  end;
end;

end.
