﻿unit RiggVar.FederModel.ActionMapTablet;

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
  System.UITypes,
  System.UIConsts,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionMap;

type
  TActionMapTablet = class(TActionMap)
  private
    procedure InitDefault;
    procedure InitAC(cl: TCornerLocation; bi, fa: Integer; cla: TAlphaColor);
  protected
    procedure InitActionsRG(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

{

FrameLocation: absolute position

[01][03][04][05][06]----[14][13][12][11][07]
[02]------------------------------------[08]
----------------------------------------[09]
----------------------------------------[10]
--------------------------------------------
[22]----------------------------------------
[21]----------------------------------------
[20]------------------------------------[24]
[15][16][17][18][19]----[28][27][26][25][23]

CornerLocation: relative positions

[1][2][3][4][5]----[1][2][3][4][5]
[6]----------------------------[6]
-------------------------------[7]
-------------------------------[8]
----------------------------------
[8]-------------------------------
[7]-------------------------------
[6]----------------------------[6]
[1][2][3][4][5]----[1][2][3][4][5]

}

implementation

const
  claForm = claCornflowerblue;
  claParam = claPlum;
  claSample = claTeal;
  claOption = claBeige;
  claUI = claYellow;
  claHull = claBeige;
  claReport = claDimgray;
  claColorScheme = claDimgray;
  claMemory = claBeige;


{$ifdef MSWINDOWS}
  PageCountRG = 10;
  EscapePageCountRG = 6;
{$endif}

{$ifdef OSX}
  PageCountRG = 10;
  EscapePageCountRG = 6;
{$endif}

{$ifdef IOS}
  PageCountRG = 8;
  EscapePageCountRG = 6;
{$endif}

{$ifdef Android}
  PageCountRG = 8;
  EscapePageCountRG = 6;
{$endif}

constructor TActionMapTablet.Create;
begin
  inherited;
  FPageCount := PageCountRG;
  FEscapeIndex := EscapePageCountRG;
  TestName := 'Tablet Page';
end;

procedure TActionMapTablet.InitActions(Layout: Integer);
begin
  InitDefault;
  InitActionsRG(Layout);
end;

procedure TActionMapTablet.InitDefault;
begin
  IAC(1, faActionPageM, claYellow);
  IAC(7, faActionPageP, claYellow);
end;

procedure TActionMapTablet.InitAC(cl: TCornerLocation; bi,
  fa: Integer; cla: TAlphaColor);
var
  j: Integer; // FrameLocation
begin
  { First, translate from CornerLocation to FrameLocation }
  j := 0;
  case cl of
    TopLeft:
    begin
      case bi of
        1: j := 1;
        2: j := 3;
        3: j := 4;
        4: j := 5;
        5: j := 6;
        6: j := 2;
      end;
    end;

    TopRight:
    begin
      case bi of
        1: j := 14;
        2: j := 13;
        3: j := 12;
        4: j := 11;
        5: j := 7;
        6: j := 8;
        7: j := 9;
        8: j := 10;
      end;
    end;

    BottomRight:
    begin
      case bi of
        1: j := 28;
        2: j := 27;
        3: j := 26;
        4: j := 25;
        5: j := 23;
        6: j := 24;
      end;
    end;

    BottomLeft:
    begin
      case bi of
        1: j := 15;
        2: j := 16;
        3: j := 17;
        4: j := 18;
        5: j := 19;
        6: j := 20;
        7: j := 21;
        8: j := 22;
      end;
    end;
  end;
  { Init button with Action and Color }
  IAC(j, fa, cla);
end;

procedure TActionMapTablet.InitActionsRG(Layout: Integer);
var
  cl: TCornerLocation;
  cla: TAlphaColor;
begin
{
[1][2][3][4][5]-----[1][2][3][4][5]
[6]-----------------------------[6]
--------------------------------[7]
--------------------------------[8]
-----------------------------------
-----------------------------------
[8]--------------------------------
[7]--------------------------------
[6]-----------------------------[6]
[1][2][3][4][5]-----[1][2][3][4][5]
}

  case Layout of
    1:
    begin
      CurrentPageCaption := 'Home Page';
 
      cl := TopLeft;
      cla := claParam;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faSalingH, cla);
      InitAC(cl, 3, faSalingA, cla);
      InitAC(cl, 4, faSalingL, cla);
      InitAC(cl, 5, faSalingW, cla);
      InitAC(cl, 6, faMastfallF0F, cla);

      cl := TopRight;
      cla := claWhite;
      InitAC(cl, 1, faViewpointA, cla);
      InitAC(cl, 2, faViewpointT, cla);
      InitAC(cl, 3, faViewpointS, cla);
      InitAC(cl, 4, faViewpoint3, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faRotaForm1, claForm);
      InitAC(cl, 7, faRotaForm2, claForm);
      InitAC(cl, 8, faRotaForm3, claForm);

      cl := BottomLeft;
      cla := claParam;
      InitAC(cl, 1, faMastfussD0X, cla);
      InitAC(cl, 2, faVorstag, cla);
      InitAC(cl, 3, faWante, cla);
      InitAC(cl, 4, faWOben, cla);
      InitAC(cl, 5, faController, cla);
      InitAC(cl, 6, faMastfallVorlauf, cla);
      InitAC(cl, 7, faBiegung, cla);
      InitAC(cl, 8, faMastfallF0C, cla);

      cl := BottomRight;
      cla := claOption;
      InitAC(cl, 1, faSalingTypFest, cla);
      InitAC(cl, 2, faSalingTypDrehbar, cla);
      InitAC(cl, 3, faSalingTypOhne, cla);
      InitAC(cl, 4, faSalingTypOhneStarr, cla);
      InitAC(cl, 5, faDemo, claPlum);
      InitAC(cl, 6, fa420, claSample);
    end;

    2:
    begin
      CurrentPageCaption := 'Page 2';

      cl := TopLeft;
      cla := claParam;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faSalingH, cla);
      InitAC(cl, 3, faSalingA, cla);
      InitAC(cl, 4, faSalingL, cla);
      InitAC(cl, 5, faSalingW, cla);
      InitAC(cl, 6, faLogo, claSample);

      cl := TopRight;
      cla := claWhite;
      InitAC(cl, 1, faFixpointA, cla);
      InitAC(cl, 2, faFixpointB, cla);
      InitAC(cl, 3, faFixpointC, cla);
      InitAC(cl, 4, faFixpointD, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faFixpointE, cla);
      InitAC(cl, 7, faFixpointF, cla);
      InitAC(cl, 8, fa420, claSample);

      cl := BottomLeft;
      cla := claParam;
      InitAC(cl, 1, faMastfussD0X, cla);
      InitAC(cl, 2, faVorstag, cla);
      InitAC(cl, 3, faWante, cla);
      InitAC(cl, 4, faWOben, cla);
      InitAC(cl, 5, faController, cla);
      InitAC(cl, 6, faMastfallVorlauf, cla);
      InitAC(cl, 7, faBiegung, cla);
      InitAC(cl, 8, faMastfallF0C, cla);

      cl := BottomRight;
      cla := claWhite;
      InitAC(cl, 1, faFixpointA0, cla);
      InitAC(cl, 2, faFixpointB0, cla);
      InitAC(cl, 3, faFixpointC0, cla);
      InitAC(cl, 4, faFixpointD0, cla);
      InitAC(cl, 5, faFixpointE0, cla);
      InitAC(cl, 6, faFixpointF0, cla);
    end;

    3:
    begin
      CurrentPageCaption := 'Sonstiges';

      cl := TopLeft;
      cla := claWhite;
//      InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faCycleColorSchemeM, claColorScheme);
      InitAC(cl, 3, faCycleColorSchemeP, claColorScheme);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);

      cl := TopRight;
      cla := claWhite;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faToggleDataText, claReport);
      InitAC(cl, 3, faToggleDiffText, claReport);
      InitAC(cl, 4, faToggleTrimmText, claReport);
//      InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faPan, claWhite);
      InitAC(cl, 7, faReset, claWhite);
      InitAC(cl, 8, fa420, claSample);

      cl := BottomLeft;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faSofortBtn, claWhite);
      InitAC(cl, 8, faToggleAllText, claWhite);

      cl := BottomRight;
      InitAC(cl, 1, faToggleLanguage, claWhite);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faToggleSpeedPanel, claUI);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faRggHull, claHull);
    end;

    4:
    begin
      CurrentPageCaption := 'Data';

      cl := TopLeft;
      cla := claWhite;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faCopyTrimmFile, cla);
      InitAC(cl, 3, faCopyTrimmItem, cla);
      InitAC(cl, 4, faReadTrimmFile, claCoral);
      InitAC(cl, 5, faSaveTrimmFile, claCoral);
      InitAC(cl, 6, faPasteTrimmItem, cla);

      cl := TopRight;
      cla := claWhite;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faToggleDataText, claReport);
      InitAC(cl, 3, faToggleDiffText, claReport);
      InitAC(cl, 4, faToggleTrimmText, claReport);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      cla := claSample;
      InitAC(cl, 1, faTrimm4, cla);
      InitAC(cl, 2, faTrimm5, cla);
      InitAC(cl, 3, faTrimm6, cla);
      InitAC(cl, 4, fa420, cla);
      InitAC(cl, 5, faLogo, cla);
      InitAC(cl, 6, faTrimm3, cla);
      InitAC(cl, 7, faTrimm2, cla);
      InitAC(cl, 8, faTrimm1, cla);

      cl := BottomRight;
      cla := claWhite;
      InitAC(cl, 1, faUpdateTrimm0, cla);
      InitAC(cl, 2, faCopyAndPaste, cla);
      InitAC(cl, 3, faToggleSpeedPanel, claUI);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faMemoryRecallBtn, claMemory);
      InitAC(cl, 6, faMemoryBtn, claMemory);
    end;

    5:
    begin
      CurrentPageCaption := 'Reports';

      cl := TopLeft;
      cla := claReport;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faReportLong, cla);
      InitAC(cl, 3, faReportShort, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faToggleAllText, claWhite);

      cl := TopRight;
      InitAC(cl, 1, faReportJsonText, cla);
      InitAC(cl, 2, faReportDataText, cla);
      InitAC(cl, 3, faReportDiffText, cla);
      InitAC(cl, 4, faReportTrimmText, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faPan, claWhite);
      InitAC(cl, 7, faNoop, claWhite);
      InitAC(cl, 8, fa420, claSample);

      cl := BottomLeft;
      InitAC(cl, 1, faReportAusgabeRL, cla);
      InitAC(cl, 2, faReportAusgabeRP, cla);
      InitAC(cl, 3, faReportAusgabeRLE, cla);
      InitAC(cl, 4, faReportAusgabeRPE, cla);
      InitAC(cl, 5, faReportAusgabeDiffL, cla);
      InitAC(cl, 6, faReportAusgabeDiffP, cla);
      InitAC(cl, 7, faSofortBtn, claWhite);
      InitAC(cl, 8, faNoop, claWhite);

      cl := BottomRight;
      InitAC(cl, 1, faReportNone, cla);
      InitAC(cl, 2, faReportReadme, cla);
      InitAC(cl, 3, faToggleSpeedPanel, claUI);
      InitAC(cl, 4, faNoop, claWhite);
      InitAC(cl, 5, faActionPageE, claUI);
      InitAC(cl, 6, faReportDebugReport, cla);
    end;

    6:
    begin
      CurrentPageCaption := 'Forms';

      cl := TopLeft;
      cla := claOption;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faWantRenderH, cla);
      InitAC(cl, 3, faWantRenderP, cla);
      InitAC(cl, 4, faWantRenderF, cla);
      InitAC(cl, 5, faWantRenderE, cla);
      InitAC(cl, 6, faWantRenderS, cla);

      cl := TopRight;
      cla := claWhite;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faToggleDataText, claReport);
      InitAC(cl, 3, faToggleDiffText, claReport);
      InitAC(cl, 4, faToggleTrimmText, claReport);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faShowDrawings, claForm);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      cla := claWhite;
      InitAC(cl, 1, faParamValuePlus10, cla);
      InitAC(cl, 2, faShowActions, claForm);
      InitAC(cl, 3, faShowMemo, claForm);
      InitAC(cl, 4, faShowConfig, claForm);
      InitAC(cl, 5, faShowTrimmTab, claForm);
      InitAC(cl, 6, faParamValuePlus1, cla);
      InitAC(cl, 7, faParamValueMinus1, cla);
      InitAC(cl, 8, faParamValueMinus10, cla);

      cl := BottomRight;
      InitAC(cl, 1, faResetPosition, cla);
      InitAC(cl, 2, faResetRotation, cla);
      InitAC(cl, 3, faToggleSpeedPanel, claUI);
      InitAC(cl, 4, faResetZoom, cla);
      InitAC(cl, 5, faActionPageX, claUI);
      InitAC(cl, 6, faRggHull, claHull);
    end;

    7:
    begin
      CurrentPageCaption := 'Extra';

      cl := TopLeft;
      cla := claWhite;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faToggleUseDisplayList, claPlum);
      InitAC(cl, 3, faToggleUseQuickSort, claPlum);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faToggleShowLegend, claPlum);

      cl := TopRight;
//      cla := claWhite;
      InitAC(cl, 1, faToggleSalingGraph, claCornflowerblue);
      InitAC(cl, 2, faToggleControllerGraph, claCornflowerblue);
      InitAC(cl, 3, faToggleChartGraph, claCornflowerblue);
      InitAC(cl, 4, faToggleMatrixText, claCornflowerblue);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faRotaForm1, claForm);
      InitAC(cl, 7, faRotaForm2, claForm);
      InitAC(cl, 8, faRotaForm3, claForm);

      cl := BottomLeft;
      cla := claAqua;
      InitAC(cl, 1, faToggleSegmentF, cla);
      InitAC(cl, 2, faToggleSegmentR, cla);
      InitAC(cl, 3, faToggleSegmentS, cla);
      InitAC(cl, 4, faToggleSegmentM, cla);
      InitAC(cl, 5, faToggleSegmentV, cla);
      InitAC(cl, 6, faToggleSegmentW, cla);
      InitAC(cl, 7, faToggleSegmentC, cla);
      InitAC(cl, 8, faToggleSegmentA, cla);

      cl := BottomRight;
      cla := claWhite;
      InitAC(cl, 1, faSofortBtn, cla);
      InitAC(cl, 2, faGrauBtn, cla);
      InitAC(cl, 3, faBlauBtn, cla);
      InitAC(cl, 4, faMultiBtn, cla);
      InitAC(cl, 5, faRggKoppel, cla);
      InitAC(cl, 6, faRggBogen, cla);
    end;

    8:
    begin
      CurrentPageCaption := 'Legend';

      cla := claWhite;
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faTL02, cla);
      InitAC(cl, 3, faTL03, cla);
      InitAC(cl, 4, faTL04, cla);
      InitAC(cl, 5, faTL05, cla);
      InitAC(cl, 6, faTL06, cla);

      cl := TopRight;
      InitAC(cl, 1, faTR01, cla);
      InitAC(cl, 2, faTR02, cla);
      InitAC(cl, 3, faTR03, cla);
      InitAC(cl, 4, faTR04, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faTR06, cla);
      InitAC(cl, 7, faTR07, cla);
      InitAC(cl, 8, faTR08, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faBL01, cla);
      InitAC(cl, 2, faBL02, cla);
      InitAC(cl, 3, faBL03, cla);
      InitAC(cl, 4, faBL04, cla);
      InitAC(cl, 5, faBL05, cla);
      InitAC(cl, 6, faBL06, cla);
      InitAC(cl, 7, faBL07, cla);
      InitAC(cl, 8, faBL08, cla);

      cl := BottomRight;
      InitAC(cl, 1, faBR01, cla);
      InitAC(cl, 2, faBR02, cla);
      InitAC(cl, 3, faBR03, cla);
      InitAC(cl, 4, faBR04, cla);
      InitAC(cl, 5, faBR05, cla);
      InitAC(cl, 6, faBR06, cla);
    end;

    9:
    begin
      CurrentPageCaption := 'Chart';

      cla := claWhite;
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faChartRect, claPlum);
      InitAC(cl, 3, faChartTextRect, claPlum);
      InitAC(cl, 4, faChartLegend, claPlum);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faChartReset, claPlum);

      cl := TopRight;
      InitAC(cl, 1, faXComboMinus, cla);
      InitAC(cl, 2, faXComboPlus, cla);
      InitAC(cl, 3, faPComboMinus, cla);
      InitAC(cl, 4, faPComboPlus, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faYComboPlus, cla);
      InitAC(cl, 7, faYComboMinus, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faChartAP, cla);
      InitAC(cl, 2, faChartBP, cla);
      InitAC(cl, 3, faChartGroup, cla);
      InitAC(cl, 4, faParamCountMinus, cla);
      InitAC(cl, 5, faParamCountPlus, cla);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faParamAPW, claPlum);

      cl := BottomRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faToggleChartGraph, claForm);
      InitAC(cl, 4, faShowDiagE, claForm);
      InitAC(cl, 5, faShowDiagQ, claForm);
      InitAC(cl, 6, faShowDiagC, claForm);
    end;

    10:
    begin
      CurrentPageCaption := 'Format';

      cla := claWhite;
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faMemeGotoLandscape, claPlum);
      InitAC(cl, 3, faMemeGotoSquare, claPlum);
      InitAC(cl, 4, faMemeGotoPortrait, claPlum);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);

      cl := TopRight;
      InitAC(cl, 1, faMemeFormat0, cla);
      InitAC(cl, 2, faMemeFormat1, claWhite);
      InitAC(cl, 3, faMemeFormat2, cla);
      InitAC(cl, 4, faMemeFormat3, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faMemeFormat4, cla);
      InitAC(cl, 7, faMemeFormat5, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faMemeFormat6, cla);
      InitAC(cl, 2, faMemeFormat7, cla);
      InitAC(cl, 3, faMemeFormat8, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faMemeFormat9, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;

  end;
end;

end.
