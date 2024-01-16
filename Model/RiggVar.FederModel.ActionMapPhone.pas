unit RiggVar.FederModel.ActionMapPhone;

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
  TActionMapPhone = class(TActionMap)
  private
    cla: TAlphaColor;
  protected
    procedure InitActionsRG(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]

[9][1][2][3]----[10]
--------------------
--------------------
--------------------
--------------------
[7]-----[4][5][6][8]
}

implementation

const
  claRotaForm = claCornflowerblue;
  claForm = claCornflowerblue;
  claParam = claPlum;
  claSalingParam = claLime;
  claParamValue = claWhite;
  claFixPoint = claBeige;
  claViewPoint = claWhite;
  claSample = claTeal;
  claOption = claBeige;
  claSuperOption = claGreen;
  claRenderOption = claBeige;
  claHull = claOption;
  claReport = claDimgray;
  claData = claWhite;
  claMemory = claDimgray;
  claNavigation = claYellow;
  claColorScheme = claDimgray;

  claAlarm = claCoral;
  claAttention = claPink;

  PageCountRG = 14;
  EscapePageCountRG = 11;

constructor TActionMapPhone.Create;
begin
  inherited;
  FPageCount := PageCountRG;
  FEscapeIndex := PageCountRG + 1;
  TestName := 'Phone Page';
end;

procedure TActionMapPhone.InitActions(Layout: Integer);
begin
  InitActionsRG(Layout);
end;

procedure TActionMapPhone.InitActionsRG(Layout: Integer);
begin
{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]

[9][1][2][3]----[10]
--------------------
--------------------
--------------------
--------------------
[7]-----[4][5][6][8]
}

  case Layout of
    1:
    begin
      cla := claParam;
      IAC(1, faVorstag, cla);
      IAC(2, faWante, cla);
      IAC(3, faMastfussD0X, cla);

      IAC(4, faController, cla);
      IAC(5, faWOben, cla);
      IAC(6, faMastfallVorlauf, cla);

      IAC(7, faBiegung, cla);
      IAC(8, faMastfallF0C, cla);
    end;

    2:
    begin
      cla := claSalingParam;
      IAC(1, faSalingL, cla);
      IAC(2, faSalingW, cla);
      IAC(3, faNoop, claBeige);

      IAC(4, faNoop, claWhite);
      IAC(5, faSalingH, cla);
      IAC(6, faSalingA, cla);

      cla := claTeal;
      IAC(7, fa420, cla);
      IAC(8, faLogo, cla);
    end;

    3:
    begin
      cla := claFixPoint;
      IAC(1, faFixpointA, cla);
      IAC(2, faFixpointB, cla);
      IAC(3, faFixpointC, cla);

      IAC(4, faFixpointD, cla);
      IAC(5, faFixpointE, cla);
      IAC(6, faFixpointF, cla);

      IAC(7, faMastfallF0F, claParam);
      IAC(8, faBiegung, claParam);
    end;

    4:
    begin
      cla := claFixPoint;
      IAC(1, faFixpointA0, cla);
      IAC(2, faFixpointB0, cla);
      IAC(3, faFixpointC0, cla);

      IAC(4, faFixpointD0, cla);
      IAC(5, faFixpointE0, cla);
      IAC(6, faFixpointF0, cla);

      IAC(7, faCycleColorSchemeM, claColorScheme);
      IAC(8, faCycleColorSchemeP, claColorScheme);
    end;

    5:
    begin
      cla := claWhite;
      IAC(1, faNoop, cla);
      IAC(2, faNoop, cla);
      IAC(3, faNoop, cla);

      IAC(4, faReportTrimmText, claReport);
      IAC(5, faReportDataText, claReport);
      IAC(6, faReportDiffText, claReport);

      IAC(7, faShowInfoText, claCornflowerblue);
      IAC(8, faReportNone, claReport);
    end;

    6:
    begin
      cla := claWhite;
      IAC(1, faReset, cla);
      IAC(2, faRggHull, claOption);
      IAC(3, fa420, claSample);

      IAC(4, faViewpointS, claViewPoint);
      IAC(5, faViewpointA, claViewPoint);
      IAC(6, faViewpointT, claViewPoint);

      IAC(7, faActionPage1, claNavigation);
      IAC(8, faViewpoint3, claViewPoint);
    end;

    7:
    begin
      cla := claSample;
      IAC(1, faTrimm1, cla);
      IAC(2, faTrimm2, cla);
      IAC(3, faTrimm3, cla);

      IAC(4, faTrimm4, cla);
      IAC(5, faTrimm5, cla);
      IAC(6, faTrimm6, cla);

      IAC(7, fa420, cla);
      IAC(8, faLogo, cla);
    end;

    8:
    begin
      cla := claWhite;
      IAC(1, faNoop, cla);
      IAC(2, faNoop, cla);
      IAC(3, faCopyAndPaste, claData);

      IAC(4, faCopyTrimmFile, claData);
      IAC(5, faCopyTrimmItem, claData);
      IAC(6, faPasteTrimmItem, claData);

      IAC(7, faMemoryBtn, claMemory);
      IAC(8, faMemoryRecallBtn, claMemory);
    end;

    9:
    begin
      cla := claWhite;
      IAC(1, faParamValueMinus10, claParamValue);
      IAC(2, faParamValuePlus10, claParamValue);
      IAC(3, faNoop, cla);

      IAC(4, faParamValueMinus1, claParamValue);
      IAC(5, faParamValuePlus1, claParamValue);
      IAC(6, faNoop, cla);

      IAC(7, faReportTrimmText, claReport);
      IAC(8, faReportDiffText, claReport);
    end;

    10:
    begin
      cla := claAttention;
      IAC(1, faRggBogen, claOption);
      IAC(2, faRggKoppel, claOption);
      IAC(3, faRggHull, claHull);

      IAC(4, faRotaForm1, claRotaForm);
      IAC(5, faRotaForm2, claRotaForm);
      IAC(6, faRotaForm3, claRotaForm);

      IAC(7, faToggleLanguage, cla);
      IAC(8, faDemo, cla);
    end;

    11:
    begin
      cla := claWhite;
      IAC(1, faSuperGrau, claSuperOption);
      IAC(2, faSuperMulti, claSuperOption);
      IAC(3, faSuperDisplay, claSuperOption);

      IAC(4, faSuperSimple, claSuperOption);
      IAC(5, faSuperNormal, claSuperOption);
      IAC(6, faSuperBlau, claSuperOption);

      IAC(7, faMemoryBtn, claMemory);
      IAC(8, faRotaForm1, claRotaForm);
    end;

    12:
    begin
      cla := claWhite;
      IAC(1, faWantRenderE, claRenderOption);
      IAC(2, faWantRenderH, claRenderOption);
      IAC(3, faWantRenderP, claRenderOption);

      IAC(4, faNoop, cla);
      IAC(5, faWantRenderS, claRenderOption);
      IAC(6, faWantRenderF, claRenderOption);

      IAC(7, faActionPage1, claNavigation);
      IAC(8, faRotaForm2, claRotaForm);
    end;

    13:
    begin
      cla := claWhite;
      IAC(1, faParamValuePlus10, claParamValue);
      IAC(2, faParamValueMinus10, claParamValue);
      IAC(3, faNoop, cla);

      IAC(4, faParamValuePlus1, claParamValue);
      IAC(5, faParamValueMinus1, claParamValue);
      IAC(6, faNoop, cla);

      IAC(7, faShowActions, claForm);
      IAC(8, faShowMemo, claForm);
    end;

    14:
    begin
      cla := claWhite;
      IAC(1, faMB01, cla);
      IAC(2, faMB02, cla);
      IAC(3, faMB03, cla);

      IAC(4, faMB04, cla);
      IAC(5, faMB05, cla);
      IAC(6, faMB06, cla);

      IAC(7, faMB07, cla);
      IAC(8, faMB08, cla);
    end;

  end;
end;

end.

