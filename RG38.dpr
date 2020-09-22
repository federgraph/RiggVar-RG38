﻿program RG38;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  FrmAction in 'App\FrmAction.pas' {FormAction},
  FrmMemo in 'App\FrmMemo.pas' {FormMemo},
  FrmDrawing in 'App\FrmDrawing.pas' {FormDrawing},
  FrmConfig in 'Frm\FrmConfig.pas' {FormConfig},
  FrmTrimmTab in 'Frm\FrmTrimmTab.pas' {FormTrimmTab},
  RggCalc in 'Core\RggCalc.pas',
  RggDoc in 'Core\RggDoc.pas',
  RggFachwerk in 'Core\RggFachwerk.pas',
  RggReport in 'Core\RggReport.pas',
  RggSchnittKK in 'Core\RggSchnittKK.pas',
  RggScroll in 'Core\RggScroll.pas',
  RggTrimmTab in 'Core\RggTrimmTab.pas',
  RggTypes in 'Core\RggTypes.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  RggUnit1 in 'Core\RggUnit1.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit3 in 'Core\RggUnit3.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RggSchnittGG in 'Core\RggSchnittGG.pas',
  RggStrings in 'Core\RggStrings.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  RiggVar.App.Main1 in 'App\RiggVar.App.Main1.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.FB.ActionGroup in 'FB\RiggVar.FB.ActionGroup.pas',
  RiggVar.FB.ActionGroups in 'FB\RiggVar.FB.ActionGroups.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionKeys in 'FB\RiggVar.FB.ActionKeys.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionName in 'FB\RiggVar.FB.ActionName.pas',
  RiggVar.FB.SpeedBar in 'FB\RiggVar.FB.SpeedBar.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.ActionTable in 'FB\RiggVar.FB.ActionTable.pas',
  RiggVar.FB.ActionTest in 'FB\RiggVar.FB.ActionTest.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.SpeedColor in 'FB\RiggVar.FB.SpeedColor.pas',
  RiggVar.FB.TextBase in 'FB\RiggVar.FB.TextBase.pas',
  RiggVar.FederModel.Action in 'Model\RiggVar.FederModel.Action.pas',
  RiggVar.FederModel.ActionMapPhone in 'Model\RiggVar.FederModel.ActionMapPhone.pas',
  RiggVar.FederModel.ActionMapTablet in 'Model\RiggVar.FederModel.ActionMapTablet.pas',
  RiggVar.FederModel.Binding in 'Model\RiggVar.FederModel.Binding.pas',
  RiggVar.FederModel.Keyboard01 in 'Model\RiggVar.FederModel.Keyboard01.pas',
  RiggVar.FederModel.Touch in 'Model\RiggVar.FederModel.Touch.pas',
  RiggVar.FederModel.TouchBase in 'Model\RiggVar.FederModel.TouchBase.pas',
  RiggVar.FederModel.TouchPhone in 'Model\RiggVar.FederModel.TouchPhone.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Speed01 in 'RG\RiggVar.RG.Speed01.pas',
  RiggVar.RG.Speed02 in 'RG\RiggVar.RG.Speed02.pas',
  RiggVar.RG.Speed03 in 'RG\RiggVar.RG.Speed03.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RggDisplay in 'Graph\RggDisplay.pas',
  RggPolarKar in 'Graph\RggPolarKar.pas',
  RggRota in 'Graph\RggRota.pas',
  RggTestData in 'Graph\RggTestData.pas',
  RggBootGraph in 'Graph\RggBootGraph.pas',
  RggGraph in 'Graph\RggGraph.pas',
  RggRaumGraph in 'Graph\RggRaumGraph.pas',
  RggMatrix in 'Graph\RggMatrix.pas',
  RggProfile in 'Graph\RggProfile.pas',
  RggCtrls in 'Graph\RggCtrls.pas',
  RggHull in 'Graph\RggHull.pas',
  RggZug in 'Graph\RggZug.pas',
  RggTransformer in 'Graph\RggTransformer.pas',
  RggSaling3Eck in 'Core\RggSaling3Eck.pas',
  RggZug3D in 'Graph\RggZug3D.pas',
  RggDisplayOrder in 'Graph\RggDisplayOrder.pas',
  RggDisplayTypes in 'Graph\RggDisplayTypes.pas',
  RggChart in 'Graph\RggChart.pas',
  RggChartGraph in 'Graph\RggChartGraph.pas',
  RggTrimmTabGraph in 'Graph\RggTrimmTabGraph.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.FD.Chart in 'FD\RiggVar.FD.Chart.pas',
  RiggVar.FD.Elements in 'FD\RiggVar.FD.Elements.pas',
  RiggVar.FD.Drawings in 'FD\RiggVar.FD.Drawings.pas',
  RiggVar.FD.Drawing00 in 'FD\RiggVar.FD.Drawing00.pas',
  RiggVar.FD.Drawing01 in 'FD\RiggVar.FD.Drawing01.pas',
  RiggVar.FD.Drawing02 in 'FD\RiggVar.FD.Drawing02.pas',
  RiggVar.FD.Drawing03 in 'FD\RiggVar.FD.Drawing03.pas',
  RiggVar.FD.Drawing04 in 'FD\RiggVar.FD.Drawing04.pas',
  RiggVar.FD.Drawing05 in 'FD\RiggVar.FD.Drawing05.pas',
  RiggVar.FD.Drawing06 in 'FD\RiggVar.FD.Drawing06.pas',
  RiggVar.FD.Drawing07 in 'FD\RiggVar.FD.Drawing07.pas',
  RiggVar.FD.Drawing08 in 'FD\RiggVar.FD.Drawing08.pas',
  RiggVar.FD.Drawing09 in 'FD\RiggVar.FD.Drawing09.pas',
  RiggVar.FD.Drawing10 in 'FD\RiggVar.FD.Drawing10.pas',
  RiggVar.FD.Drawing11 in 'FD\RiggVar.FD.Drawing11.pas',
  RiggVar.FD.Drawing12 in 'FD\RiggVar.FD.Drawing12.pas',
  RiggVar.FD.Image in 'FD\RiggVar.FD.Image.pas',
  RiggVar.FD.RotationHelper in 'FD\RiggVar.FD.RotationHelper.pas',
  RiggVar.FD.SchnittKK in 'FD\RiggVar.FD.SchnittKK.pas',
  RiggVar.FD.Registry in 'FD\RiggVar.FD.Registry.pas',
  RiggVar.FZ.Registry in 'FZ\RiggVar.FZ.Registry.pas',
  RiggVar.FD.TransformHelper in 'FD\RiggVar.FD.TransformHelper.pas',
  RiggVar.FZ.Z01_Viereck in 'FZ\RiggVar.FZ.Z01_Viereck.pas',
  RiggVar.FZ.Z02_Logo in 'FZ\RiggVar.FZ.Z02_Logo.pas',
  RiggVar.FZ.Z03_Viergelenk in 'FZ\RiggVar.FZ.Z03_Viergelenk.pas',
  RiggVar.FZ.Z04_Tetraeder in 'FZ\RiggVar.FZ.Z04_Tetraeder.pas',
  RiggVar.FZ.Z05_TestRigg in 'FZ\RiggVar.FZ.Z05_TestRigg.pas',
  RiggVar.FZ.Z06_Hoehe in 'FZ\RiggVar.FZ.Z06_Hoehe.pas',
  RiggVar.FZ.Z07_Triangle in 'FZ\RiggVar.FZ.Z07_Triangle.pas',
  RiggVar.FZ.Z08_Arc in 'FZ\RiggVar.FZ.Z08_Arc.pas',
  RiggVar.FZ.Z09_Axis in 'FZ\RiggVar.FZ.Z09_Axis.pas',
  RiggVar.FZ.Z10_Lager in 'FZ\RiggVar.FZ.Z10_Lager.pas',
  RiggVar.FZ.Z11_Above in 'FZ\RiggVar.FZ.Z11_Above.pas',
  RiggVar.FZ.Z12_Atan2 in 'FZ\RiggVar.FZ.Z12_Atan2.pas',
  RiggVar.FZ.Z13_SchnittKK in 'FZ\RiggVar.FZ.Z13_SchnittKK.pas',
  RiggVar.FZ.Z14_SplitF in 'FZ\RiggVar.FZ.Z14_SplitF.pas',
  RiggVar.FZ.Z15_SchnittGG in 'FZ\RiggVar.FZ.Z15_SchnittGG.pas',
  RiggVar.FZ.Z16_Shrink in 'FZ\RiggVar.FZ.Z16_Shrink.pas',
  RiggVar.FZ.Z17_Feder in 'FZ\RiggVar.FZ.Z17_Feder.pas',
  RiggVar.FZ.Z18_BerechneWinkel in 'FZ\RiggVar.FZ.Z18_BerechneWinkel.pas',
  RiggVar.FZ.Z19_Chart in 'FZ\RiggVar.FZ.Z19_Chart.pas',
  RiggVar.FZ.Z20_Epsilon in 'FZ\RiggVar.FZ.Z20_Epsilon.pas',
  RiggVar.FZ.Z21_Rotations in 'FZ\RiggVar.FZ.Z21_Rotations.pas',
  RiggVar.FZ.Z22_BigArc in 'FZ\RiggVar.FZ.Z22_BigArc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'RG38';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
