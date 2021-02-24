﻿program RG10;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmRG10 in 'App\FrmRG10.pas' {FormMain10},
  FrmAction in 'App\FrmAction.pas' {FormAction},
  FrmMemo in 'App\FrmMemo.pas' {FormMemo},
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.App.Model in 'App\RiggVar.App.Model.pas',
  RiggVar.App.Strings in 'App\RiggVar.App.Strings.pas',
  RiggVar.RG.Calc in 'RG\RiggVar.RG.Calc.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Doc in 'RG\RiggVar.RG.Doc.pas',
  RiggVar.RG.Fachwerk in 'RG\RiggVar.RG.Fachwerk.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Inter in 'RG\RiggVar.RG.Inter.pas',
  RiggVar.RG.Model in 'RG\RiggVar.RG.Model.pas',
  RiggVar.RG.Scroll in 'RG\RiggVar.RG.Scroll.pas',
  RiggVar.RG.Strings in 'RG\RiggVar.RG.Strings.pas',
  RiggVar.RG.TrimmTab in 'RG\RiggVar.RG.TrimmTab.pas',
  RiggVar.RG.Types in 'RG\RiggVar.RG.Types.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Controller in 'App\RiggVar.App.Controller.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  RggUnit1 in 'Core\RggUnit1.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit3 in 'Core\RggUnit3.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.ActionGroup in 'FB\RiggVar.FB.ActionGroup.pas',
  RiggVar.FB.ActionGroups in 'FB\RiggVar.FB.ActionGroups.pas',
  RiggVar.FB.ActionKeys in 'FB\RiggVar.FB.ActionKeys.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionName in 'FB\RiggVar.FB.ActionName.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.ActionTest in 'FB\RiggVar.FB.ActionTest.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.ActionTable in 'FB\RiggVar.FB.ActionTable.pas',
  RiggVar.FB.TextBase in 'FB\RiggVar.FB.TextBase.pas',
  RiggVar.FederModel.Action in 'Model\RiggVar.FederModel.Action.pas',
  RiggVar.FederModel.ActionList in 'Model\RiggVar.FederModel.ActionList.pas',
  RiggVar.FederModel.ActionMapPhone in 'Model\RiggVar.FederModel.ActionMapPhone.pas',
  RiggVar.FederModel.ActionMapTablet in 'Model\RiggVar.FederModel.ActionMapTablet.pas',
  RiggVar.FederModel.Binding in 'Model\RiggVar.FederModel.Binding.pas',
  RiggVar.FederModel.Keyboard01 in 'Model\RiggVar.FederModel.Keyboard01.pas',
  RiggVar.FederModel.Menu in 'Model\RiggVar.FederModel.Menu.pas',
  RiggVar.FederModel.Touch in 'Model\RiggVar.FederModel.Touch.pas',
  RiggVar.FederModel.TouchBase in 'Model\RiggVar.FederModel.TouchBase.pas',
  RiggVar.FederModel.TouchPhone in 'Model\RiggVar.FederModel.TouchPhone.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Rota in 'RG\RiggVar.RG.Rota.pas',
  RiggVar.Graph1.DisplayList in 'Graph1\RiggVar.Graph1.DisplayList.pas',
  RiggVar.Graph1.DisplayOrder in 'Graph1\RiggVar.Graph1.DisplayOrder.pas',
  RiggVar.Graph1.DisplayTypes in 'Graph1\RiggVar.Graph1.DisplayTypes.pas',
  RiggVar.Graph1.Hull in 'Graph1\RiggVar.Graph1.Hull.pas',
  RiggVar.Graph1.Rigg in 'Graph1\RiggVar.Graph1.Rigg.pas',
  RiggVar.Graph1.Rota in 'Graph1\RiggVar.Graph1.Rota.pas',
  RiggVar.Graph1.Transform in 'Graph1\RiggVar.Graph1.Transform.pas',
  RiggVar.RG.Speed03 in 'RG\RiggVar.RG.Speed03.pas',
  RiggVar.FB.SpeedBar in 'FB\RiggVar.FB.SpeedBar.pas',
  RiggVar.FB.SpeedColor in 'FB\RiggVar.FB.SpeedColor.pas',
  RiggVar.RG.View in 'RG\RiggVar.RG.View.pas',
  RiggVar.FD.Chart in 'Graph2\RiggVar.FD.Chart.pas',
  RiggVar.FD.Drawing00 in 'Graph2\RiggVar.FD.Drawing00.pas',
  RiggVar.FD.Drawings in 'Graph2\RiggVar.FD.Drawings.pas',
  RiggVar.FD.Elements in 'Graph2\RiggVar.FD.Elements.pas',
  RiggVar.FD.Image in 'Graph2\RiggVar.FD.Image.pas',
  RiggVar.FD.Rota in 'Graph2\RiggVar.FD.Rota.pas',
  RiggVar.FD.RotationHelper in 'Graph2\RiggVar.FD.RotationHelper.pas',
  RiggVar.FD.SchnittKK in 'Graph2\RiggVar.FD.SchnittKK.pas',
  RiggVar.FD.TransformHelper in 'Graph2\RiggVar.FD.TransformHelper.pas',
  RiggVar.RG.LocalizedStrings00 in 'RG\RiggVar.RG.LocalizedStrings00.pas',
  RiggVar.RG.LocalizedStringsDE in 'RG\RiggVar.RG.LocalizedStringsDE.pas',
  RiggVar.RG.LocalizedStringsEN in 'RG\RiggVar.RG.LocalizedStringsEN.pas',
  RiggVar.RG.LocalizedStrings in 'RG\RiggVar.RG.LocalizedStrings.pas',
  RiggVar.FB.ActionLongDE in 'FB\RiggVar.FB.ActionLongDE.pas',
  RiggVar.FB.ActionLongEN in 'FB\RiggVar.FB.ActionLongEN.pas',
  RiggVar.FB.ActionShortDE in 'FB\RiggVar.FB.ActionShortDE.pas',
  RiggVar.FB.ActionShortEN in 'FB\RiggVar.FB.ActionShortEN.pas',
  RiggVar.FB.ColorGroup in 'FB\RiggVar.FB.ColorGroup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain10, FormMain10);
  Application.Run;
end.
