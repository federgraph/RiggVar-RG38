program RG38;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  RggCalc in 'Core\RggCalc.pas',
  RggDoc in 'Core\RggDoc.pas',
  RggFachwerk in 'Core\RggFachwerk.pas',
  RggSchnittKK in 'Core\RggSchnittKK.pas',
  RggScroll in 'Core\RggScroll.pas',
  RggTrimmTab in 'Core\RggTrimmTab.pas',
  RggTypes in 'Core\RggTypes.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  RggUnit1 in 'Core\RggUnit1.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit3 in 'Core\RggUnit3.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.App.Main1 in 'App\RiggVar.App.Main1.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RggReport in 'Core\RggReport.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.TextBase in 'FB\RiggVar.FB.TextBase.pas',
  RiggVar.FederModel.Action in 'Model\RiggVar.FederModel.Action.pas',
  RiggVar.FederModel.ActionMapPhone in 'Model\RiggVar.FederModel.ActionMapPhone.pas',
  RiggVar.FederModel.ActionMapTablet in 'Model\RiggVar.FederModel.ActionMapTablet.pas',
  RiggVar.FederModel.Touch in 'Model\RiggVar.FederModel.Touch.pas',
  RiggVar.FederModel.TouchBase in 'Model\RiggVar.FederModel.TouchBase.pas',
  RiggVar.FederModel.TouchPhone in 'Model\RiggVar.FederModel.TouchPhone.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
