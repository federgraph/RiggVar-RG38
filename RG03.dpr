program RG03;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmRG03 in 'App\FrmRG03.pas' {FormMain},
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Model in 'App\RiggVar.App.Model.pas',
  RiggVar.App.Strings in 'App\RiggVar.App.Strings.pas',
  RiggVar.App.Process in 'App\RiggVar.App.Process.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FD.Image in 'Graph2\RiggVar.FD.Image.pas',
  RiggVar.Graph1.Rigg in 'Graph1\RiggVar.Graph1.Rigg.pas',
  RiggVar.Graph1.Rota in 'Graph1\RiggVar.Graph1.Rota.pas',
  RiggVar.Graph1.Transform in 'Graph1\RiggVar.Graph1.Transform.pas',
  RiggVar.RG.Calc in 'RG\RiggVar.RG.Calc.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Doc in 'RG\RiggVar.RG.Doc.pas',
  RiggVar.RG.Fachwerk in 'RG\RiggVar.RG.Fachwerk.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Inter in 'RG\RiggVar.RG.Inter.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.RG.TrimmTab in 'RG\RiggVar.RG.TrimmTab.pas',
  RiggVar.RG.Types in 'RG\RiggVar.RG.Types.pas',
  RiggVar.RG.Scroll in 'RG\RiggVar.RG.Scroll.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.RG.Model in 'RG\RiggVar.RG.Model.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.LocalizedStrings in 'RG\RiggVar.RG.LocalizedStrings.pas',
  RiggVar.RG.LocalizedStrings00 in 'RG\RiggVar.RG.LocalizedStrings00.pas',
  RiggVar.RG.LocalizedStringsDE in 'RG\RiggVar.RG.LocalizedStringsDE.pas',
  RiggVar.RG.LocalizedStringsEN in 'RG\RiggVar.RG.LocalizedStringsEN.pas',
  RiggVar.RG.Strings in 'RG\RiggVar.RG.Strings.pas',
  RiggVar.RG.View in 'RG\RiggVar.RG.View.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Rota in 'RG\RiggVar.RG.Rota.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
