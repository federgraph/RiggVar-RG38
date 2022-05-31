program SK03;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmSK03 in 'SK\FrmSK03.pas' {FormMain},
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Calc in 'RG\RiggVar.RG.Calc.pas',
  RiggVar.RG.SchnittGG in 'RG\RiggVar.RG.SchnittGG.pas',
  RiggVar.RG.Types in 'RG\RiggVar.RG.Types.pas',
  RiggVar.SK.Model01 in 'SK\RiggVar.SK.Model01.pas',
  RiggVar.SK.Controller in 'SK\RiggVar.SK.Controller.pas',
  RiggVar.SK.Main in 'SK\RiggVar.SK.Main.pas',
  RiggVar.SK.Process in 'SK\RiggVar.SK.Process.pas',
  RiggVar.SK.Graph in 'SK\RiggVar.SK.Graph.pas',
  RiggVar.SK.Graph03 in 'SK\RiggVar.SK.Graph03.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
