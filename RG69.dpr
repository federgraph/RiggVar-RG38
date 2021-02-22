program RG69;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmColor in 'App\FrmColor.pas' {FormColor},
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.FB.ColorGroup in 'FB\RiggVar.FB.ColorGroup.pas',
  RiggVar.FB.ColorList in 'FB\RiggVar.FB.ColorList.pas',
  RiggVar.FB.ColorListBox in 'FB\RiggVar.FB.ColorListBox.pas',
  RiggVar.FB.ListboxPatcher in 'FB\RiggVar.FB.ListboxPatcher.pas',
  RiggVar.FB.ColorListBoxWeb in 'FB\RiggVar.FB.ColorListBoxWeb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormColor, FormColor);
  Application.Run;
end.
