﻿program RG76;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmDrawing in 'App\FrmDrawing.pas' {FormDrawing},
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.FD.RotationHelper in 'Graph2\RiggVar.FD.RotationHelper.pas',
  RiggVar.FD.Elements in 'Graph2\RiggVar.FD.Elements.pas',
  RiggVar.FD.Drawings in 'Graph2\RiggVar.FD.Drawings.pas',
  RiggVar.FD.Chart in 'Graph2\RiggVar.FD.Chart.pas',
  RiggVar.FD.TransformHelper in 'Graph2\RiggVar.FD.TransformHelper.pas',
  RiggVar.FD.SchnittKK in 'Graph2\RiggVar.FD.SchnittKK.pas',
  RiggVar.FD.Image in 'Graph2\RiggVar.FD.Image.pas',
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
  RiggVar.FD.Registry in 'FD\RiggVar.FD.Registry.pas',
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
  RiggVar.FZ.Registry in 'FZ\RiggVar.FZ.Registry.pas',
  RiggVar.FZ.Z17_Feder in 'FZ\RiggVar.FZ.Z17_Feder.pas',
  RiggVar.FZ.Z18_BerechneWinkel in 'FZ\RiggVar.FZ.Z18_BerechneWinkel.pas',
  RiggVar.FZ.Z19_Chart in 'FZ\RiggVar.FZ.Z19_Chart.pas',
  RiggVar.FZ.Z20_Epsilon in 'FZ\RiggVar.FZ.Z20_Epsilon.pas',
  RiggVar.FZ.Z21_Rotations in 'FZ\RiggVar.FZ.Z21_Rotations.pas',
  RiggVar.FZ.Z22_BigArc in 'FZ\RiggVar.FZ.Z22_BigArc.pas',
  RiggVar.FZ.Z23_Federgraph in 'FZ\RiggVar.FZ.Z23_Federgraph.pas',
  RiggVar.FZ.Z24_Template in 'FZ\RiggVar.FZ.Z24_Template.pas',
  RiggVar.RG.Calc in 'RG\RiggVar.RG.Calc.pas',
  RiggVar.RG.Types in 'RG\RiggVar.RG.Types.pas',
  RiggVar.FederModel.Circle in 'Model\RiggVar.FederModel.Circle.pas',
  RiggVar.FB.Equation in 'FB\RiggVar.FB.Equation.pas',
  RiggVar.FB.Formula in 'FB\RiggVar.FB.Formula.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.ColorGroup in 'FB\RiggVar.FB.ColorGroup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDrawing, FormDrawing);
  Application.Run;
end.
