unit RiggVar.FederModel.Binding;

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
  System.Classes;

type
  TFederBinding = class
  public
    procedure InitInfoText(SL: TStrings);
    procedure InitHelpText(SL: TStrings);
    procedure InitHelpTextForIO(SL: TStrings);

    procedure InitNormalKeys(SL: TStrings);
    procedure InitSpecialKeys(SL: TStrings);

    procedure InitHullPoints(ML: TStrings);

    procedure InitNormalKeyInfo(SL: TStrings);
    procedure InitSpecialKeyInfo(SL: TStrings);
    procedure InitSplashText(HL: TStrings);
  end;

implementation

procedure TFederBinding.InitHelpTextForIO(SL: TStrings);
begin
  SL.Add('Using a TrimmFile as TrimmFileAuto manually from the app:');
  SL.Add('');
  SL.Add('TrimmFileAuto can be evaluated automatically at start up time.');
  SL.Add('But note that a manually created TrimmFile has priority.');
  SL.Add('');
  SL.Add('( You can copy TrimmFileAuto over to TrimmFile and edit. )');
  SL.Add('');
  SL.Add('Here is how it works:');
  SL.Add('');
  SL.Add('const');
  SL.Add('  TrimmFileName = ''Trimm-File.txt''');;
  SL.Add('  TrimmFileNameAuto = ''Trimm-File-Auto.txt;''');
  SL.Add('');
  SL.Add('{ Called when desktop application starts up. }');
  SL.Add('procedure TMain1.ReadTrimmFile0;');
  SL.Add('var');
  SL.Add('  fp, sTrimmFileAuto, sTrimmFile: string;');
  SL.Add('begin');
  SL.Add('  fp := GetTrimmFilePath; // UserDocumentsDir');
  SL.Add('  if fp <> '' then');
  SL.Add('  begin');
  SL.Add('    sTrimmFile := fp + TrimmFileName;');
  SL.Add('    sTrimmFileAuto := fp + TrimmFileNameAuto;');
  SL.Add('    if FileExists(sTrimmFile) then');
  SL.Add('      DoReadTrimmFile(sTrimmFile)');
  SL.Add('    else if FileExists(sTrimmFileAuto) then');
  SL.Add('      DoReadTrimmFile(sTrimmFileAuto)');
  SL.Add('  end;');
  SL.Add('end;');
  SL.Add('');
  SL.Add('{ Called when user touches button. }');
  SL.Add('procedure TMain1.SaveTrimmFile;');
  SL.Add('begin');
  SL.Add('  SaveTrimmFileAuto;');
  SL.Add('end;');
  SL.Add('');
  SL.Add('You could also drop a TrimmFile onto the drop target');
  SL.Add('  or paste via clipboard.');
end;

procedure TFederBinding.InitInfoText(SL: TStrings);
begin
  SL.Add('Info about Trimm 420 App');
  SL.Add('');
  SL.Add('see www.federgraph.de');
  SL.Add('see federgraph''s GitHub repository: RiggVar-RG38');

{$ifdef MSWINDOWS}
  SL.Add('');
  SL.Add('');
  SL.Add('-     F');
  SL.Add('-    * * *');
  SL.Add('-   *   *   G');
  SL.Add('-  *     * *   *');
  SL.Add('- E - - - H - - - I');
  SL.Add('-  *     * *         *');
  SL.Add('-   *   *   *           *');
  SL.Add('-    * *     *             *');
  SL.Add('-     D-------A---------------B');
  SL.Add('-              *');
  SL.Add('-              (C) federgraph.de');
  SL.Add('-');
{$endif}
end;

procedure TFederBinding.InitHelpText(SL: TStrings);
begin
  SL.Add('Help Text');
  SL.Add('=========');
  SL.Add('Tip: Use space bar instead of Enter in left List.');
  SL.Add('');
  SL.Add('ToDo: Update manually written shortcut list.');
  SL.Add('--- manually written key binding report below ---');
  SL.Add('');
  InitNormalKeys(SL);
  SL.Add('');
  InitSpecialKeys(SL);
end;

procedure TFederBinding.InitNormalKeys(SL: TStrings);
begin
  SL.Add('Taste a : faSalingA');
  SL.Add('Taste A : faFixpointA0');
  SL.Add('');
  SL.Add('Taste b : faBiegung');
  SL.Add('Taste B : faFixpointB0');
  SL.Add('');
  SL.Add('Taste c : faMastfallF0C');
  SL.Add('Taste C : faFixPointC0');
  SL.Add('');
  SL.Add('Taste d : faFixpointD');
  SL.Add('Taste D : faFixpointD0');
  SL.Add('');
  SL.Add('Taste e : faFixpointE');
  SL.Add('Taste E : faFixpointE0');
  SL.Add('');
  SL.Add('Taste f : faMastfallF0F');
  SL.Add('Taste F : faFixpointF0');
  SL.Add('');
  SL.Add('Taste g : faMastfallVorlauf');
  SL.Add('Taste G : ');
  SL.Add('');
  SL.Add('Taste h : faSalingH');
  SL.Add('Taste H : faToggleHelp');
  SL.Add('');
  SL.Add('Taste i : faWheelRight');
  SL.Add('Taste I : faWheelLeft');
  SL.Add('');
  SL.Add('Taste j : faWheelUp');
  SL.Add('Taste J : faWheelDown');
  SL.Add('');
  SL.Add('Taste k : faShowNormalKeyInfo');
  SL.Add('Taste K : faRggKoppel');
  SL.Add('');
  SL.Add('Taste l : faToggleShowLegend');
  SL.Add('Taste L : faMemeGotoLandscape');
  SL.Add('');
  SL.Add('Taste m : faMemoryBtn');
  SL.Add('Taste M : faCopyAndPaste');
  SL.Add('');
  SL.Add('Taste n : faShowNormalKeyInfo');
  SL.Add('Taste N : ');
  SL.Add('');
  SL.Add('Taste o : faWoben');
  SL.Add('Taste O : ');
  SL.Add('');
  SL.Add('Taste p : faPan');
  SL.Add('Taste P : faMemeGotoPortrait');
  SL.Add('');
  SL.Add('Taste q : faToggleAllText');
  SL.Add('Taste Q : ');
  SL.Add('');
  SL.Add('Taste r : faToggleReport');
  SL.Add('Taste R : faReadTrimmFile');
  SL.Add('');
  SL.Add('Taste s : faShowSpecialKeyInfo');
  SL.Add('Taste S : faMemeGotoSquare');
  SL.Add('');
  SL.Add('Taste t : faToggleFontColor');
  SL.Add('Taste T : faToggleSpeedPanel');
  SL.Add('');
  SL.Add('Taste u : faToggleDataText');
  SL.Add('Taste U : faToggleDiffText');
  SL.Add('');
  SL.Add('Taste v : faVorstag');
  SL.Add('Taste V : ');
  SL.Add('');
  SL.Add('Taste w : faWante');
  SL.Add('Taste W : ');
  SL.Add('');
  SL.Add('Taste x : ');
  SL.Add('Taste X : ');
  SL.Add('');
  SL.Add('Taste y : ');
  SL.Add('Taste Y : ');
  SL.Add('');
  SL.Add('Taste z : faShowInfoText');
  SL.Add('Taste Z : faUpdateTrimm0');
end;

procedure TFederBinding.InitSpecialKeys(SL: TStrings);
begin
  SL.Add('Taste 0 : faTrimm0');
  SL.Add('Taste 1 : faTrimm1');
  SL.Add('Taste 2 : faTrimm2');
  SL.Add('Taste 3 : faTrimm3');
  SL.Add('Taste 4 : faTrimm4');
  SL.Add('Taste 5 : faTrimm5');
  SL.Add('Taste 6 : faTrimm6');
  SL.Add('Taste 7 : fa420');
  SL.Add('Taste 8 : faLogo');
  SL.Add('');
  SL.Add('Taste * : faActionPageM');
  SL.Add('Taste + : faActionPageP');
  SL.Add('');
//  SL.Add('Taste ! : faShowNormalKeyInfo');
//  SL.Add('Taste " : faShowSpecialKeyInfo');
//  SL.Add('Taste § : faShowInfoText');
//  SL.Add('Taste $ : faShowDebugInfo');
  SL.Add('Taste ? : faShowHelpText');
//  SL.Add('Taste = : faShowZOrderInfo');
  SL.Add('Taste # : faActionPage4');
  SL.Add('');
  SL.Add('Taste ; : faRotaForm1');
  SL.Add('Taste : : faRotaForm2');
  SL.Add('Taste _ : faRotaForm3');
  SL.Add('');
  SL.Add('Taste , : faRotaForm1');
  SL.Add('Taste . : faRotaForm2');
  SL.Add('Taste - : faRotaForm3');
end;

(*
--- generated key binding report ---


061 = faActionPageE

*)

procedure TFederBinding.InitHullPoints(ML: TStrings);
begin
  ML.Add('procedure THullMeshData.GetPoints;');
  ML.Add('begin');
  ML.Add('  //Steven');
  ML.Add('  Punkt(4200, 0, 328); //Line 6');
  ML.Add('  Punkt(4194, 0, 260); //Line 5');
  ML.Add('  Punkt(4188, 0, 195); //Line 4');
  ML.Add('  Punkt(4178, 0, 128); //Line 3');
  ML.Add('  Punkt(4168, 0, 78); //Line 2');
  ML.Add('  Punkt(4151, 0, 26); //Line 1');
  ML.Add('  Punkt(4130, 0, 0); //Line 0');
  ML.Add('');
  ML.Add('  //Section 8 at 4100');
  ML.Add('  Punkt(4100, -157, 325);');
  ML.Add('  Punkt(4100, -149, 268);');
  ML.Add('  Punkt(4100, -126, 189);');
  ML.Add('  Punkt(4100, -100, 131);');
  ML.Add('  Punkt(4100, -69, 74);');
  ML.Add('  Punkt(4100, -30, 8);');
  ML.Add('  Punkt(4100, 0, -48);');
  ML.Add('  Punkt(4100, 30, 8);');
  ML.Add('  Punkt(4100, 69, 74);');
  ML.Add('  Punkt(4100, 100, 131);');
  ML.Add('  Punkt(4100, 126, 189);');
  ML.Add('  Punkt(4100, 149, 268);');
  ML.Add('  Punkt(4100, 157, 325);');
  ML.Add('');
  ML.Add('  //Spant 7');
  ML.Add('  Punkt(4000, -244, 322);');
  ML.Add('  Punkt(4000, -237, 263);');
  ML.Add('  Punkt(4000, -219, 186);');
  ML.Add('  Punkt(4000, -193, 115);');
  ML.Add('  Punkt(4000, -159, 51);');
  ML.Add('  Punkt(4000, -88, -41);');
  ML.Add('  Punkt(4000, 0, -117);');
  ML.Add('  Punkt(4000, 88, -41);');
  ML.Add('  Punkt(4000, 159, 51);');
  ML.Add('  Punkt(4000, 193, 115);');
  ML.Add('  Punkt(4000, 219, 186);');
  ML.Add('  Punkt(4000, 237, 263);');
  ML.Add('  Punkt(4000, 244, 322);');
  ML.Add('');
  ML.Add('  //Spant 6');
  ML.Add('  Punkt(3750, -402, 315);');
  ML.Add('  Punkt(3750, -387, 263);');
  ML.Add('  Punkt(3750, -374, 176);');
  ML.Add('  Punkt(3750, -345, 96);');
  ML.Add('  Punkt(3750, -281, 7);');
  ML.Add('  Punkt(3750, -155, -93);');
  ML.Add('  Punkt(3750, 0, -178);');
  ML.Add('  Punkt(3750, 155, -93);');
  ML.Add('  Punkt(3750, 281, 7);');
  ML.Add('  Punkt(3750, 345, 96);');
  ML.Add('  Punkt(3750, 374, 176);');
  ML.Add('  Punkt(3750, 387, 263);');
  ML.Add('  Punkt(3750, 402, 315);');
  ML.Add('');
  ML.Add('  //Spant 5');
  ML.Add('  Punkt(3400, -570, 308);');
  ML.Add('  Punkt(3400, -541, 253);');
  ML.Add('  Punkt(3400, -506, 166);');
  ML.Add('  Punkt(3400, -445, 42);');
  ML.Add('  Punkt(3400, -380, -30);');
  ML.Add('  Punkt(3400, -212, -126);');
  ML.Add('  Punkt(3400, 0, -202);');
  ML.Add('  Punkt(3400, 212, -126);');
  ML.Add('  Punkt(3400, 380, -30);');
  ML.Add('  Punkt(3400, 445, 42);');
  ML.Add('  Punkt(3400, 506, 166);');
  ML.Add('  Punkt(3400, 541, 253);');
  ML.Add('  Punkt(3400, 570, 308);');
  ML.Add('');
  ML.Add('  //Spant 4');
  ML.Add('  Punkt(3000, -699, 302);');
  ML.Add('  Punkt(3000, -661, 248);');
  ML.Add('  Punkt(3000, -619, 163);');
  ML.Add('  Punkt(3000, -539, 26);');
  ML.Add('  Punkt(3000, -446, -54);');
  ML.Add('  Punkt(3000, -251, -138);');
  ML.Add('  Punkt(3000, 0, -205);');
  ML.Add('  Punkt(3000, 251, -138);');
  ML.Add('  Punkt(3000, 446, -54);');
  ML.Add('  Punkt(3000, 539, 26);');
  ML.Add('  Punkt(3000, 619, 163);');
  ML.Add('  Punkt(3000, 661, 248);');
  ML.Add('  Punkt(3000, 699, 302);');
  ML.Add('');
  ML.Add('  //Spant 3');
  ML.Add('  Punkt(2400, -793, 297);');
  ML.Add('  Punkt(2400, -749, 245);');
  ML.Add('  Punkt(2400, -716, 167);');
  ML.Add('  Punkt(2400, -634, 31);');
  ML.Add('  Punkt(2400, -500, -65);');
  ML.Add('  Punkt(2400, -296, -135);');
  ML.Add('  Punkt(2400, 0, -191);');
  ML.Add('  Punkt(2400, 296, -135);');
  ML.Add('  Punkt(2400, 500, -65);');
  ML.Add('  Punkt(2400, 634, 31);');
  ML.Add('  Punkt(2400, 716, 167);');
  ML.Add('  Punkt(2400, 749, 245);');
  ML.Add('  Punkt(2400, 793, 297);');
  ML.Add('');
  ML.Add('  //Spant 2');
  ML.Add('  Punkt(1800, -800, 290);');
  ML.Add('  Punkt(1800, -755, 241);');
  ML.Add('  Punkt(1800, -725, 173);');
  ML.Add('  Punkt(1800, -634, 34);');
  ML.Add('  Punkt(1800, -480, -58);');
  ML.Add('  Punkt(1800, -269, -115);');
  ML.Add('  Punkt(1800, 0, -161);');
  ML.Add('  Punkt(1800, 269, -115);');
  ML.Add('  Punkt(1800, 480, -58);');
  ML.Add('  Punkt(1800, 634, 34);');
  ML.Add('  Punkt(1800, 725, 173);');
  ML.Add('  Punkt(1800, 755, 241);');
  ML.Add('  Punkt(1800, 800, 290);');
  ML.Add('');
  ML.Add('  //Spant 1');
  ML.Add('  Punkt(1000, -730, 275);');
  ML.Add('  Punkt(1000, -696, 237);');
  ML.Add('  Punkt(1000, -674, 185);');
  ML.Add('  Punkt(1000, -585, 56);');
  ML.Add('  Punkt(1000, -414, -23);');
  ML.Add('  Punkt(1000, -214, -64);');
  ML.Add('  Punkt(1000, 0, -97);');
  ML.Add('  Punkt(1000, 214, -64);');
  ML.Add('  Punkt(1000, 414, -23);');
  ML.Add('  Punkt(1000, 585, 56);');
  ML.Add('  Punkt(1000, 674, 185);');
  ML.Add('  Punkt(1000, 696, 237);');
  ML.Add('  Punkt(1000, 730, 275);');
  ML.Add('');
  ML.Add('  //Spiegel');
  ML.Add('  Punkt(0, -580, 250); //Line 6');
  ML.Add('  Punkt(0, -568, 226); //Line 5');
  ML.Add('  Punkt(0, -560, 187); //Line 4');
  ML.Add('  Punkt(0, -485, 89); //Line 3');
  ML.Add('  Punkt(0, -300, 30); //Line 2');
  ML.Add('  Punkt(0, -167, 13); //Line 1');
  ML.Add('  Punkt(0, 0, 0); //Basispunkt');
  ML.Add('  Punkt(0, 167, 13);');
  ML.Add('  Punkt(0, 300, 30);');
  ML.Add('  Punkt(0, 485, 89);');
  ML.Add('  Punkt(0, 560, 187);');
  ML.Add('  Punkt(0, 568, 226);');
  ML.Add('  Punkt(0, 580, 250);');
  ML.Add('end;');
  ML.Add('');
end;

procedure TFederBinding.InitNormalKeyInfo(SL: TStrings);
begin
  SL.Add('Normal Key Info (subset):');
  SL.Add('a : faSalingA');
  SL.Add('b : faBiegung');
  SL.Add('c : faMastfallF0C');
  SL.Add('f : faMastfallF0F');
  SL.Add('g : faMastfallVorlauf');
  SL.Add('h : faSalingH');
  SL.Add('o : faWoben');
  SL.Add('p : faPan');
  SL.Add('v : faVorstag');
  SL.Add('w : faWante');

  SL.Add('');
  SL.Add('A : faFixpointA0');
  SL.Add('B : faFixpointB0');
  SL.Add('d : faFixpointD');
  SL.Add('D : faFixpointD0');
  SL.Add('f : faFixpointF');
  SL.Add('F : faFixpointF0');

  SL.Add('');
  SL.Add('i : faWheelRight');
  SL.Add('I : faWheelLeft');
  SL.Add('j : faWheelUp');
  SL.Add('J : faWheelDown');

  SL.Add('');
  SL.Add('m : faMemoryBtn');
  SL.Add('M : faCopyAndPaste');
  SL.Add('R : faReadTrimmFile');
  SL.Add('T : faToggleSpeedPanel');
  SL.Add('Z : faUpdateTrimm0');
end;

procedure TFederBinding.InitSpecialKeyInfo(SL: TStrings);
begin
  SL.Add('Special Key Info (subset):');
  SL.Add('0 : faTrimm0');
  SL.Add('1 : faTrimm1');
  SL.Add('2 : faTrimm2');
  SL.Add('3 : faTrimm3');
  SL.Add('4 : faTrimm4');
  SL.Add('5 : faTrimm5');
  SL.Add('6 : faTrimm6');
  SL.Add('');
  SL.Add('7 : fa420');
  SL.Add('8 : faLogo');

  SL.Add('');
  SL.Add('* : faActionPageM');
  SL.Add('+ : faActionPageP');

  SL.Add('');
  SL.Add('! : ShowNormalKeyInfo');
  SL.Add('" : ShowSpecialKeyInfo');
  SL.Add('§ : ShowInfoText');
  SL.Add('= : faShowZOrder');
  SL.Add('? : faHelp');

  SL.Add('');
  SL.Add('# : faActionPage4');

  SL.Add('');
  SL.Add(', : faRotaForm1');
  SL.Add('. : faRotaForm2');
  SL.Add('- : faRotaForm3');
end;

procedure TFederBinding.InitSplashText(HL: TStrings);
begin
  HL.Clear;
  HL.Add('Control Text with Keys:');
  HL.Add('  n    - show Normal key info');
  HL.Add('  H    - toggle Help text');
  HL.Add('  q    - toggle All text');
  HL.Add('  r    - toggle Report text');
  HL.Add('  s    - show Special key info');
  HL.Add('  u    - toggle Data Text');
  HL.Add('  U    - toggle Diff Text');
  HL.Add('  z    - show Info text');
  HL.Add('  ?    - show Help text');
  HL.Add('');
  HL.Add('Select current parameter:');
  HL.Add('  with Button, Key, or in ListBox');
  HL.Add('');
  HL.Add('Change param value with Wheel!');
  HL.Add('  Wheel = small step');
  HL.Add('  Shift-Wheel  = bigger step');
  HL.Add('  Ctrl-Wheel = zoom in and out');
  HL.Add('');
  HL.Add('Goto stored Trimm');
  HL.Add('  1..8, 0 - Trimm selection');
  HL.Add('Change Format of Window');
  HL.Add('  L, P, S - Landscape, Portrait, Square');
  HL.Add('');
  HL.Add('Forms:');
  HL.Add('  FA - search in Actions');
  HL.Add('  FM - show Text in Memo');
  HL.Add('  FD - show documentation drawings');
end;

end.
