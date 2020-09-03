unit RggUnit4;

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
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  RggStrings,
  RggTypes,
  RggUnit3,
  RggDoc,
  RggCalc,
  RiggVar.RG.Data;

type
  TRigg = class(TRiggFS)
  private
    function GetRealTrimm(Index: TTrimmIndex): single;
  public
{$ifdef MSWindows}
    procedure WriteXml(ML: TStrings; AllTags: Boolean = False);
{$endif}
    procedure AusgabeText(ML: TStrings; WantAll: Boolean = True);
    procedure AusgabeKommentar(ML: TStrings);

    procedure SaveToFederData(fd: TRggData);
    procedure LoadFromFederData(fd: TRggData);
    procedure WriteToDocFile(FileName: string);
    procedure LoadFromDocFile(FileName: string);
    procedure Assign(Source: TPersistent); override;
    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);
    procedure SetDefaultDocument;
    procedure GetRealTrimmRecord(var RealTrimm: TRealTrimm);
    property RealTrimm[Index: TTrimmIndex]: single read GetRealTrimm;
    property Trimm: TTrimm read FTrimm;
  end;

implementation

{ TRigg }

{
  procedure TRigg.WriteToIniFile(FileName: String);
  var
  IniFile: TIniFile;
  begin
    IniFile := TIniFile.Create(FileName);
    try
      inherited WriteToIniFile(IniFile);
    finally
      IniFile.Free;
    end;
  end;

  procedure TRigg.LoadFromIniFile(FileName: String);
  var
    IniFile: TIniFile;
  begin
    IniFile := TIniFile.Create(FileName);
    try
      inherited LoadFromIniFile(IniFile);
    finally
      IniFile.Free;
    end;
  end;
}

{$ifdef MSWindows}
procedure TRigg.WriteXml(ML: TStrings; AllTags: Boolean);
var
  Document: TRggDocument;
begin
  Document := TRggDocument.Create;
  Document.WantFestigkeitsWerteInXml := AllTags;
  Document.WantTrimmTabInXml := AllTags;
  try
    GetDocument(Document);
    Document.WriteXML(ML);
  finally
    Document.Free;
  end;
end;
{$endif}

procedure TRigg.WriteToDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    GetDocument(Document);
    s := ExtractFileExt(FileName);
    if s = RGI_File_Extension then
    begin
      Document.WriteToIniFile(FileName);
    end
    else if s = RGG_File_Extension then
    begin
      { write as .rgi, .rgg no longer supported }
      s := ChangeFileExt(FileName, RGI_File_Extension);
      Document.WriteToIniFile(s);
      // Document.SaveToFile(FileName);
    end;
  finally
    Document.Free;
  end;
end;

procedure TRigg.LoadFromDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    s := ExtractFileExt(FileName);
    if s = RGI_File_Extension then
    begin
      Document.LoadFromIniFile(FileName);
      SetDocument(Document);
    end;
    // if S = RGG_File_Extension then
    // begin
    //   Document.LoadFromFile(FileName);
    //   SetDocument(Document);
    // end;
  finally
    Document.Free;
  end;
end;

procedure TRigg.Assign(Source: TPersistent);
var
  Document: TRggDocument;
begin
  if Source is TRigg then
  begin
    Document := TRggDocument.Create;
    try
      (Source as TRigg).GetDocument(Document);
      SetDocument(Document);
    finally
      Document.Free;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TRigg.GetDocument(Doc: TRggDocument);
begin
  UpdateGSB;
  { Rigg: Typ }
  Doc.SalingTyp := SalingTyp;
  Doc.ControllerTyp := ControllerTyp;
  Doc.CalcTyp := CalcTyp;
  { Mast: Abmessungen }
  Doc.FiMastL := Round(FrMastLength);
  Doc.FiMastunten := Round(FrMastUnten);
  Doc.FiMastoben := Round(FrMastOben);
  Doc.FiMastfallVorlauf := Round(FrMastfallVorlauf);
  Doc.FiControllerAnschlag := Round(FiControllerAnschlag);
  { Rumpf: Koordinaten }
  Doc.iP := rP;
  { Festigkeitswerte }
  Doc.rEA := rEA;
  Doc.EI := EI;
  { Grenzwerte und Istwerte }
  Doc.GSB.Assign(GSB);
  { Trimmtabelle }
  Doc.TrimmTabDaten := TrimmTab.TrimmTabDaten;
end;

procedure TRigg.SetDocument(Doc: TRggDocument);
var
  InputRec: TTrimmControls;
  tempManipulatorMode: Boolean;
begin
  { Mast: Abmessungen }
  FrMastLength := Doc.FiMastL;
  FrMastUnten := Doc.FiMastunten;
  FrMastOben := Doc.FiMastoben;
  FrMastfallVorlauf := Doc.FiMastfallVorlauf;
  FiControllerAnschlag := Doc.FiControllerAnschlag;
  { Rumpf: Koordinaten }
  rP := Doc.iP;
  { Festigkeitswerte }
  rEA := Doc.rEA;
  EI := Doc.EI;
  { Grenzwerte }
  GSB.Assign(Doc.GSB);
  { Trimmtabelle }
  TrimmTab.TrimmTabDaten := Doc.TrimmTabDaten;
  { Istwerte }
  InputRec.Controller := Round(Doc.GSB.Controller.Ist);
  InputRec.Winkel := Round(Doc.GSB.Winkel.Ist);
  InputRec.Vorstag := Round(Doc.GSB.Vorstag.Ist);
  InputRec.Wanten := Round(Doc.GSB.Wante.Ist);
  InputRec.Woben := Round(Doc.GSB.Woben.Ist);
  InputRec.Wunten := Round(InputRec.Wanten - InputRec.Woben);
  InputRec.SalingH := Round(Doc.GSB.SalingH.Ist);
  InputRec.SalingA := Round(Doc.GSB.SalingA.Ist);
  InputRec.SalingL := Round(Doc.GSB.SalingL.Ist);
  InputRec.Vorstag := Round(Doc.GSB.VorstagOS.Ist);
  InputRec.WPowerOS := Round(Doc.GSB.WPowerOS.Ist);
  Glieder := InputRec; { --> IntGliederToReal }
  Reset; { restliche Gleitkommawerte für Rumpf und Mast aktualisieren }

  { Rigg: Typ }
  SalingTyp := Doc.SalingTyp;
  ControllerTyp := Doc.ControllerTyp;
  CalcTyp := Doc.CalcTyp;

  tempManipulatorMode := ManipulatorMode;
  ManipulatorMode := false;
  UpdateGetriebe;
  UpdateRigg;
  ManipulatorMode := tempManipulatorMode;

  UpdateGSB;
end;

procedure TRigg.SetDefaultDocument;
var
  Document: TRggDocument;
begin
  Document := TRggDocument.Create;
  try
   Document.GetDefaultDocument;
   SetDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TRigg.GetRealTrimmRecord(var RealTrimm: TRealTrimm);
{ Die Funktion überprüft nicht, ob die Werte in Rigg aktualisiert sind.
  Einige Werte stehen schon nach Aufruf von UpdateGetriebe() zur Verfügung.
  Andere erst nach Aufruf von UpdateRigg(). }
begin
  { Auslenkung und Wantenspannung }
  RealTrimm.VorstagDiff := VorstagDiff;
  RealTrimm.SpannungW := SpannungW;
  with RealTrimm do
   begin
    { Mastfall }
    MastfallF0F := (rP[ooF0] - rP[ooF]).Length; { in mm }
    { Vorstagspannung }
    if abs(rF[14]) < 32000 then
      SpannungV := rF[14] { in N }
    else
    begin
      if rF[14] > 32000 then
        SpannungV := 32000;
      if rF[14] < -32000 then
        SpannungV := -32000;
    end;
    { Biegung an den Salingen }
    BiegungS := hd; { in mm }
    { Biegung am Controller }
    BiegungC := he; { in mm }
    { "Elastizität" }
    FlexWert := (rP[ooC] - rPe[ooC]).Length; { in mm }
  end;
end;

function TRigg.GetRealTrimm(Index: TTrimmIndex): single;
var
  temp: single;
begin
  temp := 0;
  case Index of
    tiMastfallF0F: temp := (rP[ooF0] - rP[ooF]).Length;
    tiMastfallF0C: temp := (rP[ooF0] - rP[ooC]).Length;
    tiVorstagDiff: temp := VorstagDiff;
    tiVorstagDiffE: temp := (rPe[ooC0] - rPe[ooC]).Length - rL[14];
    tiSpannungW: temp := SpannungW;
    tiSpannungV:
      begin
        if abs(rF[14]) < 32000 then
          temp := rF[14] { in N }
        else
        begin
          if rF[14] > 32000 then
            temp := 32000;
          if rF[14] < -32000 then
            temp := -32000;
      end;
    end;
    tiBiegungS: temp := hd;
    tiBiegungC: temp := he;
    tiFlexWert: temp := (rP[ooC] - rPe[ooC]).Length; { in mm }
  end;
  result := temp;
end;

procedure TRigg.SaveToFederData(fd: TRggData);
begin
  fd.A0X := Round(rP[ooA0].X);
  fd.A0Y := Round(rP[ooA0].Y);
  fd.A0Z := Round(rP[ooA0].Z);

  fd.C0X := Round(rP[ooC0].X);
  fd.C0Y := Round(rP[ooC0].Y);
  fd.C0Z := Round(rP[ooC0].Z);

  fd.D0X := Round(rP[ooD0].X);
  fd.D0Y := Round(rP[ooD0].Y);
  fd.D0Z := Round(rP[ooD0].Z);

  fd.E0X := Round(rP[ooE0].X);
  fd.E0Y := Round(rP[ooE0].Y);
  fd.E0Z := Round(rP[ooE0].Z);

  fd.F0X := Round(rP[ooF0].X);
  fd.F0Y := Round(rP[ooF0].Y);
  fd.F0Z := Round(rP[ooF0].Z);

  fd.MU := Round(FrMastUnten);
  fd.MO := Round(FrMastOben);
  fd.ML := Round(FrMastLength);
  fd.MV := Round(MastfallVorlauf);
  fd.CA := Round(FiControllerAnschlag);

  fd.CPMin := Round(GSB.Controller.Min);
  fd.CPPos := Round(GSB.Controller.Ist);
  fd.CPMax := Round(GSB.Controller.Max);

  fd.SHMin := Round(GSB.SalingH.Min);
  fd.SHPos := Round(GSB.SalingH.Ist);
  fd.SHMax := Round(GSB.SalingH.Max);

  fd.SAMin := Round(GSB.SalingA.Min);
  fd.SAPos := Round(GSB.SalingA.Ist);
  fd.SaMax := Round(GSB.SalingA.Max);

  fd.SLMin := Round(GSB.SalingL.Min);
  fd.SLPos := Round(GSB.SalingL.Ist);
  fd.SLMax := Round(GSB.SalingL.Max);

  fd.VOMin := Round(GSB.Vorstag.Min);
  fd.VOPos := Round(GSB.Vorstag.Ist);
  fd.VOMax := Round(GSB.Vorstag.Max);

  fd.WIMin := Round(GSB.Winkel.Min);
  fd.WIPos := Round(GSB.Winkel.Ist);
  fd.WIMax := Round(GSB.Winkel.Max);

  fd.WLMin := Round(GSB.Wante.Min);
  fd.WLPos := Round(GSB.Wante.Ist);
  fd.WLMax := Round(GSB.Wante.Max);

  fd.WOMin := Round(GSB.Woben.Min);
  fd.WOPos := Round(GSB.Woben.Ist);
  fd.WOMax := Round(GSB.Woben.Max);

  fd.F0C := Round(RealTrimm[tiMastfallF0C]);
  fd.F0F := Round(RealTrimm[tiMastfallF0F]);
  fd.Bie := Round(RealTrimm[tiBiegungS]);
end;

procedure TRigg.LoadFromFederData(fd: TRggData);
var
  InputRec: TTrimmControls;
  tempManipulatorMode: Boolean;
begin
  { Mast: Abmessungen }
  FrMastLength := fd.ML;
  FrMastUnten := fd.MU;
  FrMastOben := fd.MO;
  FiControllerAnschlag := fd.CA;
  MastfallVorlauf := fd.MV;

  { Rumpf: Koordinaten }

  //rP := Doc.iP;
  rP[ooA0].X := fd.A0X;
  rP[ooA0].Y := -fd.A0Y;
  rP[ooA0].Z := fd.A0Z;
  rP[ooB0].X := fd.A0X;
  rP[ooB0].Y := fd.A0Y;
  rP[ooB0].Z := fd.A0Z;
  rP[ooC0].X := fd.C0X;
  rP[ooC0].Y := fd.C0Y;
  rP[ooC0].Z := fd.C0Z;
  rP[ooD0].X := fd.D0X;
  rP[ooD0].Y := fd.D0Y;
  rP[ooD0].Z := fd.D0Z;
  rP[ooE0].X := fd.E0X;
  rP[ooE0].Y := fd.E0Y;
  rP[ooE0].Z := fd.E0Z;
  rP[ooF0].X := fd.F0X;
  rP[ooF0].Y := fd.F0Y;
  rP[ooF0].Z := fd.F0Z;
  rP[ooP0].X := fd.A0X;
  rP[ooP0].Y := 0;
  rP[ooP0].Z := fd.A0Z;

  rP[ooP].X := fd.A0X;
  rP[ooP].Y := 0;
  rP[ooP].Z := fd.A0Z;

  { Festigkeitswerte }
//  rEA := Doc.rEA;
//  EI := Doc.EI;

  { Grenzwerte }
  GSB.Controller.Min := fd.CPMin;
  GSB.Controller.Ist := fd.CPPos;
  GSB.Controller.Max := fd.CPMax;
  GSB.Winkel.Min := fd.WIMin;
  GSB.Winkel.Ist := fd.WIPos;
  GSB.Winkel.Max := fd.WIMax;
  GSB.Vorstag.Min := fd.VOMin;
  GSB.Vorstag.Ist := fd.VOPos;
  GSB.Vorstag.Max := fd.VOMax;
  GSB.Wante.Min := fd.WLMin;
  GSB.Wante.Ist := fd.WLPos;
  GSB.Wante.Max := fd.WLMax;
  GSB.Woben.Min := fd.WOMin;
  GSB.Woben.Ist := fd.WOPos;
  GSB.Woben.Max := fd.WOMax;
  GSB.SalingH.Min := fd.SHMin;
  GSB.SalingH.Ist := fd.SHPos;
  GSB.SalingH.Max := fd.SHMax;
  GSB.SalingA.Min := fd.SAMin;
  GSB.SalingA.Ist := fd.SAPos;
  GSB.SalingA.Max := fd.SAMax;
  GSB.SalingL.Min := fd.SLMin;
  GSB.SalingL.Ist := fd.SLPos;
  GSB.SalingL.Max := fd.SLMax;
  GSB.VorstagOS.Min := fd.VOMin;
  GSB.VorstagOS.Ist := fd.VOPos;
  GSB.VorstagOS.Max := fd.VOMax;
//  GSB.WPowerOS.Min := 0;
//  GSB.WPowerOS.Ist := 0;
//  GSB.WPowerOS.Max := 0;

  { Trimmtabelle }
//  TrimmTab.TrimmTabDaten := Doc.TrimmTabDaten;
  { Istwerte }
  InputRec.Controller := fd.CPPos;
  InputRec.Winkel := fd.WIPos;
  InputRec.Vorstag := fd.VOPos;
  InputRec.Wanten := fd.WLPos;
  InputRec.Woben := fd.WOPos;
  InputRec.Wunten := fd.WLPos - fd.WOPos;
  InputRec.SalingH := fd.SHPos;
  InputRec.SalingA := fd.SAPos;
  InputRec.SalingL := fd.SLPos;
  InputRec.Vorstag := fd.VOPos;
  InputRec.WPowerOS := 0;
  Glieder := InputRec; { --> IntGliederToReal }
  Reset; { restliche Gleitkommawerte für Rumpf und Mast aktualisieren }

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctOhne;
  CalcTyp := ctKraftGemessen;

  tempManipulatorMode := ManipulatorMode;
  ManipulatorMode := false;
  UpdateGetriebe;
  UpdateRigg;
  ManipulatorMode := tempManipulatorMode;

  UpdateGSB;
end;

procedure TRigg.AusgabeText(ML: TStrings; WantAll: Boolean = True);
var
  tempSalingDaten: TSalingDaten;
begin
  tempSalingDaten := SalingDaten;

//  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
//  ML := OutputForm.DisplayMemo.Lines;
//  ML.BeginUpdate;
//  ML.Clear;

  { Text setzen }
//  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
//  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
//  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  ML.Add('Trimm:');
  ML.Add(Format('  Mastfall F0F     = %8.1f cm', [Trimm.Mastfall / 10]));
  ML.Add(Format('  Vorstagspannung  = %8.1f N', [rF[14]]));
  ML.Add(Format('  Durchbiegung hd  = %8.1f cm', [hd / 10]));

  ML.Add('');
  ML.Add('Saling:');
  ML.Add(Format('  Saling Länge   = %6.2f mm', [tempSalingDaten.SalingL]));
  ML.Add(Format('  Saling Höhe    = %6.2f mm', [tempSalingDaten.SalingH]));
  ML.Add(Format('  Saling Abstand = %6.2f mm', [tempSalingDaten.SalingA]));
  ML.Add(Format('  Saling Winkel  = %6.2f Grad', [tempSalingDaten.SalingW]));
  ML.Add(Format('  Wanten Winkel  = %6.2f Grad', [tempSalingDaten.WantenWinkel]));
  ML.Add(Format('  Kraft Winkel   = %6.2f Grad', [tempSalingDaten.KraftWinkel]));

  ML.Add('');
  ML.Add('Winkel:');
  ML.Add(Format('  phi       = %6.2f Grad', [RadToDeg(Phi)]));
  ML.Add(Format('  psi       = %6.2f Grad', [RadToDeg(psi)]));
  ML.Add(Format('  alpha     = %6.2f Grad', [RadToDeg(alpha)]));
  ML.Add(Format('  phi-alpha = %6.2f Grad (Mast-Neigung)', [RadToDeg(Phi-alpha)]));
  ML.Add(Format('  psi-alpha = %6.2f Grad (Wanten-Neigung)', [RadToDeg(psi-alpha)]));

  ML.Add('');
  ML.Add('MastWinkel:');
  ML.Add(Format('  epsB = %6.2f Grad', [RadToDeg(epsB)]));
  ML.Add(Format('  eps2 = %6.2f Grad', [RadToDeg(eps2)]));
  ML.Add(Format('  eps1 = %6.2f Grad', [RadToDeg(eps1)]));
  ML.Add(Format('  epsA = %6.2f Grad', [RadToDeg(epsA)]));
  ML.Add(Format('  Epsilon  = %6.2f Grad', [RadToDeg(epsilon)]));

  ML.Add('');
  ML.Add('SchnittWinkel:');
  ML.Add(Format('  alpha1 = %6.2f Grad', [RadToDeg(alpha1)]));
  ML.Add(Format('  alpha2 = %6.2f Grad', [RadToDeg(alpha2)]));
  ML.Add(Format('  delta1 = %6.2f Grad', [RadToDeg(delta1)]));
  ML.Add(Format('  delta2 = %6.2f Grad', [RadToDeg(delta2)]));
  ML.Add(Format('  gamma  = %6.2f Grad', [RadToDeg(gamma)]));
  ML.Add(Format('  beta   = %6.2f Grad', [RadToDeg(beta)]));

  if not WantAll then
    Exit;

  ML.Add('');
  ML.Add('SchnittKräfte:');
  ML.Add(Format('  FC  = %8.2f N    (Mastdruckkraft)', [FC]));
  ML.Add(Format('  FB  = %8.2f N    (Wanten/Vorstag)', [FB]));
  ML.Add(Format('  F2  = %8.2f N    (Saling)', [F2]));
  ML.Add(Format('  F1  = %8.2f N    (Controller)', [F1]));
  ML.Add(Format('  FA  = %8.2f N    (Mastfuß)', [FA]));
  ML.Add(Format('  hd  = %8.2f mm   (Saling Durchbiegung)', [hd]));
  ML.Add(Format('  he  = %8.2f mm   (Controller Durchbiegung)', [he]));
  ML.Add(Format('  sd  = %8.2f mm   (hd-FSalingWegKnick)', [hd-FSalingWegKnick]));

  ML.Add('');
  ML.Add('BiegeKnicken:');
  ML.Add(Format('  KoppelFaktor       = %8.5f', [FKoppelFaktor]));
  ML.Add(Format('  SalingAlpha        = %8.5f mm/N', [FSalingAlpha]));
  ML.Add(Format('  ControllerAlpha    = %8.5f mm/N', [FControllerAlpha]));
  ML.Add(Format('  SalingWeg          = %8.2f mm', [FSalingWeg]));
  ML.Add(Format('  SalingWegKnick     = %8.2f mm', [FSalingWegKnick]));
  ML.Add(Format('  ControllerWeg      = %8.2f mm', [FControllerWeg]));
  ML.Add(Format('  FSchnittPunktKraft = %8.2f N', [FSchnittPunktKraft]));
  ML.Add(Format('  FwSchnittOhne      = %8.2f mm', [FwSchnittOhne]));
  ML.Add(Format('  FwSchnittMit       = %8.2f mm', [FwSchnittMit]));
  ML.Add(Format('  FwSchnittOffset    = %8.2f mm', [FwSchnittOffset]));

//  SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
//  ML.EndUpdate;
end;

procedure TRigg.AusgabeKommentar(ML: TStrings);
var
  temp: single;
begin
//  ML := OutputForm.KommentarMemo.Lines;
//  ML.BeginUpdate;
//  ML.Clear;

  temp := hd / 10; { Biegung in cm }
  if temp < 0 then
    ML.Add('Mastbiegung negativ!');
  if temp < 2 then
    ML.Add('Mast hat fast keine Vorbiegung.');
  if temp > 10 then
    ML.Add('Mastbiegung zu groß.');

  temp := rF[14]; { Vorstagspannung in N }
  if temp < 800 then
    ML.Add('Vorstagspannung zu gering.');
  if temp > 2000 then
    ML.Add('Vorstagspannung zu groß.');

//  ML.EndUpdate;
end;

end.
