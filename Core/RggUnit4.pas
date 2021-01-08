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
  RiggVar.RG.Inter,
  RiggVar.App.Strings,
  RiggVar.RG.Types,
  RggUnit3,
  RiggVar.RG.Doc,
  RiggVar.RG.Scroll,
  RiggVar.RG.Data;

type
  TRigg1 = class(TRiggFS, IRigg0)
  private
    procedure AusgabeTextDE(ML: TStrings; WantAll, WantForce: Boolean);
    procedure AusgabeTextEN(ML: TStrings; WantAll, WantForce: Boolean);
  public
    FMastKurve: TMastKurve;
    function GetRealTrimm(Index: TTrimmIndex): single;
    function FindBogenIndexOf(P: TPoint3D): Integer;
    function GetMastKurve: TMastKurve;
    function GetMastKurvePoint(const Index: Integer): TPoint3D;
    procedure SetMastLineData(const Value: TLineDataR100; L, Beta: single);

    procedure WriteXml(ML: TStrings; AllTags: Boolean = False);
    procedure AusgabeText(ML: TStrings; WantAll: Boolean = True; WantForce: Boolean = False);
    procedure AusgabeKommentar(ML: TStrings);

    procedure InitFactArray;
    procedure UpdateFactArray(CurrentParam: TFederParam);
    procedure ChangeRigg(CurrentParam: TFederParam; Value: single);
    function GetPlotValue(CurrentParam: TFederParam; PlotID: Integer; x, y: single): single;
    function GetPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRelaxedPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRiggDistance(Value: TRiggRod): single;
    function GetStabKraft(Value: TRiggRod): single;

    procedure SaveToFederData(fd: TRggData);
    procedure LoadFromFederData(fd: TRggData);
    procedure WriteToDocFile(FileName: string);
    procedure LoadFromDocFile(FileName: string);
    procedure Assign(Source: TRigg1);
    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);
    procedure SetDefaultDocument;
    procedure GetRealTrimmRecord(var RealTrimm: TRealTrimm);

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    property RealTrimm[Index: TTrimmIndex]: single read GetRealTrimm;
    property MastKurve: TMastKurve read GetMastKurve;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRigg1 }

procedure TRigg1.WriteToDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    GetDocument(Document);
    s := ExtractFileExt(FileName);
    if s = RggStrings.RGI_File_Extension then
    begin
      Document.WriteToIniFile(FileName);
    end
    else if s = RggStrings.RGG_File_Extension then
    begin
      { write as .rgi, .rgg no longer supported }
      s := ChangeFileExt(FileName, RggStrings.RGI_File_Extension);
      Document.WriteToIniFile(s);
      // Document.SaveToFile(FileName);
    end;
  finally
    Document.Free;
  end;
end;

{$ifdef MSWindows}
procedure TRigg1.WriteXml(ML: TStrings; AllTags: Boolean);
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
{$else}
procedure TRigg1.WriteXml(ML: TStrings; AllTags: Boolean);
begin
end;
{$endif}

procedure TRigg1.LoadFromDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    s := ExtractFileExt(FileName);
    if s = RggStrings.RGI_File_Extension then
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

procedure TRigg1.Assign(Source: TRigg1);
var
  Document: TRggDocument;
begin
  if Source is TRigg1 then
  begin
    Document := TRggDocument.Create;
    try
      (Source as TRigg1).GetDocument(Document);
      SetDocument(Document);
    finally
      Document.Free;
    end;
    Exit;
  end;
end;

procedure TRigg1.GetDocument(Doc: TRggDocument);
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

procedure TRigg1.SetDocument(Doc: TRggDocument);
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

procedure TRigg1.SetDefaultDocument;
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

procedure TRigg1.GetRealTrimmRecord(var RealTrimm: TRealTrimm);
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
    MastfallF0F := rP.F0.Distance(rP.F); { in mm }
    { Vorstagspannung }
    if abs(rF.C0C) < 32000 then
      SpannungV := rF.C0C { in N }
    else
    begin
      if rF.C0C > 32000 then
        SpannungV := 32000;
      if rF.C0C < -32000 then
        SpannungV := -32000;
    end;
    { Biegung an den Salingen }
    BiegungS := hd; { in mm }
    { Biegung am Controller }
    BiegungC := he; { in mm }
    { "Elastizität" }
    FlexWert := rP.C.Distance(rPe.C); { in mm }
  end;
end;

function TRigg1.GetRelaxedPoint3D(Value: TRiggPoint): TPoint3D;
begin
  result := rPe.V[Value];
end;

function TRigg1.GetRealTrimm(Index: TTrimmIndex): single;
var
  temp: single;
begin
  temp := 0;
  case Index of
    tiMastfallF0F: temp := rP.F0.Distance(rP.F);
    tiMastfallF0C: temp := rP.F0.Distance(rP.C);
    tiVorstagDiff: temp := VorstagDiff;
    tiVorstagDiffE: temp := rPe.C0.Distance(rPe.C) - rL.C0C;
    tiSpannungW: temp := SpannungW;
    tiSpannungV:
      begin
        if abs(rF.C0C) < 32000 then
          temp := rF.C0C { in N }
        else
        begin
          if rF.C0C > 32000 then
            temp := 32000;
          if rF.C0C < -32000 then
            temp := -32000;
      end;
    end;
    tiBiegungS: temp := hd;
    tiBiegungC: temp := he;
    tiFlexWert: temp := rP.C.Distance(rPe.C); { in mm }
  end;
  result := temp;
end;

procedure TRigg1.SaveToFederData(fd: TRggData);
begin
  fd.A0X := Round(rP.A0.X);
  fd.A0Y := -Round(rP.A0.Y);
  fd.A0Z := Round(rP.A0.Z);

  fd.C0X := Round(rP.C0.X);
  fd.C0Y := Round(rP.C0.Y);
  fd.C0Z := Round(rP.C0.Z);

  fd.D0X := Round(rP.D0.X);
  fd.D0Y := Round(rP.D0.Y);
  fd.D0Z := Round(rP.D0.Z);

  fd.E0X := Round(rP.E0.X);
  fd.E0Y := Round(rP.E0.Y);
  fd.E0Z := Round(rP.E0.Z);

  fd.F0X := Round(rP.F0.X);
  fd.F0Y := Round(rP.F0.Y);
  fd.F0Z := Round(rP.F0.Z);

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

procedure TRigg1.LoadFromFederData(fd: TRggData);
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
  rP.A0.X := fd.A0X;
  rP.A0.Y := -fd.A0Y;
  rP.A0.Z := fd.A0Z;
  rP.B0.X := fd.A0X;
  rP.B0.Y := fd.A0Y;
  rP.B0.Z := fd.A0Z;
  rP.C0.X := fd.C0X;
  rP.C0.Y := fd.C0Y;
  rP.C0.Z := fd.C0Z;
  rP.D0.X := fd.D0X;
  rP.D0.Y := fd.D0Y;
  rP.D0.Z := fd.D0Z;
  rP.E0.X := fd.E0X;
  rP.E0.Y := fd.E0Y;
  rP.E0.Z := fd.E0Z;
  rP.F0.X := fd.F0X;
  rP.F0.Y := fd.F0Y;
  rP.F0.Z := fd.F0Z;
  rP.P0.X := fd.A0X;
  rP.P0.Y := 0;
  rP.P0.Z := fd.A0Z;

  rP.P.X := fd.A0X;
  rP.P.Y := 0;
  rP.P.Z := fd.A0Z;

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

procedure TRigg1.AusgabeText(ML: TStrings; WantAll: Boolean = True; WantForce: Boolean = False);
begin
//  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
//  ML := OutputForm.DisplayMemo.Lines;
//  ML.BeginUpdate;
//  ML.Clear;

  if MainVar.WantLocalizedText and MainVar.WantGermanText then
    AusgabeTextDE(ML, WantAll, WantForce)
  else
    AusgabeTextEN(ML, WantAll, WantForce);

//  SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
//  ML.EndUpdate;
end;

procedure TRigg1.AusgabeTextDE(ML: TStrings; WantAll: Boolean; WantForce: Boolean);
var
  t: TSalingDaten;
begin
  t := SalingDaten;

  { Text setzen }
//  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
//  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
//  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  ML.Add('Trimm:');
  ML.Add(Format('  Mastfall F0F     = %8.0f mm', [rP.F0.Distance(rP.F)]));
  if WantForce then
  ML.Add(Format('  Vorstagspannung  = %8.0f N', [rF.C0C]));
  ML.Add(Format('  Durchbiegung hd  = %8.0f mm', [hd]));

  ML.Add('');
  ML.Add('Saling:');
  ML.Add(Format('  Saling Länge   = %6.2f mm', [t.SalingL]));
  ML.Add(Format('  Saling Höhe    = %6.2f mm', [t.SalingH]));
  ML.Add(Format('  Saling Abstand = %6.2f mm', [t.SalingA]));
  ML.Add(Format('  Saling Winkel  = %6.2f Grad', [t.SalingW]));
  ML.Add(Format('  Wanten Winkel  = %6.2f Grad', [t.WantenWinkel]));
  ML.Add(Format('  Kraft Winkel   = %6.2f Grad', [t.KraftWinkel]));

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
end;

procedure TRigg1.AusgabeTextEN(ML: TStrings; WantAll: Boolean; WantForce: Boolean);
var
  t: TSalingDaten;
  s: string;
  fs: string;
begin
  t := SalingDaten;

  { Text setzen }
//  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
//  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
//  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  s := RggLocalizedStrings.AusgabeTokenGrad;

  ML.Add('Trim:');
  ML.Add(Format('  Distance F0F     = %8.0f mm', [rP.F0.Distance(rP.F)]));
  if WantForce then
  ML.Add(Format('  Headstay tension = %8.0f N', [rF.C0C]));
  ML.Add(Format('  Displacement hd  = %8.0f mm', [hd]));

  ML.Add('');
  ML.Add('Spreaders:');
  ML.Add(Format('  Spreader Length   = %6.2f mm', [t.SalingL]));
  ML.Add(Format('  Spreader Height   = %6.2f mm', [t.SalingH]));
  ML.Add(Format('  Spreader Distance = %6.2f mm', [t.SalingA]));
  ML.Add(Format('  Spreader Angle    = %6.2f %s', [t.SalingW, s]));
  ML.Add(Format('  Shroud Angle      = %6.2f %s', [t.WantenWinkel, s]));
  ML.Add(Format('  Force Angle       = %6.2f %s', [t.KraftWinkel, s]));

  ML.Add('');
  ML.Add('System angles:');
  ML.Add(Format('  phi       = %6.2f %s', [RadToDeg(Phi), s]));
  ML.Add(Format('  psi       = %6.2f %s', [RadToDeg(psi), s]));
  ML.Add(Format('  alpha     = %6.2f %s', [RadToDeg(alpha), s]));
  ML.Add(Format('  phi-alpha = %6.2f %s (Mast-Tilt)', [RadToDeg(Phi-alpha), s]));
  ML.Add(Format('  psi-alpha = %6.2f %s (Shroud-Tilt)', [RadToDeg(psi-alpha), s]));

  ML.Add('');
  ML.Add('Mast angles:');
  ML.Add(Format('  epsB = %6.2f %s', [RadToDeg(epsB), s]));
  ML.Add(Format('  eps2 = %6.2f %s', [RadToDeg(eps2), s]));
  ML.Add(Format('  eps1 = %6.2f %s', [RadToDeg(eps1), s]));
  ML.Add(Format('  epsA = %6.2f %s', [RadToDeg(epsA), s]));
  ML.Add(Format('  Epsilon  = %6.2f %s', [RadToDeg(epsilon), s]));

  fs := '  %6s = %6.2f %s';
  ML.Add('');
  ML.Add('Intersection angles:');
  ML.Add(Format(fs, ['alpha1', RadToDeg(alpha1), s]));
  ML.Add(Format(fs, ['alpha2', RadToDeg(alpha2), s]));
  ML.Add(Format(fs, ['delta1', RadToDeg(delta1), s]));
  ML.Add(Format(fs, ['delta2', RadToDeg(delta2), s]));
  ML.Add(Format(fs, ['gamma', RadToDeg(gamma), s]));
  ML.Add(Format(fs, ['beta', RadToDeg(beta), s]));

  if not WantAll then
    Exit;

  ML.Add('');
  ML.Add('Force values:');
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
end;

procedure TRigg1.AusgabeKommentar(ML: TStrings);
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

  temp := rF.C0C; { Vorstagspannung in N }
  if temp < 800 then
    ML.Add('Vorstagspannung zu gering.');
  if temp > 2000 then
    ML.Add('Vorstagspannung zu groß.');

//  ML.EndUpdate;
end;

procedure TRigg1.InitFactArray;
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := RggFA.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := RggFA.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  GSB.SalingW.Ist := Round(RadToDeg(arctan2(tempH * 2, tempA)));

  GSB.MastfallF0C.Ist := RealTrimm[tiMastfallF0C];
  GSB.MastfallF0F.Ist := RealTrimm[tiMastfallF0F];
  GSB.Biegung.Ist := RealTrimm[tiBiegungS];
  GSB.D0X.Ist := rP.D0.X;

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  { speziell überschreiben }
  if WantLogoData then
  begin
    GSB.Controller.Min := 50;
    GSB.Winkel.Min := 70;
    GSB.Winkel.Max := 120;
    // GSB.Woben.Min := 2000;
    // GSB.Woben.Max := 2100;
    GSB.SalingW.Min := 40;
    GSB.SalingW.Max := 60;
    // GSB.MastfallF0F.Max := 6400;
    GSB.Biegung.Min := 0;
    GSB.Biegung.Max := 120;
  end
  else
  begin
    GSB.Controller.Min := 50;

    GSB.Wante.Min := 4020;
    GSB.Wante.Max := 4220;

    GSB.Vorstag.Min := 4200;
    GSB.Vorstag.Max := 5000;

    GSB.Winkel.Min := 80;
    GSB.Winkel.Max := 115;

    GSB.Woben.Min := 2000;
    GSB.Woben.Max := 2100;

    GSB.SalingH.Min := 170;
    GSB.SalingH.Max := 1020;

    GSB.SalingA.Min := 250;
    GSB.SalingA.Max := 1550;

    GSB.SalingL.Ist := 480;
    GSB.SalingL.Min := 240;
    GSB.SalingL.Max := 1200;

    GSB.SalingW.Min := 15;
    GSB.SalingW.Max := 87;

    GSB.D0X.Min := 2600;
    GSB.D0X.Ist := 2870;
    GSB.D0X.Max := 3300;

    GSB.MastfallF0C.Min := 4000;
    GSB.MastfallF0C.Ist := 4800;
    GSB.MastfallF0C.Max := 5100;

    GSB.MastfallF0F.Min := 5370;
    GSB.MastfallF0F.Ist := 6070;
    GSB.MastfallF0F.Max := 6570;

    GSB.MastfallVorlauf.Min := 4950;
    GSB.MastfallVorlauf.Ist := 5000;
    GSB.MastfallVorlauf.Max := 5150;

    GSB.Biegung.Min := 0;
    GSB.Biegung.Max := 500;

    GSB.ResetVolatile;
  end;
end;

procedure TRigg1.UpdateFactArray(CurrentParam: TFederParam);
var
  i: TFederParam;
  sb: TRggSB;
begin
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    case i of
      fpController:
        sb.Ist := RealGlied[fpController];
      fpWinkel:
        sb.Ist := RadToDeg(RealGlied[fpWinkel]);
      fpVorstag:
        sb.Ist := RealGlied[fpVorstag];
      fpWante:
        sb.Ist := RealGlied[fpWante];
      fpWoben:
        sb.Ist := RealGlied[fpWoben];
      fpSalingH:
        sb.Ist := RealGlied[fpSalingH];
      fpSalingA:
        sb.Ist := RealGlied[fpSalingA];
      fpSalingL:
        sb.Ist := RealGlied[fpSalingL];
      fpSalingW:
        sb.Ist := RadToDeg(arctan2(RealGlied[fpSalingH] * 2, RealGlied[fpSalingA]));
      fpMastfallF0C:
        sb.Ist := rP.F0.Distance(rP.C);
      fpMastfallF0F:
        sb.Ist := rP.F0.Distance(rP.F);
      fpBiegung:
        sb.Ist := DurchbiegungHD;
      fpD0X:
        sb.Ist := rP.D0.X;
    end;
  end;

  if CurrentParam <> fpWinkel then
  begin
    sb := GSB.Find(fpWinkel);
    sb.Ist := RadToDeg(RealGlied[fpWinkel]);
  end;
end;

procedure TRigg1.ChangeRigg(CurrentParam: TFederParam; Value: single);
var
  tempH, tempA, tempL, tempW: single;
begin
  case CurrentParam of
    fpController: RealGlied[fpController] := Value;

    fpWinkel: RealGlied[fpWinkel] := DegToRad(Value);

    fpVorstag: RealGlied[fpVorstag] := Value;
    fpWante: RealGlied[fpWante] := Value;
    fpWoben: RealGlied[fpWoben] := Value;

    fpSalingH:
    begin
      tempH := GSB.SalingH.Ist;
      tempA := GSB.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := RadToDeg(arctan2(tempH * 2, tempA));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      // SalingH no change (just changed)
      // SalingA no change (kept unchanged)
      GSB.SalingL.Ist := tempL;
      GSB.SalingW.Ist := tempW;
    end;

    fpSalingA:
    begin
      tempH := GSB.SalingH.Ist;
      tempA := GSB.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := RadToDeg(arctan2(tempH * 2, tempA));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      // SalingH no change (kept unchanged)
      // SalingA no change (just changed)
      GSB.SalingL.Ist := tempL;
      GSB.SalingW.Ist := tempW;
    end;

    fpSalingL:
    begin
      tempW := GSB.SalingW.Ist;
      tempL := GSB.SalingL.Ist;
      tempH := tempL * sin(DegToRad(tempW));
      tempA := 2 * tempL * cos(DegToRad(tempW));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      GSB.SalingH.Ist := tempH;
      GSB.SalingA.Ist := tempA;
      // SalingL no change (just changed)
      // SalingW no change (kept unchanged)
    end;

    fpSalingW:
    begin
      tempW := GSB.SalingW.Ist;
      tempL := GSB.SalingL.Ist;
      tempH := tempL * sin(DegToRad(tempW));
      tempA := 2 * tempL * cos(DegToRad(tempW));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      GSB.SalingH.Ist := tempH;
      GSB.SalingA.Ist := tempA;
      // SalingL no change
      // SalingW no change
    end;

    fpMastfallF0F:
      NeigeF(Value - GSB.MastfallVorlauf.Ist);

    fpMastfallF0C:
      BiegeUndNeigeC(Value, GSB.Biegung.Ist);

    fpMastfallVorlauf:
      MastfallVorlauf := Value;

    fpBiegung:
      BiegeUndNeigeC(GSB.MastfallF0C.Ist, Value);

    fpD0X:
      rP.D0.X := Round(Value);
  end;
end;

function TRigg1.GetPlotValue(CurrentParam: TFederParam; PlotID: Integer; x, y: single): single;
var
  tx, ty: single;
begin
  case PlotID of
    1..12:
    begin
      tx := GSB.Vorstag.Ist;
      ty := GSB.SalingL.Ist;
      RealGlied[fpVorstag] := tx + x;
      RealGlied[fpSalingA] := ty + y / 10;
      UpdateGetriebe;
      if GetriebeOK then
      begin
        result := rP.F0.Distance(rP.F);
        UpdateFactArray(CurrentParam);
      end
      else
        result := 0;
    end;
    else
      result := 0;
  end;
end;

function TRigg1.GetPoint3D(Value: TRiggPoint): TPoint3D;
begin
  result := rP.V[Value];
end;

function TRigg1.GetRiggDistance(Value: TRiggRod): single;
begin
  result := rL.Rod[Value];
end;

function TRigg1.GetStabKraft(Value: TRiggRod): single;
begin
  result := rF.Rod[Value];
end;

procedure TRigg1.LoadTrimm(fd: TRggData);
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
  SetDefaultDocument;
  LoadFromFederData(fd);

//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
  tempH := RggFA.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
  tempA := RggFA.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  GSB.SalingW.Ist := Round(RadToDeg(arctan2(tempH * 2, tempA)));

  GSB.MastfallF0C.Ist := RealTrimm[tiMastfallF0C];
  GSB.MastfallF0F.Ist := RealTrimm[tiMastfallF0F];
  GSB.Biegung.Ist := RealTrimm[tiBiegungS];
  GSB.D0X.Ist := rP.D0.X;

  fd.F0C := Round(GSB.MastfallF0C.Ist);
  fd.F0F := Round(GSB.MastfallF0F.Ist);
  fd.Bie := Round(GSB.Biegung.Ist);

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  GSB.Controller.Min := fd.CPMin;
  GSB.Controller.Max := fd.CPMax;

  GSB.Wante.Min := fd.WLMin;
  GSB.Wante.Max := fd.WLMax;

  GSB.Vorstag.Min := fd.VOMin;
  GSB.Vorstag.Max := fd.VOMax;

  GSB.Winkel.Min := fd.WIMin;
  GSB.Winkel.Max := fd.WIMax;

  GSB.Woben.Min := fd.WOMin;
  GSB.Woben.Max := fd.WOMax;

  GSB.SalingH.Min := fd.SHMin;
  GSB.SalingH.Max := fd.SHMax;

  GSB.SalingA.Min := fd.SAMin;
  GSB.SalingA.Max := fd.SAMax;

  GSB.SalingL.Min := fd.SLMin;
  GSB.SalingL.Max := fd.SLMax;

  GSB.SalingW.Min := fd.SWMin;
  GSB.SalingW.Max := fd.SWMax;

  GSB.D0X.Min := fd.D0X - 100;
  GSB.D0X.Max := fd.D0X + 100;

  GSB.MastfallVorlauf.Ist := fd.MV;
  GSB.MastfallVorlauf.Min := fd.MV - 100;
  GSB.MastfallVorlauf.Max := fd.MV + 100;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
//  tempH := GSB.MastfallF0C.Ist;
//  temp := tempA - tempH; // = 0
  temp := GSB.MastfallF0C.Ist;
  GSB.MastfallF0C.Min := temp - 700;
  GSB.MastfallF0C.Max := temp + 500;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
//  tempH := GSB.MastfallF0F.Ist;
//  temp := tempA - tempH; // = 0
  temp := GSB.MastfallF0F.Ist;
  GSB.MastfallF0F.Min := temp - 700;
  GSB.MastfallF0F.Max := temp + 500;

  GSB.Biegung.Min := 0;
  GSB.Biegung.Max := 500;

  GSB.ResetVolatile;
end;

procedure TRigg1.SaveTrimm(fd: TRggData);
begin
  SaveToFederData(fd);

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

  fd.SWMin := Round(GSB.SalingW.Min);
  fd.SWPos := Round(GSB.SalingW.Ist);
  fd.SWMax := Round(GSB.SalingW.Max);

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
end;

procedure TRigg1.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: single;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - Beta);
  temp2 := cos(Beta);
  temp3 := sin(pi / 2 - Beta);
  temp4 := sin(Beta);
  for j := 0 to BogenMax do
  begin
    k := Round(j * 100 / BogenMax);
    tempL := j * L / BogenMax;
    FMastKurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    FMastKurve[j].Y := 0;
    FMastKurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

function TRigg1.GetMastKurve: TMastKurve;
begin
  SetMastLineData(MastLinie, MastLC, MastBeta);
  result := FMastKurve;
end;

function TRigg1.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(FMastKurve)) then
    result := FMastKurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TRigg1.FindBogenIndexOf(P: TPoint3D): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: single;
  a: single;
begin
  j := Length(FMastKurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := (P - FMastKurve[i]).Length;
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

end.
