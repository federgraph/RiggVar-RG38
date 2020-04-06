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
  RggTypes,
  RggUnit3,
  RggDoc,
  RggCalc,
  RiggVar.RG.Data;

type
  TRigg = class(TRiggFS)
  private
    function GetRealTrimm(Index: TTrimmIndex): double;
  public
{$ifdef MSWindows}
    procedure WriteXml(ML: TStrings; AllTags: Boolean = False);
{$endif}
    procedure SaveToFederData(fd: TRggData);
    procedure LoadFromFederData(fd: TRggData);
    procedure WriteToDocFile(FileName: String);
    procedure LoadFromDocFile(FileName: String);
    procedure Assign(Source: TPersistent); override;
    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);
    procedure SetDefaultDocument;
    procedure GetRealTrimmRecord(var RealTrimm: TRealTrimm);
    property RealTrimm[Index: TTrimmIndex]: double read GetRealTrimm;
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

procedure TRigg.WriteToDocFile(FileName: String);
var
  Document: TRggDocument;
  S: String;
begin
  Document := TRggDocument.Create;
  try
    GetDocument(Document);
    S := ExtractFileExt(FileName);
    if S = '.rgi' then
    begin
      Document.WriteToIniFile(FileName);
    end
    else if S = '.rgg' then
    begin
      S := ChangeFileExt(FileName, '.rgi');
      Document.WriteToIniFile(FileName);
      // Document.SaveToFile(FileName);
    end;
  finally
    Document.Free;
  end;
end;

procedure TRigg.LoadFromDocFile(FileName: String);
var
  Document: TRggDocument;
  S: String;
begin
  Document := TRggDocument.Create;
  try
    S := ExtractFileExt(FileName);
    if S = '.rgi' then
    begin
      Document.LoadFromIniFile(FileName);
      SetDocument(Document);
    end;
    // if S = '.rgg' then
    // begin
    // Document.LoadFromFile(FileName);
    // SetDocument(Document);
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
  Doc.FiMastL := Round(FiMastL);
  Doc.FiMastunten := Round(FiMastunten);
  Doc.FiMastoben := Round(FiMastoben);
  Doc.FiMastfallVorlauf := Round(FiMastfallVorlauf);
  Doc.FiControllerAnschlag := Round(FiControllerAnschlag);
  { Rumpf: Koordinaten }
  Doc.iP := iP;
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
  FiMastL := Doc.FiMastL;
  FiMastunten := Doc.FiMastunten;
  FiMastoben := Doc.FiMastoben;
  FiMastfallVorlauf := Doc.FiMastfallVorlauf;
  FiControllerAnschlag := Doc.FiControllerAnschlag;
  { Rumpf: Koordinaten }
  iP := Doc.iP;
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
    MastfallF0F := Abstand(rP[ooF0], rP[ooF]); { in mm }
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
    FlexWert := Abstand(rP[ooC], rPe[ooC]); { in mm }
  end;
end;

function TRigg.GetRealTrimm(Index: TTrimmIndex): double;
var
  temp: double;
begin
  temp := 0;
  case Index of
    tiMastfallF0F: temp := Abstand(rP[ooF0], rP[ooF]);
    tiMastfallF0C: temp := Abstand(rP[ooF0], rP[ooC]);
    tiVorstagDiff: temp := VorstagDiff;
    tiVorstagDiffE: temp := Abstand(rPe[ooC0], rPe[ooC]) - rL[14];
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
    tiFlexWert: temp := Abstand(rP[ooC], rPe[ooC]); { in mm }
  end;
  result := temp;
end;

procedure TRigg.SaveToFederData(fd: TRggData);
begin
  fd.A0X := Round(iP[ooA0, X]);
  fd.A0Y := Round(iP[ooA0, Y]);
  fd.A0Z := Round(iP[ooA0, Z]);

  fd.C0X := Round(iP[ooC0, X]);
  fd.C0Y := Round(iP[ooC0, Y]);
  fd.C0Z := Round(iP[ooC0, Z]);

  fd.D0X := Round(iP[ooD0, X]);
  fd.D0Y := Round(iP[ooD0, Y]);
  fd.D0Z := Round(iP[ooD0, Z]);

  fd.E0X := Round(iP[ooE0, X]);
  fd.E0Y := Round(iP[ooE0, Y]);
  fd.E0Z := Round(iP[ooE0, Z]);

  fd.F0X := Round(iP[ooF0, X]);
  fd.F0Y := Round(iP[ooF0, Y]);
  fd.F0Z := Round(iP[ooF0, Z]);

  fd.MU := Round(FiMastUnten);
  fd.MO := Round(FiMastOben);
  fd.ML := Round(FiMastL);
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
  FiMastL := fd.ML;
  FiMastunten := fd.MU;
  FiMastoben := fd.MO;
  FiControllerAnschlag := fd.CA;
  MastfallVorlauf := fd.MV;

  { Rumpf: Koordinaten }

  //iP := Doc.iP;
  iP[ooA0, x] := fd.A0X;
  iP[ooA0, y] := fd.A0Y;
  iP[ooA0, z] := fd.A0Z;
  iP[ooB0, x] := fd.A0X;
  iP[ooB0, y] := -fd.A0Y;
  iP[ooB0, z] := fd.A0Z;
  iP[ooC0, x] := fd.C0X;
  iP[ooC0, y] := fd.C0Y;
  iP[ooC0, z] := fd.C0Z;
  iP[ooD0, x] := fd.D0X;
  iP[ooD0, y] := fd.D0Y;
  iP[ooD0, z] := fd.D0Z;
  iP[ooE0, x] := fd.E0X;
  iP[ooE0, y] := fd.E0Y;
  iP[ooE0, z] := fd.E0Z;
  iP[ooF0, x] := fd.F0X;
  iP[ooF0, y] := fd.F0Y;
  iP[ooF0, z] := fd.F0Z;
  iP[ooP0, x] := fd.A0X;
  iP[ooP0, y] := 0;
  iP[ooP0, z] := fd.A0Z;

  iP[ooP, x] := fd.A0X;
  iP[ooP, y] := 0;
  iP[ooP, z] := fd.A0Z;

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

end.
