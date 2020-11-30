unit RggDoc;

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

{
  Als primäre Daten werden die Rumpfkoordinaten und die RiggLängen betrachtet.
  Der Vollständigkeit halber werden aber auch die Riggkoordinaten gespeichert.
  Dies stellt zwar eine Redundanz dar, hat aber den Vorteil, daß eine vollständige
  Koordinatenschnittstelle nach außen zur Verfügung steht. Die RumpfLängen werden
  jedoch nicht im Dokument gespeichert.

  Alle Koordinaten und Längen sind Integerwerte! Sie sind in mm angegeben. Es ist
  nicht praxisbezogen, für die Ein- und Ausgabe dieser Werte Kommastellen zuzulassen.

  Die Rigglängen sind in den Istwerten des GSB Arrays gespeichert.
  (GSB = GetriebeScrollBar)
  Die Kraft FiWPowerOS ist ebenfalls im GSB Array untegebracht.
  Die Arrayfelder TinyStep und BigStep können eventuell eingespart werden.

  Das Salingdreieck wird grundsätzlich mit den beiden Werten FiSalingH und
  FiSalingA beschrieben. Der zugehörige Wert FiSalingL wird zwar gespeichert,
  sollte aber immer aus FiSalingH und FiSalingA berechnet werden. Da FiSalingH
  und FiSalingA Integerwerte sind, kann für FiSalingL nämlich kein exakt passender
  Integerwert gespeichert werden.

  Der allgemeine und zu bevorzugende SalingTyp ist stFest. Ein Rigg vom SalingTyp
  stDrehbar, stOhneStarr bzw. stOhneBiegt kann auch mit SalingTyp stFest gespeichert
  werden.

  Die Festigkeitswerte EA und EI sowie die meisten Felder der Trimmtabelle sind
  Gleitkommazahlen. Wenn als Calctyp Biegeknicken angegeben ist, dann werden in
  der 'Trimmtabelle' die Biegeknickparameter gespeichert.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Inifiles,
  System.Math,
{$ifdef MSWindows}
  Xml.XmlDoc,
  Xml.XmlIntf,
{$endif}
  RggScroll,
  RggTypes;

const
  RggDocSignature: string = 'RGGDOC01';

type
  TRggDocument = class
  private
    procedure GetLogoDoc;
    procedure GetDefaultDoc;
  public
    Signature: string;
    { Rigg: Typ }
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    CalcTyp: TCalcTyp;
    { Mast: Abmessungen in mm }
    FiMastL: Integer;
    FiMastunten: Integer;
    FiMastoben: Integer;
    FiMastfallVorlauf: Integer;
    FiControllerAnschlag: Integer;
    FiReserved: Integer;
    { Rumpf: Koordinaten in mm }
    iP: TRiggPoints; { Array enthält auch die Riggkoordinaten }
    { Festigkeitswerte }
    rEA: TRiggRods; { N }
    EI: single; { Nmm^2 }
    { Grenzwerte und Istwerte }
    GSB: TRggFA;
    { Trimmtabelle }
    TrimmTabDaten: TTrimmTabDaten;

    WantFestigkeitsWerteInXml: Boolean;
    WantTrimmTabInXml: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure GetDefaultDocument; virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    procedure LoadFromIniFile(FileName: string); virtual;
    procedure WriteToIniFile(FileName: string); virtual;
    procedure DumpToMemo(Memo: TStrings); virtual;
{$ifdef MSWindows}
    procedure WriteXML(ML: TStrings);
    function SaveToXML(d: IXMLNode): string;
{$endif}
    procedure LoadFromXML(s: string);
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.Classes;

constructor TRggDocument.Create;
begin
  inherited Create;
  GSB := TRggFA.Create;
end;

destructor TRggDocument.Destroy;
begin
  GSB.Free;
  inherited;
end;

procedure TRggDocument.LoadFromFile(FileName: string);
begin
  LoadFromIniFile(FileName);
end;

procedure TRggDocument.SaveToFile(FileName: string);
begin
  { The intention is to never call this method in FR38!
    But please double check again, convince yourself,
    with Ctrl-Shift-Enter on 'SaveToFile'. }
  if not MainConst.MustBeSandboxed then
  begin
    { This is how it used to be. }
    WriteToIniFile(FileName);
    { How should that be done in the future, if at all? }
  end;
end;

procedure TRggDocument.LoadFromIniFile(FileName: string);
var
  S, S1, S2: string;
  i: Integer;
  T: TTrimmTabDaten;
  IniFile: TIniFile;
begin
  GetDefaultDocument;
  IniFile := TIniFile.Create(FileName);
  try
    with IniFile do
    begin
      S := 'Rigg';
      SalingTyp := TSalingTyp(ReadInteger(S, 'SalingTyp', Ord(stFest)));
      ControllerTyp := TControllerTyp(ReadInteger(S, 'ControllerTyp', Ord(ctDruck)));
      CalcTyp := TCalcTyp(ReadInteger(S, 'CalcTyp', Ord(ctBiegeKnicken)));

      S := 'Trimmtabelle';
      T := DefaultTrimmTabDaten;
      with TrimmTabDaten do
      begin
        try
          T.TabellenTyp := TTabellenTyp(ReadInteger(S, 'TabellenTyp', Ord(itParabel)));
          S1 := ReadString(S, 'a0', FloatToStrF(T.a0, ffGeneral, 8, 2));
          T.a0 := StrToFloat(S1);
          S1 := ReadString(S, 'a1', FloatToStrF(T.a1, ffGeneral, 8, 2));
          T.a1 := StrToFloat(S1);
          S1 := ReadString(S, 'a2', FloatToStrF(T.a2, ffGeneral, 8, 2));
          T.a2 := StrToFloat(S1);
          S1 := ReadString(S, 'x0', FloatToStrF(T.x0, ffGeneral, 8, 2));
          T.x0 := StrToFloat(S1);
          S1 := ReadString(S, 'x1', FloatToStrF(T.x1, ffGeneral, 8, 2));
          T.x1 := StrToFloat(S1);
          S1 := ReadString(S, 'x2', FloatToStrF(T.x2, ffGeneral, 8, 2));
          T.x2 := StrToFloat(S1);
        except
          on EConvertError do
            T := DefaultTrimmTabDaten;
        end;
      end;
      TrimmTabDaten := T;

      S := 'Mast';
      FiMastL := ReadInteger(S, 'MastL', FiMastL);
      FiMastunten := ReadInteger(S, 'Mastunten', FiMastunten);
      FiMastoben := ReadInteger(S, 'Mastoben', FiMastoben);
      FiMastfallVorlauf := ReadInteger(S, 'MastfallVorlauf', FiMastfallVorlauf);
      FiControllerAnschlag := ReadInteger(S, 'ControllerAnschlag', FiControllerAnschlag);
      EI := ReadInteger(S, 'EI', 14700) * 1E6;

      S := 'Ist';
      GSB.Controller.Ist := ReadInteger(S, 'Controller', Round(GSB.Controller.Ist));
      GSB.Winkel.Ist := ReadInteger(S, 'Winkel', Round(GSB.Winkel.Ist));
      GSB.Vorstag.Ist := ReadInteger(S, 'Vorstag', Round(GSB.Vorstag.Ist));
      GSB.Wante.Ist := ReadInteger(S, 'Wante', Round(GSB.Wante.Ist));
      GSB.Woben.Ist := ReadInteger(S, 'Woben', Round(GSB.Woben.Ist));
      GSB.SalingH.Ist := ReadInteger(S, 'SalingH', Round(GSB.SalingH.Ist));
      GSB.SalingA.Ist := ReadInteger(S, 'SalingA', Round(GSB.SalingA.Ist));
      GSB.SalingL.Ist := ReadInteger(S, 'SalingL', Round(GSB.SalingL.Ist));
      GSB.VorstagOS.Ist := ReadInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Ist));
      GSB.WPowerOS.Ist := ReadInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Ist));

      S := 'Min';
      GSB.Controller. Min := ReadInteger(S, 'Controller', Round(GSB.Controller. Min));
      GSB.Winkel.Min := ReadInteger(S, 'Winkel', Round(GSB.Winkel. Min));
      GSB.Vorstag.Min := ReadInteger(S, 'Vorstag', Round(GSB.Vorstag.Min));
      GSB.Wante.Min := ReadInteger(S, 'Wante', Round(GSB.Wante.Min));
      GSB.Woben.Min := ReadInteger(S, 'Woben', Round(GSB.Woben.Min));
      GSB.SalingH.Min := ReadInteger(S, 'SalingH', Round(GSB.SalingH.Min));
      GSB.SalingA.Min := ReadInteger(S, 'SalingA', Round(GSB.SalingA.Min));
      GSB.SalingL.Min := ReadInteger(S, 'SalingL', Round(GSB.SalingL.Min));
      GSB.VorstagOS.Min := ReadInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Min));
      GSB.WPowerOS.Min := ReadInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Min));

      S := 'Max';
      GSB.Controller.Max := ReadInteger(S, 'Controller', Round(GSB.Controller.Max));
      GSB.Winkel.Max := ReadInteger(S, 'Winkel', Round(GSB.Winkel.Max));
      GSB.Vorstag.Max := ReadInteger(S, 'Vorstag', Round(GSB.Vorstag.Max));
      GSB.Wante.Max := ReadInteger(S, 'Wante', Round(GSB.Wante.Max));
      GSB.Woben.Max := ReadInteger(S, 'Woben', Round(GSB.Woben.Max));
      GSB.SalingH.Max := ReadInteger(S, 'SalingH', Round(GSB.SalingH.Max));
      GSB.SalingA.Max := ReadInteger(S, 'SalingA', Round(GSB.SalingA.Max));
      GSB.SalingL.Max := ReadInteger(S, 'SalingL', Round(GSB.SalingL.Max));
      GSB.VorstagOS.Max := ReadInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Max));
      GSB.WPowerOS.Max := ReadInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Max));

      S := 'Koordinaten Rumpf';
      iP.A0.X := ReadInteger(S, 'A0x', Round(iP.A0.X));
      iP.A0.Y := ReadInteger(S, 'A0y', Round(iP.A0.Y));
      iP.A0.Z := ReadInteger(S, 'A0z', Round(iP.A0.Z));
      iP.B0.X := ReadInteger(S, 'B0x', Round(iP.B0.X));
      iP.B0.Y := ReadInteger(S, 'B0y', Round(iP.B0.Y));
      iP.B0.Z := ReadInteger(S, 'B0z', Round(iP.B0.Z));
      iP.C0.X := ReadInteger(S, 'C0x', Round(iP.C0.X));
      iP.C0.Y := ReadInteger(S, 'C0y', Round(iP.C0.Y));
      iP.C0.Z := ReadInteger(S, 'C0z', Round(iP.C0.Z));
      iP.D0.X := ReadInteger(S, 'D0x', Round(iP.D0.X));
      iP.D0.Y := ReadInteger(S, 'D0y', Round(iP.D0.Y));
      iP.D0.Z := ReadInteger(S, 'D0z', Round(iP.D0.Z));
      iP.E0.X := ReadInteger(S, 'E0x', Round(iP.E0.X));
      iP.E0.Y := ReadInteger(S, 'E0y', Round(iP.E0.Y));
      iP.E0.Z := ReadInteger(S, 'E0z', Round(iP.E0.Z));
      iP.F0.X := ReadInteger(S, 'F0x', Round(iP.F0.X));
      iP.F0.Y := ReadInteger(S, 'F0y', Round(iP.F0.Y));
      iP.F0.Z := ReadInteger(S, 'F0z', Round(iP.F0.Z));

      S := 'Koordinaten Rigg';
      iP.A.X := ReadInteger(S, 'Ax', Round(iP.A.X));
      iP.A.Y := ReadInteger(S, 'Ay', Round(iP.A.Y));
      iP.A.Z := ReadInteger(S, 'Az', Round(iP.A.Z));
      iP.B.X := ReadInteger(S, 'Bx', Round(iP.B.X));
      iP.B.Y := ReadInteger(S, 'By', Round(iP.B.Y));
      iP.B.Z := ReadInteger(S, 'Bz', Round(iP.B.Z));
      iP.C.X := ReadInteger(S, 'Cx', Round(iP.C.X));
      iP.C.Y := ReadInteger(S, 'Cy', Round(iP.C.Y));
      iP.C.Z := ReadInteger(S, 'Cz', Round(iP.C.Z));
      iP.D.X := ReadInteger(S, 'Dx', Round(iP.D.X));
      iP.D.Y := ReadInteger(S, 'Dy', Round(iP.D.Y));
      iP.D.Z := ReadInteger(S, 'Dz', Round(iP.D.Z));
      iP.E.X := ReadInteger(S, 'Ex', Round(iP.E.X));
      iP.E.Y := ReadInteger(S, 'Ey', Round(iP.E.Y));
      iP.E.Z := ReadInteger(S, 'Ez', Round(iP.E.Z));
      iP.F.X := ReadInteger(S, 'Fx', Round(iP.F.X));
      iP.F.Y := ReadInteger(S, 'Fy', Round(iP.F.Y));
      iP.F.Z := ReadInteger(S, 'Fz', Round(iP.F.Z));

      S := 'EA';
      for i := 0 to 19 do
      begin
        S1 := IntToStr(i);
        S2 := ReadString(S, S1, '100000');
        rEA.V[i] := StrToFloat(S2);
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TRggDocument.WriteToIniFile(FileName: string);
var
  S, S1, S2: string;
  i, tempEI: Integer;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    with IniFile do
    begin
      S := 'Rigg';
      WriteInteger(S, 'SalingTyp', Ord(SalingTyp));
      WriteInteger(S, 'ControllerTyp', Ord(ControllerTyp));
      WriteInteger(S, 'CalcTyp', Ord(CalcTyp));

      S := 'Trimmtabelle';
      with TrimmTabDaten do
      begin
        WriteInteger(S, 'TabellenTyp', Ord(TabellenTyp));
        S1 := Format('%10.5f', [a0]);
        WriteString(S, 'a0', S1);
        S1 := Format('%10.5f', [a1]);
        WriteString(S, 'a1', S1);
        S1 := Format('%10.5f', [a2]);
        WriteString(S, 'a2', S1);
        S1 := Format('%10.5f', [x0]);
        WriteString(S, 'x0', S1);
        S1 := Format('%10.5f', [x1]);
        WriteString(S, 'x1', S1);
        S1 := Format('%10.5f', [x2]);
        WriteString(S, 'x2', S1);
      end;

      S := 'Mast';
      WriteInteger(S, 'MastL', FiMastL);
      WriteInteger(S, 'Mastunten', FiMastunten);
      WriteInteger(S, 'Mastoben', FiMastoben);
      WriteInteger(S, 'MastfallVorlauf', FiMastfallVorlauf);
      tempEI := Round(EI / 1E6);
      WriteInteger(S, 'EI', tempEI);

      S := 'Ist';
      WriteInteger(S, 'Controller', Round(GSB.Controller.Ist));
      WriteInteger(S, 'Winkel', Round(GSB.Winkel.Ist));
      WriteInteger(S, 'Vorstag', Round(GSB.Vorstag.Ist));
      WriteInteger(S, 'Wante', Round(GSB.Wante.Ist));
      WriteInteger(S, 'Woben', Round(GSB.Woben.Ist));
      WriteInteger(S, 'SalingH', Round(GSB.SalingH.Ist));
      WriteInteger(S, 'SalingA', Round(GSB.SalingA.Ist));
      WriteInteger(S, 'SalingL', Round(GSB.SalingL.Ist));
      WriteInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Ist));
      WriteInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Ist));

      S := 'Min';
      WriteInteger(S, 'Controller', Round(GSB.Controller.Min));
      WriteInteger(S, 'Winkel', Round(GSB.Winkel.Min));
      WriteInteger(S, 'Vorstag', Round(GSB.Vorstag.Min));
      WriteInteger(S, 'Wante', Round(GSB.Wante.Min));
      WriteInteger(S, 'Woben', Round(GSB.Woben.Min));
      WriteInteger(S, 'SalingH', Round(GSB.SalingH.Min));
      WriteInteger(S, 'SalingA', Round(GSB.SalingA.Min));
      WriteInteger(S, 'SalingL', Round(GSB.SalingL.Min));
      WriteInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Min));
      WriteInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Min));

      S := 'Max';
      WriteInteger(S, 'Controller', Round(GSB.Controller.Max));
      WriteInteger(S, 'Winkel', Round(GSB.Winkel.Max));
      WriteInteger(S, 'Vorstag', Round(GSB.Vorstag.Max));
      WriteInteger(S, 'Wante', Round(GSB.Wante.Max));
      WriteInteger(S, 'Woben', Round(GSB.Woben.Max));
      WriteInteger(S, 'SalingH', Round(GSB.SalingH.Max));
      WriteInteger(S, 'SalingA', Round(GSB.SalingA.Max));
      WriteInteger(S, 'SalingL', Round(GSB.SalingL.Max));
      WriteInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Max));
      WriteInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Max));

      S := 'Koordinaten Rumpf';
      WriteInteger(S, 'A0x', Round(iP.A0.X));
      WriteInteger(S, 'A0y', Round(iP.A0.Y));
      WriteInteger(S, 'A0z', Round(iP.A0.Z));
      WriteInteger(S, 'B0x', Round(iP.B0.X));
      WriteInteger(S, 'B0y', Round(iP.B0.Y));
      WriteInteger(S, 'B0z', Round(iP.B0.Z));
      WriteInteger(S, 'C0x', Round(iP.C0.X));
      WriteInteger(S, 'C0y', Round(iP.C0.Y));
      WriteInteger(S, 'C0z', Round(iP.C0.Z));
      WriteInteger(S, 'D0x', Round(iP.D0.X));
      WriteInteger(S, 'D0y', Round(iP.D0.Y));
      WriteInteger(S, 'D0z', Round(iP.D0.Z));
      WriteInteger(S, 'E0x', Round(iP.E0.X));
      WriteInteger(S, 'E0y', Round(iP.E0.Y));
      WriteInteger(S, 'E0z', Round(iP.E0.Z));
      WriteInteger(S, 'F0x', Round(iP.F0.X));
      WriteInteger(S, 'F0y', Round(iP.F0.Y));
      WriteInteger(S, 'F0z', Round(iP.F0.Z));

      S := 'Koordinaten Rigg';
      WriteInteger(S, 'Ax', Round(iP.A.X));
      WriteInteger(S, 'Ay', Round(iP.A.Y));
      WriteInteger(S, 'Az', Round(iP.A.Z));
      WriteInteger(S, 'Bx', Round(iP.B.X));
      WriteInteger(S, 'By', Round(iP.B.Y));
      WriteInteger(S, 'Bz', Round(iP.B.Z));
      WriteInteger(S, 'Cx', Round(iP.C.X));
      WriteInteger(S, 'Cy', Round(iP.C.Y));
      WriteInteger(S, 'Cz', Round(iP.C.Z));
      WriteInteger(S, 'Dx', Round(iP.D.X));
      WriteInteger(S, 'Dy', Round(iP.D.Y));
      WriteInteger(S, 'Dz', Round(iP.D.Z));
      WriteInteger(S, 'Ex', Round(iP.E.X));
      WriteInteger(S, 'Ey', Round(iP.E.Y));
      WriteInteger(S, 'Ez', Round(iP.E.Z));
      WriteInteger(S, 'Fx', Round(iP.F.X));
      WriteInteger(S, 'Fy', Round(iP.F.Y));
      WriteInteger(S, 'Fz', Round(iP.F.Z));

      S := 'EA';
      for i := 0 to 19 do
      begin
        S1 := IntToStr(i);
        S2 := Format('%.6g', [rEA.V[i]]);
        WriteString(S, S1, S2);
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TRggDocument.GetDefaultDocument;
begin
  if WantLogoData then
    GetLogoDoc
  else
    GetDefaultDoc;
end;

procedure TRggDocument.GetDefaultDoc;
const
  EModulStahl = 210E3; { N/mm^2 }
//  EModulAlu = 70E3; { N/mm^2 } // not used
  EAgross = 100E6; { N }
  EARumpf = 10E6; { N }
  EASaling = 1E6; { N }
begin
  // see (update) similar code (duplication) in TGetriebe.GetDefaultData

  { Signature }
  Signature := RggDocSignature;

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctDruck;
  CalcTyp := ctBiegeKnicken;

  { Mast: Abmessungen }
  FiMastL := 6115; { Gesamtlänge Mast }
  FiMastunten := 2600; { unterer Teil Mast }
  FiMastoben := 2000; { oberer Teil Mast }
  FiMastfallVorlauf := 5000; { Abstand der Meßmarken }
  FiControllerAnschlag := 50;
  FiReserved := 0;

  { Rumpf: Koordinaten }
  iP.A0.X := 2560; { Pütting Stbd }
  iP.A0.Y := -765;
  iP.A0.Z := 430;

  iP.B0.X := 2560; { Püttinge Bb }
  iP.B0.Y := 765;
  iP.B0.Z := 430;

  iP.C0.X := 4140; { Vorstag }
  iP.C0.Y := 0;
  iP.C0.Z := 340;

  iP.D0.X := 2870; { Mastfuß }
  iP.D0.Y := 0;
  iP.D0.Z := -100;

  iP.E0.X := 2970; { Controller }
  iP.E0.Y := 0;
  iP.E0.Z := 450;

  iP.F0.X := -30; { Spiegel }
  iP.F0.Y := 0;
  iP.F0.Z := 300;

  iP.P0 := iP.A0;
  iP.P0.Y := 0;

  { iP.A]..iP.F] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA.D0C := EAgross;
  rEA.C0D0 := EARumpf;
  rEA.B0C0 := EARumpf;
  rEA.A0C0 := EARumpf;
  rEA.B0D0 := EARumpf;
  rEA.A0D0 := EARumpf;
  rEA.A0B0 := EARumpf;
  rEA.B0B := 13 * EModulStahl;
  rEA.A0A := 13 * EModulStahl;
  rEA.BD := EAgross;
  rEA.AD := EAgross;
  rEA.AB := EASaling;
  rEA.BC := 13 * EModulStahl;
  rEA.AC := 13 * EModulStahl;
  rEA.C0C := 13 * EModulStahl;
  rEA.DC := EAgross;
  rEA.D0D := EAgross;
  rEA.ED := EAgross;
  rEA.D0E := EAgross;
  rEA.E0E := EAgross;

  EI := 14.7E9; { Nmm^2 }

  { Grenzwerte und Istwerte }
  GSB.Controller.Ist := 100; { Controllerposition bzw. Abstand E0-E }
  GSB.Winkel.Ist := 95; { Winkel der unteren Wantabschnitte Winkel in Grad }
  GSB.Vorstag.Ist := 4500;
  GSB.Wante.Ist := 4120;
  GSB.Woben.Ist := 2020;
  GSB.SalingH.Ist := 220;
  GSB.SalingA.Ist := 850;
  GSB.SalingL.Ist := Round(sqrt(sqr(GSB.SalingH.Ist) + sqr(GSB.SalingA.Ist / 2)));
  GSB.VorstagOS.Ist := GSB.Vorstag.Ist;
  GSB.WPowerOS.Ist := 1000; { angenommene Wantenspannung 3d }

  GSB.InitStepDefault;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB.Controller.Min := 50;
  GSB.Controller.Max := 200;
  GSB.Winkel.Min := 85;
  GSB.Winkel.Max := 105;
  GSB.Vorstag.Min := 4400;
  GSB.Vorstag.Max := 5000; //4600;
  GSB.Wante.Min := 4050;
  GSB.Wante.Max := 4200;
  GSB.Woben.Min := 2000;
  GSB.Woben.Max := 2070;
  GSB.SalingH.Min := 140;
  GSB.SalingH.Max := 300;
  GSB.SalingA.Min := 780;
  GSB.SalingA.Max := 1000;
  GSB.SalingL.Min := 450;
  GSB.SalingL.Max := 600;
  GSB.VorstagOS.Min := 4200;
  GSB.VorstagOS.Max := 4700;
  GSB.WPowerOS.Min := 100;
  GSB.WPowerOS.Max := 3000;

  { TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten; } { siehe RggTypes }
  with TrimmTabDaten do
  begin
    TabellenTyp := itGerade;
    a0 := 0; { zur Zeit nicht verwendet }
    a1 := 0.1;
    a2 := 0;
    x0 := 0; { zur Zeit nicht verwendet }
    x1 := 500;
    x2 := 1000;
  end;
end;

procedure TRggDocument.GetLogoDoc;
const
  EModulStahl = 210E3; { N/mm^2 }
  //EModulAlu = 70E3; { N/mm^2 }
  EAgross = 100E6; { N }
  EARumpf = 10E6; { N }
  EASaling = 1E6; { N }

var
  f, ox, oz: Integer;
begin
  // see similar code (duplication) in TGetriebe.GetLogoData

  { Signature }
  Signature := string(RggDocSignature);

  ox := 1400;
  oz := -350;

  f := 18;

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctDruck;
  CalcTyp := ctBiegeKnicken;

  { Mast: Abmessungen }
  FiMastL := Round((40 + sqrt(250) * 10) * f); { Gesamtlänge Mast }
  FiMastunten := Round((sqrt(40) + sqrt(10)) * 10 * f); { unterer Teil Mast }
  FiMastoben := Round(sqrt(40) * 10 * f); { oberer Teil Mast }
  FiMastfallVorlauf := Round(FiMastL * 0.75); { Abstand der Meßmarken }
  FiControllerAnschlag := 50;
  FiReserved := 0;

  { RumpfKoordinaten in mm }
  iP.A0.X := 30 * f + ox; { Pütting Stbd }
  iP.A0.Y := -40 * f;
  iP.A0.Z := 40 * f + oz;

  iP.B0.X := 30 * f + ox;
  iP.B0.Y := 40 * f;
  iP.B0.Z := 40 * f + oz;

  iP.C0.X := 150 * f + ox;
  iP.C0.Y := 0;
  iP.C0.Z := 40 * f + oz;

  iP.D0.X := 80 * f + ox;
  iP.D0.Y := 0;
  iP.D0.Z := 10 * f + oz;

  iP.E0.X := 85 * f + ox;
  iP.E0.Y := 0;
  iP.E0.Z := 50 * f + oz;

  iP.F0.X := -85 * f + ox;
  iP.F0.Y := 0;
  iP.F0.Z := 40 * f + oz;

  iP.P0 := iP.A0;
  iP.P0.Y := 0;

  { iP.A]..iP.F] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA.D0C := EAgross;
  rEA.C0D0 := EARumpf;
  rEA.B0C0 := EARumpf;
  rEA.A0C0 := EARumpf;
  rEA.B0D0 := EARumpf;
  rEA.A0D0 := EARumpf;
  rEA.A0B0 := EARumpf;
  rEA.B0B := 13 * EModulStahl;
  rEA.A0A := 13 * EModulStahl;
  rEA.BD := EAgross;
  rEA.AD := EAgross;
  rEA.AB := EASaling;
  rEA.BC := 13 * EModulStahl;
  rEA.AC := 13 * EModulStahl;
  rEA.C0C := 13 * EModulStahl;
  rEA.DC := EAgross;
  rEA.D0D := EAgross;
  rEA.ED := EAgross;
  rEA.D0E := EAgross;
  rEA.E0E := EAgross;

  EI := 14.7E9; { Nmm^2 }

  { Grenzwerte und Istwerte }

  GSB.Controller.Ist := 100; { Controllerposition bzw. Abstand E0-E }
  GSB.Winkel.Ist := Round(90 + RadToDeg(arctan2(1, 3)));
  { Winkel der unteren Wantabschnitte Winkel in Grad }
  GSB.Vorstag.Ist := Round(sqrt(288) * 10 * f);
  GSB.Wante.Ist := Round((sqrt(40) + sqrt(56)) * 10 * f);
  GSB.Woben.Ist := Round(sqrt(56) * 10 * f);
  GSB.SalingH.Ist := 40 * f;
  GSB.SalingA.Ist := 80 * f;
  GSB.SalingL.Ist := Round(sqrt(sqr(GSB.SalingH.Ist) + sqr(GSB.SalingA.Ist / 2)));
  GSB.VorstagOS.Ist := GSB.Vorstag.Ist;
  GSB.WPowerOS.Ist := 1000; { angenommene Wantenspannung 3d }
  GSB.MastfallVorlauf.Ist := FiMastfallVorlauf;

  GSB.InitStepDefault;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB.Controller.Min := 50;
  GSB.Controller.Max := 200;
  GSB.Winkel.Min := 70;
  GSB.Winkel.Max := 120;
  GSB.Vorstag.Min := GSB.Vorstag.Ist - 10 * f;
  GSB.Vorstag.Max := GSB.Vorstag.Ist + 10 * f;
  GSB.Wante.Min := GSB.Wante.Ist - 10 * f;
  GSB.Wante.Max := GSB.Wante.Ist + 10 * f;
  GSB.Woben.Min := GSB.Woben.Ist - 10 * f;
  GSB.Woben.Max := GSB.Woben.Ist + 10 * f;
  GSB.SalingH.Min := GSB.SalingH.Ist - 10 * f;
  GSB.SalingH.Max := GSB.SalingH.Ist + 10 * f;
  GSB.SalingA.Min := GSB.SalingA.Ist - 10 * f;
  GSB.SalingA.Max := GSB.SalingA.Ist + 10 * f;
  GSB.SalingL.Min := GSB.SalingL.Ist - 10 * f;
  GSB.SalingL.Max := GSB.SalingL.Ist + 10 * f;
  GSB.VorstagOS.Min := GSB.VorstagOS.Ist - 10 * f;
  GSB.VorstagOS.Max := GSB.VorstagOS.Ist + 10 * f;
  GSB.WPowerOS.Min := 100;
  GSB.WPowerOS.Max := 3000;
  GSB.MastfallVorlauf.Min := GSB.MastfallVorlauf.Ist - 10 * f;
  GSB.MastfallVorlauf.Max := GSB.MastfallVorlauf.Ist + 10 * f;

  { TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten; } { siehe RggTypes }
  with TrimmTabDaten do
  begin
    TabellenTyp := itGerade;
    a0 := 0; { zur Zeit nicht verwendet }
    a1 := 0.1;
    a2 := 0;
    x0 := 0; { zur Zeit nicht verwendet }
    x1 := 500;
    x2 := 1000;
  end;
end;

procedure TRggDocument.DumpToMemo(Memo: TStrings);
var
  S1, S2: string;
  i, tempEI: Integer;
begin
  with Memo do
  begin
    Add('[Rigg]');
    Add(Format('SalingTyp=%d', [Ord(SalingTyp)]));
    Add(Format('ControllerTyp=%d', [Ord(ControllerTyp)]));
    Add(Format('CalcTyp=%d', [Ord(CalcTyp)]));
    Add('');

    Add('[Trimmtabelle]');
    with TrimmTabDaten do
    begin
      Add(Format('TabellenTyp=%d', [Ord(TabellenTyp)]));
      Add(Format('a0=%10.5f', [a0]));
      Add(Format('a1=%10.5f', [a1]));
      Add(Format('a2=%10.5f', [a2]));
      Add(Format('x0=%10.5f', [x0]));
      Add(Format('x1=%10.5f', [x1]));
      Add(Format('x2=%10.5f', [x2]));
    end;
    Add('');

    Add('[Mast]');
    Add(Format('MastL=%d', [FiMastL]));
    Add(Format('Mastunten=%d', [FiMastunten]));
    Add(Format('Mastoben=%d', [FiMastoben]));
    Add(Format('MastfallVorlauf=%d', [FiMastfallVorlauf]));
    tempEI := Round(EI / 1E6);
    Add(Format('EI=%d', [tempEI]));
    Add('');

    Add('[Ist]');
    Add(Format('Controller=%d', [GSB.Controller.Ist]));
    Add(Format('Winkel=%d', [GSB.Winkel.Ist]));
    Add(Format('Vorstag=%d', [GSB.Vorstag.Ist]));
    Add(Format('Wante=%d', [GSB.Wante.Ist]));
    Add(Format('Woben=%d', [GSB.Woben.Ist]));
    Add(Format('SalingH=%d', [GSB.SalingH.Ist]));
    Add(Format('SalingA=%d', [GSB.SalingA.Ist]));
    Add(Format('SalingL=%d', [GSB.SalingL.Ist]));
    Add(Format('VorstagOS=%d', [GSB.VorstagOS.Ist]));
    Add(Format('WPowerOS=%d', [GSB.WPowerOS.Ist]));
    Add('');

    Add('[Min]');
    Add(Format('Controller=%d', [GSB.Controller.Min]));
    Add(Format('Winkel=%d', [GSB.Winkel.Min]));
    Add(Format('Vorstag=%d', [GSB.Vorstag.Min]));
    Add(Format('Wante=%d', [GSB.Wante.Min]));
    Add(Format('Woben=%d', [GSB.Woben.Min]));
    Add(Format('SalingH=%d', [GSB.SalingH.Min]));
    Add(Format('SalingA=%d', [GSB.SalingA.Min]));
    Add(Format('SalingL=%d', [GSB.SalingL.Min]));
    Add(Format('VorstagOS=%d', [GSB.VorstagOS.Min]));
    Add(Format('WPowerOS=%d', [GSB.WPowerOS.Min]));
    Add('');

    Add('[Max]');
    Add(Format('Controller=%d', [GSB.Controller.Max]));
    Add(Format('Winkel=%d', [GSB.Winkel.Max]));
    Add(Format('Vorstag=%d', [GSB.Vorstag.Max]));
    Add(Format('Wante=%d', [GSB.Wante.Max]));
    Add(Format('Woben=%d', [GSB.Woben.Max]));
    Add(Format('SalingH=%d', [GSB.SalingH.Max]));
    Add(Format('SalingA=%d', [GSB.SalingA.Max]));
    Add(Format('SalingL=%d', [GSB.SalingL.Max]));
    Add(Format('VorstagOS=%d', [GSB.VorstagOS.Max]));
    Add(Format('WPowerOS=%d', [GSB.WPowerOS.Max]));
    Add('');

    Add('[Koordinaten Rumpf]');
    Add(Format('A0x=%d', [iP.A0.X]));
    Add(Format('A0y=%d', [iP.A0.Y]));
    Add(Format('A0z=%d', [iP.A0.Z]));
    Add(Format('B0x=%d', [iP.B0.X]));
    Add(Format('B0y=%d', [iP.B0.Y]));
    Add(Format('B0z=%d', [iP.B0.Z]));
    Add(Format('C0x=%d', [iP.C0.X]));
    Add(Format('C0y=%d', [iP.C0.Y]));
    Add(Format('C0z=%d', [iP.C0.Z]));
    Add(Format('D0x=%d', [iP.D0.X]));
    Add(Format('D0y=%d', [iP.D0.Y]));
    Add(Format('D0z=%d', [iP.D0.Z]));
    Add(Format('E0x=%d', [iP.E0.X]));
    Add(Format('E0y=%d', [iP.E0.Y]));
    Add(Format('E0z=%d', [iP.E0.Z]));
    Add(Format('F0x=%d', [iP.F0.X]));
    Add(Format('F0y=%d', [iP.F0.Y]));
    Add(Format('F0z=%d', [iP.F0.Z]));
    Add('');

    Add('[Koordinaten Rigg]');
    Add(Format('Ax=%d', [iP.A.X]));
    Add(Format('Ay=%d', [iP.A.Y]));
    Add(Format('Az=%d', [iP.A.Z]));
    Add(Format('Bx=%d', [iP.B.X]));
    Add(Format('By=%d', [iP.B.Y]));
    Add(Format('Bz=%d', [iP.B.Y]));
    Add(Format('Cx=%d', [iP.C.X]));
    Add(Format('Cy=%d', [iP.C.Y]));
    Add(Format('Cz=%d', [iP.C.Z]));
    Add(Format('Dx=%d', [iP.D.X]));
    Add(Format('Dy=%d', [iP.D.Y]));
    Add(Format('Dz=%d', [iP.D.Z]));
    Add(Format('Ex=%d', [iP.E.X]));
    Add(Format('Ey=%d', [iP.E.Y]));
    Add(Format('Ez=%d', [iP.E.Z]));
    Add(Format('Fx=%d', [iP.F.X]));
    Add(Format('Fy=%d', [iP.F.Y]));
    Add(Format('Fz=%d', [iP.F.Z]));
    Add('');

    Add('[EA]');
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := Format('%.6g', [rEA.V[i]]);
      Add(Format('%s=%s', [S1, S2]));
    end;
    Add('');
  end;
end;

procedure TRggDocument.LoadFromXML(s: string);
begin
end;

{$ifdef MSWindows}
procedure TRggDocument.WriteXML(ML: TStrings);
var
  doc: IXMLDocument;
  root: IXMLNode;
begin
  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];
  doc.Active := True;

  root := doc.AddChild('RG');
  SaveToXML(root);
  ML.Text := doc.XML.Text;
  doc.Active := false;
end;

function TRggDocument.SaveToXML(d: IXMLNode): string;
var
  OldDecimalSeparator: Char;
  i: Integer;
  rp: TRiggPoint;
  tempEI: single;
  SBParam: TsbParam; // = (Ist, Min, Max, TinyStep, BigStep);
  SBName: TsbName;
  a, b, c: IXMLNode;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    { RiggType }
    d.SetAttribute('SalingTyp', IntToStr(Ord(SalingTyp)));
    d.SetAttribute('ControllerTyp', IntToStr(Ord(ControllerTyp)));
    d.SetAttribute('CalcTyp', IntToStr(Ord(CalcTyp)));

    { GSB - Grenzwerte und Istwerte }
    a := d.AddChild('Scrollbar');
    for SBName := Low(TsbName) to fpSalingL do
    begin
      b := a.AddChild('Param');
      b.SetAttribute('ID', XMLSBName[SBName]);
      b.SetAttribute('Label', XMLSBNameLabels[SBName]);
      for SBParam := Low(TSBParam) to High(TSBParam) do
      begin
        b.SetAttribute(XMLSBParamLabels[SBParam], IntToStr(Round(GSB.GetSB(SBName).GetValue(SBParam))));
      end;
    end;

    { Koord }
    a := d.AddChild('Koordinaten');

    b := a.AddChild('Rigg');
    for rp := ooA to ooP do
    begin
      c := b.AddChild('Koord');
      c.SetAttribute('ID', KoordTexteXML[rp]);
      c.SetAttribute('Label', XMLKoordLabels[rp]);
      c.SetAttribute('x', IntToStr(Round(iP.V[rp].X)));
      c.SetAttribute('y', IntToStr(Round(iP.V[rp].Y)));
      c.SetAttribute('z', IntToStr(Round(iP.V[rp].Z)));
    end;

    b := a.AddChild('Rumpf');
    for rp := ooA0 to ooP0 do
    begin
      c := b.AddChild('Koord');
      c.SetAttribute('ID', KoordTexteXML[rp]);
      c.SetAttribute('Label', XMLKoordLabels[rp]);
      c.SetAttribute('x', IntToStr(Round(iP.V[rp].X)));
      c.SetAttribute('y', IntToStr(Round(iP.V[rp].Y)));
      c.SetAttribute('z', IntToStr(Round(iP.V[rp].Z)));
    end;

    { Mast: Abmessungen in mm }
    a := d.AddChild('Mast');

    b := a.AddChild('L');
    b.SetAttribute('ID', 'D0F');
    b.SetAttribute('Label', 'Mastfuss-Top');
    b.Text := IntToStr(FiMastL);

    b := a.AddChild('L');
    b.SetAttribute('ID', 'D0D');
    b.SetAttribute('Label', 'Mastfuss-Saling');
    b.Text :=IntToStr(FiMastunten);

    b := a.AddChild('L');
    b.SetAttribute('ID', 'DC');
    b.SetAttribute('Label', 'Saling-Vorstag');
    b.Text := IntToStr(FiMastoben);

    b := a.AddChild('L');
    b.SetAttribute('ID', 'FX');
    b.SetAttribute('Label', 'MastfallVorlauf');
    b.Text := IntToStr(FiMastfallVorlauf);

    b := a.AddChild('L');
    b.SetAttribute('ID', 'C0X');
    b.SetAttribute('Label', 'ControllerAnschlag');
    b.Text := IntToStr(FiControllerAnschlag);

    FormatSettings.DecimalSeparator := '.';

    if WantFestigkeitsWerteInXml then
    begin
      { Festigkeitswerte }
      a := d.AddChild('Festigkeit');
      b := a.AddChild('ZugDruck');
      for i := 0 to 19 do
      begin
        c := b.AddChild('EA');
        c.SetAttribute('Stab', IntToStr(i));
        c.SetAttribute('Value', Format('%.6g', [rEA.V[i]]));
      end;
      b := a.AddChild('Biegung');
      c := b.AddChild('EI');
      tempEI := Round(EI/1E6);
      c.SetAttribute('Label', 'Mast');
      c.SetAttribute('Value', Format('%.6g', [tempEI]));
    end;

    if WantTrimmTabInXml then
    begin
      { Trimmtabelle }
      a := d.AddChild('Trimmtabelle');
      with TrimmTabDaten do
      begin
        a.SetAttribute('KurvenTyp', IntToStr(Ord(TabellenTyp)));
        b := a.AddChild('T');
        b.SetAttribute('ID', 'a0');
        b.Text := Format('%.6g',[a0]);

        b := a.AddChild('T');
        b.SetAttribute('ID', 'a1');
        b.Text := Format('%.6g',[a1]);

        b := a.AddChild('T');
        b.SetAttribute('ID', 'a2');
        b.Text := Format('%.6g',[a2]);

        b := a.AddChild('T');
        b.SetAttribute('ID', 'x0');
        b.Text := Format('%.6g',[x0]);

        b := a.AddChild('T');
        b.SetAttribute('ID', 'x1');
        b.Text := Format('%.6g',[x1]);

        b := a.AddChild('T');
        b.SetAttribute('ID', 'x2');
        b.Text := Format('%.6g',[x2]);
      end;
    end;

  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
  result := '';
end;
{$endif}

end.
