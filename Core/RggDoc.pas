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
  stDrehbar, stOhne bzw. stOhne_2 kann auch mit SalingTyp stFest gespeichert
  werden.

  Die Festigkeitswerte EA und EI sowie die meisten Felder der Trimmtabelle sind
  Gleitkommazahlen. Wenn als Calctyp Biegeknicken angegeben ist, dann werden in
  der 'Trimmtabelle' die Biegeknickparameter gespeichert.

  Binärformat:
  Pos Size Description, Typ
  0   8    Signature, Array[1..8] of Char
  8   4    SalingTyp, Integer
  12  4    ControllerTyp, Integer
  16  4    CalcTyp, Integer
  20  4    FiMastL, Integer
  24  4    FiMastunten, Integer
  28  4    FiMastoben, Integer
  32  4    FiMastfallVorlauf, Integer
  36  4    FiControllerAnschlag, Integer
  40  4    FiReserved, Integer
  iP, TIntRiggPoints
  rEA, TRiggLvektor
  EI, double
  GSB, TsbArray
  TrimmTabDaten, TTrimmTabDaten

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
    FFileName: string; // nur zum Zwischenspeichern des Dateinamens
    procedure GetLogoDoc;
    procedure GetDefaultDoc;
    procedure LoadSignatureFromStream(strm: TStream);
    procedure SaveSignatureToStream(strm: TStream);
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
    iP: TIntRiggPoints; { Array enthält auch die Riggkoordinaten }
    { Festigkeitswerte }
    rEA: TRiggLvektor; { N }
    EI: double; { Nmm^2 }
    { Grenzwerte und Istwerte }
    GSB: TRggFA;
    { Trimmtabelle }
    TrimmTabDaten: TTrimmTabDaten;

    WantFestigkeitsWerteInXml: Boolean;
    WantTrimmTabInXml: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure TestStream;

    function SignatureOK: Boolean; virtual;
    procedure GetDefaultDocument; virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    procedure LoadFromStream(strm: TStream); virtual;
    procedure SaveToStream(strm: TStream); virtual;
    procedure LoadFromIniFile(FileName: string); virtual;
    procedure WriteToIniFile(FileName: string); virtual;
    procedure DumpToMemo(Memo: TStrings); virtual;
    procedure ReadFromDFM(Memo: TStrings);
    procedure WriteToDFM(Memo: TStrings);
    function SaveToString: string;
    procedure LoadFromString(s: string);
{$ifdef MSWindows}
    procedure WriteXML(ML: TStrings);
    function SaveToXML(d: IXMLNode): string;
{$endif}
    procedure LoadFromXML(s: string);
    function SaveToXMLBase64: string;
    function LoadFromXMLBase64(s: string): string;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.RG.Def,
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
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    FFileName := FileName;
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggDocument.SaveToFile(FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TRggDocument.TestStream;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TRggDocument.SaveSignatureToStream(strm: TStream);
var
  ss: TStringStream;
  c: Integer;
begin
  //strm.WriteBuffer(RggDocSignature[1], 8);

  ss := TStringStream.Create('');
  ss.WriteString(RggDocSignature);
  c := ss.Position;
  ss.Position := 0;
  strm.CopyFrom(ss, c);
  ss.Free;
end;

procedure TRggDocument.LoadSignatureFromStream(strm: TStream);
var
  ss: TStringStream;
  c: Integer;
begin
  { Signature }
  ss := TStringStream.Create('');
  ss.CopyFrom(strm, RggDocSignature.Length);
  c := ss.Position;
  ss.Position := 0;
  Signature := 'temp';
  Signature := ss.ReadString(c);
  ss.Free;
end;

procedure TRggDocument.LoadFromStream(strm: TStream);
var
  temp: Integer;
begin
  { Signature }
  LoadSignatureFromStream(strm);
  if not SignatureOK then
    raise EFileFormatError.Create('FormatError');

  { Rigg: Typ }
  strm.ReadBuffer(temp, SizeOf(Integer));
  SalingTyp := TSalingTyp(temp);
  strm.ReadBuffer(temp, SizeOf(Integer));
  ControllerTyp := TControllerTyp(temp);
  strm.ReadBuffer(temp, SizeOf(Integer));
  CalcTyp := TCalcTyp(temp);
  { Mast: Abmessungen }
  strm.ReadBuffer(FiMastL, SizeOf(Integer));
  strm.ReadBuffer(FiMastunten, SizeOf(Integer));
  strm.ReadBuffer(FiMastoben, SizeOf(Integer));
  strm.ReadBuffer(FiMastfallVorlauf, SizeOf(Integer));
  strm.ReadBuffer(FiControllerAnschlag, SizeOf(Integer));
  strm.ReadBuffer(FiReserved, SizeOf(Integer));
  { Rumpf: Koordinaten }
  strm.ReadBuffer(iP, SizeOf(TIntRiggPoints));
  { Festigkeitswerte }
  strm.ReadBuffer(rEA, SizeOf(TRiggLvektor));
  strm.ReadBuffer(EI, SizeOf(double));
  { Grenzwerte }
  GSB.LoadFromStream(strm);
  { Trimmtabelle }
  strm.ReadBuffer(TrimmTabDaten, SizeOf(TTrimmTabDaten));
end;

procedure TRggDocument.SaveToStream(strm: TStream);
begin
  { Signature }
  SaveSignatureToStream(strm);

  { Rigg: Typ }
  strm.WriteBuffer(SalingTyp, SizeOf(Integer));
  strm.WriteBuffer(ControllerTyp, SizeOf(Integer));
  strm.WriteBuffer(CalcTyp, SizeOf(Integer));
  { Mast: Abmessungen }
  strm.WriteBuffer(FiMastL, SizeOf(Integer));
  strm.WriteBuffer(FiMastunten, SizeOf(Integer));
  strm.WriteBuffer(FiMastoben, SizeOf(Integer));
  strm.WriteBuffer(FiMastfallVorlauf, SizeOf(Integer));
  strm.WriteBuffer(FiControllerAnschlag, SizeOf(Integer));
  strm.WriteBuffer(FiReserved, SizeOf(Integer));
  { Rumpf: Koordinaten }
  strm.WriteBuffer(iP, SizeOf(TIntRiggPoints));
  { Festigkeitswerte }
  strm.WriteBuffer(rEA, SizeOf(TRiggLvektor));
  strm.WriteBuffer(EI, SizeOf(double));
  { Grenzwerte }
  GSB.SaveToStream(strm);
  { Trimmtabelle }
  strm.WriteBuffer(TrimmTabDaten, SizeOf(TTrimmTabDaten));
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
      iP[ooA0, x] := ReadInteger(S, 'A0x', Round(iP[ooA0, x]));
      iP[ooA0, y] := ReadInteger(S, 'A0y', Round(iP[ooA0, y]));
      iP[ooA0, z] := ReadInteger(S, 'A0z', Round(iP[ooA0, z]));
      iP[ooB0, x] := ReadInteger(S, 'B0x', Round(iP[ooB0, x]));
      iP[ooB0, y] := ReadInteger(S, 'B0y', Round(iP[ooB0, y]));
      iP[ooB0, z] := ReadInteger(S, 'B0z', Round(iP[ooB0, z]));
      iP[ooC0, x] := ReadInteger(S, 'C0x', Round(iP[ooC0, x]));
      iP[ooC0, y] := ReadInteger(S, 'C0y', Round(iP[ooC0, y]));
      iP[ooC0, z] := ReadInteger(S, 'C0z', Round(iP[ooC0, z]));
      iP[ooD0, x] := ReadInteger(S, 'D0x', Round(iP[ooD0, x]));
      iP[ooD0, y] := ReadInteger(S, 'D0y', Round(iP[ooD0, y]));
      iP[ooD0, z] := ReadInteger(S, 'D0z', Round(iP[ooD0, z]));
      iP[ooE0, x] := ReadInteger(S, 'E0x', Round(iP[ooE0, x]));
      iP[ooE0, y] := ReadInteger(S, 'E0y', Round(iP[ooE0, y]));
      iP[ooE0, z] := ReadInteger(S, 'E0z', Round(iP[ooE0, z]));
      iP[ooF0, x] := ReadInteger(S, 'F0x', Round(iP[ooF0, x]));
      iP[ooF0, y] := ReadInteger(S, 'F0y', Round(iP[ooF0, y]));
      iP[ooF0, z] := ReadInteger(S, 'F0z', Round(iP[ooF0, z]));

      S := 'Koordinaten Rigg';
      iP[ooA, x] := ReadInteger(S, 'Ax', Round(iP[ooA, x]));
      iP[ooA, y] := ReadInteger(S, 'Ay', Round(iP[ooA, y]));
      iP[ooA, z] := ReadInteger(S, 'Az', Round(iP[ooA, z]));
      iP[ooB, x] := ReadInteger(S, 'Bx', Round(iP[ooB, x]));
      iP[ooB, y] := ReadInteger(S, 'By', Round(iP[ooB, y]));
      iP[ooB, z] := ReadInteger(S, 'Bz', Round(iP[ooB, z]));
      iP[ooC, x] := ReadInteger(S, 'Cx', Round(iP[ooC, x]));
      iP[ooC, y] := ReadInteger(S, 'Cy', Round(iP[ooC, y]));
      iP[ooC, z] := ReadInteger(S, 'Cz', Round(iP[ooC, z]));
      iP[ooD, x] := ReadInteger(S, 'Dx', Round(iP[ooD, x]));
      iP[ooD, y] := ReadInteger(S, 'Dy', Round(iP[ooD, y]));
      iP[ooD, z] := ReadInteger(S, 'Dz', Round(iP[ooD, z]));
      iP[ooE, x] := ReadInteger(S, 'Ex', Round(iP[ooE, x]));
      iP[ooE, y] := ReadInteger(S, 'Ey', Round(iP[ooE, y]));
      iP[ooE, z] := ReadInteger(S, 'Ez', Round(iP[ooE, z]));
      iP[ooF, x] := ReadInteger(S, 'Fx', Round(iP[ooF, x]));
      iP[ooF, y] := ReadInteger(S, 'Fy', Round(iP[ooF, y]));
      iP[ooF, z] := ReadInteger(S, 'Fz', Round(iP[ooF, z]));

      S := 'EA';
      for i := 0 to 19 do
      begin
        S1 := IntToStr(i);
        S2 := ReadString(S, S1, '100000');
        rEA[i] := StrToFloat(S2);
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
      WriteInteger(S, 'A0x', Round(iP[ooA0, x]));
      WriteInteger(S, 'A0y', Round(iP[ooA0, y]));
      WriteInteger(S, 'A0z', Round(iP[ooA0, z]));
      WriteInteger(S, 'B0x', Round(iP[ooB0, x]));
      WriteInteger(S, 'B0y', Round(iP[ooB0, y]));
      WriteInteger(S, 'B0z', Round(iP[ooB0, z]));
      WriteInteger(S, 'C0x', Round(iP[ooC0, x]));
      WriteInteger(S, 'C0y', Round(iP[ooC0, y]));
      WriteInteger(S, 'C0z', Round(iP[ooC0, z]));
      WriteInteger(S, 'D0x', Round(iP[ooD0, x]));
      WriteInteger(S, 'D0y', Round(iP[ooD0, y]));
      WriteInteger(S, 'D0z', Round(iP[ooD0, z]));
      WriteInteger(S, 'E0x', Round(iP[ooE0, x]));
      WriteInteger(S, 'E0y', Round(iP[ooE0, y]));
      WriteInteger(S, 'E0z', Round(iP[ooE0, z]));
      WriteInteger(S, 'F0x', Round(iP[ooF0, x]));
      WriteInteger(S, 'F0y', Round(iP[ooF0, y]));
      WriteInteger(S, 'F0z', Round(iP[ooF0, z]));

      S := 'Koordinaten Rigg';
      WriteInteger(S, 'Ax', Round(iP[ooA, x]));
      WriteInteger(S, 'Ay', Round(iP[ooA, y]));
      WriteInteger(S, 'Az', Round(iP[ooA, z]));
      WriteInteger(S, 'Bx', Round(iP[ooB, x]));
      WriteInteger(S, 'By', Round(iP[ooB, y]));
      WriteInteger(S, 'Bz', Round(iP[ooB, z]));
      WriteInteger(S, 'Cx', Round(iP[ooC, x]));
      WriteInteger(S, 'Cy', Round(iP[ooC, y]));
      WriteInteger(S, 'Cz', Round(iP[ooC, z]));
      WriteInteger(S, 'Dx', Round(iP[ooD, x]));
      WriteInteger(S, 'Dy', Round(iP[ooD, y]));
      WriteInteger(S, 'Dz', Round(iP[ooD, z]));
      WriteInteger(S, 'Ex', Round(iP[ooE, x]));
      WriteInteger(S, 'Ey', Round(iP[ooE, y]));
      WriteInteger(S, 'Ez', Round(iP[ooE, z]));
      WriteInteger(S, 'Fx', Round(iP[ooF, x]));
      WriteInteger(S, 'Fy', Round(iP[ooF, y]));
      WriteInteger(S, 'Fz', Round(iP[ooF, z]));

      S := 'EA';
      for i := 0 to 19 do
      begin
        S1 := IntToStr(i);
        S2 := Format('%.6g', [rEA[i]]);
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
  iP[ooA0, x] := 2560; { Pütting Stbd }
  iP[ooA0, y] := 765;
  iP[ooA0, z] := 430;

  iP[ooB0, x] := 2560; { Püttinge Bb }
  iP[ooB0, y] := -765;
  iP[ooB0, z] := 430;

  iP[ooC0, x] := 4140; { Vorstag }
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 340;

  iP[ooD0, x] := 2870; { Mastfuß }
  iP[ooD0, y] := 0;
  iP[ooD0, z] := -100;

  iP[ooE0, x] := 2970; { Controller }
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 450;

  iP[ooF0, x] := -30; { Spiegel }
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 300;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  { iP[ooA]..iP[ooF] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA[0] := EAgross;
  rEA[1] := EARumpf;
  rEA[2] := EARumpf;
  rEA[3] := EARumpf;
  rEA[4] := EARumpf;
  rEA[5] := EARumpf;
  rEA[6] := EARumpf;
  rEA[7] := 13 * EModulStahl;
  rEA[8] := 13 * EModulStahl;
  rEA[9] := EAgross;
  rEA[10] := EAgross;
  rEA[11] := EASaling;
  rEA[12] := 13 * EModulStahl;
  rEA[13] := 13 * EModulStahl;
  rEA[14] := 13 * EModulStahl;
  rEA[15] := EAgross;
  rEA[16] := EAgross;
  rEA[17] := EAgross;
  rEA[18] := EAgross;
  rEA[19] := EAgross;

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
  GSB.T1.Ist := 650;
  GSB.T2.Ist := 150;

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
  GSB.T1.Min := 0;
  GSB.T1.Max := 800;
  GSB.T2.Min := 1;
  GSB.T2.Max := 800;

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
  EModulAlu = 70E3; { N/mm^2 }
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
  iP[ooA0, x] := 30 * f + ox; { Pütting Stbd }
  iP[ooA0, y] := 40 * f;
  iP[ooA0, z] := 40 * f + oz;

  iP[ooB0, x] := 30 * f + ox;
  iP[ooB0, y] := -40 * f;
  iP[ooB0, z] := 40 * f + oz;

  iP[ooC0, x] := 150 * f + ox;
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 40 * f + oz;

  iP[ooD0, x] := 80 * f + ox;
  iP[ooD0, y] := 0;
  iP[ooD0, z] := 10 * f + oz;

  iP[ooE0, x] := 85 * f + ox;
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 50 * f + oz;

  iP[ooF0, x] := -85 * f + ox;
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 40 * f + oz;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  { iP[ooA]..iP[ooF] werden hier nicht gefüllt! }

  { Festigkeitswerte }
  rEA[0] := EAgross;
  rEA[1] := EARumpf;
  rEA[2] := EARumpf;
  rEA[3] := EARumpf;
  rEA[4] := EARumpf;
  rEA[5] := EARumpf;
  rEA[6] := EARumpf;
  rEA[7] := 13 * EModulStahl;
  rEA[8] := 13 * EModulStahl;
  rEA[9] := EAgross;
  rEA[10] := EAgross;
  rEA[11] := EASaling;
  rEA[12] := 13 * EModulStahl;
  rEA[13] := 13 * EModulStahl;
  rEA[14] := 13 * EModulStahl;
  rEA[15] := EAgross;
  rEA[16] := EAgross;
  rEA[17] := EAgross;
  rEA[18] := EAgross;
  rEA[19] := EAgross;

  EI := 14.7E9; { Nmm^2 }

  { Grenzwerte und Istwerte }

  GSB.Controller.Ist := 100; { Controllerposition bzw. Abstand E0-E }
  GSB.Winkel.Ist := Round(90 + arctan2(1, 3) * 180 / pi);
  { Winkel der unteren Wantabschnitte Winkel in Grad }
  GSB.Vorstag.Ist := Round(sqrt(288) * 10 * f);
  GSB.Wante.Ist := Round((sqrt(40) + sqrt(56)) * 10 * f);
  GSB.Woben.Ist := Round(sqrt(56) * 10 * f);
  GSB.SalingH.Ist := 40 * f;
  GSB.SalingA.Ist := 80 * f;
  GSB.SalingL.Ist := Round(sqrt(sqr(GSB.SalingH.Ist) + sqr(GSB.SalingA.Ist / 2)));
  GSB.VorstagOS.Ist := GSB.Vorstag.Ist;
  GSB.WPowerOS.Ist := 1000; { angenommene Wantenspannung 3d }
  GSB.T1.Ist := 500;
  GSB.T2.Ist := 100;
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
  GSB.T1.Min := 0;
  GSB.T1.Max := 800;
  GSB.T2.Min := 1;
  GSB.T2.Max := 800;
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

function TRggDocument.SignatureOK: Boolean;
var
  S, SCaption, S1: string;
begin
  result := False;
  if Signature = string(RggDocSignature) then
    result := True
  else
  begin
    SCaption := Format('Lesen von %s', [ExtractFileName(FFileName)]);
    S := 'FormatFehler';
    S1 := Copy(Signature, 1, 6);
    if S1 = 'RGGDOC' then
    begin
      S := S + #13 + 'vorliegend: Version ' + Signature;
      S := S + #13 + 'erforderlich: Version ' + string(RggDocSignature);
    end
    else
    begin
      S := S + #13 + 'Die Datei enthält kein gültiges';
      S := S + #13 + 'Rigg - Dokument.';
    end;
    Main.Logger.Info(Format(S, [SCaption]));
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
    Add(Format('A0x=%d', [iP[ooA0, x]]));
    Add(Format('A0y=%d', [iP[ooA0, y]]));
    Add(Format('A0z=%d', [iP[ooA0, z]]));
    Add(Format('B0x=%d', [iP[ooB0, x]]));
    Add(Format('B0y=%d', [iP[ooB0, y]]));
    Add(Format('B0z=%d', [iP[ooB0, z]]));
    Add(Format('C0x=%d', [iP[ooC0, x]]));
    Add(Format('C0y=%d', [iP[ooC0, y]]));
    Add(Format('C0z=%d', [iP[ooC0, z]]));
    Add(Format('D0x=%d', [iP[ooD0, x]]));
    Add(Format('D0y=%d', [iP[ooD0, y]]));
    Add(Format('D0z=%d', [iP[ooD0, z]]));
    Add(Format('E0x=%d', [iP[ooE0, x]]));
    Add(Format('E0y=%d', [iP[ooE0, y]]));
    Add(Format('E0z=%d', [iP[ooE0, z]]));
    Add(Format('F0x=%d', [iP[ooF0, x]]));
    Add(Format('F0y=%d', [iP[ooF0, y]]));
    Add(Format('F0z=%d', [iP[ooF0, z]]));
    Add('');

    Add('[Koordinaten Rigg]');
    Add(Format('Ax=%d', [iP[ooA, x]]));
    Add(Format('Ay=%d', [iP[ooA, y]]));
    Add(Format('Az=%d', [iP[ooA, z]]));
    Add(Format('Bx=%d', [iP[ooB, x]]));
    Add(Format('By=%d', [iP[ooB, y]]));
    Add(Format('Bz=%d', [iP[ooB, z]]));
    Add(Format('Cx=%d', [iP[ooC, x]]));
    Add(Format('Cy=%d', [iP[ooC, y]]));
    Add(Format('Cz=%d', [iP[ooC, z]]));
    Add(Format('Dx=%d', [iP[ooD, x]]));
    Add(Format('Dy=%d', [iP[ooD, y]]));
    Add(Format('Dz=%d', [iP[ooD, z]]));
    Add(Format('Ex=%d', [iP[ooE, x]]));
    Add(Format('Ey=%d', [iP[ooE, y]]));
    Add(Format('Ez=%d', [iP[ooE, z]]));
    Add(Format('Fx=%d', [iP[ooF, x]]));
    Add(Format('Fy=%d', [iP[ooF, y]]));
    Add(Format('Fz=%d', [iP[ooF, z]]));
    Add('');

    Add('[EA]');
    for i := 0 to 19 do
    begin
      S1 := IntToStr(i);
      S2 := Format('%.6g', [rEA[i]]);
      Add(Format('%s=%s', [S1, S2]));
    end;
    Add('');
  end;
end;

procedure TRggDocument.WriteToDFM(Memo: TStrings);
var
  i, tempEI: Integer;
begin
  with Memo do
  begin
    // Rigg - diese Properties werden im Objektinspektor gesetzt
    // Add(Format('SalingTyp=%d',[Ord(SalingTyp)]));
    // Add(Format('ControllerTyp=%d',[Ord(ControllerTyp)]));
    // Add(Format('CalcTyp=%d',[Ord(CalcTyp)]));

    // Trimmtabelle
    with TrimmTabDaten do
    begin
      Add(Format('TabellenTyp= %d', [Ord(TabellenTyp)]));
      Add(Format('Ta0= %10.5f', [a0]));
      Add(Format('Ta1= %10.5f', [a1]));
      Add(Format('Ta2= %10.5f', [a2]));
      Add(Format('Tx0= %10.5f', [x0]));
      Add(Format('Tx1= %10.5f', [x1]));
      Add(Format('Tx2= %10.5f', [x2]));
    end;

    // Mast
    Add(Format('MastL= %d', [FiMastL]));
    Add(Format('Mastunten= %d', [FiMastunten]));
    Add(Format('Mastoben= %d', [FiMastoben]));
    Add(Format('MastfallVorlauf= %d', [FiMastfallVorlauf]));
    tempEI := Round(EI / 1E6);
    Add(Format('EI= %d', [tempEI]));

    // GSB
    Add(Format('%s= %6d %6d %6d', ['Controller', GSB.Controller.Min, GSB.Controller.Ist, GSB.Controller.Max]));
    Add(Format('%s= %6d %6d %6d', ['Winkel', GSB.Winkel.Min, GSB.Winkel.Ist, GSB.Winkel.Max]));
    Add(Format('%s= %6d %6d %6d', ['Vorstag', GSB.Vorstag.Min, GSB.Vorstag.Ist, GSB.Vorstag.Max]));
    Add(Format('%s= %6d %6d %6d', ['Wante', GSB.Wante.Min, GSB.Wante.Ist, GSB.Wante.Max]));
    Add(Format('%s= %6d %6d %6d', ['Woben', GSB.Woben.Min, GSB.Woben.Ist, GSB.Woben.Max]));
    Add(Format('%s= %6d %6d %6d', ['SalingH', GSB.SalingH.Min, GSB.SalingH.Ist, GSB.SalingH.Max]));
    Add(Format('%s= %6d %6d %6d', ['SalingA', GSB.SalingA.Min, GSB.SalingA.Ist, GSB.SalingA.Max]));
    Add(Format('%s= %6d %6d %6d', ['SalingL', GSB.SalingL.Min, GSB.SalingL.Ist, GSB.SalingL.Max]));
    Add(Format('%s= %6d %6d %6d', ['VorstagOS', GSB.VorstagOS.Min, GSB.VorstagOS.Ist, GSB.VorstagOS.Max]));
    Add(Format('%s= %6d %6d %6d', ['WPowerOS', GSB.WPowerOS.Min, GSB.WPowerOS.Ist, GSB.WPowerOS.Max]));

    // Koordinaten
    Add(Format('%s= %6d %6d %6d', ['A0', iP[ooA0, x], iP[ooA0, y], iP[ooA0, z]]));
    Add(Format('%s= %6d %6d %6d', ['B0', iP[ooB0, x], iP[ooB0, y], iP[ooB0, z]]));
    Add(Format('%s= %6d %6d %6d', ['C0', iP[ooC0, x], iP[ooC0, y], iP[ooC0, z]]));
    Add(Format('%s= %6d %6d %6d', ['D0', iP[ooD0, x], iP[ooD0, y], iP[ooD0, z]]));
    Add(Format('%s= %6d %6d %6d', ['E0', iP[ooE0, x], iP[ooE0, y], iP[ooE0, z]]));
    Add(Format('%s= %6d %6d %6d', ['F0', iP[ooF0, x], iP[ooF0, y], iP[ooF0, z]]));
    Add(Format('%s= %6d %6d %6d', ['A', iP[ooA, x], iP[ooA, y], iP[ooA, z]]));
    Add(Format('%s= %6d %6d %6d', ['B', iP[ooB, x], iP[ooB, y], iP[ooB, z]]));
    Add(Format('%s= %6d %6d %6d', ['C', iP[ooC, x], iP[ooC, y], iP[ooC, z]]));
    Add(Format('%s= %6d %6d %6d', ['D', iP[ooD, x], iP[ooD, y], iP[ooD, z]]));
    Add(Format('%s= %6d %6d %6d', ['E', iP[ooE, x], iP[ooE, y], iP[ooE, z]]));
    Add(Format('%s= %6d %6d %6d', ['F', iP[ooF, x], iP[ooF, y], iP[ooF, z]]));

    // EA
    for i := 0 to 19 do
      Add(Format('EA%d= %.6g', [i, rEA[i]]));
  end;
end;

procedure TRggDocument.ReadFromDFM(Memo: TStrings);
  procedure ReadGSB(c: TsbName; S: string);
  var
    S1: string;
    sb: TRggSB;
  begin
    if S = '' then
      Exit;
    sb := GSB.GetSB(c);
    if not Assigned(sb) then
      Exit;
    S := Trim(S);
    S1 := TUtils.StripFirstWord(S);
    sb.Min := StrToInt(S1);
    S := Trim(S);
    S1 := TUtils.StripFirstWord(S);
    sb.Ist := StrToInt(S1);
    sb.Max := StrToInt(S);
  end;
  procedure ReadKoord(k: TRiggPoint; S: string);
  var
    S1: string;
  begin
    if S = '' then
      Exit;
    S := Trim(S);
    S1 := TUtils.StripFirstWord(S);
    iP[k, x] := StrToInt(S1);
    S := Trim(S);
    S1 := TUtils.StripFirstWord(S);
    iP[k, y] := StrToInt(S1);
    iP[k, z] := StrToInt(S);
  end;
  procedure ReadInteger(S: string; var a: Integer);
  begin
    if S = '' then
      Exit;
    a := StrToInt(S);
  end;
  procedure ReadFloat(S: string; var a: double);
  begin
    if S = '' then
      Exit;
    a := StrToFloat(S);
  end;

var
  S: string;
  i, tempEI: Integer;
  T: TTrimmTabDaten;
begin
  with Memo do
  begin
    // Rigg - diese Properties werden im Objektinspektor gesetzt
    // SalingTyp := TSalingTyp(StrToInt(Values['SalingTyp']));
    // ControllerTyp := TControllerTyp(StrToInt(Values['ControllerTyp']));
    // CalcTyp := TCalcTyp(StrToInt(Values['CalcTyp']));

    // Trimmtabelle
    T := DefaultTrimmTabDaten;
    try
      S := Values['TabellenTyp'];
      if S <> '' then
        T.TabellenTyp := TTabellenTyp(StrToInt(S));
      S := Values['Ta0'];
      ReadFloat(S, T.a0);
      S := Values['Ta1'];
      ReadFloat(S, T.a1);
      S := Values['Ta2'];
      ReadFloat(S, T.a2);
      S := Values['Tx0'];
      ReadFloat(S, T.x0);
      S := Values['Tx1'];
      ReadFloat(S, T.x1);
      S := Values['Tx2'];
      ReadFloat(S, T.x2);
    except
      on EConvertError do
      begin
        Main.Logger.Info('DefaultTrimmTabDaten geladen');
        T := DefaultTrimmTabDaten;
      end;
    end;
    TrimmTabDaten := T;

    // Mast
    S := Values['MastL'];
    ReadInteger(S, FiMastL);
    S := Values['Mastunten'];
    ReadInteger(S, FiMastunten);
    S := Values['Mastoben'];
    ReadInteger(S, FiMastoben);
    S := Values['MastfallVorlauf'];
    ReadInteger(S, FiMastfallVorlauf);
    S := Values['EI'];
    ReadInteger(S, tempEI);
    if S <> '' then
      EI := tempEI * 1E6;

    // GSB (Min,Ist,Max)
    S := Values['Controller'];
    ReadGSB(fpController, S);
    S := Values['Winkel'];
    ReadGSB(fpWinkel, S);
    S := Values['Vorstag'];
    ReadGSB(fpVorstag, S);
    S := Values['Wante'];
    ReadGSB(fpWante, S);
    S := Values['Woben'];
    ReadGSB(fpWoben, S);
    S := Values['SalingH'];
    ReadGSB(fpSalingH, S);
    S := Values['SalingA'];
    ReadGSB(fpSalingA, S);
    S := Values['SalingL'];
    ReadGSB(fpSalingL, S);
    S := Values['VorstagOS'];
    ReadGSB(fpVorstagOS, S);
    S := Values['WPowerOS'];
    ReadGSB(fpWPowerOS, S);

    // Koordinaten (x,y,z)
    S := Values['A0'];
    ReadKoord(ooA0, S);
    S := Values['B0'];
    ReadKoord(ooB0, S);
    S := Values['C0'];
    ReadKoord(ooC0, S);
    S := Values['D0'];
    ReadKoord(ooD0, S);
    S := Values['E0'];
    ReadKoord(ooE0, S);
    S := Values['F0'];
    ReadKoord(ooF0, S);
    S := Values['A'];
    ReadKoord(ooA, S);
    S := Values['B'];
    ReadKoord(ooB, S);
    S := Values['C'];
    ReadKoord(ooC, S);
    S := Values['D'];
    ReadKoord(ooD, S);
    S := Values['E'];
    ReadKoord(ooE, S);
    S := Values['F'];
    ReadKoord(ooF, S);

    // EA
    for i := 0 to 19 do
    begin
      S := Values[Format('EA%d', [i])];
      if S <> '' then
        rEA[i] := StrToFloat(S);
    end;
  end;
end;

 procedure TRggDocument.LoadFromString(s: string);
// var
// Decoder: TIdDecoderMime;
// sDecoded: string;
// Stream: TStringStream;
 begin
// Decoder := TIdDecoderMime.Create(nil);
// sDecoded := Decoder.DecodeString(s);;
// Stream := TStringStream.Create(sDecoded);
// Stream.Seek(0, soFromBeginning);
// LoadFromStream(Stream);
// Stream.Free;
// Decoder.Free;
 end;

// procedure TRggDocument.LoadFromStream(s: string);
// var
// Stream: TStream;
// begin
// Stream := TMemoryStream.Create;
// Decoder := TIdDecoderMime.Create(nil);
// Decoder.DecodeToStream(s, Stream);
// Stream.Seek(0, soFromBeginning);
// LoadFromStream(Stream);
// Stream.Free;
// Decoder.Free;
// end;

function TRggDocument.SaveToString: string;
// var
// Encoder: TIdEncoderMime;
// Stream: TStream;
begin
// Stream := TMemoryStream.Create;
// Encoder := TIdEncoderMime.Create(nil);
// SaveToStream(Stream);
// Stream.Seek(0, soFromBeginning);
// result := Encoder.Encode(Stream, Stream.Size);
// Stream.Free;
// Encoder.Free;
end;

function TRggDocument.SaveToXMLBase64: string;
// var
//   Encoder: TIdEncoderMime;
begin
//   Encoder := TIdEncoderMime.Create(nil);
//   result := Encoder.Encode(SaveToXML);
//   Encoder.Free;
end;

function TRggDocument.LoadFromXMLBase64(s: string): string;
// var
//   Decoder: TIdDecoderMime;
begin
//   Decoder := TIdDecoderMime.Create(nil);
//   result := Decoder.DecodeString(s);
//   Decoder.Free;
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
  tempEI: double;
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
      c.SetAttribute('x', IntToStr(Round(iP[rp][x])));
      c.SetAttribute('y', IntToStr(Round(iP[rp][y])));
      c.SetAttribute('z', IntToStr(Round(iP[rp][z])));
    end;

    b := a.AddChild('Rumpf');
    for rp := ooA0 to ooP0 do
    begin
      c := b.AddChild('Koord');
      c.SetAttribute('ID', KoordTexteXML[rp]);
      c.SetAttribute('Label', XMLKoordLabels[rp]);
      c.SetAttribute('x', IntToStr(Round(iP[rp][x])));
      c.SetAttribute('y', IntToStr(Round(iP[rp][y])));
      c.SetAttribute('z', IntToStr(Round(iP[rp][z])));
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
        c.SetAttribute('Value', Format('%.6g', [rEA[i]]));
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
