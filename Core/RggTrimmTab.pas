unit RggTrimmTab;

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
  System.Types,
  System.Inifiles,
  System.Math,
  System.UIConsts,
  RggVector,
  RggTypes;

const
  PunkteMax = 20; { maximale Anzahl Punkte im MemoScript }
  { Konstanten für Bezierkurve }
  BezierKurveVomGrad = 2; { quadratische Bezierkurve, 3 Control Points }
  AnzahlKurvenPunkte = 100; { AnzahlKurvenPunkte + 1 KurvenPunkte }

type
  { for TTabellenTyp, see RggTypes }
  { TTabellenTyp = (itKonstante, itGerade, itParabel, itBezier); }

  TBezier = class;

  TTrimmTabKurve = array [1 .. PunkteMax] of TPoint;

  TTrimmTabGraphModel = class
  public
    TabellenTyp: TTabellenTyp;

    x1, y1, x2, y2: Integer;

    EndwertWeg: Integer;
    EndwertKraft: Integer;

    PunkteAnzahl: Integer;
    Kurve: TTrimmTabKurve; { Punkt 0 ist der NullPunkt }

    LineDataX: TLineDataR100;
    LineDataY: TLineDataR100;
  end;

  ITrimmTab = interface
  ['{B3EE7E6D-ED13-4F90-A4F9-ABFFD56D0A95}']

    function EvalY(x: double): double;
    function EvalX(y: double): double;

    function GetEvalDirection: Boolean;
    procedure SetEvalDirection(const Value: Boolean);

    function GetTabellenTyp: TTabellenTyp;
    procedure SetTabellenTyp(Value: TTabellenTyp);

    function GetEndwertKraft: Integer;
    procedure SetEndwertKraft(Value: Integer);

    function GetEndwertWeg: Integer;
    procedure SetEndwertWeg(Value: Integer);

    function GetMittelPunkt: TPoint;
    procedure SetMittelPunkt(Value: TPoint);

    function GetTrimmTabDaten: TTrimmTabDaten;
    procedure SetTrimmTabDaten(Value: TTrimmTabDaten);

    procedure GetMemoLines(ML: TStrings);
    procedure ProcessTrimmTab(ML: TStrings);

    procedure UpdateGraphModel(Model: TTrimmTabGraphModel);

    procedure LoadFromIniFile(IniFile: TIniFile);
    procedure WriteToIniFile(IniFile: TIniFile);

    procedure LoadFromStream(S: TStream);
    procedure SaveToStream(S: TStream);

    property EvalDirection: Boolean read GetEvalDirection write SetEvalDirection;
    property MittelPunkt: TPoint read GetMittelPunkt write SetMittelPunkt;
    property TabellenTyp: TTabellenTyp read GetTabellenTyp write SetTabellenTyp;
    property EndwertKraft: Integer read GetEndwertKraft write SetEndwertKraft;
    property EndwertWeg: Integer read GetEndwertKraft write SetEndwertWeg;
    property TrimmtabDaten: TTrimmTabDaten read GetTrimmTabDaten write SetTrimmTabDaten;
  end;

  TTrimmTab = class(TInterfacedObject, ITrimmTab)
  private
    FTabellenTyp: TTabellenTyp;
    FValid: Boolean;
    Fry1: double; { immer y2/2 im Fall itParabel }
    FEvalDirection: Boolean;
    function GetEndwertKraft: Integer;
    function GetEndwertWeg: Integer;
    procedure SetEvalDirection(const Value: Boolean);
    function GetEvalDirection: Boolean;
  protected
    a1, a2: double;
    procedure SetTabellenTyp(Value: TTabellenTyp);
    function GetTabellenTyp: TTabellenTyp;
    procedure SetMittelPunkt(Value: TPoint); virtual;
    function GetMittelPunkt: TPoint; virtual;
    procedure SetEndPunkt(Value: TPoint);
    function GetEndPunkt: TPoint;
    procedure SetEndwertKraft(Value: Integer); virtual;
    procedure SetEndwertWeg(Value: Integer); virtual;
    procedure SetTrimmTabDaten(Value: TTrimmTabDaten); virtual;
    function GetTrimmTabDaten: TTrimmTabDaten; virtual;
  public
    FScale: single;
    Kurve: TTrimmTabKurve; { Punkt 0 ist der NullPunkt }
    PunkteAnzahl: Integer; { tatsächliche Anzahl Punkte entsprechend Memo }
    EndKraftMin, EndWegMin, KraftMax, WegMax: Integer;
    Bezier: TBezier;
    x1, y1, x2, y2: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure GetPolynom; virtual;

    procedure LoadFromIniFile(IniFile: TIniFile);
    procedure WriteToIniFile(IniFile: TIniFile);
    procedure LoadFromStream(S: TStream);
    procedure SaveToStream(S: TStream);
    procedure GetMemoLines(ML: TStrings);
    procedure UpdateGraphModel(Model: TTrimmTabGraphModel);
    procedure ProcessTrimmTab(ML: TStrings);
    function EvalY(x: double): double; virtual;
    function EvalX(y: double): double; virtual;

    property EvalDirection: Boolean read GetEvalDirection write SetEvalDirection;
    property TabellenTyp: TTabellenTyp read GetTabellenTyp write SetTabellenTyp;
    property Valid: Boolean read FValid write FValid;
    property MittelPunkt: TPoint read GetMittelPunkt write SetMittelPunkt;
    property EndPunkt: TPoint read GetEndPunkt write SetEndPunkt;
    property EndwertKraft: Integer read GetEndwertKraft write SetEndwertKraft;
    property EndwertWeg: Integer read GetEndwertWeg write SetEndwertWeg;
    property TrimmtabDaten: TTrimmTabDaten read GetTrimmTabDaten write SetTrimmTabDaten;
  end;

  TControlPunkte = array [1 .. BezierKurveVomGrad + 1] of vec3;
  TBezierKurve = array [1 .. AnzahlKurvenPunkte + 1] of vec3;
  TKoeffizientenArray = array [1 .. BezierKurveVomGrad + 1] of Integer;

  TBezier = class
  private
    c: TKoeffizientenArray; { n+1 }
    n: Integer; { there are n+1 Control Points }
    m: Integer; { there are m+1 points along the interval of 0 <= u <= 1 }
    function BlendingValue(u: double; k: Integer): double;
    procedure ComputePoint(u: double; out pt: vec3);
  public
    Curve: TBezierKurve; { m+1 }
    Controls: TControlPunkte; { n+1 }
    constructor Create;
    procedure ComputeCoefficients;
    procedure GenerateCurve;
  end;

implementation

{
  y-Achse: Weg in mm
  x-Achse: Kraft in N
  Die Kurve muß für 0 < y < y2 vollständig im 1.Quadranten liegen.
  TabellenTyp bzw. KurveTyp:
  itGerade: Gerade durch NullPunkt(0,0) und EndPunkt(x2,y2).
  itParabel: quadratisches Interpolationspolymom durch drei gegebene Punkte.
  1. Nullpunkt als Anfangspunkt der Kurve (0,0)
  2. Endpunkt der Kurve (x2,y2)
  3. Punkt in der Mitte, der die Krümmung bestimmt. (x1,y1)
  Für diesen Punkt wird nur der Kraftwert x1 eingegeben.
  Der Weg Fry1 ist immer y2/2.
  itBezier: Kurve durch Nullpunkt und Endpunkt. Die Tangente an die Kurve in
  diesen Punkten geht durch den Kontrollpunkt (x1,y1).
}

{ TTrimmTab }

constructor TTrimmTab.Create;
begin
  FScale := 1.0;
  Bezier := TBezier.Create;
  EndKraftMin := 100;
  KraftMax := 3000; { in N }
  EndWegMin := 10;
  WegMax := 300; { in mm }
  x2 := 1000; { EndwertKraft in N }
  y2 := 100; { EndwertWeg in mm }
  x1 := x2 div 2;
  y1 := y2 div 2;
  TabellenTyp := itGerade; { --> SetTabellenTyp --> GetPolynom }
  TrimmtabDaten := DefaultTrimmTabDaten; // war vorher auskommentiert
end;

destructor TTrimmTab.Destroy;
begin
  Bezier.Free;
  inherited Destroy;
end;

procedure TTrimmTab.SetTabellenTyp(Value: TTabellenTyp);
begin
  // if Value = FTabellenTyp then Exit;
  FTabellenTyp := Value;
  case TabellenTyp of
    itKonstante:
      begin
      end;
    itGerade:
      begin
        x1 := x2 div 2;
        y1 := y2 div 2;
      end;
    itParabel:
      begin
        Fry1 := y2 / 2;
        y1 := y2 div 2;
      end;
    itBezier:
      begin
      end;
  end;
  MittelPunkt := MittelPunkt;
  GetPolynom;
end;

function TTrimmTab.GetTabellenTyp: TTabellenTyp;
begin
  result := FTabellenTyp;
end;

function TTrimmTab.GetTrimmTabDaten: TTrimmTabDaten;
begin
  if not Valid then
    GetPolynom;
  result.TabellenTyp := TabellenTyp;
  result.a0 := 0;
  result.x0 := 0;
  case TabellenTyp of
    itKonstante:
      begin
        result.a1 := x1;
        result.a2 := 0;
        result.x1 := y2;
        result.x2 := x2;
      end;
    itGerade:
      begin
        result.a1 := y2 / x2;
        result.a2 := 0;
        result.x1 := 0;
        result.x2 := x2;
      end;
    itParabel:
      begin
        result.a1 := a1;
        result.a2 := a2;
        result.x1 := x1;
        result.x2 := x2;
      end;
    itBezier:
      begin
        result.a1 := y1; { ControllPoint }
        result.a2 := x1; { ControllPoint }
        result.x1 := y2; { EndPunkt }
        result.x2 := x2; { EndPunkt }
      end;
  end;
end;

procedure TTrimmTab.SetTrimmTabDaten(Value: TTrimmTabDaten);
begin
  FTabellenTyp := Value.TabellenTyp; // SetTabellenTyp() hier nicht aufrufen
  Valid := True; // Aufruf von GetPolynom in EvalY() unterbinden
  a1 := Value.a1;
  a2 := Value.a2;
  case Value.TabellenTyp of
    itKonstante:
      begin
        x1 := Round(Value.a1);
        y2 := Round(Value.x1);
        x2 := Round(Value.x2);
      end;
    itGerade:
      begin
        x2 := Round(Value.x2);
        y2 := Round(EvalY(x2));
      end;
    itParabel:
      begin
        x1 := Round(Value.x1);
        x2 := Round(Value.x2);
        y2 := Round(EvalY(x2));
        y1 := y2 div 2;
        Fry1 := y2 / 2;
      end;
    itBezier:
      begin
        y1 := Round(Value.a1); { ControllPoint }
        x1 := Round(Value.a2); { ControllPoint }
        y2 := Round(Value.x1); { EndPoint }
        x2 := Round(Value.x2); { EndPoint }
        with Bezier do
        begin
          Controls[2].x := x1;
          Controls[2].y := y1;
          Controls[3].x := x2;
          Controls[3].y := y2;
        end;
      end;
  end;
  TabellenTyp := Value.TabellenTyp; // SetTabellenTyp() aufrufen
end;

procedure TTrimmTab.WriteToIniFile(IniFile: TIniFile);
var
  S, S1: string;
begin
  with IniFile do
  begin
    S := 'Trimmtabelle';
    WriteInteger(S, 'TabellenTyp', Ord(FTabellenTyp));
    with TrimmtabDaten do
    begin
      S1 := Format('%f', [a1]);
      WriteString(S, 'a1', S1);
      S1 := Format('%f', [a2]);
      WriteString(S, 'a2', S1);
      S1 := Format('%f', [x1]);
      WriteString(S, 'x1', S1);
      S1 := Format('%f', [x2]);
      WriteString(S, 'x2', S1);
    end;
  end;
end;

procedure TTrimmTab.SaveToStream(S: TStream);
var
  T: TTrimmTabDaten;
begin
  T := TrimmtabDaten;
  S.WriteBuffer(T, SizeOf(TTrimmTabDaten));
end;

procedure TTrimmTab.LoadFromIniFile(IniFile: TIniFile);
var
  S, S1: string;
  T: TTrimmTabDaten;
begin
  T := TrimmtabDaten; { T mit den aktuellen Werten als Default initialisieren }
  with IniFile do
  begin
    S := 'Trimmtabelle';
    try
      T.TabellenTyp := TTabellenTyp(ReadInteger(S, 'TabellenTyp', Ord(FTabellenTyp)));
      S1 := ReadString(S, 'a1', FloatToStrF(T.a1, ffGeneral, 8, 2));
      T.a1 := StrToFloat(S1);
      S1 := ReadString(S, 'a2', FloatToStrF(T.a2, ffGeneral, 8, 2));
      T.a2 := StrToFloat(S1);
      S1 := ReadString(S, 'x1', FloatToStrF(T.x1, ffGeneral, 8, 2));
      T.x1 := StrToFloat(S1);
      S1 := ReadString(S, 'x2', FloatToStrF(T.x2, ffGeneral, 8, 2));
      T.x2 := StrToFloat(S1);
    except
      on EConvertError do
        T := DefaultTrimmTabDaten;
    end;
  end;
  TrimmtabDaten := T;
end;

procedure TTrimmTab.LoadFromStream(S: TStream);
var
  T: TTrimmTabDaten;
begin
  T := DefaultTrimmTabDaten;
  try
    S.ReadBuffer(T, SizeOf(TTrimmTabDaten));
  except
    on EStreamError do
      T := DefaultTrimmTabDaten;
  end;
  TrimmtabDaten := T;
end;

procedure TTrimmTab.GetMemoLines(ML: TStrings);
begin
  with ML do
  begin
    Clear;
    Add('[X/mm = Y/N]');
    case TabellenTyp of
      itKonstante:
        begin
          Add(Format('%d=%d', [Round(y2 * 0.2), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.4), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.6), x1]));
          Add(Format('%d=%d', [Round(y2 * 0.8), x1]));
          Add(Format('%d=%d', [y2, x2]));
        end;
      itGerade, itParabel, itBezier:
        begin
          if EvalDirection then
          begin
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.2)), Round(x2 * 0.2)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.4)), Round(x2 * 0.4)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.6)), Round(x2 * 0.6)]));
            Add(Format('%d=%d', [Round(EvalY(x2 * 0.8)), Round(x2 * 0.8)]));
            Add(Format('%d=%d', [y2, x2]));
          end
          else
          begin
            Add(Format('%d=%d', [Round(y2 * 0.2), Round(EvalX(y2 * 0.2))]));
            Add(Format('%d=%d', [Round(y2 * 0.4), Round(EvalX(y2 * 0.4))]));
            Add(Format('%d=%d', [Round(y2 * 0.6), Round(EvalX(y2 * 0.6))]));
            Add(Format('%d=%d', [Round(y2 * 0.8), Round(EvalX(y2 * 0.8))]));
            Add(Format('%d=%d', [y2, x2]));
          end;
        end;
    end;
  end;
end;

function TTrimmTab.GetEndPunkt: TPoint;
begin
  result := Point(x2, y2);
end;

function TTrimmTab.GetEndwertKraft: Integer;
begin
  result := x2;
end;

function TTrimmTab.GetEndwertWeg: Integer;
begin
  result := y2;
end;

function TTrimmTab.GetEvalDirection: Boolean;
begin
  result := FEvalDirection;
end;

procedure TTrimmTab.SetEndPunkt(Value: TPoint);
begin
  Valid := False;
  EndwertKraft := Value.x;
  EndwertWeg := Value.y;
end;

procedure TTrimmTab.SetEndwertKraft(Value: Integer);
begin
  Valid := False;
  x2 := Value;
  if x2 < EndKraftMin then
    x2 := EndKraftMin;
  if x2 > KraftMax then
    x2 := KraftMax;
  case TabellenTyp of
    itKonstante:
      begin
      end;
    itGerade:
      begin
      end;
    itParabel:
      begin
      end;
    itBezier:
      begin
        Bezier.Controls[3].x := x2;
      end;
  end;
end;

procedure TTrimmTab.SetEndwertWeg(Value: Integer);
begin
  Valid := False;
  y2 := Value;
  if y2 < EndWegMin then
    y2 := EndWegMin;
  if y2 > WegMax then
    y2 := WegMax;
  case TabellenTyp of
    itGerade:
      begin
      end;
    itParabel:
      begin
        Fry1 := y2 / 2;
        y1 := y2 div 2;
      end;
    itBezier:
      begin
        Bezier.Controls[3].y := y2;
      end;
  end;
end;

procedure TTrimmTab.SetEvalDirection(const Value: Boolean);
begin
  FEvalDirection := Value;
end;

procedure TTrimmTab.SetMittelPunkt(Value: TPoint);
var
  rTemp, min, max: double;
  iTemp: Integer;
  Wurzel2Halbe: double;
begin
  Valid := False;
  x1 := Value.x;
  y1 := Value.y;
  if x1 < 0 then
    x1 := 0; { ist garantiert }
  if y1 < 0 then
    y1 := 0; { ist garantiert }
  if x1 > x2 then
    x1 := x2;
  if y1 > y2 then
    y1 := y2;
  case TabellenTyp of
    itKonstante:
      begin
        y1 := y2 div 2; { Value.y wird ignoriert indem auf Standard gesetzt }
      end;
    itGerade:
      begin
        x1 := x2 div 2; { Value wird ignoriert }
        y1 := y2 div 2;
      end;
    itParabel:
      begin
        iTemp := Value.x;
        rTemp := iTemp;
        Wurzel2Halbe := sqrt(2) / 2;
        max := x2 * Wurzel2Halbe; { max := x2 * 4 div 5 }
        min := x2 - max; { min := x2 div 5 }
        if rTemp < min then
          iTemp := Ceil(min)
        else if rTemp > max then
          iTemp := Floor(max);
        x1 := iTemp;
        y1 := y2 div 2; { Value.y wird ignoriert }
      end;
    itBezier:
      begin
        Bezier.Controls[2].x := x1;
        Bezier.Controls[2].y := y1;
      end;
  end;
end;

function TTrimmTab.GetMittelPunkt: TPoint;
begin
  result := Point(x1, y1);
  (*
    case TabellenTyp of
    itKonstante: result := Point(x1, 0);
    itGerade: result := Point(x1, y1);
    itParabel: result := Point(x1, Round(Fry1));
    itBezier:
      result := Point(
        Round(Bezier.Controls[2].x),
        Round(Bezier.Controls[2].y)
      );
    end;
  *)
end;

procedure TTrimmTab.GetPolynom;
begin
  case TabellenTyp of
    itKonstante:
      begin
        a1 := x1;
      end;
    itGerade:
      begin
        a1 := y2 / x2;
      end;
    itParabel:
      begin
        a1 := Fry1 / x1;
        a2 := ( (y2 - Fry1)/(x2 - x1) - a1 ) / (x2);
      end;
    itBezier:
      begin
        with Bezier do
        begin
          Controls[1].x := 0;
          Controls[1].y := 0;
          Controls[1].z := 0;
          Controls[2].x := x1;
          Controls[2].y := y1;
          Controls[2].z := 0;
          Controls[3].x := x2;
          Controls[3].y := y2;
          Controls[3].z := 0;
          ComputeCoefficients;
        end;
      end;
  end;
  Valid := True;
end;

{ liefert den Weg y in mm als Funktion der Kraft x in N zurück }
function TTrimmTab.EvalY(x: double): double;
var
  KraftSoll, KraftIst, Diff: double; { Kräfte in N }
  uA, uB, uIst: double; { Parameter u: 0 <= u <= 1 }
  Zaehler: Integer;
  Temp: vec3;
begin
  result := 0;

  if not Valid then
    GetPolynom;
  { Maximalwert des Weges begrenzen auf das WegEnde y2 }
  if x > x2 then
  begin
    result := y2;
    Exit;
  end;
  if x < 0 then
  begin
    result := 0;
    Exit;
  end;

  case TabellenTyp of
    itKonstante:
      begin
        result := 0; { result ist undefiniert - ev. Exception auslösen }
      end;
    itGerade:
      begin
        result := a1 * x;
      end;
    itParabel:
      begin
      { result := a1 * (x-x0) + a2 * (x-x0) * (x-x1); }
        result := a1 * x + a2 * x * (x - x1);
      end;
    itBezier:
      begin
        KraftSoll := x; { nur der Lesbarkeit halber }
        uA := 0;
        uB := 1;
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          uIst := (uA + uB) / 2;
          Bezier.ComputePoint(uIst, Temp);
          KraftIst := Temp.x;
          Diff := KraftIst - KraftSoll;
          if Diff < 0 then
            uA := uIst
          else
            uB := uIst;
        until
          (abs(Diff) < 0.1) or (Zaehler = 100);
        if Zaehler < 100 then
          result := Temp.y
        else
          result := y2;
      end;
  end;
end;

{ liefert die Kraft x in N als Funktion des Weges y in mm zurück }
{ bzw. liefert Wantenspannung 3D in Abhängigkeit von der Auslenkung des Vorstags }
function TTrimmTab.EvalX(y: double): double;
var
  WegSoll, WegIst, Diff: double; { Wege in mm }
  KraftA, KraftB, KraftIst: double; { Kräfte in N }
  uA, uB, uIst: double;
  Zaehler: Integer;
  Temp: vec3;
begin
  result := 0;

  if not Valid then
    GetPolynom;
  { Maximalwert der Kraft begrenzen auf das KraftEnde x2 }
  if y > y2 then
  begin
    result := x2;
    Exit;
  end;
  if y < 0 then
  begin
    result := 0;
    Exit;
  end;

  case TabellenTyp of
    itKonstante:
      begin
        result := x1;
      end;
    itGerade:
      begin
        result := 1 / a1 * y;
      end;

    itParabel:
      begin
        { Umkehrfunktion zu y = a1*(x-x0) + a2*(x-x0)*(x-x1); }
        { Normalfall: Kraft zwischen Null und KraftEnde }
        WegSoll := y; { nur der Lesbarkeit halber }
        KraftA := 0; { KraftA := KraftAnfang; }
        KraftB := x2; { KraftB := KraftEnde; }
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          KraftIst := (KraftA + KraftB) / 2;
          WegIst := a1 * KraftIst + a2 * KraftIst * (KraftIst - x1);
          Diff := WegIst - WegSoll;
          if Diff < 0 then
            KraftA := KraftIst
          else
            KraftB := KraftIst;
        until
          (abs(Diff) < 0.01) or (Zaehler = 100);
        result := KraftIst;
      end;

    itBezier:
      begin
        WegSoll := y; { nur der Lesbarkeit halber }
        uA := 0;
        uB := 1;
        Zaehler := 0;
        repeat
          Zaehler := Zaehler + 1;
          uIst := (uA + uB) / 2;
          Bezier.ComputePoint(uIst, Temp);
          WegIst := Temp.y;
          Diff := WegIst - WegSoll;
          if Diff < 0 then
            uA := uIst
          else
            uB := uIst;
        until
          (abs(Diff) < 0.01) or (Zaehler = 100);
        if Zaehler < 100 then
          result := Temp.x
        else
          result := x2;
      end;
  end;
end;

procedure TTrimmTab.ProcessTrimmTab(ML: TStrings);
var
  i, Code1, Code2: Integer;
  Punkt: TPoint;
  S, S1, S2: string;
begin
  Valid := False;
  PunkteAnzahl := 0;
  { Achtung: Endweg wird nicht richtig erfaßt, wenn EndKraftMin zu klein ist! }
  EndPunkt := Point(EndKraftMin, EndWegMin);
  { später Nebenwirkung über Eigenschaft }
  for i := 0 to ML.Count - 1 do
  begin
    S := ML[i];

    if S = '' then
    begin
      Continue; { Daher niemals Zugriff auf S[1] }
    end;

    { '*' steht für ungültigen Eintrag }
    if Pos('*', S) <> 0 then
    begin
      Continue; { vermeidet Zugriff auf S[1] }
    end;

    { Zugriff auf S[1] verursacht Exception, wenn String leer. }
    { if S[1] = '*' then
        Continue;
    }

    { String ohne '=' überspringen }
    if Pos('=', S) = 0 then
    begin
      ML[i] := Concat('***', S);
      Continue;
    end;

    { Negative Zahlen nicht erlaubt }
    if Pos('-', S) <> 0 then
    begin
      ML[i] := Concat('***', S);
      Continue;
    end;

    S1 := ML.Names[i]; { String vor dem '=' }
    S2 := ML.Values[S1]; { String nach dem '=' }
    Val(Trim(S1), Punkt.y, Code1); { Weg y }
    Val(Trim(S2), Punkt.x, Code2); { Kraft x }

    { Fehler bei der Umwandlung in Integerwert? }
    if (Code1 <> 0) or (Code2 <> 0) then
    begin
      ML[i] := Concat('***', S);
      Continue;
    end;

    if PunkteAnzahl < PunkteMax then
    begin
      Inc(PunkteAnzahl);
      Kurve[PunkteAnzahl] := Punkt;
      if Punkt.x >= x2 then
        EndPunkt := Punkt;
    end;
  end;

  { Mittelpunkt auf Gültigkeit kontrollieren }
  MittelPunkt := MittelPunkt;
end;

procedure TTrimmTab.UpdateGraphModel(Model: TTrimmTabGraphModel);
var
  i: Integer;
begin
  if not Valid then
    GetPolynom;
  if TabellenTyp = itBezier then
    Bezier.GenerateCurve;

  Model.TabellenTyp := TabellenTyp;

  Model.x1 := x1;
  Model.y1 := y1;

  Model.x2 := x2;
  Model.y2 := y2;

  Model.EndwertWeg := EndwertWeg;
  Model.EndwertKraft := EndwertKraft;

  Model.Kurve := Kurve;
  Model.PunkteAnzahl := PunkteAnzahl;

  { LineData }
  case TabellenTyp of
    itParabel, itBezier:
      begin
        for i := 0 to 100 do
        begin
          case TabellenTyp of
            itParabel:
              begin
                { Weg }
                Model.LineDataX[i] := EvalY(i / 100 * EndwertKraft) / (EndwertWeg);
                { Kraft als Argument auf y-Achse }
                Model.LineDataY[i] := i / 100;
              end;
            itBezier:
              begin
                { Weg }
                Model.LineDataX[i] := Bezier.Curve[i + 1].y / EndwertWeg;
                { Kraft }
                Model.LineDataY[i] := Bezier.Curve[i + 1].x / EndwertKraft;
              end;
          end;
        end;
      end;
  end;

end;

{ TBezier }

constructor TBezier.Create;
begin
  { there are n+1 Control Points }
  n := BezierKurveVomGrad;

  { there are m+1 points along the interval of 0<= u <= 1 }
  m := AnzahlKurvenPunkte;
end;

procedure TBezier.ComputeCoefficients;
var
  j, k: Integer;
begin
  for k := 0 to n do
  begin
    { compute n!/(k!(n-k)!) }
    c[k + 1] := 1;
    for j := n downto k + 1 do
      c[k + 1] := c[k + 1] * j;
    for j := n - k downto 2 do
      c[k + 1] := c[k + 1] div j;
  end;
end;

function TBezier.BlendingValue(u: double; k: Integer): double;
var
  j: Integer;
  bv: double;
begin
  { compute c[k] * (u to kth power) * ((1-u) to (n-k) power) }
  bv := c[k];
  for j := 1 to k - 1 do
    bv := bv * u;
  for j := 1 to n - k + 1 do
    bv := bv * (1 - u);
  result := bv;
end;

procedure TBezier.ComputePoint(u: double; out pt: vec3);
var
  k: Integer;
  b: double;
begin
 { pt := Null; }
  pt.x := 0.0;
  pt.y := 0.0;
  pt.z := 0.0;
  for k := 1 to n + 1 do
  begin
    { add in influence of each control point }
    b := BlendingValue(u, k);
    pt.x := pt.x + Controls[k].x * b;
    pt.y := pt.y + Controls[k].y * b;
    pt.z := pt.z + Controls[k].z * b;
  end;
end;

procedure TBezier.GenerateCurve;
{ Uses n+1 control points.
  Generates curve by  finding m+1 points along the interval of 0 <= u <= 1 }
var
  k: Integer;
begin
  ComputeCoefficients;
  for k := 0 to m do
    ComputePoint(k / m, Curve[k + 1]);
end;

end.
