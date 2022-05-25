unit RiggVar.FB.Equation;

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
  RiggVar.FB.Formula;

type
  TFederEquation = class(TFederFormulaBase)
  private
    function GetSampleInfo: string;
  protected
    function Kraft(k, l, l0: single): single;
    function Force(k, l, l0: single; m: Integer): single;
    function Diff1(i: Integer; x, y: single): single; virtual;
    function Diff2(i: Integer; x, y: single): single; virtual;
  public
    FaktorEQ: Integer;

    ErrorCounter: Integer;
    HasError: Boolean;
    SourceFormat: Integer;

    MaxPlotFigure: Integer;

    a1, a2, a3, a4: single;
    b1, b2, b3, b4: single;
    t1, t2, t3, t4: single;
    f1, f2, f3, f4: single;

    m1, m2, m3, m4: Integer;
    x1, x2, x3, x4: single;
    y1, y2, y3, y4: single;
    z1, z2, z3, z4: single;
    l1, l2, l3, l4: single;
    k1, k2, k3, k4: single;
    iv, iw, jv, jw: single;

    d: single;
    d1, d2, d3, d4: single;
    fx, fy: single;

    y3f: single;
    l3f: single;
    Lf: single;

    fcap: single;
    pcap: Integer;
    mcap: Integer;

    Gain: Integer;
    Limit: Integer;
    Figure: Integer;
    Plot: Integer;
    Dim: Integer;

    PlusCap: Boolean;
    MinusCap: Boolean;

    Vorzeichen: Boolean;
    SolutionMode: Boolean;
    LinearForce: Boolean;
    ForceMode: Integer;
    SliceMode: Integer;
    DiffMode: Integer;
    PlotFigure: Integer;
    WantDiff: Boolean;
    WantDiffOnce: Boolean;

    OffsetZ: Integer;

    Hub: Integer;
    Sample: Integer;

    constructor Create; override;

    function GetValue(x, y : single): single; override;
    procedure PrepareCalc; override;

    procedure InitFormula;
    procedure InitRandom(ID: Integer);
    function CalcFederFormula(x, y: single): single;

    procedure InitKoord; virtual;
    procedure InitFaktorEQ;
    procedure InitMaxPlotFigure;
    procedure InitMemo(ML: TStrings); virtual;
    procedure CalcBF(x, y: single); virtual;
    function CalcRaw(x, y : single): single; virtual;
    function CalcValue(x, y : single; af: Integer): single; virtual;
    function GetStatusLine: string; virtual;
    property SampleInfo: string read GetSampleInfo;
  end;

resourcestring
  StrBetrag = 'Amount';
  StrRichtung = 'Direction';
  StrEnergie = 'Energy';
  StrBerechneWinkel = 'CalculateAngle';
  StrKraft = 'Force';
  StrVorzeichen = 'Sign';
  StrGleich = 'Equal';

  StrPlusCap = 'PlusCap';
  StrMinusCap = 'MinusCap';
  StrBigmap = 'Bigmap';
  StrOpacity = 'Opacity';
  StrOpenMesh = 'OpenMesh';
  StrPolarMesh = 'PolarMesh';
  StrLinearMesh = 'LinearMesh';
  StrFuzzyMesh = 'FuzzyMesh';
  StrSolutionMode = 'SolutionMode';

implementation

{ TFederEquation }

procedure TFederEquation.CalcBF(x, y: single);
begin
  a1 := sqr(x-x1) + sqr(y-y1);
  a2 := sqr(x-x2) + sqr(y-y2);
  a3 := sqr(x-x3) + sqr(y-y3);

  t1 := sqrt(a1);
  t2 := sqrt(a2);
  t3 := sqrt(a3);

  f1 := k1 * (t1 - l1);
  f2 := k2 * (t2 - l2);
  f3 := k3 * (t3 - l3);
end;

function TFederEquation.CalcValue(x, y: single; af: Integer): single;
begin
  result := 0;
end;

constructor TFederEquation.Create;
begin
  inherited Create;

  DiffMode := 2;
  Randomize;
  PlotFigure := 1;
  iv := 30;
  iw := 5;
  jv := 150;
  jw := 5;
  InitKoord;
end;

procedure TFederEquation.InitFormula;
begin
  x1 := 65;
  x2 := -65;
  x3 := 0;

  y1 := 65;
  y2 := 65;
  y3 := -65;

  z1 := 0;
  z2 := 0;
  z3 := 0;

  l1 := 90;
  l2 := 90;
  l3 := 90;

  k1 := 1;
  k2 := 1;
  k3 := 1;

  m1 := 0;
  m2 := 0;
  m3 := 0;

  ForceMode := 0;
end;

procedure TFederEquation.InitRandom(ID: Integer);
var
  lT: single;
  lL: single;
  lZ: single;
begin
  lT := - 5 + Random(10);

  x1 := 65 + lT;
  x2 := -65 + lT;
  x3 := 0;
  y1 := 65 + lT;
  y2 := 65 + lT;
  y3 := -65 + lT;

  lZ := - 5 + Random(10);
  z1 := lZ;
  z2 := lZ;
  z3 := lZ;

  lL := - 10 + Random(20);
  l1 := 90 + lL;
  l2 := 90 + lL;
  l3 := 90 + lL;

  k1 := 1;
  k2 := 1;
  k3 := 1;

  m1 := 0;
  m2 := 0;
  m3 := 0;

  ForceMode := 0;
end;

function TFederEquation.CalcFederFormula(x, y: single): single;
begin
  a1 := sqr(x-x1) + sqr(y-y1);
  a2 := sqr(x-x2) + sqr(y-y2);
  a3 := sqr(x-x3) + sqr(y-y3);

  t1 := sqrt(a1 + sqr(z1));
  t2 := sqrt(a2 + sqr(z2));
  t3 := sqrt(a3 + sqr(z3));

  b1 := t2 * t3 * (t1-l1) * k1;
  b2 := t1 * t3 * (t2-l2) * k2;
  b3 := t1 * t2 * (t3-l3) * k3;

  result := abs( (b1 * (x-x1) + b2 * (x-x2) + b3 * (x-x3)) / 1000000);
end;

function TFederEquation.CalcRaw(x, y: single): single;
begin
  result := 0;
end;

function TFederEquation.GetSampleInfo: string;
begin
  if SpringCount > 4 then
  begin
    result := Format('/* Federgraph, Dim %d, Plot %d, Hub %d, Sample %d*/',
    [Dim, Plot, Hub, Sample]);
  end
  else
  begin
    result := Format('/* Federgraph, Scene %d, Plot %d, Hub %d, Sample %d*/',
    [SpringCount, Plot, Hub, Sample]);
  end;
end;

function TFederEquation.GetStatusLine: string;
begin
  result := '';
end;

function TFederEquation.GetValue(x, y: single): single;
begin
  result := CalcFederFormula(x, y);
end;

procedure TFederEquation.InitKoord;
begin
  InitFaktorEQ;
  InitMaxPlotFigure;
end;

procedure TFederEquation.InitFaktorEQ;
begin
  case SpringCount of
    1: FaktorEQ := 1;
    2: FaktorEQ := 1 * 1000;
    3: FaktorEQ := 100 * 1000;
    4: FaktorEQ := 500 * 1000;
    else FaktorEQ := 800;
  end;
end;

procedure TFederEquation.InitMaxPlotFigure;
begin
  case SpringCount of
    1: MaxPlotFigure := 2;
    2: MaxPlotFigure := 13;
    3: MaxPlotFigure := 4;
    4: MaxPlotFigure := 4;
    else FaktorEQ := 4;
  end;
end;

procedure TFederEquation.InitMemo(ML: TStrings);
begin
end;

function TFederEquation.Kraft(k, l, l0: single): single;
begin
  result := Force(k, l, l0, ForceMode);
end;

function TFederEquation.Force(k, l, l0: single; m: Integer): single;
begin
  if LinearForce then
    result := k * (l - l0)
  else
  begin
    result := k * sqr(l - l0) * 50;
  end;
  case m of
    1:
    begin
      if result < 0 then
        result := 0;
    end;
    2:
    begin
      if result > 0 then
        result := 0;
    end;
  end;
end;

procedure TFederEquation.PrepareCalc;
var
  gain1: single;
  gain2: single;
  gain3: single;
  gain4: single;
begin
  gain1 := Abs(Gain);

  if gain1 < 1000 then
    gain2 := sqr(gain1 / 100) * 100 * 1000
  else
    gain2 := sqr(10) * 100 * 1000;

  case SpringCount of
    1: gain3 := 0.005;
    2: gain3 := 1;
    3: gain3 := 500;
    4: gain3 := 25;
    else
      gain3 := 1;
  end;

  gain4 := (1000 + gain2) * gain3;

  case Figure of
    1: fcap := gain4 * 0.1;
    2: fcap := gain4 * 1;
    3: fcap := gain4 * 10;
    4: fcap := gain4 * 100;
    5: fcap := gain4 * 1000;
  end;

  pcap := 100 + Limit;
  mcap := -pcap;
end;

function TFederEquation.Diff1(i: Integer; x, y: single): single;
begin
  //virtual
  result := 0;
end;

function TFederEquation.Diff2(i: Integer; x, y: single): single;
begin
  //virtual
  result := 0;
end;

end.
