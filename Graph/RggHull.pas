unit RggHull;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  System.Types,
  System.Math.Vectors,
  FMX.Graphics,
  RggTypes,
  RggCalc,
  RggMatrix,
  RggDisplay,
  RggDisplayTypes,
  RggRaumGraph,
  RggTransformer,
  RggZug;

type
  TConColors = array [0 .. 15] of TAlphaColor;

  THullGraph0 = class
  private
    FColor: TAlphaColor;
    FColored: Boolean;
    GrafikOK: Boolean; // loaded with data
    Updated: Boolean; // transformed
    KoppelKurveNeedFill: Boolean;

    { Vertices }
    vert: TVertArrayF; { Gleitkomma-Koordinaten }
    tvert: TVertArrayI; { Integer-Koordinaten - transformed }
    nvert: Integer;
    { Connections }
    con: TConArray;
    ncon: Integer;

    {Palette}
    ColorArray: array of TAlphaColor;

    procedure ReadVerts420;
    procedure ReadCons420(k, l: Integer);
    procedure ReadVertices;
    procedure ReadConnections;

    procedure InitColorArray;
    function GetColor(i: Integer): TAlphaColor;

    function AddVert(x, y, z: single): Integer;
    procedure AddLine(p1, p2: Integer);
    procedure Paint(g: TCanvas);

    procedure SetColor(const Value: TAlphaColor);
    procedure SetColored(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(Value: single);
    function GetFixPoint: TRiggPoint;
    function GetZoom: single;
    procedure Transform;
  public
    RaumGraphData: TRaumGraphData;
    RaumGraphProps: TRaumGraphProps;

    Transformer: TRggTransformer; // injected, not owned

    Factor: TPoint3D;
    ModelFactor: TPoint3D;

    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Update;

    procedure AddToDisplayList(DL: TRggDisplayList);
    procedure DrawToCanvas(Canvas: TCanvas);

    property FixPoint: TRiggPoint read GetFixPoint write SetFixPoint;
    property Zoom: single read GetZoom write SetZoom;
    property Coloriert: Boolean read FColored write SetColored;
    property Color: TAlphaColor read FColor write SetColor;
  end;

implementation

constructor THullGraph0.Create;
begin
  RaumGraphData := TRaumGraphData.Create;
  RaumGraphProps := TRaumGraphProps.Create;
  FColor := claGray;
  FColored := True;

  InitColorArray;

  Factor := TPoint3D.Create(1.0, 1.0, 1.0);
  ModelFactor := Factor;

  Load;
end;

destructor THullGraph0.Destroy;
begin
  RaumGraphData.Free;
  RaumGraphProps.Free;
  inherited;
end;

procedure THullGraph0.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  RaumGraphProps.Color := Value;
end;

procedure THullGraph0.SetColored(const Value: Boolean);
begin
  FColored := Value;
  RaumGraphProps.Coloriert := FColored;
end;

procedure THullGraph0.SetFixPoint(const Value: TRiggPoint);
begin
  Transformer.FixPoint := Value;
  Updated := False;
end;

procedure THullGraph0.SetZoom(Value: single);
begin
  Transformer.Zoom := Value;
  Updated := False;
  KoppelKurveNeedFill := True;
end;

function THullGraph0.GetFixPoint: TRiggPoint;
begin
  result := Transformer.FixPoint;
end;

function THullGraph0.GetZoom: single;
begin
  result := Transformer.Zoom;
end;

procedure THullGraph0.Load;
begin
  nvert := 0;
  ncon := 0;
  ReadVertices; // virtual
  ReadConnections; // virtual
  GrafikOK := True;
end;

procedure THullGraph0.ReadVertices;
begin
  ReadVerts420;
end;

procedure THullGraph0.ReadConnections;
begin
  ReadCons420(10, 7);
end;

{ Add a vertex to the Model }
function THullGraph0.AddVert(x, y, z: single): Integer;
var
  i: Integer;
begin
  i := nvert;
  if i >= maxvert then
  begin
    result := nvert;
    Exit;
  end;
  i := i * 3;
  vert[i + 0] := Factor.X * ModelFactor.X * x;
  vert[i + 1] := Factor.Y * ModelFactor.Y * y;
  vert[i + 2] := Factor.Z * ModelFactor.Z * z;
  Inc(nvert);
  result := nvert;
end;

{ Add a line from vertex p1 to vertex p2 }
procedure THullGraph0.AddLine(p1, p2: Integer);
var
  i, t: Integer;
begin
  i := ncon;
  if (p1 >= nvert) or (p2 >= nvert) then
    Exit;
  if (i >= maxcon) then
    Exit;
  if (p1 > p2) then
  begin
    { vertauschen }
    t := p1;
    p1 := p2;
    p2 := t;
  end;
  con[i] := (p1 shl 16) or p2;
  ncon := i + 1;
end;

procedure THullGraph0.Update;
begin
  if not GrafikOK then
    Exit;
  if nvert <= 0 then
    Exit;
  Transform;
  Updated := True;
end;

procedure THullGraph0.Transform;
var
  i, j: Integer;
  x, y, z: single;
  P: TPoint3D;
begin
  for j := nvert downto 0 do
  begin
    i := j * 3;
    x := vert[i + 0];
    y := vert[i + 1];
    z := vert[i + 2];

    P := TPoint3D.Create(x, y, z) * Transformer.Matrix;

    tvert[i + 0] := Round(P.X);
    tvert[i + 1] := Round(P.Y);
    tvert[i + 2] := Round(P.Z);
  end;
end;

procedure THullGraph0.DrawToCanvas(Canvas: TCanvas);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
    Update;
  Paint(Canvas);
end;

procedure THullGraph0.Paint(g: TCanvas);
var
  i, lim, t, p1, p2, grey: Integer;
  c: TConArray;
  v: TVertArrayI;
  StartPoint, EndPoint: TPointF;
begin
  if nvert <= 0 then
    Exit;

  lim := ncon;
  c := con;
  v := tvert;
  if (lim <= 0) or (nvert <= 0) then
    Exit;

  for i := 0 to lim - 1 do
  begin
    { Indizes in das Vertice-array bestimmen }
    t := c[i]; // T wie Temp
    p1 := ((t shr 16) and $FFFF) * 3; // Index Punkt1
    p2 := (t and $FFFF) * 3; // Index Punkt2

    { Farbe bestimmen Variante 1 }
    if Coloriert then
    begin
      grey := v[p1 + 1] + v[p2 + 1]; // Summe der z-Werte
      if (grey < 0) then
        grey := 0; // grey zwischen 0 und 15
      if (grey > 15) then
        grey := 15;
      g.Stroke.Color := GetColor(grey);
    end
    else
      g.Stroke.Color := claSilver;

    g.Stroke.Thickness := 1.0;

    { Linie zeichnen }
    StartPoint := PointF(v[p1], -v[p1 + 2]);
    EndPoint := PointF(v[p2], -v[p2 + 2]);
    g.DrawLine(StartPoint, EndPoint, 1.0);
  end;
end;

procedure THullGraph0.AddToDisplayList(DL: TRggDisplayList);
var
  ConCount: Integer;
  i, t, p1, p2: Integer;
  yVal: Integer;
  c: TConArray;
  v: TVertArrayI;
  StartPoint, EndPoint: TPointF;
  rp1, rp2: TPoint3D;
  cla: TAlphaColor;
  s: string;
begin
  if nvert <= 0 then
    Exit;

  ConCount := ncon;
  c := con;
  v := tvert;
  if (ConCount <= 0) or (nvert <= 0) then
    Exit;

  for i := 0 to ConCount - 1 do
  begin
    { Indizes in das Vertice-array bestimmen }
    t := c[i]; // t wie Temp
    p1 := ((t shr 16) and $FFFF) * 3; // Index Punkt1
    p2 := (t and $FFFF) * 3; // Index Punkt2

    { Farbe bestimmen Variante 1 }
    if Coloriert then
    begin
      yVal := v[p1 + 1] + v[p2 + 1]; // Summe der y-Werte
      { yVal zwischen 0 und 15 }
      if (yVal < 0) then
        yVal := 0;
      if (yVal > 15) then
        yVal := 15;
      cla := GetColor(yVal);
    end
    else
      cla := claRed;

    { Farbe bestimmen, Varinate 2 }
    // if Coloriert then
    //   cla := GetColor(i)
    // else
    //   cla := clBtnFace;

    StartPoint := PointF(v[p1], -v[p1 + 2]);
    EndPoint := PointF(v[p2], -v[p2 + 2]);

    rp1.X := v[p1 + 0];
    rp1.Y := v[p1 + 1];
    rp1.Z := v[p1 + 2];

    rp2.X := v[p2 + 0];
    rp2.Y := v[p2 + 1];
    rp2.Z := v[p2 + 2];

    DL.DI.StrokeWidth := 3.0;
    DL.DI.StrokeColor := cla;
    s := Format('con-%d',  [i]);
    DL.Line(s, deHullEdge, rp1, rp2, StartPoint, EndPoint, claRed);
  end;
end;

function THullGraph0.GetColor(i: Integer): TAlphaColor;
var
  idx: Word;
  R, G, B: Byte;
begin
  R := 0;
  G := 0;
  B := 1;
  idx := Round(R * 32 + G * 64 + B * 96 + i * 2);
//  result := PaletteIndex(idx);
  if i < Length(ColorArray) then
    result := ColorArray[idx]
  else
    result := claRed;
end;

procedure THullGraph0.ReadVerts420;
begin
  ModelFactor.X := 1;
  ModelFactor.Y := 1;
  ModelFactor.Z := 1;

  AddVert(4200, 0, 328); // Steven Spant 1, Koord 1..7
  AddVert(4194, 0, 260);
  AddVert(4188, 0, 195);
  AddVert(4178, 0, 128);
  AddVert(4168, 0, 78);
  AddVert(4151, 0, 26);
  AddVert(4130, 0, 0);

  AddVert(4100, -157, 325); // Spant 2, Koord 8..20
  AddVert(4100, -149, 268);
  AddVert(4100, -126, 189);
  AddVert(4100, -100, 131);
  AddVert(4100, -69, 74);
  AddVert(4100, -30, 8);
  AddVert(4100, 0, -48);
  AddVert(4100, 30, 8);
  AddVert(4100, 69, 74);
  AddVert(4100, 100, 131);
  AddVert(4100, 126, 189);
  AddVert(4100, 149, 268);
  AddVert(4100, 157, 325);

  AddVert(4000, -244, 322); // Spant 3, Koord 21..33
  AddVert(4000, -237, 263);
  AddVert(4000, -219, 186);
  AddVert(4000, -193, 115);
  AddVert(4000, -159, 51);
  AddVert(4000, -88, -41);
  AddVert(4000, 0, -117);
  AddVert(4000, 88, -41);
  AddVert(4000, 159, 51);
  AddVert(4000, 193, 115);
  AddVert(4000, 219, 186);
  AddVert(4000, 237, 263);
  AddVert(4000, 244, 322);

  AddVert(3750, -402, 315); // pant 4, Koord 34..46
  AddVert(3750, -387, 263);
  AddVert(3750, -374, 176);
  AddVert(3750, -345, 96);
  AddVert(3750, -281, 7);
  AddVert(3750, -155, -93);
  AddVert(3750, 0, -178);
  AddVert(3750, 155, -93);
  AddVert(3750, 281, 7);
  AddVert(3750, 345, 96);
  AddVert(3750, 374, 176);
  AddVert(3750, 387, 263);
  AddVert(3750, 402, 315);

  AddVert(3400, -570, 308); // Spant 5, Koord 47..59
  AddVert(3400, -541, 253);
  AddVert(3400, -506, 166);
  AddVert(3400, -445, 42);
  AddVert(3400, -380, -30);
  AddVert(3400, -212, -126);
  AddVert(3400, 0, -202);
  AddVert(3400, 212, -126);
  AddVert(3400, 380, -30);
  AddVert(3400, 445, 42);
  AddVert(3400, 506, 166);
  AddVert(3400, 541, 253);
  AddVert(3400, 570, 308);

  AddVert(3000, -699, 302); // Spant 6, Koord 60..72
  AddVert(3000, -661, 248);
  AddVert(3000, -619, 163);
  AddVert(3000, -539, 26);
  AddVert(3000, -446, -54);
  AddVert(3000, -251, -138);
  AddVert(3000, 0, -205);
  AddVert(3000, 251, -138);
  AddVert(3000, 446, -54);
  AddVert(3000, 539, 26);
  AddVert(3000, 619, 163);
  AddVert(3000, 661, 248);
  AddVert(3000, 699, 302);

  AddVert(2400, -793, 297); // Spant 7, Koord 73..85
  AddVert(2400, -749, 245);
  AddVert(2400, -716, 167);
  AddVert(2400, -634, 31);
  AddVert(2400, -500, -65);
  AddVert(2400, -296, -135);
  AddVert(2400, 0, -191);
  AddVert(2400, 296, -135);
  AddVert(2400, 500, -65);
  AddVert(2400, 634, 31);
  AddVert(2400, 716, 167);
  AddVert(2400, 749, 245);
  AddVert(2400, 793, 297);

  AddVert(1800, -800, 290); // Spant 8, Koord 86..98
  AddVert(1800, -755, 241);
  AddVert(1800, -725, 173);
  AddVert(1800, -634, 34);
  AddVert(1800, -480, -58);
  AddVert(1800, -269, -115);
  AddVert(1800, 0, -161);
  AddVert(1800, 269, -115);
  AddVert(1800, 480, -58);
  AddVert(1800, 634, 34);
  AddVert(1800, 725, 173);
  AddVert(1800, 755, 241);
  AddVert(1800, 800, 290);

  AddVert(1000, -730, 275); // Spant 9, Koord 99..111
  AddVert(1000, -696, 237);
  AddVert(1000, -674, 185);
  AddVert(1000, -585, 56);
  AddVert(1000, -414, -23);
  AddVert(1000, -214, -64);
  AddVert(1000, 0, -97);
  AddVert(1000, 214, -64);
  AddVert(1000, 414, -23);
  AddVert(1000, 585, 56);
  AddVert(1000, 674, 185);
  AddVert(1000, 696, 237);
  AddVert(1000, 730, 275);

  AddVert(0, -580, 250); // Spant 10, Koord 112..124
  AddVert(0, -568, 226);
  AddVert(0, -560, 187);
  AddVert(0, -485, 89);
  AddVert(0, -300, 30);
  AddVert(0, -167, 13);
  AddVert(0, 0, 0);
  AddVert(0, 167, 13);
  AddVert(0, 300, 30);
  AddVert(0, 485, 89);
  AddVert(0, 560, 187);
  AddVert(0, 568, 226);
  AddVert(0, 580, 250);
end;

procedure THullGraph0.ReadCons420(k, l: Integer);

  procedure AddSection(a, b, c, n: Integer);
  { a = 1.Punkt
    b = 2.Punkt
    c = Increment zwischen Punkten ab dem 2. Punkt
    n = Anzahl der Verbindungen
  }
  var
    i: Integer;
  begin
    AddLine(a - 1, b - 1);
    if n = 1 then
    begin
      Exit; // does not happen
    end;
    for i := 2 to n do
    begin
      a := b;
      b := b + c;
      AddLine(a - 1, b - 1);
    end;
  end;

var
  SpantenZahl: Integer;
  LinienZahl: Integer;
  i, a, b, conCountS, conCountL: Integer;
begin
  { Beispiel-Eingaben }
  // k := 10; // Anzahl der Spanten einschließlich Steven
  // l := 7; // Anzahl der Linien

  SpantenZahl := k - 1; // Anzahl Spanten = 9
  LinienZahl := 2 * l - 1; // Anzahl Linien = 13
  conCountS := l - 1; // Anzahl Verbindungen eines Spantes = 6
  conCountL := k - 1; // Anzahl Verbindungen einer Linie = 9

  a := 1;
  AddSection(a, a + 1, 1, conCountS); { der Steven }
  a := l;
  for i := 1 to SpantenZahl do
  begin
    AddSection(a + 1, a + 2, 1, 2 * conCountS); { die Spanten }
    a := a + LinienZahl;
  end;

  a := 1;
  b := l + 1;
  for i := 1 to l - 1 do
  begin
    AddSection(a, b, LinienZahl, conCountL); { Linien links und Kiel }
    a := a + 1;
    b := b + 1;
  end;
  for i := l to LinienZahl do
  begin
    AddSection(a, b, LinienZahl, conCountL); { Linien rechts }
    a := a - 1;
    b := b + 1;
  end;
end;

procedure THullGraph0.InitColorArray;
var
  i: Integer;
  ac: TAlphaColor;
begin
  SetLength(ColorArray, 257);
  for i := 0 to 256 do
  begin
    ac := CorrectColor(HSLtoRGB(i/256, 0.8, 0.5));
    TAlphaColorRec(ac).A := 200;
    ColorArray[i] := ac;
  end;
end;

end.
