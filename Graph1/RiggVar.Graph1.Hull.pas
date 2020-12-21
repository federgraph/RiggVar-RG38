unit RiggVar.Graph1.Hull;

interface

{$define WantDisplayList}

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  System.Types,
  System.Math.Vectors,
  FMX.Graphics,
  RiggVar.RG.Types,
{$ifdef WantDisplayList}
  RiggVar.Graph1.DisplayList,
  RiggVar.Graph1.DisplayTypes,
{$endif}
  RiggVar.Graph1.Transform;

type
  TCon = record
    L: Integer;
    R: Integer;
    C: TAlphaColor;
  end;

  THullGraph0 = class
  private const
    maxvert = 400;
    maxcon = 1000;
  protected type
  TConColors = array [0 .. 15] of TAlphaColor;
    TVertArrayF = array [0 .. maxvert] of single;
    TVertArrayI = array [0 .. maxvert] of Integer;
    TConArray = array [0 .. maxcon] of TCon;
  private
    FWantLineColors: Boolean;
    DataLoaded: Boolean;
    Updated: Boolean; // transformed

    { Vertices }
    vert: TVertArrayF; { Gleitkomma-Koordinaten }
    tvert: TVertArrayI; { Integer-Koordinaten - transformed }
    nvert: Integer;

    { Connections }
    con: TConArray;
    ncon: Integer;

    { Palette }
    ColorArray: array of TAlphaColor;
    TempColor: TAlphaColor;

    procedure ReadVerts420;
    procedure ReadCons420(k, l: Integer);
    procedure ReadVertices;
    procedure ReadConnections;

    procedure InitColorArray;
    function GetColor(i: Integer): TAlphaColor;

    function AddVert(x, y, z: single): Integer;
    procedure AddLine(p1, p2: Integer);
    procedure Paint(g: TCanvas);

    procedure SetWantLineColors(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(Value: single);
    function GetFixPoint: TRiggPoint;
    function GetZoom: single;
    procedure Transform;
  protected
    xmin, xmax: single;
    ymin, ymax: single;
    zmin, zmax: single;
    yRange: Integer;
    procedure FindBoundingBox;
    procedure FindDepthRange;
    function FindColorIndex(v: single): Integer;
  public
    Transformer: TRggTransformer; // injected, not owned

    Factor: TPoint3D;
    ModelFactor: TPoint3D;

    constructor Create;

    procedure Load;
    procedure Update;

{$ifdef WantDisplayList}
    procedure AddToDisplayList(DL: TRggDisplayList);
{$endif}
    procedure DrawToCanvas(g: TCanvas);

    property FixPoint: TRiggPoint read GetFixPoint write SetFixPoint;
    property Zoom: single read GetZoom write SetZoom;
    property WantLineColors: Boolean read FWantLineColors write SetWantLineColors;
  end;

implementation

uses
  RiggVar.FB.Color;

constructor THullGraph0.Create;
begin
  TempColor := claBlack;
  FWantLineColors := False;

  InitColorArray;

  Factor := TPoint3D.Create(1.0, 1.0, 1.0);
  ModelFactor := Factor;

  Load;
end;

procedure THullGraph0.SetWantLineColors(const Value: Boolean);
begin
  FWantLineColors := Value;
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
  DataLoaded := True;
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
    { swap }
    t := p1;
    p1 := p2;
    p2 := t;
  end;
  con[i].L := p1;
  con[i].R := p2;
  con[i].C := TempColor;
  ncon := i + 1;
end;

procedure THullGraph0.Update;
begin
  if not DataLoaded then
    Exit;
  if nvert <= 0 then
    Exit;
  Transform;
  FindDepthRange;
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

procedure THullGraph0.DrawToCanvas(g: TCanvas);
begin
  if not DataLoaded then
    Exit;
  if not Updated then
    Update;
  Paint(g);
end;

procedure THullGraph0.Paint(g: TCanvas);
var
  i, p1, p2: Integer;
  sd: Integer;
  ci: Integer;
  StartPoint, EndPoint: TPointF;
begin
  if nvert <= 0 then
    Exit;

  if (ncon <= 0) or (nvert <= 0) then
    Exit;

  for i := 0 to ncon - 1 do
  begin
    p1 := con[i].L * 3; // index of point 1
    p2 := con[i].R * 3; // index of point 2

    if WantLineColors then
    begin
      g.Stroke.Color := con[i].C;
    end
    else
    begin
      sd := tvert[p1 + 1] + tvert[p2 + 1]; // sum of 'depth' values
      ci := FindColorIndex(sd);
      g.Stroke.Color := GetColor(ci);
    end;

    g.Stroke.Thickness := 1.0;

    { draw line }
    StartPoint := PointF(tvert[p1], -tvert[p1 + 2]);
    EndPoint := PointF(tvert[p2], -tvert[p2 + 2]);
    g.DrawLine(StartPoint, EndPoint, 1.0);
  end;
end;

{$ifdef WantDisplayList}
procedure THullGraph0.AddToDisplayList(DL: TRggDisplayList);
var
  ConCount: Integer;
  i, p1, p2: Integer;
  sd: Integer;
  ci: Integer;
  StartPoint, EndPoint: TPointF;
  rp1, rp2: TPoint3D;
  cla: TColor;
  s: string;
begin
  if nvert <= 0 then
    Exit;

  ConCount := ncon;
  if (ConCount <= 0) or (nvert <= 0) then
    Exit;

  for i := 0 to ConCount - 1 do
  begin
    p1 := con[i].L * 3;
    p2 := con[i].R * 3;

    if WantLineColors then
    begin
      cla := con[i].C;
    end
    else
    begin
      sd := tvert[p1 + 1] + tvert[p2 + 1]; // sum of 'depth' values
      ci := FindColorIndex(sd);
      cla := GetColor(ci);
    end;

    StartPoint := PointF(tvert[p1], -tvert[p1 + 2]);
    EndPoint := PointF(tvert[p2], -tvert[p2 + 2]);

    rp1.X := tvert[p1 + 0];
    rp1.Y := tvert[p1 + 1];
    rp1.Z := tvert[p1 + 2];

    rp2.X := tvert[p2 + 0];
    rp2.Y := tvert[p2 + 1];
    rp2.Z := tvert[p2 + 2];

    DL.DI.StrokeWidth := 3.0;
    DL.DI.StrokeColor := cla;
    s := Format('con-%d',  [i]);
    DL.Line(s, deHullEdge, rp1, rp2, StartPoint, EndPoint, cla);
  end;
end;
{$endif}

function THullGraph0.GetColor(i: Integer): TAlphaColor;
begin
  if (i < Length(ColorArray)) and (i > -1) then
    result := ColorArray[i]
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
  { a = 1. Punkt
    b = 2. Punkt
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
  TempColor := claBlue;
  AddSection(a, a + 1, 1, conCountS); { der Steven }

  a := l;
  TempColor := claYellow;
  for i := 1 to SpantenZahl do
  begin
    AddSection(a + 1, a + 2, 1, 2 * conCountS); { die Spanten }
    a := a + LinienZahl;
  end;

  a := 1;
  b := l + 1;
  TempColor := claGreen;
  for i := 1 to l - 1 do
  begin
    AddSection(a, b, LinienZahl, conCountL); { right }
    a := a + 1;
    b := b + 1;
  end;

  TempColor := claBlue;
  for i := l to l do
  begin
    AddSection(a, b, LinienZahl, conCountL); { keel }
    a := a - 1;
    b := b + 1;
  end;

  TempColor := claRed;
  for i := l + 1 to LinienZahl do
  begin
    AddSection(a, b, LinienZahl, conCountL); { left }
    a := a - 1;
    b := b + 1;
  end;
end;

procedure THullGraph0.InitColorArray;
var
  i: Integer;
begin
  SetLength(ColorArray, 256);
  for i := 0 to 255 do
  begin
    ColorArray[i] := TRggColors.ColorFromRGB(i, 50, 128);
  end;
end;

procedure THullGraph0.FindBoundingBox;
var
  v: TVertArrayF;
  lxmin, lymin, lzmin: single;
  lxmax, lymax, lzmax: single;
  x, y, z: single;
  i, j: Integer;
begin
  { Find the bounding box of this model }

  if (nvert <= 0) then
    Exit;

  v := vert;
  lxmin := v[0];
  lxmax := lxmin;
  lymin := v[1];
  lymax := lymin;
  lzmin := v[2];
  lzmax := lzmin;
  for j := nvert downto 0 do
  begin
    i := j * 3;
    x := v[i];
    if (x < lxmin) then
      lxmin := x;
    if (x > lxmax) then
      lxmax := x;

    y := v[i + 1];
    if (y < lymin) then
      lymin := y;
    if (y > lymax) then
      lymax := y;

    z := v[i + 2];
    if (z < lzmin) then
      lzmin := z;
    if (z > lzmax) then
      lzmax := z;
  end;
  xmax := lxmax; // 4200
  xmin := lxmin; //    0
  ymax := lymax; //  800
  ymin := lymin; // -800
  zmax := lzmax; //  328
  zmin := lzmin; // -205
end;

procedure THullGraph0.FindDepthRange;
var
  v: TVertArrayI;
  lymin: single;
  lymax: single;
  y: single;
  i, j: Integer;
begin
  if (nvert <= 0) then
    Exit;

  v := tvert;
  lymin := v[1];
  lymax := lymin;
  for j := nvert downto 0 do
  begin
    i := j * 3;

    y := v[i + 1];
    if (y < lymin) then
      lymin := y;
    if (y > lymax) then
      lymax := y;

  end;
  ymax := lymax;
  ymin := lymin;
  yRange := Round(ymax - ymin);
end;

function THullGraph0.FindColorIndex(v: single): Integer;
var
  v1: single;
  v2: single;
begin
  result := 0;

  if YRange = 0 then
    Exit;

  v1 := v / 2;
  v2 := (v1 - ymin) * 255 / YRange;
  result := Round(255 - v2);

  { result in 0..15 }
  if (result < 0) then
    result := 0;
  if (result > 255) then
    result := 255;
end;

end.
