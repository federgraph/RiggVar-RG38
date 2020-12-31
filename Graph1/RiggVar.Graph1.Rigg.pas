unit RiggVar.Graph1.Rigg;

interface

uses
  System.IniFiles,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  System.Math.Vectors,
  FMX.Graphics,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Graph,
  RiggVar.RG.Def,
  RiggVar.RG.Calc,
  RiggVar.RG.Types,
  RiggVar.Graph1.DisplayTypes,
  RiggVar.Graph1.DisplayList,
  RiggVar.Graph1.DisplayOrder,
  RiggVar.Graph1.Transform;

type
  TRaumGraphData = class
  public
    xA0, xB0, xC0, xD0, xE0, xF0, xA, xB, xC, xD, xE, xF: single;
    yA0, yB0, yC0, yD0, yE0, yF0, yA, yB, yC, yD, yE, yF: single;
    zA0, zB0, zC0, zD0, zE0, zF0, zA, zB, zC, zD, zE, zF: single;

    xP0, yP0: single;
    xX, yX: single;
    xY, yY: single;
    xZ, yZ: single;
    xM, yM: single;
    xN, yN: single;
    xP, yP: single;
  end;

  TRaumGraphProps = class
  public
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    BogenIndexD: Integer;
    Bogen: Boolean;
    Coloriert: Boolean;
    Color: TAlphaColor;
    Koppel: Boolean;
    Gestrichelt: Boolean;
    RiggLED: Boolean;
  end;

  TZug0 = class
  protected
    PD: TPathData;
  public
    Data: TRaumGraphData; // injected
    Props: TRaumGraphProps; // injected
    constructor Create;
    destructor Destroy; override;
  end;

  TZug3DBase = class(TZug0)
  public
    ZugRumpf: TPolygon;
    ZugMast: TPolygon;
    ZugMastKurve: TPolygon;
    ZugSalingFS: TPolygon;
    ZugSalingDS: TPolygon;
    ZugWanteStb: TPolygon;
    ZugWanteBb: TPolygon;
    ZugController: TPolygon;
    ZugVorstag: TPolygon;
    ZugKoppelKurve: TPolygon;
    ZugAchsen: TPolygon;
    ZugMastfall: TPolygon;
    ZugRP: TPolygon;

    { no need to call SetLength for these, will be copied via Copy }
    ZugMastKurveD0D: TPolygon;
    ZugMastKurveDC: TPolygon;

    constructor Create;
    procedure FillZug; virtual; abstract;
    procedure DrawToCanvas(g: TCanvas); virtual; abstract;
    procedure GetPlotList(ML: TStrings); virtual;
  end;

  TZug3D = class(TZug3DBase)
  public
    procedure FillZug; override;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure GetPlotList(ML: TStrings); override;
  end;

  TRaumGraph = class
  private
    FColor: TAlphaColor;
    FColored: Boolean;
    GrafikOK: Boolean; // loaded with data
    Updated: Boolean; // transformed
    KoppelKurveNeedFill: Boolean;

    { original definition of Achsen }
    AchseN: TPoint3D;
    AchseX: TPoint3D;
    AchseY: TPoint3D;
    AchseZ: TPoint3D;

    { transformed coordinates Achsen }
    AchseNT: TPoint3D;
    AchseXT: TPoint3D;
    AchseYT: TPoint3D;
    AchseZT: TPoint3D;

    { transformed coordinates of Rigg }
    A0, B0, C0, D0, E0, F0, P0: TPoint3D;
    A,  B,  C,  D,  E,  F,  P:  TPoint3D;
    M, N: TPoint3D;
    Zug3D: TZug3DBase; // injected via constructor

    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FKoppelKurve: TKoordLine;
    FKoppel: Boolean;
    FBogen: Boolean;
    FGestrichelt: Boolean;
    FViewPoint: TViewPoint;
    FRiggLED: Boolean;

    BogenIndexD: Integer;
    function FindBogenIndexOf(P: TPoint3D): Integer;
    function GetFreshRiggPoints: TRiggPoints;

    procedure SetKoppel(const Value: Boolean);
    procedure SetKoordinaten(const Value: TRiggPoints);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);

    function GetFixPunkt: TPoint3D;
    function GetStrokeWidthS: single;
    procedure UpdateZugProps;
    procedure Update2;

    procedure SetColor(const Value: TAlphaColor);
    procedure SetColored(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(Value: single);
    function GetFixPoint: TRiggPoint;
    function GetZoom: single;
  public
    rP: TRiggPoints;
    Kurve: TMastKurve;

    DF: TRggFrame;
    DL: TRggDisplayList;
    PD: TPathData;

    WantFixPunkt: Boolean;
    WantRumpf: Boolean;
    WantSaling: Boolean;
    WantController: Boolean;
    WantWante: Boolean;
    WantMast: Boolean;
    WantVorstag: Boolean;
    WantAchsen: Boolean;

    WantRenderE: Boolean;
    WantRenderF: Boolean;
//    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderS: Boolean;

    RaumGraphData: TRaumGraphData;
    RaumGraphProps: TRaumGraphProps;

    Transformer: TRggTransformer; // injected, not owned

    constructor Create(AZug3D: TZug3DBase);
    destructor Destroy; override;

    procedure LoadFromIniFile(FileName: string);

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetKoppelKurve(const Value: TKoordLine);
    function GetMastKurvePoint(const Index: Integer): TPoint3D;

    procedure Update;
    procedure UpdateDisplayList;
    procedure DrawToCanvas(g: TCanvas);

    procedure SetChecked(fa: Integer; Value: Boolean);
    function GetChecked(fa: Integer): Boolean;
    procedure GetPlotList(ML: TStrings);
    property FixPunkt: TPoint3D read GetFixPunkt;

    property Koordinaten: TRiggPoints read rP write SetKoordinaten;
    property KoppelKurve: TKoordLine read FKoppelKurve write SetKoppelKurve;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property Bogen: Boolean read FBogen write SetBogen;
    property WanteGestrichelt: Boolean read FGestrichelt write SetWanteGestrichelt;
    property RiggLED: Boolean read FRiggLED write SetRiggLED;

    property WantRenderH: Boolean read WantRumpf write WantRumpf;
    property StrokeWidthS: single read GetStrokeWidthS;

    property FixPoint: TRiggPoint read GetFixPoint write SetFixPoint;
    property Zoom: single read GetZoom write SetZoom;
    property Coloriert: Boolean read FColored write SetColored;
    property Color: TAlphaColor read FColor write SetColor;
  end;

implementation

{ TZug0 }

constructor TZug0.Create;
begin
  PD := TPathData.Create;
end;

destructor TZug0.Destroy;
begin
  PD.Free;
  inherited;
end;

{ TZug3DBase }

constructor TZug3DBase.Create;
begin
  inherited;
  SetLength(ZugRumpf, 8);
  SetLength(ZugMast, 4);
  SetLength(ZugMastKurve, BogenMax + 2);
  SetLength(ZugSalingFS, 4);
  SetLength(ZugSalingDS, 3);
  SetLength(ZugWanteStb, 3);
  SetLength(ZugWanteBb, 3);
  SetLength(ZugController, 2);
  SetLength(ZugVorstag, 2);
  SetLength(ZugAchsen, 4);
  SetLength(ZugMastfall, 3);
  SetLength(ZugRP, 4);
  SetLength(ZugKoppelKurve, 101);
end;

procedure TZug3DBase.GetPlotList(ML: TStrings);
begin

end;

{ TZug3D }

procedure TZug3D.FillZug;
begin
  with Data do
  begin
    { ZugMastfall }
    ZugMastfall[0].x := xF;
    ZugMastfall[0].y := -yF;
    ZugMastfall[1].x := xM;
    ZugMastfall[1].y := -yM;
    ZugMastfall[2].x := xF0;
    ZugMastfall[2].y := -yF0;

    { ZugRP }
    ZugRP[0].x := xN;
    ZugRP[0].y := -yN;
    ZugRP[1].x := xD0;
    ZugRP[1].y := -yD0;
    ZugRP[2].x := xP0;
    ZugRP[2].y := -yP0;
    ZugRP[3].x := xF0;
    ZugRP[3].y := -yF0;

    { Achsen }
    ZugAchsen[0].x := xN;
    ZugAchsen[0].y := -yN;
    ZugAchsen[1].x := xX;
    ZugAchsen[1].y := -yX;
    ZugAchsen[2].x := xY;
    ZugAchsen[2].y := -yY;
    ZugAchsen[3].x := xZ;
    ZugAchsen[3].y := -yZ;

    { Rumpf }
    ZugRumpf[0].x := xA0;
    ZugRumpf[0].y := -yA0;
    ZugRumpf[1].x := xB0;
    ZugRumpf[1].y := -yB0;
    ZugRumpf[2].x := xC0;
    ZugRumpf[2].y := -yC0;
    ZugRumpf[3].x := xA0;
    ZugRumpf[3].y := -yA0;

    ZugRumpf[4].x := xD0;
    ZugRumpf[4].y := -yD0;
    ZugRumpf[5].x := xB0;
    ZugRumpf[5].y := -yB0;
    ZugRumpf[6].x := xC0;
    ZugRumpf[6].y := -yC0;
    ZugRumpf[7].x := xD0;
    ZugRumpf[7].y := -yD0;

    { Mast }
    ZugMast[0].x := xD0;
    ZugMast[0].y := -yD0;
    ZugMast[1].x := xD;
    ZugMast[1].y := -yD;
    ZugMast[2].x := xC;
    ZugMast[2].y := -yC;
    ZugMast[3].x := xF;
    ZugMast[3].y := -yF;

    { WanteBb }
    ZugWanteBb[0].x := xB0;
    ZugWanteBb[0].y := -yB0;
    ZugWanteBb[1].x := xB;
    ZugWanteBb[1].y := -yB;
    ZugWanteBb[2].x := xC;
    ZugWanteBb[2].y := -yC;

    { WanteStb }
    ZugWanteStb[0].x := xA0;
    ZugWanteStb[0].y := -yA0;
    ZugWanteStb[1].x := xA;
    ZugWanteStb[1].y := -yA;
    ZugWanteStb[2].x := xC;
    ZugWanteStb[2].y := -yC;

    { SalingFS }
    ZugSalingFS[0].x := xA;
    ZugSalingFS[0].y := -yA;
    ZugSalingFS[1].x := xD;
    ZugSalingFS[1].y := -yD;
    ZugSalingFS[2].x := xB;
    ZugSalingFS[2].y := -yB;
    ZugSalingFS[3].x := xA;
    ZugSalingFS[3].y := -yA;

    { SalingDS }
    ZugSalingDS[0].x := xA;
    ZugSalingDS[0].y := -yA;
    ZugSalingDS[1].x := xD;
    ZugSalingDS[1].y := -yD;
    ZugSalingDS[2].x := xB;
    ZugSalingDS[2].y := -yB;

    { Controller }
    ZugController[0].x := xE0;
    ZugController[0].y := -yE0;
    ZugController[1].x := xE;
    ZugController[1].y := -yE;

    { Vorstag }
    ZugVorstag[0].x := xC0;
    ZugVorstag[0].y := -yC0;
    ZugVorstag[1].x := xC;
    ZugVorstag[1].y := -yC;

    { MastKurve }
    ZugMastKurve[BogenMax + 1].x := xF;
    ZugMastKurve[BogenMax + 1].y := -yF;
  end;

  ZugMastKurveD0D := Copy(ZugMastKurve, 0, Props.BogenIndexD + 1);

  ZugMastKurveDC := Copy(
    ZugMastKurve, // string or dynamic array
    Props.BogenIndexD, // start index
    Length(ZugMastKurve) - (Props.BogenIndexD + 1) // count of elements
  );
end;

procedure TZug3D.DrawToCanvas(g: TCanvas);
var
  R: TRectF;

  procedure DrawPoly(P: TPolygon; AOpacity: single);
  var
    ap: TPointF;
  begin
    PD.Clear;
    PD.MoveTo(P[0]);
    for ap in P do
      PD.LineTo(ap);
    g.DrawPath(PD, AOpacity);
  end;

begin
  { FixPunkt }
  g.Stroke.Thickness := 4.0;
  if Props.RiggLED then
    g.Stroke.Color := claLime
  else
    g.Stroke.Color := claYellow;
  R := TRectF.Create(PointF(0, 0));
  R.Inflate(TransKreisRadius, TransKreisRadius);
  g.DrawEllipse(R, 0.8);

  { Koppelkurve }
  if Props.Koppel then
  begin
    g.Stroke.Thickness := 1.0;
    g.Stroke.Color := claKoppelKurve;
    DrawPoly(ZugKoppelkurve, 1.0);
  end;

  { Rumpf }
  g.Stroke.Color := claGray;
  g.Stroke.Thickness := 8.0;
  g.DrawPolygon(ZugRumpf, 0.9);

  { Saling }
  g.Stroke.Thickness := 5;
  if Props.Coloriert then
  begin
  g.Stroke.Color := claSaling;
  if Props.SalingTyp = stFest then
  begin
    g.Stroke.Thickness := 1;
    g.Fill.Color := claSaling;

    g.FillPolygon(ZugSalingFS, 0.3);
    g.Stroke.Thickness := 5;
    DrawPoly(ZugSalingFS, 1.0);
  end
  else if Props.SalingTyp = stDrehbar then
  begin
    DrawPoly(ZugSalingDS, 1.0);
  end;
  end
  else
  begin
    g.Stroke.Color := Props.Color;
    g.Stroke.Thickness := 1;
    if Props.SalingTyp = stFest then
      DrawPoly(ZugSalingFS, 1.0)
    else if Props.SalingTyp = stDrehbar then
      DrawPoly(ZugSalingDS, 1.0);
  end;

  { Mast }
  if Props.Coloriert and Props.Bogen then
  begin
    g.Stroke.Color := claCornflowerblue;
    g.Stroke.Thickness := 12.0;
    DrawPoly(ZugMastKurve, 0.5);

    g.Stroke.Color := claMast;
    g.Stroke.Thickness := 1.0;
    DrawPoly(ZugMastKurve, 1.0);
  end
  else if Props.Coloriert then
  begin
    g.Stroke.Color := claCornflowerblue;
    g.Stroke.Thickness := 12.0;
    DrawPoly(ZugMast, 0.5);

    g.Stroke.Color := claMast;
    g.Stroke.Thickness := 1.0;
    DrawPoly(ZugMast, 0.5);
  end
  else
  begin
    g.Stroke.Color := Props.Color;
    g.Stroke.Thickness := 1.0;
    DrawPoly(ZugMast, 0.5);
  end;

  { Controller }
  if Props.ControllerTyp <> ctOhne then
  begin
    g.Stroke.Thickness := 10.0;
    g.Stroke.Color := claController;
    DrawPoly(ZugController, 0.5);
  end;

  g.Stroke.Thickness := 2.0;

  { Wante Bb }
  if Props.Coloriert then
  begin
  if Props.Gestrichelt then
    g.Stroke.Color := TAlphaColors.Antiquewhite
  else
    g.Stroke.Color := claRed;
  end
  else
    g.Stroke.Color := Props.Color;
  DrawPoly(ZugWanteBb, 1.0);

  { Wante Stb }
  if Props.Coloriert then
  begin
  if Props.Gestrichelt then
    g.Stroke.Color := TAlphaColors.Antiquewhite
  else
    g.Stroke.Color := claGreen;
  end
  else
    g.Stroke.Color := Props.Color;
  DrawPoly(ZugWanteStb, 1.0);

  { Vorstag }
  if Props.Coloriert then
  begin
    g.Stroke.Thickness := 3.0;
  g.Stroke.Color := claVorstag;
  end
  else
  begin
    g.Stroke.Thickness := 1.0;
    g.Stroke.Color := Props.Color;
  end;
  DrawPoly(ZugVorstag, 1.0);
end;

procedure TZug3D.GetPlotList(ML: TStrings);
  procedure Plot(L: TPolygon);
  var
    s: string;
    i: Integer;
  begin
    with ML do
    begin
      s := Format('PU %d %d;', [L[0].x, L[0].y]);
      Add(s);
      for i := 1 to High(L) do
      begin
        s := Format('PD %d %d;', [L[i].x, L[i].y]);
        Add(s);
      end;
    end;
  end;

begin
  with ML do
  begin
    { Rumpf }
    Add('SP 1;');
    Plot(ZugRumpf);
    { Saling }
    if (Props.SalingTyp = stFest) or (Props.SalingTyp = stDrehbar) then
    begin
      Add('SP 2;');
      if Props.SalingTyp = stFest then
        Plot(ZugSalingFS)
      else if Props.SalingTyp = stDrehbar then
        Plot(ZugSalingDS);
    end;
    { Mast }
    Add('SP 3;');
    Plot(ZugMast);
    Add('SP 4;');
    Plot(ZugMastKurve);
    { Controller }
    Add('SP 5;');
    if Props.ControllerTyp <> ctOhne then
      Plot(ZugController);
    { Wanten }
    Add('SP 6;');
    Plot(ZugWanteStb);
    Add('SP 7;');
    Plot(ZugWanteBb);
    { Vorstag }
    Add('SP 8;');
    Plot(ZugVorstag);
  end;
end;

{ TRaumGraph }

constructor TRaumGraph.Create(AZug3D: TZug3DBase);
begin
  FSalingTyp := stFest;
  FControllerTyp := ctOhne;

  RaumGraphData := TRaumGraphData.Create;
  RaumGraphProps := TRaumGraphProps.Create;
  FColor := claGray;
  FColored := True;

  WantFixPunkt := True;
  WantRumpf := True;
  WantSaling := True;
  WantController := False;
  WantWante := True;
  WantMast := True;
  WantVorstag := True;
  WantAchsen := False;

  Zug3D := AZug3D;
  Zug3D.Data := RaumGraphData;
  Zug3D.Props := RaumGraphProps;

  DF := TRggFrame.Create;
  DL := TRggDisplayList.Create;
  DL.DF := DF;
  PD := TPathData.Create;

  AchseN.X := 0;
  AchseN.Y := 0;
  AchseN.Z := 0;

  AchseX.X := 1;
  AchseX.Y := 0;
  AchseX.Z := 0;

  AchseY.X := 0;
  AchseY.Y := 1;
  AchseY.Z := 0;

  AchseZ.X := 0;
  AchseZ.Y := 0;
  AchseZ.Z := 1;

  AchseX := AchseX * 1000;
  AchseY := AchseY * 1000;
  AchseZ := AchseZ * 1000;
end;

destructor TRaumGraph.Destroy;
begin
  Zug3D.Free;
  DL.Free;
  PD.Free;
  DF.Free;
  RaumGraphData.Free;
  RaumGraphProps.Free;
  inherited;
end;

procedure TRaumGraph.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  RaumGraphProps.Color := Value;
end;

procedure TRaumGraph.SetColored(const Value: Boolean);
begin
  FColored := Value;
  RaumGraphProps.Coloriert := FColored;
end;

procedure TRaumGraph.SetFixPoint(const Value: TRiggPoint);
begin
  Transformer.FixPoint := Value;
  Updated := False;
end;

procedure TRaumGraph.SetZoom(Value: single);
begin
  Transformer.Zoom := Value;
  Updated := False;
  KoppelKurveNeedFill := True;
end;

function TRaumGraph.GetFixPoint: TRiggPoint;
begin
  result := Transformer.FixPoint;
end;

function TRaumGraph.GetZoom: single;
begin
  result := Transformer.Zoom;
end;

procedure TRaumGraph.SetKoordinaten(const Value: TRiggPoints);
begin
  rP := Value;
  GrafikOK := True;
  Updated := False;
end;

procedure TRaumGraph.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  RaumGraphProps.Koppel := True;
end;

procedure TRaumGraph.SetKoppelKurve(const Value: TKoordLine);
begin
  FKoppelKurve := Value;
  KoppelKurveNeedFill := True;
end;

procedure TRaumGraph.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
  RaumGraphProps.ControllerTyp := Value;
end;

procedure TRaumGraph.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
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
    Kurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    Kurve[j].Y := 0;
    Kurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TRaumGraph.SetRiggLED(const Value: Boolean);
begin
  FRiggLED := Value;
end;

procedure TRaumGraph.SetMastKurve(const Value: TMastKurve);
begin
  Kurve := Value;
  Updated := False;
end;

procedure TRaumGraph.SetSalingTyp(const Value: TSalingTyp);
begin
  FSalingTyp := Value;
  RaumGraphProps.SalingTyp := Value;
end;

procedure TRaumGraph.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  RaumGraphProps.Bogen := Value;
  Updated := False;
end;

procedure TRaumGraph.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  Updated := False;
end;

procedure TRaumGraph.SetWanteGestrichelt(const Value: Boolean);
begin
  FGestrichelt := Value;
  RaumGraphProps.Gestrichelt := Value;
end;

function TRaumGraph.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(Kurve)) then
    result := Kurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TRaumGraph.FindBogenIndexOf(P: TPoint3D): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: single;
  a: single;
begin
  j := Length(Kurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := P.Distance(Kurve[i]);
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

function TRaumGraph.GetFreshRiggPoints: TRiggPoints;
var
  i: TRiggPoint;
begin
  for i := Low(TRiggPoint) to High(TRiggPoint) do
  begin
    result.V[i] := TPoint3D.Zero;
  end;
end;

procedure TRaumGraph.LoadFromIniFile(FileName: string);
var
  IniFile: TIniFile;
  S: string;
  i: TRiggPoint;
  iP: TRiggPoints;
begin
  iP := GetFreshRiggPoints;
  IniFile := TIniFile.Create(FileName);
  S := 'Koordinaten Rumpf';
  try
    with IniFile do
    begin
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
    end;
    for i := ooA0 to ooF0 do
    begin
      rP.V[i] := iP.V[i];
    end;
    for i := ooA to ooF do
    begin
      rP.V[i] := iP.V[i];
    end;
    GrafikOK := True;
    Updated := False;
  finally
    IniFile.Free;
  end;
end;

procedure TRaumGraph.Update;
begin
  Update2;
  UpdateZugProps;
  Zug3D.FillZug; // needs updated Props (BogenIndex)
  Updated := True;
end;

procedure TRaumGraph.Update2;
var
  i: TRiggPoint;
  j: Integer;
  RPT: TRiggPoints;
  MKT: array [0 .. BogenMax] of TPoint3D;
  KKT: TKoordLine;
begin
  { Graph drehen }
  if Assigned(Transformer) then
  begin
    for i := Low(TRiggPoint) to High(TRiggPoint) do
      RPT.V[i] := Transformer.TransformPoint(rP.V[i]);
    for j := 0 to BogenMax do
      MKT[j] := Transformer.TransformPoint(Kurve[j]);

    if Koppel then
    for j := 0 to 100 do
      KKT[j] := Transformer.TransformPoint(KoppelKurve[j]);
  end;

  DF.Koordinaten := RPT;

  AchseNT := Transformer.TransformPoint(AchseN);
  AchseXT := Transformer.TransformPoint(AchseX);
  AchseYT := Transformer.TransformPoint(AchseY);
  AchseZT := Transformer.TransformPoint(AchseZ);

  A0 := RPT.A0;
  B0 := RPT.B0;
  C0 := RPT.C0;
  D0 := RPT.D0;
  E0 := RPT.E0;
  F0 := RPT.F0;
  P0 := RPT.P0;

  A := RPT.A;
  B := RPT.B;
  C := RPT.C;
  D := RPT.D;
  E := RPT.E;
  F := RPT.F;
  P := RPT.P;

  M := RPT.M;

  { Es wurde nicht nur rotiert,
    sondern bereits auch verschoben und skaliert }

  with RaumGraphData do
  begin
    xA0 := RPT.A0.X;
    yA0 := RPT.A0.Z;
    xB0 := RPT.B0.X;
    yB0 := RPT.B0.Z;
    xC0 := RPT.C0.X;
    yC0 := RPT.C0.Z;
    xD0 := RPT.D0.X;
    yD0 := RPT.D0.Z;
    xE0 := RPT.E0.X;
    yE0 := RPT.E0.Z;
    xF0 := RPT.F0.X;
    yF0 := RPT.F0.Z;

    xA := RPT.A.X;
    yA := RPT.A.Z;
    xB := RPT.B.X;
    yB := RPT.B.Z;
    xC := RPT.C.X;
    yC := RPT.C.Z;
    xD := RPT.D.X;
    yD := RPT.D.Z;
    xE := RPT.E.X;
    yE := RPT.E.Z;
    xF := RPT.F.X;
    yF := RPT.F.Z;

    xP0 := RPT.P0.X;
    yP0 := RPT.P0.Z;
    xP := RPT.P.X;
    yP := RPT.P.Z;
    xM := RPT.M.X;
    yM := RPT.M.Z;
    xN := AchseNT.X;
    yN := AchseNT.Z;

    xX := AchseXT.X;
    yX := AchseXT.Z;
    xY := AchseYT.X;
    yY := AchseYT.Z;
    xZ := AchseZT.X;
    yZ := AchseZT.Z;
  end;

  { MastKurve }
  for j := 0 to BogenMax do
  begin
    Zug3D.ZugMastKurve[j].x := MKT[j].X;
    Zug3D.ZugMastKurve[j].y := -MKT[j].Z;
  end;

  { Koppelkurve }
  if Koppel then
  begin
    for j := 0 to 100 do
    begin
      Zug3D.ZugKoppelKurve[j].X := KKT[j].X;
      Zug3D.ZugKoppelKurve[j].Y := -KKT[j].Z;
    end;
  end;
end;

procedure TRaumGraph.DrawToCanvas(g: TCanvas);
begin
  if not GrafikOK then
    Exit;

  if not Updated then
    Update;

  Zug3D.DrawToCanvas(g);
end;

function TRaumGraph.GetFixPunkt: TPoint3D;
begin
  result := Transformer.TransformedFixPunkt;
end;

procedure TRaumGraph.GetPlotList(ML: TStrings);
begin
  if not GrafikOK then
    Exit;
  if not Updated then
      Update;

  Zug3D.GetPlotList(ML);
end;

function TRaumGraph.GetStrokeWidthS: single;
begin
  if WantRenderS then
    result := 5.0
  else
    result := 2.0;
end;

procedure TRaumGraph.UpdateZugProps;
var
  cr: TRaumGraphProps;
begin
  BogenIndexD := FindBogenIndexOf(rP.D);

  cr := RaumGraphProps;

  cr.BogenIndexD := BogenIndexD;
  cr.Bogen := Bogen;

  cr.Koppel := Koppel;
  cr.Gestrichelt := WanteGestrichelt;

  cr.SalingTyp := SalingTyp;
  cr.ControllerTyp := ControllerTyp;

  cr.Coloriert := Coloriert;
  cr.Color := Color;

  cr.RiggLED := RiggLED;
end;

procedure TRaumGraph.UpdateDisplayList;
var
  DI: TDisplayItem;
begin
  DL.Clear;
  DI := DL.DI;

  with Zug3D do
  begin

    if WantFixpunkt then
    begin
      DI.StrokeColor := claYellow;
      DI.StrokeWidth := 1;
      DL.Ellipse('Fixpunkt', deFixPunkt, FixPunkt, FixPunkt, PointF(0, 0), TKR);
    end;

    { Rumpf }
    if WantRumpf then
    begin
    DI.StrokeColor := claAqua;
      DI.StrokeWidth := 3;
      DL.Line('A0-B0', deA0B0, A0, B0, ZugRumpf[0], ZugRumpf[1], claBlue);
      DL.Line('B0-C0', deB0C0, B0, C0, ZugRumpf[1], ZugRumpf[2], claDodgerBlue);
      DL.Line('C0-A0', deA0C0, C0, A0, ZugRumpf[2], ZugRumpf[0], claCornflowerblue);

      DL.Line('A0-D0', deA0D0, A0, D0, ZugRumpf[0], ZugRumpf[4], claGreen);
      DL.Line('B0-D0', deB0D0, B0, D0, ZugRumpf[1], ZugRumpf[4], claRed);
      DL.Line('C0-D0', deC0D0, C0, D0, ZugRumpf[2], ZugRumpf[4], claYellow);
    end;

    { Mast }
    if WantMast then
    begin
      DI.StrokeColor := TAlphaColors.Cornflowerblue;
      DI.StrokeWidth := 5;
      if Props.Bogen then
      begin
        DL.PolyLine('D0-D', deD0D, D0, D, ZugMastKurveD0D, TAlphaColors.Cornflowerblue);
        DL.PolyLine('D-C', deCD, D, C, ZugMastKurveDC, TAlphaColors.Plum);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TAlphaColors.Navy);
      end
      else
      begin
        DL.Line('D0-D', deD0D, D0, D, ZugMast[0], ZugMast[1], TAlphaColors.Cornflowerblue);
        DL.Line('D-C', deCD, D, C, ZugMast[1], ZugMast[2], TAlphaColors.Lime);
        DL.Line('C-F', deCF, C, F, ZugMast[2], ZugMast[3], TAlphaColors.Navy);
      end;
    end;

    { Wanten }
    if WantWante then
    begin
      { Wante Stb }
      DI.StrokeColor := claGreen;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('A0-A', deA0A, A0, A, ZugWanteStb[0], ZugWanteStb[1], claGreen);
      DL.Line('A-C', deAC, A, C, ZugWanteStb[1], ZugWanteStb[2], claLime);

      { Wante Bb }
      DI.StrokeColor := claRed;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('B0-B', deB0B, B0, B, ZugWanteBb[0], ZugWanteBb[1], claRed);
      DL.Line('B-C', deBC, B, C, ZugWanteBb[1], ZugWanteBb[2], claLime);
    end;

    { Saling }
    if WantSaling then
    begin
      DI.StrokeColor := claChartreuse;
      DI.StrokeWidth := 6;
      if Props.SalingTyp = stFest then
      begin
        DL.Line('A-D', deAD, A, D, ZugSalingFS[0], ZugSalingFS[1], claLime);
        DL.Line('B-D', deBD, B, D, ZugSalingFS[2], ZugSalingFS[1], claChartreuse);
        DL.Line('A-B', deAB, A, B, ZugSalingFS[0], ZugSalingFS[2], claTeal);
      end;
      if Props.SalingTyp = stDrehbar then
      begin
        DI.StrokeColor := claChartreuse;
        DI.StrokeWidth := 2;
        DL.Line('A-D', deAD, A, D, ZugSalingDS[0], ZugSalingDS[1], claChartreuse);
        DL.Line('B-D', deBD, B, D, ZugSalingDS[2], ZugSalingDS[1], claChartreuse);
      end;
    end;

    { Controller }
    if WantController then
    begin
      if Props.ControllerTyp <> ctOhne then
      begin
        DI.StrokeColor := claOrchid;
        DI.StrokeWidth := 4;
        DL.Line('E0-E', deE0E, E0, E, ZugController[0], ZugController[1], claTeal);
      end;
    end;

    { Vorstag }
    if WantVorstag then
    begin
      DI.StrokeColor := claYellow;
      DI.StrokeWidth := StrokeWidthS;
      DL.Line('C0-C', deC0C, C0, C, ZugVorstag[0], ZugVorstag[1], claYellow);
    end;

    { Achsen }
    if WantAchsen then
    begin
      DI.StrokeWidth := 1.0;
      DI.StrokeColor := claFuchsia;
      DL.Line('N-X', deNX, AchseNT, AchseXT, ZugAchsen[0], ZugAchsen[1], claRed);
      DI.StrokeColor := claLime;
      DL.Line('N-Y', deNY, AchseNT, AchseYT, ZugAchsen[0], ZugAchsen[2], claGreen);
      DI.StrokeColor := claAqua;
      DL.Line('N-Z', deNZ, AchseNT, AchseZT, ZugAchsen[0], ZugAchsen[3], claBlue);
    end;

    if WantRenderF then
    begin
      DI.StrokeColor := claGoldenrod;
      DI.StrokeWidth := 1;
      DL.Line('F-M', deMastFall, F, M, ZugMastfall[0], ZugMastfall[1], claGoldenrod);
      DI.StrokeWidth := 4;
      DL.Line('M-F0', deMastFall, M, F0, ZugMastfall[1], ZugMastfall[2], claYellow);
    end;

    if WantRenderP then
    begin
      DI.StrokeColor := claSilver;
      DI.StrokeWidth := 1;
      DL.Line('N-D0', deHullFrame, N, D0, ZugRP[0], ZugRP[1], claFuchsia);
      DL.Line('D0-P0', deHullFrame, D0, P0, ZugRP[1], ZugRP[2], claLime);
      DL.Line('P0-F0', deHullFrame, P0, F0, ZugRP[2], ZugRP[3], claAqua);
      DL.Line('F0-N', deHullFrame, F0, N, ZugRP[3], ZugRP[0], claSilver);
    end;

  end;

  DF.WantController := WantController;
  DF.WantAchsen := WantAchsen;
  DF.Sort;
end;

function TRaumGraph.GetChecked(fa: Integer): Boolean;
begin
  case fa of
    faToggleSegmentF: result := WantFixPunkt;
    faToggleSegmentR: result := WantRumpf;
    faToggleSegmentS: result := WantSaling;
    faToggleSegmentM: result := WantMast;
    faToggleSegmentV: result := WantVorstag;
    faToggleSegmentW: result := WantWante;
    faToggleSegmentC: result := WantController;
    faToggleSegmentA: result := WantAchsen;

    faRggBogen: result := Bogen;
    faRggKoppel: result := Koppel;
    else
      result := False;
  end;
end;

procedure TRaumGraph.SetChecked(fa: Integer; Value: Boolean);
begin
  case fa of
    faToggleSegmentF: WantFixPunkt := Value;
    faToggleSegmentR: WantRumpf := Value;
    faToggleSegmentS: WantSaling := Value;
    faToggleSegmentM: WantMast := Value;
    faToggleSegmentV: WantVorstag := Value;
    faToggleSegmentW: WantWante := Value;
    faToggleSegmentC: WantController := Value;
    faToggleSegmentA: WantAchsen := Value;
  end;
end;

end.
