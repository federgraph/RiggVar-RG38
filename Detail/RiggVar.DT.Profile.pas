unit RiggVar.DT.Profile;

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math.Vectors,
  FMX.Graphics,
  RiggVar.RG.Types;

type
  TLage = (Hoch, Quer);

  TMastProfile = class
  protected
    Lage: TLage;
    OffsetX: single;
    OffsetY: single;
  protected
    SalingZoom1: single;
    SalingZoom2: single;
    SalingZoom3: single;
  protected
    ControllerZoom1: single;
    ControllerZoom2: single;
    ControllerZoom3: single;
  protected
    ProfileOpacity: single;
    ProfileZoom: single;
    WantInner: Boolean;
    WantOuter: Boolean;
    WantLeft: Boolean;
    WantRight: Boolean;
    WantSegmentColor: Boolean;
    procedure InternalDrawProfile3(Canvas: TCanvas);
    procedure InternalDrawProfile6(Canvas: TCanvas);
  private
    FPathDataHoch: TPathData;
    FPathDataQuer: TPathData;
    function InitPathData(lg: TLage): TPathData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProfileDrawTest(g: TCanvas; lg: TLage);
    procedure ProfilePathTest(g: TCanvas; lg: TLage);

    procedure ProfileDraw(g: TCanvas; lg: TLage; Opacity: single);
    procedure ProfilePath(g: TCanvas; lg: TLage; Opacity: single);

    property ProfileHoch: TPathData read FPathDataHoch;
    property ProfileQuer: TPathData read FPathDataQuer;
  end;

implementation

constructor TMastProfile.Create;
begin
  ControllerZoom1 := 1.0;
  ControllerZoom2 := 0.1;
  ControllerZoom3 := 1.0;

  SalingZoom1 := 5.0;
  SalingZoom2 := 10.0;
  SalingZoom3 := 1.0;

  ProfileZoom := 1.0;
  ProfileOpacity := 1.0;

  FPathDataHoch := InitPathData(Hoch);
  FPathDataQuer := InitPathData(Quer);
end;

destructor TMastProfile.Destroy;
begin
  FPathDataHoch.Free;
  FPathDataQuer.Free;
  inherited;
end;

procedure TMastProfile.ProfilePathTest(g: TCanvas; lg: TLage);
var
  ox: single;
  oy: single;
  zf: single;
  pd: TPathData;

  procedure MoveTo(x1, y1: single);
  begin
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    case lg of
      Quer: pd.MoveTo(PointF(y1, x1));
      Hoch: pd.MoveTo(PointF(x1, y1));
    end;
  end;

  procedure LineTo(x2, y2: single);
  begin
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer: pd.LineTo(PointF(y2, x2));
      Hoch: pd.LineTo(PointF(x2, y2));
    end;
  end;

  procedure AddArc(xm, ym, Radius, phi1, phi2: single);
  var
    a1, a2: single;
  begin
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := 90 - phi1;
      a2 := phi1-phi2;
      pd.AddArc(PointF(ym, xm), PointF(Radius, Radius), a1, a2);
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1;
      a2 := phi2-phi1;
      pd.AddArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2);
    end;
  end;

begin
  g.Stroke.Cap := TStrokeCap.Round;
  g.Stroke.Join := TStrokeJoin.Round;
  g.Stroke.Thickness := 3.0;
  ox := 200;
  oy := 10;
  zf := 5.0;
  pd := TPathData.Create;

  { outer path right}
  MoveTo(   5.00,  0.00);
  LineTo(   2.00,  0.00);
  AddArc( -18.50, 40.70, 47.00, -60.00, 0.00);
  LineTo(  28.50, 43.50);
  AddArc(   0.00, 43.50, 28.50, 0.00, 90.00);

  AddArc(   0.00, 43.50, 28.50, 90.00, 180.00);
  LineTo( -28.50, 40.70);
  AddArc(  18.50, 40.70, 47.00, -180.00, -120.00);
  LineTo(  -2.00, 0.00);

  AddArc(  -3.50,  0.00, 1.50, 0.00, 132.53);
  AddArc(  18.50, 40.70, 45.80, -129.17, -120.16);
  LineTo( -10.44, 5.20);
  AddArc(   0.00,  4.30, 7.50, 130.85, 90.00);

  AddArc(   0.00,  4.30, 7.50, 90.00, 49.15);
  LineTo(  10.44, 5.20);
  AddArc( -18.50, 40.70, 45.80, -59.83,  -50.83);
  AddArc(   3.50,  0.00, 1.50, 47.46,  180.00);

  { inner path right }
  MoveTo(    0.0, 13.00);
  LineTo(   3.50, 13.00);

  LineTo(   3.50, 13.00);
  AddArc( -18.50, 40.70, 45.80, -49.09, -16.70);
  AddArc(  11.00, 31.85, 15.00, -16.71, 0.00);
  LineTo(  26.00, 49.00);
  AddArc(  11.00, 49.00, 15.00, 0.00, 26.58);
  AddArc(   0.00, 43.50, 27.30, 26.58, 90.00);

  { inner path left }
  AddArc(   0.00, 43.50, 27.30, 90.00, 153.42);
  AddArc( -11.00, 49.00, 15.00, 153.42, 180.00);
  LineTo( -26.00, 31.85);
  AddArc( -11.00, 31.85, 15.00, -180.00, -163.29);
  AddArc(  18.50, 40.70, 45.80, -163.29, -130.91);
  LineTo(  -3.50, 13.00);
  LineTo(   0.00, 13.00);

  { symmetry line }
//  MoveTo(   0.00,  0.00);
//  LineTo(   0.00, 72.00);

  g.Stroke.Color := claRed;
  g.DrawPath(pd, 1.0);
  pd.Free;
end;

procedure TMastProfile.ProfileDrawTest(g: TCanvas; lg: TLage);
var
  ox: single;
  oy: single;
  zf: single;

  procedure MetaLINE(x1, y1, x2, y2: single; c: TAlphaColor);
  begin
    g.Stroke.Color := c;
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer: g.DrawLine(PointF(y1, x1), PointF(y2, x2), 1.0);
      Hoch: g.DrawLine(PointF(x1, y1), PointF(x2, y2), 1.0);
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; c: TAlphaColor);
  var
    a1, a2: single;
  begin
    g.Stroke.Thickness := 2.0;
    g.Stroke.Color := c;
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := 90 - phi1;
      a2 := phi1-phi2;
      g.DrawArc(PointF(ym, xm), PointF(Radius, Radius), a1, a2, 1.0);
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1;
      a2 := phi2-phi1;
      g.DrawArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2, 1.0 );
    end;
  end;

begin
  g.Stroke.Cap := TStrokeCap.Round;
  g.Stroke.Join := TStrokeJoin.Round;
  g.Stroke.Thickness := 3.0;
  ox := 200;
  oy := 10;
  zf := 5.0;

  { outer path clockwise }
  MetaLINE(   5.00,  0.00,  2.00,  0.00, claMagenta);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00, claMagenta);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, claMagenta);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, claMagenta);

  MetaARC(    0.00, 43.50, 28.50,   90.00,  180.00, claAqua);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, claAqua);
  MetaARC(   18.50, 40.70, 47.00, -180.00, -120.00, claAqua);
  MetaLINE(  -5.00,  0.00,  -2.00,  0.00, claAqua);

  MetaARC(   -3.50,  0.00,  1.50,    0.00,  132.53, claLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, claLime);
  MetaLINE(  -4.85, 10.02, -10.44,  5.20, claLime);
  MetaARC(    0.00,  4.30,  7.50,   90.00,  130.85, claLime);

  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, claAqua);
  MetaLINE(   4.90,  9.97, 10.44,  5.20, claAqua);
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, claAqua);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, claAqua);

  { inner path right }
  MetaLINE(    0.0, 13.00,  3.50, 13.00, claBlue);
  MetaLINE(   3.50, 13.00, 11.48,  6.09, claBlue);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, claBlue);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, claBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, claBlue);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, claBlue);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, claBlue);

  { inner path left }
  MetaARC(    0.00, 43.50, 27.30,   90.00,  153.42, claRed);
  MetaARC(  -11.00, 49.00, 15.00,  153.42,  180.00, claRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, claRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, claRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, claRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, claYellow);
  MetaLINE(  -3.50, 13.00,  0.00, 13.00, claYellow);

  { symmetry line }
//  MetaLINE(   0.00, 72.00,   0.00,  0.00, claRed);

{ Legend: }
{ MetaLINE(    x1,   y1,    x2,     y2); }
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
end;

procedure TMastProfile.InternalDrawProfile3(Canvas: TCanvas);

  procedure MetaLINE(x1, y1, x2, y2: single);
  begin
    if Lage = Quer then
    begin
      x1 := x1 * ControllerZoom3 + OffsetX;
      y1 := y1 * ControllerZoom3 + OffsetY;
      x2 := x2 * ControllerZoom3 + OffsetX;
      y2 := y2 * ControllerZoom3 + OffsetY;
      Canvas.DrawLine(PointF(y1, x1), PointF(y2, x2), 1.0);
    end
    else if Lage = Hoch then
    begin
      x1 := x1 * SalingZoom3 + OffsetX;
      y1 := y1 * SalingZoom3 + OffsetY;
      x2 := x2 * SalingZoom3 + OffsetX;
      y2 := y2 * SalingZoom3 + OffsetY;
      Canvas.DrawLine(PointF(x1, y1), PointF(x2, y2), 1.0);
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single);
  var
    a1, a2: single;
  begin
    if Lage = Quer then
    begin
      xm := xm * ControllerZoom3 + OffsetX;
      ym := ym * ControllerZoom3 + OffsetY;
      Radius := Radius * ControllerZoom3;
      a1 := 90 - phi1;
      a2 := phi1-phi2;
      Canvas.DrawArc(PointF(ym, xm), PointF(Radius, Radius), a1, a2, 1.0);
    end
    else if Lage = Hoch then
    begin
      xm := xm * SalingZoom3 + OffsetX;
      ym := ym * SalingZoom3 + OffsetY;
      Radius := Radius * SalingZoom3;
      a1 := phi1;
      a2 := phi2-phi1;
      Canvas.DrawArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2, 1.0 );
    end;
  end;

begin
{ MetaLINE(    x1,   y1,    x2,     y2); }
  MetaLINE(   4.90,  9.97,  10.44,    5.20);
  MetaLINE(  28.50, 40.70,  28.50,   43.50);
  MetaLINE(   3.50, 13.00,   0.00,   13.00);
  MetaLINE(  11.48,  6.09,   3.50,   13.00);
  MetaLINE(   2.00,  0.00,   5.00,    0.00);
  MetaLINE(  26.00, 31.85,  26.00,   49.00);

  MetaLINE(  -4.85, 10.02, -10.44,    5.20);
  MetaLINE(   0.00, 13.00,  -3.50,   13.00);
  MetaLINE(  -3.50, 13.00, -11.55,    6.04);
  MetaLINE( -28.50, 43.50, -28.50,   40.70);
  MetaLINE(  -5.00,  0.00,  -2.00,    0.00);
  MetaLINE( -26.00, 49.00, -26.00,   31.85);
{ MetaLINE(     0, 7200,     0,      0); }

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00);
  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58);

{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -11.00, 49.00,  15.00,  153.42, 180.00);
  MetaARC(  -11.00, 31.85,  15.00, -180.00, -163.29);
  MetaARC(   -3.50,  0.00,   1.50,    0.00,  132.53);
  MetaARC(    0.00,  4.30,   7.50,   90.00,  130.85);
  MetaARC(    0.00, 43.50,  28.50,   90.00, 180.00);
  MetaARC(    0.00, 43.50,  27.30,   90.00,  153.42);
  MetaARC(   18.50, 40.70,  45.80, -129.17, -120.16);
  MetaARC(   18.50, 40.70,  45.80, -163.29, -130.91);
  MetaARC(   18.50, 40.70,  47.00, -180.00, -120.00);
end;

procedure TMastProfile.InternalDrawProfile6(Canvas: TCanvas);

  procedure MetaLINE(x1, y1, x2, y2: single; cla: TAlphaColor);
  begin
    if WantSegmentColor then
      Canvas.Stroke.Color := cla;
    x1 := x1 * ProfileZoom + OffsetX;
    y1 := -y1 * ProfileZoom + OffsetY;
    x2 := x2 * ProfileZoom + OffsetX;
    y2 := -y2 * ProfileZoom + OffsetY;
    Canvas.DrawLine(PointF(x1, y1), PointF(x2, y2), ProfileOpacity);
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; cla: TAlphaColor);
  var
    a1, a2: single;
  begin
    if WantSegmentColor then
      Canvas.Stroke.Color := cla;
    xm := xm * ProfileZoom + OffsetX;
    ym := -ym * ProfileZoom + OffsetY;
    Radius := Radius * ProfileZoom;
    a1 := -phi1;
    a2 := -(phi2-phi1);
    Canvas.DrawArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2, ProfileOpacity);
  end;

begin

  if WantOuter then
  begin
  MetaLINE(   5.00,  0.00, 2.00, 0.00, claMagenta);
  MetaARC(  -18.50, 40.70, 47.00, -60.00, 0.00, claMagenta);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, claMagenta);
  MetaARC(    0.00, 43.50, 28.50, 0.00, 90.00, claMagenta);

  MetaARC(    0.00, 43.50, 28.50, 90.00, 180.00, claAqua);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, claAqua);
  MetaARC(   18.50, 40.70, 47.00, -180.00, -120.00, claAqua);
  MetaLINE(  -5.00,  0.00, -2.00, 0.00, claAqua);

  MetaARC(   -3.50,  0.00, 1.50, 0.00, 132.53, claLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, claLime);
  MetaLINE(  -4.85, 10.02, -10.44, 5.20, claLime);
  MetaARC(    0.00,  4.30, 7.50, 90.00, 130.85, claLime);

  MetaARC(    0.00,  4.30, 7.50, 49.15, 90.00, claAqua);
  MetaLINE(   4.90,  9.97, 10.44, 5.20, claAqua);
  MetaARC(  -18.50, 40.70, 45.80, -59.83, -50.83, claAqua);
  MetaARC(    3.50,  0.00, 1.50, 47.46, 180.00, claAqua);
  end;

  if WantInner then
  begin
  { right }
  MetaLINE(    0.0, 13.00, 3.50, 13.00, claBlue);
  MetaLINE(   3.50, 13.00, 11.48, 6.09, claBlue);
  MetaARC(  -18.50, 40.70, 45.80, -49.09,  -16.70, claBlue);
  MetaARC(   11.00, 31.85, 15.00, -16.71,    0.00, claBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, claBlue);
  MetaARC(   11.00, 49.00, 15.00, 0.00,   26.58, claBlue);
  MetaARC(    0.00, 43.50, 27.30, 26.58,   90.00, claBlue);

  { left }
  MetaARC(    0.00, 43.50, 27.30, 90.00,  153.42, claRed);
  MetaARC(  -11.00, 49.00, 15.00, 153.42,  180.00, claRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, claRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, claRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, claRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, claYellow);
  MetaLINE(  -3.50, 13.00, 0.00, 13.00, claYellow);
  end;

  if WantRight then
  begin
{ MetaLINE(     x1,    y1,     x2,      y2, cla); }
  MetaLINE(   4.90,  9.97,  10.44,    5.20, claRed);
  MetaLINE(  28.50, 40.70,  28.50,   43.50, claGreen);
  MetaLINE(   3.50, 13.00,   0.00,   13.00, claGreen);
  MetaLINE(  11.48,  6.09,   3.50,   13.00, claYellow);
  MetaLINE(   2.00,  0.00,   5.00,    0.00, claMagenta);
  MetaLINE(  26.00, 31.85,  26.00,   49.00, claCyan);
  end;

  if WantLeft then
  begin
  MetaLINE(  -4.85, 10.02, -10.44,    5.20, claRed);
  MetaLINE(   0.00, 13.00,  -3.50,   13.00, claGreen);
  MetaLINE(  -3.50, 13.00, -11.55,    6.04, claBlue);
  MetaLINE( -28.50, 43.50, -28.50,   40.70, claYellow);
  MetaLINE(  -5.00,  0.00,  -2.00,    0.00, claMagenta);
  MetaLINE( -26.00, 49.00, -26.00,   31.85, claCyan);
{ MetaLINE(     0,  72.00,   0.00,    0.00, cla); }
  end;

  if WantRight then
  begin
{ MetaARC(      xm,    ym,      r,    phi1,    phi2); }
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, claRed);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, claGreen);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00, claBlue);
  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, claYellow);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, claMagenta);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, claCyan);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, claLime);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, claOrange);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, claTeal);
  end;

  if WantLeft then
  begin
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
  MetaARC(  -11.00, 49.00,  15.00,  153.42, 180.00, claRed);
  MetaARC(  -11.00, 31.85,  15.00, -180.00, -163.29, claGreen);
  MetaARC(   -3.50,  0.00,   1.50,    0.00,  132.53, claBlue);
  MetaARC(    0.00,  4.30,   7.50,   90.00,  130.85, claYellow);
  MetaARC(    0.00, 43.50,  28.50,   90.00, 180.00, claMagenta);
  MetaARC(    0.00, 43.50,  27.30,   90.00,  153.42, claCyan);
  MetaARC(   18.50, 40.70,  45.80, -129.17, -120.16, claLime);
  MetaARC(   18.50, 40.70,  45.80, -163.29, -130.91, claOrange);
  MetaARC(   18.50, 40.70,  47.00, -180.00, -120.00, claTeal);
  end;
end;

procedure TMastProfile.ProfileDraw(g: TCanvas; lg: TLage; Opacity: single);
var
  ox: single;
  oy: single;
  zf: single;

  procedure MetaLINE(x1, y1, x2, y2: single; c: TAlphaColor);
  begin
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer: g.DrawLine(PointF(y1, x1), PointF(y2, x2), 1.0);
      Hoch: g.DrawLine(PointF(x1, y1), PointF(x2, y2), 1.0);
    end;
  end;

  procedure MetaARC(xm, ym, Radius: single; phi1, phi2: single; c: TAlphaColor);
  var
    a1, a2: single;
  begin
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := 90 - phi1;
      a2 := phi1-phi2;
      g.DrawArc(PointF(ym, xm), PointF(Radius, Radius), a1, a2, 1.0);
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1;
      a2 := phi2-phi1;
      g.DrawArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2, 1.0 );
    end;
  end;

begin
  g.Stroke.Cap := TStrokeCap.Round;
  g.Stroke.Join := TStrokeJoin.Round;
  ox := 0;
  oy := 0;
  zf := 1.0;

  { outer path clockwise }
  MetaLINE(   5.00,  0.00,  2.00,  0.00, claMagenta);
  MetaARC(  -18.50, 40.70,  47.00,  -60.00,    0.00, claMagenta);
  MetaLINE(  28.50, 40.70, 28.50, 43.50, claMagenta);
  MetaARC(    0.00, 43.50,  28.50,    0.00,   90.00, claMagenta);

  MetaARC(    0.00, 43.50, 28.50,   90.00,  180.00, claAqua);
  MetaLINE( -28.50, 43.50, -28.50, 40.70, claAqua);
  MetaARC(   18.50, 40.70, 47.00, -180.00, -120.00, claAqua);
  MetaLINE(  -5.00,  0.00,  -2.00,  0.00, claAqua);

  MetaARC(   -3.50,  0.00,  1.50,    0.00,  132.53, claLime);
  MetaARC(   18.50, 40.70, 45.80, -129.17, -120.16, claLime);
  MetaLINE(  -4.85, 10.02, -10.44,  5.20, claLime);
  MetaARC(    0.00,  4.30,  7.50,   90.00,  130.85, claLime);

  MetaARC(    0.00,  4.30,   7.50,   49.15,   90.00, claAqua);
  MetaLINE(   4.90,  9.97, 10.44,  5.20, claAqua);
  MetaARC(  -18.50, 40.70,  45.80,  -59.83,  -50.83, claAqua);
  MetaARC(    3.50,  0.00,   1.50,   47.46,  180.00, claAqua);

  { inner path right }
  MetaLINE(    0.0, 13.00,  3.50, 13.00, claBlue);
  MetaLINE(   3.50, 13.00, 11.48,  6.09, claBlue);
  MetaARC(  -18.50, 40.70,  45.80,  -49.09,  -16.70, claBlue);
  MetaARC(   11.00, 31.85,  15.00,  -16.71,    0.00, claBlue);
  MetaLINE(  26.00, 31.85, 26.00, 49.00, claBlue);
  MetaARC(   11.00, 49.00,  15.00,    0.00,   26.58, claBlue);
  MetaARC(    0.00, 43.50,  27.30,   26.58,   90.00, claBlue);

  { inner path left }
  MetaARC(    0.00, 43.50, 27.30,   90.00,  153.42, claRed);
  MetaARC(  -11.00, 49.00, 15.00,  153.42,  180.00, claRed);
  MetaLINE( -26.00, 49.00, -26.00, 31.85, claRed);
  MetaARC(  -11.00, 31.85, 15.00, -180.00, -163.29, claRed);
  MetaARC(   18.50, 40.70, 45.80, -163.29, -130.91, claRed);
  MetaLINE( -11.55,  6.04, -3.50, 13.00, claYellow);
  MetaLINE(  -3.50, 13.00,  0.00, 13.00, claYellow);

  { symmetry line }
//  MetaLINE(   0.00, 72.00,   0.00,  0.00, claRed);

{ MetaLINE(    x1,   y1,    x2,     y2); }
{ MetaARC(     xm,   ym,     r,    phi1,    phi2); }
end;

function TMastProfile.InitPathData(lg: TLage): TPathData;
var
  ox: single;
  oy: single;
  zf: single;
  pd: TPathData;

  procedure MoveTo(x1, y1: single);
  begin
    x1 := ox + x1 * zf;
    y1 := oy + y1 * zf;
    case lg of
      Quer: pd.MoveTo(PointF(y1, x1));
      Hoch: pd.MoveTo(PointF(x1, y1));
    end;
  end;

  procedure LineTo(x2, y2: single);
  begin
    x2 := ox + x2 * zf;
    y2 := oy + y2 * zf;
    case lg of
      Quer: pd.LineTo(PointF(y2, x2));
      Hoch: pd.LineTo(PointF(x2, y2));
    end;
  end;

  procedure AddArc(xm, ym, Radius, phi1, phi2: single);
  var
    a1, a2: single;
  begin
    if lg = Quer then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := 90 - phi1;
      a2 := phi1-phi2;
      pd.AddArc(PointF(ym, xm), PointF(Radius, Radius), a1, a2);
    end
    else if lg = Hoch then
    begin
      xm := ox + xm * zf;
      ym := oy + ym * Zf;
      Radius := Radius * zf;
      a1 := phi1;
      a2 := phi2-phi1;
      pd.AddArc(PointF(xm, ym), PointF(Radius, Radius), a1, a2);
    end;
  end;

begin
  ox := 0.0;
  oy := 0.0;
  zf := 1.0;
  pd := TPathData.Create;

  { outer path }
  MoveTo(   2.00,  0.00);
  LineTo(   5.00,  0.00);
  AddArc( -18.50, 40.70, 47.00, -60.00, 0.00);
  LineTo(  28.50, 43.50);
  AddArc(   0.00, 43.50, 28.50, 0.00, 90.00);

  AddArc(   0.00, 43.50, 28.50, 90.00, 180.00);
  LineTo( -28.50, 40.70);
  AddArc(  18.50, 40.70, 47.00, -180.00, -120.00);
  LineTo(  -2.00, 0.00);

  AddArc(  -3.50,  0.00, 1.50, 0.00, 132.53);
  AddArc(  18.50, 40.70, 45.80, -120.16, -129.17);
//  LineTo( -10.44, 5.20);
  AddArc(   0.00,  4.30, 7.50, 130.85, 90.00);

  AddArc(   0.00,  4.30, 7.50, 90.00, 49.15);
//  LineTo(  10.44, 5.20);
  AddArc( -18.50, 40.70, 45.80, -50.83, -59.83);
  AddArc(   3.50,  0.00, 1.50, 47.46,  180.00);

  { inner path }
  MoveTo(    0.0, 13.00);
  LineTo(   3.50, 13.00);

  LineTo(   3.50, 13.00);
  AddArc( -18.50, 40.70, 45.80, -49.09, -16.70);
  AddArc(  11.00, 31.85, 15.00, -16.71, 0.00);
  LineTo(  26.00, 49.00);
  AddArc(  11.00, 49.00, 15.00, 0.00, 26.58);
  AddArc(   0.00, 43.50, 27.30, 26.58, 90.00);

  AddArc(   0.00, 43.50, 27.30, 90.00, 153.42);
  AddArc( -11.00, 49.00, 15.00, 153.42, 180.00);
  LineTo( -26.00, 31.85);
  AddArc( -11.00, 31.85, 15.00, -180.00, -163.29);
  AddArc(  18.50, 40.70, 45.80, -163.29, -130.91);
  LineTo(  -3.50, 13.00);
  LineTo(   0.00, 13.00);

  { symmetry line }
//  MoveTo(   0.00,  0.00);
//  LineTo(   0.00, 72.00);

  { To avoid the edge being clipped
    when Path.Stroke.Thickness > 1.0
    and Path.WrapMode == TPathWrapMode.Fit;
    and Path.Stroke.Join == TStrokeJoin.Miter; }
//  MoveTo(   0.00,  75.00);

  result := pd;
end;

procedure TMastProfile.ProfilePath(g: TCanvas; lg: TLage; Opacity: single);
begin
  g.Stroke.Cap := TStrokeCap.Round;
  g.Stroke.Join := TStrokeJoin.Round;
  case lg of
    Hoch: g.DrawPath(FPathDataHoch, Opacity);
    Quer: g.DrawPath(FPathDataQuer, Opacity);
  end;
end;

end.
