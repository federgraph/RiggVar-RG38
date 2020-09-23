unit RggCtrls;

interface

uses
  RiggVar.FD.Image,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects,
  RggTypes,
  RggProfile;

type
  TFigure = (
    dtTest,
    dtSalingAll,
    dtSalingDetail,
    dtController,
    dtProfileDrawHoch,
    dtProfileDrawQuer,
    dtProfilePathHoch,
    dtProfilePathQuer,
    dtProfileOuter,
    dtProfileInner,
    dtProfileLeft,
    dtProfileRight
  );

  TSalingGraph = class(TMastProfile)
  private
    { Fixed Width and Height }
    FWidth: Integer;
    FHeight: Integer;
    procedure ClearBackground(g: TCanvas);
  private
    FImage: TOriginalImage; // injected, not owned
    procedure InitBitmap;
    procedure SetImage(const Value: TOriginalImage);
  private
    SavedMatrix: TMatrix;
    NewMatrix: TMatrix;
    TempMatrix: TMatrix;
    procedure BeginTransform(g: TCanvas);
    procedure EndTransform(g: TCanvas);
  private
    procedure DrawToCanvas(g: TCanvas);
    procedure DrawTestFigure(g: TCanvas);
    procedure DrawSalingAll(Canvas: TCanvas);
    procedure DrawSalingDetail(Canvas: TCanvas);
    procedure DrawController(Canvas: TCanvas);
  private
    Figure: TFigure;
    DrawCounter: Integer;
    DebugColor: TAlphaColor;
    WantDebugColor: Boolean;
    WantTextRect: Boolean;
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  public
    BackgroundColor: TAlphaColor;
    ImageOpacity: single;

    ControllerTyp: TControllerTyp;

    EdgePos: Integer; { Abstand von E0 zur Anschlagkante Deck + Klotzdicke }
    ControllerPos: Integer; { Abstand(iP[ooE0,x], iP[ooE ,x]) in mm }
    ParamXE: single;  { Abstand(iP[ooD0,x], iP[ooE,x]) in mm }
    ParamXE0: Integer; { Abstand(iP[ooD0,x], iP[ooE0,x]) in mm }

    SalingA: Integer; { Abstand(iP[ooA,x], iP[ooB,x]) in mm }
    SalingH: Integer; { Abstand Verbindungslinie Salinge zu Hinterkante Mast in mm }
    SalingL: Integer; { Salinglänge in mm - außerhalb berechnen }
    SalingHOffset: Integer; { Abstand Hinterkante Mast zur neutrale Faser in mm }

    constructor Create;

    procedure Draw(f: TFigure);

    property Image: TOriginalImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TSalingGraph.Create;
begin
  inherited;

  FWidth := 453;
  FHeight := 220;

  BackgroundColor := claAntiquewhite;
  ImageOpacity := 1.0;

  ControllerTyp := ctDruck;

  { Properties für ControllerGraph in mm }
  EdgePos := 25;
  ControllerPos := 80;
  ParamXE := -20;
  ParamXE0 := 110;

  { Properties für SalingGraph in mm }
  SalingHOffset := 37;
  SalingH := 80;
  SalingA := 800;
  SalingL := 1000;
end;

procedure TSalingGraph.InitBitmap;
begin
  Image.Width := Width;
  Image.Height := Height;
end;

procedure TSalingGraph.SetImage(const Value: TOriginalImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TSalingGraph.DrawSalingAll(Canvas: TCanvas);
var
  SalingX: single;
  SalingY: single;
  R: TRectF;
  rw: single;
begin
  Lage := Hoch;

  Canvas.Stroke.Cap := TStrokeCap.Round;
  Canvas.Stroke.Join := TStrokeJoin.Round;

  { SalingH }
  SalingY := SalingH-SalingHOffset;
  Canvas.Stroke.Thickness := 10.0;
  Canvas.Stroke.Color := claFuchsia;
  Canvas.DrawLine(PointF(0, SalingY), PointF(0, 0), 1.0);

  { SalingA }
  SalingX := SalingA / 2;
  SalingY := (SalingH);
  Canvas.Stroke.Thickness := 10.0;
  Canvas.Stroke.Color := claBlue;
  Canvas.DrawLine(PointF(-SalingX, 0), PointF(SalingX, 0), 1.0);

  { SalingL }
  Canvas.Stroke.Thickness := 35.0;
  Canvas.Stroke.Color := TAlphaColors.Lightgreen;
  Canvas.DrawLine(PointF(SalingX, 0), PointF(0, SalingY), 1.0);
  Canvas.DrawLine(PointF(0, SalingY), PointF(-SalingX, 0), 1.0);

  { Wanten als Kreise }
  Canvas.Stroke.Thickness := 1.0;
  Canvas.Stroke.Color := claGray;
  Canvas.Fill.Color := claRed;
  rw := 8.0;
  R := RectF(-SalingX - rw, rw, -SalingX + rw, -rw);
  Canvas.DrawEllipse(R, 1.0);
  R := RectF(SalingX - rw, rw,  SalingX + rw, -rw);
  Canvas.DrawEllipse(R, 1.0);
  Canvas.Stroke.Color := claBlack;
  Canvas.Fill.Color := claRed;
  R := RectF(-rw, SalingY + rw,  rw, SalingY - rw);
  Canvas.DrawEllipse(R, 1.0);

  { Profilschnitt }
  OffsetX := 0;
  OffsetY := (SalingH-SalingHOffset);
  SalingZoom3 := 1.0;
  Canvas.Stroke.Thickness := 1.0;
  Canvas.Stroke.Color := claBlue;
  InternalDrawProfile3(Canvas);
end;

procedure TSalingGraph.DrawSalingDetail(Canvas: TCanvas);
var
  SalingX, SalingY: single;
  PosX, PosY: single;
  s: string;
  LineToPoint: TPointF;
  R: TRectF;
  rw: single;

  oy: single;
  th: single;

  procedure MoveToLineTo(x1, y1, x2, y2: single);
  begin
    LineToPoint := PointF(x2, oy + y2);
    Canvas.DrawLine(PointF(x1, oy + y1), LineToPoint, 1.0);
  end;

  procedure LineTo(x2, y2: single);
  begin
    Canvas.DrawLine(LineToPoint, PointF(x2, oy + y2), 1.0);
    LineToPoint := PointF(x2, oy + y2);
  end;

  procedure Ellipse(x1, y1, x2, y2: single);
  begin
    R := RectF(x1, oy + y1, x2, oy + y2);
    Canvas.DrawEllipse(R, 1.0);
  end;

  procedure FillEllipse(x1, y1, x2, y2: single);
  begin
    R := RectF(x1, oy + y1, x2, oy + y2);
    Canvas.FillEllipse(R, 1.0);
  end;

  procedure TextOut(x, y: single; s: string; ha, va: TTextAlign);
  begin
    R := RectF(x, y, x + 450, y + 60);
    if WantTextRect then
      Canvas.DrawRect(R, 0, 0, [], 1.0);
    Canvas.FillText(
      R,
      s,
      false, // WordWrap
      1.0, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

begin
  oy := 350;

  Canvas.Stroke.Cap := TStrokeCap.Round;
  Canvas.Stroke.Join := TStrokeJoin.Round;

  { SalingL }
  Canvas.Stroke.Thickness := 25.0;
  Canvas.Stroke.Color := TAlphaColors.Lightgreen;
  MoveToLineTo(-SalingA / 2, 0, 0, -SalingH);
  LineTo(SalingA / 2, 0);

  { SalingH }
  Canvas.Stroke.Thickness := 5.0;
  Canvas.Stroke.Color := claNavy;
  MoveToLineTo( 0, -SalingH, 0, 0);

  { SalingH - SalingHOffset }
  SalingY := SalingH - SalingHOffset;
  Canvas.Stroke.Thickness := 5.0;
  Canvas.Stroke.Color := claFuchsia;
  MoveToLineTo( -10, -SalingY, -10, 0);

  { SalingA }
  SalingX := (SalingA * 0.5);
  SalingY := SalingH;
  Canvas.Stroke.Thickness := 5.0;
  Canvas.Stroke.Color := claBlue;
  MoveToLineTo(-SalingX, 0, SalingX, 0);

  { Wanten als Kreise }
  rw := 10.0;
  Canvas.Stroke.Thickness := 1.0;
  Canvas.Stroke.Color := claGray;
  Canvas.Fill.Color := claRed;
  Ellipse(-SalingX - rw, -rw, -SalingX + rw, rw);
  Ellipse( SalingX - rw, -rw,  SalingX + rw, rw);
  rw := 10.0;
  Canvas.Fill.Color := claRed;
  FillEllipse( -rw, -SalingY - rw, rw, -SalingY + rw);

  { Profilschnitt }
  if True then
  begin
    Canvas.Stroke.Thickness := 5.0;
    Canvas.Stroke.Color := claAqua;
    Canvas.Fill.Color := claSilver;
    WantSegmentColor := False;
    WantRight := False;
    WantLeft := False;
    WantOuter := True;
    WantInner := False;
    OffsetX := 0;
    OffsetY := (oy - SalingH + SalingHOffset);
    ProfileZoom := 1.0;
    ProfileOpacity := 0.5;
    InternalDrawProfile6(Canvas);
  end;

  { Text }
  Canvas.Font.Size := 40.0;
  th := 80;

  Canvas.Fill.Color := claYellow;
  PosX := 50;
  PosY := 50;
  s := Format('SHO = %d mm',[SalingHOffset]);
  TextOut(PosX, PosY, s, TTextAlign.Leading, TTextAlign.Leading);

  Canvas.Fill.Color := claLime;
  PosX := -480;
  PosY := oy - 3 * th;
  s := Format('SalingL = %d mm',[SalingL]);
  TextOut(PosX, PosY, s, TTextAlign.Center, TTextAlign.Leading);

  Canvas.Fill.Color := claAqua;
  PosX := 50;
  PosY := oy - 1.5 * th;
  s := Format('SalingH = %d mm',[SalingH]);
  TextOut(PosX, PosY, s, TTextAlign.Leading, TTextAlign.Leading);

  Canvas.Fill.Color := claFuchsia;
  PosX := -480;
  PosY := oy - 1.5 * th;
  s := Format('SalingH - SHO = %d mm',[SalingH - SalingHOffset]);
  TextOut(PosX, PosY, s, TTextAlign.Leading, TTextAlign.Leading);

  Canvas.Fill.Color := claWhite;
  PosX := -300;
  PosY := oy + 10;
  s := Format('SalingA = %d mm',[SalingA]);
  TextOut(PosX, PosY, s, TTextAlign.Center, TTextAlign.Leading);
end;

procedure TSalingGraph.DrawController(Canvas: TCanvas);
var
  R: TRectF;
  i: Integer;
  KlotzX1: single;
  KlotzX2: single;
  PosXE0: single;
  StrichX: single;
  PositionXE0: single;
  PositionXE: single;
  ProfilPosMastfuss: single;
  ProfilPosXE: single;
  EdgePosition: single;
  claDeck: TAlphaColor;

  s: string;

  procedure TextOut(x, y: single; s: string; ha, va: TTextAlign);
  begin
    R := RectF(x - 45, y, x + 45, y + 9);
    if WantTextRect then
      Canvas.DrawRect(R, 0, 0, [], 1.0);
    Canvas.FillText(
      R,
      s,
      false, // WordWrap
      1.0, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

  procedure SetFillColor(cla: TAlphaColor);
  begin
    if WantDebugColor then
      Canvas.Fill.Color := DebugColor
    else
      Canvas.Fill.Color := cla;
  end;

begin
  WantDebugColor := False;
  Canvas.Font.Size := 5.0;

  PositionXE0 := 95; { Position der Ablesemarke, Konstante in der Grafik }
  PositionXE := PositionXE0 - ControllerPos; { Position linke Kante Mastklotz }
  ProfilPosMastfuss := PositionXE0 - ParamXE0 - 72; { Position Hinterkante Mastfuss }
  ProfilPosXE := ProfilPosMastfuss + ParamXE; { Position Hinterkante Mast in Höhe Punkt E }
  EdgePosition := PositionXE0-EdgePos + 15; { Abstand Deckanschlag - E0 }

  OffsetX := 0;
  Lage := Quer;

  claDeck := claCornflowerblue;

  { new window extension: 2 * 100 = -100..100 }

  { Profil Grau - Mastfuß }
  OffsetY := ProfilPosMastfuss; { OffsetY entspricht OffsetX, da gedreht }
  Canvas.Stroke.Thickness := 0.6;
  Canvas.Stroke.Color := claGray;
  InternalDrawProfile3(Canvas);

  { Deck }
  Canvas.Stroke.Thickness := 1.0;
  Canvas.Stroke.Color := claBlack;

  { Deck Seite Stb }
  DebugColor := claGreen;
  SetFillColor(claDeck);
  R := RectF(-100, -80, 85, -32);
  Canvas.FillRect(R, 20, 20, AllCorners, 1.0);

  { Deck Seite Bbd }
  DebugColor := claRed;
  SetFillColor(claDeck);
  R := RectF(-100, 80, 85, 32);
  Canvas.FillRect(R, 20, -20, AllCorners, 1.0);

  { Deck vorn }
  KlotzX1 := EdgePosition - 10 - 5; // EdgePosition - Radius, at least
  KlotzX2 := 100; // or bigger
  R := RectF(KlotzX1, -80, KlotzX2,  80);
  DebugColor := claLime;
  SetFillColor(claDeck);
  Canvas.FillRect(R, 0, 0, [], 1.0);

  { rechter Klotz mit Rundung im Deckausschnitt }
  KlotzX2 := EdgePosition; // Kante Deck vorn !
  KlotzX1 := KlotzX2 - 50; //does not matter much, should just be wide enough
  DebugColor := claBeige;
  SetFillColor(BackgroundColor);
  R := RectF(KlotzX1, -32, KlotzX2,  32);
  Canvas.FillRect(R, 10, 10, AllCorners, 1.0);

  { Profil Blau - in Höhe E }
  OffsetY := ProfilPosXE;
  Canvas.Stroke.Thickness := 1.0;
  Canvas.Stroke.Color := claAqua;
  InternalDrawProfile3(Canvas);

  { Only show Controller when used }
  if ControllerTyp <> ctOhne then
  begin
    { linker Klotz }
    KlotzX1 := PositionXE;
    KlotzX2 := KlotzX1 + 15;
    R := RectF(KlotzX1, -40, KlotzX2,  40);
    Canvas.Stroke.Thickness := 2.0;
    Canvas.Stroke.Color := claGray;
    Canvas.Fill.Color := claAqua;
    Canvas.DrawRect(R, 0,0, [], 1.0);
    Canvas.FillRect(R, 0,0, [], 1.0);

    { Maßband Hintergrund }
    PosXE0 := PositionXE0;
    R := RectF(KlotzX1, -7, PosXE0 + 10, 7);
    Canvas.Stroke.Thickness := 2.0;
    Canvas.Stroke.Color := claRed;
    Canvas.DrawRect(R, 0,0, [], 1.0);
    Canvas.Fill.Color := claGray;
    Canvas.FillRect(R, 0,0, [], 1.0);

    { Teilung }
    StrichX := KlotzX1;
    Canvas.Stroke.Thickness := 1.0;
    Canvas.Stroke.Color := claWhite;
    for i := 1 to 20 do
    begin
      StrichX := StrichX + 10;
      Canvas.DrawLine(PointF(StrichX, -5), PointF(StrichX, 5), 1.0);
    end;

    { Hintergrund für Teilung Text}
    PosXE0 := PositionXE0;
    R := RectF(KlotzX1, -3.5, PosXE0 + 10, 3.5);
    Canvas.Fill.Color := claGray;
    Canvas.FillRect(R, 0,0, [], 1.0);

    { Ablesemarke (Rechteck) an Stelle EO }
    Canvas.Stroke.Thickness := 1.0;
    Canvas.Stroke.Color := claYellow;
    R := RectF(PosXE0-2.5, -11, PosXE0 + 2.5, 11);
    Canvas.DrawRect(R, 0, 0, [], 1.0);

    { Teilung Text }
    WantTextRect := False;
    Canvas.Stroke.Thickness := 0.2;
    Canvas.Stroke.Color := claBlack;
    Canvas.Fill.Color := claYellow;
    StrichX := KlotzX1;
    for i := 1 to 20 do
    begin
      s := IntToStr(i);
      StrichX := StrichX + 10;
      TextOut(StrichX, -3.5, s, TTextAlign.Center, TTextAlign.Leading);
    end;

    { Ablesemarke Text }
    Canvas.Stroke.Thickness := 5.0;
    Canvas.Stroke.Color := claGray;
    Canvas.Fill.Color := claYellow;
    WantTextRect := False;
    TextOut(PosXE0, -22, 'E0', TTextAlign.Center, TTextAlign.Leading);
    TextOut(50, 34, 'Ablesemarke an Position E0 + Offset', TTextAlign.Center, TTextAlign.Leading);
  end;
end;

procedure TSalingGraph.DrawTestFigure(g: TCanvas);
var
  P1, P2: TPointF;
  x, y: single;
  A, B: TPointF;
  R: TRectF;
  Corners: TCorners;
  CornerRadius: single;
begin
  P1 := PointF(0, 0);
  P2 := PointF(100, 100);

  g.Stroke.Thickness := 2.0;
  g.Stroke.Color := claCrimson;
  g.DrawLine(P1, P2, 1.0);


  x := 200;
  y := 200;
  A := PointF(-x, -y);
  B := PointF(x, y);
  R := TRectF.Create(A, B);


  CornerRadius := 50;
  Corners := [
    TCorner.TopLeft,
    TCorner.TopRight,
    TCorner.BottomLeft,
    TCorner.BottomRight
  ];
  g.Fill.Color := claAqua;
  g.DrawRect(R, 30, 10, [TCorner.BottomRight], 1.0, TCornerType.Bevel);
  g.FillRect(R, CornerRadius, CornerRadius, Corners, 0.2, TCornerType.Bevel);
end;

procedure TSalingGraph.BeginTransform(g: TCanvas);
var
  w: single;
  h: single;
  Extent: single;
  NewExtent: single;
  PaintboxZoom: single;
  OriginX: single;
  OriginY: single;
  Zoom: single;
begin
  w := Width;
  h := Height;

  OriginX := 0.5 * w;
  OriginY := 0.5 * h;

  Extent := 72; // Profile = 72 mm hoch

  case Figure of
    dtTest:
    begin
      NewExtent := 2 * 500;
      OriginY := -250;
    end;

    dtProfileOuter:
    begin
      NewExtent := w;
      OriginY := h;
    end;

    dtProfileDrawHoch:
    begin
      Zoom := 0.9;
      NewExtent := Extent * w / h;
      NewExtent := NewExtent / Zoom;
      OriginY := 0;
    end;

    dtProfileDrawQuer:
    begin
      Zoom := 0.5;
      NewExtent := Extent / Zoom;
      OriginX := 0.25 * w;
    end;

    dtProfilePathHoch:
    begin
      NewExtent := Extent * w / h;
      OriginY := 0;
    end;

    dtProfilePathQuer:
    begin
      Zoom := 0.5;
      NewExtent := Extent / Zoom;
    end;

    dtSalingDetail:
    begin
      NewExtent := 2 * 500;
      OriginY := 0;
    end;

    dtSalingAll:
    begin
      NewExtent := 2 * 500;
      OriginY := 20;
    end;

    dtController:
    begin
      NewExtent := 2 * 100;
      OriginY := 0.25 * w;
    end;

    else
    begin
      NewExtent := w;
      OriginY := 0;
    end;
  end;

  PaintboxZoom := w / NewExtent;

  SavedMatrix := g.Matrix;
  NewMatrix := TMatrix.Identity;

  TempMatrix := TMatrix.CreateScaling(PaintboxZoom * Main.Scale, PaintboxZoom * Main.Scale);
  NewMatrix := NewMatrix * TempMatrix;
  TempMatrix := TMatrix.CreateTranslation(OriginX * Main.Scale, OriginY * Main.Scale);
  NewMatrix := NewMatrix * TempMatrix;

  g.SetMatrix(NewMatrix);
end;

procedure TSalingGraph.EndTransform(g: TCanvas);
begin
  g.SetMatrix(SavedMatrix);
end;

procedure TSalingGraph.DrawToCanvas(g: TCanvas);
begin
  Inc(DrawCounter);
  if g.BeginScene then
  try
    BeginTransform(g);
    try
      ClearBackground(g);
      case Figure of
        dtTest: DrawTestFigure(g);
        dtSalingDetail: DrawSalingDetail(g);
        dtSalingAll: DrawSalingAll(g);
        dtController: DrawController(g);

        dtProfileOuter:
        begin
          g.Stroke.Thickness := 1.0;
          g.Stroke.Color := claRed;
          g.Fill.Color := claSilver;
          WantSegmentColor := False;
          WantRight := False;
          WantLeft := False;
          WantOuter := True;
          WantInner := False;
          OffsetX := 0;
          OffsetY := -10;
          ProfileZoom := 2.5;
          ProfileOpacity := 1.0;
          InternalDrawProfile6(g);
        end;
        dtProfileInner: ;
        dtProfileLeft: ;
        dtProfileRight: ;

        dtProfileDrawHoch:
        begin
          g.Stroke.Thickness := 1.0;
          g.Stroke.Color := claGreen;
          ProfileDraw(g, Hoch, 0.5);
        end;
        dtProfileDrawQuer:
        begin
          g.Stroke.Thickness := 1.0;
          g.Stroke.Color := TAlphaColors.Cornflowerblue;
          ProfileDraw(g, Quer, 1.0);
        end;
        dtProfilePathHoch:
        begin
          g.Stroke.Thickness := 0.5;
          g.Stroke.Color := claCrimson;
          ProfilePath(g, Hoch, 1.0);
        end;
        dtProfilePathQuer:
        begin
          g.Stroke.Thickness := 0.5;
          g.Stroke.Color := claOrange;
          ProfilePath(g, Quer, 1.0);
        end;
      end;
    finally
      EndTransform(g);
    end;
  finally
    g.EndScene;
  end;
end;

procedure TSalingGraph.Draw(f: TFigure);
begin
  if (Image <> nil) then
  begin
    Figure := f;
    DrawToCanvas(Image.Bitmap.Canvas);
  end;
end;

procedure TSalingGraph.ClearBackground(g: TCanvas);
var
  R: TRectF;
begin
  g.Clear(claNull);
  if Image = nil then
  begin
    g.Clear(BackgroundColor);
  end
  else
  begin
    R := RectF(0, 0, Width, Height);
    g.Fill.Color := BackgroundColor;
    g.FillRect(R, 0, 0, [], ImageOpacity);
  end;
end;

end.

