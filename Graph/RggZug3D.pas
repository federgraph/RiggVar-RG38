unit RggZug3D;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math.Vectors,
  FMX.Graphics,
  RggTypes,
  RggZug;

type
  TZug3D = class(TZug3DBase)
  public
    procedure FillZug; override;
    procedure DrawToCanvas(g: TCanvas); override;
    procedure GetPlotList(ML: TStrings); override;
  end;

implementation

uses
  RiggVar.RG.Def;

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
    ZugWanteBb[0].x := xA0;
    ZugWanteBb[0].y := -yA0;
    ZugWanteBb[1].x := xA;
    ZugWanteBb[1].y := -yA;
    ZugWanteBb[2].x := xC;
    ZugWanteBb[2].y := -yC;

    { WanteStb }
    ZugWanteStb[0].x := xB0;
    ZugWanteStb[0].y := -yB0;
    ZugWanteStb[1].x := xB;
    ZugWanteStb[1].y := -yB;
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

end.
