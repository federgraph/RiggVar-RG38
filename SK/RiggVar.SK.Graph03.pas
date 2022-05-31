unit RiggVar.SK.Graph03;

interface

uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects,
  RiggVar.SK.Graph;

type
  TClockGraph03 = class(TClockGraph)
  private
    CircleRect: TRectF;
    Circle: TPath;
    Finger: TPath;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitComponents(AParent: TFmxObject); override;
    procedure Update; override;
    procedure Draw; override;
  end;


implementation

constructor TClockGraph03.Create(AOwner: TComponent);
begin
  inherited;
  StrokeThicknessFinger := 40;
  StrokeThicknessCircle := StrokeThicknessFinger;
end;

procedure TClockGraph03.InitComponents(AParent: TFmxObject);
begin
  inherited;

//  Update;

  Circle := TPath.Create(Self);
  Circle.Parent := AParent;
  Circle.Align := TAlignLayout.Client;
  Circle.WrapMode := TPathWrapMode.Original;
  Circle.Stroke.Color := claLime;
//  Circle.Stroke.Thickness := StrokeThicknessCircle;
  Circle.Stroke.Cap := TStrokeCap.Round;
  Circle.Fill.Color := claNull;

  Circle.Data.Clear;
  Circle.Data.MoveTo(P1);
  Circle.Data.AddEllipse(CircleRect);

  Finger := TPath.Create(Self);
  Finger.Parent := AParent;
  Finger.Align := TAlignLayout.Client;
  Finger.WrapMode := TPathWrapMode.Original;
  Finger.Stroke.Color := claFuchsia;
//  Finger.Stroke.Thickness := StrokeThicknessFinger;
  Finger.Stroke.Cap := TStrokeCap.Round;
  Finger.Fill.Color := claNull;

  Finger.Data.Clear;
  Finger.Data.MoveTo(P1);
  Finger.Data.LineTo(P2);
end;

procedure TClockGraph03.Update;
var
  RE: single;
begin
  Circle.Stroke.Thickness := StrokeThicknessCircle;
  Finger.Stroke.Thickness := StrokeThicknessFinger;

  Circle.Stroke.Color := StrokeColorCircle;
  Finger.Stroke.Color := StrokeColorFinger;

  STFingerH := StrokeThicknessFinger / 2;
  STCircleH := StrokeThicknessCircle / 2;

  RE := Radius * cos(Elevation * PID180);
  R := RE - STFingerH;

  P1.X := CenterPoint.X - STCircleH;
  P1.Y := CenterPoint.Y - STCircleH;

  CircleRect := TRectF.Create(
    P1.X - Radius,
    P1.Y - Radius,
    P1.X + Radius,
    P1.Y + Radius);

  P1.X := CenterPoint.X - STFingerH;
  P1.Y := CenterPoint.Y - STFingerH;

  P2.X := P1.X + R * cos(Angle * PID180);
  P2.Y := P1.Y + R * sin(Angle * PID180);
end;

procedure TClockGraph03.Draw;
begin
  if not InitOK then
    Exit;

  inherited;

  if InitOK then
  begin
    Circle.Data.Clear;
    Circle.Data.MoveTo(P1);
    Circle.Data.AddEllipse(CircleRect);

    Finger.Data.Clear;
    Finger.Data.MoveTo(P1);
    Finger.Data.LineTo(P2);
  end;
end;

end.

