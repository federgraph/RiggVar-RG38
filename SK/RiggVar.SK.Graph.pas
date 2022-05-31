unit RiggVar.SK.Graph;

interface

uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects;

type
  TClockImageParam = (
    fpAngle,
    fpElevation,
    fpStrokeThicknessCircle,
    fpStrokeThicknessFinger
  );

  TClockImage = class(TComponent)
  private
    FCenterPoint: TPointF;
    FRadius: single;
    FAngle: single;
    FElevation: single;
    FStrokeThicknessCircle: single;
    FStrokeThicknessFinger: single;
    FSimpleImp: Boolean;
    FGrowAroundCenter: Boolean;
    FStrokeColorCircle: TAlphaColor;
    FStrokeColorFinger: TAlphaColor;
    procedure SetRadius(const Value: single);
    procedure SetAngle(const Value: single);
    procedure SetCenterPoint(const Value: TPointF);
    procedure SetElevation(const Value: single);
    procedure SetSimpleImp(const Value: Boolean);
    procedure SetStrokeThicknessCircle(const Value: single);
    procedure SetStrokeThicknessFinger(const Value: single);
    procedure SetGrowAroundCenter(const Value: Boolean);
    procedure SetStrokeColorCircle(const Value: TAlphaColor);
    procedure SetStrokeColorFinger(const Value: TAlphaColor);
  protected
    PID180: single;
    InitOK: Boolean;
    Updated: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Update; virtual;
    procedure Draw; virtual;

    property CenterPoint: TPointF read FCenterPoint write SetCenterPoint;
    property Radius: single read FRadius write SetRadius;
    property Angle: single read FAngle write SetAngle;
    property Elevation: single read FElevation write SetElevation;

    property GrowAroundCenter: Boolean read FGrowAroundCenter write SetGrowAroundCenter;
    property SimpleImp: Boolean read FSimpleImp write SetSimpleImp;
    property StrokeColorCircle: TAlphaColor read FStrokeColorCircle write SetStrokeColorCircle;
    property StrokeColorFinger: TAlphaColor read FStrokeColorFinger write SetStrokeColorFinger;
    property StrokeThicknessCircle: single read FStrokeThicknessCircle write SetStrokeThicknessCircle;
    property StrokeThicknessFinger: single read FStrokeThicknessFinger write SetStrokeThicknessFinger;
  end;

  TClockGraph = class(TClockImage)
  protected
    P1: TPointF;
    P2: TPointF;

    R: single;

    STCircleH: single;
    STFingerH: single;
  public
    Width: Integer;
    Height: Integer;

    procedure InitComponents(AParent: TFmxObject); virtual;
    procedure HandleDelta(fp: TClockImageParam; Delta: single); virtual;
  end;


implementation

{ TClockImage }

constructor TClockImage.Create(AOwner: TComponent);
begin
  inherited;

  PID180 := PI / 180;

  CenterPoint := TPointF.Create(100, 100);
  Radius := 100.0;
  Angle := 45;
  GrowAroundCenter := True;

  StrokeThicknessCircle := 5;
  StrokeThicknessFinger := 5;
  StrokeColorCircle := claSlateblue;
  StrokeColorFinger := claCrimson;
end;

procedure TClockImage.SetAngle(const Value: single);
begin
  FAngle := Value;
  Updated := False;
end;

procedure TClockImage.SetCenterPoint(const Value: TPointF);
begin
  FCenterPoint := Value;
  Updated := False;
end;

procedure TClockImage.SetElevation(const Value: single);
begin
  if (Value >= -89) and (Value < 89) then
  begin
    FElevation := Value;
    Updated := False;
  end;
end;

procedure TClockImage.SetGrowAroundCenter(const Value: Boolean);
begin
  FGrowAroundCenter := Value;
end;

procedure TClockImage.SetRadius(const Value: single);
begin
  FRadius := Value;
  Updated := False;
end;

procedure TClockImage.SetSimpleImp(const Value: Boolean);
begin
  FSimpleImp := Value;
end;

procedure TClockImage.SetStrokeColorCircle(const Value: TAlphaColor);
begin
  FStrokeColorCircle := Value;
end;

procedure TClockImage.SetStrokeColorFinger(const Value: TAlphaColor);
begin
  FStrokeColorFinger := Value;
end;

procedure TClockImage.SetStrokeThicknessCircle(const Value: single);
begin
  FStrokeThicknessCircle := Value;
  Updated := False;
end;

procedure TClockImage.SetStrokeThicknessFinger(const Value: single);
begin
  FStrokeThicknessFinger := Value;
  Updated := False;
end;

procedure TClockImage.Update;
begin

end;

procedure TClockImage.Draw;
begin
  if not Updated then
    Update;
end;

{ TClockGraph }

procedure TClockGraph.InitComponents(AParent: TFmxObject);
begin
  InitOK := True;
  STFingerH := StrokeThicknessFinger / 2;
  STCircleH := StrokeThicknessCircle / 2;
end;

procedure TClockGraph.HandleDelta(fp: TClockImageParam; Delta: single);
begin
  case fp of
    fpAngle: Angle := Angle + Delta;
    fpElevation: Elevation := Elevation + Delta;
    fpStrokeThicknessCircle: StrokeThicknessCircle := StrokeThicknessCircle + Delta;
    fpStrokeThicknessFinger: StrokeThicknessFinger := StrokeThicknessFinger + Delta;
  end;
  Updated := False;
end;

end.
