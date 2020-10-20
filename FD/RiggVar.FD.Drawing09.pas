unit RiggVar.FD.Drawing09;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD09 = class(TRggDrawing)
  public
    Origin: TRggCircle;
    AX: TRggCircle;
    AY: TRggCircle;
    AZ: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingD09 }

procedure TRggDrawingD09.InitDefaultPos;
var
  ox, oy, oz: single;
begin
  ox := 400;
  oy := 400;
  oz := 0;

  Origin.Center.X := ox;
  Origin.Center.Y := oy;
  Origin.Center.Z := 0;

  AX.Center.X := ox + 200;
  AX.Center.Y := oy;
  AX.Center.Z := oz;

  AY.Center.X := ox;
  AY.Center.Y := oy + 200;
  AY.Center.Z := oz;

  AZ.Center.X := ox;
  AZ.Center.Y := oy;
  AZ.Center.Z := oz + 200;
end;

constructor TRggDrawingD09.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D09-Axis';

  { Points }

  AX := TRggCircle.Create;
  AX.Caption := 'X';
  AX.StrokeColor := TRggColors.Red;

  AY := TRggCircle.Create;
  AY.Caption := 'Y';
  AY.StrokeColor := TRggColors.Green;

  AZ := TRggCircle.Create;
  AZ.Caption := 'Z';
  AZ.StrokeColor := TRggColors.Blue;

  Origin := TRggCircle.Create;
  Origin.Caption := 'Origin';
  Origin.StrokeColor := TRggColors.Yellow;
  Origin.ShowCaption := False;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AX');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := Origin;
  L.Point2 := AX;
  Add(L);

  L := TRggLine.Create('AY');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := Origin;
  L.Point2 := AY;
  Add(L);

  L := TRggLine.Create('AZ');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := Origin;
  L.Point2 := AZ;
  Add(L);

  Add(Origin);
  Add(AX);
  Add(AY);
  Add(AZ);

  FixPoint3D := Origin.Center.C;
  WantRotation := True;
  WantSort := True;
end;

end.
