unit RiggVar.FD.Drawing04;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD04 = class(TRggDrawing)
  public
    A0: TRggCircle;
    B0: TRggCircle;
    C0: TRggCircle;
    D0: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingD04 }

procedure TRggDrawingD04.InitDefaultPos;
begin
  A0.Center.X := 100;
  A0.Center.Y := 100;
  A0.Center.Z := -200;

  B0.Center.X := 100;
  B0.Center.Y := 100;
  B0.Center.Z := 200;

  C0.Center.X := 600;
  C0.Center.Y := 200;
  C0.Center.Z := 0;

  D0.Center.X := 200;
  D0.Center.Y := 400;
  D0.Center.Z := 0;
end;

constructor TRggDrawingD04.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D04-Tetrahedron';

  { Points }

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := TRggColors.Red;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := TRggColors.Green;

  C0 := TRggCircle.Create;
  C0.Caption := 'C0';
  C0.StrokeColor := TRggColors.Yellow;

  D0 := TRggCircle.Create;
  D0.Caption := 'D0';
  D0.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  { Lines }

  L := TRggLine.Create('A0B0');
  L.StrokeColor := TRggColors.Dodgerblue;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create('A0C0');
  L.StrokeColor := TRggColors.Aquamarine;
  L.Point1 := A0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create('B0C0');
  L.StrokeColor := TRggColors.Antiquewhite;
  L.Point1 := B0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create('A0D0');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create('B0D0');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := B0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create('C0D0');
  L.StrokeColor := TRggColors.Yellow;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);

  Add(A0);
  Add(B0);
  Add(C0);
  Add(D0);

  FixPoint3D := D0.Center.C;
  WantRotation := True;
  WantSort := True;
end;

end.
