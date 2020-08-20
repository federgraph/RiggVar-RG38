unit RiggVar.FD.Drawing04;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.Generics.Collections,
  FMX.Graphics,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing04 = class(TRggDrawing)
  public
    A0: TRggCircle;
    B0: TRggCircle;
    C0: TRggCircle;
    D0: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawing04 }

procedure TRggDrawing04.InitDefaultPos;
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

constructor TRggDrawing04.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := '04-Tetrahedron';

  { Points }

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := claRed;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := claGreen;

  C0 := TRggCircle.Create;
  C0.Caption := 'C0';
  C0.StrokeColor := claYellow;

  D0 := TRggCircle.Create;
  D0.Caption := 'D0';
  D0.StrokeColor := claBlue;

  InitDefaultPos;

  { Lines }

  L := TRggLine.Create;
  L.Caption := 'A0B0';
  L.StrokeColor := claDodgerblue;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'A0C0';
  L.StrokeColor := claAquamarine;
  L.Point1 := A0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'B0C0';
  L.StrokeColor := claAntiquewhite;
  L.Point1 := B0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'A0D0';
  L.StrokeColor := claRed;
  L.Point1 := A0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'B0D0';
  L.StrokeColor := claGreen;
  L.Point1 := B0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'C0D0';
  L.StrokeColor := claYellow;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);

  Add(A0);
  Add(B0);
  Add(C0);
  Add(D0);

  FixPoint := D0.Center.C;
  WantRotation := True;
  WantSort := True;
end;

end.
