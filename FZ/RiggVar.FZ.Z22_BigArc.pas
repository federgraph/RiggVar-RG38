unit RiggVar.FZ.Z22_BigArc;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ22 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    AB: TRggLine;

    Arc: TRggBigArc;

    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ22 }

procedure TRggDrawingZ22.InitDefaultPos;
var
  ox, oy, oz: single;
begin
  ox := 400;
  oy := 400;
  oz := 0;

  A.Center.X := ox - 200;
  A.Center.Y := oy;
  A.Center.Z := oz;

  B.Center.X := ox + 200;
  B.Center.Y := oy;
  B.Center.Z := oz;
end;

constructor TRggDrawingZ22.Create;
begin
  inherited;
  Name := 'Z22-BigArc';

  { Points }

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Dodgerblue;

  AB := TRggLine.Create('AB');
  AB.Point1 := A;
  AB.Point2 := B;
  AB.ShowCaption := False;
  AB.StrokeThickness := 2;
  AB.StrokeColor := TRggColors.Aquamarine;

  InitDefaultPos;

  Arc := TRggBigArc.Create('C');
  Arc.Point1 := A;
  Arc.Point2 := B;
  Arc.StrokeThickness := 1;
  Arc.StrokeColor := TRggColors.Plum;
  Add(Arc);

  Add(A);
  Add(B);
  Add(AB);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := B;
end;

end.
