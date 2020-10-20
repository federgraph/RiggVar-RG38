unit RiggVar.FZ.Z17_Feder;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ17 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    AB: TRggFederLine;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ17 }

procedure TRggDrawingZ17.InitDefaultPos;
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

constructor TRggDrawingZ17.Create;
begin
  inherited;
  Name := 'Z17-Feder';

  { Points }

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Dodgerblue;

  AB := TRggFederLine.Create;
  AB.Caption := 'AB';
  AB.Point1 := A;
  AB.Point2 := B;
  AB.ShowCaption := False;
  AB.StrokeThickness := 2;
  AB.StrokeColor := TRggColors.Aquamarine;

  InitDefaultPos;

  Add(A);
  Add(B);
  Add(AB);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := B;
end;

end.
