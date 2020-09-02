unit RiggVar.FZ.Z08_Arc;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ08 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    Alpha: TRggArc;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ08 }

procedure TRggDrawingZ08.InitDefaultPos;
begin
  A.Center.X := 100;
  A.Center.Y := 400;
  A.Center.Z := 0;

  B.Center.X := 400;
  B.Center.Y := 400;
  B.Center.Z := 0;

  C.Center.X := 200;
  C.Center.Y := 200;
  C.Center.Z := 0;
end;

constructor TRggDrawingZ08.Create;
var
  L: TRggLine;
  T: TRggTriangle;
  W: TRggArc;
begin
  inherited;
  Name := 'Z08-Arc';

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Orangered;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Dodgerblue;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Aquamarine;

  InitDefaultPos;

  T := TRggTriangle.Create;
  T.Caption := 'ABC';
  T.StrokeColor := TRggColors.Aqua;
  T.Point1 := A;
  T.Point2 := B;
  T.Point3 := C;
  Add(T);

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AC');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('BC');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  W := TRggArc.Create('Alpha');
  W.StrokeColor := TRggColors.Red;
  W.Point1 := C;
  W.Point2 := B;
  W.Point3 := A;
  Add(W);

  W := TRggArc.Create('Beta');
  W.StrokeColor := TRggColors.Blue;
  W.Point1 := B;
  W.Point2 := C;
  W.Point3 := A;
  Add(W);

  W := TRggArc.Create('Gamma');
  W.StrokeColor := TRggColors.Green;
  W.Point1 := A;
  W.Point2 := B;
  W.Point3 := C;
  Add(W);

  Add(A);
  Add(B);
  Add(C);

  DefaultElement := C;
end;

end.
