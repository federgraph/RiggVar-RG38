unit RiggVar.FZ.Z07_Triangle;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ07 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ07 }

procedure TRggDrawingZ07.InitDefaultPos;
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

constructor TRggDrawingZ07.Create;
var
  L: TRggLine;
  T: TRggTriangle;
begin
  inherited;
  Name := 'Z07-Triangle';

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
  L.StrokeColor := TRggColors.Aquamarine;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AC');
  L.StrokeColor := TRggColors.Dodgerblue;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('BC');
  L.StrokeColor := TRggColors.Plum;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
end;

end.
