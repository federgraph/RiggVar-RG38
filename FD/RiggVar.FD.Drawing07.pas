unit RiggVar.FD.Drawing07;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD07 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingD07 }

procedure TRggDrawingD07.InitDefaultPos;
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

constructor TRggDrawingD07.Create;
var
  L: TRggLine;
  T: TRggTriangle;
begin
  inherited;
  Name := 'D07-Triangle';

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TWebColors.Orangered;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TWebColors.Dodgerblue;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TWebColors.Aquamarine;

  InitDefaultPos;

  T := TRggTriangle.Create;
  T.Caption := 'ABC';
  T.StrokeColor := TWebColors.Cyan;
  T.Point1 := A;
  T.Point2 := B;
  T.Point3 := C;
  Add(T);

  L := TRggLine.Create('AB');
  L.StrokeColor := TWebColors.Aquamarine;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AC');
  L.StrokeColor := TWebColors.Dodgerblue;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('BC');
  L.StrokeColor := TWebColors.Plum;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
end;

end.
