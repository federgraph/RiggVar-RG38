unit RiggVar.FD.Drawing07;

interface

uses
  System.UIConsts,
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
  A.StrokeColor := claOrangered;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := claDodgerblue;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := claAquamarine;

  InitDefaultPos;

  T := TRggTriangle.Create;
  T.Caption := 'ABC';
  T.StrokeColor := claAqua;
  T.Point1 := A;
  T.Point2 := B;
  T.Point3 := C;
  Add(T);

  L := TRggLine.Create('AB');
  L.StrokeColor := claAquamarine;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AC');
  L.StrokeColor := claDodgerblue;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('BC');
  L.StrokeColor := claPlum;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
end;

end.
