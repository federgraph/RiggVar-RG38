unit RiggVar.FZ.Z14_SplitF;

interface

uses
  System.UIConsts,
  System.Math.Vectors,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ14 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;

    C: TSchnittKKCircle;

    D: TRggCircle;
    E: TRggCircle;

    F: TRggCircle;
    F1: TRggCircle;

    AB: TRggLine;
    AC: TRggLine;
    BC: TRggLine;
    CF: TRggLine;

    Param: TRggParam;

    Alpha: TRggArc;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ14 }

procedure TRggDrawingZ14.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 100;
  oy := 600;

  A.Center.X := ox;
  A.Center.Y := oy;
  A.Center.Z := 0;

  B.Center.X := ox + 400;
  B.Center.Y := oy;
  B.Center.Z := 0;
end;

procedure TRggDrawingZ14.Compute;
var
  ff: single;
  t: single;
  ph, pf: TPoint3D;
begin
  C.Radius2 := C.Radius1;
  C.Compute;

  D.Center.C := A.Center.C + (B.Center.C - A.Center.C) / 2;

  ff := 0.75 * Param.Value / Param.OriginalValue;
  F.Center.C := C.Center.C + (D.Center.C - C.Center.C) * ff;
  E.Center.C := C.Center.C + (F.Center.C - C.Center.C) / 2;

  ph := C.Center.C - D.Center.C;
  pf := C.Center.C - F.Center.C;
  t := pf.Length / ph.Length / 2;

  if ff < 0 then
    F1.Center.C := C.Center.C - (B.Center.C - C.Center.C) * t
  else
    F1.Center.C := C.Center.C + (B.Center.C - C.Center.C) * t;
end;

constructor TRggDrawingZ14.Create;
var
  L: TRggLine;
  T: TRggTriangle;
begin
  inherited;
  Name := 'Z14-SplitF';

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := claPlum;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := claPlum;

  InitDefaultPos;

  C := TSchnittKKCircle.Create;
  C.Caption := 'C';
  C.Radius1 := 500;
  C.Radius2 := C.Radius1;
  C.StrokeColor := claAquamarine;

  D := TRggCircle.Create;
  D.Caption := 'D';
  D.ShowCaption := False;
  D.IsComputed := True;
  D.StrokeColor := claGray;

  E := TRggCircle.Create;
  E.Caption := 'e';
  E.ShowCaption := False;
  E.IsComputed := True;
  E.StrokeColor := claGray;

  F := TRggCircle.Create;
  F.Caption := 'f';
  F.ShowCaption := False;
  F.IsComputed := True;
  F.StrokeColor := claGray;

  F1 := TRggCircle.Create;
  F1.Caption := 'F1';
  F1.IsComputed := True;
  F1.StrokeColor := claGray;

  T := TRggTriangle.Create;
  T.Caption := 'F2';
  T.StrokeColor := claAqua;
  T.Point1 := F1;
  T.Point2 := E;
  T.Point3 := F;
  Add(T);

  AB := TRggLine.Create;
  L := AB;
  L.Caption := 'l2';
  L.StrokeColor := claAquamarine;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  AC := TRggLine.Create('AC');
  L := AC;
  L.ShowCaption := False;
  L.StrokeColor := claPlum;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  BC := TRggLine.Create;
  L := BC;
  L.Caption := 'l1';
  L.StrokeColor := claPlum;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'h';
  L.ShowCaption := False;
  L.StrokeThickness := 1.0;
  L.StrokeColor := claGray;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  CF := TRggLine.Create('F');
  L := CF;
  L.StrokeThickness := 4.0;
  L.StrokeColor := claDodgerblue;
  L.Point1 := C;
  L.Point2 := F;
  Add(L);

  Alpha := TRggArc.Create('Alpha');
  Alpha.Point1 := C;
  Alpha.Point2 := A;
  Alpha.Point3 := D;
  Add(Alpha);

  Param := TRggParam.Create;
  Param.Caption := 'Force F';
  Add(Param);

  C.L1 := AC;
  C.L2 := BC;

  Add(A);
  Add(B);
  Add(C);
  Add(D);
  Add(E);
  Add(F);
  Add(F1);

  DefaultElement := Param;
end;

end.
