unit RiggVar.FZ.Z15_SchnittGG;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.Math.Vectors,
  RggCalc,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing15 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;
    S: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawing15}

procedure TRggDrawing15.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 400;
  oy := 400;

  A.Center.X := ox - 200;
  A.Center.Y := oy;
  A.Center.Z := 0;

  B.Center.X := ox + 200;
  B.Center.Y := oy;
  B.Center.Z := 0;

  C.Center.X := ox;
  C.Center.Y := oy - 200;
  C.Center.Z := 0;

  D.Center.X := ox;
  D.Center.Y := oy + 200;
  D.Center.Z := 0;

  S.Center.C := TPoint3D.Zero;
end;

procedure TRggDrawing15.Compute;
var
  P1, P2: TPoint3D;
  P3, P4: TPoint3D;
  SP: TPoint3D;
begin
  P1.X := A.Center.X;
  P1.Z := A.Center.Y;
  P2.X := B.Center.X;
  P2.Z := B.Center.Y;

  P3.X := C.Center.X;
  P3.Z := C.Center.Y;
  P4.X := D.Center.X;
  P4.Z := D.Center.Y;

  { SchnittGG is using XZ }
  SchnittGG(P1, P2, P3, P4, SP);

  S.Center.X := SP.X;
  S.Center.Y := SP.Z;
end;

constructor TRggDrawing15.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := '15-SchnittGG';

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := claYellow;

  B := TRggCircle.Create('B');
  B.StrokeColor := claRed;

  C := TRggCircle.Create('C');
  C.StrokeColor := claGreen;

  D := TRggCircle.Create('D');
  D.StrokeColor := claBlue;

  S := TRggCircle.Create('S');
  S.StrokeColor := claCoral;
  S.IsComputed := True;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create;
  L.Caption := 'AB';
  L.StrokeColor := claDodgerblue;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'CD';
  L.StrokeColor := claAquamarine;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
  Add(D);
  Add(S);

  FixPoint := A.Center.C;
  WantRotation := False;
  WantSort := False;
  DefaultElement := D;
end;

end.
