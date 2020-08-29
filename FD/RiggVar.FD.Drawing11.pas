unit RiggVar.FD.Drawing11;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD11 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingD11}

procedure TRggDrawingD11.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 400;
  oy := 400;

  A.Center.X := ox + 100;
  A.Center.Y := oy;
  A.Center.Z := 0;

  B.Center.X := ox + 200;
  B.Center.Y := oy;
  B.Center.Z := 0;

  C.Center.X := ox;
  C.Center.Y := oy;
  C.Center.Z := 100;

  D.Center.X := ox + 100;
  D.Center.Y := oy + 100;
  D.Center.Z := 100;
end;

constructor TRggDrawingD11.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D11-Above';

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := claYellow;

  B := TRggCircle.Create('B');
  B.StrokeColor := claRed;

  C := TRggCircle.Create('C');
  C.StrokeColor := claGreen;

  D := TRggCircle.Create('D');
  D.StrokeColor := claBlue;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AB');
  L.StrokeColor := claDodgerblue;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('CD');
  L.StrokeColor := claAquamarine;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
  Add(D);

  FixPoint := A.Center.C;
  WantRotation := True;
  WantSort := True;
end;

end.
