unit RiggVar.FD.Drawing06;

interface

uses
  System.SysUtils,
  System.UIConsts,
  System.Math.Vectors,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing06 = class(TRggDrawing)
  private
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;

    SeiteA: TRggLine;
    SeiteB: TRggLine;
    SeiteC: TRggLine;

    LineH: TRggLine;
  protected
    procedure Compute2D;
    procedure Compute3D;
  public
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

uses
  System.Types,
  RggCalc;

{ TRggDrawing06 }

procedure TRggDrawing06.InitDefaultPos;
begin
  A.Center.X := 300;
  A.Center.Y := 300;
  B.Center.X := 700;
  B.Center.Y := 500;
  C.Center.X := 100;
  C.Center.Y := 500;
  D.Center.X := 300;
  D.Center.Y := 500;
end;

procedure TRggDrawing06.Compute;
begin
  A.Center.Z := 0;
  B.Center.Z := 0;
  C.Center.Z := 0;
  D.Center.Z := 0;
  Compute2D;
end;

procedure TRggDrawing06.Compute2D;
var
  la, lb, lc: single;
  k: single;
  h: single;
  temp: TPointF;
begin
  la := SeiteA.V2.Length;
  lb := SeiteB.V2.Length;
  lc := SeiteC.V2.Length;

  h := Hoehe(la, lb, lc, k);
  LineH.Caption := Format('%s = %.2f', ['h', h]);

  temp := C.Center.P + k * SeiteA.V2;

  D.Center.X := temp.X;
  D.Center.Y := temp.Y;
end;

procedure TRggDrawing06.Compute3D;
var
  la, lb, lc: single;
  k: single;
  h: single;
begin
  la := SeiteA.V3.Length;
  lb := SeiteB.V3.Length;
  lc := SeiteC.V3.Length;

  h := Hoehe(la, lb, lc, k);
  LineH.Caption := Format('%s = %.2f', ['h', h]);

  D.Center.C := C.Center.C + k * SeiteA.V3;
end;

constructor TRggDrawing06.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := '06-Hoehe';

  { Points }

  DefaultShowCaption := True;

  A := TRggCircle.Create('A');
  A.StrokeColor := claRed;

  B := TRggCircle.Create('B');
  B.StrokeColor := claLime;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := claAqua;

  D := TRggCircle.Create('D');
  D.ShowCaption := False;
  D.StrokeColor := claYellow;
  D.IsComputed := True;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := True;

  L := TRggLine.Create('a');
  L.ShowCaption := False;
  L.StrokeColor := claWhite;
  L.StrokeThickness := 1;
  L.Point1 := C;
  L.Point2 := B;
  Add(L);
  SeiteA := L;

  L := TRggLine.Create('b');
  L.StrokeColor := claLime;
  L.StrokeThickness := 4;
  L.Point1 := C;
  L.Point2 := A;
  Add(L);
  SeiteB := L;

  L := TRggLine.Create('c');
  L.StrokeColor := claAqua;
  L.StrokeThickness := 4;
  L.Point1 := B;
  L.Point2 := A;
  Add(L);
  SeiteC := L;

  { --- }

  L := TRggLine.Create;
  L.Caption := '(k) * a';
  L.StrokeColor := claRed;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  L := TRggLine.Create;
  L.Caption := '(1-k) * a';
  L.StrokeColor := claRed;
  L.Point1 := D;
  L.Point2 := B;
  Add(L);

  { --- }

  L := TRggLine.Create('h');
  L.StrokeColor := claBlack;
  L.Point1 := A;
  L.Point2 := D;
  Add(L);
  LineH := L;

  Add(A);
  Add(B);
  Add(C);
  Add(D);

  FixPoint := A.Center.C;
  WantRotation := True;
  WantSort := True;
end;

end.
