unit RiggVar.FD.Drawing06;

interface

uses
  System.Types,
  System.SysUtils,
  System.Math.Vectors,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD06 = class(TRggDrawing)
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
  RggCalc;

{ TRggDrawingD06 }

procedure TRggDrawingD06.InitDefaultPos;
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

procedure TRggDrawingD06.Compute;
begin
  A.Center.Z := 0;
  B.Center.Z := 0;
  C.Center.Z := 0;
  D.Center.Z := 0;
  Compute2D;
end;

procedure TRggDrawingD06.Compute2D;
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

procedure TRggDrawingD06.Compute3D;
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

constructor TRggDrawingD06.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D06-Height';

  { Points }

  DefaultShowCaption := True;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Lime;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Aqua;

  D := TRggCircle.Create('D');
  D.ShowCaption := False;
  D.StrokeColor := TRggColors.Yellow;
  D.IsComputed := True;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := True;

  L := TRggLine.Create('a');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.White;
  L.StrokeThickness := 1;
  L.Point1 := C;
  L.Point2 := B;
  Add(L);
  SeiteA := L;

  L := TRggLine.Create('b');
  L.StrokeColor := TRggColors.Lime;
  L.StrokeThickness := 4;
  L.Point1 := C;
  L.Point2 := A;
  Add(L);
  SeiteB := L;

  L := TRggLine.Create('c');
  L.StrokeColor := TRggColors.Aqua;
  L.StrokeThickness := 4;
  L.Point1 := B;
  L.Point2 := A;
  Add(L);
  SeiteC := L;

  { --- }

  L := TRggLine.Create;
  L.Caption := '(k) * a';
  L.StrokeColor := TRggColors.Red;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  L := TRggLine.Create;
  L.Caption := '(1-k) * a';
  L.StrokeColor := TRggColors.Red;
  L.Point1 := D;
  L.Point2 := B;
  Add(L);

  { --- }

  L := TRggLine.Create('h');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := A;
  L.Point2 := D;
  Add(L);
  LineH := L;

  Add(A);
  Add(B);
  Add(C);
  Add(D);

  FixPoint3D := A.Center.C;
  WantRotation := True;
  WantSort := True;

  DefaultElement := A;
end;

end.
