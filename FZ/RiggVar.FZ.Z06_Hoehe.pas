unit RiggVar.FZ.Z06_Hoehe;

interface

uses
  System.Types,
  System.SysUtils,
  System.Math.Vectors,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ06 = class(TRggDrawing)
  private
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;

    SeiteA: TRggLine;
    SeiteB: TRggLine;
    SeiteC: TRggLine;

    LineH: TRggLine;
    HT: TRggLabel;
    function GetHelpText: string;
  protected
    procedure Compute2D;
    procedure Compute3D;
  public
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
    procedure GoDark; override;
    procedure GoLight; override;
  end;

implementation

uses
  RggCalc;

{ TRggDrawingZ06 }

procedure TRggDrawingZ06.InitDefaultPos;
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

procedure TRggDrawingZ06.Compute;
begin
  A.Center.Z := 0;
  B.Center.Z := 0;
  C.Center.Z := 0;
  D.Center.Z := 0;
  Compute2D;
end;

procedure TRggDrawingZ06.Compute2D;
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

procedure TRggDrawingZ06.Compute3D;
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

constructor TRggDrawingZ06.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z06-Hoehe';

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Red;
  HT.IsMemoLabel := True;
  Add(HT);

  { Points }

  DefaultShowCaption := True;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Aquamarine;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Dodgerblue;

  D := TRggCircle.Create('D');
  D.ShowCaption := False;
  D.StrokeColor := TRggColors.Yellow;
  D.IsComputed := True;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := True;

  L := TRggLine.Create('a');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Red;
  L.StrokeThickness := 1;
  L.Point1 := C;
  L.Point2 := B;
  Add(L);
  SeiteA := L;

  L := TRggLine.Create('b');
  L.StrokeColor := TRggColors.Aquamarine;
  L.StrokeThickness := 4;
  L.Point1 := C;
  L.Point2 := A;
  Add(L);
  SeiteB := L;

  L := TRggLine.Create('c');
  L.StrokeColor := TRggColors.Dodgerblue;
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

procedure TRggDrawingZ06.GoDark;
begin
  inherited;
  A.StrokeColor := TRggColors.Tomato;
  B.StrokeColor := TRggColors.Lime;
  C.StrokeColor := TRggColors.Dodgerblue;

  SeiteA.StrokeColor := TRggColors.Tomato;
  SeiteB.StrokeColor := TRggColors.Lime;
  SeiteC.StrokeColor := TRggColors.Dodgerblue;

  D.StrokeColor := TRggColors.Yellow;
  LineH.StrokeColor := TRggColors.Yellow;
end;

procedure TRggDrawingZ06.GoLight;
begin
  inherited;
  A.StrokeColor := TRggColors.Red;
  B.StrokeColor := TRggColors.Aquamarine;
  C.StrokeColor := TRggColors.Dodgerblue;

  SeiteA.StrokeColor := TRggColors.Red;
  SeiteB.StrokeColor := TRggColors.Aquamarine;
  SeiteC.StrokeColor := TRggColors.Dodgerblue;

  D.StrokeColor := TRggColors.Orange;
  LineH.StrokeColor := TRggColors.Black;
end;

function TRggDrawingZ06.GetHelpText: string;
begin
  ML.Add('function Hoehe(a, b, c: single; out k: single): single;');
  ML.Add('var');
  ML.Add('  t: single;');
  ML.Add('begin');
  ML.Add('  k := sqr(a) + sqr(b) - sqr(c);');
  ML.Add('  k := k / 2 / a / a;');
  ML.Add('  t := sqr(b) - sqr(k) * sqr(a);');
  ML.Add('  if t < 0.001 then');
  ML.Add('    result := 0');
  ML.Add('  else');
  ML.Add('    result := sqrt(t);');
  ML.Add('  if IsNan(result) then');
  ML.Add('    result := 0;');
  ML.Add('end;');

  result := ML.Text;
  ML.Clear;
end;

end.
