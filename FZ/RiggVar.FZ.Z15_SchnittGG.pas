unit RiggVar.FZ.Z15_SchnittGG;

interface

uses
  System.Math.Vectors,
  RiggVar.RG.Calc,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ15 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;
    S: TRggCircle;
    HT: TRggLabel;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ15 }

procedure TRggDrawingZ15.InitDefaultPos;
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

procedure TRggDrawingZ15.Compute;
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
  TRggCalc.SchnittGG(P1, P2, P3, P4, SP);

  S.Center.X := SP.X;
  S.Center.Y := SP.Z;
end;

constructor TRggDrawingZ15.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z15-SchnittGG';

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  Add(HT);

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Yellow;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Red;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Green;

  D := TRggCircle.Create('D');
  D.StrokeColor := TRggColors.Blue;

  S := TRggCircle.Create('S');
  S.StrokeColor := TRggColors.Coral;
  S.IsComputed := True;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Dodgerblue;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('CD');
  L.StrokeColor := TRggColors.Aquamarine;
  L.Point1 := C;
  L.Point2 := D;
  Add(L);

  Add(A);
  Add(B);
  Add(C);
  Add(D);
  Add(S);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;
  DefaultElement := D;
end;

function TRggDrawingZ15.GetHelpText: string;
begin
  ML.Add('SchnittGG = Schnitt Gerade Gerade = Intersection Line Line.');
  ML.Add('  2D drawing with 2 lines AB and CD.');
  ML.Add('');
  ML.Add('Points A, B, C and D are instances of the TRggCircle class.');
  ML.Add('  TRggLine instances reference two TRggCircle instances.');
  ML.Add('    Line AB references Circle A and Circle B.');
  ML.Add('');
  ML.Add('Schnittpunkt SP is computed with SchnittGG(P1, P2, P3, P4, SP);');
  ML.Add('  with P1.X := A.Center.X; and so on see code at GitHub.');
  ML.Add('');
  ML.Add('Intersection point S is a computed point (Circle S).');
  ML.Add('  Computed circle elements (--) cannot be selected/dragged.');

  result := ML.Text;
  ML.Clear;
end;

end.
