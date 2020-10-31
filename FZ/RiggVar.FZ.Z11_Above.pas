unit RiggVar.FZ.Z11_Above;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ11 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;
    HT: TRggLabel;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ11 }

procedure TRggDrawingZ11.InitDefaultPos;
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

constructor TRggDrawingZ11.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z11-Above';

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

  FixPoint3D := A.Center.C;
  WantRotation := True;
  WantSort := True;
end;

function TRggDrawingZ11.GetHelpText: string;
begin
  ML.Add('Drawing with 3D rotations enabled:');
  ML.Add('  WantRotation := True;');
  ML.Add('  WantSort := True;');
  ML.Add('');
  ML.Add('You can rotate around 3 axis by dragging the mouse.');
  ML.Add('  with left mouse button down: x and y.');
  ML.Add('  with right mouse button down: z');
  ML.Add('');
  ML.Add('Lines will be drawn in sorted order - sorted by which is "above",');
  ML.Add('  this is not so straight forward as it seems to be,');
  ML.Add('    and this is one of the first tests that should be done.');
  ML.Add('');
  ML.Add('Lines detected as above should be drawn on top.');

  result := ML.Text;
  ML.Clear;
end;

end.
