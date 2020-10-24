unit RiggVar.FZ.Z22_BigArc;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ22 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A: TRggCircle;
    B: TRggCircle;
    AB: TRggLine;

    Arc: TRggBigArc;

    HT: TRggLabel;

    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ22 }

procedure TRggDrawingZ22.InitDefaultPos;
var
  ox, oy, oz: single;
begin
  ox := 400;
  oy := 400;
  oz := 0;

  A.Center.X := ox - 200;
  A.Center.Y := oy;
  A.Center.Z := oz;

  B.Center.X := ox + 200;
  B.Center.Y := oy;
  B.Center.Z := oz;
end;

constructor TRggDrawingZ22.Create;
begin
  inherited;
  Name := 'Z22-BigArc';

  { Points }

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Dodgerblue;

  AB := TRggLine.Create('AB');
  AB.Point1 := A;
  AB.Point2 := B;
  AB.ShowCaption := False;
  AB.StrokeThickness := 2;
  AB.StrokeColor := TRggColors.Aquamarine;

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  Add(HT);

  InitDefaultPos;

  Arc := TRggBigArc.Create('C');
  Arc.Point1 := A;
  Arc.Point2 := B;
  Arc.StrokeThickness := 1;
  Arc.StrokeColor := TRggColors.Plum;
  Add(Arc);

  Add(A);
  Add(B);
  Add(AB);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := B;
end;

function TRggDrawingZ22.GetHelpText: string;
begin
  ML.Add('Many drawing elements have parameters.');
  ML.Add('  Param 1 can be changed via mouse wheel.');
  ML.Add('  Param 2 can be changed via shift mouse wheel.');
  ML.Add('');
  ML.Add('Params of Circle element are center coordinates x and y.');
  ML.Add('Params of Line element are length and angle.');
  ML.Add('');
  ML.Add('The circles are the "points" of the drawing.');
  ML.Add('Circle elements may be referenced by other elements.');
  ML.Add('  A selected circle element can be dragged with the mouse,');
  ML.Add('    if it is not a -- computed element.');
  ML.Add('');
  ML.Add('Each element has a caption.');
  ML.Add('  Element captions can be moved via params,');
  ML.Add('  and visibility of captions can be changed.');
  ML.Add('');
  ML.Add('Toggle layout with key v (vertical) and h.');

  result := ML.Text;
  ML.Clear;
end;

end.
