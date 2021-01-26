unit RiggVar.FZ.Z24_Template;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ24 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TRggCircle;
    HT: TRggLabel;
    constructor Create;
    procedure InitDefaultPos; override;
//    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ24 }

procedure TRggDrawingZ24.InitDefaultPos;
begin
  A.Center.X := 100;
  A.Center.Y := 100;
  A.Center.Z := 0;

  B.Center.X := 400;
  B.Center.Y := 100;
  B.Center.Z := 0;
end;

constructor TRggDrawingZ24.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z24-Template';
  WantSort := False;

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  HT.Position.Y := 500;
  Add(HT);

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Orangered;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  { Lines }

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  { Add points last so that they stay in front. }
  Add(A);
  Add(B);
end;

function TRggDrawingZ24.GetHelpText: string;
begin
  ML.Add('First select a drawing element, Circle or Line, and play with Params.');
  ML.Add('');
  ML.Add('Change Param 1 with mouse wheel.');
  ML.Add('Change Param 2 with Shift-Wheel');
  ML.Add('  Make sure the mouse is over the drawing when you wheel.');
  ML.Add('');
  ML.Add('A Circle element can be moved in x and y direction.');
  ML.Add('A Line element maps length and angle to params 1 and 2.');
  ML.Add('');
  ML.Add('Drag a Selected circle with the mouse.');
  ML.Add('');
  ML.Add('Press Esc key to reset the drawing.');
  ML.Add('  For other keyboard shortcuts see drawing Z10-Lager.');
  result := ML.Text;
  ML.Clear;
end;

end.
