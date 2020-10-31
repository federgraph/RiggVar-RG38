unit RiggVar.FZ.Z01_Viereck;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ01 = class(TRggDrawing)
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
  end;

implementation

{ TRggDrawingZ01 }

procedure TRggDrawingZ01.InitDefaultPos;
begin
  A0.Center.X := 100;
  A0.Center.Y := 400;
  A0.Center.Z := 0;

  B0.Center.X := 400;
  B0.Center.Y := 400;
  B0.Center.Z := 0;

  A.Center.X := 100;
  A.Center.Y := 100;
  A.Center.Z := 0;

  B.Center.X := 400;
  B.Center.Y := 100;
  B.Center.Z := 0;
end;

constructor TRggDrawingZ01.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z01-Viereck';
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

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := TRggColors.Orangered;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := TRggColors.Blue;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Orangered;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  { Lines }

  L := TRggLine.Create('A0B0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create('A0A');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);

  L := TRggLine.Create('B0B');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  { Add points last so that they stay in front. }
  Add(A0);
  Add(B0);
  Add(A);
  Add(B);
end;

function TRggDrawingZ01.GetHelpText: string;
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
