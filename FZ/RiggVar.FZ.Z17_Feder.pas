unit RiggVar.FZ.Z17_Feder;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ17 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A: TRggCircle;
    B: TRggCircle;
    AB: TRggFederLine;
    HT: TRggLabel;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ17 }

procedure TRggDrawingZ17.InitDefaultPos;
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

constructor TRggDrawingZ17.Create;
begin
  inherited;
  Name := 'Z17-Feder';

  { Points }

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := TRggColors.Dodgerblue;

  AB := TRggFederLine.Create;
  AB.Caption := 'AB';
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

  Add(A);
  Add(B);
  Add(AB);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := B;
end;

function TRggDrawingZ17.GetHelpText: string;
begin
  ML.Add('Element captions can be toggled on or off.');
  ML.Add('- make sure button GSC is NOT pressed');
  ML.Add('- select a Circle element');
  ML.Add('- use key t to toggle caption visibility.');
  ML.Add('');
  ML.Add('If button GSC (Global Show Caption) is pressed');
  ML.Add('  then all element captions are shown.');
  ML.Add('');
  ML.Add('Element captions can be moved by scrolling the mouse wheel.');
  ML.Add('For a Circle:');
  ML.Add('- use Ctrl-Wheel: angle (Param 3)');
  ML.Add('- use Shift-Ctrl-Wheel: radius (Param 4)');
  ML.Add('');
  ML.Add('Moving caption text may be different for other elements.');

  result := ML.Text;
  ML.Clear;
end;

end.
