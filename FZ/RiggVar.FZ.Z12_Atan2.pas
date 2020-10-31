unit RiggVar.FZ.Z12_Atan2;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Math,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ12 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    AB: TRggLine;
    AC: TRggLine;
    Figure: Integer;
    ResultLabel: TRggLabel;
    FigureString: string;
    HT: TRggLabel;
    constructor Create(AFigure: Integer = 0);
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

{ TRggDrawingZ12 }

procedure TRggDrawingZ12.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 400;
  oy := 400;

  A.Center.X := ox;
  A.Center.Y := oy;
  A.Center.Z := 0;

  B.Center.X := ox + 200;
  B.Center.Y := oy;
  B.Center.Z := 0;

  C.Center.X := ox + 200;
  C.Center.Y := oy - 200;
  C.Center.Z := 0;
end;

procedure TRggDrawingZ12.Compute;
var
  x, y, t: single;
  s: string;
begin
  x := C.Center.X - A.Center.X;
  y := C.Center.Y - A.Center.Y;

  case Figure of
    0: t := RadToDeg(arctan2(y, x));
    1:
    begin
      if abs(x) < 0.001 then
        t := 0
      else
        t := RadToDeg(arctan(y / x));
    end;

    else
      t := 0;
  end;

  s := Format('%s = %.2f', [FigureString, t]);

  ResultLabel.Text := s;
end;

constructor TRggDrawingZ12.Create(AFigure: Integer);
var
  L: TRggLine;
  W: TRggArc;
begin
  inherited Create;

  Figure := AFigure;
  case AFigure of
    0: Name := 'Z12-ArcTan2-Demo';
    1: Name := 'Z12-ArcTan-Demo';
    else
      Name := 'Z12-Figure-Demo';
  end;

  case AFigure of
    0: FigureString := 'arctan2';
    1: FigureString := 'arctan';
    else
      FigureString := 'Figure Value';
  end;

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  HT.Position.Y := 100;
  Add(HT);

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Yellow;
  A.IsComputed := True;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Red;
  B.IsComputed := True;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Green;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Dodgerblue;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);
  AB := L;

  L := TRggLine.Create('AC');
  L.StrokeColor := TRggColors.Dodgerblue;
  L.Point1 := A;
  L.Point2 := C;
  L.ShowCaption := True;
  Add(L);
  AC := L;

  W := TRggArc.Create('alpha');
  W.StrokeColor := TRggColors.Aquamarine;
  W.Point1 := A;
  W.Point2 := B;
  W.Point3 := C;
  Add(W);

  Add(A);
  Add(B);
  Add(C);

  ResultLabel := TRggLabel.Create;
  ResultLabel.Caption := 'RL';
  ResultLabel.Text := FigureString;
  ResultLabel.StrokeColor := TRggColors.Tomato;
  Add(ResultLabel);

  FixPoint3D := A.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := C;
end;

function TRggDrawingZ12.GetHelpText: string;
begin
  ML.Add('Atan and Atan2 samples.');
  ML.Add('  One file, one class - two samples.');
  ML.Add('');
  ML.Add('constructor Create(AFigure: Integer = 0);');

  result := ML.Text;
  ML.Clear;
end;

end.
