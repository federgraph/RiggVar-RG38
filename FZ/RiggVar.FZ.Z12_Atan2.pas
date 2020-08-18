unit RiggVar.FZ.Z12_Atan2;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.Generics.Collections,
  System.UITypes,
  FMX.Graphics,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing12 = class(TRggDrawing)
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    AB: TRggLine;
    AC: TRggLine;
    Figure: Integer;
    ResultLabel: TRggLabel;
    FigureString: string;
    constructor Create(AFigure: Integer = 0);
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

uses
  Math;

{ TRggDrawing12 }

procedure TRggDrawing12.InitDefaultPos;
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

procedure TRggDrawing12.Compute;
var
  x, y, t: single;
  s: string;
begin
  x := C.Center.X - A.Center.X;
  y := C.Center.Y - A.Center.Y;

  case Figure of
    0: t := arctan2(y, x) * 180 / PI;
    1:
    begin
      if abs(x) < 0.001 then
        t := 0
      else
        t := arctan(y / x) * 180 / PI;
    end;

    else
      t := 0;
  end;

  s := Format('%s = %.2f', [FigureString, t]);

  ResultLabel.Text := s;
end;

constructor TRggDrawing12.Create(AFigure: Integer);
var
  L: TRggLine;
  W: TRggArc;
begin
  inherited Create;

  Figure := AFigure;
  case AFigure of
    0: Name := '12-ArcTan2-Demo';
    1: Name := '12-ArcTan-Demo';
    else
      Name := '12-Figure-Demo';
  end;

  case AFigure of
    0: FigureString := 'arctan2';
    1: FigureString := 'arctan';
    else
      FigureString := 'Figure Value';
  end;

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := claYellow;
  A.IsComputed := True;

  B := TRggCircle.Create('B');
  B.StrokeColor := claRed;
  B.IsComputed := True;

  C := TRggCircle.Create('C');
  C.StrokeColor := claGreen;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create;
  L.Caption := 'AB';
  L.StrokeColor := claDodgerblue;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);
  AB := L;

  L := TRggLine.Create;
  L.Caption := 'AC';
  L.StrokeColor := claDodgerblue;
  L.Point1 := A;
  L.Point2 := C;
  L.ShowCaption := True;
  Add(L);
  AC := L;

  W := TRggArc.Create('Alpha');
  W.Caption := 'alpha';
  W.StrokeColor := claAquamarine;
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
  ResultLabel.StrokeColor := TAlphaColors.Tomato;
  Add(ResultLabel);

  FixPoint := A.Center.C;
  WantRotation := False;
  WantSort := False;
end;

end.
