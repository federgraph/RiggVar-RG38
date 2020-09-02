unit RiggVar.FZ.Z01_Viereck;

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ01 = class(TRggDrawing)
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TRggCircle;
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

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := TRggColors.Orangered;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := TRggColors.Blue;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Orangered;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

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

  Add(A0);
  Add(B0);
  Add(A);
  Add(B);
end;

end.
