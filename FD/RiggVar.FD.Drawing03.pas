unit RiggVar.FD.Drawing03;

interface

uses
  System.UIConsts,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing03 = class(TRggDrawing)
  public
    A0: TRggCircle;
    B0: TRggCircle;
    A: TRggCircle;
    B: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawing03 }

procedure TRggDrawing03.InitDefaultPos;
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

constructor TRggDrawing03.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := '03-Viergelenk';
  WantSort := False;

  A0 := TRggCircle.Create;
  A0.Caption := 'A0';
  A0.StrokeColor := claOrangered;

  B0 := TRggCircle.Create;
  B0.Caption := 'B0';
  B0.StrokeColor := claBlue;

  A := TRggCircle.Create;
  A.Caption := 'A';
  A.StrokeColor := claOrangered;

  B := TRggCircle.Create;
  B.Caption := 'B';
  B.StrokeColor := claBlue;

  InitDefaultPos;

  L := TRggLine.Create;
  L.Caption := 'A0B0';
  L.StrokeColor := claGray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'A0A';
  L.StrokeColor := claRed;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'B0B';
  L.StrokeColor := claBlue;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create;
  L.Caption := 'AB';
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  Add(A0);
  Add(B0);
  Add(A);
  Add(B);
end;

end.
