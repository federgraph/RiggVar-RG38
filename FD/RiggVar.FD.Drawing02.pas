unit RiggVar.FD.Drawing02;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

{$ifdef FPC}
  {$mode Delphi}
{$endif}

interface

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD02 = class(TRggDrawing)
  public
    D0, D: TRggCircle;
    P0, P: TRggCircle;
    C0, C: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingD02 }

procedure TRggDrawingD02.InitDefaultPos;
var
  ox, oy, f: single;
begin
  f := 40;
  ox := 50;
  oy := 750;

  D0.Center.X := ox + 8 * f;
  D0.Center.Y := oy - 1 * f;
  D0.Center.Z := 0;

  P0.Center.X := ox + 3 * f;
  P0.Center.Y := oy - 4 * f;
  P0.Center.Z := 0;

  C0.Center.X := ox +15 * f;
  C0.Center.Y := oy - 4 * f;
  C0.Center.Z := 0;

  D.Center.X := ox + 5 * f;
  D.Center.Y := oy - 10 * f;
  D.Center.Z := 0;

  P.Center.X := ox + 1 * f;
  P.Center.Y := oy - 10 * f;
  P.Center.Z := 0;

  C.Center.X := ox + 3 * f;
  C.Center.Y := oy - 16 * f;
  C.Center.Z := 0;
end;

constructor TRggDrawingD02.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D02-Logo';
  WantSort := False;

  D0 := TRggCircle.Create;
  D0.Caption := 'D0';
  D0.StrokeColor := TRggColors.Orangered;

  P0 := TRggCircle.Create;
  P0.Caption := 'P0';
  P0.StrokeColor := TRggColors.Blue;

  C0 := TRggCircle.Create;
  C0.Caption := 'C0';
  C0.StrokeColor := TRggColors.Blue;

  D := TRggCircle.Create;
  D.Caption := 'D';
  D.StrokeColor := TRggColors.Orangered;

  P := TRggCircle.Create;
  P.Caption := 'P';
  P.StrokeColor := TRggColors.Blue;

  C := TRggCircle.Create;
  C.Caption := 'C';
  C.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  L := TRggLine.Create('D0P0');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := D0;
  L.Point2 := P0;
  Add(L);

  L := TRggLine.Create('P0C0');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := P0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create('C0D0');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create('P0P');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.OrangeRed;
  L.Point1 := P0;
  L.Point2 := P;
  Add(L);

  L := TRggLine.Create('PC');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.OrangeRed;
  L.Point1 := P;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('C0C');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Orange;
  L.Point1 := C0;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('D0D');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D0;
  L.Point2 := D;
  Add(L);

  L := TRggLine.Create('DC');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('PD');
  L.ShowCaption := False;
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := P;
  L.Point2 := D;
  Add(L);

  Add(D0);
  Add(P0);
  Add(C0);
  Add(D);
  Add(P);
  Add(C);
end;

end.
