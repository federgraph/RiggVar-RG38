unit RiggVar.FD.Drawing05;

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

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  System.Math.Vectors,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingD05 = class(TRggDrawing)
  public
    A0, A: TRggCircle;
    B0, B: TRggCircle;
    C0, C: TRggCircle;
    D0, D: TRggCircle;
    F: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure Load;
    procedure UpdateFromRigg;
  end;

implementation

uses
  RggTypes,
  RggTestData;

{ TRggDrawingD05 }

procedure TRggDrawingD05.InitDefaultPos;
var
  ox, oy, g: single;
begin
  ox := 100;
  oy := 700;
  g := 3.0;

  A0.Center.X := ox + 30 * g;
  A0.Center.Y := oy - 40 * g;
  A0.Center.Z := -40 * g;

  B0.Center.X := ox + 30 * g;
  B0.Center.Y := oy - 40 * g;
  B0.Center.Z := 40 * g;

  C0.Center.X := ox + 150 * g;
  C0.Center.Y := oy - 40 * g;
  C0.Center.z := 0;

  D0.Center.X := ox + 80 * g;
  D0.Center.Y := oy - 10 * g;
  D0.Center.z := 0;

  A.Center.X := ox + 10 * g;
  A.Center.Y := oy - 100 * g;
  A.Center.Z := -30 * g;

  B.Center.X := ox + 10 * g;
  B.Center.Y := oy - 100 * g;
  B.Center.Z := 30 * g;

  C.Center.X := ox + 30 * g;
  C.Center.Y := oy - 160 * g;
  C.Center.z := 0;

  D.Center.X := ox + 50 * g;
  D.Center.Y := oy - 100 * g;
  D.Center.Z := 0;

  F.Center.X := ox + 10 * g;
  F.Center.Y := oy - 220 * g;
  F.Center.Z := 0;
end;

constructor TRggDrawingD05.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D05-Test-Rigg';

  DefaultShowCaption := True;

  { Points }

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := TRggColors.Red;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := TRggColors.Green;

  C0 := TRggCircle.Create('C0');
  C0.StrokeColor := TRggColors.Yellow;

  D0 := TRggCircle.Create('D0');
  D0.StrokeColor := TRggColors.Blue;

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Green;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Yellow;

  D := TRggCircle.Create('D');
  D.StrokeColor := TRggColors.Blue;

  F := TRggCircle.Create('F');
  F.StrokeColor := TRggColors.Gray;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('A0B0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);

  L := TRggLine.Create('A0C0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := A0;
  L.Point2 := C0;
  Add(L);

  L := TRggLine.Create('B0C0');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := B0;
  L.Point2 := C0;
  Add(L);

  { --- }

  L := TRggLine.Create('A0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := A0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create('B0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := B0;
  L.Point2 := D0;
  Add(L);

  L := TRggLine.Create('C0D0');
  L.StrokeColor := TRggColors.Black;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);

  { --- }

  L := TRggLine.Create('A0A');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);

  L := TRggLine.Create('B0B');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('C0C');
  L.StrokeColor := TRggColors.Yellow;
  L.Point1 := C0;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('D0D');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D0;
  L.Point2 := D;
  Add(L);

  { --- }

  L := TRggLine.Create('AC');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('BC');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);

  L := TRggLine.Create('DC');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := D;
  L.Point2 := C;
  Add(L);

  { --- }

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  L := TRggLine.Create('AD');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := D;
  Add(L);

  L := TRggLine.Create('BD');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := B;
  L.Point2 := D;
  Add(L);

  L := TRggLine.Create('CF');
  L.StrokeColor := TRggColors.Gray;
  L.Point1 := C;
  L.Point2 := F;
  Add(L);

  Add(A0);
  Add(B0);
  Add(C0);
  Add(D0);

  Add(A);
  Add(B);
  Add(C);
  Add(D);

  Add(F);

  FixPoint := D.Center.C;
  WantRotation := True;
  WantSort := True;

  Load;
end;

procedure TRggDrawingD05.Load;
var
  cr: TRggCircle;
begin
  try
    cr := Find('A0');
    cr.Center.X := 235.42;
    cr.Center.Y := 552.60;
    cr.Center.Z := -164.01;

    cr := Find('B0');
    cr.Center.X := 142.15;
    cr.Center.Y := 589.33;
    cr.Center.Z := 54.05;

    cr := Find('C0');
    cr.Center.X := 520.27;
    cr.Center.Y := 606.96;
    cr.Center.Z := 80.73;

    cr := Find('D0');
    cr.Center.X := 323.92;
    cr.Center.Y := 674.45;
    cr.Center.Z := -14.62;

    cr := Find('A');
    cr.Center.X := 174.48;
    cr.Center.Y := 374.22;
    cr.Center.Z := -127.01;

    cr := Find('B');
    cr.Center.X := 104.53;
    cr.Center.Y := 401.77;
    cr.Center.Z := 36.54;

    cr := Find('C');
    cr.Center.X := 200.72;
    cr.Center.Y := 217.03;
    cr.Center.Z := 9.74;

    cr := Find('D');
    cr.Center.X := 250.00;
    cr.Center.Y := 400.00;
    cr.Center.Z := 0.00;

    cr := Find('F');
    cr.Center.X := 151.44;
    cr.Center.Y := 34.07;
    cr.Center.Z := 19.49;
  except
  end;
end;

procedure TRggDrawingD05.UpdateFromRigg;
var
{$ifdef Rgg}
  Rigg: TRigg;
{$endif}
  rP: TRiggPoints;
  cr: TRggCircle;
  t, p, q: TPoint3D;
  s: string;
  f: single;

  procedure Temp(oo: TRiggPoint);
  begin
    p := rP.V[oo];
    t := p - q;
    s := KoordTexteXML[oo];
    cr := Find(s);
    cr.Center.X := 400 + t.X * f;
    cr.Center.Y := 700 - t.Z * f;
    cr.Center.Z := t.Y * f;
    cr.Save;
  end;
begin
  f := 1 / 12;
{$ifdef Rgg}
  Rigg := Main.Rigg;
  rP := Rigg.rP;
{$else}
  rP := TRggTestData.GetKoordinaten420;
{$endif}
  q := rP.V[ooD0];

  try
    Temp(ooA0);
    Temp(ooB0);
    Temp(ooC0);
    Temp(ooD0);
    Temp(ooA);
    Temp(ooB);
    Temp(ooC);
    Temp(ooD);
    Temp(ooF);
  except
  end;

  FixPoint := Find('D').Center.C;
end;

end.
