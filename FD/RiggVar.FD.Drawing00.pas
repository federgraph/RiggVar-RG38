unit RiggVar.FD.Drawing00;

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
  RggTypes,
  System.UIConsts,
  System.Math.Vectors,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

{$define WantPoly}

type
  { This will be the Live drawing - connected to the model your real App. }
  TRggDrawingD00 = class(TRggDrawing)
  private
    A0B0: TRggLine;
    A0C0: TRggLine;
    B0C0: TRggLine;
    A0D0: TRggLine;

    B0D0: TRggLine;
    C0D0: TRggLine;

    A0A: TRggLine;
    B0B: TRggLine;
    C0C: TRggLine;

    CF: TRggLine;

    AC: TRggLine;
    BC: TRggLine;

    AB: TRggLine;
    AD: TRggLine;
    BD: TRggLine;
  public
    A0, A: TRggCircle;
    B0, B: TRggCircle;
    C0, C: TRggCircle;
    D0, D: TRggCircle;
    F: TRggCircle;

    D0D: TRggLine;
    DC: TRggLine;

{$ifdef WantPoly}
    MK: TRggPolyLine3D;
    KK: TRggPolyLine3D;
{$endif}

    Koordinaten: TRiggPoints;
    rP_D0: TPoint3D;
    rP_FX: TPoint3D;
    OffsetX: single;
    OffsetY: single;
    InitialZoom: single;

    OffsetXDefault: single;
    OffsetYDefault: single;
    InitialZoomDefault: single;

    FixPoint: TRiggPoint;
    FX: TRggFixpointCircle;

    constructor Create;
    procedure InitDefaultPos; override;
    procedure Load;
    procedure Transform(AM: TMatrix3D); override;
    procedure GoDark; override;
    procedure GoLight; override;

    function GetFixRggCircle: TRggCircle;
    procedure UpdateFromRigg;
    procedure UpdateFX;
  end;

implementation

uses
  RggCalc;

{ TRggDrawingD00 }

procedure TRggDrawingD00.InitDefaultPos;
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

constructor TRggDrawingD00.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'D00-Live-Rigg';

  OffsetXDefault := 540;
  OffsetYDefault := 420;
  InitialZoomDefault := 0.09;

  OffsetX := OffsetXDefault;
  OffsetY := OffsetYDefault;
  InitialZoom := InitialZoomDefault;

  DefaultShowCaption := True;

  { Points }

  FX := TRggFixpointCircle.Create;
  FX.Caption := 'Fixpoint';
  Add(FX);

  A0 := TRggCircle.Create('A0');
  A0.StrokeColor := claGreen;

  B0 := TRggCircle.Create('B0');
  B0.StrokeColor := claRed;

  C0 := TRggCircle.Create('C0');
  C0.StrokeColor := claYellow;

  D0 := TRggCircle.Create('D0');
  D0.StrokeColor := claBlue;

  A := TRggCircle.Create('A');
  A.StrokeColor := claGreen;

  B := TRggCircle.Create('B');
  B.StrokeColor := claRed;

  C := TRggCircle.Create('C');
  C.StrokeColor := claYellow;

  D := TRggCircle.Create('D');
  D.StrokeColor := claBlue;

  F := TRggCircle.Create('F');
  F.StrokeColor := claGray;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('A0B0');
  L.StrokeColor := claGray;
  L.Point1 := A0;
  L.Point2 := B0;
  Add(L);
  A0B0 := L;

  L := TRggLine.Create('A0C0');
  L.StrokeColor := claGray;
  L.Point1 := A0;
  L.Point2 := C0;
  Add(L);
  A0C0 := L;

  L := TRggLine.Create('B0C0');
  L.StrokeColor := claGray;
  L.Point1 := B0;
  L.Point2 := C0;
  Add(L);
  B0C0 := L;

  { --- }

  L := TRggLine.Create('A0D0');
  L.StrokeColor := claBlack;
  L.Point1 := A0;
  L.Point2 := D0;
  Add(L);
  A0D0 := L;

  L := TRggLine.Create('B0D0');
  L.StrokeColor := claBlack;
  L.Point1 := B0;
  L.Point2 := D0;
  Add(L);
  B0D0 := L;

  L := TRggLine.Create('C0D0');
  L.StrokeColor := claBlack;
  L.Point1 := C0;
  L.Point2 := D0;
  Add(L);
  C0D0 := L;

  { --- }

  L := TRggLine.Create('A0A');
  L.StrokeColor := claRed;
  L.Point1 := A0;
  L.Point2 := A;
  Add(L);
  A0A := L;

  L := TRggLine.Create('B0B');
  L.StrokeColor := claGreen;
  L.Point1 := B0;
  L.Point2 := B;
  Add(L);
  B0B := L;

  L := TRggLine.Create('C0C');
  L.StrokeColor := claYellow;
  L.Point1 := C0;
  L.Point2 := C;
  Add(L);
  C0C := L;

  L := TRggLine.Create('D0D');
  L.StrokeColor := claBlue;
  L.Point1 := D0;
  L.Point2 := D;
  Add(L);
  D0D := L;

  { --- }

  L := TRggLine.Create('AC');
  L.StrokeColor := claRed;
  L.Point1 := A;
  L.Point2 := C;
  Add(L);
  AC := L;

  L := TRggLine.Create('BC');
  L.StrokeColor := claGreen;
  L.Point1 := B;
  L.Point2 := C;
  Add(L);
  BC := L;

  L := TRggLine.Create('DC');
  L.StrokeColor := claBlue;
  L.Point1 := D;
  L.Point2 := C;
  Add(L);
  DC := L;

  { --- }

  L := TRggLine.Create('AB');
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);
  AB := L;

  L := TRggLine.Create('AD');
  L.StrokeColor := claLime;
  L.Point1 := A;
  L.Point2 := D;
  Add(L);
  AD := L;

  L := TRggLine.Create('BD');
  L.StrokeColor := claLime;
  L.Point1 := B;
  L.Point2 := D;
  Add(L);
  BD := L;

  L := TRggLine.Create('CF');
  L.StrokeColor := claGray;
  L.Point1 := C;
  L.Point2 := F;
  Add(L);
  CF := L;

{$ifdef WantPoly}
  KK := TRggPolyLine3D.Create('KK', High(TKoordLine) + 1);
  KK.StrokeThickness := 1;
  KK.StrokeColor := claYellow;
  KK.Point1 := D;
  KK.Point2 := C;
  KK.ShowPoly := True;
  KK.WantRotation := True;
  Add(KK);

  MK := TRggPolyLine3D.Create('MK', BogenMax + 1);
  MK.StrokeThickness := 2;
  MK.StrokeColor := claDodgerblue;
  MK.Point1 := D0;
  MK.Point2 := C;
  MK.ShowPoly := True;
  MK.WantRotation := True;
  Add(MK);
{$endif}

  Add(A0);
  Add(B0);
  Add(C0);
  Add(D0);

  Add(A);
  Add(B);
  Add(C);
  Add(D);

  Add(F);

  FixPoint3D := D.Center.C;
  FixPoint := ooD;

  WantRotation := True;
  WantSort := True;

  Load;
end;

procedure TRggDrawingD00.Load;
var
  cr: TRggCircle;
begin
  try
    cr := A0;
    cr.Center.X := 235.42;
    cr.Center.Y := 552.60;
    cr.Center.Z := -164.01;

    cr := B0;
    cr.Center.X := 142.15;
    cr.Center.Y := 589.33;
    cr.Center.Z := 54.05;

    cr := C0;
    cr.Center.X := 520.27;
    cr.Center.Y := 606.96;
    cr.Center.Z := 80.73;

    cr := D0;
    cr.Center.X := 323.92;
    cr.Center.Y := 674.45;
    cr.Center.Z := -14.62;

    cr := A;
    cr.Center.X := 174.48;
    cr.Center.Y := 374.22;
    cr.Center.Z := -127.01;

    cr := B;
    cr.Center.X := 104.53;
    cr.Center.Y := 401.77;
    cr.Center.Z := 36.54;

    cr := C;
    cr.Center.X := 200.72;
    cr.Center.Y := 217.03;
    cr.Center.Z := 9.74;

    cr := D;
    cr.Center.X := 250.00;
    cr.Center.Y := 400.00;
    cr.Center.Z := 0.00;

    cr := F;
    cr.Center.X := 151.44;
    cr.Center.Y := 34.07;
    cr.Center.Z := 19.49;
  except
  end;
end;

procedure TRggDrawingD00.Transform(AM: TMatrix3D);
begin
  inherited;
{$ifdef WantPoly}
  MK.Transform;
  KK.Transform;
{$endif}
end;

procedure TRggDrawingD00.GoLight;
begin
  inherited;
  A0.StrokeColor := claRed;
  B0.StrokeColor := claGreen;
  C0.StrokeColor := claYellow;
  D0.StrokeColor := claBlue;

  A.StrokeColor := claRed;
  B.StrokeColor := claGreen;
  C.StrokeColor := claYellow;
  D.StrokeColor := claBlue;

  F.StrokeColor := claGray;

  A0B0.StrokeColor := claGray;
  A0C0.StrokeColor := claGray;
  B0C0.StrokeColor := claGray;
  A0D0.StrokeColor := claGray;

  B0D0.StrokeColor := claBlack;
  C0D0.StrokeColor := claBlack;

  A0A.StrokeColor := claRed;
  B0B.StrokeColor := claGreen;
  C0C.StrokeColor := claYellow;
  D0D.StrokeColor := claBlue;

  AC.StrokeColor := claRed;
  BC.StrokeColor := claGreen;

  DC.StrokeColor := claBlue;

  AB.StrokeColor := claLime;
  AD.StrokeColor := claLime;
  BD.StrokeColor := claLime;

  CF.StrokeColor := claDodgerblue;

{$ifdef WantPoly}
  KK.StrokeColor := claYellow;
  MK.StrokeColor := claDodgerblue;
{$endif}
end;

procedure TRggDrawingD00.GoDark;
begin
  inherited;
  A0.StrokeColor := claRed;
  B0.StrokeColor := claGreen;
  C0.StrokeColor := claYellow;
  D0.StrokeColor := claBlue;

  A.StrokeColor := claRed;
  B.StrokeColor := claGreen;
  C.StrokeColor := claYellow;
  D.StrokeColor := claDodgerblue;

  F.StrokeColor := claGray;

  A0B0.StrokeColor := claGray;
  A0C0.StrokeColor := claGray;
  B0C0.StrokeColor := claGray;

  A0D0.StrokeColor := claCyan;
  B0D0.StrokeColor := claCyan;
  C0D0.StrokeColor := claCyan;

  A0A.StrokeColor := claRed;
  B0B.StrokeColor := claGreen;
  C0C.StrokeColor := claYellow;
  D0D.StrokeColor := claDodgerblue;

  AC.StrokeColor := claRed;
  BC.StrokeColor := claGreen;

  DC.StrokeColor := claDodgerblue;

  AB.StrokeColor := claLime;
  AD.StrokeColor := claLime;
  BD.StrokeColor := claLime;

  CF.StrokeColor := claCyan;

{$ifdef WantPoly}
  KK.StrokeColor := claYellow;
  MK.StrokeColor := claDodgerblue;
{$endif}
end;

function TRggDrawingD00.GetFixRggCircle: TRggCircle;
var
  cr: TRggCircle;
begin
  cr := D;
  case FixPoint of
    ooN0: ;
    ooA0: cr := A0;
    ooB0: cr := B0;
    ooC0: cr := C0;
    ooD0: cr := D0;
//    ooE0: cr := E0;
//    ooF0: cr := F0;
    ooP0: ;
    ooA: cr := A;
    ooB: cr := B;
    ooC: cr := C;
    ooD: cr := D;
//    ooE: cr := E;
    ooF: cr := F;
//    ooP: cr := P;
//    ooM: cr := M;
  end;
  result := cr;
end;

procedure TRggDrawingD00.UpdateFromRigg;
var
  t: TPoint3D;

  procedure Temp(cr: TRggCircle; oo: TRiggPoint);
  begin
    t := Koordinaten.V[oo] - rP_FX;
    cr.Center.X := OffsetX + t.X * InitialZoom;
    cr.Center.Y := OffsetY - t.Z * InitialZoom;
    cr.Center.Z := -t.Y * InitialZoom;
    cr.Save;
  end;
begin
  rP_D0 := Koordinaten.V[ooD0];
  rP_FX := Koordinaten.V[FixPoint];

    Temp(A0, ooA0);
    Temp(B0, ooB0);
    Temp(C0, ooC0);
    Temp(D0, ooD0);
    Temp(A, ooA);
    Temp(B, ooB);
    Temp(C, ooC);
    Temp(D, ooD);
    Temp(F, ooF);

  UpdateFX;
end;

procedure TRggDrawingD00.UpdateFX;
var
  cr: TRggCircle;
begin
  cr := GetFixRggCircle;
  FixPoint3D := cr.Center.C;
  FX.OriginalCenter := cr.OriginalCenter;
  FX.Center := cr.Center;
end;

end.
