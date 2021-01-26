unit RiggVar.FZ.Registry;

interface

{$define WantAllSamples}

uses
  RiggVar.FD.Drawings,
  RiggVar.FD.Registry;

type
  TRggDrawingRegistry = class(TRggDrawingRegistry00)
  public
    class procedure InitFY(DL: TRggDrawings);
    class procedure InitFZ(DL: TRggDrawings);
    class procedure Init(DL: TRggDrawings);
  end;

implementation

uses
  RiggVar.FZ.Z01_Viereck,
  RiggVar.FZ.Z02_Logo,
  RiggVar.FZ.Z03_Viergelenk,
  RiggVar.FZ.Z04_Tetraeder,
  RiggVar.FZ.Z05_TestRigg,
  RiggVar.FZ.Z06_Hoehe,
  RiggVar.FZ.Z07_Triangle,
  RiggVar.FZ.Z08_Arc,
  RiggVar.FZ.Z09_Axis,
  RiggVar.FZ.Z10_Lager,
  RiggVar.FZ.Z11_Above,
  RiggVar.FZ.Z12_Atan2,
  RiggVar.FZ.Z13_SchnittKK,
  RiggVar.FZ.Z14_SplitF,
  RiggVar.FZ.Z15_SchnittGG,
  RiggVar.FZ.Z16_Shrink,
  RiggVar.FZ.Z17_Feder,
  RiggVar.FZ.Z18_BerechneWinkel,
  RiggVar.FZ.Z19_Chart,
  RiggVar.FZ.Z20_Epsilon,
  RiggVar.FZ.Z21_Rotations,
  RiggVar.FZ.Z22_BigArc,
  RiggVar.FZ.Z23_Federgraph,
  RiggVar.FZ.Z24_Template;

class procedure TRggDrawingRegistry.Init(DL: TRggDrawings);
begin
  InitFY(DL);

{$ifdef WantAllSamples}
  InitFZ(DL);
{$endif}

  DefaultIndex := DL.DrawingList.Count-1;
end;

class procedure TRggDrawingRegistry.InitFY(DL: TRggDrawings);
begin
  DL.Add(TRggDrawingZ01.Create); // Quadrilateral
  DL.Add(TRggDrawingZ02.Create); // Logo
  DL.Add(TRggDrawingZ03.Create); // Viergelenk
  DL.Add(TRggDrawingZ04.Create); // Tetrahedron

  DL.Add(TRggDrawingZ24.Create); // Template

  DefaultIndex := DL.DrawingList.Count-1;
end;

class procedure TRggDrawingRegistry.InitFZ(DL: TRggDrawings);
begin
  DL.Add(TRggDrawingZ05.Create); // TestRigg
  DL.Add(TRggDrawingZ06.Create); // Triangle Height
  DL.Add(TRggDrawingZ07.Create); // Triangle
  DL.Add(TRggDrawingZ08.Create); // Arc
  DL.Add(TRggDrawingZ09.Create); // Axis
  DL.Add(TRggDrawingZ10.Create); // Bearing
  DL.Add(TRggDrawingZ11.Create); // Above

  DL.Add(TRggDrawingZ12.Create); // Atan2
  DL.Add(TRggDrawingZ12.Create(1)); // Atan

  DL.Add(TRggDrawingZ13.Create); // Intersection Circle Circle (SchnittKK)
  DL.Add(TRggDrawingZ14.Create); // SplitForce (SplitF)
  DL.Add(TRggDrawingZ15.Create); // Intersection Line Line (SchnittGG)
  DL.Add(TRggDrawingZ16.Create); // Shrink
  DL.Add(TRggDrawingZ17.Create); // Spring (Feder)
  DL.Add(TRggDrawingZ18.Create); // ComputeAngle (BerechneWinkel)
  DL.Add(TRggDrawingZ19.Create); // Chart
  DL.Add(TRggDrawingZ20.Create); // Epsilon
  DL.Add(TRggDrawingZ21.Create); // Rotations
  DL.Add(TRggDrawingZ22.Create); // BigArc
  DL.Add(TRggDrawingZ23.Create); // Federgraph

  DefaultIndex := DL.DrawingList.Count-1;
end;

end.
