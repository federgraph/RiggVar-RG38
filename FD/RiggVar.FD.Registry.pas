unit RiggVar.FD.Registry;

interface

uses
  RiggVar.FD.Drawings;

type
  TRggDrawingRegistry = class
  public
    class procedure InitFD(DL: TRggDrawings);
    class procedure InitFZ(DL: TRggDrawings);
    class procedure Init(DL: TRggDrawings);
  end;

implementation

uses
  RiggVar.FD.Drawing01,
  RiggVar.FD.Drawing02,
  RiggVar.FD.Drawing03,
  RiggVar.FD.Drawing04,
  RiggVar.FD.Drawing05,
  RiggVar.FD.Drawing06,
  RiggVar.FD.Drawing07,
  RiggVar.FD.Drawing08,
  RiggVar.FD.Drawing09,
  RiggVar.FD.Drawing10,
  RiggVar.FD.Drawing11,
  RiggVar.FD.Drawing12,

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
  RiggVar.FZ.Z16_Shrink;

class procedure TRggDrawingRegistry.Init(DL: TRggDrawings);
begin
  InitFZ(DL);
end;

class procedure TRggDrawingRegistry.InitFD(DL: TRggDrawings);
begin
  DL.Add(RiggVar.FD.Drawing01.TRggDrawing01.Create); // Viereck
  DL.Add(RiggVar.FD.Drawing02.TRggDrawing02.Create); // Logo
  DL.Add(RiggVar.FD.Drawing03.TRggDrawing03.Create); // Getriebe
  DL.Add(RiggVar.FD.Drawing04.TRggDrawing04.Create); // Tetrahedron
  DL.Add(RiggVar.FD.Drawing05.TRggDrawing05.Create); // Rigg
  DL.Add(RiggVar.FD.Drawing06.TRggDrawing06.Create); // Hight
  DL.Add(RiggVar.FD.Drawing07.TRggDrawing07.Create); // Triangle
  DL.Add(RiggVar.FD.Drawing08.TRggDrawing08.Create); // Arc
  DL.Add(RiggVar.FD.Drawing09.TRggDrawing09.Create); // Axis
  DL.Add(RiggVar.FD.Drawing10.TRggDrawing10.Create); // Lager
  DL.Add(RiggVar.FD.Drawing11.TRggDrawing11.Create); // Above
  DL.Add(RiggVar.FD.Drawing12.TRggDrawing12.Create); // Playground
end;

class procedure TRggDrawingRegistry.InitFZ(DL: TRggDrawings);
begin
  DL.Add(TRggDrawing01.Create); // Viereck
  DL.Add(TRggDrawing02.Create); // Logo
  DL.Add(TRggDrawing03.Create); // Viergelenk
  DL.Add(TRggDrawing04.Create); // Tetraeder
  DL.Add(TRggDrawing05.Create); // TestRigg
  DL.Add(TRggDrawing06.Create); // Höhe
  DL.Add(TRggDrawing07.Create); // Triangle
  DL.Add(TRggDrawing08.Create); // Arc
  DL.Add(TRggDrawing09.Create); // Axis
  DL.Add(TRggDrawing10.Create); // Lager
  DL.Add(TRggDrawing11.Create); // Above

  DL.Add(TRggDrawing12.Create); // Atan2
  DL.Add(TRggDrawing12.Create(1)); // Atan

  DL.Add(TRggDrawing13.Create); // SchnittKK
  DL.Add(TRggDrawing14.Create); // SplitF
  DL.Add(TRggDrawing15.Create); // SchnitttGG
  DL.Add(TRggDrawing16.Create); // Shrink
end;

end.
