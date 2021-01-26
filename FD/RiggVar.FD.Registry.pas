unit RiggVar.FD.Registry;

interface

uses
  RiggVar.FD.Drawings;

type
  TRggDrawingRegistry00 = class
  public
    class var
      DefaultIndex: Integer;
    class procedure InitFD(DL: TRggDrawings);
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
  RiggVar.FD.Drawing12;

class procedure TRggDrawingRegistry00.InitFD(DL: TRggDrawings);
begin
  DL.Add(RiggVar.FD.Drawing01.TRggDrawingD01.Create); // Viereck
  DL.Add(RiggVar.FD.Drawing02.TRggDrawingD02.Create); // Logo
  DL.Add(RiggVar.FD.Drawing03.TRggDrawingD03.Create); // Getriebe
  DL.Add(RiggVar.FD.Drawing04.TRggDrawingD04.Create); // Tetrahedron
  DL.Add(RiggVar.FD.Drawing05.TRggDrawingD05.Create); // Rigg
  DL.Add(RiggVar.FD.Drawing06.TRggDrawingD06.Create); // Height
  DL.Add(RiggVar.FD.Drawing07.TRggDrawingD07.Create); // Triangle
  DL.Add(RiggVar.FD.Drawing08.TRggDrawingD08.Create); // Arc
  DL.Add(RiggVar.FD.Drawing09.TRggDrawingD09.Create); // Axis
  DL.Add(RiggVar.FD.Drawing10.TRggDrawingD10.Create); // Lager
  DL.Add(RiggVar.FD.Drawing11.TRggDrawingD11.Create); // Above
  DL.Add(RiggVar.FD.Drawing12.TRggDrawingD12.Create); // Playground

  DefaultIndex := DL.DrawingList.Count-1;
end;

end.
