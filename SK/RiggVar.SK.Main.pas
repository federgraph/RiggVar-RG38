unit RiggVar.SK.Main;

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

interface

uses
  RiggVar.SK.Controller;

type
  TMain = TMain0;

var
  Main: TMain;

type
  MainVar = class
  public
  class var
    Raster: Integer;
    ClientWidth: Integer;
    ClientHeight: Integer;
    class constructor Create;
  end;

implementation

{ MainVars }

class constructor MainVar.Create;
begin
  Raster := 70;
end;

end.

