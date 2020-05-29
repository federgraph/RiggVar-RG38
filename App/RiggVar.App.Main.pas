unit RiggVar.App.Main;

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
  RiggVar.FB.Scheme,
  RiggVar.App.Main1;

type
  TMain = TMain1;

var
  Main: TMain;

type
  MainConst = class
  const
    DefaultBtnFontSize = 24;
    TrackbarFrequency = 0.1;
  end;

  MainVar = class
  public
  class var
    RG: Boolean;
    AppIsClosing: Boolean;
    ShowDebugData: Boolean;
    WantOnResize: Boolean;
    ColorScheme: TColorScheme;
    Raster: Integer;
    ScaledRaster: Integer;
    Scale: single;
    ClientWidth: Integer;
    ClientHeight: Integer;
    class constructor Create;
  end;

var
  IsSandboxed: Boolean = false;

const
  ColorSchemeCount = 7;

  TrimmFileName = 'Trimm-File.txt';
  TrimmFileNameAuto = 'Trimm-File-Auto.txt';

implementation

{ MainVars }

class constructor MainVar.Create;
begin
  ColorScheme := TColorScheme.Create(5);
  Raster := 70;
end;

end.

