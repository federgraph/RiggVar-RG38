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
  RiggVar.RG.Main;

type
  TMain = TRggMain;

var
  Main: TMain;

type
  MainConst = class
  const
    ColorSchemeCount = 7;
    DefaultBtnFontSize = 24;
    TrackbarFrequency = 0.1;
    MustBeSandboxed: Boolean = true;
    TrimmFileName = 'Trimm-File.txt';
    TrimmFileNameAuto = 'Trimm-File-Auto.txt';
  end;

  MainVar = class
  public
  class var
    IsSandboxed: Boolean;
    AllProps: Boolean;
    AllTags: Boolean;
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

implementation

{ MainVars }

class constructor MainVar.Create;
begin
  IsSandboxed := true;
  ColorScheme := TColorScheme.Create(5);
  Raster := 70;
end;

end.

