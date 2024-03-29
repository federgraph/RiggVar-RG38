﻿unit RiggVar.App.Main;

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

{ 'MinimalRG' should be defined in project options (for RG03),
  in order to exclude stuff not needed for the minimal app. }

{ If MinimalRG is NOT defined, then go ahead and define stuff, for RG38: }
{$ifndef MinimalRG}
{$define WantColorScheme}
{$endif}

{.$define WantDummyController}

uses
{$ifdef WantColorScheme}
  RiggVar.FB.Scheme,
{$endif}
{$ifdef WantDummyController}
  RiggVar.App.Controller;
{$else}
  RiggVar.RG.Main;
{$endif}

type
  TMain = TRggMain;

var
  Main: TMain;

type
  MainConst = class
  const
    CanReadIniFile = False;
    ColorSchemeCount = 7;
    DefaultBtnFontSize = 24;
    TrackbarFrequency = 0.1;
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
{$ifdef WantColorScheme}
    ColorScheme: TColorScheme;
{$endif}
    Raster: Integer;
    ScaledRaster: Integer;
    Scale: single;
    ClientWidth: Integer;
    ClientHeight: Integer;
    WantFederText: Boolean;
    StatusBarHeight: Integer;
    WantLocalizedText: Boolean;
    WantGermanText: Boolean;
    class constructor Create;
  end;

implementation

{ MainVars }

class constructor MainVar.Create;
begin
{$ifdef MSWINDOWS}
  IsSandboxed := true;
{$endif}
{$ifdef OSX}
  IsSandboxed := true;
{$endif}
{$ifdef Android}
  IsSandboxed := false;
{$endif}
{$ifdef iOS}
  IsSandboxed := false;
{$endif}

{$ifdef WantColorScheme}
  ColorScheme := TColorScheme.Create(5);
{$endif}
  Raster := 70;
  Scale := 1.0;
end;

end.

