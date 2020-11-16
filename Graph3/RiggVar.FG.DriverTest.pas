unit RiggVar.FG.DriverTest;

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

{$ifdef MSWINDOWS}

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.D3DCommon,
  FMX.Viewport3D,
  FMX.Context.DX9,
  FMX.Context.DX11;

type
  TDeviceCheck = class
  private
    SL: TStringList;
    dt: string;
    fl: string;
    DriverType: D3D_DRIVER_TYPE;
    FeatureLevel: UINT;
    procedure AddLines(ML: TStrings);
  public
    Viewport: TViewport3D;
    constructor Create;
    destructor Destroy; override;
    procedure CheckNormal;
    function CheckSpecial: Boolean;
    procedure GetDeviceReport(ML: TStrings);
  end;

  TContextClass = TCustomDX11Context;
{$endif}

{$ifdef MACOS}
uses
  Classes;

type
  TDeviceCheck = class
  public
    procedure CheckNormal;
    function CheckSpecial: Boolean;
    procedure GetDeviceReport(ML: TStrings);
  end;
{$endif}

var
  DeviceCheck: TDeviceCheck;

implementation

{$ifdef MSWINDOWS}
uses
  FMX.Types,
  FMX.Dialogs,
  FMX.Platform,
  RiggVar.FB.Classes,
  RiggVar.Util.AppUtils,
  RiggVar.App.Main;

{ TDeviceCheck }

constructor TDeviceCheck.Create;
begin
  SL := TStringList.Create;
end;

destructor TDeviceCheck.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TDeviceCheck.AddLines(ML: TStrings);
begin
  case DriverType of
    D3D_DRIVER_TYPE_HARDWARE: dt := 'Hardware';
    D3D_DRIVER_TYPE_REFERENCE: dt := 'Referenz';
    D3D_DRIVER_TYPE_NULL: dt := 'Null';
    D3D_DRIVER_TYPE_SOFTWARE: dt := 'Software';
    D3D_DRIVER_TYPE_WARP: dt := 'Warp';
    else
      dt := 'Unknown';
  end;

  case FeatureLevel of
    D3D_FEATURE_LEVEL_9_1:
      fl := '9.1';
    D3D_FEATURE_LEVEL_9_2:
      fl := '9.2';
    D3D_FEATURE_LEVEL_9_3:
      fl := '9.3';
    D3D_FEATURE_LEVEL_10_0:
      fl := '10.0';
    D3D_FEATURE_LEVEL_10_1:
      fl := '10.1';
    D3D_FEATURE_LEVEL_11_0:
      fl := '11.0';
    D3D_FEATURE_LEVEL_11_1:
      fl := '11.1';
    D3D_FEATURE_LEVEL_12_0:
      fl := '12.0';
    D3D_FEATURE_LEVEL_12_1:
      fl := '12.1';
  else
    fl := 'Unknown';
  end;

  ML.Add(Format('  Driver type is %s.', [dt]));
  ML.Add(Format('  Feature level is %s.', [fl]));
end;

procedure TDeviceCheck.CheckNormal;
var
  DX9OK: Boolean;
begin
  SL.Add('Testing for TContextDX11-Hardware:');
  GlobalUseDXSoftware := False;
  TContextClass.TestDriverSupport(DriverType, FeatureLevel);
  AddLines(SL);

  SL.Add('');
  SL.Add('Testing for TContextDX11-Warp:');
  GlobalUseDXSoftware := True;
//  TContextClass.ResetDriverSupportTested;
  TContextClass.TestDriverSupport(DriverType, FeatureLevel);
  AddLines(SL);

  SL.Add('');
  SL.Add('Testing for TContextDX9-Hardware:');
  DX9OK := TCustomDX9Context.HardwareSupported;
  if DX9OK then
    SL.Add('  ok')
  else
    SL.Add('  not available');

  ShowMessage(SL.Text);

  GlobalUseDXSoftware := False;
//  TContextClass.ResetDriverSupportTested;
end;

function TDeviceCheck.CheckSpecial: Boolean;
var
  DriverOK: Boolean;
begin
  DriverOK := False;
  TContextClass.TestDriverSupport(DriverType, FeatureLevel);
  if FeatureLevel < D3D_FEATURE_LEVEL_11_0 then
  begin
    SL.Add('11_0 hardware driver not found.');
    GlobalUseDXSoftware := True;
//    TContextClass.ResetDriverSupportTested;
    TContextClass.TestDriverSupport(DriverType, FeatureLevel);
    if FeatureLevel < D3D_FEATURE_LEVEL_11_0 then
    begin
      SL.Add('Testing for Warp driver.');
      SL.Add('Feature level is < D3D_FEATURE_LEVEL_11_0');
      SL.Add('Program will be closed.');
    end
    else
    begin
      DriverOK := True; // Software driver ok
    end;
  end
  else
  begin
    DriverOK := True; // Hardware driver ok
  end;

  if not DriverOK then
    ShowMessage(SL.Text);

  result := DriverOK;
end;

procedure TDeviceCheck.GetDeviceReport(ML: TStrings);
var
  cr9: TCustomDX9Context;
begin
  { DeviceCheck instance usually created in dpr project file. }
  { Viewport instance usually injected in FormMain, if RotaForm3 is used. }
  if (Main <> nil) and (Viewport <> nil) then
  begin
    ML.Add(Viewport.Context.ClassName);
    if Viewport.Context is TContextClass then
    begin
//      DriverType := TContextClass.DriverType;
      FeatureLevel := TContextClass.FeatureLevel;
      AddLines(ML);
    end;
    if Viewport.Context is TCustomDX9Context then
    begin
      cr9 := Viewport.Context as TCustomDX9Context;
      ML.Add(Format('  MaxTextureWidth = %d', [cr9.MaxTextureSize]));
    end;
  end;
end;

{$endif}

{$ifdef MACOS}

procedure TDeviceCheck.GetDeviceReport(ML: TStrings);
begin
end;

procedure TDeviceCheck.CheckNormal;
begin
end;

function TDeviceCheck.CheckSpecial: Boolean;
begin
  result := True;
end;

{$endif}

end.
