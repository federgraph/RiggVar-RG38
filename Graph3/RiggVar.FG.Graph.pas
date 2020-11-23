unit RiggVar.FG.Graph;

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

{.$define WantOrtho}

uses
  System.Math,
  System.Math.Vectors,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Viewport3D,
  FMX.Materials,
  FMX.MaterialSources,
  RggTypes,
  RiggVar.FB.Classes,
  RiggVar.FG.Objects,
  RiggVar.FG.Mesh,
  RiggVar.RG.Def;

type
  TBorderTrack = (
    btNoop,
    btTop,
    btBottom,
    btLeft,
    btRight,
    btInside,
    btOutside,
    btTopLeft,
    btTopRight,
    btBottomLeft,
    btBottomRight
  );

  TFederGraph0 = class(TInterfacedObject)
  private
    FIsOrthoProjection: Boolean;
    FOnViewportChanged: TNotifyEvent;
    FMouseRotationSpeed: Extended;

    SB: TStringBuilder;

    OldX, OldY: single;
    Down: Boolean;

    mmfmk: TFederMessageKind;
    mmX, mmY, mmDelta: single;

    FLastTrack: TBorderTrack;
    RasterR, RasterB: single;
    RasterW, RasterH: single;

    WantLinearMove: Boolean;
    WantLinearZoom: Boolean;

    FWheelBetrag: Extended;
    FWheelValue: Extended;
    FWheelDelta: Extended;

    WantIdleMove: Boolean;
    RotationInfo: TPoint3D;

    BMP: TBitmap;
    MaterialSourceL: TLightMaterialSource;
    ModelParent: TFmxObject;
    ModelOwner: TComponent;

    GlobalZoom: single;
    GlobalZoomMin: single;
    GlobalZoomMax: single;
    GlobalZoomSpeed: single;

    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: single);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: single);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: single);
    procedure HandleNormalMove(Sender: TObject; Shift: TShiftState; X, Y: single);

    procedure SetOnViewportChanged(const Value: TNotifyEvent);
    procedure SetMoveMode(const Value: Boolean);
    function GetMoveMode: Boolean;
    function GetIsClean: Boolean;
    function GetMoveModeText: string;
    function GetDefaultRX: Integer;

    function GetBitmap: TBitmap;
    function GetBitmap99(color: Boolean; szmin, szmax, offset, gain: Integer): TBitmap;
    procedure AssignBitmap;

    procedure InitDummy;
    procedure InitHull;

    function GetIsUp: Boolean;

    procedure SetIsOrthoProjection(const Value: Boolean);

    procedure HandleClick(Sender: TObject);
    procedure ClearIdleMoveInfo;
    procedure ViewportChanged;

    procedure HandleTrackMove(X, Y: single);
    procedure UpdateBorderTrack(X, Y: single);
  protected
    procedure InitFrame(vp: TViewport3D);
  public
    CameraDummy: TDummy;
    FederMesh: THullMesh;

{$ifdef WantOrtho}
    ModelGroup: TFederDummy;
    Camera: TFederCamera;
{$else}
    ModelGroup: TDummy;
    Camera: TCamera;
{$endif}

    Viewport: TViewPort3D; // injected

    InitOK: Boolean;

    class var RggGlobalOffsetX: single;
    class var RggGlobalOffsetY: single;
    class var RggGlobalOffsetZ: single;
    class var RggGlobalScale: single;

    constructor Create;
    destructor Destroy; override;

    procedure InitGraph;
    procedure InitBitmap;

    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);

    procedure ViewportKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: single);
    procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: single);
    procedure ViewportMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: single);
    procedure ViewportClick(Sender: TObject);

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure ImageClick(Sender: TObject);

    procedure HandleKey(KeyChar: Char);

    procedure UpdateSize(X, Y: single);
    procedure DoOnIdle;

    procedure Reset;
    procedure ResetRotation;
    procedure ResetPosition;
    procedure ResetZoom;
    procedure ResetPositionAndRotation;

{$ifdef WantOrtho}
    procedure ResetOrtho; virtual; abstract;
    procedure ResetOrthoZoom; virtual; abstract;
    procedure ResetOrthoRotation; virtual; abstract;

    procedure OrthoRotXYZ(p: TPoint3D); virtual; abstract;
    procedure OrthoRotZYX(const pz, py, px: single); virtual; abstract;
    procedure OrthoRotDeltaXY(const DeltaX, DeltaY: single); virtual; abstract;
    procedure OrthoRotDeltaZ(const DeltaZ: single); virtual; abstract;
    procedure OrthoZoomDelta(const Value: single); virtual; abstract;
{$endif}

    procedure DoZoom(Delta: single);
    procedure DoZoomTimed(Delta: single);
    procedure DoZoomTimed3D(Delta: single);
    procedure DoZoomTimed3DPerspective(Delta: single);
    procedure DoMM(fmk: TFederMessageKind; X, Y: single);
    procedure DoMMTimed(fmk: TFederMessageKind; X, Y: single);
    procedure DoMMRemote(fmk: TFederMessageKind; X, Y: single);
    procedure DoParamValueChange(fp: TFederParam; pv: double);

    procedure LoadEulerAngle(ed: TPoint3D);
    function GetRotationInfoFrame: TPoint3D;

    procedure UpdateRasterSize(RetinaScale: single; X, Y: single);
    function DoCameraZoom(Delta, CameraPositionZ: single): single;

    property IsUp: Boolean read GetIsUp;
    property IsOrthoProjection: Boolean read FIsOrthoProjection write SetIsOrthoProjection;

    property MouseDown: Boolean read Down;
    property IsClean: Boolean read GetIsClean;
    property MoveMode: Boolean read GetMoveMode write SetMoveMode;
    property OnViewportChanged: TNotifyEvent read FOnViewportChanged write SetOnViewportChanged;
    property MoveModeText: string read GetMoveModeText;

    property DefaultRX: Integer read GetDefaultRX;
  end;

{$ifdef WantOrtho}
  TFederGraph = TFederGraph1;
{$else}
  TFederGraph = TFederGraph0;
{$endif}

implementation

uses
  RiggVar.FB.ActionConst,
  RiggVar.App.Main,
  RiggVar.RG.Main;

constructor TFederGraph0.Create;
begin
  GlobalZoom := 20.0;
  GlobalZoomMin := -100;
  GlobalZoomMax := 500.0;
  GlobalZoomSpeed := 0.2;

  RggGlobalOffsetX := 2870;
  RggGlobalOffsetY := 0;
  RggGlobalOffsetZ := -100;
  RggGlobalScale := 500;

  FWheelBetrag := GlobalZoom;

  WantIdleMove := True;

  MaterialSourceL := TLightMaterialSource.Create(nil);
  MaterialSourceL.Shininess := 30;
  MaterialSourceL.Ambient := claWhite;
  MaterialSourceL.Diffuse := claWhite;
  MaterialSourceL.Specular := claWhite;

  SB := TStringBuilder.Create;
end;

destructor TFederGraph0.Destroy;
begin
  SB.Free;

  BMP.Free;
  MaterialSourceL.Free;
  inherited;
end;

procedure TFederGraph0.InitGraph;
begin
  ModelOwner := Viewport.Owner;
  ModelParent := Viewport;
  InitBitmap;
  InitDummy;
  InitHull;
end;

procedure TFederGraph0.InitBitmap;
begin
  if Assigned(BMP) then
    BMP.Free;
  BMP := GetBitmap;
  AssignBitmap;
  Viewport.Repaint;
end;

procedure TFederGraph0.AssignBitmap;
begin
  MaterialSourceL.Texture := BMP;
end;

procedure TFederGraph0.InitDummy;
begin
  if Assigned(ModelGroup) then
    ModelGroup.Free;
{$ifdef WantOrtho}
  ModelGroup := TFederDummy.Create(ModelOwner);
{$else}
  ModelGroup := TDummy.Create(ModelOwner);
{$endif}

  ModelParent.AddObject(ModelGroup);
end;

procedure TFederGraph0.InitHull;
begin
  FederMesh := THullMesh.Create(ModelOwner);
  FederMesh.FederModel_FT1 := 650;
  FederMesh.FederModel_FT2 := 150;
  FederMesh.TwoSide := True;
  FederMesh.MeshData.Inside := True;
  FederMesh.Opacity := 1.0;
  FederMesh.MaterialSource := MaterialSourceL;
  FederMesh.InitMesh;

  ModelGroup.AddObject(FederMesh);
  FederMesh.Visible := True;
end;

function TFederGraph0.GetIsUp: Boolean;
begin
  result := False;
  if Assigned(Main) then
    result := Main.IsUP;
end;

function TFederGraph0.GetBitmap: TBitmap;
begin
  result := GetBitmap99(True, 0, 360, 0, 1);
end;

function TFederGraph0.GetBitmap99(color: Boolean; szmin, szmax, offset, gain: Integer): TBitmap;
var
  Data: TBitmapData;
  i: Integer;
  u, v: Integer;
  h, l, s: single;
  cla: TAlphaColor;
begin
  result := TBitmap.Create(1, 360);
  if result.Map(TMapAccess.Write, Data) then
  begin
    s := 0.75;
    l := 0.5;
    u := szmin;
    v := 1;
    for i := 0 to 359 do
    begin

      h := (offset + u * gain) / 360;
      if h < 0 then
        h := 0;
      if h > 1 then
        h := 1;

      if color then
        cla := HSLtoRGB(h, s, l)
      else
        cla := MakeColor(round(h * $FF), round(h * $FF), round(h * $FF));

      Data.SetPixel(0, i, cla);

      { modulate color between toolchain szmin and szmax }
      if u >= szmax then
      begin
        Dec(u);
        v := -1;
      end
      else if u <= szmin then
      begin
        Inc(u);
        v := 1;
      end
      else
        u := u + v;
    end;
    result.Unmap(Data);
  end;
end;

procedure TFederGraph0.DoMM(fmk: TFederMessageKind; X, Y: single);
begin
  if InitOK then
  begin
    case fmk of
      fmkTX, fmkTY:
      begin
        mmfmk := fmk;
        mmX := mmX - X * 0.1;
        mmY := mmY + Y * 0.1;
      end;

      fmkRX, fmkRY:
      begin
        mmfmk := fmk;
        mmX := mmX - Y * 0.2;
        mmY := mmY - X * 0.2;
      end;

      fmkRZ:
      begin
        mmfmk := fmk;
        mmX := mmX + X * 0.2;
      end;
    end;
  end;
end;

function TFederGraph0.GetMoveMode: Boolean;
begin
  result := WantLinearMove;
end;

function TFederGraph0.GetMoveModeText: string;
begin
  if MoveMode then
  begin
    if Main.IsPhone then
      result := 'mm'
    else
      result := 'Fine move'
  end
  else
    result := ''; // normal move
end;

procedure TFederGraph0.SetMoveMode(const Value: Boolean);
begin
  WantLinearMove := Value;
  WantLinearZoom := Value;
end;

function TFederGraph0.GetDefaultRX: Integer;
begin
  result := 0;
end;

function TFederGraph0.GetIsClean: Boolean;
begin
  result := mmfmk = fmkNoop;
end;

procedure TFederGraph0.ClearIdleMoveInfo;
begin
  mmfmk := fmkNoop;
  mmX := 0;
  mmY := 0;
  mmDelta := 0;
end;

procedure TFederGraph0.UpdateRasterSize(RetinaScale: single; X, Y: single);
begin
  if RetinaScale > 1 then
  begin
    RasterW := X * RetinaScale;
    RasterH := Y * RetinaScale;
    RasterR := RasterW - MainVar.Raster * RetinaScale;
    RasterB := RasterH - MainVar.Raster * RetinaScale;
  end
  else
  begin
    RasterW := X;
    RasterH := Y;
    RasterR := RasterW - MainVar.Raster;
    RasterB := RasterH - MainVar.Raster;
  end;
end;

procedure TFederGraph0.UpdateBorderTrack(X, Y: single);
begin
  FLastTrack := btNoop;

  { Inside}
  if (X > MainVar.Raster) and (X < RasterR) and (Y > MainVar.Raster) and (Y < RasterB) then
    FLastTrack := btInside

  { Outside }
  else if (X < 0) or (X > RasterW) or (Y < 0) or (Y > RasterH) then
    FLastTrack := btOutside

  { Corner }
  else if (X < MainVar.Raster) and (Y < MainVar.Raster) then
    FLastTrack := btTopLeft
  else if (X > RasterR) and (Y < MainVar.Raster) then
    FLastTrack := btTopRight
  else if (X < MainVar.Raster) and (Y > RasterB) then
    FLastTrack := btBottomLeft
  else if (X > RasterR) and (Y > RasterB) then
    FLastTrack := btBottomRight

  { Bar }
  else if Y < MainVar.Raster then
    FLastTrack := btTop
  else if Y > RasterB then
    FLastTrack := btBottom
  else if X < MainVar.Raster then
    FLastTrack := btLeft
  else if X > RasterR then
    FLastTrack := btRight;
end;

procedure TFederGraph0.HandleTrackMove(X, Y: single);
begin
  if FLastTrack = btBottom then
  begin
    if Abs(X - OldX) > 0 then
    begin
      DoZoom((OldX-X) / 20);
      OldX := X;
      OldY := Y;
    end;
  end
  else if FLastTrack = btTop then
  begin
    if Abs(X - OldX) > 0 then
    begin
      DoMM(fmkRZ, (X-OldX) / 1, 0);
      OldX := X;
      OldY := Y;
    end;
  end
  else if FLastTrack = btLeft then
  begin
    if (Round(Abs(Y - OldY)) div 8) > 0 then
    begin
      Main.DoBigWheel(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else if FLastTrack = btRight then
  begin
    if (Round(Abs(Y - OldY)) div 8) > 0 then
    begin
      Main.DoSmallWheel(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else
  begin
    OldX := X;
    OldY := Y;
  end;
end;

procedure TFederGraph0.HandleClick(Sender: TObject);
begin
  UpdateBorderTrack(OldX, OldY);
  case FLastTrack of
    btTopLeft: Main.ActionHandler.Execute(faSalingA);
    btTopRight: Main.ActionHandler.Execute(faSalingH);
    btBottomLeft: Main.ActionHandler.Execute(faWante);
    btBottomRight: Main.ActionHandler.Execute(faVorstag);
  end;
end;

procedure TFederGraph0.SetOnViewportChanged(const Value: TNotifyEvent);
begin
  FOnViewportChanged := Value;
end;

procedure TFederGraph0.ViewportChanged;
begin
  if Assigned(OnViewportChanged) then
    OnViewportChanged(nil);
end;

procedure TFederGraph0.ViewportMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  HandleMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TFederGraph0.ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  HandleMouseMove(Sender, Shift, X, Y);
end;

procedure TFederGraph0.ViewportMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  HandleMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TFederGraph0.ImageClick(Sender: TObject);
begin
  HandleClick(Sender);
end;

procedure TFederGraph0.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos: TVector3D; RayDir: TVector3D);
begin
  HandleMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TFederGraph0.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single; RayPos: TVector3D; RayDir: TVector3D);
begin
  HandleMouseMove(Sender, Shift, X, Y);
end;

procedure TFederGraph0.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos: TVector3D; RayDir: TVector3D);
begin
  HandleMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TFederGraph0.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  OldX := X;
  OldY := Y;
  ClearIdleMoveInfo;
  UpdateBorderTrack(X, Y);
  Down := True;
end;

procedure TFederGraph0.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FLastTrack := btNoop;
  Down := False;
  Main.FederText.OwnsMouse := False;
end;

procedure TFederGraph0.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if InitOK then
  begin
    if not Main.FederText.FrameVisible then
    begin
      UpdateBorderTrack(X, Y);
      if Down then
      begin
        if FLastTrack <> btInside then
          HandleTrackMove(X, Y)
        else
          HandleNormalMove(Sender, Shift, X, Y);
      end;
    end
    else
      HandleNormalMove(Sender, Shift, X, Y);
  end;
end;

procedure TFederGraph0.HandleNormalMove(Sender: TObject; Shift: TShiftState; X, Y: single);
var
  ldx, ldy: single;
  rx, ry: Integer;
begin
  if InitOK and Down and not Main.FederText.OwnsMouse then
  begin
    ldx := X - OldX;
    ldy := Y - OldY;
    rx := Round(ldx);
    ry := Round(ldy);

    if (ssCtrl in Shift) and (ssLeft in Shift) then
    begin
      { Pan }
      Camera.Position.X := Camera.Position.X - ldx * 0.01;
      Camera.Position.Y := Camera.Position.Y + ldy * 0.01;
      OldX := X;
      OldY := Y;
    end
    else if (ssLeft in Shift) then
    begin
      if WantLinearMove and (FWheelValue > 1) then
        FMouseRotationSpeed := 0.2 / FWheelValue
      else
        FMouseRotationSpeed := 0.2;

      if WantIdleMove then
      begin
        mmfmk := fmkRX;
        mmX := mmX + ldy * FMouseRotationSpeed;
        mmY := mmY + ldx * FMouseRotationSpeed;
        OldX := X;
        OldY := Y;
      end
      else
      begin
        if IsOrthoProjection then
        begin
{$ifdef WantOrtho}
          OrthoRotDeltaXY(
          ldx * FMouseRotationSpeed * -0.01,
          ldy * FMouseRotationSpeed * 0.01
          );
{$endif}
        end
        else
        begin
          if ry <> 0 then
            CameraDummy.RotationAngle.X := CameraDummy.RotationAngle.X - ldy * FMouseRotationSpeed;
          if rx <> 0 then
            CameraDummy.RotationAngle.Y := CameraDummy.RotationAngle.Y - ldx * FMouseRotationSpeed;
        end;
        OldX := X;
        OldY := Y;
        RotationInfo := GetRotationInfoFrame;
        ViewportChanged;
      end;
    end
    else if (ssRight in Shift) then
    begin
      if WantLinearMove and (FWheelValue > 1) then
        FMouseRotationSpeed := 0.2 / FWheelValue
      else
        FMouseRotationSpeed := 0.2;

      if WantIdleMove then
      begin
        mmfmk := fmkRZ;
        mmX := mmX + ldx * FMouseRotationSpeed;
        OldX := X;
      end
      else
      begin
        if IsOrthoProjection then
        begin
{$ifdef WantOrtho}
        if rx <> 0 then
          OrthoRotDeltaZ(ldx * 0.003);
{$endif}
        end
        else
        begin
          if rx <> 0 then
            CameraDummy.RotationAngle.Z := CameraDummy.RotationAngle.Z + ldx * 0.3;
        end;
        OldX := X;
        OldY := Y;
        ViewportChanged;
      end;
    end;
  end;
end;

procedure TFederGraph0.DoMMRemote(fmk: TFederMessageKind; X, Y: single);
begin
  if InitOK then
  begin
    case fmk of
      fmkTX, fmkTY:
      begin
        Camera.Position.X := Camera.Position.X + X * 0.1;
        Camera.Position.Y := Camera.Position.Y + Y * 0.1;
      end;

      fmkRX, fmkRY:
      begin
        CameraDummy.RotationAngle.X := CameraDummy.RotationAngle.X + Y * 0.2;
        CameraDummy.RotationAngle.Y := CameraDummy.RotationAngle.Y + X * 0.2;
        ViewportChanged;
      end;

      fmkRZ:
      begin
        CameraDummy.RotationAngle.Z := CameraDummy.RotationAngle.Z + X * 0.3;
        ViewportChanged;
      end;
    end;
  end;
end;

procedure TFederGraph0.DoMMTimed(fmk: TFederMessageKind; X, Y: Single);
begin
  if InitOK then
  begin
    case fmk of
      fmkTX, fmkTY:
      begin
        Camera.Position.X := Camera.Position.X - X;
        Camera.Position.Y := Camera.Position.Y + Y;
      end;

      fmkRX, fmkRY:
      begin
        CameraDummy.RotationAngle.X := CameraDummy.RotationAngle.X - X;
        CameraDummy.RotationAngle.Y := CameraDummy.RotationAngle.Y - Y;
        ViewportChanged;
      end;

      fmkRZ:
      begin
        CameraDummy.RotationAngle.Z := CameraDummy.RotationAngle.Z + X;
        ViewportChanged;
      end;
    end;
  end;
  ClearIdleMoveInfo;
end;

procedure TFederGraph0.DoZoom(Delta: single);
begin
{$ifdef WantOrtho}
  if Camera.ViewType = TViewType.Orthographic then
  begin
    OrthoZoomDelta(-Delta);
  end
  else
  begin
    mmfmk := fmkCZ;
    mmDelta := mmDelta + Delta;
  end;
  Exit;
{$endif}

  mmfmk := fmkCZ;
  mmDelta := mmDelta + Delta;
end;

procedure TFederGraph0.DoZoomTimed(Delta: single);
var
  temp: single;
begin
  if Delta <> 0 then
  begin
    temp := Delta;
    if WantLinearMove then
      temp := Delta * 0.5;
    DoZoomTimed3D(temp);
    ClearIdleMoveInfo;
  end;
end;

procedure TFederGraph0.DoZoomTimed3D(Delta: single);
begin
  if InitOK then
  begin
    if Delta > 3 then
      Delta := 3;
    if Delta < -3 then
      Delta := -3;

{$ifdef WantOrtho}
    if Camera.ViewType = TViewType.Orthographic then
    begin
      OrthoZoomDelta(-Delta);
    end
    else
    begin
       DoZoomTimed3DPerspective(Delta);
    end;
   ViewportChanged;
{$else}
  DoZoomTimed3DPerspective(Delta);
  ViewportChanged;
{$endif}
  end;
end;

procedure TFederGraph0.DoZoomTimed3DPerspective(Delta: single);
var
  v: TPoint3D;
  l: single;
begin
  if InitOK then
  begin
    if Delta > 3 then
      Delta := 3;
    if Delta < -3 then
      Delta := -3;

    l := DoCameraZoom(Delta, Camera.Position.Z);

    v := TPoint3D.Create(0, 0, 1);
    v := v * l;
    v.X := Camera.Position.X;
    v.Y := Camera.Position.Y;

    Camera.Position.X := v.X;
    Camera.Position.Y := v.Y;
    Camera.Position.Z := v.Z;
  end;
end;

procedure TFederGraph0.DoParamValueChange(fp: TFederParam; pv: double);
begin
  if InitOK then
  case fp of

{$ifdef WantOrtho}
    fprx:
    begin
      CameraDummy.RotationAngle.X := Round(pv) mod 360;
    end;
    fpry:
    begin
      CameraDummy.RotationAngle.Y := Round(pv) mod 360;
    end;
    fprz:
    begin
      CameraDummy.RotationAngle.Z := Round(pv) mod 360;
    end;
{$endif}

    fptx:
    begin
      if Abs(pv) < 100 then
        Camera.Position.X := pv;
    end;
    fpty:
    begin
      if Abs(pv) < 100 then
        Camera.Position.Y := pv;
    end;
    fpcz:
    begin
      if (pv >= GlobalZoomMin) and (pv <= GlobalZoomMax) then
        Camera.Position.Z := pv;
    end;
    fppx:
    begin
      Camera.Position.X := pv / 20;
    end;
    fppy:
    begin
      Camera.Position.Y := pv / 20;
    end;

{$ifdef WantOrtho}
    fpva:
    begin
      if not IsOrthoProjection then
      begin
        if (pv >= 2) and (pv <= 122) then
        begin
          Camera.AngleW := pv;
          Camera.Position.Z := 1650 / pv / 5;
        end;
      end;
    end;
    fpnp:
    begin
      Camera.NearPlane := pv;
    end;
    fpfp:
    begin
      Camera.FarPlane := pv;
    end;
{$endif}

  end;
  ClearIdleMoveInfo;
end;

procedure TFederGraph0.ViewportClick(Sender: TObject);
begin
  HandleClick(Sender);
end;

procedure TFederGraph0.DoOnIdle;
begin
  if mmfmk = fmkCZ then
    DoZoomTimed(mmDelta)
  else
    DoMMTimed(mmfmk, mmX, mmY);
end;

procedure TFederGraph0.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if ssShift in Shift then
  begin
    Main.DoBigWheel(WheelDelta);
  end
  else if ssCtrl in Shift then
  begin
    DoZoomTimed(-WheelDelta/120);
  end
  else
  begin
    Main.DoSmallWheel(WheelDelta);
  end;
  Handled := True;
end;

procedure TFederGraph0.ViewportKeyDown(Sender: TObject; var Key: Word;
var KeyChar: Char; Shift: TShiftState);
begin
  if Main.CurrentRotaForm <> 3 then
    Exit;

  if Main.IsUp then
  begin
    if Key = vkC then
      Main.ActionHandler.Execute(faCopyTrimmItem)
    else if Key = vkV then
      Main.ActionHandler.Execute(faPasteTrimmItem)

    else if Key = VKEscape then
      Main.ActionHandler.Execute(faReset)

    else
    begin
      HandleKey(KeyChar);
    end;
  end;
end;

procedure TFederGraph0.UpdateSize(X, Y: single);
var
  ss: single;
begin
  ss := MainVar.Scale;
  if ss > 1 then
  begin
    RasterW := X * ss;
    RasterH := Y * ss;
  end
  else
  begin
    RasterW := X;
    RasterH := Y;
  end;
  RasterR := RasterW - MainVar.Raster;
  RasterB := RasterH - MainVar.Raster;
end;

procedure TFederGraph0.InitFrame(vp: TViewport3D);
begin
  if vp = nil then
    Exit;

  CameraDummy := TDummy.Create(vp);
  vp.AddObject(CameraDummy);

{$ifdef WantOrtho}
  Camera := TFederCamera.Create(vp);
  Camera.WantOrthoProjection := IsOrthoProjection;
  Camera.Reset;
{$else}
  Camera := TCamera.Create(vp);
{$endif}


  CameraDummy.AddObject(Camera);

{$ifdef WantOrtho}
  if Camera.Context <> nil then
  begin
    { Context can be nil if Viewport.Size is still (0, 0) }
    Camera.Context.OnPick := Camera.Pick;
    Camera.Context.OnGetMatrix := Camera.GetProjectionMatrix;
  end;
{$endif}

  if Assigned(vp) then
  begin
    vp.Color := MainVar.ColorScheme.claBackground;
    vp.Camera := Camera;
    if Main.IsDesktop then
      vp.CanFocus := true;
  end;
  Reset;
end;

procedure TFederGraph0.Reset;
begin
  if (CameraDummy = nil) or (Camera = nil) then
    Exit;

  CameraDummy.Position.X := 0;
  CameraDummy.Position.Y := 0;
  CameraDummy.Position.Z := 0;

  CameraDummy.ResetRotationAngle;
  CameraDummy.RotationAngle.X := DefaultRX;
  CameraDummy.RotationAngle.Y := 0;
  CameraDummy.RotationAngle.Z := 0;

  Camera.Position.X := 0.0;
  Camera.Position.Y := 0.0;
  Camera.Position.Z := GlobalZoom;

  Camera.ResetRotationAngle;
  Camera.RotationAngle.X := 180;
  Camera.RotationAngle.Y := 0;
  Camera.RotationAngle.Z := 0;

{$ifdef WantOrtho}
  if IsOrthoProjection then
    ResetOrtho
  else
{$endif}
  FWheelBetrag := Camera.Position.Z;
end;

procedure TFederGraph0.ResetPositionAndRotation;
begin
  if (CameraDummy = nil) or (Camera = nil) then
    Exit;

  CameraDummy.Position.X := 0;
  CameraDummy.Position.Y := 0;
  CameraDummy.Position.Z := 0;

  CameraDummy.ResetRotationAngle;
  CameraDummy.RotationAngle.X := DefaultRX;
  CameraDummy.RotationAngle.Y := 0;
  CameraDummy.RotationAngle.Z := 0;

  Camera.Position.X := 0.0;
  Camera.Position.Y := 0.0;

  Camera.ResetRotationAngle;
  Camera.RotationAngle.X := 180;
  Camera.RotationAngle.Y := 0;
  Camera.RotationAngle.Z := 0;

{$ifdef WantOrtho}
  if IsOrthoProjection then
    ResetOrtho;
{$endif}
end;

procedure TFederGraph0.ResetRotation;
begin
  if (CameraDummy = nil) or (Camera = nil) then
    Exit;

  CameraDummy.ResetRotationAngle;
  CameraDummy.RotationAngle.X := DefaultRX;
  CameraDummy.RotationAngle.Y := 0;
  CameraDummy.RotationAngle.Z := 0;

  Camera.ResetRotationAngle;
  Camera.RotationAngle.X := 180;
  Camera.RotationAngle.Y := 0;
  Camera.RotationAngle.Z := 0;

{$ifdef WantOrtho}
  ResetOrthoRotation;
{$endif}
end;

procedure TFederGraph0.ResetPosition;
begin
  if (CameraDummy = nil) or (Camera = nil) then
    Exit;

  CameraDummy.Position.X := 0;
  CameraDummy.Position.Y := 0;
  CameraDummy.Position.Z := 0;

  Camera.Position.X := 0.0;
  Camera.Position.Y := 0.0;
end;

procedure TFederGraph0.ResetZoom;
begin
  if (CameraDummy = nil) or (Camera = nil) then
    Exit;

  Camera.Position.Z := GlobalZoom;

{$ifdef WantOrtho}
  if IsOrthoProjection then
    ResetOrthoZoom;
{$endif}
end;

procedure TFederGraph0.LoadEulerAngle(ed: TPoint3D);
begin
  if Assigned(CameraDummy)
{$ifdef WantOrtho}
    and (Camera.ViewType = TViewType.Perspective)
{$endif}
  then
  begin
    CameraDummy.ResetRotationAngle;
    CameraDummy.RotationAngle.X := DefaultRX;
    CameraDummy.RotationAngle.X := ed.X;
    CameraDummy.RotationAngle.Y := ed.Y;
    CameraDummy.RotationAngle.Z := ed.Z;
  end;
end;

function TFederGraph0.GetRotationInfoFrame: TPoint3D;
var
  rm: TMatrix3D;
  ax, ay, az: single;
  mt: single;
  ayt: single;
begin
  if CameraDummy = nil then
    Exit;

  rm := CameraDummy.LocalMatrix;

  ay := arctan2(-rm.m31, sqrt(sqr(rm.m32) + sqr(rm.m33)));

  mt := rm.m31;
  { argument mt for ArcSin function must be in range -1..1 }
  if mt > 1 then
    mt := 1
  else if rm.m31 < -1 then
    mt := -1;
  ayt := -ArcSin(mt);
  if TUtils.IsEssentiallyZero(ayt) then
  begin
    az := 0;
    ay := 0;
    ax := 0;
    if (CameraDummy.RotationAngle.Y = 0)
    and  (CameraDummy.RotationAngle.Z = 0)
    then
    begin
      ax := CameraDummy.RotationAngle.X;
      if ax > 180 then
        ax := ax - 360;
      ax := DegToRad(ax);
      ax := -ax;
    end
    else if (CameraDummy.RotationAngle.X = 180)
    and  (CameraDummy.RotationAngle.Y = 0)
    then
    begin
      ax := DegToRad(180);
      az := CameraDummy.RotationAngle.Z;
      if az > 180 then
        az := az - 360;
      az := DegToRad(az);
      az := -az;
    end
  end
  else
  begin
    ax := arctan2(rm.m32, rm.m33);
    az := arctan2(rm.m21, rm.m11);
  end;

  result.X := RadToDeg(ax);
  result.Y := RadToDeg(ay);
  result.Z := RadToDeg(az);
end;

function TFederGraph0.DoCameraZoom(Delta, CameraPositionZ: single): single;
var
  l: single;
begin
  FWheelValue := Abs(CameraPositionZ);
  FWheelDelta := 0.2 * Delta;

  if (Abs(FWheelValue) >= 0.5) then
  begin
    if WantLinearZoom and (Abs(FWheelValue) > 1) then
      FWheelDelta := Sign(FWheelDelta) * 0.05
    else
      FWheelDelta := GlobalZoomSpeed * FWheelValue * Delta; // <-- normal
  end
  else
    FWheelValue := 0.5;

  l := FWheelBetrag + FWheelDelta;

  if (l < GlobalZoomMin) then
    l := GlobalZoomMin
  else if (l > GlobalZoomMax) then
    l := GlobalZoomMax;

  result := l;
  FWheelBetrag := l;
end;

procedure TFederGraph0.HandleKey(KeyChar: Char);
var
  fa: TFederAction;
  Key: Word;
begin
  fa := Main.Keyboard.KeyUpAction(Key, KeyChar, []);
  Key := 0;

  if fa <> faNoop then
    Main.ActionHandler.Execute(fa);
end;

procedure TFederGraph0.SetIsOrthoProjection(const Value: Boolean);
begin
{$ifdef WantOrtho}
  FIsOrthoProjection := Value;

  if Camera = nil then
    Exit;

  if Value then
    Camera.ViewType := TViewType.Orthographic
  else
    Camera.ViewType := TViewType.Perspective;

  Viewport.Repaint;
{$endif}
end;

end.

