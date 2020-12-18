unit RiggVar.FG.Rota;

interface

{.$define WantOrtho}

uses
  RiggVar.RG.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  System.UIConsts,
  FMX.Layouts,
  System.Rtti,
  System.UITypes,
  FMX.Viewport3D,
  FMX.Types3D,
  RiggVar.Util.Logger,
  RiggVar.FB.Classes,
  RiggVar.FB.Action,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Scheme,
  RiggVar.FG.Graph,
  RiggVar.FG.GridUpdate,
  RiggVar.FG.StrokeGraph,
  RiggVar.RG.Def,
  RiggVar.RG.Graph,
  RiggVar.RG.Main;

type
  TRotaForm3 =  class(TFederGraph, IStrokeRigg)
  private
    StrokeRigg: TStrokeRigg;

    FRetinaScale: single;
    FDarkMode: Boolean;
    FBackgroundColor: TAlphaColor;
    procedure InitStrokeGraph(cr: TStrokeRigg);

    procedure SetBogen(const Value: Boolean);
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetGrauZeichnen(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetKoordinaten(const Value: TRiggPoints);
    procedure SetKoordinatenE(const Value: TRiggPoints);
    procedure SetKoordinatenR(const Value: TRiggPoints);
    procedure SetKoppel(const Value: Boolean);
    procedure SetKoppelKurve(const Value: TKoordLine);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetRiggLED(const Value: Boolean);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetDarkMode(const Value: Boolean);
  public
    ModelID: Integer;
    IsRetina: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure DoOnResize;
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: TFederAction): Boolean;

    procedure RotateZ(Delta: single);
    procedure Zoom(Delta: single);

    procedure Viewpoint3;
    procedure ViewpointA;
    procedure ViewpointS;
    procedure ViewpointT;

    procedure UpdateHullTexture;
    procedure UpdateCameraX(Delta: single);
    procedure UpdateCameraY(Delta: single);

    property RetinaScale: single read FRetinaScale write FRetinaScale;

    procedure Draw;
    procedure InitPosition(w, h, x, y: single);

    function GetMastKurvePoint(const Index: Integer): TPoint3D;
    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;

    procedure DoOnUpdateStrokeRigg;

    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property DarkMode: Boolean read FDarkMode write SetDarkMode;

    property Koordinaten: TRiggPoints write SetKoordinaten;
    property KoordinatenE: TRiggPoints write SetKoordinatenE;
    property KoordinatenR: TRiggPoints write SetKoordinatenR;
    property KoppelKurve: TKoordLine write SetKoppelKurve;
    property MastKurve: TMastKurve write SetMastKurve;
    property WanteGestrichelt: Boolean write SetWanteGestrichelt;
    property Bogen: Boolean write SetBogen;
    property Koppel: Boolean write SetKoppel;
    property HullVisible: Boolean write SetHullVisible;
    property FixPoint: TRiggPoint write SetFixPoint;
    property ViewPoint: TViewPoint write SetViewPoint;
    property SalingTyp: TSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp write SetControllerTyp;
    property RiggLED: Boolean write SetRiggLED;
    property SofortBerechnen: Boolean write SetSofortBerechnen;
    property GrauZeichnen: Boolean write SetGrauZeichnen;
    property BtnGrauDown: Boolean write SetBtnGrauDown;
    property BtnBlauDown: Boolean write SetBtnBlauDown;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRotaForm3 }

constructor TRotaForm3.Create;
begin
  inherited;
end;

destructor TRotaForm3.Destroy;
begin
  StrokeRigg.Free;
  inherited;
end;

procedure TRotaForm3.DoOnResize;
begin
  RetinaScale := Viewport.Scene.GetSceneScale;
  IsRetina := RetinaScale > 1;
  UpdateRasterSize(RetinaScale, MainVar.ClientWidth, MainVar.ClientHeight);
end;

procedure TRotaForm3.DoOnUpdateStrokeRigg;
begin

end;

procedure TRotaForm3.Draw;
begin
  if Assigned(FederMesh) then
  begin
    FederMesh.UpdatePosition;
  end;
  StrokeRigg.Draw;
end;

procedure TRotaForm3.ToggleRenderOption(const fa: Integer);
begin
  StrokeRigg.ToggleRenderOption(fa);
end;

function TRotaForm3.GetChecked(fa: TFederAction): Boolean;
begin
  result := False;
//  case fa of
//    faRggBogen: result := StrokeRigg.Bogen;
//    faRggKoppel: result := StrokeRigg.Koppel;

    { current Viewpoint shall not be indicated }
//    faViewpointS: result := StrokeRigg.ViewPoint = TViewPoint.vpSeite;
//    faViewpointA: result := StrokeRigg.ViewPoint = TViewPoint.vpAchtern;
//    faViewpointT: result := StrokeRigg.ViewPoint = TViewPoint.vpTop;
//    faViewpoint3: result := StrokeRigg.ViewPoint = TViewPoint.vp3D;
//  end;
end;

function TRotaForm3.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  result := StrokeRigg.GetMastKurvePoint(Index);
end;

procedure TRotaForm3.Init;
begin
  RetinaScale := Viewport.Scene.GetSceneScale;
  if RetinaScale > 1 then
    IsRetina := True;

  Main.Logger.InfoVerbose('in TMain.Init');
  Main.Logger.InfoVerbose('  Scale = ' + FloatToStr(RetinaScale));
  Main.Logger.InfoVerbose('  Retina = ' + BoolStr[IsRetina]);

  InitGraph;

  Viewport.OnKeyDown := ViewportKeyDown;
  Viewport.OnMouseDown := ViewportMouseDown;
  Viewport.OnMouseMove := ViewportMouseMove;
  Viewport.OnMouseUp := ViewportMouseUp;
  Viewport.OnMouseWheel := HandleMouseWheel;
  Viewport.OnClick := ViewportClick;

  InitFrame(Viewport);
  UpdateSize(MainVar.ClientWidth, MainVar.ClientHeight);

  InitOK := True;

  { von UpdateTouch }
  if Assigned(Main.FederText) and Main.FederText.InitOK then
  begin
    UpdateRasterSize(RetinaScale, MainVar.ClientWidth, MainVar.ClientHeight);
  end;

  StrokeRigg := TStrokeRigg.Create(Viewport);
  InitStrokeGraph(StrokeRigg);

  UpdateHullTexture;
end;

procedure TRotaForm3.HandleAction(fa: Integer);
begin
  case fa of
    faReset: Reset;
    faResetPosition: ResetPosition;
    faResetZoom: ResetZoom;
{$ifdef WantOrtho}
    faToggleViewType: IsOrthoProjection := not IsOrthoProjection;
{$endif}

    else
      inherited;
  end;
end;

procedure TRotaForm3.ViewpointS;
begin
  ResetRotation;
  if IsOrthoProjection then
  begin
{$ifdef WantOrtho}
    OrthoRotDeltaXY(0, DegToRad(-90));
{$endif}
  end
  else
  begin
    CameraDummy.RotationAngle.X := 90;
  end;
end;

procedure TRotaForm3.ViewpointA;
begin
  ResetRotation;
  if IsOrthoProjection then
  begin
{$ifdef WantOrtho}
    OrthoRotDeltaXY(DegToRad(90), 0);
    OrthoRotDeltaZ(DegToRad(90));
{$endif}
  end
  else
  begin
    CameraDummy.RotationAngle.X := 90;
    CameraDummy.RotationAngle.Y := -90;
  end;
end;

procedure TRotaForm3.ViewpointT;
begin
  ResetRotation;
  if IsOrthoProjection then
  begin
    //do nothing
  end
  else
  begin
    CameraDummy.RotationAngle.X := 0;
  end;
end;

procedure TRotaForm3.Viewpoint3;
begin
  ResetRotation;
  if IsOrthoProjection then
  begin
{$ifdef WantOrtho}
    OrthoRotDeltaXY(0, DegToRad(-5));
{$endif}
  end
  else
  begin
    CameraDummy.RotationAngle.X := 5;
  end;
end;

procedure TRotaForm3.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
//  Viewport.Color := MainVar.ColorScheme.claBackground;
  Viewport.Color := Value;
end;

procedure TRotaForm3.SetBogen(const Value: Boolean);
begin
  StrokeRigg.Bogen := Value;
end;

procedure TRotaForm3.SetBtnBlauDown(const Value: Boolean);
begin
//  StrokeRigg.BtnBlauDown := Value;
end;

procedure TRotaForm3.SetBtnGrauDown(const Value: Boolean);
begin
//  StrokeRigg.BtnGrauDown := Value;
end;

procedure TRotaForm3.SetControllerTyp(const Value: TControllerTyp);
begin
  StrokeRigg.ControllerTyp := Value;
end;

procedure TRotaForm3.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
end;

procedure TRotaForm3.SetFixPoint(const Value: TRiggPoint);
begin
  RggGlobalOffsetX := Main.FixPunkt.X;
  RggGlobalOffsetY := Main.FixPunkt.Y;
  RggGlobalOffsetZ := Main.FixPunkt.Z;
  StrokeRigg.FixPoint := Value;
  Draw;
end;

procedure TRotaForm3.SetGrauZeichnen(const Value: Boolean);
begin
//  StrokeRigg.GrauZeichnen := Value;
end;

procedure TRotaForm3.SetHullVisible(const Value: Boolean);
begin
  StrokeRigg.HullVisible := Value;
  FederMesh.Visible := Value;
end;

procedure TRotaForm3.SetKoordinaten(const Value: TRiggPoints);
begin
  StrokeRigg.Koordinaten := Value;
end;

procedure TRotaForm3.SetKoordinatenE(const Value: TRiggPoints);
begin
  StrokeRigg.KoordinatenE := Value;
end;

procedure TRotaForm3.SetKoordinatenR(const Value: TRiggPoints);
begin
//  StrokeRigg.KoordinatenR := Value;
end;

procedure TRotaForm3.SetKoppel(const Value: Boolean);
begin
//  StrokeRigg.Koppel := Value;
end;

procedure TRotaForm3.SetKoppelKurve(const Value: TKoordLine);
begin
//  StrokeRigg.SetKoppelKurve(Value);
end;

procedure TRotaForm3.SetMastKurve(const Value: TMastKurve);
begin
  StrokeRigg.SetMastKurve(Value);
end;

procedure TRotaForm3.SetMastLineData(const Value: TLineDataR100; L, Beta: single);
begin
  StrokeRigg.SetMastLineData(Value, L, Beta);
end;

procedure TRotaForm3.SetRiggLED(const Value: Boolean);
begin
  StrokeRigg.RiggLED := Value;
end;

procedure TRotaForm3.SetSalingTyp(const Value: TSalingTyp);
begin
  StrokeRigg.SalingTyp := Value;
end;

procedure TRotaForm3.SetSofortBerechnen(const Value: Boolean);
begin
  StrokeRigg.SofortBerechnen := Value;
end;

procedure TRotaForm3.SetViewPoint(const Value: TViewPoint);
begin
  StrokeRigg.ViewPoint := Value;
  case Value of
    vpSeite: ViewpointS;
    vpAchtern: ViewpointA;
    vpTop: ViewpointT;
    vp3D: Viewpoint3;
  end;
end;

procedure TRotaForm3.SetWanteGestrichelt(const Value: Boolean);
begin
  StrokeRigg.WanteGestrichelt := Value;
end;

procedure TRotaForm3.InitStrokeGraph(cr: TStrokeRigg);
begin
  StrokeRigg := cr;

  cr.Scale.X := 1 / RggGlobalScale;
  cr.Scale.Y := cr.Scale.X;
  cr.Scale.Z := cr.Scale.X;

  cr.Width := 1.0;
  cr.Height := 1.0;
  cr.Depth := 1.0;

  ModelGroup.AddObject(cr);
  cr.Init(ModelGroup);

  cr.OnMouseDown := ImageMouseDown;
  cr.OnMouseMove := ImageMouseMove;
  cr.OnMouseUp := ImageMouseUp;
  cr.OnClick := ImageClick;

  cr.HullVisible := FederMesh.Visible;
end;

function TRotaForm3.QueryRenderOption(const fa: Integer): Boolean;
begin
  result := StrokeRigg.QueryRenderOption(fa);
end;

procedure TRotaForm3.RotateZ(Delta: single);
begin
  if Delta > 0 then
    DoMM(fmkRZ, Delta, 0)
  else
    DoMM(fmkRZ, Delta, 0);
end;

procedure TRotaForm3.Zoom(Delta: single);
begin
  if Delta > 0 then
    DoZoom(-0.5)
  else
    DoZoom(0.5);
end;

procedure TRotaForm3.UpdateHullTexture;
begin
  FederMesh.InitTexko;
end;

procedure TRotaForm3.UpdateCameraX(Delta: single);
begin
  Camera.Position.X := Camera.Position.X - Sign(Delta) * 0.05;
end;

procedure TRotaForm3.UpdateCameraY(Delta: single);
begin
  Camera.Position.Y := Camera.Position.Y - Sign(Delta) * 0.05;
end;

procedure TRotaForm3.InitPosition(w, h, x, y: single);
begin
  Camera.Position.X := 0;
  Camera.Position.Y := 0;
end;

end.
