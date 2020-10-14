unit RiggVar.FD.Rota;

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
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  RggTypes,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Graph,
  RiggVar.FD.Drawing00,
  RiggVar.FD.Drawings,
  RiggVar.FD.Elements,
  RiggVar.FD.Image,
  RiggVar.FD.TransformHelper,
  FMX.Graphics,
  FMX.Objects;

type
  TRotaForm2 = class(TInterfacedObject, IStrokeRigg)
  private
    FBogen: Boolean;
    FKoppel: Boolean;
    FViewpoint: TViewpoint;
    FFixpoint: TRiggPoint;
    FDarkMode: Boolean;
    FBackgroundColor: TAlphaColor;
    FLegendItemChecked: Boolean;
    FWantLIneColors: Boolean;
    FUseDisplayList: Boolean;
    FUseQuickSort: Boolean;
    FMatrixItemChecked: Boolean;
    FWantOverlayedRiggs: Boolean;
    procedure SetFixpoint(const Value: TRiggPoint);
    procedure SetViewpoint(const Value: TViewpoint);
    procedure SetBogen(const Value: Boolean);
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetControllerTyp(const Value: TControllerTyp);
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
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure Rota3D;
    procedure RotaSeite;
    procedure RotaAchtern;
    procedure RotaTop;
    procedure RotaHelper(aRotX, aRotY, aRotZ, aOffsetY, aRelativeZoom: single);
    procedure SetDarkMode(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetLegendItemChecked(const Value: Boolean);
    procedure SetWantLIneColors(const Value: Boolean);
    procedure SetUseDisplayList(const Value: Boolean);
    procedure SetUseQuickSort(const Value: Boolean);
    procedure SetMatrixItemChecked(const Value: Boolean);
    procedure SetWantOverlayedRiggs(const Value: Boolean);
  protected
    MouseDown: Boolean;
    MousePos: TPointF;
    TH: TTransformHelper;
    DL: TRggDrawings;
    RD: TRggDrawingD00;
    CurrentDrawing: TRggDrawing;
    CurrentElement: TRggElement;
    DrawCounter: Integer;

    procedure ClearImage;
    procedure DrawToCanvas(g: TCanvas);
    procedure DoDrawToCanvas(Sender: TObject);

    procedure DoReset;
    procedure ResetBtnClick(Sender: TObject);
    procedure UpdateFromRiggBtnClick(Sender: TObject);

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  public
    Image: TOriginalImage;
    ZoomIndex: Integer;
    IsUp: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Swap;
    procedure RotateZ(delta: single);
    procedure Zoom(delta: single);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);

    procedure LegendBtnClick(Sender: TObject);
    procedure UseDisplayListBtnClick(Sender: TObject);
    procedure UseQuickSortBtnClick(Sender: TObject);
    procedure MatrixItemClick(Sender: TObject);

    function GetChecked(fa: Integer): Boolean;
    procedure SetChecked(fa: Integer; Value: Boolean);

    procedure Draw;
    procedure ImageScreenScaleChanged(Sender: TObject);

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    function GetMastKurvePoint(const Index: Integer): TPoint3D;

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;
    procedure UpdateHullTexture;
    procedure UpdateCameraX(Delta: single);
    procedure UpdateCameraY(Delta: single);

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

    property LegendItemChecked: Boolean read FLegendItemChecked write SetLegendItemChecked;
    property WantLineColors: Boolean read FWantLineColors write SetWantLineColors;
    property UseDisplayList: Boolean read FUseDisplayList write SetUseDisplayList;
    property UseQuickSort: Boolean read FUseQuickSort write SetUseQuickSort;
    property MatrixItemChecked: Boolean read FMatrixItemChecked write SetMatrixItemChecked;
    property WantOverlayedRiggs: Boolean read FWantOverlayedRiggs write SetWantOverlayedRiggs;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRotaForm }

procedure TRotaForm2.Draw;
begin
  if (Image = nil) or (Image.Bitmap = nil) then
    Exit;
  TH.Draw;
end;

function TRotaForm2.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin

end;

procedure TRotaForm2.Init;
begin

end;

procedure TRotaForm2.Swap;
begin
  Image.OnMouseDown := ImageMouseDown;
  Image.OnMouseMove := ImageMouseMove;
  Image.OnMouseUp := ImageMouseUp;
//  Image.OnMouseWheel := ImageMouseWheel;
  Image.OnScreenScaleChanged := ImageScreenScaleChanged;
end;

function TRotaForm2.QueryRenderOption(const fa: Integer): Boolean;
begin
  case fa of
    faRggBogen: result := FBogen;
    faRggKoppel: result := FKoppel;
    else
      result := False;
  end;
end;

procedure TRotaForm2.RotateZ(delta: single);
begin
  TH.IsRightMouseBtn := True;
  TH.DoOnMouse([], delta, -delta);
end;

procedure TRotaForm2.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TRotaForm2.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
  RD.MK.Visible := FBogen;
  RD.D0D.Visible := not FBogen;
  RD.DC.Visible := not FBogen;
end;

procedure TRotaForm2.SetBtnBlauDown(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetBtnGrauDown(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetControllerTyp(const Value: TControllerTyp);
begin

end;

procedure TRotaForm2.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  DL.UseDarkColorScheme := Value;
  CurrentDrawing.UseDarkColorScheme := Value;
  CurrentDrawing.Colors.BackgroundColor := FBackgroundColor;
  Draw;
end;

procedure TRotaForm2.SetFixpoint(const Value: TRiggPoint);
var
  cr: TRggCircle;
begin
  FFixpoint := Value;

  if CurrentDrawing = nil then
    Exit;

  cr := nil;
  case Value of
    ooN0: ;
    ooA0: cr := RD.A0;
    ooB0: cr := RD.B0;
    ooC0: cr := RD.C0;
    ooD0: cr := RD.D0;
//    ooE0: cr := RggDrawingD00.E0;
//    ooF0: cr := RggDrawingD00.F0;
    ooP0: ;
    ooA: cr := RD.A;
    ooB: cr := RD.B;
    ooC: cr := RD.C;
    ooD: cr := RD.D;
    ooE: ;
    ooF: cr := RD.F;
    ooP: ;
    ooM: ;
  end;

  if cr <> nil then
  begin
    CurrentDrawing.FixPoint := cr.Center.C;
  end;

end;

procedure TRotaForm2.SetGrauZeichnen(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetHullVisible(const Value: Boolean);
begin
end;

procedure TRotaForm2.SetKoordinaten(const Value: TRiggPoints);
begin
  RD.UpdateFromRigg;
  RD.Transform(TH.AccuMatrix);
end;

procedure TRotaForm2.SetKoordinatenE(const Value: TRiggPoints);
begin

end;

procedure TRotaForm2.SetKoordinatenR(const Value: TRiggPoints);
begin

end;

procedure TRotaForm2.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
  RD.KK.Visible := Value;
end;

procedure TRotaForm2.SetKoppelKurve(const Value: TKoordLine);
var
  i: Integer;
  p: TPoint3D;
begin
  if RD.KK = nil then
    Exit;

  p := RD.rP_D0;

  for i := 0 to 100 do
  begin
    RD.KK.RggPoly[i].X := RD.OffsetX + (Value[i].X - p.X) * RD.InitialZoom;
    RD.KK.RggPoly[i].Y := RD.OffsetY - (Value[i].Z - p.Z) * RD.InitialZoom;
    RD.KK.RggPoly[i].Z := 0;
  end;
  RD.KK.Transform;
end;

procedure TRotaForm2.SetMastKurve(const Value: TMastKurve);
begin
  { currently not called from RiggVar.RGMain }
end;

procedure TRotaForm2.SetMastLineData(const Value: TLineDataR100; L, Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: single;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 + Beta);
  temp2 := cos(beta);
  temp3 := sin(pi / 2 + Beta);
  temp4 := sin(beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    RD.MK.RggPoly[j].X := RD.OffsetX + (tempL * temp1 + Value[k] * temp2) * RD.InitialZoom;
    RD.MK.RggPoly[j].Y := RD.OffsetY - (tempL * temp3 + Value[k] * temp4) * RD.InitialZoom;
    RD.MK.RggPoly[j].Z := 0;
  end;
  RD.MK.Transform;
end;

procedure TRotaForm2.SetRiggLED(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetSalingTyp(const Value: TSalingTyp);
begin

end;

procedure TRotaForm2.SetSofortBerechnen(const Value: Boolean);
begin

end;

procedure TRotaForm2.SetViewpoint(const Value: TViewpoint);
begin
  FViewpoint := Value;
  case FViewpoint of
    vpSeite: RotaSeite;
    vpAchtern: RotaAchtern;
    vpTop: RotaTop;
    vp3D: Rota3D;
  end;
end;

procedure TRotaForm2.SetWanteGestrichelt(const Value: Boolean);
begin

end;

procedure TRotaForm2.ToggleRenderOption(const fa: Integer);
begin

end;

procedure TRotaForm2.Zoom(delta: single);
begin
  TH.DoOnMouse([ssShift], delta, -delta * 0.3);
end;

procedure TRotaForm2.ZoomInBtnClick(Sender: TObject);
begin

end;

procedure TRotaForm2.ZoomOutBtnClick(Sender: TObject);
begin

end;

constructor TRotaForm2.Create;
begin
  RD := TRggDrawingD00.Create;
  DL := TRggDrawings.Create;
  DL.UseDarkColorScheme := True;
  DL.Add(RD);

  TH := TTransformHelper.Create;
  TH.OnDrawToCanvas := DoDrawToCanvas;
//  TH.OnShowRotation := DoShowRotation;

  CurrentDrawing := RD;
  TH.CurrentDrawing := CurrentDrawing;
end;

procedure TRotaForm2.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MouseDown := True;
  MousePos.X := X;
  MousePos.Y := Y;
  TH.IsRightMouseBtn := Button = TMouseButton.mbRight;
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRotaForm2.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
 dx, dy: single;
begin
  if not MouseDown then
    Exit;

  dx := X - MousePos.X;
  dy := Y - MousePos.Y;

  TH.DoOnMouse(Shift, dx, dy);

  MousePos.X := X;
  MousePos.Y := Y;
end;

procedure TRotaForm2.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MouseDown := False;
end;

procedure TRotaForm2.UpdateCameraX(Delta: single);
begin
  TH.DoOnMouse([ssCtrl], Delta, 0);
end;

procedure TRotaForm2.UpdateCameraY(Delta: single);
begin
  TH.DoOnMouse([ssCtrl], 0, -Delta);
end;

procedure TRotaForm2.UpdateFromRiggBtnClick(Sender: TObject);
begin
  RD.UpdateFromRigg;
  Draw;
end;

procedure TRotaForm2.UpdateHullTexture;
begin

end;

procedure TRotaForm2.ResetBtnClick(Sender: TObject);
begin
//  Memo.Lines.Clear;
  DoReset;
//  ShowInfo;
end;

//procedure TRotaForm.SwapThickLines;
//var
//  e: TRggLine;
//  st: single;
//begin
//  WantThickLines := not WantThickLines;
//
//  if WantThickLines then
//    st := 6.0
//  else
//    st := 3.0;
//
//  for e in CurrentDrawing.LineList do
//  begin
//    e.StrokeThickness := st;
//  end;
//
//  TH.Rotation := TPoint3D.Zero;
//
//  Draw;
//end;

destructor TRotaForm2.Destroy;
begin
  TH.Free;
  DL.Free;
  inherited;
end;

procedure TRotaForm2.DoReset;
begin
  TH.Reset;
  FixPoint := FFixPoint;
end;

procedure TRotaForm2.ClearImage;
var
  g: TCanvas;
begin
  if (Image = nil) or (Image.Bitmap = nil) then
    Exit;
  DrawCounter := 0;
  g := Image.Bitmap.Canvas;
  g.BeginScene;
  g.Clear(claAliceblue);
  g.EndScene;
  Image.Repaint;
end;

procedure TRotaForm2.DoDrawToCanvas(Sender: TObject);
begin
  DrawToCanvas(Image.Bitmap.Canvas);
end;

procedure TRotaForm2.DrawToCanvas(g: TCanvas);
var
  ss: single;
begin
  ss := Image.Scene.GetSceneScale;
  g.Offset := TH.Offset;
  if g.BeginScene then
  try
    g.Clear(claNull);
    g.SetMatrix(TMatrix.CreateScaling(ss, ss));
    g.Fill.Color := claYellow;
    g.Stroke.Color := claAqua;
    g.Stroke.Thickness := 1.0;
    g.Font.Size := 16;
    g.Font.Family := 'Consolas';
    CurrentDrawing.Draw(g);
  finally
    g.EndScene;
  end;
  Image.Repaint;
end;

procedure TRotaForm2.RotaSeite;
begin
  RotaHelper(0, 0, 0, 0, 1.0);
end;

procedure TRotaForm2.RotaAchtern;
begin
  RotaHelper(0, 90, 0, 0, 1.0)
end;

procedure TRotaForm2.RotaTop;
begin
  RotaHelper(-90, 0, 0, 400, 3.0)
end;

procedure TRotaForm2.Rota3D;
begin
  RotaHelper(-80, 0, 0, 300, 2.5)
end;

procedure TRotaForm2.RotaHelper(aRotX, aRotY, aRotZ, aOffsetY, aRelativeZoom: single);
var
  x, y, z: single;
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  mx := TMatrix3D.CreateRotationX(x);
  my := TMatrix3D.CreateRotationY(y);
  mz := TMatrix3D.CreateRotationZ(z);

  mr := mx * my * mz;

  TH.ResetTransform;

  RD.OffsetX := RD.OffsetXDefault;
  RD.OffsetY := RD.OffsetYDefault + aOffsetY;
  RD.InitialZoom := RD.InitialZoomDefault * aRelativeZoom;
  Main.UpdateStrokeRigg;
  TH.InitTransform(mr);
end;

procedure TRotaForm2.SetLegendItemChecked(const Value: Boolean);
begin
  FLegendItemChecked := Value;
end;

procedure TRotaForm2.SetMatrixItemChecked(const Value: Boolean);
begin
  FMatrixItemChecked := Value;
end;

procedure TRotaForm2.SetUseDisplayList(const Value: Boolean);
begin
  FUseDisplayList := Value;
end;

procedure TRotaForm2.SetUseQuickSort(const Value: Boolean);
begin
  FUseQuickSort := Value;
end;

procedure TRotaForm2.SetWantLineColors(const Value: Boolean);
begin
  FWantLIneColors := Value;
end;

procedure TRotaForm2.SetWantOverlayedRiggs(const Value: Boolean);
begin
  FWantOverlayedRiggs := Value;
end;

procedure TRotaForm2.LegendBtnClick(Sender: TObject);
begin

end;

procedure TRotaForm2.MatrixItemClick(Sender: TObject);
begin

end;

procedure TRotaForm2.UseDisplayListBtnClick(Sender: TObject);
begin

end;

procedure TRotaForm2.UseQuickSortBtnClick(Sender: TObject);
begin

end;

function TRotaForm2.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

procedure TRotaForm2.SetChecked(fa: Integer; Value: Boolean);
begin

end;

procedure TRotaForm2.ImageScreenScaleChanged(Sender: TObject);
begin
  Draw;
end;

end.
