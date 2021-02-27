unit FrmDrawing;

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

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define WantMultipleLists}
{$define WantDrawingList}
{$define WantDynamicFixPoint}
{$define WantMemoOutput}
{$define WantMemo}
{.$define WantBetterListView}

{$define FMX}
{.$define VCL}
{.$define LCL}
{.$define BitmapBGRA}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings,
  RiggVar.FD.Image,
  RiggVar.FD.TransformHelper,
  RiggVar.FB.Color,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Colors,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView;

type
  TFormDrawing = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
{$ifdef WantMemo}
    Memo: TMemo;
{$endif}
{$ifdef WantDrawingList}
    DrawingList: TListView;
{$endif}
    ElementList: TListView;
    Image: TOriginalImage;
    InplaceShape: TCircle;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure ImageScreenScaleChanged(Sender: TObject);

    procedure InplaceShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InplaceShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure InplaceShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

{$ifdef WantDrawingList}
    procedure DrawingListItemClick(const Sender: TObject; const AItem: TListViewItem);
{$endif}
    procedure ElementListChange(Sender: TObject);

    procedure CodeBtnClick(Sender: TObject);
    procedure GlobalShowCaptionBtnClick(Sender: TObject);
    procedure ToggleShowCaptionBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure InitScreenPos;
    procedure InitScreenPos1;
    procedure InitScreenPos2;
  private
    MouseDown: Boolean;
    MousePos: TPointF;
    InplaceMouseDown: Boolean;
    InplaceMousePos: TPointF;
  private
    FormShown: Boolean;
    ListboxWidth: single;
    MemoWidth: single;
    DL: TRggDrawings;
    CurrentDrawing: TRggDrawing;
    CurrentElement: TRggElement;
    TempList: TStringList;
{$ifdef WantMultipleLists}
    WantExampleDrawings: Boolean;
{$endif}
    WantVerticalButtons: Boolean;
    WantThickLines: Boolean;
    procedure InitComponentSize;
    procedure LayoutComponents;
    procedure LayoutComponentsH;
    procedure LayoutComponentsV;
    procedure DrawToCanvas(g: TCanvas);
    procedure CreateComponents;
    procedure InitComponentProps;
    procedure LinkComponents;
{$ifdef WantMemo}
    procedure SetupMemo(MM: TMemo);
{$endif}
    procedure UpdateMemo;
    procedure HandleWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure UpdateInplacePosition;
    procedure InitDrawings;
    procedure SelectDrawing(ii: Integer);
    procedure InitElements;
    procedure SelectElement(ii: Integer);
    procedure ShowDrawingInfo;
    procedure ClearImage;
    procedure InitSpecialButtons;
    procedure DoOnUpdateDrawing(Sender: TObject);
    procedure DoDrawToCanvas(Sender: TObject);
    procedure DoShowRotation(Sender: TObject);
    procedure ShowPoint3D(P: TPoint3D; WantClear: Boolean = True);
  protected
    FAdjustW: Integer;
    FAdjustH: Integer;
    FScreenPosID: Integer;
    FScale: single;
    cr: TControl;
    TempR: single;
    TempB: single;
    FMaxRight: single;
    FMaxBottom: single;
    Margin: single;
    Raster: single;
    BitmapWidth: Integer;
    BitmapHeight: Integer;
    procedure RecordMax;
    procedure AnchorReset(c: TControl);
    procedure AnchorH(c: TControl);
    procedure AnchorV(c: TControl);
    procedure AnchorHV(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
    procedure AdjustWH;
    procedure ResetLayout;
    procedure UpdateLayout(Horz: Boolean);
  protected
    WantMemoOutput: Boolean;
    DrawCounter: Integer;
    ClickCounter: Integer;
    ShowPointCounter: Integer;
    procedure CreateDrawings;
    procedure Draw;
  private
    MemoBackgroundRect: TRectangle;
    procedure SetupMemoBackground(cla: TAlphaColor);
{$ifdef WantBetterListView}
    procedure UpdateListViewColors;
{$endif}
  private
{$ifdef WantMemo}
    CodeBtn: TSpeedButton;
{$endif}
    GlobalShowCaptionBtn: TSpeedButton;
    ToggleShowCaptionBtn: TSpeedButton;
    ResetBtn: TSpeedButton;

    Btn1: TSpeedButton;
    Btn2: TSpeedButton;
    Btn3: TSpeedButton;
    Btn4: TSpeedButton;
    Btn5: TSpeedButton;
    Btn6: TSpeedButton;
    Btn7: TSpeedButton;
    Btn8: TSpeedButton;
    Btn9: TSpeedButton;
    Btn0: TSpeedButton;

    BtnA: TSpeedButton;
    BtnB: TSpeedButton;
    BtnC: TSpeedButton;
    BtnD: TSpeedButton;
    BtnE: TSpeedButton;
    BtnF: TSpeedButton;

    ButtonGroup: TRggButtonGroup;

    DummyControl: TControl;
    procedure CopyBitmap;
  public
    ML: TStrings;
    TH: TTransformHelper;
    procedure SwapDrawingLists;
    procedure SwapLayout;
    procedure SwapColorScheme;
    procedure SwapThickLines;
    procedure ShowInfo;
    procedure DoReset;
    procedure UpdateFixPoint;
  end;

var
  FormDrawing: TFormDrawing;

implementation

{$R *.fmx}

uses
  FMX.Platform,
  RiggVar.FZ.Registry;

procedure TFormDrawing.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    LayoutComponents;
{$ifdef WantDrawingList}
    DrawingList.ItemIndex := DL.DrawingList.Count-1;
    SelectDrawing(DrawingList.ItemIndex);
    DrawingList.SetFocus;
{$else}
    SelectDrawing(TRggDrawingRegistry.DefaultIndex);
{$endif}
  end;
end;

procedure TFormDrawing.CreateDrawings;
begin
  DL := TRggDrawings.Create;
  TRggDrawingRegistry.Init(DL);
  InitDrawings;
end;

procedure TFormDrawing.InitDrawings;
{$ifdef WantDrawingList}
var
  i: Integer;
  li: TListViewItem;
{$endif}
begin
  DL.InitItems(TempList);
{$ifdef WantDrawingList}
  for i := 0 to TempList.Count-1 do
  begin
    li := DrawingList.Items.Add;
    li.Text := TempList[i];
  end;
{$endif}
end;

procedure TFormDrawing.InitElements;
var
  i: Integer;
  li: TListViewItem;
begin
  if CurrentDrawing = nil then
    Exit;

  CurrentDrawing.InitItems(TempList);
  ElementList.Items.Clear;
  for i := 0 to TempList.Count-1 do
  begin
    li := ElementList.Items.Add;
    li.Text := TempList[i];
  end;
end;

procedure TFormDrawing.ShowPoint3D(P: TPoint3D; WantClear: Boolean);
begin
   if not WantMemoOutput then
     Exit;

{$ifdef WantMemoOutput}
  ML.BeginUpdate;
  if WantClear then
    ML.Clear;
  Inc(ShowPointCounter);
  ML.Add(Format('C = %d', [ShowPointCounter]));
  ML.Add(Format('X = %.2f', [P.X]));
  ML.Add(Format('Y = %.2f', [P.Y]));
  ML.Add(Format('Z = %.2f', [P.Z]));
  ML.Add('');
  ML.EndUpdate;
{$endif}
end;

procedure TFormDrawing.DoShowRotation(Sender: TObject);
begin
  ShowPoint3D(TH.RotR, TH.RotB);
end;

procedure TFormDrawing.DoDrawToCanvas(Sender: TObject);
begin
  DrawToCanvas(Image.Bitmap.Canvas);
end;

procedure TFormDrawing.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';

  Caption := 'Rgg test and documentation drawings';

  FScale := Handle.Scale;
  BitmapWidth := 800;
  BitmapHeight := 800;

  InitScreenPos;

  Margin := 10;
  Raster := 70;

  TH := TTransformHelper.Create;
  TH.OnDrawToCanvas := DoDrawToCanvas;
  TH.OnShowRotation := DoShowRotation;
  TRggDrawing.TH := TH;

  TempList := TStringList.Create;

  CreateComponents;
  InitComponentSize;
  LinkComponents;

  Self.OnDestroy := FormDestroy;
  Self.OnShow := FormShow;
  Self.OnKeyUp := FormKeyUp;

{$ifdef WantMemo}
  ML := Memo.Lines;
  ML.Clear;
  SetupMemo(Memo);
  WantMemoOutput := False;
{$ifdef WantMemoOutput}
  WantMemoOutput := True;
{$endif}
{$endif}

  CreateDrawings;

  Image.Width := BitmapWidth;
  Image.Height := BitmapHeight;

  GlobalShowCaptionBtn.StaysPressed := True;
  GlobalShowCaptionBtn.IsPressed := TRggElement.GlobalShowCaption;

  InitComponentProps;
end;

procedure TFormDrawing.FormDestroy(Sender: TObject);
begin
  TH.Free;
  ButtonGroup.Free;
  DL.Free;
  TempList.Free;
end;

procedure TFormDrawing.CreateComponents;
begin
{$ifdef WantDrawingList}
  DrawingList := TListView.Create(Self);
  DrawingList.Parent := Self;
  DrawingList.ItemAppearanceName := 'ListItem';
  DrawingList.ItemAppearance.ItemHeight := 24;
  DrawingList.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
  DrawingList.ItemAppearanceObjects.ItemObjects.Text.Font.Family := 'Consolas';
  DrawingList.ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  DrawingList.ItemAppearanceObjects.ItemObjects.Text.TextColor := TAlphaColors.Dodgerblue;
  DrawingList.ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  DrawingList.ItemAppearanceObjects.FooterObjects.Text.Visible := False;
  DrawingList.OnItemClick := DrawingListItemClick;
{$endif}

  ElementList := TListView.Create(Self);
  ElementList.Parent := Self;
  ElementList.ItemAppearanceName := 'ListItem';
  ElementList.ItemAppearance.ItemHeight := 24;
  ElementList.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
  ElementList.ItemAppearanceObjects.ItemObjects.Text.Font.Family := 'Consolas';
  ElementList.ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  ElementList.ItemAppearanceObjects.ItemObjects.Text.TextColor := TAlphaColors.Orangered;
  ElementList.ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  ElementList.ItemAppearanceObjects.FooterObjects.Text.Visible := False;
  ElementList.OnChange := ElementListChange;

  Image := TOriginalImage.Create(Self, BitmapWidth, BitmapHeight);
  Image.Parent := Self;
  Image.OnMouseDown := ImageMouseDown;
  Image.OnMouseMove := ImageMouseMove;
  Image.OnMouseUp := ImageMouseUp;
  Image.OnMouseWheel := ImageMouseWheel;
  Image.OnScreenScaleChanged := ImageScreenScaleChanged;

{$ifdef WantMemo}
  Memo := TMemo.Create(Self);
  Memo.Parent := Self;
{$endif}

  InplaceShape := TCircle.Create(Self);
  InplaceShape.Parent := Self;
  InplaceShape.AutoCapture := True;
  InplaceShape.Fill.Kind := TBrushKind.None;
  InplaceShape.Width := 50;
  InplaceShape.Height := 50;
  InplaceShape.OnMouseDown := InplaceShapeMouseDown;
  InplaceShape.OnMouseMove := InplaceShapeMouseMove;
  InplaceShape.OnMouseUp := InplaceShapeMouseUp;
  InplaceShape.OnMouseWheel := ImageMouseWheel;

{$ifdef WantMemo}
  CodeBtn := TSpeedButton.Create(Self);
  CodeBtn.Parent := Self;
  CodeBtn.Text := 'Code';
{$endif}

  GlobalShowCaptionBtn := TSpeedButton.Create(Self);
  GlobalShowCaptionBtn.Parent := Self;
  GlobalShowCaptionBtn.Text := 'GSC';

  ToggleShowCaptionBtn := TSpeedButton.Create(Self);
  ToggleShowCaptionBtn.Parent := Self;
  ToggleShowCaptionBtn.Text := 'TSC';

  ResetBtn := TSpeedButton.Create(Self);
  ResetBtn.Parent := Self;
  ResetBtn.Text := 'Reset';

  DummyControl := TControl.Create(Self);
  DummyControl.Parent := Self;
  DummyControl.Visible := False;
  DummyControl.Width := 20;
  DummyControl.Height := 10;

  Btn1 := TSpeedButton.Create(Self);
  Btn1.Parent := Self;
  Btn1.Text := '-X';

  Btn2 := TSpeedButton.Create(Self);
  Btn2.Parent := Self;
  Btn2.Text := '+X';

  Btn3 := TSpeedButton.Create(Self);
  Btn3.Parent := Self;
  Btn3.Text := '-Y';

  Btn4 := TSpeedButton.Create(Self);
  Btn4.Parent := Self;
  Btn4.Text := '+Y';

  Btn5 := TSpeedButton.Create(Self);
  Btn5.Parent := Self;
  Btn5.Text := '-Z';

  Btn6 := TSpeedButton.Create(Self);
  Btn6.Parent := Self;
  Btn6.Text := '+Z';

  Btn7 := TSpeedButton.Create(Self);
  Btn7.Parent := Self;
  Btn7.Text := 'Btn 7';

  Btn8 := TSpeedButton.Create(Self);
  Btn8.Parent := Self;
  Btn8.Text := 'Btn 8';

  Btn9 := TSpeedButton.Create(Self);
  Btn9.Parent := Self;
  Btn9.Text := 'Btn 9';

  Btn0 := TSpeedButton.Create(Self);
  Btn0.Parent := Self;
  Btn0.Text := 'Btn 0';

  BtnA := TSpeedButton.Create(Self);
  BtnA.Parent := Self;
  BtnA.Text := 'A';

  BtnB := TSpeedButton.Create(Self);
  BtnB.Parent := Self;
  BtnB.Text := 'B';

  BtnC := TSpeedButton.Create(Self);
  BtnC.Parent := Self;
  BtnC.Text := 'C';

  BtnD := TSpeedButton.Create(Self);
  BtnD.Parent := Self;
  BtnD.Text := 'D';

  BtnE := TSpeedButton.Create(Self);
  BtnE.Parent := Self;
  BtnE.Text := 'E';

  BtnF := TSpeedButton.Create(Self);
  BtnF.Parent := Self;
  BtnF.Text := 'F';

  ButtonGroup := TRggButtonGroup.Create;
  ButtonGroup.OnUpdateDrawing := DoOnUpdateDrawing;

  ButtonGroup.Btn1 := Btn1;
  ButtonGroup.Btn2 := Btn2;
  ButtonGroup.Btn3 := Btn3;
  ButtonGroup.Btn4 := Btn4;
  ButtonGroup.Btn5 := Btn5;
  ButtonGroup.Btn6 := Btn6;
  ButtonGroup.Btn7 := Btn7;
  ButtonGroup.Btn8 := Btn8;
  ButtonGroup.Btn9 := Btn9;
  ButtonGroup.Btn0 := Btn0;

  ButtonGroup.BtnA := BtnA;
  ButtonGroup.BtnB := BtnB;
  ButtonGroup.BtnC := BtnC;
  ButtonGroup.BtnD := BtnD;
  ButtonGroup.BtnE := BtnE;
  ButtonGroup.BtnF := BtnF;
end;

procedure TFormDrawing.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkEscape: DoReset;
    vkF1: ShowInfo;
    vkF2: SwapColorScheme;
    vkF3: SwapThickLines;
    vkF6: SwapDrawingLists;
    vkF11: SwapLayout;
  end;

  case KeyChar of
    'C': SwapColorScheme;
    'G': GlobalShowCaptionBtnClick(nil);
    'R': DoReset;
{$ifdef WantMultipleLists}
    'L': SwapDrawingLists;
{$endif}
    'T': SwapLayout;
    'W': SwapThickLines;

    'c': CodeBtnClick(nil);
    'h': UpdateLayout(True);
    'i': ShowInfo;
    't': ToggleShowCaptionBtnClick(nil);
    'v': UpdateLayout(False);

    'Ü': CopyBitmap;
  end;
end;

procedure TFormDrawing.ImageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  HandleWheel(Sender, Shift, WheelDelta, Handled);
  Handled := True;
end;

procedure TFormDrawing.HandleWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  t: single;
  cr: TRggCircle;
begin
  if CurrentElement = nil then
    Exit;
  if CurrentDrawing = nil then
    Exit;

  if WheelDelta > 0 then
    t := 1
  else
    t := -1;

  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
      CurrentElement.Param8(t) // Text Radius
    else
      CurrentElement.Param7(t * 5); // Text Angle
  end
  else
  begin
    if ssShift in Shift then
    begin
      CurrentElement.Param2(t * 5);
    end
    else if ssAlt in Shift then
    begin
      CurrentElement.Param3(t * 5);
    end
    else
    begin
      CurrentElement.Param1(t * 5);
    end;

    CurrentDrawing.Compute;

    if CurrentElement is TRggCircle then
    begin
      cr := CurrentElement as TRggCircle;
      { remember that Fixpoint needs to be updated later, after drawing. }
      CurrentDrawing.WheelFlag := True;
      { transform only this point using the acumulated matrix }
      TRggCircle.Matrix := TH.AccuMatrix;
      cr.Transform;
    end;
  end;

  { this will transform all points using incremental NewMatrix }
  Draw;
end;

procedure TFormDrawing.GlobalShowCaptionBtnClick(Sender: TObject);
begin
  TRggElement.GlobalShowCaption := not TRggElement.GlobalShowCaption;
  Draw;
end;

procedure TFormDrawing.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormDrawing.CodeBtnClick(Sender: TObject);
begin
{$ifdef WantMemo}
  if CurrentDrawing <> nil then
  begin
    Memo.Lines.Clear;
    CurrentDrawing.WriteCode(Memo.Lines);
  end;
{$endif}
end;

{$ifdef WantMemo}
procedure TFormDrawing.SetupMemo(MM: TMemo);
begin
  if MM = nil then
    Exit;

{$ifdef FMX}
  MM.ControlType := TControlType.Styled;
  MM.StyledSettings := [];
  MM.ShowScrollBars := True;
  MM.TextSettings.Font.Family := 'Consolas';
  MM.TextSettings.Font.Size := 14;
  MM.TextSettings.FontColor := claDodgerblue;
{$endif}

{$ifdef VCL}
  MM.Parent := Self;
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := TScrollStyle.ssBoth;
{$endif}

{$ifdef LCL}
  MM.Parent := Self;
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := TScrollStyle.ssBoth;
{$endif}
end;
{$endif}

procedure TFormDrawing.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormDrawing.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormDrawing.ToggleShowCaptionBtnClick(Sender: TObject);
begin
  if CurrentElement <> nil then
  begin
    CurrentElement.ShowCaption := not CurrentElement.ShowCaption;
    Draw;
  end;
end;

procedure TFormDrawing.ResetBtnClick(Sender: TObject);
begin
{$ifdef WantMemo}
  Memo.Lines.Clear;
{$endif}
  DoReset;
  ShowInfo;
end;

procedure TFormDrawing.AnchorReset(c: TControl);
begin
  c.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
end;

procedure TFormDrawing.AnchorV(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
end;

procedure TFormDrawing.AnchorH(c: TControl);
begin
  c.Width := ClientWidth - c.Position.X - Margin;
  c.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
end;

procedure TFormDrawing.AnchorHV(c: TControl);
begin
  c.Width := ClientWidth - c.Position.X - Margin;
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
end;

procedure TFormDrawing.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MouseDown := True;
  MousePos.X := X;
  MousePos.Y := Y;
  TH.IsRightMouseBtn := Button = TMouseButton.mbRight;
  TH.Rotation := TPoint3D.Zero;
end;

procedure TFormDrawing.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
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

procedure TFormDrawing.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MouseDown := False;
end;

procedure TFormDrawing.InitComponentProps;
begin
  ToggleShowCaptionBtn.Hint := 'toggle ShowCaption for RggElement';
  GlobalShowCaptionBtn.Hint := 'toggle global ShowCaption';
{$ifdef WantMemo}
  CodeBtn.Hint := 'write Code to Memo';
{$endif}
  ResetBtn.Hint := 'Reset Transform';
end;

procedure TFormDrawing.InitComponentSize;
var
  w: single;
begin
  w := 40;

{$ifdef WantMemo}
  CodeBtn.Width := w;
{$endif}
  GlobalShowCaptionBtn.Width := w;
  ToggleShowCaptionBtn.Width := w;
  ResetBtn.Width := w;

  Btn1.Width := w;
  Btn2.Width := w;
  Btn3.Width := w;
  Btn4.Width := w;
  Btn5.Width := w;
  Btn6.Width := w;
  Btn7.Width := w;
  Btn8.Width := w;
  Btn9.Width := w;
  Btn0.Width := w;

  BtnA.Width := w;
  BtnB.Width := w;
  BtnC.Width := w;
  BtnD.Width := w;
  BtnE.Width := w;
  BtnF.Width := w;

  ListboxWidth := 200;
  MemoWidth := 220;

{$ifdef WantDrawingList}
  DrawingList.Width := ListboxWidth;
  DrawingList.Height := 300;
{$endif}

  ElementList.Width := ListboxWidth;
  ElementList.Height := 100;

{$ifdef WantMemo}
  Memo.Width := MemoWidth;
{$endif}
end;

procedure TFormDrawing.InplaceShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  InplaceMouseDown := True;
  InplaceMousePos.X := X;
  InplaceMousePos.Y := Y;
end;

procedure TFormDrawing.InplaceShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
 dx, dy, dt: single;
{$ifdef WantDynamicFixPoint}
 cr: TRggCircle;
{$endif}
begin
  if not InplaceMouseDown then
    Exit;
  if CurrentElement = nil then
    Exit;
  Assert(CurrentElement is TRggCircle);

  dx := X - InplaceMousePos.X;
  dy := Y - InplaceMousePos.Y;

  dt := 10;
  if dx > dt then
    dx := dt;
  if dx < -dt then
    dx := -dt;
  if dy > dt then
    dy := dt;
  if dy < -dt then
    dy := -dt;

{$ifdef WantDynamicFixPoint}
  { InplaceShape.Position is updated in DrawToCanvas }
  cr := CurrentElement as TRggCircle;

  cr.Param1I(dx);
  cr.Param2I(dy);

  CurrentDrawing.InplaceFlag := True;
  cr.Matrix := TH.BuildMatrixI;
  CurrentDrawing.Compute;
  cr.TransformI;

  Draw;

  if WantMemoOutput then
  begin
    ML.Add(Format('dx = %.2f', [dx]));
    ML.Add(Format('dy = %.2f', [dy]));
  end;

{$else}
  InplaceShape.Position.X := InplaceShape.Position.X + dx;
  InplaceShape.Position.Y := InplaceShape.Position.Y + dy;

  CurrentElement.Param1(dx);
  CurrentElement.Param2(dy);

  CurrentDrawing.Compute;
  Draw;
{$endif}
end;

procedure TFormDrawing.InplaceShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  InplaceMouseDown := False;
end;

procedure TFormDrawing.LayoutComponents;
begin
  LayoutComponentsH;
end;

procedure TFormDrawing.LayoutComponentsH;
begin
  ResetLayout;

{$ifdef WantMemo}
  cr := CodeBtn;
  cr.Position.X := Margin;
  cr.Position.Y := Margin;
  StackH(GlobalShowCaptionBtn);
{$else}
  cr := GlobalShowCaptionBtn;
  cr.Position.X := Margin;
  cr.Position.Y := Margin;
{$endif}
  StackH(ToggleShowCaptionBtn);
  StackH(ResetBtn);

  StackH(DummyControl);

  StackH(Btn1);
  StackH(Btn2);
  StackH(Btn3);
  StackH(Btn4);
  StackH(Btn5);
  StackH(Btn6);
  StackH(Btn7);
  StackH(Btn8);
  StackH(Btn9);
  StackH(Btn0);

  StackH(DummyControl);

  StackH(BtnA);
  StackH(BtnB);
  StackH(BtnC);
  StackH(BtnD);
  StackH(BtnE);
  StackH(BtnF);

{$ifdef WantMemo}
  cr := CodeBtn;
{$else}
  cr := GlobalShowCaptionBtn;
{$endif}
{$ifdef WantDrawingList}
  StackV(DrawingList);
  StackH(ElementList);
{$else}
  StackV(ElementList);
{$endif}
  StackH(Image);
{$ifdef WantMemo}
  StackH(Memo);
{$endif}

  AdjustWH;

{$ifdef WantDrawingList}
  AnchorV(DrawingList);
{$endif}
  AnchorV(ElementList);
  AnchorV(Image);
{$ifdef WantMemo}
  AnchorHV(Memo);
{$endif}
end;

procedure TFormDrawing.LayoutComponentsV;
begin
  ResetLayout;

{$ifdef WantMemo}
  cr := CodeBtn;
  cr.Position.X := Margin;
  cr.Position.Y := Margin;
  StackV(GlobalShowCaptionBtn);
{$else}
  cr := GlobalShowCaptionBtn;
  cr.Position.X := Margin;
  cr.Position.Y := Margin;
{$endif}
  StackV(ToggleShowCaptionBtn);
  StackV(ResetBtn);

  StackV(DummyControl);

  StackV(Btn1);
  StackV(Btn2);
  StackV(Btn3);
  StackV(Btn4);
  StackV(Btn5);
  StackV(Btn6);
  StackV(Btn7);
  StackV(Btn8);
  StackV(Btn9);
  StackV(Btn0);

  StackV(DummyControl);

  StackV(BtnA);
  StackV(BtnB);
  StackV(BtnC);
  StackV(BtnD);
  StackV(BtnE);
  StackV(BtnF);

{$ifdef WantMemo}
  cr := CodeBtn;
{$else}
  cr := GlobalShowCaptionBtn;
{$endif}
{$ifdef WantDrawingList}
  StackH(DrawingList);
{$endif}
  StackH(ElementList);
{$ifdef WantMemo}
  StackH(Memo);
{$endif}
  StackH(Image);

  AdjustWH;

{$ifdef WantDrawingList}
  AnchorV(DrawingList);
{$endif}
  AnchorV(ElementList);
{$ifdef WantMemo}
  AnchorV(Memo);
{$endif}
  AnchorHV(Image);
end;

procedure TFormDrawing.AdjustWH;
var
  ss: single;
  w, h: Integer;
  d: TDisplay;
  wa: TRect;
begin
  ss := Handle.Scale;
  w := Round(FMaxRight + Margin);
  h := Round(FMaxBottom + Margin);

  d := Screen.DisplayFromForm(self);
  wa := d.WorkArea;

  if (Left + w) * ss > wa.Right then
  begin
    Width := Round((wa.Right - Left) / ss);
    FAdjustW := 1;
  end
  else
  begin
    ClientWidth := w;
    FAdjustW := 2;
    if (Left + w) * ss > wa.Right - (Width - ClientWidth) then
    begin
      Width := Round((wa.Right - Left) / ss);
    end
  end;

  if (Top + h) * ss > wa.Bottom then
  begin
    Height := Round((wa.Bottom - Top) / ss);
    FAdjustH := 1;
  end
  else
  begin
    ClientHeight := h;
    FAdjustH := 2;
    if (Top + h) * ss > wa.Bottom - (Height - ClientHeight) then
    begin
      Height := Round((wa.Bottom - Top) / ss);
    end;
  end;
end;

procedure TFormDrawing.LinkComponents;
begin
{$ifdef WantMemo}
  CodeBtn.OnClick := CodeBtnClick;
{$endif}
  GlobalShowCaptionBtn.OnClick := GlobalShowCaptionBtnClick;
  ToggleShowCaptionBtn.OnClick := ToggleShowCaptionBtnClick;
  ResetBtn.OnClick := ResetBtnClick;

  Image.OnMouseDown := ImageMouseDown;
  Image.OnMouseMove := ImageMouseMove;
  Image.OnMouseUp := ImageMouseUp;

  InplaceShape.OnMouseDown := InplaceShapeMouseDown;
  InplaceShape.OnMouseMove := InplaceShapeMouseMove;
  InplaceShape.OnMouseUp := InplaceShapeMouseUp;
end;

{$ifdef WantDrawingList}
procedure TFormDrawing.DrawingListItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  ii: Integer;
begin
  ii := AItem.Index;
  if ii > -1 then
  begin
    SelectDrawing(ii);
  end;
end;
{$endif}

procedure TFormDrawing.SelectDrawing(ii: Integer);
begin
  CurrentElement := nil;

  if (ii > -1) and (ii < DL.DrawingList.Count) then
  begin
    TH.ResetTransform;
    CurrentDrawing := DL.DrawingList[ii];
    TH.CurrentDrawing := CurrentDrawing;
    ShowDrawingInfo;
    InitSpecialButtons;
    if CurrentDrawing.IsValid then
    begin
      InitElements;
      CurrentDrawing.Compute;
      CurrentDrawing.IsDark := DL.UseDarkColorScheme;
      Draw;
      ElementList.ItemIndex := CurrentDrawing.DefaultElementIndex;
      SelectElement(CurrentDrawing.DefaultElementIndex);
    end
    else
    begin
      ClearImage;
    end;
  end;
end;

procedure TFormDrawing.ElementListChange(Sender: TObject);
var
  ii: Integer;
begin
  if ElementList.Selected = nil then
    Exit;

  ii := ElementList.Selected.Index;
  if ii > -1 then
    SelectElement(ii);
end;

procedure TFormDrawing.SelectElement(ii: Integer);
{$ifdef WantDynamicFixPoint}
var
  cr: TRggCircle;
{$endif}
begin
  Inc(ClickCounter);

  if CurrentDrawing = nil then
    Exit;

  if ii > -1 then
  begin
    CurrentElement := CurrentDrawing.Element[ii];
{$ifdef WantDynamicFixPoint}
    if CurrentElement is TRggCircle then
    begin
      cr := CurrentElement as TRggCircle;
      CurrentDrawing.FixPoint3D := cr.Center.C;
      TH.AccuMatrix := TH.BuildMatrixF;
      cr.Matrix := TH.BuildMatrixI;
      cr.Save;
      cr.TransformI;
    end;
{$endif}
    Draw;
  end;
end;

procedure TFormDrawing.UpdateInplacePosition;
var
  cr: TRggCircle;
begin
  if CurrentElement = nil then
  begin
    InplaceShape.Visible  := False;
    Exit;
  end;

  if CurrentElement.IsComputed then
  begin
    InplaceShape.Visible  := False;
    Exit;
  end;

  if CurrentElement is TRggCircle then
  begin
    cr := CurrentElement as TRggCircle;
    InplaceShape.Position.X := Image.Position.X + TH.Offset.X + cr.Center.X - InplaceShape.Width / 2;
    InplaceShape.Position.Y := Image.Position.Y + TH.Offset.Y + cr.Center.Y - InplaceShape.Height / 2;
    InplaceShape.Visible := True;

{$ifdef WantDynamicFixPoint}
    if CurrentDrawing.InplaceFlag then
    begin
      TH.AccuMatrix := TH.BuildMatrixG(cr.Center.C);
    end;

    if CurrentDrawing.WheelFlag then
    begin
      CurrentDrawing.FixPoint3D := cr.Center.C;
    end;

    CurrentDrawing.InplaceFlag := False;
    CurrentDrawing.WheelFlag := False;
{$endif}

    Exit;
  end;

  InplaceShape.Visible  := False;
end;

procedure TFormDrawing.ClearImage;
var
  g: TCanvas;
begin
  if (Image = nil) then
    Exit;
  DrawCounter := 0;
  g := Image.Bitmap.Canvas;
  g.BeginScene;
  g.Clear(claAliceblue);
  g.EndScene;
  Image.Repaint;
end;

procedure TFormDrawing.Draw;
begin
  if (Image = nil) then
    Exit;
  TH.Draw;
end;

procedure TFormDrawing.DrawToCanvas(g: TCanvas);
var
{$ifdef WantDynamicFixPoint}
  cr: TRggCircle;
{$endif}
{$ifdef FMX}
  ss: single;
{$endif}
begin
{$ifdef WantDynamicFixPoint}
  if CurrentElement is TRggCircle then
  begin
    Inc(DrawCounter);
    cr := CurrentElement as TRggCircle;
    if WantMemoOutput then
    begin
      ML.Clear;
      ML.Add('DrawCounter = ' + IntToStr(DrawCounter));
      ML.Add(cr.Caption + '.OriginalCenter:');
      ShowPoint3D(cr.OriginalCenter.C, False);
      ML.Add(cr.Caption + '.Center:');
      ShowPoint3D(cr.Center.C, False);
    end;
  end;
{$endif}

  UpdateInplacePosition;
  UpdateMemo;

{$ifdef FMX}
  ss := Image.Scene.GetSceneScale;
  if g.BeginScene then
  try
    g.SetMatrix(TMatrix.CreateScaling(ss, ss));
    g.Clear(CurrentDrawing.Colors.BackgroundColor);
    g.Fill.Color := claYellow;
    g.Stroke.Color := claAqua;
    g.Stroke.Thickness := 1.0;
    g.Font.Size := 16;
    g.Font.Family := 'Consolas';
    CurrentDrawing.FaxPoint3D.C := TH.Offset;
    CurrentDrawing.Draw(g);
  finally
    g.EndScene;
  end;
  Image.Repaint;
{$endif}
end;

procedure TFormDrawing.UpdateMemo;
begin
{$ifdef WantMemo}
  if CurrentDrawing.WantMemoLines then
    Memo.Text := CurrentDrawing.ML.Text;
{$endif}
end;

procedure TFormDrawing.ShowInfo;
begin
  if not WantMemoOutput then
    Exit;
  if ML = nil then
    Exit;

{$ifdef FMX}
  ML.Add('W and H:');
  ML.Add(Format('PosID  = %d, %d, %d', [FScreenPosID, FAdjustW, FAdjustH]));
  ML.Add(Format('Screen = (%d, %d)', [Screen.Width, Screen.Height]));
  ML.Add(Format('WA     = (%d, %d)', [Screen.WorkAreaWidth, Screen.WorkAreaHeight]));
  ML.Add(Format('FormLT = (%d, %d)', [Left, Top]));
  ML.Add(Format('FormWH = (%d, %d)', [Width, Height]));
  ML.Add(Format('Client = (%d, %d)', [ClientWidth, ClientHeight]));
  ML.Add(Format('Bitmap = (%d, %d)', [Image.Bitmap.Width, Image.Bitmap.Height]));
  ML.Add(Format('Image  = (%.1f, %.1f)', [Image.Width, Image.Height]));
{$ifdef WantMemo}
  ML.Add(Format('Memo   = (%.1f, %.1f)', [Memo.Width, Memo.Height]));
{$endif}
{$ifdef WantDrawingList}
  ML.Add(Format('DL     = (%.1f, %.1f)', [DrawingList.Width, DrawingList.Height]));
{$endif}
  ML.Add(Format('Scale  = %.2f', [Handle.Scale]));
  ML.Add(Format('ImgSS  = %.2f', [Image.ScreenScale]));
  ML.Add(Format('R1     = (%.1f, %.1f)', [Image.R1.Width, Image.R1.Height]));
  ML.Add(Format('R2     = (%.1f, %.1f)', [Image.R2.Width, Image.R2.Height]));
  ML.Add(Format('IR     = (%.1f, %.1f)', [Image.IR.Width, Image.IR.Height]));
  ML.Add('');
{$endif}
end;

procedure TFormDrawing.ShowDrawingInfo;
begin
  if not WantMemoOutput then
    Exit;

  if not CurrentDrawing.IsValid then
  begin
    ML.Clear;
    ML.Add(CurrentDrawing.Name);
    ML.Add('Invalid');
    CurrentDrawing.GetInfo(ML);
  end;
end;

procedure TFormDrawing.InitSpecialButtons;
begin
  if ButtonGroup = nil then
    Exit;
  CurrentDrawing.InitButtons(ButtonGroup);
end;

procedure TFormDrawing.DoOnUpdateDrawing(Sender: TObject);
begin
  CurrentDrawing.Compute;
  Draw;
end;

procedure TFormDrawing.SwapDrawingLists;
var
  WasDark: Boolean;
begin
  WasDark := DL.UseDarkColorScheme;

  CurrentDrawing := nil;
  CurrentElement := nil;

{$ifdef WantDrawingList}
  DrawingList.Items.Clear;
{$endif}
  ElementList.Items.Clear;

  DL.Free;
  DL := TRggDrawings.Create;

{$ifdef WantMultipleLists}
  WantExampleDrawings := not WantExampleDrawings;
  if WantExampleDrawings then
    TRggDrawingRegistry.InitFD(DL)
  else
    TRggDrawingRegistry.InitFZ(DL);
{$else}
  TRggDrawingRegistry.Init(DL);
{$endif}

  InitDrawings;
  SelectDrawing(TRggDrawingRegistry.DefaultIndex);

  DL.UseDarkColorScheme := not WasDark;
  SwapColorScheme;
end;

procedure TFormDrawing.ResetLayout;
begin
  AnchorReset(Image);
{$ifdef WantDrawingList}
  AnchorReset(DrawingList);
{$endif}
  AnchorReset(ElementList);
{$ifdef WantMemo}
  AnchorReset(Memo);
{$endif}
{$ifdef WantDrawingList}
  DrawingList.Width := ListboxWidth;
  DrawingList.Height := 200;
{$endif}

  ElementList.Height := ListboxWidth;
  ElementList.Height := 200;

  Image.Width := BitmapWidth;
  Image.Height := BitmapHeight;

{$ifdef WantMemo}
  Memo.Width := MemoWidth;
  Memo.Height := 200;
{$endif}
  FMaxRight := 0;
  FMaxBottom := 0;
end;

procedure TFormDrawing.SwapLayout;
begin
  WantVerticalButtons := not WantVerticalButtons;
  if WantVerticalButtons then
    LayoutComponentsV
  else
    LayoutComponentsH;

  Draw;
end;

procedure TFormDrawing.SwapColorScheme;
begin
  DL.UseDarkColorScheme := not DL.UseDarkColorScheme;
  CurrentDrawing.IsDark := DL.UseDarkColorScheme;

  if DL.UseDarkColorScheme then
  begin
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := claGray;
    InplaceShape.Stroke.Color := claAntiquewhite;
    SetupMemoBackground(claGray);
  end
  else
  begin
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := TRggColors.Windowgray;
    InplaceShape.Stroke.Color := claBlack;
    SetupMemoBackground(claWhite);
  end;

{$ifdef WantBetterListView}
  UpdateListViewColors;
{$endif}

  Draw;
end;

{$ifdef WantBetterListView}
procedure TFormDrawing.UpdateListViewColors;
var
  cla: TAlphaColor;
  LV: TListView;
begin
  if DL.UseDarkColorScheme then
  begin
    cla := CurrentDrawing.Colors.BackgroundColor;

{$ifdef WantDrawingList}
    LV := DrawingList;
    LV.SetColorItemFill(cla);
    LV.SetColorBackground(claDimGray);
    LV.SetColorItemSeparator(claDarkgray);
    LV.SetColorItemSelected(claDarkgray);
    LV.SetColorText(claAquamarine);
{$endif}

    LV := ElementList;
    LV.SetColorItemFill(cla);
    LV.SetColorBackground(claDimGray);
    LV.SetColorItemSeparator(claDarkGray);
    LV.SetColorItemSelected(claDarkgray);
    LV.SetColorText(claAntiquewhite);
  end
  else
  begin
    cla := claWhite;

{$ifdef WantDrawingList}
    LV := DrawingList;
    LV.SetColorBackground(cla);
    LV.SetColorItemFill(cla);
    LV.SetColorItemSeparator(TRggColors.Windowgray);
    LV.SetColorItemSelected(TRggColors.Azure);
    LV.SetColorText(claDodgerblue);
{$endif}

    LV := ElementList;
    LV.SetColorBackground(cla);
    LV.SetColorItemFill(cla);
    LV.SetColorItemSeparator(TRggColors.Windowgray);
    LV.SetColorItemSelected(TRggColors.Aliceblue);
    LV.SetColorText(claTomato);
  end;
end;
{$endif}

procedure TFormDrawing.SwapThickLines;
var
  e: TRggLine;
  st: single;
begin
  WantThickLines := not WantThickLines;

  if WantThickLines then
    st := 6.0
  else
    st := 3.0;

  for e in CurrentDrawing.LineList do
  begin
    e.StrokeThickness := st;
  end;

  TH.Rotation := TPoint3D.Zero;

  Draw;
end;

procedure TFormDrawing.DoReset;
begin
  TH.Reset;
  UpdateFixPoint;
end;

procedure TFormDrawing.UpdateFixPoint;
var
  cr: TRggCircle;
begin
  if CurrentDrawing = nil then
    Exit;
  if CurrentElement = nil then
    Exit;
  if CurrentElement is TRggCircle then
  begin
    cr := CurrentElement as TRggCircle;
    CurrentDrawing.FixPoint3D := cr.Center.C;
  end;
end;

procedure TFormDrawing.InitScreenPos;
begin
  if FScale = 1.0 then
    InitScreenPos1
  else
    InitScreenPos2;
end;

procedure TFormDrawing.InitScreenPos1;
begin
  if (Screen.Width >= 1920) and (Screen.Height >= 1024) then
  begin
    { Normal HD screen with FScale = 1.0 }
    Left := 200;
    Top := 50;
    Width := 1000;
    Height := 960;
    FScreenPosID := 1;
  end
  else
  begin
    { smaller screen at }
    Left := 20;
    Top := 20;
    Width := 1000;
    Height := 700;
    FScreenPosID := 2;
  end;
end;

procedure TFormDrawing.InitScreenPos2;
begin
  if (Screen.Width >= FScale * 1920) and (Screen.Height >= FScale * 1024) then
  begin
    Left := 200;
    Top := 50;
    Width := 1540;
    Height := 860;
    FScreenPosID := 3;
  end
  else
  begin
    { Microsoft Surface Tablet with FScale = 2.0 }
    Left := 0;
    Top := 0;
    Width := 1350; { maximal 2736 div 2 = 1368 }
    Height := 750; { maximal 1744 div 2 = 872 based on Screen.WorkAreaHeight }
    FScreenPosID := 4;
  end;
end;

procedure TFormDrawing.UpdateLayout(Horz: Boolean);
begin
  if Horz then
    LayoutComponentsH
  else
    LayoutComponentsV;

  UpdateInplacePosition;

  ML.Clear;
  ShowInfo;
end;

procedure TFormDrawing.ImageScreenScaleChanged(Sender: TObject);
begin
  Draw;
end;

procedure TFormDrawing.CopyBitmap;
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(Image.Bitmap);
end;

procedure TFormDrawing.SetupMemoBackground(cla: TAlphaColor);
var
  cr: TFmxObject;
  R: TRectangle;
begin
  if not WantMemoOutput then
    Exit;

  if MemoBackgroundRect = nil then
  begin
    cr := Memo.FindStyleResource('background');
    if cr <> nil then
    begin
      R := TRectangle.Create(cr);
      cr.AddObject(R);
      R.Align := TAlignLayout.Client;
      R.Fill.Color := cla;
      R.HitTest := False;
      R.SendToBack;
      MemoBackgroundRect := R;
    end
    else
    begin
      WantMemoOutput := False;
      Exit;
    end;
  end;

  if MemoBackgroundRect = nil then
    Exit;

  R := MemoBackgroundRect;

  if DL.UseDarkColorScheme then
  begin
    Memo.TextSettings.FontColor := claAntiquewhite;
    R.Fill.Color := cla;
  end
  else
  begin
    Memo.TextSettings.FontColor := claDodgerBlue;
    R.Fill.Color := cla;
  end;

end;

end.
