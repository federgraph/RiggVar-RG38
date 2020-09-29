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

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  RiggVar.FD.TransformHelper,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings,
  RiggVar.FD.Drawing00,
  RiggVar.FD.Image,
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

{$define FMX}

type
  TFormDrawing = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    Memo: TMemo;
    DrawingList: TListView;
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

    procedure DrawingListItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ElementListChange(Sender: TObject);

    procedure UpdateFromRiggBtnClick(Sender: TObject);
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
    WantExampleDrawings: Boolean;
    WantVerticalButtons: Boolean;
    WantThickLines: Boolean;
    procedure InitComponentSize;
    procedure UpdateLayout(Horz: Boolean);
    procedure LayoutComponents;
    procedure LayoutComponentsH;
    procedure LayoutComponentsV;
    procedure DrawToCanvas(g: TCanvas);
    procedure CreateComponents;
    procedure InitComponentProps;
    procedure LinkComponents;
    procedure SetupMemo(MM: TMemo);
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
  private
    RggDrawingD00: TRggDrawingD00;
    DrawCounter: Integer;
    ClickCounter: Integer;
    procedure CreateDrawings;
    procedure Draw;
  private
    UpdateFromRiggBtn: TSpeedButton;
    CodeBtn: TSpeedButton;
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
  public
    ML: TStrings;
    TH: TTransformHelper;
    procedure SwapDrawingLists;
    procedure SwapLayout;
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
  RiggVar.FZ.Registry;

procedure TFormDrawing.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    LayoutComponents;
    DrawingList.ItemIndex := DL.DrawingList.Count-1;
    SelectDrawing(DrawingList.ItemIndex);
    DrawingList.SetFocus;
  end;
end;

procedure TFormDrawing.CreateDrawings;
begin
  RggDrawingD00 := TRggDrawingD00.Create;
  DL := TRggDrawings.Create;
  DL.Add(RggDrawingD00);
  TRggDrawingRegistry.Init(DL);
  InitDrawings;
end;

procedure TFormDrawing.InitDrawings;
var
  i: Integer;
  li: TListViewItem;
begin
  DL.InitItems(TempList);
  for i := 0 to TempList.Count-1 do
  begin
    li := DrawingList.Items.Add;
    li.Text := TempList[i];
  end;
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
var
  ML: TStrings;
begin
  ML := Memo.Lines;
  if WantClear then
    ML.Clear;
  ML.Add(Format('X = %.2f', [P.X]));
  ML.Add(Format('Y = %.2f', [P.Y]));
  ML.Add(Format('Z = %.2f', [P.Z]));
  ML.Add('');
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
{$ifdef debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
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

  ML := Memo.Lines;
  ML.Clear;
  SetupMemo(Memo);

  CreateDrawings;

  Image.Width := BitmapWidth;
  Image.Height := BitmapHeight;

  GlobalShowCaptionBtn.StaysPressed := True;
  GlobalShowCaptionBtn.IsPressed := GlobalShowCaption;

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

  ElementList := TListView.Create(Self);
  ElementList.Parent := Self;
  ElementList.ItemAppearanceName := 'ListItem';
  ElementList.ItemAppearance.ItemHeight := 29;
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

  Memo := TMemo.Create(Self);
  Memo.Parent := Self;

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

  UpdateFromRiggBtn := TSpeedButton.Create(Self);
  UpdateFromRiggBtn.Parent := Self;
  UpdateFromRiggBtn.Text := 'UFR';

  CodeBtn := TSpeedButton.Create(Self);
  CodeBtn.Parent := Self;
  CodeBtn.Text := 'Code';

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
    vkF11: SwapLayout;
    vkF6: SwapDrawingLists;
    vkF3: SwapThickLines;
    vkF1: ShowInfo;
  end;

  if KeyChar = 'h' then
    UpdateLayout(True);

  if KeyChar = 'v' then
    UpdateLayout(False);
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
      CurrentDrawing.WheelFlag := True;
      TRggCircle.Matrix := TH.AccuMatrix;
      cr.Transform;
    end;
  end;

  Draw;
end;

procedure TFormDrawing.GlobalShowCaptionBtnClick(Sender: TObject);
begin
  GlobalShowCaption := not GlobalShowCaption;
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
  if CurrentDrawing <> nil then
  begin
    Memo.Lines.Clear;
    CurrentDrawing.WriteCode(Memo.Lines);
  end;
end;

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

{$ifdef Vcl}
  MM.Parent := Self;
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := TScrollStyle.ssBoth;
{$endif}
end;

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

procedure TFormDrawing.UpdateFromRiggBtnClick(Sender: TObject);
begin
  RggDrawingD00.UpdateFromRigg;
  Draw;
end;

procedure TFormDrawing.ResetBtnClick(Sender: TObject);
begin
  Memo.Lines.Clear;
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
  UpdateFromRiggBtn.Hint := 'Update Drawing 05 from Rigg-Data';
  ToggleShowCaptionBtn.Hint := 'toggle ShowCaption for RggElement';
  GlobalShowCaptionBtn.Hint := 'toggle global ShowCaption';
  CodeBtn.Hint := 'write Code to Memo';
  ResetBtn.Hint := 'Reset Transform';
end;

procedure TFormDrawing.InitComponentSize;
var
  w: single;
begin
  w := 40;

  UpdateFromRiggBtn.Width := w;
  CodeBtn.Width := w;
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

  DrawingList.Width := ListboxWidth;
  DrawingList.Height := 300;

  ElementList.Width := ListboxWidth;
  ElementList.Height := 100;

  Memo.Width := MemoWidth;
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
 cr: TRggCircle;
begin
  if not InplaceMouseDown then
    Exit;
  if CurrentElement = nil then
    Exit;
  Assert(CurrentElement is TRggCircle);
  cr := CurrentElement as TRggCircle;

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

  { InplaceShap.Position is updated in DrawToCanvas }

  cr.Param1I(dx);
  cr.Param2I(dy);

  CurrentDrawing.InplaceFlag := True;
  cr.Matrix := TH.BuildMatrixI;
  CurrentDrawing.Compute;
  cr.TransformI;

  Draw;

  ML.Add(Format('dx = %.2f', [dx]));
  ML.Add(Format('dy = %.2f', [dy]));
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

  CodeBtn.Position.X := Margin;
  CodeBtn.Position.Y := Margin;

  cr := CodeBtn;
  StackH(UpdateFromRiggBtn);
  StackH(GlobalShowCaptionBtn);
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

  cr := CodeBtn;
  StackV(DrawingList);
  StackH(ElementList);
  StackH(Image);
  StackH(Memo);

  AdjustWH;

  AnchorV(DrawingList);
  AnchorV(ElementList);
  AnchorV(Image);
  AnchorHV(Memo);
end;

procedure TFormDrawing.LayoutComponentsV;
begin
  ResetLayout;

  CodeBtn.Position.X := Margin;
  CodeBtn.Position.Y := Margin;

  cr := CodeBtn;
  StackV(UpdateFromRiggBtn);
  StackV(GlobalShowCaptionBtn);
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

  cr := CodeBtn;
  StackH(DrawingList);
  StackH(ElementList);
  StackH(Memo);
  StackH(Image);

  AdjustWH;

  AnchorV(DrawingList);
  AnchorV(ElementList);
  AnchorV(Memo);
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
  UpdateFromRiggBtn.OnClick := UpdateFromRiggBtnClick;
  CodeBtn.OnClick := CodeBtnClick;
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
var
  cr: TRggCircle;
begin
  Inc(ClickCounter);
//  Caption := IntToStr(ClickCounter);

  if CurrentDrawing = nil then
    Exit;

  if ii > -1 then
  begin
    CurrentElement := CurrentDrawing.Element[ii];
    if CurrentElement is TRggCircle then
    begin
      cr := CurrentElement as TRggCircle;
      CurrentDrawing.FixPoint := cr.Center.C;
      TH.AccuMatrix := TH.BuildMatrixF;
      cr.Matrix := TH.BuildMatrixI;
      cr.Save;
      cr.TransformI;
    end;
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
    InplaceShape.Position.X := Image.Position.X + TH.Offset.X + cr.Center.P.X - InplaceShape.Width / 2;
    InplaceShape.Position.Y := Image.Position.Y + TH.Offset.Y + cr.Center.P.Y - InplaceShape.Height / 2;
    InplaceShape.Visible := True;

    if CurrentDrawing.InplaceFlag then
    begin
      TH.AccuMatrix := TH.BuildMatrixG(cr.Center.C);
    end;

    if CurrentDrawing.WheelFlag then
    begin
      CurrentDrawing.FixPoint := cr.Center.C;
    end;

    CurrentDrawing.InplaceFlag := False;
    CurrentDrawing.WheelFlag := False;

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
  cr: TRggCircle;
  ss: single;
begin
  if CurrentElement is TRggCircle then
  begin
    Inc(DrawCounter);
    cr := CurrentElement as TRggCircle;
    ML.Clear;
    ML.Add('DrawCounter = ' + IntToStr(DrawCounter));
    ML.Add(cr.Caption + '.OriginalCenter:');
    ShowPoint3D(cr.OriginalCenter.C, False);
    ML.Add(cr.Caption + '.Center:');
    ShowPoint3D(cr.Center.C, False);
  end;

  { FMX }
  ss := Image.Scene.GetSceneScale;
  g.Offset := TH.Offset;
  if g.BeginScene then
  try
    g.SetMatrix(TMatrix.CreateScaling(ss, ss));
    g.Clear(claWhite);
    g.Fill.Color := claYellow;
    g.Stroke.Color := claAqua;
    g.Stroke.Thickness := 1.0;
    g.Font.Size := 16;
    g.Font.Family := 'Consolas';
    CurrentDrawing.Draw(g);
  finally
    g.EndScene;
  end;

  UpdateInplacePosition;
  UpdateMemo;
  Image.Repaint;
end;

procedure TFormDrawing.UpdateMemo;
begin
  if CurrentDrawing.WantMemoLines then
    Memo.Text := CurrentDrawing.ML.Text;
end;

procedure TFormDrawing.ShowInfo;
begin
  ML.Add('W and H:');
  ML.Add(Format('PosID  = %d, %d, %d', [FScreenPosID, FAdjustW, FAdjustH]));
  ML.Add(Format('Screen = (%d, %d)', [Screen.Width, Screen.Height]));
  ML.Add(Format('WA     = (%d, %d)', [Screen.WorkAreaWidth, Screen.WorkAreaHeight]));
  ML.Add(Format('FormLT = (%d, %d)', [Left, Top]));
  ML.Add(Format('FormWH = (%d, %d)', [Width, Height]));
  ML.Add(Format('Client = (%d, %d)', [ClientWidth, ClientHeight]));
  ML.Add(Format('Bitmap = (%d, %d)', [Image.Bitmap.Width, Image.Bitmap.Height]));
  ML.Add(Format('Image  = (%.1f, %.1f)', [Image.Width, Image.Height]));
  ML.Add(Format('Memo   = (%.1f, %.1f)', [Memo.Width, Memo.Height]));
  ML.Add(Format('DL     = (%.1f, %.1f)', [DrawingList.Width, DrawingList.Height]));
  ML.Add(Format('Scale  = %.2f', [Handle.Scale]));
  ML.Add(Format('ImgSS  = %.2f', [Image.ScreenScale]));
  ML.Add(Format('R1     = (%.1f, %.1f)', [Image.R1.Width, Image.R1.Height]));
  ML.Add(Format('R2     = (%.1f, %.1f)', [Image.R2.Width, Image.R2.Height]));
  ML.Add(Format('IR     = (%.1f, %.1f)', [Image.IR.Width, Image.IR.Height]));
  ML.Add('');
end;

procedure TFormDrawing.ShowDrawingInfo;
begin
  ML.Clear;
  if not CurrentDrawing.IsValid then
  begin
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
begin
  CurrentDrawing := nil;
  CurrentElement := nil;

  DrawingList.Items.Clear;
  ElementList.Items.Clear;

  DL.Free;
  DL := TRggDrawings.Create;

  WantExampleDrawings := not WantExampleDrawings;
  if WantExampleDrawings then
    TRggDrawingRegistry.InitFD(DL)
  else
    TRggDrawingRegistry.InitFZ(DL);

  InitDrawings;
  SelectDrawing(0);
end;

procedure TFormDrawing.ResetLayout;
begin
  AnchorReset(Image);
  AnchorReset(DrawingList);
  AnchorReset(ElementList);
  AnchorReset(Memo);

  DrawingList.Width := ListboxWidth;
  DrawingList.Height := 200;

  ElementList.Height := ListboxWidth;
  ElementList.Height := 200;

  Image.Width := BitmapWidth;
  Image.Height := BitmapHeight;

  Memo.Width := MemoWidth;
  Memo.Height := 200;

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
    CurrentDrawing.FixPoint := cr.Center.C;
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

end.
