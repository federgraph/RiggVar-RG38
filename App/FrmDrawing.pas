﻿unit FrmDrawing;

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
  RiggVar.FD.Drawing05;

{$define FMX}

type

  { TFormDrawing }

  TFormDrawing = class(TForm)
    Memo: TMemo;
    DrawingList: TListBox;
    ElementList: TListBox;
    Image: TImage;
    InplaceShape: TCircle;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure ImageMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);

    procedure InplaceShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InplaceShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure InplaceShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure DrawingListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure DrawingListKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure ElementListChange(Sender: TObject);
    procedure ElementListKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    procedure UpdateFromRiggBtnClick(Sender: TObject);
    procedure CodeBtnClick(Sender: TObject);
    procedure GlobalShowCaptionBtnClick(Sender: TObject);
    procedure ToggleShowCaptionBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
  protected
    IsRightMouseBtn: Boolean;
    MouseDown: Boolean;
    MousePos: TPointF;
    Offset: TPointF;
    Rotation: TPoint3D;
    ZoomDelta: single;

    NewMatrix: TMatrix3D;
    AccuMatrix: TMatrix3D;

    InplaceMouseDown: Boolean;
    InplaceMousePos: TPointF;

    procedure BuildMatrix(mr: TMatrix3D);
    procedure BuildMatrixM;
    procedure ResetTransform;
    procedure UpdateTransform;
    procedure InitTransform(mr: TMatrix3D);
  private
    FormShown: Boolean;
    ListboxWidth: single;
    DL: TRggDrawings;
    CurrentDrawing: TRggDrawing;
    CurrentElement: TRggElement;
    TempList: TStringList;
//    Counter: Integer;
    procedure InitComponentSize;
    procedure LayoutComponents;
    procedure DrawToCanvas(g: TCanvas);
    procedure CreateComponents;
    procedure InitComponentProps;
    procedure LinkComponents;
    procedure SetupMemo(MM: TMemo);
    procedure HandleWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure UpdateInplacePosition;
    procedure Reset;
    procedure ShowInfo;
    procedure InitDrawings;
    procedure SelectDrawing(ii: Integer);
    procedure InitElements;
    procedure SelectElement(ii: Integer);
    procedure ShowDrawingInfo;
    procedure ClearImage;
  protected
    cr: TControl;
    TempR: single;
    TempB: single;
    FMaxRight: single;
    FMaxBottom: single;
    Margin: single;
    Raster: single;
    Bitmap: TBitmap;
    procedure RecordMax;
    procedure AnchorHorizontal(c: TControl);
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
  public
    RggDrawing05: TRggDrawing05;
    DrawCounter: Integer;
    procedure CreateDrawings;
    procedure Draw;
  public
    UpdateFromRiggBtn: TSpeedButton;
    CodeBtn: TSpeedButton;
    GlobalShowCaptionBtn: TSpeedButton;
    ToggleShowCaptionBtn: TSpeedButton;
    ResetBtn: TSpeedButton;
  end;

var
  FormDrawing: TFormDrawing;

implementation

{$R *.fmx}

uses
//  RiggVar.FD.Drawing01,
//  RiggVar.FD.Drawing02,
//  RiggVar.FZ.Drawing03,
//  RiggVar.FD.Drawing04,
//  RiggVar.FD.Drawing05, // see uses clause in interface section
//  RiggVar.FD.Drawing06,
//  RiggVar.FD.Drawing07,
//  RiggVar.FD.Drawing08,
//  RiggVar.FD.Drawing09,
//  RiggVar.FD.Drawing10,
//  RiggVar.FD.Drawing11,
  RiggVar.FZ.Z01_Viereck,
  RiggVar.FZ.Z02_Logo,
  RiggVar.FZ.Z03_Viergelenk,
  RiggVar.FZ.Z04_Tetraeder,
  { RiggVar.FZ.Z05_Rigg, }
  RiggVar.FZ.Z06_Hoehe,
  RiggVar.FZ.Z07_Triangle,
  RiggVar.FZ.Z08_Arc,
  RiggVar.FZ.Z09_Axis,
  RiggVar.FZ.Z10_Lager,
  RiggVar.FZ.Z11_Above,
  RiggVar.FZ.Z12_Atan2,
  RiggVar.FZ.Z13_SchnittKK,
  RiggVar.FZ.Z14_SplitF,
  RiggVar.FZ.Z15_SchnittGG,
  RiggVar.FZ.Z16_Shrink;

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
  RggDrawing05 := TRggDrawing05.Create;

  DL := TRggDrawings.Create;

  DL.Add(TRggDrawing01.Create); // Viereck
  DL.Add(TRggDrawing02.Create); // Logo
  DL.Add(TRggDrawing03.Create); // Viergelenk
  DL.Add(TRggDrawing04.Create); // Tetraeder
  DL.Add(RggDrawing05); // Rigg
  DL.Add(TRggDrawing06.Create); // Höhe
  DL.Add(TRggDrawing07.Create); // Triangle
  DL.Add(TRggDrawing08.Create); // Arc
  DL.Add(TRggDrawing09.Create); // Axis
  DL.Add(TRggDrawing10.Create); // Lager
  DL.Add(TRggDrawing11.Create); // Above

  DL.Add(TRggDrawing12.Create); // Atan2
  DL.Add(TRggDrawing12.Create(1)); // Atan

  DL.Add(TRggDrawing13.Create);  // SchnittKK
  DL.Add(TRggDrawing14.Create);  // SplitF
  DL.Add(TRggDrawing15.Create); // SchnitttGG
  DL.Add(TRggDrawing16.Create); // Shrink

  InitDrawings;
end;

procedure TFormDrawing.InitDrawings;
var
  i: Integer;
//  li: TListViewItem;
begin
  DL.InitItems(TempList);
  for i := 0 to TempList.Count-1 do
  begin
    DrawingList.Items.Add(TempList[i]);
//    li := DrawingList.Items.Add;
//    li.Text := TempList[i];
  end;
end;

procedure TFormDrawing.InitElements;
var
  i: Integer;
//  li: TListViewItem;
begin
  if CurrentDrawing = nil then
    Exit;

  CurrentDrawing.InitItems(TempList);
  ElementList.Items.Clear;
  for i := 0 to TempList.Count-1 do
  begin
    ElementList.Items.Add(TempList[i]);
//    li := ElementList.Items.Add;
//    li.Text := TempList[i];
  end;
end;

procedure TFormDrawing.FormCreate(Sender: TObject);
begin
{$ifdef debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  Caption := 'Rgg test and documentation drawings';

  if (Screen.Width >= 1920) and (Screen.Height >= 1024) then
  begin
    { Tested on normal HD screen }
    Left := 200;
    Top := 50;
    Width := 1000;
    Height := 960;
  end
  else
  begin
    { Tested on Microsoft Surface Tablet }
    Left := 20;
    Top := 30;
    Width := 1000;
    Height := 700;
  end;

  Margin := 10;
  Raster := 70;

  ZoomDelta := 1;

  ResetTransform;

  TempList := TStringList.Create;

  CreateComponents;
  InitComponentSize;
  LinkComponents;

  Memo.Lines.Clear;
  SetupMemo(Memo);

  CreateDrawings;

  Bitmap := TBitmap.Create(800, 800);
  Bitmap.Clear(claWhite);

  Image.Width := Bitmap.Width;
  Image.Height := Bitmap.Height;

  Image.Bitmap := Bitmap;
  Image.WrapMode := TImageWrapMode.Original;

  GlobalShowCaptionBtn.StaysPressed := True;
  GlobalShowCaptionBtn.IsPressed := GlobalShowCaption;

  InitComponentProps;
end;

procedure TFormDrawing.FormDestroy(Sender: TObject);
begin
  DL.Free;
  Bitmap.Free;
  TempList.Free;
end;

procedure TFormDrawing.CreateComponents;
begin
  InplaceShape.AutoCapture := True;
  InplaceShape.Fill.Kind := TBrushKind.None;
  InplaceShape.Width := 50;
  InplaceShape.Height := 50;

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
end;

procedure TFormDrawing.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = VKEscape then
  begin
    Reset;
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
      TRggCircle.Matrix := AccuMatrix;
      CurrentDrawing.Compute;
    CurrentElement.Transform;
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
  RggDrawing05.UpdateFromRigg;
  Draw;
end;

procedure TFormDrawing.ResetBtnClick(Sender: TObject);
begin
  Memo.Lines.Clear;
  Reset;
  ShowInfo;
end;

procedure TFormDrawing.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDrawing.AnchorHorizontal(c: TControl);
begin
  c.Width := ClientWidth - c.Position.X - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akRight];
end;

procedure TFormDrawing.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MouseDown := True;
  MousePos.X := X;
  MousePos.Y := Y;
  IsRightMouseBtn := Button = TMouseButton.mbRight;
  Rotation := TPoint3D.Zero;
end;

procedure TFormDrawing.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
 dx, dy: single;
begin
  if not MouseDown then
    Exit;

  dx := X - MousePos.X;
  dy := Y - MousePos.Y;

  if (ssShift in Shift) or (ssMiddle in Shift) then
  begin
    if dy > 0 then
      ZoomDelta := 1 - 0.01
    else
      ZoomDelta := 1 + 0.01;
  end
  else if ssCtrl in Shift then
  begin
    Offset.X := Offset.X + dx;
    Offset.Y := Offset.Y + dy;
  end

  else
  begin
    if IsRightMouseBtn then
    begin
      Rotation.Z := Rotation.Z + dx * 0.005;
    end
    else
    begin
      Rotation.X := Rotation.X - dy * 0.005;
      Rotation.Y := Rotation.Y + dx * 0.005;
    end;
  end;

  Draw;

  ZoomDelta := 1;
  Rotation := TPoint3D.Zero;
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

  ListboxWidth := 200;

  DrawingList.Width := ListboxWidth;
  DrawingList.Height := 300;

  ElementList.Width := ListboxWidth;
  ElementList.Height := 100;

  Memo.Width := 220;
end;

procedure TFormDrawing.InplaceShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  InplaceMouseDown := True;
  InplaceMousePos.X := X;
  InplaceMousePos.Y := Y;
end;

procedure TFormDrawing.InplaceShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
 dx, dy: single;
begin
  if not InplaceMouseDown then
    Exit;
  if CurrentElement = nil then
    Exit;

  dx := X - InplaceMousePos.X;
  dy := Y - InplaceMousePos.Y;

  InplaceShape.Position.X := InplaceShape.Position.X + dx;
  InplaceShape.Position.Y := InplaceShape.Position.Y + dy;

  CurrentElement.Param1(dx);
  CurrentElement.Param2(dy);

  CurrentDrawing.Compute;
  Draw;
end;

procedure TFormDrawing.InplaceShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  InplaceMouseDown := False;
end;

procedure TFormDrawing.LayoutComponents;
begin
  CodeBtn.Position.X := Margin;
  CodeBtn.Position.Y := Margin;

  cr := CodeBtn;
  StackH(UpdateFromRiggBtn);
  StackH(GlobalShowCaptionBtn);
  StackH(ToggleShowCaptionBtn);
  StackH(ResetBtn);

  cr := CodeBtn;
  StackV(Memo);
  StackH(DrawingList);
  StackH(ElementList);
  StackH(Image);

  ClientWidth := Round(FMaxRight + Margin);
  ClientHeight := Round(FMaxBottom + Margin);

  AnchorVertical(DrawingList);
  AnchorVertical(ElementList);
  AnchorVertical(Memo);
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

procedure TFormDrawing.DrawingListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if DrawingList.Selected = nil then
    Exit;
  SelectDrawing(DrawingList.ItemIndex);
end;

procedure TFormDrawing.SelectDrawing(ii: Integer);
begin
  CurrentElement := nil;

  if (ii > -1) and (ii < DL.DrawingList.Count) then
  begin
    ResetTransform;
    CurrentDrawing := DL.DrawingList[ii];
    ShowDrawingInfo;
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

procedure TFormDrawing.DrawingListKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = ' ' then
    SelectDrawing(DrawingList.ItemIndex);
end;

procedure TFormDrawing.ElementListChange(Sender: TObject);
var
  ii: Integer;
begin
  if ElementList.Selected = nil then
    Exit;

  ii := ElementList.ItemIndex;
  if ii > -1 then
    SelectElement(ii);
end;

procedure TFormDrawing.SelectElement(ii: Integer);
begin
//  Inc(Counter);
//  Caption := IntToStr(Counter);

  if CurrentDrawing = nil then
    Exit;

  if ii > -1 then
  begin
    CurrentElement := CurrentDrawing.Element[ii];
    Draw;
  end;
end;

procedure TFormDrawing.ElementListKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = ' ' then
    ToggleShowCaptionBtnClick(nil);
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
    InplaceShape.Position.X := Image.Position.X + Offset.X + cr.Center.P.X - InplaceShape.Width / 2;
    InplaceShape.Position.Y := Image.Position.Y + Offset.Y + cr.Center.P.Y - InplaceShape.Height / 2;
    InplaceShape.Visible := True;
    Exit;
  end;

  InplaceShape.Visible  := False;
end;

procedure TFormDrawing.ResetTransform;
begin
  TRggCircle.Matrix := TMatrix3D.Identity;
  AccuMatrix := TMatrix3D.Identity;
  Rotation := TPoint3D.Zero;
  Offset := TPointF.Zero;
end;

procedure TFormDrawing.ClearImage;
var
  g: TCanvas;
begin
  if (Image = nil) or (Bitmap = nil) then
    Exit;
  DrawCounter := 0;
  g := Image.Bitmap.Canvas;
  g.BeginScene;
  g.Clear(claAliceblue);
  g.EndScene;
end;

procedure TFormDrawing.Draw;
begin
  if (Image = nil) or (Bitmap = nil) then
    Exit;
  Inc(DrawCounter);

  if CurrentDrawing.WantRotation then
  begin
    BuildMatrixM;
    UpdateTransform;
  end;

  DrawToCanvas(Image.Bitmap.Canvas);
end;

procedure TFormDrawing.DrawToCanvas(g: TCanvas);
begin
  { FMX }
  g.Offset := Offset;
  if g.BeginScene then
  try
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
end;

procedure TFormDrawing.Reset;
begin
  ResetTransform;
  CurrentDrawing.Reset;
  Draw;
end;

procedure TFormDrawing.UpdateTransform;
begin
  AccuMatrix := AccuMatrix * NewMatrix;
  CurrentDrawing.Transform(NewMatrix);
end;

procedure TFormDrawing.InitTransform(mr: TMatrix3D);
begin
  BuildMatrix(mr);
  UpdateTransform;
  DrawToCanvas(Image.Bitmap.Canvas);
end;

procedure TFormDrawing.BuildMatrixM;
var
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  mx := TMatrix3D.CreateRotationX(Rotation.X);
  my := TMatrix3D.CreateRotationY(Rotation.Y);
  mz := TMatrix3D.CreateRotationZ(Rotation.Z);
  mr := mx * my * mz;

  BuildMatrix(mr);
end;

procedure TFormDrawing.BuildMatrix(mr: TMatrix3D);
var
  mt1, mt2, ms: TMatrix3D;
begin
  mt1 := TMatrix3D.CreateTranslation(-CurrentDrawing.FixPoint);
  ms := TMatrix3D.CreateScaling(TPoint3D.Create(ZoomDelta, ZoomDelta, ZoomDelta));
  mt2 := TMatrix3D.CreateTranslation(CurrentDrawing.FixPoint);
  NewMatrix := mt1 * mr * ms * mt2;
end;

procedure TFormDrawing.ShowInfo;
var
  ML: TStrings;
begin
  ML := Memo.Lines;
  ML.Add('Width and Height:');
  ML.Add(Format('Form   = (%d, %d)', [Width, Height]));
  ML.Add(Format('Client = (%d, %d)', [ClientWidth, ClientHeight]));
  ML.Add(Format('Bitmap = (%d, %d)', [Bitmap.Width, Bitmap.Height]));
  ML.Add(Format('Image  = (%.1f, %.1f)', [Image.Width, Image.Height]));
//  ML.Add(Format('Scale  = %.2f', [FScale]));
  ML.Add('');
end;

procedure TFormDrawing.ShowDrawingInfo;
var
  ML: TStrings;
begin
  ML := Memo.Lines;
  ML.Clear;
  if not CurrentDrawing.IsValid then
  begin
    ML.Add(CurrentDrawing.Name);
    ML.Add('Invalid');
    CurrentDrawing.GetInfo(ML);
  end;
end;

end.