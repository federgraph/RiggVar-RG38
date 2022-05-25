unit FrmRG03;

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

{$define FMX}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.UIConsts,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Forms,
  FMX.Graphics,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Text,
  RiggVar.App.Strings,
  RiggVar.App.Model,
  RiggVar.App.Process,
  RiggVar.FD.Image,
  RiggVar.RG.Model,
  RiggVar.RG.Rota,
  RiggVar.RG.Types,
  RiggVar.RG.View;

type
  TFormMain = class(TForm, IFormMain)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    ParamCombo: TComboBox;
    Trackbar: TTrackbar;
    TrackBtn: TButton;
    TrimmText: TText;
    Image: TOriginalImage;

    BitmapWidth: Integer;
    BitmapHeight: Integer;

    Margin: Integer;
    Raster: Integer;

    TrackbarOldValue: single;
    FormShown: Boolean;
    IsUp: Boolean;

    procedure CreateComponents;
    procedure LayoutComponents;

    procedure SetupText(T: TText; fs: single);

    procedure TrackbarChange(Sender: TObject);
    procedure TrackBtnClick(Sender: TObject);
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure InitParamCombo;
    procedure ParamComboChange(Sender: TObject);
    procedure UpdateItemIndexParamsCB;
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
  protected
    Rigg: IRigg;
    TL: TStrings;
    Processor: TIdleHandler;
    RotaForm: TRotaForm;

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;

    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;

    procedure UpdateColorScheme;
    procedure ToggleButtonSize;

    procedure RotaFormRotateZ(Delta: single);
    procedure RotaFormZoom(Delta: single);

    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;

    procedure UpdateOnParamValueChanged;
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;

    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);

    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;

  public
    procedure ShowTrimm;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}

  Caption := 'RG03';
  Margin := 10;
  Raster := 70;

  Width := 800;
  Height := 600;

  TL := TStringList.Create;

  CreateComponents;

  SetupText(TrimmText, 16.0);

  MainVar.WantGermanText := not MainVar.WantGermanText;
  RggLocalizedStrings.UpdateText;

  Rigg := TModelFactory.NewRigg;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg, Self, Self);
  Main.Logger.Verbose := True;
  Main.UseTimedDrawing := False;
  Main.InitialFixPoint := ooD0;
  Main.FixPoint := Main.InitialFixPoint;

  RotaForm := TRotaForm.Create;
  RotaForm.Image := Image;
  RotaForm.Init;
  RotaForm.SwapRota(1);

  TrackBtn.OnClick := TrackBtnClick;
  Trackbar.OnChange := TrackbarChange;
  Trackbar.SetFocus;

  if ParamCombo <> nil then
  begin
    InitParamCombo;
  end;

  Main.Param := fpVorstag;

  UpdateItemIndexParams;
  ShowTrimm;

  Processor := TIdleHandler.Create;
  Application.OnIdle := ApplicationEventsIdle;
  IsUp := True;

  Fill.Kind := TBrushKind.Solid;
  Fill.Color := claNavy;
  TrimmText.Color := claBeige;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  IsUp := False;
  MainVar.AppIsClosing := True;
  Application.OnIdle := nil;

  Processor.Free;

  Main.Free;
  Main := nil;

  TModelFactory.ReleaseIfAppropriate(Rigg);

  TL.Free;

  Image.Free;
  RotaForm.Free;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  f: Integer;
begin
  if ssShift in Shift then
    f := 5
  else if ssCtrl in Shift then
    f := 10
  else
    f := 1;

  if WheelDelta > 0 then
    f := -f;

  Processor.Delta := f;
  Handled := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    { ClientHeight is now 'available' }
    LayoutComponents;
    RotaForm.IsUp := True;
    RotaForm.Draw;
  end;
end;

procedure TFormMain.CreateComponents;
begin
  ParamCombo := TComboBox.Create(Self);
  ParamCombo.Name := 'ParamCombo';
  ParamCombo.Parent := Self;

  Trackbar := TTrackbar.Create(Self);
  Trackbar.Name := 'Trackbar';
  Trackbar.Parent := Self;

  TrackBtn := TButton.Create(Self);
  TrackBtn.Name := 'TrackBtn';
  TrackBtn.Parent := Self;

  TrimmText := TText.Create(Self);
  TrimmText.Name := 'TrimmText';
  TrimmText.Parent := Self;

  BitmapWidth := 1024;
  BitmapHeight := 768;
  Image := TOriginalImage.Create(Self, BitmapWidth, BitmapHeight);
  Image.Name := 'Image';
  Image.Parent := Self;
end;

procedure TFormMain.LayoutComponents;
begin
  ParamCombo.Width := 200;
  ParamCombo.Position.X := Margin;
  ParamCombo.Position.Y := Margin;

  Trackbar.Width := 200;
  Trackbar.Position.X := ParamCombo.Position.X + ParamCombo.Width + Margin;
  Trackbar.Position.Y := Margin;
  Trackbar.Max := 500;
  Trackbar.Frequency := 1.0;
  Trackbar.Value := Trackbar.Max / 2;

  TrackbarOldValue := Trackbar.Value;

  TrackBtn.Width := 80;
  TrackBtn.Position.X := Trackbar.Position.X + Trackbar.Width + Margin;
  TrackBtn.Position.Y := Margin;
  TrackBtn.Text := 'Center Track';
  TrackBtn.Hint := 'reset Trackbar thumb to center';

  TrimmText.Width := 400;
  TrimmText.Height := 400;
  TrimmText.Position.X := Margin;
  TrimmText.Position.Y := TrackBtn.Position.Y + TrackBtn.Height + 2 * Margin;

  Image.Width := BitmapWidth;
  Image.Height := BitmapHeight;
  Image.Position.X := 0;
  Image.Position.Y := TrimmText.Position.Y;
end;

procedure TFormMain.ToggleButtonSize;
begin

end;

procedure TFormMain.TrackBtnClick(Sender: TObject);
begin
  TrackbarOldValue := Trackbar.Max / 2;
  Trackbar.Value := TrackbarOldValue;
  Trackbar.SetFocus;
end;

procedure TFormMain.SetupText(T: TText; fs: single);
begin
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.SetShowDataText(const Value: Boolean);
begin

end;

procedure TFormMain.SetShowDiffText(const Value: Boolean);
begin

end;

procedure TFormMain.SetShowTrimmText(const Value: Boolean);
begin

end;

procedure TFormMain.ShowTrimm;
begin
  if TL <> nil then
  begin
    Main.UpdateTrimmText(TL);
    TrimmText.Text := TL.Text;
  end;
end;

procedure TFormMain.UpdateColorScheme;
begin

end;

procedure TFormMain.UpdateItemIndexParams;
begin
  UpdateItemIndexParamsCB;
end;

procedure TFormMain.UpdateItemIndexParamsCB;
var
  ii: Integer;
  ik: Integer;
begin
  if ParamCombo = nil then
    Exit;
  ii := ParamCombo.ItemIndex;
  ik := FindItemIndexOfParam(ParamCombo.Items);
  if ii <> ik then
  begin
    ParamCombo.OnChange := nil;
    ParamCombo.ItemIndex := ik;
    ParamCombo.OnChange := ParamComboChange;
  end;
end;

procedure TFormMain.UpdateItemIndexReports;
begin

end;

procedure TFormMain.UpdateItemIndexTrimms;
begin

end;

procedure TFormMain.UpdateOnParamValueChanged;
begin

end;

function TFormMain.FindItemIndexOfParam(ML: TStrings): Integer;
var
  fp: TFederParam;
  i: Integer;
begin
  fp := Main.Param;
  result := -1;
  for i := 0 to ML.Count-1 do
  begin
    if TFederParam(ML.Objects[i]) = fp then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TFormMain.InitParamCombo;
  procedure Add(fp: TFederParam);
  begin
    ParamCombo.Items.AddObject(Main.Param2Text(fp), TObject(fp));
  end;
begin
  Add(fpVorstag);
  Add(fpWinkel);
  Add(fpController);
  Add(fpWante);
  Add(fpWoben);
  Add(fpSalingH);
  Add(fpSalingA);
  Add(fpSalingL);
  Add(fpSalingW);
  Add(fpMastfallF0C);
  Add(fpMastfallF0F);
  Add(fpMastfallVorlauf);
  Add(fpBiegung);
  Add(fpD0X);

  ParamCombo.DropDownCount := ParamCombo.Items.Count;
end;

procedure TFormMain.RotaFormRotateZ(Delta: single);
begin

end;

procedure TFormMain.RotaFormZoom(Delta: single);
begin

end;

function TFormMain.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := 0;
end;

function TFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
begin
  result := 0;
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

function TFormMain.GetOpenFileName(dn, fn: string): string;
begin

end;

function TFormMain.GetSaveFileName(dn, fn: string): string;
begin

end;

function TFormMain.GetShowDataText: Boolean;
begin
  result := False;
end;

function TFormMain.GetShowDiffText: Boolean;
begin
  result := False;
end;

function TFormMain.GetShowTrimmText: Boolean;
begin
  result := False;
end;

procedure TFormMain.HandleAction(fa: Integer);
begin

end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  if ParamCombo <> nil then
  begin
    ii := ParamCombo.ItemIndex;
    fp := TFederParam(ParamCombo.Items.Objects[ii]);
    Processor.Param := fp;
  end;
end;

procedure TFormMain.TrackbarChange(Sender: TObject);
var
  t: single;
  delta: single;
begin
  t := Trackbar.Value;

  delta := 0;
  if t > TrackbarOldValue then
    delta := 1;
  if t < TrackbarOldValue then
    delta := -1;

  if delta <> 0 then
  begin
    Processor.Delta := delta;
  end;

  TrackbarOldValue := t;
end;

procedure TFormMain.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if IsUp then
  begin
    Processor.DoOnIdle;
  end;
  Done := True;
end;

end.
