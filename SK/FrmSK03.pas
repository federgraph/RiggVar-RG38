unit FrmSK03;

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
  RiggVar.RG.Def,
  RiggVar.SK.Graph,
  RiggVar.SK.Model01,
  RiggVar.SK.Process,
  RiggVar.RG.Types,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
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
  FMX.Text;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    ParamCombo: TComboBox;
    Trackbar: TTrackbar;
    TrackBtn: TButton;

    ReportText: TText;

    Clock1: TClockGraph;
    Clock2: TClockGraph;
    CircleS1: TCircle;
    CircleS2: TCircle;
    CircleS3: TCircle;

    TrackbarOldValue: single;
    FormShown: Boolean;

    SL: TStrings;
    Processor: TIdleHandler;

    Model01: TSKModel01;

    FSelectedCircle: Integer;
    FParam: TCircleParam;
    FWantButtonFrameReport: Boolean;

    FScale: single;
    Margin: single;
    Raster: Integer;

    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure Flash(s: string);
    procedure InitShapes;
    procedure InitModel01;
    procedure UpdateModel01;
    procedure InitNormal(i: Integer);
    procedure InitSelected(c: TClockGraph);
    procedure SetSelectedCircle(const Value: Integer);
    function GetIsUp: Boolean;
    function GetParamText: string;
    procedure SetupText(T: TText; fs: single);
    procedure TrackbarChange(Sender: TObject);
    procedure TrackBtnClick(Sender: TObject);
    function FindItemIndexOfParam(ML: TStrings): Integer;
    procedure InitParamCombo;
    procedure ParamComboChange(Sender: TObject);
    procedure UpdateItemIndexParamsCB;
    procedure UpdateItemIndexParams;
    procedure CreateComponents;
    procedure CreateShapes;
    procedure LayoutComponents;
    property IsUp: Boolean read GetIsUp;
  protected
    procedure InitHelpText;
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    property SelectedCircle: Integer read FSelectedCircle write SetSelectedCircle;
    property WantButtonFrameReport: Boolean read FWantButtonFrameReport;
  public
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    procedure UpdateParam(fp: TCircleParam);
    procedure HandleWheel(Delta: Integer);
    procedure UpdateReport;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.SK.Main,
  RiggVar.SK.Graph03;

const
//  MaxEdgeDistance = 200;
//  BoolStr: array[Boolean] of string = ('False', 'True');

  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'SK03';

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FScale := 1.0;
{$ifdef MSWINDOWS}
  { On MACOS Screen.WorkAreaHeight is not scaled,
    so it would be wrong to div by scale.

    On Windows Screen.WorkAreaHeight is scaled and should be divved. }
  FScale := Handle.Scale;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;

  Caption := ApplicationTitleText;
  Margin := 10;
  Raster := 70;

  Width := 800;
  Height := 700;

  SL := TStringList.Create;

  Main := TMain.Create;
  Main.IsUp := True;

  CreateShapes;
  CreateComponents;

  SetupText(ReportText, 16.0);

  TrackBtn.OnClick := TrackBtnClick;
  Trackbar.OnChange := TrackbarChange;
  Trackbar.SetFocus;

  if ParamCombo <> nil then
  begin
    InitParamCombo;
  end;

  Processor := TIdleHandler.Create;
  Application.OnIdle := ApplicationEventsIdle;
  Main.IsUp := True;

  InitShapes;
  InitModel01;
  UpdateItemIndexParams;

  Fill.Kind := TBrushKind.Solid;
  Fill.Color := claSlateblue;
  ReportText.Color := claBeige;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Main.IsUp := False;
  Application.OnIdle := nil;

  Processor.Free;

  SL.Free;
  Model01.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
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
  end;
end;

procedure TFormMain.UpdateReport;
begin
  if not ReportText.Visible then
    Exit;

  SL.Clear;

  SL.Add('Current Param = ' + GetParamText);
  SL.Add(Format('M1 = (%.2f, %.2f)', [Model01.M1X, Model01.M1Y]));
  SL.Add(Format('M2 = (%.2f, %.2f)', [Model01.M2X, Model01.M2Y]));
  SL.Add(Format('R  = (%.2f, %.2f)', [Model01.R1, Model01.R2]));
  SL.Add(Format('A  = (%.2f, %.2f)', [Model01.A1, Model01.A2]));
  SL.Add('');
  SL.Add(Format('Bem KK = %s', [Model01.BemerkungKK]));
  SL.Add(Format('Bem GG = %s', [Model01.BemerkungGG]));

  ReportText.Text := SL.Text;
  ReportText.Visible := True;
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

  ReportText := TText.Create(Self);
  ReportText.Name := 'TrimmText';
  ReportText.Parent := Self;
end;

procedure TFormMain.LayoutComponents;
begin
  ParamCombo.Width := 200;
  ParamCombo.Position.X := 2 * Margin;
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
  TrackBtn.Text := 'Center';
  TrackBtn.Hint := 'reset Trackbar thumb to center';

  ReportText.Width := 400;
  ReportText.Height := 400;
  ReportText.Position.X := Margin;
  ReportText.Position.Y := TrackBtn.Position.Y + TrackBtn.Height + 2 * Margin;
end;

procedure TFormMain.TrackBtnClick(Sender: TObject);
begin
  TrackbarOldValue := Trackbar.Max / 2;
  Trackbar.Value := TrackbarOldValue;
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

function TFormMain.FindItemIndexOfParam(ML: TStrings): Integer;
var
  fp: TCircleParam;
  i: Integer;
begin
  fp := Main.Param;
  result := -1;
  for i := 0 to ML.Count-1 do
  begin
    if TCircleParam(ML.Objects[i]) = fp then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TFormMain.InitParamCombo;
  procedure Add(fp: TCircleParam);
  begin
    ParamCombo.Items.AddObject(Main.Param2Text(fp), TObject(fp));
  end;
begin
  Add(fpR1);
  Add(fpR2);
  Add(fpM1X);
  Add(fpM1Y);
//  Add(fpM1Z);
  Add(fpM2X);
  Add(fpM2Y);
//  Add(fpM2Z);
  Add(fpA1);
  Add(fpA2);
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

procedure TFormMain.HandleAction(fa: Integer);
begin

end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TCircleParam;
begin
  if ParamCombo <> nil then
  begin
    ii := ParamCombo.ItemIndex;
    fp := TCircleParam(ParamCombo.Items.Objects[ii]);
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

procedure TFormMain.UpdateParam(fp: TCircleParam);
begin
  FParam := fp;
  Flash(GetParamText);
  UpdateReport;
end;

function TFormMain.GetIsUp: Boolean;
begin
  result := (Main <> nil) and Main.IsUp;
end;

procedure TFormMain.InitHelpText;
var
  ML: TStringList;
begin
  ML := TStringList.Create;

  ML.Add('Toggle Text with Keys:');
  ML.Add('  h    - toggle help');
  ML.Add('  r    - toggle Report');
  ML.Add('');
  ML.Add('Select current param with Button:');
  ML.Add('  i, I - select param R1, R2');
  ML.Add('  j, J - select param M1X, M1Y');
  ML.Add('  k, K - select param M2X, M2Y');
  ML.Add('');
  ML.Add('Change param value with Wheel!');
  ML.Add('  Shift-Wheel = small step');
  ML.Add('  Ctrl-Wheel  = bigger step');
  ML.Add('');
  ML.Add('Another Test: change Format of Window.');
  ML.Add('  1..9, 0 - Format selection');
  ML.Add('  1, p, s - Landscape, Portrait, Square');

//  HelpText.Text := ML.Text;
//  HelpText.AutoSize := False;
//  HelpText.AutoSize := True;
//  HelpText.Visible := False;

  ML.Free;
end;

procedure TFormMain.InitModel01;
begin
  Model01 := TSKModel01.Create;
  Model01.Clock1 := Clock1;
  Model01.Clock2 := Clock2;
  Model01.S1 := CircleS1;
  Model01.S2 := CircleS2;
  Model01.S3 := CircleS3;

  UpdateModel01; // setting Width and Height
  Model01.Load; // do after setting Width and Height
  Model01.Enabled := True;
  Model01.Draw; // draw intersection points for the first time
end;

procedure TFormMain.UpdateModel01;
begin
  Model01.UpdateWH(ClientWidth, ClientHeight);
end;

procedure TFormMain.InitShapes;
var
  cla: TAlphaColor;
  c: TCircle;
begin
  cla := claCoral;
  c := CircleS1;
  c.Width := 32;
  c.Height := c.Width;
  c.Stroke.Color := cla;
  c.Fill.Color := cla;

  cla := claCornflowerblue;
  c := CircleS2;
  c.Width := 32;
  c.Height := c.Width;
  c.Stroke.Color := cla;
  c.Fill.Color := cla;

  cla := claMediumvioletred;
  c := CircleS3;
  c.Width := 32;
  c.Height := c.Width;
  c.Stroke.Color := cla;
  c.Fill.Color := cla;
end;

procedure TFormMain.SetSelectedCircle(const Value: Integer);
begin
  FSelectedCircle := Value;
  InitNormal(1);
  InitNormal(2);
  case Value of
    1: InitSelected(Clock1);
    2: InitSelected(Clock2);
    else
      { do nothing}
  end;
  Model01.Draw;
end;

procedure TFormMain.InitSelected(c: TClockGraph);
begin
  c.StrokeColorCircle := claCrimson;
end;

procedure TFormMain.InitNormal(i: Integer);
begin
  case i of
    1: Clock1.StrokeColorCircle := claYellow;
    2: Clock2.StrokeColorCircle := claLime;
  end;
  Model01.Draw;
end;

procedure TFormMain.HandleWheel(Delta: Integer);
var
  f: single;
  M: TSKModel01;
begin
  M := Model01;
  case FParam of
    fpR1:
    begin
      f := M.R1 + 2 * Delta;
      M.R1 := f;
      Flash(Format('R1 = %d', [Round(f)]));
      M.Draw;
    end;

    fpR2:
    begin
      f := M.R2 + 2 * Delta;
      M.R2 := f;
      Flash(Format('R2 = %d', [Round(f)]));
      M.Draw;
    end;

    fpM1X:
    begin
      f := M.M1X + Delta;
      M.M1X := f;
      Flash(Format('M1X = %d', [Round(f)]));
      M.Draw;
    end;

    fpM1Y:
    begin
      f := M.M1Y - Delta;
      M.M1Y := f;
      Flash(Format('M1Y = %d', [Round(f)]));
      M.Draw;
    end;

    fpM2X:
    begin
      f := M.M2X + Delta;
      M.M2X := f;
      Flash(Format('M2X = %d', [Round(f)]));
      M.Draw;
    end;

    fpM2Y:
    begin
      f := M.M2Y - Delta;
      M.M2Y := f;
      Flash(Format('M2Y = %d', [Round(f)]));
      M.Draw;
    end;

    fpA1:
    begin
      f := M.A1 + 1 * Delta;
      M.A1 := f;
      Flash(Format('A1 = %d', [Round(f)]));
      M.Draw;
    end;

    fpA2:
    begin
      f := M.A2 + 1 * Delta;
      M.A2 := f;
      Flash(Format('A2 = %d', [Round(f)]));
      M.Draw;
    end;

  end;

end;

procedure TFormMain.CreateShapes;
begin
  Clock1 := TClockGraph03.Create(Self);
  Clock1.SimpleImp := False;
  Clock1.GrowAroundCenter := False;
  Clock1.CenterPoint := TPointF.Create(336, 440);
  Clock1.Radius := 160;
  Clock1.StrokeThicknessCircle := 5.0;
  Clock1.StrokeThicknessFinger := 5.0;
  Clock1.StrokeColorCircle := claYellow;
  Clock1.StrokeColorFinger := claYellow;
  Clock1.InitComponents(Self);

  Clock2 := TClockGraph03.Create(Self);
  Clock2.CenterPoint := TPointF.Create(528, 320);
  Clock2.Radius := 120;
  Clock2.StrokeThicknessCircle := 5.0;
  Clock2.StrokeThicknessFinger := 5.0;
  Clock2.StrokeColorCircle := claChartreuse;
  Clock2.StrokeColorFinger := claChartreuse;
  Clock2.InitComponents(Self);

  CircleS1 := TCircle.Create(Self);
  CircleS1.Parent := Self;
  CircleS1.Fill.Color := claAqua;
  CircleS1.Position.X := 624.0;
  CircleS1.Position.Y := 440.0;
  CircleS1.Width := 50.0;
  CircleS1.Height := 50.0;
  CircleS1.Stroke.Color := claAqua;
  CircleS1.Stroke.Thickness := 5.0;
  CircleS1.HitTest := False;

  CircleS2 := TCircle.Create(Self);
  CircleS2.Parent := Self;
  CircleS2.Fill.Color := claAqua;
  CircleS2.Position.X := 680.0;
  CircleS2.Position.Y := 328.0;
  CircleS2.Width := 20.0;
  CircleS2.Height := 20.0;
  CircleS2.Stroke.Color := claDodgerblue;
  CircleS2.Stroke.Thickness := 5.0;
  CircleS2.HitTest := False;

  CircleS3 := TCircle.Create(Self);
  CircleS3.Parent := Self;
  CircleS3.Fill.Color := claMediumvioletred;
  CircleS3.Position.X := 744.0;
  CircleS3.Position.Y := 112.0;
  CircleS3.Width := 30.0;
  CircleS3.Height := 30.0;
  CircleS3.Stroke.Color := claMediumvioletred;
  CircleS3.Stroke.Thickness := 0.0;
  CircleS3.HitTest := False;
end;

procedure TFormMain.Flash(s: string);
begin
  Caption := s;
end;

function TFormMain.GetParamText: string;
begin
  case FParam of
    fpR1: result := 'Radius 1';
    fpR2: result := 'Radius 2';
    fpM1X: result := 'Center Point 1.X';
    fpM1Y: result := 'Center Point 1.Y';
    fpM2X: result := 'Center Point 2.X';
    fpM2Y: result := 'Center Point 2.Y';
    fpA1: result := 'Line 1 Angle';
    fpA2: result := 'Line 2 Angle';
    else
      result := 'None';
  end;
end;

end.
