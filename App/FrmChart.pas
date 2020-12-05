unit FrmChart;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  RiggVar.FD.Image,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  RggChartModel,
  RggChartGraph,
  FMX.Controls.Presentation,
  FMX.SpinBox,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects;

type
  TFormChart = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    LayoutBtn: TButton;
    XBox: TListBox;
    PBox: TListBox;
    YBox: TListBox;
    AText: TText;
    GText: TText;
    AToggle: TSwitch;
    GToggle: TSwitch;
    UpDown: TSpinBox;
    Image: TOriginalImage;

    procedure XBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);

    procedure XBoxChange(Sender: TObject);
    procedure YBoxChange(Sender: TObject);
    procedure PBoxChange(Sender: TObject);

    procedure AToggleClick(Sender: TObject);
    procedure GToggleClick(Sender: TObject);
  private
    FScale: single;
    BoxWidth: Integer;
    BoxHeight: Integer;
    Layout: Integer;
    cr: TControl;
    Margin: Integer;
    FormShown: Boolean;
    procedure UpdateMemo;
    procedure LayoutComponentsV;
    procedure LayoutComponentsH;
    procedure InitComponentLinks;
    procedure InitComponentSize;
    procedure UpDownChange(Sender: TObject);
    procedure UpdateACaption;
    procedure UpdateGCaption;
  protected
    TempR: single;
    TempB: single;
    FMaxRight: single;
    FMaxBottom: single;
    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
  public
    WantAutoUpdate: Boolean;
    ChartGraph: TChartGraph;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormChart: TFormChart;

implementation

{$R *.fmx}

uses
  FrmMain,
  RiggVar.App.Main;

const
  ACaptionOn = 'Bereich';
  ACaptionOff  = 'Arbeitspunkt';

  GCaptionOn = 'Grouping On';
  GCaptionOff = 'Grouping off';

procedure TFormChart.FormCreate(Sender: TObject);
begin
  Caption := 'Form Chart';

  { no scaling in FMX}
  FScale := 1.0;

  Margin := Round(10 * FScale);
  Width := Round(1500 * FScale);
  Height := Round(800 * FScale);

  BoxWidth := Round(200 * FScale);
  BoxHeight := Round(160 * FScale);

  WantAutoUpdate := True;

  CreateComponents;
  Layout := 2;

  ChartGraph := TChartGraph.Create(FormMain.Rigg);
  ChartGraph.Image := Image;
  ChartGraph.BackgroundColor := TAlphaColors.Navy;
end;

procedure TFormChart.FormDestroy(Sender: TObject);
begin
  ChartGraph.Free;
end;

procedure TFormChart.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    ChartGraph.SuperCalc; // --> Draw
    UpdateUI(nil); // --> update Listboxes
  end;
end;

procedure TFormChart.CreateComponents;
begin
  LayoutBtn := TButton.Create(Self);
  LayoutBtn.Parent := Self;
  LayoutBtn.Text := 'Layout';

  AToggle := TSwitch.Create(Self);
  AToggle.Parent := Self;
  AToggle.OnClick := nil;
  AToggle.IsChecked := False;
  AText := TText.Create(Self);
  AText.Parent := Self;
  AText.Text := ACaptionOff;
  AText.WordWrap := False;
  AText.AutoSize := True;

  GToggle := TSwitch.Create(Self);
  GToggle.Parent := Self;
  GToggle.OnClick := nil;
  GToggle.IsChecked := False;
  GText := TText.Create(Self);
  GText.Parent := Self;
  GText.Text := GCaptionOff;
  GText.WordWrap := False;
  GText.AutoSize := True;

  UpDown := TSpinBox.Create(Self);
  UpDown.Parent := Self;
  UpDown.Min := 1;
  UpDown.Max := 100;
  UpDown.Value := 30;

  XBox := TListBox.Create(Self);
  XBox.Parent := Self;

  PBox := TListBox.Create(Self);
  PBox.Parent := Self;

  YBox := TListBox.Create(Self);
  YBox.Parent := Self;

  Image := TOriginalImage.Create(Self, 650, 400);
  Image.Parent := Self;

  InitComponentSize;
  InitComponentLinks;
end;

procedure TFormChart.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;
end;

procedure TFormChart.InitComponentLinks;
begin
  LayoutBtn.OnClick := LayoutBtnClick;

  XBox.OnChange := XBoxChange;
  PBox.OnChange := PBoxChange;
  YBox.OnChange := YBoxChange;

  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;

  UpDown.OnChange := UpDownChange;
end;

procedure TFormChart.UpdateMemo;
begin
  ChartGraph.GetMemoText;
end;

procedure TFormChart.UpdateACaption;
begin
  if AToggle.IsChecked then
    AText.Text := ACaptionOn
  else
    AText.Text := aCaptionOff;
end;

procedure TFormChart.UpdateGCaption;
begin
  if GToggle.IsChecked then
    GText.Text := GCaptionOn
  else
    GText.Text := GCaptionOff;
end;

procedure TFormChart.UpdateUI(Sender: TObject);
begin
  if ChartGraph = nil then
    Exit;

  if not Visible then
    Exit;

  AToggle.IsChecked := not ChartGraph.AP;
  GToggle.IsChecked := ChartGraph.ShowGroup;

  XBox.Items := ChartGraph.XComboItems;
  PBox.Items := ChartGraph.PComboItems;
  YBox.Items := ChartGraph.YComboItems;

  XBox.ItemIndex := ChartGraph.XComboItemIndex;
  PBox.ItemIndex := ChartGraph.PComboItemIndex;
  YBox.ItemIndex := ChartGraph.YComboItemIndex;
  YBox.Enabled := not ChartGraph.ShowGroup;

  UpdateMemo;

  UpDown.Value := ChartGraph.APWidth;
end;

procedure TFormChart.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartGraph.XComboItemIndex := XBox.ItemIndex;

  if ChartGraph.XComboItemIndex >= ChartGraph.XComboItems.Count then
    ChartGraph.XComboItemIndex := 0;

  ChartGraph.UpdatePCombo(ChartGraph.FSalingTyp);

  PBox.Items := ChartGraph.PComboItems;
  PBox.ItemIndex := ChartGraph.PComboItemIndex;
end;

procedure TFormChart.CalcBtnClick(Sender: TObject);
begin
  { Called before Items are ceated }
  if YBox.ItemIndex = -1 then
    Exit; // do not override default ChartGraph YComboItemInde

  ChartGraph.PComboItemIndex := PBox.ItemIndex;
  ChartGraph.YComboItemIndex := YBox.ItemIndex;

  ChartGraph.AP := not AToggle.IsChecked;
  ChartGraph.ShowGroup := GToggle.IsChecked;

  ChartGraph.SuperCalc;
end;

procedure TFormChart.XBoxChange(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormChart.PBoxChange(Sender: TObject);
begin
  ChartGraph.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormChart.YBoxChange(Sender: TObject);
begin
  if not GToggle.IsChecked then
  begin
    ChartGraph.YComboItemIndex := YBox.ItemIndex;
    ChartGraph.Calc;
  end;
end;

procedure TFormChart.AToggleClick(Sender: TObject);
begin
  ChartGraph.AP := not AToggle.IsChecked;
  ChartGraph.Calc;
  Main.FederTextCheckState;
  UpdateACaption;
end;

procedure TFormChart.GToggleClick(Sender: TObject);
begin
  ChartGraph.ShowGroup := GToggle.IsChecked;
  ChartGraph.DrawGroup;
  YBox.Enabled := not ChartGraph.ShowGroup;
  Main.FederTextCheckState;
  UpdateGCaption;
  UpdateMemo;
end;

procedure TFormChart.UpDownChange(Sender: TObject);
begin
  ChartGraph.APWidth := Round(UpDown.Value);
  ChartGraph.SuperCalc;
end;

procedure TFormChart.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormChart.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormChart.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormChart.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormChart.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormChart.LayoutComponents;
begin
  FMaxRight := 0;
  FMaxBottom := 0;

  XBox.Position.X := Margin;
  XBox.Position.Y := Margin;

  cr := XBox;

  case Layout of
    1: LayoutComponentsV;
    2: LayoutComponentsH;
  end;

  ClientWidth := Round(FMaxRight + Margin);
  ClientHeight := Round(FMaxBottom + Margin);
end;

procedure TFormChart.LayoutComponentsV;
begin
  { Vertical ListBoxes }
  cr := XBox;
  StackV(PBox);
  StackH(UpDown);

  cr := PBox;
  StackV(LayoutBtn);

  cr := XBox;
  StackH(YBox);
  StackH(Image);
  StackV(AToggle);
  StackH(AText);
  StackH(GToggle);
  StackH(GText);
end;

procedure TFormChart.LayoutComponentsH;
begin
  { Horizontal ListBoxes }
  StackH(PBox);
  StackH(YBox);
  StackH(GText);

  StackV(GToggle);
  StackV(AText);
  StackV(AToggle);
  StackV(UpDown);

  cr := XBox;
  StackV(Image);
  StackH(LayoutBtn);
end;

end.
