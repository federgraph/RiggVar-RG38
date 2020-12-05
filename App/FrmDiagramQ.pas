unit FrmDiagramQ;

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
  TFormDiagramQ = class(TForm)
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
    ChartModel: TChartGraph;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormDiagramQ: TFormDiagramQ;

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

procedure TFormDiagramQ.FormCreate(Sender: TObject);
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

  ChartModel := TChartGraph.Create(FormMain.Rigg);
  ChartModel.Image := Image;
  ChartModel.BackgroundColor := TAlphaColors.Navy;
end;

procedure TFormDiagramQ.FormDestroy(Sender: TObject);
begin
  ChartModel.Free;
end;

procedure TFormDiagramQ.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    UpdateUI(nil); // --> update Listboxes
    ChartModel.SuperCalc; // --> Draw
  end;
end;

procedure TFormDiagramQ.CreateComponents;
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

procedure TFormDiagramQ.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;
end;

procedure TFormDiagramQ.InitComponentLinks;
begin
  LayoutBtn.OnClick := LayoutBtnClick;

  XBox.OnChange := XBoxChange;
  PBox.OnChange := PBoxChange;
  YBox.OnChange := YBoxChange;

  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;

  UpDown.OnChange := UpDownChange;
end;

procedure TFormDiagramQ.UpdateMemo;
begin
  ChartModel.GetMemoText;
end;

procedure TFormDiagramQ.UpdateACaption;
begin
  if AToggle.IsChecked then
    AText.Text := ACaptionOn
  else
    AText.Text := aCaptionOff;
end;

procedure TFormDiagramQ.UpdateGCaption;
begin
  if GToggle.IsChecked then
    GText.Text := GCaptionOn
  else
    GText.Text := GCaptionOff;
end;

procedure TFormDiagramQ.UpdateUI(Sender: TObject);
begin
  if ChartModel = nil then
    Exit;

  if not Visible then
    Exit;

  AToggle.IsChecked := not ChartModel.AP;
  GToggle.IsChecked := ChartModel.ShowGroup;

  XBox.Items := ChartModel.XComboItems;
  PBox.Items := ChartModel.PComboItems;
  YBox.Items := ChartModel.YComboItems;

  XBox.ItemIndex := ChartModel.XComboItemIndex;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
  YBox.ItemIndex := ChartModel.YComboItemIndex;
  YBox.Enabled := not ChartModel.ShowGroup;

  UpdateMemo;

  UpDown.Value := ChartModel.APWidth;
end;

procedure TFormDiagramQ.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartModel.XComboItemIndex := XBox.ItemIndex;

  if ChartModel.XComboItemIndex >= ChartModel.XComboItems.Count then
    ChartModel.XComboItemIndex := 0;

  ChartModel.UpdatePCombo(ChartModel.FSalingTyp);

  PBox.Items := ChartModel.PComboItems;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
end;

procedure TFormDiagramQ.CalcBtnClick(Sender: TObject);
begin
  if YBox.ItemIndex = -1 then
    Exit;

  ChartModel.PComboItemIndex := PBox.ItemIndex;
  ChartModel.YComboItemIndex := YBox.ItemIndex;

  ChartModel.AP := not AToggle.IsChecked;
  ChartModel.ShowGroup := GToggle.IsChecked;

  ChartModel.SuperCalc;
end;

procedure TFormDiagramQ.XBoxChange(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramQ.PBoxChange(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramQ.YBoxChange(Sender: TObject);
begin
  if not GToggle.IsChecked then
  begin
    ChartModel.YComboItemIndex := YBox.ItemIndex;
    ChartModel.Calc;
  end;
end;

procedure TFormDiagramQ.AToggleClick(Sender: TObject);
begin
  ChartModel.AP := not AToggle.IsChecked;
  ChartModel.Calc;
  Main.FederTextCheckState;
  UpdateACaption;
end;

procedure TFormDiagramQ.GToggleClick(Sender: TObject);
begin
  ChartModel.ShowGroup := GToggle.IsChecked;
  ChartModel.DrawGroup;
  YBox.Enabled := not ChartModel.ShowGroup;
  Main.FederTextCheckState;
  UpdateGCaption;
  UpdateMemo;
end;

procedure TFormDiagramQ.UpDownChange(Sender: TObject);
begin
  ChartModel.APWidth := Round(UpDown.Value);
  ChartModel.SuperCalc;
end;

procedure TFormDiagramQ.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormDiagramQ.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramQ.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramQ.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDiagramQ.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormDiagramQ.LayoutComponents;
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

procedure TFormDiagramQ.LayoutComponentsV;
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

procedure TFormDiagramQ.LayoutComponentsH;
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
