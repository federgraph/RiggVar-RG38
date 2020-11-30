unit FrmDiagramE;

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
  RggDiagram,
  RggChartModel01,
  FMX.Controls.Presentation,
  FMX.Memo,
  FMX.SpinBox,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects;

type
  TFormDiagramE = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    UpdateBtn: TButton;
    XBtn: TButton;
    CalcBtn: TButton;
    LayoutBtn: TButton;

    XBox: TListBox;
    PBox: TListBox;
    YBox: TListBox;

    AutoText: TText;
    AText: TText;
    GText: TText;

    AutoToggle: TSwitch;
    AToggle: TSwitch;
    GToggle: TSwitch;
    UpDown: TSpinBox;

    Image: TOriginalImage;
    Memo: TMemo;
    AuswahlBtn: TButton;

    procedure AuswahlBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);

    procedure XBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure LayoutBtnClick(Sender: TObject);

    procedure XBoxChange(Sender: TObject);
    procedure YBoxChange(Sender: TObject);
    procedure PBoxChange(Sender: TObject);

    procedure AutoToggleClick(Sender: TObject);
    procedure AToggleClick(Sender: TObject);
    procedure GToggleClick(Sender: TObject);
  private
    FScale: single;
    BoxWidth: Integer;
    BoxHeight: Integer;
    MemoWidth: single;
    Layout: Integer;
    cr: TControl;
    Margin: single;
    FormShown: Boolean;
    procedure UpdateMemo;
    procedure LayoutComponentsV;
    procedure LayoutComponentsH;
    procedure InitComponentLinks;
    procedure InitComponentSize;
    procedure UpDownChange(Sender: TObject);
    procedure UpdateAutoCaption;
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
    ChartModel: TRggChartModel01;
    ChartGraph: TRggDiagram;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormDiagramE: TFormDiagramE;

implementation

{$R *.fmx}

uses
  FrmMain,
  RiggVar.App.Main;

const
  AutoCaptionOn = 'Auto On';
  AutoCaptionOff  = 'Auto off';

  ACaptionOn = 'Bereich';
  ACaptionOff  = 'Arbeitspunkt';

  GCaptionOn = 'Grouping On';
  GCaptionOff = 'Grouping off';

procedure TFormDiagramE.FormCreate(Sender: TObject);
begin
  Caption := 'Form Chart';

  { no scaling in FMX}
  FScale := 1.0;

  Margin := Round(10 * FScale);
  Width := Round(1500 * FScale);
  Height := Round(800 * FScale);

  BoxWidth := Round(200 * FScale);
  BoxHeight := Round(160 * FScale);
  MemoWidth := Round(350 * FScale);

  WantAutoUpdate := True;

  CreateComponents;
  Layout := 2;

  ChartModel := TRggChartModel01.Create(FormMain.Rigg);

  ChartGraph := TRggDiagram.Create(ChartModel);
  ChartGraph.Image := Image;
  ChartGraph.BackgroundColor := TAlphaColors.Navy;

  ChartModel.OnUpdateAvailable := ChartGraph.Draw;

  UpdateAutoCaption;
  UpdateACaption;
  UpdateGCaption;
end;

procedure TFormDiagramE.FormDestroy(Sender: TObject);
begin
  ChartGraph.Free;
  ChartModel.Free;
end;

procedure TFormDiagramE.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    ChartModel.SuperCalc; // --> Draw
    UpdateUI(nil); // --> update Listboxes
  end;
end;

procedure TFormDiagramE.CreateComponents;
begin
  UpdateBtn := TButton.Create(Self);
  UpdateBtn.Parent := Self;
  UpdateBtn.Text := 'Update UI';

  XBtn := TButton.Create(Self);
  XBtn.Parent := Self;
  XBtn.Text := 'Select X';

  CalcBtn := TButton.Create(Self);
  CalcBtn.Parent := Self;
  CalcBtn.Text := 'Calc';

  LayoutBtn := TButton.Create(Self);
  LayoutBtn.Parent := Self;
  LayoutBtn.Text := 'Layout';

  AutoToggle := TSwitch.Create(Self);
  AutoToggle.Parent := Self;
  AutoToggle.OnClick := nil;
  AutoToggle.IsChecked := WantAutoUpdate;

  AutoText := TText.Create(Self);
  AutoText.Parent := Self;
  AutoText.WordWrap := False;
  AutoText.AutoSize := True;

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

  Memo := TMemo.Create(Self);
  Memo.Parent := Self;

  Image := TOriginalImage.Create(Self, 650, 400);
  Image.Parent := Self;

  AuswahlBtn := TButton.Create(Self);
  AuswahlBtn.Parent := Self;
  AuswahlBtn.Text := 'YAV';

  InitComponentSize;
  InitComponentLinks;
end;

procedure TFormDiagramE.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;

  Memo.Width := MemoWidth;
end;

procedure TFormDiagramE.InitComponentLinks;
begin
  LayoutBtn.OnClick := LayoutBtnClick;
  AuswahlBtn.OnClick := AuswahlBtnClick;

  XBox.OnChange := XBoxChange;
  PBox.OnChange := PBoxChange;
  YBox.OnChange := YBoxChange;

  UpdateBtn.OnClick := UpdateBtnClick;
  XBtn.OnClick := XBtnClick;
  CalcBtn.OnClick := CalcBtnClick;

  AutoToggle.OnClick := AutoToggleClick;
  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;

  UpDown.OnChange := UpDownChange;
end;

procedure TFormDiagramE.UpdateMemo;
begin
  ChartModel.GetMemoText;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.UpdateAutoCaption;
begin
  if AutoToggle.IsChecked then
    AutoText.Text := AutoCaptionOn
  else
    AutoText.Text := AutoCaptionOff;
end;

procedure TFormDiagramE.UpdateACaption;
begin
  if AToggle.IsChecked then
    AText.Text := ACaptionOn
  else
    AText.Text := ACaptionOff;
end;

procedure TFormDiagramE.UpdateGCaption;
begin
  if GToggle.IsChecked then
    GText.Text := GCaptionOn
  else
    GText.Text := GCaptionOff;
end;

procedure TFormDiagramE.UpdateUI(Sender: TObject);
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

procedure TFormDiagramE.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartModel.XComboItemIndex := XBox.ItemIndex;

  if ChartModel.XComboItemIndex >= ChartModel.XComboItems.Count then
    ChartModel.XComboItemIndex := 0;

  ChartModel.UpdatePCombo(ChartModel.FSalingTyp);

  PBox.Items := ChartModel.PComboItems;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
end;

procedure TFormDiagramE.UpdateBtnClick(Sender: TObject);
begin
  UpdateUI(nil);
end;

procedure TFormDiagramE.CalcBtnClick(Sender: TObject);
begin
  if YBox.ItemIndex = -1 then
    Exit;

  ChartModel.PComboItemIndex := PBox.ItemIndex;
  ChartModel.YComboItemIndex := YBox.ItemIndex;

  ChartModel.AP := not AToggle.IsChecked;
  ChartModel.ShowGroup := GToggle.IsChecked;

  ChartModel.SuperCalc;

  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.XBoxChange(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramE.PBoxChange(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramE.YBoxChange(Sender: TObject);
begin
  if not GToggle.IsChecked then
  begin
    ChartModel.YComboItemIndex := YBox.ItemIndex;
    ChartModel.Calc;
  end;
end;

procedure TFormDiagramE.AutoToggleClick(Sender: TObject);
begin
  WantAutoUpdate := AutoToggle.IsChecked;
  UpdateAutoCaption;
end;

procedure TFormDiagramE.AToggleClick(Sender: TObject);
begin
  ChartModel.AP := not AToggle.IsChecked;
  ChartModel.Calc;
  Main.FederText.CheckState;
  Memo.Text := ChartModel.MemoLines.Text;
  UpdateACaption;
end;

procedure TFormDiagramE.GToggleClick(Sender: TObject);
begin
  ChartModel.ShowGroup := GToggle.IsChecked;
  ChartModel.DrawGroup;
  YBox.Enabled := not ChartModel.ShowGroup;
  Main.FederText.CheckState;
  UpdateGCaption;
  UpdateMemo;
end;

procedure TFormDiagramE.UpDownChange(Sender: TObject);
begin
  ChartModel.APWidth := Round(UpDown.Value);
  ChartModel.SuperCalc;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramE.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormDiagramE.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramE.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramE.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDiagramE.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormDiagramE.LayoutComponents;
begin
  FMaxRight := 0;
  FMaxBottom := 0;

  UpdateBtn.Position.X := Margin;
  UpdateBtn.Position.Y := Margin;

  cr := UpdateBtn;
  StackH(XBtn);
  StackH(CalcBtn);
  StackH(AuswahlBtn);
  StackH(LayoutBtn);

  cr := XBox;

  case Layout of
    1: LayoutComponentsV;
    2: LayoutComponentsH;
  end;

  AnchorVertical(Memo);
end;

procedure TFormDiagramE.LayoutComponentsV;
begin
  { Vertical ListBoxes }
  cr := UpdateBtn;
  StackV(XBox);

  StackV(PBox);

  StackV(AutoToggle);
  StackH(AutoText);
  cr := AutoToggle;

  StackV(AToggle);
  StackH(AText);
  cr := AToggle;

  StackV(UpDown);

  cr := PBox;
  StackV(LayoutBtn);

  cr := XBox;
  StackH(Memo);
  StackH(YBox);
  StackH(GToggle);
  StackH(GText);
  cr := YBox;
  StackV(Image);
  StackV(LayoutBtn);

  ClientWidth := Round(Image.Position.X + Image.Width + Margin);
end;

procedure TFormDiagramE.LayoutComponentsH;
begin
  { Horizontal ListBoxes }
  cr := UpdateBtn;
  StackV(XBox);
  StackH(PBox);
  StackH(YBox);

  StackH(AutoToggle);
  StackH(AutoText);
  cr := AutoToggle;

  StackV(GToggle);
  StackH(GText);
  cr := GToggle;

  StackV(AToggle);
  StackH(AText);
  cr := AToggle;

  StackV(UpDown);

  cr := XBox;
  StackV(Memo);
  StackH(Image);
  StackV(LayoutBtn);

  ClientWidth := Round(Image.Position.X + Image.Width + Margin);
end;

procedure TFormDiagramE.AuswahlBtnClick(Sender: TObject);
begin
  ChartModel.YAuswahlClick;
  YBox.Items := ChartModel.YComboItems;
end;

end.
