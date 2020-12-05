unit FrmDiagramC;

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
  FMX.Memo,
  FMX.SpinBox,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects;

type
  TFormDiagramC = class(TForm)
    procedure FormCreate(Sender: TObject);
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

    Memo: TMemo;

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
    MemoWidth: single;
    MemoHeight: single;
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
    ChartModel: TChartModel; // injected
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure UpdateUI(Sender: TObject);
  end;

var
  FormDiagramC: TFormDiagramC;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main;

const
  ACaptionOn = 'Bereich';
  ACaptionOff  = 'Arbeitspunkt';

  GCaptionOn = 'Grouping On';
  GCaptionOff = 'Grouping off';

procedure TFormDiagramC.FormCreate(Sender: TObject);
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
  MemoHeight := Round(300 * FScale);

  WantAutoUpdate := True;

  CreateComponents;
  Layout := 2;
end;

procedure TFormDiagramC.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
    ChartModel.SuperCalc; // --> Draw
    UpdateUI(nil); // --> update Listboxes
  end;
end;

procedure TFormDiagramC.CreateComponents;
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

  Memo := TMemo.Create(Self);
  Memo.Parent := Self;

  InitComponentSize;
  InitComponentLinks;
end;

procedure TFormDiagramC.InitComponentSize;
begin
  XBox.Width := BoxWidth;
  XBox.Height := BoxHeight;

  PBox.Width := BoxWidth;
  PBox.Height := BoxHeight;

  YBox.Width := BoxWidth;
  YBox.Height := BoxHeight;

  Memo.Width := MemoWidth;
  Memo.Height := MemoHeight;
end;

procedure TFormDiagramC.InitComponentLinks;
begin
  LayoutBtn.OnClick := LayoutBtnClick;

  XBox.OnChange := XBoxChange;
  PBox.OnChange := PBoxChange;
  YBox.OnChange := YBoxChange;

  GToggle.OnClick := GToggleClick;
  AToggle.OnClick := AToggleClick;

  UpDown.OnChange := UpDownChange;
end;

procedure TFormDiagramC.UpdateMemo;
begin
  ChartModel.GetMemoText;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramC.UpdateACaption;
begin
  if AToggle.IsChecked then
    AText.Text := ACaptionOn
  else
    AText.Text := aCaptionOff;
end;

procedure TFormDiagramC.UpdateGCaption;
begin
  if GToggle.IsChecked then
    GText.Text := GCaptionOn
  else
    GText.Text := GCaptionOff;
end;

procedure TFormDiagramC.UpdateUI(Sender: TObject);
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

procedure TFormDiagramC.XBtnClick(Sender: TObject);
begin
  { Step 1 - select X }
  ChartModel.XComboItemIndex := XBox.ItemIndex;

  if ChartModel.XComboItemIndex >= ChartModel.XComboItems.Count then
    ChartModel.XComboItemIndex := 0;

  ChartModel.UpdatePCombo(ChartModel.FSalingTyp);

  PBox.Items := ChartModel.PComboItems;
  PBox.ItemIndex := ChartModel.PComboItemIndex;
end;

procedure TFormDiagramC.CalcBtnClick(Sender: TObject);
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

procedure TFormDiagramC.XBoxChange(Sender: TObject);
begin
  if WantAutoUpdate then
  begin
    XBtnClick(nil);
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramC.PBoxChange(Sender: TObject);
begin
  ChartModel.PComboItemIndex := PBox.ItemIndex;

  if WantAutoUpdate then
  begin
    CalcBtnClick(nil);
  end;
end;

procedure TFormDiagramC.YBoxChange(Sender: TObject);
begin
  if not GToggle.IsChecked then
  begin
    ChartModel.YComboItemIndex := YBox.ItemIndex;
    ChartModel.Calc;
  end;
end;

procedure TFormDiagramC.AToggleClick(Sender: TObject);
begin
  ChartModel.AP := not AToggle.IsChecked;
  ChartModel.Calc;
  Main.FederTextCheckState;
  UpdateACaption;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramC.GToggleClick(Sender: TObject);
begin
  ChartModel.ShowGroup := GToggle.IsChecked;
  ChartModel.DrawGroup;
  YBox.Enabled := not ChartModel.ShowGroup;
  Main.FederTextCheckState;
  UpdateGCaption;
  UpdateMemo;
end;

procedure TFormDiagramC.UpDownChange(Sender: TObject);
begin
  ChartModel.APWidth := Round(UpDown.Value);
  ChartModel.SuperCalc;
  Memo.Text := ChartModel.MemoLines.Text;
end;

procedure TFormDiagramC.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormDiagramC.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramC.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormDiagramC.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

procedure TFormDiagramC.LayoutBtnClick(Sender: TObject);
begin
  Inc(Layout);
  if Layout = 3 then
    Layout := 1;
  LayoutComponents;
end;

procedure TFormDiagramC.LayoutComponents;
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

procedure TFormDiagramC.LayoutComponentsV;
begin
  { Vertical ListBoxes }
  cr := XBox;
  StackV(PBox);
  StackH(UpDown);
  StackV(LayoutBtn);

  cr := XBox;
  StackH(YBox);
  StackH(Memo);
  StackV(AToggle);
  StackH(AText);
  StackH(GToggle);
  StackH(GText);
end;

procedure TFormDiagramC.LayoutComponentsH;
begin
  { Horizontal ListBoxes }
  StackH(PBox);
  StackH(YBox);

  cr := XBox;
  StackV(Memo);

  StackH(GToggle);
  StackH(GText);
  cr := GToggle;

  StackV(AToggle);
  StackH(AText);
  cr := AToggle;

  StackV(UpDown);
  StackV(LayoutBtn);
end;

end.
