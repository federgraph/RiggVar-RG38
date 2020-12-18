unit FrmTrimmTab;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  RiggVar.FD.Image,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  FMX.Forms,
  FMX.Controls,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Menus,
  RiggVar.App.Model,
  RiggVar.RG.Types,
  RiggVar.RG.TrimmTab,
  RiggVar.DT.TrimmTabGraph,
  FMX.Edit,
  FMX.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  FMX.EditBox,
  FMX.SpinBox;

type
  TFormTrimmTab = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    OKBtn: TButton;
    CancelBtn: TButton;

    rbKonstante: TRadioButton;
    rbGerade: TRadioButton;
    rbParabel: TRadioButton;
    rbBezier: TRadioButton;

    MemoLabel: TLabel;
    Memo: TMemo;

    Image: TOriginalImage;

    X1Label: TLabel;
    Y1Label: TLabel;
    X2Label: TLabel;
    Y2Label: TLabel;

    W2Edit: TEdit;
    K2Edit: TEdit;

    W1SpinBox: TSpinBox;
    K1SpinBox: TSpinBox;

    ReadMemoBtn: TButton;
    WriteMemoBtn: TButton;
    EvalOptionBtn: TButton;

    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure CalcBtnClick(Sender: TObject);
    procedure EvalOptionBtnClick(Sender: TObject);
    procedure Kraft1EditChange(Sender: TObject);
    procedure rbKonstanteClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: single);
  protected
    cr: TControl;
    TempR: single;
    TempB: single;
    FMaxRight: single;
    FMaxBottom: single;

    Margin: single;

    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);
  private
    FTrimmTabGraph: TTrimmTabGraph;
    FTrimmTabDaten: TTrimmTabDaten;
    FTrimmTabelle: TTrimmTab;
    FTabellenTyp: TTabellenTyp;
    FTabChanging: Boolean;
   procedure ImageScreenScaleChanged(Sender: TObject);
    procedure DrawTrimmTab;
    procedure CheckTabelle;
    procedure CreateComponents;
    procedure InitComponentLinks;
    procedure InitComponentSize;
    procedure InitComponentProps;
    procedure InitTabOrder;
    procedure LayoutComponents;
  public
    Rigg: IRigg;
    procedure Init(ARigg: IRigg);
  end;

var
  FormTrimmTab: TFormTrimmTab;

implementation

{$R *.fmx}

{ TFormTrimmTab }

procedure TFormTrimmTab.FormCreate(Sender: TObject);
begin
  Margin := 10;

  FTrimmTabGraph := TTrimmTabGraph.Create;

  CreateComponents;
  InitComponentSize;
  InitComponentProps;
  InitComponentLinks;
  InitTabOrder;

  LayoutComponents;
  FTrimmTabGraph.Image := Image;
end;

procedure TFormTrimmTab.FormDestroy(Sender: TObject);
begin
  FTrimmTabGraph.Free;
end;

procedure TFormTrimmTab.Init(ARigg: IRigg);
begin
  Rigg := ARigg;

  FTabellenTyp := itGerade;
  rbGerade.IsChecked := True;
  K1SpinBox.Enabled := False;
  W1SpinBox.Enabled := False;

  FTrimmTabelle := Rigg.TrimmTabelle;
  FTrimmTabDaten := FTrimmTabelle.TrimmTabDaten;
  CheckTabelle;

  Assert(FTrimmTabelle <> nil);
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
end;

procedure TFormTrimmTab.CheckTabelle;
begin
  { wenn Tabelle außerhalb des Dialoges verändert wurde - d.h. neu eingelesen }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTrimmTabelle.TabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.IsChecked := True;
      itGerade: rbGerade.IsChecked := True;
      itParabel: rbParabel.IsChecked := True;
      itBezier: rbBezier.IsChecked := True;
    end;
  FTrimmTabelle.GetMemoLines(Memo.Lines);
  ApplyBtnClick(Self);
end;

procedure TFormTrimmTab.OKBtnClick(Sender: TObject);
begin
  FTabellenTyp := FTrimmTabelle.TabellenTyp;
end;

procedure TFormTrimmTab.CancelBtnClick(Sender: TObject);
begin
  { wenn TabellenTyp verändert wurde }
  if FTabellenTyp <> FTrimmTabelle.TabellenTyp then
    case FTabellenTyp of
      { Checked ändern --> Click() wird aufgerufen }
      itKonstante: rbKonstante.IsChecked := True;
      itGerade: rbGerade.IsChecked := True;
      itParabel: rbParabel.IsChecked := True;
      itBezier: rbBezier.IsChecked := True;
    end;
  { restore }
  FTrimmTabelle.TrimmTabDaten := FTrimmTabDaten;
end;

procedure TFormTrimmTab.ApplyBtnClick(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.ProcessTrimmTab(Memo.Lines);
  K1SpinBox.Increment := FTrimmTabelle.EndwertKraft div 30 + 1;
  W2Edit.Text := IntToStr(FTrimmTabelle.EndwertWeg);
  K2Edit.Text := IntToStr(FTrimmTabelle.EndwertKraft);
  Temp := FTrimmTabelle.MittelPunkt;
  { Temp ist notwendig, siehe KraftEditChange,
    W1Edit ist noch nicht gesetzt und verfälscht sonst den Mittelunkt,
    auch umgekehrt. }
  FTabChanging := True;
  K1SpinBox.Text := IntToStr(Temp.X);
  W1SpinBox.Text := IntToStr(Temp.Y);
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TFormTrimmTab.Kraft1EditChange(Sender: TObject);
var
  Temp: TPoint;
begin
  Assert(FTrimmTabelle <> nil);
  Temp.X := StrToInt(K1SpinBox.Text);
  Temp.Y := StrToInt(W1SpinBox.Text);
  FTrimmTabelle.MittelPunkt := Temp; { Temp ist ein Vorschlag für neuen MittelPunkt }
  Temp := FTrimmTabelle.MittelPunkt; { Temp ist jetzt überprüft und ev. korrigiert }
  K1SpinBox.Text := IntToStr(Temp.X);
  W1SpinBox.Text := IntToStr(Temp.Y);
  if not FTabChanging then
    DrawTrimmTab;
end;

procedure TFormTrimmTab.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: single);
var
  tempX, tempY: double;
begin
  tempY := Image.Height;
  tempY := (tempY -Y) * FTrimmTabelle.EndwertKraft / tempY;
  tempX := X * FTrimmTabelle.EndwertWeg / Image.Width;
  FTabChanging := True;
  K1SpinBox.Text := IntToStr(Round(tempY));
  W1SpinBox.Text := IntToStr(Round(tempX));
  FTabChanging := False;
  DrawTrimmTab;
end;

procedure TFormTrimmTab.rbKonstanteClick(Sender: TObject);
begin
  if Sender = rbKonstante then
  begin
    { rbKonstante.Checked := True; }
    K1SpinBox.Enabled := True;
    W1SpinBox.Enabled := False;
  end
  else if Sender = rbGerade then
  begin
    { rbGerade.Checked := True; }
    K1SpinBox.Enabled := False;
    W1SpinBox.Enabled := False;
  end
  else if Sender = rbParabel then
  begin
    { rbParabel.Checked := True; }
    K1SpinBox.Enabled := True;
    W1SpinBox.Enabled := False;
  end
  else if Sender = rbBezier then
  begin
    { rbBezier.Checked := True; }
    K1SpinBox.Enabled := True;
    W1SpinBox.Enabled := True;
  end;
  Assert(FTrimmTabelle <> nil);
  FTrimmTabelle.TabellenTyp := TTabellenTyp((Sender as TRadioButton).Tag);
  DrawTrimmTab;
end;

procedure TFormTrimmTab.CalcBtnClick(Sender: TObject);
begin
  FTrimmTabelle.GetMemoLines(Memo.Lines);
end;

procedure TFormTrimmTab.EvalOptionBtnClick(Sender: TObject);
begin
  FTrimmTabelle.EvalDirection := EvalOptionBtn.IsPressed;
end;

procedure TFormTrimmTab.InitComponentSize;
var
  w, h: Integer;
begin
  ClientHeight := 400;
  ClientWidth := 600;

  w := 75;
  h := 17;
  rbKonstante.Width := w;
  rbKonstante.Height := h;
  rbGerade.Width := w;
  rbGerade.Height := h;
  rbParabel.Width := w;
  rbParabel.Height := h;
  rbBezier.Width := w;
  rbBezier.Height := h;

  Memo.Width := 130;
  Memo.Height := 200;

  Image.Width := 319;
  Image.Height := 158;

  MemoLabel.Width := 108;
  MemoLabel.Height := 13;

  w := 25;
  h := 25;
  WriteMemoBtn.Width := w;
  WriteMemoBtn.Height := h;
  ReadMemoBtn.Width := w;
  ReadMemoBtn.Height := h;
  EvalOptionBtn.Width := w;
  EvalOptionBtn.Height := h;

  w := 20;
  h := 13;
  X1Label.Width := w;
  X1Label.Height := h;
  Y1Label.Width := w;
  Y1Label.Height := h;
  X2Label.Width := w;
  X2Label.Height := h;
  Y2Label.Width := w;
  Y2Label.Height := h;

  w := 40;
  h := 21;
  W2Edit.Width := w;
  W2Edit.Height := h;
  K2Edit.Width := w;
  K2Edit.Height := h;

  w := 80;
  h := 21;
  W1SpinBox.Width := w;
  W1SpinBox.Height := h;
  K1SpinBox.Width := w;
  K1SpinBox.Height := h;

  w := 100;
  h := 27;
  OKBtn.Width := w;
  OKBtn.Height := h;

  CancelBtn.Width := w;
  CancelBtn.Height := h;
end;

procedure TFormTrimmTab.LayoutComponents;
begin
  { Form }

  Left := 400;
  Top := 300;

  { Memo }

  MemoLabel.Position.X := Margin;
  MemoLabel.Position.Y := Margin;

  cr := MemoLabel;
  StackV(Memo);

  { Converter Buttons, between Memo and Image }

  cr := Memo;
  StackH(ReadMemoBtn);
  ReadMemoBtn.Position.Y := ReadMemoBtn.Position.Y + 3 * Margin;
  StackV(WriteMemoBtn);
  StackV(EvalOptionBtn);

  { Radio Buttons, stacked horizontal at top right of form }

  cr := MemoLabel;
  StackH(rbKonstante);
  rbKonstante.Position.X := WriteMemoBtn.Position.X + WriteMemoBtn.Width + Margin;
  StackH(rbGerade);
  StackH(rbParabel);
  StackH(rbBezier);

  { Image }
  cr := rbKonstante;
  StackV(Image);

  { XY Controls }

  StackV(X1Label);
  StackH(W1SpinBox);
  StackH(Y1Label);
  StackH(K1SpinBox);

  cr := W1SpinBox;
  StackV(X2Label);
  StackH(W2Edit);
  StackH(Y2Label);
  StackH(K2Edit);

  { Bottom Buttons }

  StackV(OKBtn);
  OKBtn.Position.X := 80;
  OKBtn.Position.Y := OKBtn.Position.Y + Margin;
  StackH(CancelBtn);

  { Form Size }

  ClientWidth := Round(FMaxRight + Margin);
  ClientHeight := Round(FMaxBottom + Margin);

  { final adjustments }

  Memo.Position.Y := Image.Position.Y;
end;

procedure TFormTrimmTab.InitTabOrder;
begin
  W2Edit.TabStop := False;
  K2Edit.TabStop := False;

  Memo.TabOrder := 0;

  ReadMemoBtn.TabOrder := 1;
  WriteMemoBtn.TabOrder := 2;
  EvalOptionBtn.TabOrder := 3;

  rbKonstante.TabOrder := 4;
  rbGerade.TabOrder := 5;
  rbParabel.TabOrder := 6;
  rbBezier.TabOrder := 7;

  W2Edit.TabOrder := 8;
  K2Edit.TabOrder := 9;

  W1SpinBox.TabOrder := 10;
  K1SpinBox.TabOrder := 11;

  OKBtn.TabOrder := 12;
  CancelBtn.TabOrder := 13;
end;

procedure TFormTrimmTab.InitComponentLinks;
begin
  rbKonstante.OnClick := rbKonstanteClick;
  rbGerade.OnClick := rbKonstanteClick;
  rbParabel.OnClick := rbKonstanteClick;
  rbBezier.OnClick := rbKonstanteClick;

  WriteMemoBtn.OnClick := CalcBtnClick;
  ReadMemoBtn.OnClick := ApplyBtnClick;
  EvalOptionBtn.OnClick := EvalOptionBtnClick;

  Image.OnMouseDown := ImageMouseDown;
  Image.OnScreenScaleChanged := ImageScreenScaleChanged;

  W1SpinBox.OnChange := Kraft1EditChange;
  K1SpinBox.OnChange := Kraft1EditChange;

  OKBtn.OnClick := OKBtnClick;
  CancelBtn.OnClick := CancelBtnClick;
end;

procedure TFormTrimmTab.CreateComponents;
begin
  OKBtn := TButton.Create(Self);
  CancelBtn := TButton.Create(Self);
  MemoLabel := TLabel.Create(Self);
  X1Label := TLabel.Create(Self);
  Y1Label := TLabel.Create(Self);
  X2Label := TLabel.Create(Self);
  Y2Label := TLabel.Create(Self);
  Memo := TMemo.Create(Self);
  Image := TOriginalImage.Create(Self, FTrimmTabGraph.Width, FTrimmTabGraph.Height);
  W1SpinBox := TSpinBox.Create(Self);
  K1SpinBox := TSpinBox.Create(Self);
  W2Edit := TEdit.Create(Self);
  K2Edit := TEdit.Create(Self);
  rbKonstante := TRadioButton.Create(Self);
  rbGerade := TRadioButton.Create(Self);
  rbParabel := TRadioButton.Create(Self);
  rbBezier := TRadioButton.Create(Self);
  EvalOptionBtn := TButton.Create(Self);
  WriteMemoBtn := TButton.Create(Self);
  ReadMemoBtn := TButton.Create(Self);

  rbKonstante.Parent := Self;
  rbGerade.Parent := Self;
  rbParabel.Parent := Self;
  rbBezier.Parent := Self;

  EvalOptionBtn.Parent := Self;
  WriteMemoBtn.Parent := Self;
  ReadMemoBtn.Parent := Self;

  X1Label.Parent := Self;
  Y1Label.Parent := Self;
  X2Label.Parent := Self;
  Y2Label.Parent := Self;
  Memo.Parent := Self;

  Image.Parent := Self;

  W2Edit.Parent :=  Self;
  K2Edit.Parent := Self;

  W1SpinBox.Parent := Self;
  K1SpinBox.Parent := Self;

  OKBtn.Parent := Self;
  CancelBtn.Parent := Self;

  MemoLabel.Parent := Self;
end;

procedure TFormTrimmTab.InitComponentProps;
var
  ML: TStrings;
begin
  Caption := 'Form Trimm Tab';

  MemoLabel.WordWrap := False;
  MemoLabel.AutoSize := True;
  MemoLabel.Text := 'Tabelle (Weg = Kraft)';

  ML := Memo.Lines;
  ML.Add('[X/mm=Y/N]');
  ML.Add('10=60');
  ML.Add('20=90');
  ML.Add('30=100');
  ML.Add('40=160');
  ML.Add('50=183');
  ML.Add('60=200');
  ML.Add('70=205');
  ML.Add('80=208');
  ML.Add('90=220');
  ML.Add('100=225');

  rbKonstante.Text := 'Konstante';
  rbGerade.Text := 'Gerade';
  rbParabel.Text := 'Parabel';
  rbBezier.Text := 'Bezier';

  rbGerade.Tag := 1;
  rbParabel.Tag := 2;
  rbBezier.Tag := 3;

  rbGerade.isChecked := True;

  X1Label.Text := 'X1';
  Y1Label.Text := 'Y1';
  X2Label.Text := 'X2';
  Y2Label.Text := 'Y2';

  W1SpinBox.Text := '0';
  K1SpinBox.Text := '100';
  W2Edit.Text := 'Weg2Edit';
  K2Edit.Text := 'Kraft2Edit';

  W1SpinBox.Hint := 'Wegwert für Punkt 1';
  K1SpinBox.Hint := 'Kraftwert für Punkt 1';
  W2Edit.Hint := 'Endwert Weg';
  K2Edit.Hint := 'Endwert Kraft';

  W2Edit.ParentShowHint := False;
  W2Edit.ReadOnly := True;
  W2Edit.ShowHint := True;

  K2Edit.ParentShowHint := False;
  K2Edit.ReadOnly := True;
  K2Edit.ShowHint := True;

  ReadMemoBtn.Text := '>';
  WriteMemoBtn.Text := '<';
  EvalOptionBtn.Text := 'K';
  EvalOptionBtn.Hint := 'Weg oder Kraft als Argument verwenden|';

  EvalOptionBtn.StaysPressed := True;
  EvalOptionBtn.ParentShowHint := False;
  EvalOptionBtn.ShowHint := True;

  W1SpinBox.Enabled := False;
  K1SpinBox.Enabled := False;
  K1SpinBox.Max := 5000;
  K1SpinBox.Value := 100;

  OKBtn.Text := 'OK';
  OKBtn.ModalResult := 1;

  CancelBtn.Cancel := True;
  CancelBtn.Text := 'Abbrechen';
  CancelBtn.Default := True;
  CancelBtn.ModalResult := 2;
end;

procedure TFormTrimmTab.ImageScreenScaleChanged(Sender: TObject);
begin
  DrawTrimmTab;
end;

procedure TFormTrimmTab.DrawTrimmTab;
begin
  FTrimmTabelle.UpdateGraphModel(FTrimmTabGraph.Model);
  FTrimmTabGraph.Draw;
  Image.Repaint;
end;

procedure TFormTrimmTab.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormTrimmTab.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormTrimmTab.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormTrimmTab.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

end.
