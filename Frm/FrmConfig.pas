unit FrmConfig;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  RggScroll,
  RggUnit4,
  RggTypes,
  System.IniFiles,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Pickers,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.TabControl,
  FMX.ListBox,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.EditBox,
  FMX.SpinBox;

type

  { TFormConfig }

  TFormConfig = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OKBtn: TButton;
    CancelBtn: TButton;

    PageControl: TTabControl;

    { Trimm }
    tsTrimm: TTabItem;
    GroupBoxTrimm: TGroupBox;

    LabelMin: TLabel;
    LabelPos: TLabel;
    LabelMax: TLabel;

    MinEdit: TEdit;
    PosEdit: TEdit;
    MaxEdit: TEdit;

    LengthEditLabel: TLabel;

    TrimmComboLabel: TLabel;
    TrimmCombo: TComboBox;

    { Fachwerk / Material }
    tsFachwerk: TTabItem;
    GroupBoxMaterial: TGroupBox;

    ElementLabel: TLabel;
    ElementCombo: TComboBox;

    EAEdit: TEdit;
    EAEditLabel: TLabel;
    TakeOverBtn: TButton;
    MaterialCombo: TComboBox;
    MaterialComboLabel: TLabel;
    QuerschnittComboLabel: TLabel;
    QuerschnittCombo: TComboBox;
    ALabel: TLabel;
    AEdit: TEdit;
    EEdit: TEdit;
    ELabel: TLabel;
    EEditLabel: TLabel;
    AEditLabel: TLabel;

    { Mast }
    tsMast: TTabItem;
    GroupBoxMast: TGroupBox;

    MastTypeComboLabel: TLabel;
    MastTypeCombo: TComboBox;
    EIEdit: TEdit;
    EILabel: TLabel;

    MastMassComboLabel: TLabel;
    MastMassCombo: TComboBox;
    MastMassEdit: TEdit;
    MassMassEditLabel: TLabel;

    { Rumpf }
    tsRumpf: TTabItem;
    GroupBoxRumpf: TGroupBox;

    Grid: TStringGrid;

    RumpfLabel: TLabel;
    RumpfEdit: TEdit;
    RumpfBtn: TButton;
    RumpfSpinEdit: TSpinBox;

    { Ini Memo }
    tsIniMemo: TTabItem;
    IniMemo: TMemo;
    SaveIniBtn: TButton;
    LoadIniBtn: TButton;

    procedure OKBtnClick(Sender: TObject);

    procedure MastMassEditExit(Sender: TObject);
    procedure MastMassEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure MastTypeComboChange(Sender: TObject);
    procedure MastMassComboChange(Sender: TObject);

    procedure TrimmComboChange(Sender: TObject);
    procedure MinEditExit(Sender: TObject);
    procedure MinEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure QuerschnittComboChange(Sender: TObject);
    procedure MaterialComboChange(Sender: TObject);
    procedure ElementComboChange(Sender: TObject);
    procedure StoreItemClick(Sender: TObject);
    procedure LoadItemClick(Sender: TObject);
    procedure TakeOverBtnClick(Sender: TObject);

    procedure GridSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
    procedure RumpfBtnClick(Sender: TObject);
    procedure RumpfSpinEditChanging(Sender: TObject);
  private
    FiMastSaling: Integer;
    FiMastWante: Integer;
    FiMastTop: Integer;
    FiEI: Integer;
    FEAarray: TRiggLvektor;

    FMastTypList: TStringList;
    FMastMassList: TStringList;
    FElementList: TStringList;
    FMaterialList: TStringList;
    FQuerschnittList: TStringList;
    FTrimmList: TStringList;
    FTempList: TStringList;

    FGSB: TRggFA;
    FiP: TIntRiggPoints;
    FRumpfCell: TPoint;

    FCanSelectDummy: Boolean;

    procedure GetKeyList(Source, Dest: TStringList);
    procedure FillInifileLists;
    procedure FillRiggLists;
    procedure LoadInifileCombos;
    procedure LoadRiggCombos;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure InitGrid;
  public
    Margin: single;
    Raster: Integer;

    Rigg: TRigg;
    IniFileName: string;
    FormShown: Boolean;
    procedure Init(ARigg: TRigg);
    procedure LoadFromIniFile;
    procedure WriteToIniFile;
  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.fmx}

uses
  RiggVar.RG.Def;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  Caption := 'Form Config';

  FMastTypList := TStringList.Create;
  FMastMassList := TStringList.Create;
  FElementList := TStringList.Create;
  FMaterialList := TStringList.Create;
  FQuerschnittList := TStringList.Create;
  FTrimmList := TStringList.Create;
  FTempList := TStringList.Create;

  Margin := 10;
  Raster := 70;

  CreateComponents;

  PageControl.TabIndex := tsFachwerk.Index;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
  FMastTypList.Free;
  FMastMassList.Free;
  FElementList.Free;
  FMaterialList.Free;
  FQuerschnittList.Free;
  FTrimmList.Free;
  FTempList.Free;
end;

procedure TFormConfig.GetKeyList(Source, Dest: TStringList);
var
  i: Integer;
  s: string;
begin
  Dest.Clear;
  for i := 0 to Source.Count - 1 do
  begin
    s := Copy(Source[i], 1, Pos('=', Source[i]) - 1);
    Dest.Add(s);
  end;
end;

procedure TFormConfig.Init(ARigg: TRigg);
begin
  Rigg := ARigg;
  IniFileName := ChangeFileExt(ParamStr(0), '.ini');

  FRumpfCell := Point(1, 0);
  RumpfLabel.Text := 'Feld A0x';

  FillRiggLists;
  LoadRiggCombos;
  FillInifileLists;
  LoadInifileCombos;
end;

procedure TFormConfig.FillRiggLists;
begin
  FGSB := Rigg.GSB;
  FEAarray := Rigg.EA; { EA in KN }
  FiEI := Rigg.MastEI;
  FiMastSaling := Round(Rigg.MastUnten);
  FiMastWante := FiMastSaling + Round(Rigg.MastOben);
  FiMastTop := Round(Rigg.MastLaenge);
  FiP := Rigg.iP;

  FMastMassList.Clear;
  FElementList.Clear;
  FTrimmList.Clear;

  FMastMassList.Add(Format('Saling=%d', [FiMastSaling]));
  FMastMassList.Add(Format('Wante=%d', [FiMastWante]));
  FMastMassList.Add(Format('Top=%d', [FiMastTop]));

  FElementList.Add(Format('Wanten=%.6g', [FEAarray[7]]));
  FElementList.Add(Format('Vorstag=%.6g', [FEAarray[14]]));
  FElementList.Add(Format('Mast=%.6g', [FEAarray[0]]));
  FElementList.Add(Format('Saling=%.6g', [FEAarray[9]]));
  FElementList.Add(Format('Saling-Verbindung=%.6g', [FEAarray[11]]));
  FElementList.Add(Format('Rumpfstäbe=%.6g', [FEAarray[1]]));

  FTrimmList.Add('Controller');
  FTrimmList.Add('Winkel');
  FTrimmList.Add('Vorstag');
  FTrimmList.Add('Wante');
  FTrimmList.Add('Wante oben');
  FTrimmList.Add('Saling Höhe');
  FTrimmList.Add('Saling Abstand');
  FTrimmList.Add('Saling Länge');
end;

procedure TFormConfig.FillInifileLists;
var
  ML: TStrings;
begin
  FMastTypList.Clear;
  FQuerschnittList.Clear;
  FMaterialList.Clear;

  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    if FMastTypList.Count = 0 then
      FMastTypList.Add('TestProfil=15000');
    if FQuerschnittList.Count = 0 then
      FQuerschnittList.Add('Rund D 4 mm=12.56');
    if FMaterialList.Count = 0 then
      FMaterialList.Add('Stahl=210');

    IniMemo.Lines.Clear;
    IniMemo.Lines.LoadFromFile(InifileName);

    Exit;
  end;

  { wenn Inifile nicht existiert dann Standardwerte laden }

  { EI in Nm^2 }
  ML := FMastTypList;
  ML.Add('PD=14700');
  ML.Add('PE=15000');
  ML.Add('PK=18000');

  { A in mm^2 }
  ML := FQuerschnittList;
  ML.Add('Rund D 4 mm=12.56');
  ML.Add('Rund D 10 mm=78.5');
  ML.Add('Profil=315');
  FQuerschnittList.Add('Faktor 100=100');

  { E in KN/mm^2 }
  ML := FMaterialList;
  ML.Add('Stahl=210');
  ML.Add('Niro=250');
  ML.Add('Alu=70');
  ML.Add('Kevlar=200');
  ML.Add('EAklein=10');
  ML.Add('EAmittel=100');
  ML.Add('EAgross=1000');

  ML := IniMemo.Lines;
  ML.Clear;
  ML.Add('[Profile]');
  ML.Add('Profil D=14700');
  ML.Add('Profil E=15000');
  ML.Add('Profil K=18000');
  ML.Add('');
  ML.Add('[Querschnitte]');
  ML.Add('Rund D 4 mm=12.56');
  ML.Add('Rund D 10 mm=78.5');
  ML.Add('Profil=315');
  ML.Add('Faktor 100=100');
  ML.Add('');
  ML.Add('[Material]');
  ML.Add('Stahl=210');
  ML.Add('Niro=250');
  ML.Add('Alu=70');
  ML.Add('Kevlar=200');
  ML.Add('EAklein=10');
  ML.Add('EAmittel=100');
  ML.Add('EAgross=1000');
end;

procedure TFormConfig.LoadRiggCombos;
var
  m: TRiggPoint;
  n: TKoord;
  c, r: Integer;
begin
  { Trimm }
  TrimmCombo.Items := FTrimmList;
  TrimmCombo.ItemIndex := Ord(fpWante);
  MinEdit.Text := IntToStr(Round(FGSB.Wante.Min));
  PosEdit.Text := IntToStr(Round(FGSB.Wante.Ist));
  MaxEdit.Text := IntToStr(Round(FGSB.Wante.Max));

  { Elemente }
  GetKeyList(FElementList, FTempList);
  ElementCombo.Items := FTempList;
  ElementCombo.ItemIndex := 0;
  if ElementCombo.Selected <> nil then
    EAEdit.Text := FElementList.Values[ElementCombo.Selected.Text];

  { MastMaße }
  GetKeyList(FMastMassList, FTempList);
  MastMassCombo.Items := FTempList;
  MastMassCombo.ItemIndex := 0;
  if MastMassCombo.Selected <> nil then
    MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Selected.Text];

  { Werte in FiP im StringGrid anzeigen }
  r := -1; // -1 in FMX
  for m := ooA0 to ooF0 do
  begin
    Inc(r);
    for n := x to z do
    begin
      c := Ord(n) + 1;
      Grid.Cells[c, r] := Format('%4.0f', [FiP[m, n]]);
    end;
  end;
end;

procedure TFormConfig.LoadIniFileCombos;
var
  i, j: Integer;
begin
  { Material }
  GetKeyList(FMaterialList, FTempList);
  MaterialCombo.Items := FTempList;
  MaterialCombo.ItemIndex := 0;
  if MaterialCombo.Selected <> nil then
    EEdit.Text := FMaterialList.Values[MaterialCombo.Selected.Text];

  { Querschnitt }
  GetKeyList(FQuerschnittList, FTempList);
  QuerschnittCombo.Items := FTempList;
  QuerschnittCombo.ItemIndex := 0;
  if QuerschnittCombo.Selected <> nil then
    AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Selected.Text];

  { MastTyp }
  GetKeyList(FMastTypList, FTempList);
  MastTypeCombo.Items := FTempList;
  j := 0;
  for i := 0 to FMastTypList.Count - 1 do
    if IntToStr(FiEI) = FMastTypList.Values[MastTypeCombo.Items[i]] then
      j := i;
  MastTypeCombo.ItemIndex := j;
  if MastTypeCombo.Selected <> nil then
    EIEdit.Text := FMastTypList.Values[MastTypeCombo.Selected.Text];
end;

procedure TFormConfig.LoadFromIniFile;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(IniFileName);
  try
    FMaterialList.Clear;
    IniFile.ReadSectionValues('Material', FMaterialList);
    FQuerschnittList.Clear;
    IniFile.ReadSectionValues('Querschnitte', FQuerschnittList);
    FMastTypList.Clear;
    IniFile.ReadSectionValues('Profile', FMastTypList);
  finally
    IniFile.Free;
  end;
end;

procedure TFormConfig.WriteToIniFile;
begin
  IniMemo.Lines.SaveToFile(InifileName);
end;

procedure TFormConfig.LoadItemClick(Sender: TObject);
var
  s: string;
begin
  if FileExists(InifileName) then
  begin
    LoadFromIniFile;
    LoadInifileCombos;
    IniMemo.Lines.Clear;
    IniMemo.Lines.LoadFromFile(InifileName);
  end
  else
  begin
    s := ExtractFileName(InifileName);
    s := s + 'nicht gefunden';
//    MessageDlg(s, mtInformation, [mbOK], 0);
  end;
end;

procedure TFormConfig.StoreItemClick(Sender: TObject);
begin
  WriteToIniFile;
end;

procedure TFormConfig.TakeOverBtnClick(Sender: TObject);
var
  s: string;
  a, b, c: double;
begin
  a := StrToFloat(EEdit.Text);
  b := StrToFloat(AEdit.Text);
  c := a * b;
  EAEdit.Text := Format('%.6g', [c]);

  if ElementCombo.Selected = nil then
    Exit;

  s := ElementCombo.Selected.Text;
  if s = '' then
    Exit;

  if s = 'Rumpflängen' then
  begin
    FEAarray[1] := c;
    FEAarray[2] := c;
    FEAarray[3] := c;
    FEAarray[4] := c;
    FEAarray[5] := c;
    FEAarray[6] := c;
  end
  else if s = 'Wanten' then
  begin
    FEAarray[7] := c;
    FEAarray[8] := c;
    FEAarray[12] := c;
    FEAarray[13] := c;
  end
  else if s = 'Vorstag' then
    FEAarray[14] := c
  else if s = 'Saling' then
  begin
    FEAarray[9] := c;
    FEAarray[10] := c;
  end
  else if s = 'Saling-Verbindung' then
    FEAarray[11] := c;

  FElementList.Values[s] := EAEdit.Text;
end;

procedure TFormConfig.TrimmComboChange(Sender: TObject);
var
  i: TFederParam;
  f: TRggSB;
begin
  i := TFederParam(TrimmCombo.ItemIndex);
  if i = TFederParam.fpWinkel then
    LengthEditLabel.Text := 'Winkel in Grad'
  else
    LengthEditLabel.Text := 'Abmessungen in mm';

  f := FGSB.Find(i);
  MinEdit.Text := IntToStr(Round(f.Min));
  PosEdit.Text := IntToStr(Round(f.Ist));
  MaxEdit.Text := IntToStr(Round(f.Max));
end;

procedure TFormConfig.MinEditKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = VKReturn then
    MinEditExit(Sender);
end;

procedure TFormConfig.MinEditExit(Sender: TObject);
var
  i: Integer;
  f: TRggSB;
  iMin, iIst, iMax: Integer;
  iVar: Integer;
begin
  i := TrimmCombo.ItemIndex;
  f := FGSB.Find(TFederParam(i));
  iMin := Round(f.Min);
  iIst := Round(f.Ist);
  iMax := Round(f.Max);

  if Sender = MinEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iMin);
    if iVar > iIst then
      iVar := iIst;
    f.Min := iVar;
    MinEdit.Text := IntToStr(iVar);
  end;

  if Sender = PosEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iIst);
    if iVar < iMin then
      iVar := iMin;
    if iVar > iMax then
      iVar := iMax;
    f.Ist := iVar;
    PosEdit.Text := IntToStr(iVar);
  end;

  if Sender = MaxEdit then
  begin
    iVar := StrToIntDef(TEdit(Sender).Text, iMax);
    if iVar < iIst then
      iVar := iIst;
    f.Max := iVar;
    MaxEdit.Text := IntToStr(iVar);
  end;
end;

procedure TFormConfig.MastMassEditKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = VKReturn then
    MastMassEditExit(Sender);
end;

procedure TFormConfig.MastMassEditExit(Sender: TObject);
var
  i: Integer;
  temp: Integer;
  s: string;
begin
  if MastMassCombo.Selected = nil then
    Exit;

  s := MastMassCombo.Selected.Text;

  temp := 0;
  if s = 'Saling' then
    temp := FiMastSaling
  else if s = 'Wante' then
    temp := FiMastWante
  else if s = 'Top' then
    temp := FiMastTop;

  i := StrToIntDef(MastMassEdit.Text, temp);
  MastMassEdit.Text := IntToStr(i);

  FMastMassList.Values[s] := IntToStr(i);
  if s = 'Saling' then
    FiMastSaling := i
  else if s = 'Wante' then
    FiMastWante := i
  else if s = 'Top' then
    FiMastTop := i;
end;

procedure TFormConfig.OKBtnClick(Sender: TObject);
begin
  Rigg.iP := FiP; { Rumpfkoordinaten}
  Rigg.MastUnten := FiMastSaling;
  Rigg.MastOben := FiMastWante - FiMastSaling;
  Rigg.MastLaenge := FiMastTop;
  Rigg.GSB := FGSB; { neue Grenzen und Istwerte }
  Rigg.EA := FEAarray;
  Rigg.MastEI := FiEI;
end;

procedure TFormConfig.MastTypeComboChange(Sender: TObject);
begin
  if MastTypeCombo.Selected <> nil then
  begin
    EIEdit.Text := FMastTypList.Values[MastTypeCombo.Selected.Text];
    FiEI := StrToInt(EIEdit.Text);
  end;
end;

procedure TFormConfig.MastMassComboChange(Sender: TObject);
begin
  if MastMassCombo.Selected <> nil then
  begin
    MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Selected.Text];
  end;
end;

procedure TFormConfig.QuerschnittComboChange(Sender: TObject);
begin
  if QuerschnittCombo.Selected <> nil then
  begin
    AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Selected.Text];
  end;
end;

procedure TFormConfig.MaterialComboChange(Sender: TObject);
begin
  if MaterialCombo.Selected <> nil then
  begin
    EEdit.Text := FMaterialList.Values[MaterialCombo.Selected.Text];
  end;
end;

procedure TFormConfig.ElementComboChange(Sender: TObject);
begin
  if ElementCombo.Selected <> nil then
  begin
    EAEdit.Text := FElementList.Values[ElementCombo.Selected.Text];
  end;
end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;
    FormShown := True;
  end;

  FillRiggLists;
  LoadRiggCombos;

  GridSelectCell(Grid, 1, 0, FCanSelectDummy);
//  RumpfSpinEdit.Value := StrToIntDef(Grid.Cells[FRumpfCell.X, FRumpfCell.Y], 0);
//  RumpfEdit.Text := Format('%4.0f mm',[RumpfSpinEdit.Value]);
end;

procedure TFormConfig.GridSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  sColumn, sHeader, SCell: string;
begin
  CanSelect := True;

  if ACol = 0 then
    CanSelect := False;

  if (ACol = 2) and (ARow > 1) then
    CanSelect := False;

  if CanSelect then
  begin
    FRumpfCell := Point(ACol, ARow);

    sColumn := TrimLeft(Grid.Cells[0, ARow]);
    sCell := Grid.Cells[ACol, ARow];
    sHeader := Grid.Columns[ACol].Header;

    RumpfLabel.Text := Format('Feld %s%s:', [sColumn, sHeader]);
    if RumpfSpinEdit <> nil then
    begin
      RumpfSpinEdit.Value := StrToIntDef(sCell, 0);
      RumpfEdit.Text := Format('%4.0f mm', [RumpfSpinEdit.Value]);
    end;
  end;
end;

procedure TFormConfig.RumpfBtnClick(Sender: TObject);
var
  oo: TRiggPoint;
  kk: TKoord;
begin
  { changed from old days:
    - introduction of ooN0
    - elements of a TIntPoint are of type double now, no longer an Integer
  }

  oo := TRiggPoint(FRumpfCell.Y); { A0 is now second element in enum }
  kk := TKoord(FRumpfCell.X - 1); {this has not changed }

  FiP[oo, kk] := RumpfSpinEdit.Value;
  Grid.Cells[FRumpfCell.X, FRumpfCell.Y] := Format('%4.0f', [RumpfSpinEdit.Value]);
  if FRumpfCell.Y = 2 then
  begin
    FiP[ooA0] := FiP[ooB0];
    FiP[ooA0, y] := -FiP[ooB0, y];
    Grid.Cells[1, 1] := Format('%4.0f', [FiP[ooA0, x]]);
    Grid.Cells[2, 1] := Format('%4.0f', [FiP[ooA0, y]]);
    Grid.Cells[3, 1] := Format('%4.0f', [FiP[ooA0, z]]);
  end;
  if FRumpfCell.Y = 1 then
  begin
    FiP[ooB0] := FiP[ooA0];
    FiP[ooB0, y] := -FiP[ooA0, y];
    Grid.Cells[1, 2] := Format('%4.0f', [FiP[ooB0, x]]);
    Grid.Cells[2, 2] := Format('%4.0f', [FiP[ooB0, y]]);
    Grid.Cells[3, 2] := Format('%4.0f', [FiP[ooB0, z]]);
  end;
end;

procedure TFormConfig.RumpfSpinEditChanging(Sender: TObject);
begin
  RumpfEdit.Text := Format('%4.0f mm',[RumpfSpinEdit.Value]);
end;

procedure TFormConfig.CreateComponents;
var
  pc: TTabControl;
  ts: TTabItem;
  gb: TGroupBox;
  ML: TStrings;
begin
  OKBtn :=  TButton.Create(Self);
  OKBtn.Parent := Self;
  OKBtn.Text := 'OK';
  OKBtn.ModalResult := 1;

  CancelBtn := TButton.Create(Self);
  CancelBtn.Parent := Self;
  CancelBtn.Cancel := True;
  CancelBtn.Text := 'Abbrechen';
  CancelBtn.Default := True;
  CancelBtn.ModalResult := 2;

  PageControl := TTabControl.Create(Self);
  pc := PageControl;
  pc.Parent := Self;

  { Page Trimm }

  tsTrimm := pc.Add(TTabItem);
  tsTrimm.Text := 'Trimm';
  ts := tsTrimm;

  GroupBoxTrimm := TGroupBox.Create(Self);
  GroupBoxTrimm.Text := 'Längen';
  gb := GroupBoxTrimm;
  gb.Parent := ts;

  LabelMin := TLabel.Create(Self);
  LabelMin.Parent := gb;
  LabelMin.Text := 'Min';

  LabelPos := TLabel.Create(Self);
  LabelPos.Parent := gb;
  LabelPos.Text := 'Pos';

  LabelMax := TLabel.Create(Self);
  LabelMax.Parent := gb;
  LabelMax.Text := 'Max';

  MinEdit := TEdit.Create(Self);
  MinEdit.Parent := gb;
  MinEdit.Text := '    ';

  PosEdit := TEdit.Create(Self);
  PosEdit.Parent := gb;
  PosEdit.Text := '    ';

  MaxEdit := TEdit.Create(Self);
  MaxEdit.Parent := gb;
  MaxEdit.Text := '    ';

  LengthEditLabel := TLabel.Create(Self);
  LengthEditLabel.Parent := gb;
  LengthEditLabel.Text := 'Abmessungen in mm';

  TrimmComboLabel := TLabel.Create(Self);
  TrimmComboLabel.Parent := gb;
  TrimmComboLabel.Text := 'Trimmvariable';

  TrimmCombo := TComboBox.Create(Self);
  TrimmCombo.Parent := gb;
  ML := TrimmCombo.Items;
  ML.Add('Controller');
  ML.Add('Winkel');
  ML.Add('Vorstag');
  ML.Add('Wante');
  ML.Add('Wante oben');
  ML.Add('Saling Höhe');
  ML.Add('Saling Abstand');
  ML.Add('Saling Länge');

  { Page Fachwerk / Material }

  tsFachwerk := pc.Add(TTabItem);
  tsFachwerk.Text := 'Fachwerk';
  ts := tsFachwerk;

  GroupBoxMaterial := TGroupBox.Create(Self);
  GroupBoxMaterial.Text := 'Material';
  gb := GroupBoxMaterial;
  gb.Parent := ts;

  ElementLabel := TLabel.Create(Self);
  ElementLabel.Parent := ts;
  ElementLabel.Text := 'Fachwerkstäbe';

  ElementCombo := TComboBox.Create(Self);
  ElementCombo.Parent := ts;
  ML := ElementCombo.Items;
  ML.Add('Rumpfstäbe');
  ML.Add('Wanten');
  ML.Add('Vorstag');
  ML.Add('Saling');
  ML.Add('Saling-Verbindung');

  EAEdit := TEdit.Create(Self);
  EAEdit.Parent := ts;
  EAEdit.Text := 'EAEdit';
  EAEdit.ReadOnly := True;

  EAEditLabel := TLabel.Create(Self);
  EAEditLabel.Parent := ts;
  EAEditLabel.Text := 'EA in KN';

  TakeOverBtn := TButton.Create(Self);
  TakeOverBtn.Parent := ts;
  TakeOverBtn.Text := 'Auswahl übernehmen';

  MaterialCombo := TComboBox.Create(Self);
  MaterialCombo.Parent := gb;

  MaterialComboLabel := TLabel.Create(Self);
  MaterialComboLabel.Parent := gb;
  MaterialComboLabel.Text := 'Material';

  QuerschnittComboLabel := TLabel.Create(Self);
  QuerschnittComboLabel.Parent := gb;
  QuerschnittComboLabel.Text := 'Querschnitt';

  QuerschnittCombo := TComboBox.Create(Self);
  QuerschnittCombo.Parent := gb;

  ALabel := TLabel.Create(Self);
  ALabel.Parent := gb;
  ALabel.Text := 'A';

  AEdit := TEdit.Create(Self);
  AEdit.Parent := gb;
  AEdit.Text := 'AEdit';
  AEdit.ReadOnly := True;

  EEdit := TEdit.Create(Self);
  EEdit.Parent := gb;
  EEdit.Text := 'EEdit';
  EEdit.ReadOnly := True;

  ELabel := TLabel.Create(Self);
  ELabel.Parent := gb;
  ELabel.Text := 'E';

  EEditLabel := TLabel.Create(Self);
  EEditLabel.Parent := gb;
  EEditLabel.Text := 'E-Modul in KN/mm^2';

  AEditLabel := TLabel.Create(Self);
  AEditLabel.Parent := gb;
  AEditLabel.Text := 'Querschnitt in mm^2';

  { Page Mast }

  tsMast := pc.Add(TTabItem);
  tsMast.Text := 'Mast';
  ts := tsMast;

  GroupBoxMast := TGroupBox.Create(Self);
  GroupBoxMast.Text := 'Mast';
  gb := GroupBoxMast;
  gb.Parent := ts;

  MastTypeComboLabel := TLabel.Create(Self);
  MastTypeComboLabel.Parent := gb;
  MastTypeComboLabel.Text := 'Profil';

  MastTypeCombo := TComboBox.Create(Self);
  MastTypeCombo.Parent := gb;

  EIEdit := TEdit.Create(Self);
  EIEdit.Parent := gb;
  EIEdit.Text := 'EIEdit';
  EIEdit.ReadOnly := True;

  EILabel := TLabel.Create(Self);
  EILabel.Parent := gb;
  EILabel.Text := 'Biegesteifigkeit EI in Nm^2';

  MastMassComboLabel := TLabel.Create(Self);
  MastMassComboLabel.Parent := gb;
  MastMassComboLabel.Text := 'Abmessungen';

  MastMassCombo := TComboBox.Create(Self);
  MastMassCombo.Parent := gb;
  ML := MastMassCombo.Items;
  ML.Add('Controller');
  ML.Add('Saling');
  ML.Add('Wante');
  ML.Add('Top');

  MastMassEdit := TEdit.Create(Self);
  MastMassEdit.Parent := gb;

  MassMassEditLabel := TLabel.Create(Self);
  MassMassEditLabel.Parent := gb;
  MassMassEditLabel.Text := 'Abstand vom Mastfuß in mm';

  { Page Rumpf }

  tsRumpf := pc.Add(TTabItem);
  tsRumpf.Text := 'Rumpf';
  ts := tsRumpf;

  GroupBoxRumpf := TGroupBox.Create(Self);
  GroupBoxRumpf.Text := 'Feld Editieren';
  gb := GroupBoxRumpf;
  gb.Parent := ts;

  RumpfLabel := TLabel.Create(Self);
  RumpfLabel.Parent := gb;
  RumpfLabel.Text := 'RumpfLabel';
  RumpfLabel.WordWrap := False;
  RumpfLabel.AutoSize := True;

  RumpfEdit := TEdit.Create(Self);
  RumpfEdit.Parent := gb;
  RumpfEdit.Text := '10';
  RumpfEdit.ReadOnly := True;

  RumpfBtn := TButton.Create(Self);
  RumpfBtn.Parent := gb;
  RumpfBtn.Text := 'Übernehmen';

  RumpfSpinEdit := TSpinBox.Create(Self);
  RumpfSpinEdit.Parent := gb;
  RumpfSpinEdit.Min := -32000;
  RumpfSpinEdit.Max := 32000;
  RumpfSpinEdit.Value := 10;

  Grid := TStringGrid.Create(Self);
  Grid.Parent := ts;
  InitGrid;

  { Page Ini }

  tsIniMemo := pc.Add(TTabItem);
  tsIniMemo.Text := 'Rigg.ini';
  ts := tsIniMemo;

  IniMemo := TMemo.Create(Self);
  IniMemo.Parent := ts;
  IniMemo.ShowScrollBars := True;

  SaveIniBtn := TButton.Create(Self);
  SaveIniBtn.Parent := ts;
  SaveIniBtn.Text := 'Speichern';

  LoadIniBtn := TButton.Create(Self);
  LoadIniBtn.Parent := ts;
  LoadIniBtn.Text := 'Laden';
end;

procedure TFormConfig.LayoutComponents;
begin
  Left := 230;
  Top := 113;

  PageControl.Position.X := 8;
  PageControl.Position.Y := 8;
  PageControl.Width := 521;
  PageControl.Height := 265;
  PageControl.TabOrder := 0;

  { Trimm }

  GroupBoxTrimm.Position.X := 16;
  GroupBoxTrimm.Position.Y := 16;
  GroupBoxTrimm.Width := 473;
  GroupBoxTrimm.Height := 169;
  GroupBoxTrimm.TabOrder := 0;

  LabelMin.Position.X := 48;
  LabelMin.Position.Y := 46;
  LabelMin.Width := 21;
  LabelMin.Height := 16;

  LabelPos.Position.X := 112;
  LabelPos.Position.Y := 46;
  LabelPos.Width := 24;
  LabelPos.Height := 16;

  LabelMax.Position.X := 176;
  LabelMax.Position.Y := 46;
  LabelMax.Width := 25;
  LabelMax.Height := 16;

  MinEdit.Position.X := 48;
  MinEdit.Position.Y := 64;
  MinEdit.Width := 41;
  MinEdit.Height := 21;
  MinEdit.MaxLength := 4;
  MinEdit.TabOrder := 1;
  MinEdit.OnExit := MinEditExit;
  MinEdit.OnKeyDown := MinEditKeyDown;

  PosEdit.Position.X := 112;
  PosEdit.Position.Y := 64;
  PosEdit.Width := 41;
  PosEdit.Height := 21;
  PosEdit.MaxLength := 4;
  PosEdit.TabOrder := 2;
  PosEdit.OnExit := MinEditExit;
  PosEdit.OnKeyDown := MinEditKeyDown;

  MaxEdit.Position.X := 176;
  MaxEdit.Position.Y := 64;
  MaxEdit.Width := 41;
  MaxEdit.Height := 21;
  MaxEdit.MaxLength := 4;
  MaxEdit.TabOrder := 0;
  MaxEdit.OnExit := MinEditExit;
  MaxEdit.OnKeyDown := MinEditKeyDown;

  LengthEditLabel.Position.X := 228;
  LengthEditLabel.Position.Y := 65;
  LengthEditLabel.Width := 125;
  LengthEditLabel.Height := 16;

  TrimmComboLabel.Position.X := 228;
  TrimmComboLabel.Position.Y := 112;
  TrimmComboLabel.Width := 87;
  TrimmComboLabel.Height := 16;

  TrimmCombo.Position.X := 48;
  TrimmCombo.Position.Y := 111;
  TrimmCombo.Width := 169;
  TrimmCombo.Height := 21;
  TrimmCombo.TabOrder := 3;
  TrimmCombo.OnChange := TrimmComboChange;

  { Fachwerk }

  ElementLabel.Position.X := 24;
  ElementLabel.Position.Y := 10;
  ElementLabel.Width := 92;
  ElementLabel.Height := 16;

  ElementCombo.Position.X := 24;
  ElementCombo.Position.Y := 32;
  ElementCombo.Width := 161;
  ElementCombo.Height := 21;
  ElementCombo.TabOrder := 0;
  ElementCombo.OnChange := ElementComboChange;

  EAEdit.Position.X := 200;
  EAEdit.Position.Y := 32;
  EAEdit.Width := 105;
  EAEdit.Height := 21;
  EAEdit.TabStop := False;
  EAEdit.TabOrder := 1;

  EAEditLabel.Position.X := 200;
  EAEditLabel.Position.Y := 10;
  EAEditLabel.Width := 52;
  EAEditLabel.Height := 16;

  TakeOverBtn.Position.X := 319;
  TakeOverBtn.Position.Y := 30;
  TakeOverBtn.Width := 182;
  TakeOverBtn.Height := 25;
  TakeOverBtn.TabOrder := 2;
  TakeOverBtn.OnClick := TakeOverBtnClick;

  GroupBoxMaterial.Position.X := 24;
  GroupBoxMaterial.Position.Y := 74;
  GroupBoxMaterial.Width := 477;
  GroupBoxMaterial.Height := 151;
  GroupBoxMaterial.TabOrder := 3;

  MaterialComboLabel.Position.X := 31;
  MaterialComboLabel.Position.Y := 30;
  MaterialComboLabel.Width := 48;
  MaterialComboLabel.Height := 16;

  MaterialCombo.Position.X := 31;
  MaterialCombo.Position.Y := 52;
  MaterialCombo.Width := 153;
  MaterialCombo.Height := 21;
  MaterialCombo.TabOrder := 0;
  MaterialCombo.OnChange := MaterialComboChange;

  ELabel.Position.X := 198;
  ELabel.Position.Y := 31;
  ELabel.Width := 9;
  ELabel.Height := 16;

  EEdit.Position.X := 198;
  EEdit.Position.Y := 53;
  EEdit.Width := 73;
  EEdit.Height := 21;
  EEdit.TabStop := False;
  EEdit.TabOrder := 3;

  EEditLabel.Position.X := EEdit.Position.X + EEdit.Width + Margin;
  EEditLabel.Position.Y := EEdit.Position.Y;
  EEditLabel.Width := 124;
  EEditLabel.Height := 16;

  QuerschnittComboLabel.Position.X := 31;
  QuerschnittComboLabel.Position.Y := 92;
  QuerschnittComboLabel.Width := 66;
  QuerschnittComboLabel.Height := 16;

  QuerschnittCombo.Position.X := 31;
  QuerschnittCombo.Position.Y := 114;
  QuerschnittCombo.Width := 153;
  QuerschnittCombo.Height := 21;
  QuerschnittCombo.TabOrder := 1;
  QuerschnittCombo.OnChange := QuerschnittComboChange;

  ALabel.Position.X := 198;
  ALabel.Position.Y := 92;
  ALabel.Width := 9;
  ALabel.Height := 16;

  AEdit.Position.X := 198;
  AEdit.Position.Y := 114;
  AEdit.Width := 73;
  AEdit.Height := 21;
  AEdit.TabStop := False;
  AEdit.TabOrder := 2;

  AEditLabel.Position.X := AEdit.Position.X + AEdit.Width + Margin;;
  AEditLabel.Position.Y := AEdit.Position.Y;
  AEditLabel.Width := 118;
  AEditLabel.Height := 16;

  { Mast }

  GroupBoxMast.Position.X := 18;
  GroupBoxMast.Position.Y := 23;
  GroupBoxMast.Width := 471;
  GroupBoxMast.Height := 194;
  GroupBoxMast.TabOrder := 0;

  MastTypeComboLabel.Position.X := 24;
  MastTypeComboLabel.Position.Y := 38;
  MastTypeComboLabel.Width := 30;
  MastTypeComboLabel.Height := 16;

  MastTypeCombo.Position.X := 24;
  MastTypeCombo.Position.Y := 56;
  MastTypeCombo.Width := 145;
  MastTypeCombo.Height := 21;
  MastTypeCombo.TabOrder := 2;
  MastTypeCombo.OnChange := MastTypeComboChange;

  EIEdit.Position.X := 185;
  EIEdit.Position.Y := 56;
  EIEdit.Width := 73;
  EIEdit.Height := 21;
  EIEdit.TabStop := False;
  EIEdit.TabOrder := 3;

  EILabel.Position.X := 264;
  EILabel.Position.Y := 57;
  EILabel.Width := 158;
  EILabel.Height := 16;

  MastMassComboLabel.Position.X := 24;
  MastMassComboLabel.Position.Y := 110;
  MastMassComboLabel.Width := 87;
  MastMassComboLabel.Height := 16;

  MastMassCombo.Position.X := 24;
  MastMassCombo.Position.Y := 132;
  MastMassCombo.Width := 145;
  MastMassCombo.Height := 21;
  MastMassCombo.TabOrder := 0;
  MastMassCombo.OnChange := MastMassComboChange;

  MastMassEdit.Position.X := 185;
  MastMassEdit.Position.Y := 132;
  MastMassEdit.Width := 73;
  MastMassEdit.Height := 21;
  MastMassEdit.MaxLength := 4;
  MastMassEdit.TabOrder := 1;
  MastMassEdit.OnExit := MastMassEditExit;
  MastMassEdit.OnKeyDown := MastMassEditKeyDown;

  MassMassEditLabel.Position.X := 264;
  MassMassEditLabel.Position.Y := 133;
  MassMassEditLabel.Width := 167;
  MassMassEditLabel.Height := 16;

  { Hull }

  GroupBoxRumpf.Position.X := Grid.Position.X + Grid.Width + Margin;
  GroupBoxRumpf.Position.Y := Grid.Position.Y + Margin;
  GroupBoxRumpf.Width := 180;
  GroupBoxRumpf.Height := 180;

  RumpfLabel.Position.X := 24;
  RumpfLabel.Position.Y := 30;

  RumpfEdit.Position.X := 24;
  RumpfEdit.Position.Y := RumpfLabel.Position.Y + RumpfLabel.Height + Margin;
  RumpfEdit.Width := 81;
  RumpfEdit.Height := 24;
  RumpfEdit.TabOrder := 1;

  RumpfSpinEdit.Position.X := RumpfEdit.Position.X;
  RumpfSpinEdit.Position.Y := RumpfEdit.Position.Y + RumpfEdit.Height + Margin;
  RumpfSpinEdit.Width := 100;
  RumpfSpinEdit.Height := 24;
  RumpfSpinEdit.TabOrder := 2;
  RumpfSpinEdit.TabStop := True;
  RumpfSpinEdit.OnChange := RumpfSpinEditChanging;

  RumpfBtn.Position.X := 24;
  RumpfBtn.Position.Y := RumpfSpinEdit.Position.Y + RumpfSpinEdit.Height + Margin;
  RumpfBtn.Width := 129;
  RumpfBtn.Height := 25;
  RumpfBtn.TabOrder := 0;
  RumpfBtn.OnClick := RumpfBtnClick;

  { Ini Memo }

  IniMemo.Position.X := 0;
  IniMemo.Position.Y := 0;
  IniMemo.Width := 393;
  IniMemo.Height := 237;
  IniMemo.Align := TAlignLayout.Left;
  IniMemo.TabOrder := 0;

  SaveIniBtn.Position.X := 408;
  SaveIniBtn.Position.Y := 16;
  SaveIniBtn.Width := 81;
  SaveIniBtn.Height := 25;
  SaveIniBtn.TabOrder := 1;
  SaveIniBtn.OnClick := StoreItemClick;

  LoadIniBtn.Position.X := 408;
  LoadIniBtn.Position.Y := 47;
  LoadIniBtn.Width := 81;
  LoadIniBtn.Height := 25;
  LoadIniBtn.TabOrder := 2;
  LoadIniBtn.OnClick := LoadItemClick;

  { Buttons }

  OKBtn.Position.X := 169;
  OKBtn.Position.Y := PageControl.Position.Y + PageControl.Height + Margin;
  OKBtn.Width := 81;
  OKBtn.Height := 27;
  OKBtn.TabOrder := 1;
  OKBtn.OnClick := OKBtnClick;

  CancelBtn.Position.X := 268;
  CancelBtn.Position.Y := OKBtn.Position.Y;
  CancelBtn.Width := 101;
  CancelBtn.Height := 27;
  CancelBtn.TabOrder := 2;

  ClientWidth := Round(PageControl.Position.X  + PageControl.Width + Margin);
  ClientHeight := Round(OKBtn.Position.Y + OKBtn.Height + Margin);
end;

procedure TFormConfig.InitGrid;
var
  c: TStringColumn;
  w: single;
  go: TGridOptions;
begin
  Grid.Position.X := Margin;
  Grid.Position.Y := Margin;
  Grid.Width := 250;
  Grid.Height := 200;

  w := 60;

  c := TStringColumn.Create(Grid);
  c.Width := w;
  c.Header := 'z';
  Grid.Model.InsertColumn(0, c);

  c := TStringColumn.Create(Grid);
  c.Width := w;
  c.Header := 'y';
  Grid.Model.InsertColumn(0, c);

  c := TStringColumn.Create(Grid);
  c.Width := w;
  c.Header := 'x';
  Grid.Model.InsertColumn(0, c);

  c := TStringColumn.Create(Grid);
  c.Width := 40;
  c.Header := '';
  Grid.Model.InsertColumn(0, c);

  Grid.RowCount := 6;
  Grid.ShowScrollBars := False;

  go := Grid.Options;
  Include(go, TGridOption.AlwaysShowSelection);
  Exclude(go, TGridOption.ColumnResize);
  Grid.Options := go;

  Grid.TabOrder := 0;

  Grid.Cells[0, 0] := '   A0';
  Grid.Cells[0, 1] := '   B0';
  Grid.Cells[0, 2] := '   C0';
  Grid.Cells[0, 3] := '   D0';
  Grid.Cells[0, 4] := '   E0';
  Grid.Cells[0, 5] := '   F0';

  FRumpfCell := Point(1, 0);
  Grid.ReadOnly := True;
  Grid.SelectCell(FRumpfCell.X, FRumpfCell.Y);
  Grid.OnSelectCell := GridSelectCell;

  { for testing only }
  GridSelectCell(Grid, 1, 0, FCanSelectDummy);
end;

end.
