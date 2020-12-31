unit FrmConfig;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  RiggVar.App.Strings,
  RiggVar.RG.Scroll,
  RiggVar.RG.Types,
  RiggVar.App.Model,
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

{.$define WantIniFile}
type

  { TFormConfig }

  TRggComboBox = class(TComboBox)
  private
    function GetText: string;
  public
    property Text: string read GetText;
  end;

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
    TrimmCombo: TRggComboBox;

    { Fachwerk / Material }
    tsFachwerk: TTabItem;
    GroupBoxMaterial: TGroupBox;

    ElementLabel: TLabel;
    ElementCombo: TRggComboBox;

    EAEdit: TEdit;
    EAEditLabel: TLabel;
    TakeOverBtn: TButton;
    MaterialCombo: TRggComboBox;
    MaterialComboLabel: TLabel;
    QuerschnittComboLabel: TLabel;
    QuerschnittCombo: TRggComboBox;
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
    MastTypeCombo: TRggComboBox;
    EIEdit: TEdit;
    EILabel: TLabel;

    MastMassComboLabel: TLabel;
    MastMassCombo: TRggComboBox;
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
  protected
    cr: TControl;
    TempR: single;
    TempB: single;
    FMaxRight: single;
    FMaxBottom: single;

    Margin: single;
    Raster: single;

    FScale: single;
    function Scale(Value: Integer): Integer;

    procedure RecordMax;
    procedure AnchorVertical(c: TControl);
    procedure StackH(c: TControl);
    procedure StackV(c: TControl);

    procedure InitComponentSize;
    procedure InitComponentProps;
    procedure InitComponentLinks;
    procedure InitTabOrder;
  private
    FiMastSaling: Integer;
    FiMastWante: Integer;
    FiMastTop: Integer;
    FiEI: Integer;
    FEAarray: TRiggRods;

    FMastTypList: TStringList;
    FMastMassList: TStringList;
    FElementList: TStringList;
    FMaterialList: TStringList;
    FQuerschnittList: TStringList;
    FTrimmList: TStringList;
    FTempList: TStringList;

    FGSB: TRggFA;
    FiP: TRiggPoints;
    FRumpfCell: TPoint;

    FCanSelectDummy: Boolean;

    procedure GetKeyList(Source, Dest: TStringList);
    procedure FillIniListsWithDefault;
    function FillIniListsFromFile: Boolean;
    procedure FillIniLists;
    procedure FillRiggLists;
    procedure LoadInifileCombos;
    procedure LoadRiggCombos;
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure InitGrid;
    procedure SelectInitialCell;
  public
    FirstColumnIndex: Integer;
    FirstRowIndex: Integer;
    SecondRowIndex: Integer;

    Rigg: IRigg;
    IniFileName: string;
    FormShown: Boolean;
    procedure Init(ARigg: IRigg);
    procedure LoadFromIniFile;
    procedure WriteToIniFile;
  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main,
  RiggVar.RG.Def;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  Caption := 'Form Config';

  FScale := 1.0;

  FGSB := TRggFA.Create;

  FMastTypList := TStringList.Create;
  FMastMassList := TStringList.Create;
  FElementList := TStringList.Create;
  FMaterialList := TStringList.Create;
  FQuerschnittList := TStringList.Create;
  FTrimmList := TStringList.Create;
  FTempList := TStringList.Create;

  FirstColumnIndex := 1;
  FirstRowIndex := 0; // 0 in FMX and 1 in VCL/LCL
  SecondRowIndex := FirstRowIndex + 1;

  Margin := Scale(10);
  Raster := Scale(70);

  CreateComponents;
  InitComponentSize;

  PageControl.TabIndex := tsRumpf.Index;
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
  FGSB.Free;
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

procedure TFormConfig.SelectInitialCell;
var
  b: Boolean;
begin
  b := True;
  FRumpfCell := Point(1, FirstRowIndex);
  GridSelectCell(nil, FRumpfCell.X, FRumpfCell.Y, b);
end;

procedure TFormConfig.Init(ARigg: IRigg);
begin
  Rigg := ARigg;

{$ifdef WantIniFile}
  if not MainConst.MustBeSandboxed then
  IniFileName := ChangeFileExt(ParamStr(0), '.ini');
{$endif}

  SelectInitialCell;

  FillRiggLists;
  LoadRiggCombos;
  FillIniLists;
  LoadInifileCombos;
end;

procedure TFormConfig.FillRiggLists;
var
  fs: string;
begin
  FGSB.Assign(Rigg.RggFA);
  FEAarray := Rigg.EA; { EA in KN }
  FiEI := Rigg.MastEI;
  FiMastSaling := Round(Rigg.MastUnten);
  FiMastWante := FiMastSaling + Round(Rigg.MastOben);
  FiMastTop := Round(Rigg.MastLength);
  FiP := Rigg.RiggPoints;

  FMastMassList.Clear;
  FElementList.Clear;
  FTrimmList.Clear;

  fs := '%s=%d';
  FMastMassList.Add(Format(fs, [RggLocalizedStrings.MastComboTextSpreader, FiMastSaling]));
  FMastMassList.Add(Format(fs, [RggLocalizedStrings.MastComboTextShroud, FiMastWante]));
  FMastMassList.Add(Format(fs, [RggLocalizedStrings.MastComboTextTop, FiMastTop]));

  fs := '%s=%.6g';
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextSpreader, FEAarray.B0B]));
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextVorstag, FEAarray.C0C]));
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextMast, FEAarray.D0C]));
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextSpreader, FEAarray.BD]));
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextSpreaderConnection, FEAarray.AB]));
  FElementList.Add(Format(fs, [RggLocalizedStrings.ComboTextHullRods, FEAarray.C0D0]));

  FTrimmList.Add(RggLocalizedStrings.ControllerString);
  FTrimmList.Add(RggLocalizedStrings.WinkelString);
  FTrimmList.Add(RggLocalizedStrings.VorstagString);
  FTrimmList.Add(RggLocalizedStrings.WanteString);
  FTrimmList.Add(RggLocalizedStrings.WanteObenString);
  FTrimmList.Add(RggLocalizedStrings.SalingHString);
  FTrimmList.Add(RggLocalizedStrings.SalingAString);
  FTrimmList.Add(RggLocalizedStrings.SalingLString);
end;

function TFormConfig.FillIniListsFromFile: Boolean;
begin
  result := False;
{$ifdef WantIniFile}
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

    result := True;
  end;
{$endif}
end;

procedure TFormConfig.FillIniLists;
begin
  FMastTypList.Clear;
  FQuerschnittList.Clear;
  FMaterialList.Clear;

  if not MainConst.MustBeSandboxed then
  begin
    if FillIniListsFromFile then
    begin
      Exit; { done }
    end;
  end;

  FillIniListsWithDefault;
end;

procedure TFormConfig.FillIniListsWithDefault;
var
  ML: TStrings;
begin
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
  ML.Add(RggLocalizedStrings.EA_S_Key + '=10');
  ML.Add(RggLocalizedStrings.EA_M_Key + '=100');
  ML.Add(RggLocalizedStrings.EA_L_Key + '=1000');

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
  ML.Add(RggLocalizedStrings.EA_S_Key + '=10');
  ML.Add(RggLocalizedStrings.EA_M_Key + '=100');
  ML.Add(RggLocalizedStrings.EA_L_Key + '=1000');
end;

procedure TFormConfig.LoadRiggCombos;
var
  m: TRiggPoint;
  r: Integer;
begin
  { Trimm }
  TrimmCombo.Items := FTrimmList;
  TrimmCombo.ItemIndex := Ord(fpWante);
  MinEdit.Text := IntToStr(Round(FGSB.Wante.Min));
  PosEdit.Text := IntToStr(Round(FGSB.Wante.Ist));
  MaxEdit.Text := IntToStr(Round(FGSB.Wante.Max));

  { elements }
  GetKeyList(FElementList, FTempList);
  ElementCombo.Items := FTempList;
  ElementCombo.ItemIndex := 0;
  EAEdit.Text := FElementList.Values[ElementCombo.Text];

  { mast length values }
  GetKeyList(FMastMassList, FTempList);
  MastMassCombo.Items := FTempList;
  MastMassCombo.ItemIndex := 0;
  MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Text];

  { hull coordinates }
  r := FirstRowIndex - 1;
  for m := ooA0 to ooF0 do
  begin
    Inc(r);
    Grid.Cells[1, r] := Format('%4.0f', [FiP.V[m].X]);
    Grid.Cells[2, r] := Format('%4.0f', [FiP.V[m].Y]);
    Grid.Cells[3, r] := Format('%4.0f', [FiP.V[m].Z]);
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
  EEdit.Text := FMaterialList.Values[MaterialCombo.Text];

  { Querschnitt }
  GetKeyList(FQuerschnittList, FTempList);
  QuerschnittCombo.Items := FTempList;
  QuerschnittCombo.ItemIndex := 0;
  AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Text];

  { MastTyp }
  GetKeyList(FMastTypList, FTempList);
  MastTypeCombo.Items := FTempList;
  j := 0;
  for i := 0 to FMastTypList.Count - 1 do
    if IntToStr(FiEI) = FMastTypList.Values[MastTypeCombo.Items[i]] then
      j := i;
  MastTypeCombo.ItemIndex := j;
  EIEdit.Text := FMastTypList.Values[MastTypeCombo.Text];
end;

procedure TFormConfig.LoadFromIniFile;
{$ifdef WantIniFile}
var
  IniFile: TMemIniFile;
{$endif}
begin
{$ifdef WantIniFile}
  IniFile := TMemIniFile.Create(IniFileName);
  try
    FMaterialList.Clear;
    IniFile.ReadSectionValues(RggStrings.Material_IniSectionString, FMaterialList);
    FQuerschnittList.Clear;
    IniFile.ReadSectionValues(RggStrings.Querschnitte_IniSectionString, FQuerschnittList);
    FMastTypList.Clear;
    IniFile.ReadSectionValues(RggStrings.Profile_IniSectionString, FMastTypList);
  finally
    IniFile.Free;
  end;
{$endif}
end;

procedure TFormConfig.WriteToIniFile;
begin
{$ifdef WantIniFile}
  IniMemo.Lines.SaveToFile(InifileName);
{$endif}
end;

procedure TFormConfig.LoadItemClick(Sender: TObject);
{$ifdef WantIniFile}
var
  s: string;
{$endif}
begin
{$ifdef WantIniFile}
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
    s := s + RggLocalizedStrings.MsgStr_NotFound;
    { MessageDlg(s, mtInformation, [mbOK], 0); }
  end;
{$endif}
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
  { Values loaded from ini-file did contain wrong decimal separator}
  s := StringReplace(EEdit.Text, ',', '.', []);
  a := StrToFloat(s);
  s := StringReplace(AEdit.Text, ',', '.', []);
  b := StrToFloat(s);
  c := a * b;
  EAEdit.Text := Format('%.6g', [c]);

  s := ElementCombo.Text;
  if s = '' then
    Exit;

  if s = RggLocalizedStrings.ComboTextHullRods then
  begin
    FEAarray.C0D0 := c;
    FEAarray.B0C0 := c;
    FEAarray.A0C0 := c;
    FEAarray.B0D0 := c;
    FEAarray.A0D0 := c;
    FEAarray.A0B0 := c;
  end
  else if s = RggLocalizedStrings.ComboTextWanten then
  begin
    FEAarray.B0B := c;
    FEAarray.A0A := c;
    FEAarray.BC := c;
    FEAarray.AC := c;
  end
  else if s = RggLocalizedStrings.ComboTextVorstag then
    FEAarray.C0C := c
  else if s = RggLocalizedStrings.ComboTextSpreader then
  begin
    FEAarray.BD := c;
    FEAarray.AD := c;
  end
  else if s = RggLocalizedStrings.ComboTextSpreaderConnection then
    FEAarray.AB := c;

  FElementList.Values[s] := EAEdit.Text;
end;

procedure TFormConfig.TrimmComboChange(Sender: TObject);
var
  i: TFederParam;
  f: TRggSB;
begin
  i := TFederParam(TrimmCombo.ItemIndex);
  if i = TFederParam.fpWinkel then
    LengthEditLabel.Text := RggLocalizedStrings.LabelText_WinkelInGrad
  else
    LengthEditLabel.Text := RggLocalizedStrings.LabelText_DistanceInMM;

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
  s := MastMassCombo.Text;
  if s = '' then
    Exit;

  temp := 0;
  if s = RggLocalizedStrings.MastComboTextSpreader then
    temp := FiMastSaling
  else if s = RggLocalizedStrings.MastComboTextShroud then
    temp := FiMastWante
  else if s = RggLocalizedStrings.MastComboTextTop then
    temp := FiMastTop;

  i := StrToIntDef(MastMassEdit.Text, temp);
  MastMassEdit.Text := IntToStr(i);

  FMastMassList.Values[s] := IntToStr(i);
  if s = RggLocalizedStrings.MastComboTextSpreader then
    FiMastSaling := i
  else if s = RggLocalizedStrings.MastComboTextShroud then
    FiMastWante := i
  else if s = RggLocalizedStrings.MastComboTextTop then
    FiMastTop := i;
end;

procedure TFormConfig.OKBtnClick(Sender: TObject);
begin
  Rigg.RiggPoints := FiP; { Rumpfkoordinaten }
  Rigg.MastUnten := FiMastSaling;
  Rigg.MastOben := FiMastWante - FiMastSaling;
  Rigg.MastLength := FiMastTop;
  Rigg.RggFA.Assign(FGSB); { neue Grenzen und Istwerte }
  Rigg.EA := FEAarray;
  Rigg.MastEI := FiEI;
end;

procedure TFormConfig.MastTypeComboChange(Sender: TObject);
begin
  EIEdit.Text := FMastTypList.Values[MastTypeCombo.Text];
    FiEI := StrToInt(EIEdit.Text);
end;

procedure TFormConfig.MastMassComboChange(Sender: TObject);
begin
  MastMassEdit.Text := FMastMassList.Values[MastMassCombo.Text];
end;

procedure TFormConfig.QuerschnittComboChange(Sender: TObject);
begin
  AEdit.Text := FQuerschnittList.Values[QuerschnittCombo.Text];
end;

procedure TFormConfig.MaterialComboChange(Sender: TObject);
begin
  EEdit.Text := FMaterialList.Values[MaterialCombo.Text];
end;

procedure TFormConfig.ElementComboChange(Sender: TObject);
begin
  EAEdit.Text := FElementList.Values[ElementCombo.Text];
end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    LayoutComponents;

    InitComponentProps;
    InitComponentLinks;
    InitTabOrder;

    FormShown := True;
  end;

  FillRiggLists;
  LoadRiggCombos;

  GridSelectCell(Grid, FirstColumnIndex, FirstRowIndex, FCanSelectDummy);
end;

procedure TFormConfig.GridSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  sRowHeaderText, sColHeaderText, sCellText: string;
begin
  CanSelect := True;

  if ACol = 0 then
    CanSelect := False;

  if (ACol = 2) and (ARow > SecondRowIndex) then
    CanSelect := False;

  if CanSelect then
  begin
    FRumpfCell := Point(ACol, ARow);

    sRowHeaderText := TrimLeft(Grid.Cells[0, ARow]);
    sColHeaderText := Grid.Columns[ACol].Header;
    sCellText := Grid.Cells[ACol, ARow];

    RumpfLabel.Text := Format('%s %s%s:', [RggLocalizedStrings.FieldString, sRowHeaderText, sColHeaderText]);
    if RumpfSpinEdit <> nil then
    begin
      RumpfSpinEdit.Value := StrToIntDef(sCellText, 0);
      RumpfEdit.Text := Format('%4.0f mm', [RumpfSpinEdit.Value]);
    end;
  end;
end;

procedure TFormConfig.RumpfBtnClick(Sender: TObject);
var
  oo: TRiggPoint;
  kk: Integer;
begin
  oo := TRiggPoint(FRumpfCell.Y - FirstRowIndex + Ord(ooA0));
  kk := FRumpfCell.X - FirstColumnIndex;

  FiP.V[oo].V[kk] := RumpfSpinEdit.Value;
  Grid.Cells[FRumpfCell.X, FRumpfCell.Y] := Format('%4.0f', [RumpfSpinEdit.Value]);
  if FRumpfCell.Y = SecondRowIndex then
  begin
    FiP.A0 := FiP.B0;
    FiP.A0.Y := -FiP.B0.Y;
    Grid.Cells[1, FirstRowIndex] := Format('%4.0f', [FiP.A0.X]);
    Grid.Cells[2, FirstRowIndex] := Format('%4.0f', [FiP.A0.Y]);
    Grid.Cells[3, FirstRowIndex] := Format('%4.0f', [FiP.A0.Z]);
  end;
  if FRumpfCell.Y = FirstRowIndex then
  begin
    FiP.B0 := FiP.A0;
    FiP.B0.Y := -FiP.A0.Y;
    Grid.Cells[1, SecondRowIndex] := Format('%4.0f', [FiP.B0.X]);
    Grid.Cells[2, SecondRowIndex] := Format('%4.0f', [FiP.B0.Y]);
    Grid.Cells[3, SecondRowIndex] := Format('%4.0f', [FiP.B0.Z]);
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
  OKBtn.Text := RggLocalizedStrings.OKBtnCaption;
  OKBtn.ModalResult := 1;

  CancelBtn := TButton.Create(Self);
  CancelBtn.Parent := Self;
  CancelBtn.Cancel := True;
  CancelBtn.Text := RggLocalizedStrings.CancelBtnCaption;
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
  LabelMin.Text := RggLocalizedStrings.MinLabelCaption; // 'Min';

  LabelPos := TLabel.Create(Self);
  LabelPos.Parent := gb;
  LabelPos.Text := RggLocalizedStrings.PosLabelCaption; // 'Pos';

  LabelMax := TLabel.Create(Self);
  LabelMax.Parent := gb;
  LabelMax.Text := RggLocalizedStrings.MaxLabelCaption; // 'Max';

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
  LengthEditLabel.Text := RggLocalizedStrings.LengthEditLabelCaption; // 'Abmessungen in mm';

  TrimmComboLabel := TLabel.Create(Self);
  TrimmComboLabel.Parent := gb;
  TrimmComboLabel.Text := RggLocalizedStrings.TrimmComboLabelCaption; // 'Trimmvariable';

  TrimmCombo := TRggComboBox.Create(Self);
  TrimmCombo.Parent := gb;
  ML := TrimmCombo.Items;
  ML.Add(RggLocalizedStrings.ControllerString);
  ML.Add(RggLocalizedStrings.WinkelString);
  ML.Add(RggLocalizedStrings.VorstagString);
  ML.Add(RggLocalizedStrings.WanteString);
  ML.Add(RggLocalizedStrings.WanteObenString);
  ML.Add(RggLocalizedStrings.SalingHString);
  ML.Add(RggLocalizedStrings.SalingAString);
  ML.Add(RggLocalizedStrings.SalingLString);

  { Page Fachwerk / Material }

  tsFachwerk := pc.Add(TTabItem);
  tsFachwerk.Text := RggLocalizedStrings.FachwerkPageCaption; // 'Fachwerk'
  ts := tsFachwerk;

  GroupBoxMaterial := TGroupBox.Create(Self);
  GroupBoxMaterial.Text := RggLocalizedStrings.GroupBoxMaterialCaption; // 'Material';
  gb := GroupBoxMaterial;
  gb.Parent := ts;

  ElementLabel := TLabel.Create(Self);
  ElementLabel.Parent := ts;
  ElementLabel.Text := RggLocalizedStrings.ElementLabelCaption; //'Fachwerkstäbe';

  ElementCombo := TRggComboBox.Create(Self);
  ElementCombo.Parent := ts;
  ML := ElementCombo.Items;
  ML.Add(RggLocalizedStrings.ComboTextHullRods);
  ML.Add(RggLocalizedStrings.ComboTextWanten);
  ML.Add(RggLocalizedStrings.ComboTextVorstag);
  ML.Add(RggLocalizedStrings.ComboTextSpreader);
  ML.Add(RggLocalizedStrings.ComboTextSpreaderConnection);

  EAEdit := TEdit.Create(Self);
  EAEdit.Parent := ts;
  EAEdit.Text := RggLocalizedStrings.EAEditText; // 'EAEdit';
  EAEdit.ReadOnly := True;

  EAEditLabel := TLabel.Create(Self);
  EAEditLabel.Parent := ts;
  EAEditLabel.Text := RggLocalizedStrings.EAEditLabelCaption; // 'EA in KN';

  TakeOverBtn := TButton.Create(Self);
  TakeOverBtn.Parent := ts;
  TakeOverBtn.Text := RggLocalizedStrings.TakeOverBtnCaption; //'Auswahl übernehmen';

  MaterialCombo := TRggComboBox.Create(Self);
  MaterialCombo.Parent := gb;

  MaterialComboLabel := TLabel.Create(Self);
  MaterialComboLabel.Parent := gb;
  MaterialComboLabel.Text := RggLocalizedStrings.MaterialComboLabelCaption; // 'Material';

  QuerschnittComboLabel := TLabel.Create(Self);
  QuerschnittComboLabel.Parent := gb;
  QuerschnittComboLabel.Text := RggLocalizedStrings.QuerschnittComboLabelCaption; // 'Querschnitt';

  QuerschnittCombo := TRggComboBox.Create(Self);
  QuerschnittCombo.Parent := gb;

  ALabel := TLabel.Create(Self);
  ALabel.Parent := gb;
  ALabel.Text := RggLocalizedStrings.ALabelCaption; // 'A';

  AEdit := TEdit.Create(Self);
  AEdit.Parent := gb;
  AEdit.Text := RggLocalizedStrings.AEditText; // 'AEdit';
  AEdit.ReadOnly := True;

  EEdit := TEdit.Create(Self);
  EEdit.Parent := gb;
  EEdit.Text := RggLocalizedStrings.EEditText; //'EEdit';
  EEdit.ReadOnly := True;

  ELabel := TLabel.Create(Self);
  ELabel.Parent := gb;
  ELabel.Text := RggLocalizedStrings.ELabelCaption; // 'E';

  EEditLabel := TLabel.Create(Self);
  EEditLabel.Parent := gb;
  EEditLabel.Text := RggLocalizedStrings.EEditLabelCaption; // 'E-Modul in KN/mm^2';

  AEditLabel := TLabel.Create(Self);
  AEditLabel.Parent := gb;
  AEditLabel.Text := RggLocalizedStrings.AEditLabelCaption; // 'Querschnitt in mm^2';

  { Page Mast }

  tsMast := pc.Add(TTabItem);
  tsMast.Text := RggLocalizedStrings.MastPageCaption; // 'Mast';
  ts := tsMast;

  GroupBoxMast := TGroupBox.Create(Self);
  GroupBoxMast.Text := RggLocalizedStrings.GroupBoxMastCaption; // 'Mast';
  gb := GroupBoxMast;
  gb.Parent := ts;

  MastTypeComboLabel := TLabel.Create(Self);
  MastTypeComboLabel.Parent := gb;
  MastTypeComboLabel.Text := RggLocalizedStrings.MastTypeComboLabelCaption; // 'Profil';

  MastTypeCombo := TRggComboBox.Create(Self);
  MastTypeCombo.Parent := gb;

  EIEdit := TEdit.Create(Self);
  EIEdit.Parent := gb;
  EIEdit.Text := RggLocalizedStrings.EIEditText; // 'EIEdit';
  EIEdit.ReadOnly := True;

  EILabel := TLabel.Create(Self);
  EILabel.Parent := gb;
  EILabel.Text := RggLocalizedStrings.EILabelCaption; // 'Biegesteifigkeit EI in Nm^2';

  MastMassComboLabel := TLabel.Create(Self);
  MastMassComboLabel.Parent := gb;
  MastMassComboLabel.Text := RggLocalizedStrings.MastMassComboLabelCaption; //'Abmessungen';

  MastMassCombo := TRggComboBox.Create(Self);
  MastMassCombo.Parent := gb;
  ML := MastMassCombo.Items;
  ML.Add(RggLocalizedStrings.MastComboTextController);
  ML.Add(RggLocalizedStrings.MastComboTextSpreader);
  ML.Add(RggLocalizedStrings.MastComboTextShroud);
  ML.Add(RggLocalizedStrings.MastComboTextTop);

  MastMassEdit := TEdit.Create(Self);
  MastMassEdit.Parent := gb;

  MassMassEditLabel := TLabel.Create(Self);
  MassMassEditLabel.Parent := gb;
  MassMassEditLabel.Text := RggLocalizedStrings.MassMassEditLabelCaption; // 'Abstand vom Mastfuß in mm';

  { Page Rumpf }

  tsRumpf := pc.Add(TTabItem);
  tsRumpf.Text := RggLocalizedStrings.HullPageCaption; // 'Rumpf';
  ts := tsRumpf;

  GroupBoxRumpf := TGroupBox.Create(Self);
  GroupBoxRumpf.Text := RggLocalizedStrings.GroupBoxHullCaption; // 'Feld Editieren';
  gb := GroupBoxRumpf;
  gb.Parent := ts;

  RumpfLabel := TLabel.Create(Self);
  RumpfLabel.Parent := gb;
  RumpfLabel.WordWrap := False;
  RumpfLabel.AutoSize := True;

  RumpfEdit := TEdit.Create(Self);
  RumpfEdit.Parent := gb;
  RumpfEdit.Text := '10';
  RumpfEdit.ReadOnly := True;

  RumpfBtn := TButton.Create(Self);
  RumpfBtn.Parent := gb;
  RumpfBtn.Text := RggLocalizedStrings.RumpfBtnCaption; // 'Übernehmen';

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
  tsIniMemo.Text := RggLocalizedStrings.IniMemoPageCaption; // 'Rigg.ini';
  ts := tsIniMemo;

  IniMemo := TMemo.Create(Self);
  IniMemo.Parent := ts;
  IniMemo.ShowScrollBars := True;

  SaveIniBtn := TButton.Create(Self);
  SaveIniBtn.Parent := ts;
  SaveIniBtn.Text := RggLocalizedStrings.SaveIniBtnCaption; // 'Speichern';
  SaveIniBtn.Enabled := IniFileName <> '';

  LoadIniBtn := TButton.Create(Self);
  LoadIniBtn.Parent := ts;
  LoadIniBtn.Text := RggLocalizedStrings.LoadIniBtnCaption; // 'Laden';
  LoadIniBtn.Enabled := IniFileName <> '';
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

{ TRggComboBox }

function TRggComboBox.GetText: string;
begin
  if Selected <> nil then
    result := Selected.Text
  else
    result := '';
end;

procedure TFormConfig.InitComponentLinks;
begin
  { Trimm }
  MinEdit.OnExit := MinEditExit;
  MinEdit.OnKeyDown := MinEditKeyDown;
  PosEdit.OnExit := MinEditExit;
  PosEdit.OnKeyDown := MinEditKeyDown;
  MaxEdit.OnExit := MinEditExit;
  MaxEdit.OnKeyDown := MinEditKeyDown;

  TrimmCombo.OnChange := TrimmComboChange;

  { Fachwerk }
  ElementCombo.OnChange := ElementComboChange;
  TakeOverBtn.OnClick := TakeOverBtnClick;
  MaterialCombo.OnChange := MaterialComboChange;
  QuerschnittCombo.OnChange := QuerschnittComboChange;

  { Mast }
  MastMassCombo.OnChange := MastMassComboChange;
  MastTypeCombo.OnChange := MastTypeComboChange;

  MastMassEdit.OnExit := MastMassEditExit;
  MastMassEdit.OnKeyDown := MastMassEditKeyDown;

  { Hull }
  RumpfSpinEdit.OnChange := RumpfSpinEditChanging;
  RumpfBtn.OnClick := RumpfBtnClick;

  { IniMemo }
  SaveIniBtn.OnClick := StoreItemClick;
  LoadIniBtn.OnClick := LoadItemClick;

  { Buttons }
  OKBtn.OnClick := OKBtnClick;
end;

procedure TFormConfig.InitComponentSize;
var
  w, h: single;
  gbw: single;
begin
  PageControl.Width := Scale(520);
  PageControl.Height := Scale(270);

  gbw := PageControl.Width - 2 * Margin;

  { Trimm }
  GroupBoxTrimm.Width := gbw;
  GroupBoxTrimm.Height := Scale(170);

  LabelMin.Width := Scale(21);
  LabelPos.Width := Scale(24);
  LabelMax.Width := Scale(25);

  w := Scale(40);
  MinEdit.Width := w;
  PosEdit.Width := w;
  MaxEdit.Width := w;

  LengthEditLabel.Width := Scale(125);
  TrimmComboLabel.Width := Scale(87);
  TrimmCombo.Width := Scale(150);

  { Fachwerk }
  ElementLabel.Width := Scale(92);
  ElementCombo.Width := Scale(161);
  EAEdit.Width := Scale(105);
  EAEditLabel.Width := Scale(52);
  TakeOverBtn.Width := Scale(170);
  TakeOverBtn.Height := Scale(25);

  GroupBoxMaterial.Width := gbw;
  GroupBoxMaterial.Height := Scale(150);

  MaterialComboLabel.Width := Scale(48);
  MaterialCombo.Width := Scale(153);
  ELabel.Width := Scale(9);
  EEdit.Width := Scale(73);
  EEditLabel.Width := Scale(124);
  QuerschnittComboLabel.Width := Scale(66);
  QuerschnittCombo.Width := Scale(153);
  ALabel.Width := Scale(9);
  AEdit.Width := Scale(73);
  AEditLabel.Width := Scale(118);

  { Mast }
  GroupBoxMast.Width := gbw;
  GroupBoxMast.Height := Scale(200);

  MastTypeComboLabel.Width := Scale(30);
  MastTypeCombo.Width := Scale(145);
  EIEdit.Width := Scale(73);
  EILabel.Width := Scale(158);
  MastMassComboLabel.Width := Scale(87);
  MastMassCombo.Width := Scale(145);
  MastMassEdit.Width := Scale(73);
  MassMassEditLabel.Width := Scale(167);

  { Hull }
  GroupBoxRumpf.Width := Scale(170);
  GroupBoxRumpf.Height := Scale(145);

  RumpfLabel.Width := Scale(73);
  RumpfLabel.Height := Scale(16);
  RumpfEdit.Width := Scale(81);
  RumpfEdit.Height := Scale(24);
  RumpfBtn.Width := Scale(129);
  RumpfBtn.Height := Scale(25);

  { IniMemo }
  IniMemo.Width := Scale(400);
  IniMemo.Height := 50;
  SaveIniBtn.Width := Scale(81);
  SaveIniBtn.Height := Scale(25);
  LoadIniBtn.Width := Scale(81);
  LoadIniBtn.Height := Scale(25);

  { Buttons }
  h := Scale(27);
  OKBtn.Width := Scale(81);
  OKBtn.Height := h;
  CancelBtn.Width := Scale(101);
  CancelBtn.Height := h;
end;

procedure TFormConfig.InitComponentProps;
begin
  MinEdit.MaxLength := 4;
  PosEdit.MaxLength := 4;
  MaxEdit.MaxLength := 4;
  MastMassEdit.MaxLength := 4;
end;

procedure TFormConfig.InitTabOrder;
begin
  PageControl.TabOrder := 0;

  { Trimm }
  GroupBoxTrimm.TabOrder := 0;
  MinEdit.TabOrder := 1;
  PosEdit.TabOrder := 2;
  MaxEdit.TabOrder := 3;
  TrimmCombo.TabOrder := 4;

  { Fachwerk }
  ElementCombo.TabOrder := 0;
  EAEdit.TabOrder := 1;
  TakeOverBtn.TabOrder := 2;
  GroupBoxMaterial.TabOrder := 3;
  MaterialCombo.TabOrder := 0;
  EEdit.TabOrder := 3;
  QuerschnittCombo.TabOrder := 1;
  AEdit.TabOrder := 2;

  AEdit.TabStop := False;
  EEdit.TabStop := False;
  EAEdit.TabStop := False;

  { Mast }
  GroupBoxMast.TabOrder := 0;
  MastTypeCombo.TabOrder := 2;
  EIEdit.TabOrder := 3;
  MastMassCombo.TabOrder := 0;
  MastMassEdit.TabOrder := 1;
  EIEdit.TabStop := False;

  { Hull }
  RumpfEdit.TabOrder := 1;
  RumpfSpinEdit.TabOrder := 2;
  RumpfSpinEdit.TabStop := True;
  RumpfBtn.TabOrder := 0;

  { IniMemo }
  IniMemo.TabOrder := 0;
  SaveIniBtn.TabOrder := 1;
  LoadIniBtn.TabOrder := 2;

  { Buttons }
  OKBtn.TabOrder := 1;
  CancelBtn.TabOrder := 2;
end;

procedure TFormConfig.LayoutComponents;
begin
  Left := Scale(230);
  Top := Scale(100);

  PageControl.Position.X := Margin;
  PageControl.Position.Y := Margin;

  { Trimm }

  GroupBoxTrimm.Position.X := Margin;
  GroupBoxTrimm.Position.Y := 2 * Margin;

  LabelMin.Position.X := 4 * Margin;
  LabelMin.Position.Y := 3 * Margin;

  cr := LabelMin;
  StackH(LabelPos);
  StackH(LabelMax);

  cr := LabelMin;
  StackV(MinEdit);
  StackH(PosEdit);
  StackH(MaxEdit);

  LabelPos.Position.X := PosEdit.Position.X;
  LabelMax.Position.X := MaxEdit.Position.X;

  cr := MaxEdit;
  StackH(LengthEditLabel);

  cr := MinEdit;
  StackV(TrimmCombo);
  StackH(TrimmComboLabel);

  { Fachwerk }

  ElementLabel.Position.X := Margin;
  ElementLabel.Position.Y := 2 * Margin;
  cr := ElementLabel;
  StackV(ElementCombo);
  StackH(EAEdit);
  StackH(TakeOverBtn);
  cr := EAEdit;
  StackV(EAEditLabel);
  EAEditLabel.Position.Y := ElementLabel.Position.Y;

  cr := ElementCombo;
  StackV(GroupBoxMaterial);

  MaterialComboLabel.Position.X := 3 * Margin;
  MaterialComboLabel.Position.Y := 2 * Margin;

  cr := MaterialComboLabel;
  StackV(MaterialCombo);
  StackH(EEdit);
  StackH(EEditLabel);
  cr := EEdit;
  StackV(ELabel);
  ELabel.Position.Y := MaterialComboLabel.Position.Y;

  cr := MaterialCombo;
  StackV(QuerschnittComboLabel);
  StackV(QuerschnittCombo);
  StackH(AEdit);
  StackH(AEditLabel);
  cr := AEdit;
  StackV(ALabel);
  ALabel.Position.Y := QuerschnittComboLabel.Position.Y;

  { Mast }

  GroupBoxMast.Position.X := Margin;
  GroupBoxMast.Position.Y := 2 * Margin;

  MastTypeComboLabel.Position.X := 3 * Margin;
  MastTypeComboLabel.Position.Y := 3 * Margin;

  cr := MastTypeComboLabel;
  StackV(MastTypeCombo);
  StackH(EIEdit);
  StackH(EILabel);

  cr := MastTypeCombo;
  StackV(MastMassComboLabel);
  StackV(MastMassCombo);
  StackH(MastMassEdit);
  StackH(MassMassEditLabel);

  { Hull }

  cr := Grid;
  StackH(GroupBoxRumpf);

  RumpfLabel.Position.X := 2 * Margin;
  RumpfLabel.Position.Y := 2 * Margin;

  cr := RumpfLabel;
  StackV(RumpfEdit);
  StackV(RumpfBtn);
  StackV(RumpfSpinEdit);

  { Ini Memo }

  IniMemo.Position.X := 0;
  IniMemo.Position.Y := 0;
  IniMemo.Align := TAlignLayout.Left;

  cr := IniMemo;
  StackH(SaveIniBtn);
  SaveIniBtn.Position.Y := SaveIniBtn.Position.Y + 30;
  StackV(LoadIniBtn);

  { Buttons }

  cr := PageControl;
  StackV(OKBtn);
  OKBtn.Position.X := Scale(169);

  cr := OKBtn;
  StackH(CancelBtn);

  { Form }

  ClientWidth := Round(PageControl.Width + 2 * Margin);
  ClientHeight := Round(OKBtn.Position.Y + OKBtn.Height + Margin);
end;

procedure TFormConfig.RecordMax;
begin
  TempR := cr.Position.X + cr.Width;
  if TempR > FMaxRight then
    FMaxRight := TempR;

  TempB := cr.Position.Y + cr.Height;
  if TempB > FMaxBottom then
    FMaxBottom := TempB;
end;

procedure TFormConfig.StackH(c: TControl);
begin
  c.Position.X := cr.Position.X + cr.Width + Margin;
  c.Position.Y := cr.Position.Y;
  cr := c;
  RecordMax;
end;

procedure TFormConfig.StackV(c: TControl);
begin
  c.Position.X := cr.Position.X;
  c.Position.Y := cr.Position.Y + cr.Height + Margin;
  cr := c;
  RecordMax;
end;

procedure TFormConfig.AnchorVertical(c: TControl);
begin
  c.Height := ClientHeight - c.Position.Y - Margin;
  c.Anchors := c.Anchors + [TAnchorKind.akBottom];
end;

function TFormConfig.Scale(Value: Integer): Integer;
begin
  result := Round(Value * FScale);
end;

end.
