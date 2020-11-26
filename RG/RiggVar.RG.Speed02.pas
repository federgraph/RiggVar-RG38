unit RiggVar.RG.Speed02;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  System.UIConsts,
  System.Classes,
  FMX.StdCtrls;

{.$define SegmentButtons}

type
  TActionSpeedBarRG02 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;
    UseDisplayListBtn: TSpeedButton;
    UseQuickSortBtn: TSpeedButton;
    LegendBtn: TSpeedButton;
    LineColorBtn: TSpeedButton;
{$ifdef SegmentButtons}
    FixpunktBtn: TSpeedButton;
    RumpfBtn: TSpeedButton;
    SalingBtn: TSpeedButton;
    WanteBtn: TSpeedButton;
    MastBtn: TSpeedButton;
    VorstagBtn: TSpeedButton;
    ControllerBtn: TSpeedButton;
    AchsenBtn: TSpeedButton;
{$endif}
    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    BogenBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    SofortBtn: TSpeedButton;
    GrauBtn: TSpeedButton;
    BlauBtn: TSpeedButton;
    MultiBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;

    MatrixBtn: TSpeedButton;
  private
    procedure ToggleColorModeBtnClick(Sender: TObject);
    procedure ToggleFontSizeBtnClick(Sender: TObject);
  protected
    procedure SpeedButtonClick(Sender: TObject); override;
  public
    procedure InitSpeedButtons; override;
    procedure UpdateSpeedButtonDown; override;
    procedure UpdateSpeedButtonEnabled; override;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RggTypes,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG02 }

procedure TActionSpeedBarRG02.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;

  Main.ActionHandler.Execute(fa);

  case fa of
    faToggleUseDisplayList,
    faMultiBtn: UpdateSpeedButtonEnabled;
  end;

  Exit;

  case fa of
    faToggleUseDisplayList:
    begin
      FormMain.RotaForm.UseDisplayListBtnClick(Sender);
      UpdateSpeedButtonEnabled;
    end;

    faToggleUseQuickSort: FormMain.RotaForm.UseQuickSortBtnClick(Sender);
    faToggleLineColor: FormMain.LineColorBtnClick(Sender);
    faToggleShowLegend: FormMain.RotaForm.LegendBtnClick(Sender);

    faToggleSegmentF .. faToggleSegmentA: FormMain.HandleSegment(fa);

    faViewpointS: FormMain.SeiteBtnClick(Sender);
    faViewpointA: FormMain.AchternBtnClick(Sender);
    faViewpointT: FormMain.TopBtnClick(Sender);
    faViewpoint3: FormMain.NullBtnClick(Sender);

    faRggZoomIn: FormMain.RotaForm.ZoomInBtnClick(Sender);
    faRggZoomOut: FormMain.RotaForm.ZoomOutBtnClick(Sender);

    faRggBogen: FormMain.BogenBtnClick(Sender);
    faRggKoppel: FormMain.KoppelBtnClick(Sender);

    faToggleMatrixText: FormMain.RotaForm.MatrixItemClick(Sender);

    faMemoryBtn: FormMain.MemoryBtnClick(Sender);
    faMemoryRecallBtn: FormMain.MemoryRecallBtnClick(Sender);

    faSofortBtn: FormMain.SofortBtnClick(Sender);
    faGrauBtn: FormMain.GrauBtnClick(Sender);
    faBlauBtn: FormMain.BlauBtnClick(Sender);
    faMultiBtn:
    begin
      FormMain.MultiBtnClick(Sender);
      UpdateSpeedButtonEnabled;
    end;
  end;
end;

procedure TActionSpeedBarRG02.UpdateSpeedButtonDown;
begin
  UseDisplayListBtn.IsPressed := FormMain.RotaForm.UseDisplayList;

  UseQuickSortBtn.IsPressed := FormMain.RotaForm.UseQuickSort;
  LegendBtn.IsPressed := FormMain.RotaForm.LegendItemChecked;
  LineColorBtn.IsPressed := Main.GetChecked(faToggleLineColor);

{$ifdef SegmentButtons}
  FixpunktBtn.IsPressed := Main.GetChecked(faToggleSegmentF);
  RumpfBtn.IsPressed := Main.GetChecked(faToggleSegmentR);
  SalingBtn.IsPressed := Main.GetChecked(faToggleSegmentS);
  WanteBtn.IsPressed := Main.GetChecked(faToggleSegmentW);
  MastBtn.IsPressed := Main.GetChecked(faToggleSegmentM);
  VorstagBtn.IsPressed := Main.GetChecked(faToggleSegmentV);
  ControllerBtn.IsPressed := Main.GetChecked(faToggleSegmentC);
  AchsenBtn.IsPressed := Main.GetChecked(faToggleSegmentA);
{$endif}

  SeiteBtn.IsPressed := False;
  TopBtn.IsPressed := False;
  AchternBtn.IsPressed := False;
  NullBtn.IsPressed := False;

  ZoomInBtn.IsPressed := False;
  ZoomOutBtn.IsPressed := False;

  BogenBtn.IsPressed := Main.GetChecked(faRggBogen);
  KoppelBtn.IsPressed := Main.GetChecked(faRggKoppel);

  MatrixBtn.IsPressed := FormMain.RotaForm.MatrixItemChecked;

  MemoryBtn.IsPressed := False;
  MemoryRecallBtn.IsPressed := False;

  SofortBtn.IsPressed := Main.GetChecked(faSofortBtn);
  MultiBtn.IsPressed := Main.GetChecked(faMultiBtn);
  GrauBtn.IsPressed := Main.GetChecked(faGrauBtn);
  BlauBtn.IsPressed := Main.GetChecked(faBlauBtn);
end;

procedure TActionSpeedBarRG02.UpdateSpeedButtonEnabled;
var
  b1, b2: Boolean;
  b: Boolean;
begin
  b1 := FormMain.RotaForm.UseDisplayList;
  b2 := FormMain.RotaForm.WantOverlayedRiggs;

  b := b1;

  UseQuickSortBtn.Enabled := b;
  LegendBtn.Enabled := b;
  LineColorBtn.Enabled := b;

  b := not b1;

  MultiBtn.Enabled := b;
  KoppelBtn.Enabled := b;

  b := (not b1) and b2;

  GrauBtn.Enabled := b;
  BlauBtn.Enabled := b;
end;

procedure TActionSpeedBarRG02.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Special Buttons }

  BtnColorValue := clvScheme;

  sb := AddSpeedBtn('FontSizeBtn', BtnGroupSpace);
  FontSizeBtn := sb;
  sb.Text := 'FS';
  sb.Hint := 'Toggle FontSize';
  sb.OnClick := ToggleFontSizeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ColorModeBtn');
  ColorModeBtn := sb;
  sb.Text := 'CM';
  sb.Hint := 'Toggle ColorMode';
  sb.OnClick := ToggleColorModeBtnClick;
  sb.Tag := faNoop;
  InitSpeedButton(sb);

  { DisplayList Graph Toggle }

  BtnColorValue := clvGraph;

  sb := AddSpeedBtn('UseDisplayListBtn', BtnGroupSpace);
  UseDisplayListBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleUseDisplayList;
  InitSpeedButton(sb);

  { DisplayList Graph Options }

  BtnColorValue := clvOption;

  sb := AddSpeedBtn('UseQuickSortBtn', BtnGroupSpace);
  UseQuickSortBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleUseQuickSort;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LegendBtn', 0);
  LegendBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleShowLegend;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LineColorBtn');
  LineColorBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleLineColor;
  InitSpeedButton(sb);

{$ifdef SegmentButtons}

  { DisplayList Graph Segments }

  BtnColorValue := clvSegment;

  sb := AddSpeedBtn('FixpunktBtn', BtnGroupSpace);
  FixpunktBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentF;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('RumpftBtn', 0);
  RumpfBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentR;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SalingBtn', 0);
  SalingBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentS;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MastBtn', 0);
  MastBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentM;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('VorstagBtn', 0);
  VorstagBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentV;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('WanteBtn', 0);
  WanteBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentW;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ControllerBtn', 0);
  ControllerBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentC;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('AchsenBtn', 0);
  AchsenBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleSegmentA;
  InitSpeedButton(sb);
{$endif}

  { Bogen and Koppel }

  BtnColorValue := clvBogen;

  sb := AddSpeedBtn('BogenBtn', BtnGroupSpace);
  BogenBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faRggBogen;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('KoppelBtn', 0);
  KoppelBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faRggKoppel;
  InitSpeedButton(sb);

  { Image Elements, and Matrix Text }

  BtnColorValue := clvImage;

  sb := AddSpeedBtn('MatrixBtn', 0);
  MatrixBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleMatrixText;
  InitSpeedButton(sb);

  { Memory Buttons }

  BtnColorValue := clvMemory;

  sb := AddSpeedBtn('MemoryBtn', BtnGroupSpace);
  MemoryBtn := sb;
  sb.Tag := faMemoryBtn;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MemoryRecallBtn', 0);
  MemoryRecallBtn := sb;
  sb.Tag := faMemoryRecallBtn;
  InitSpeedButton(sb);

  { Rigg Buttons }

  BtnColorValue := clvRigg;

  sb := AddSpeedBtn('SofortBtn', BtnGroupSpace);
  SofortBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSofortBtn;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('GrauBtn', 0);
  GrauBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faGrauBtn;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('BlauBtn', 0);
  BlauBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faBlauBtn;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MultiBtn', 0);
  MultiBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faMultiBtn;
  InitSpeedButton(sb);

  { Zoom Buttons }

  BtnColorValue := clvZoom;

  sb := AddSpeedBtn('ZoomOutBtn', BtnGroupSpace);
  ZoomOutBtn := sb;
  sb.Tag := faRggZoomOut;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ZoomInBtn', 0);
  ZoomInBtn := sb;
  sb.Tag := faRggZoomIn;
  InitSpeedButton(sb);

  { ViewPoint Buttons }

  BtnColorValue := clvView;

  sb := AddSpeedBtn('SeiteBtn', BtnGroupSpace);
  SeiteBtn := sb;
  sb.Tag := faViewpointS;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('AchternBtn', 0);
  AchternBtn := sb;
  sb.Tag := faViewpointA;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('TopBtn', 0);
  TopBtn := sb;
  sb.Tag := faViewpointT;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('NullBtn', 0);
  NullBtn := sb;
  sb.Tag := faViewpoint3;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarRG02.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBarRG02.ToggleFontSizeBtnClick(Sender: TObject);
begin
  FormMain.ToggleSpeedPanelFontSize;
end;

end.
