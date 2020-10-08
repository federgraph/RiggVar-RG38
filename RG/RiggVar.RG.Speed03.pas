unit RiggVar.RG.Speed03;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  System.UIConsts,
  System.Classes,
  FMX.StdCtrls;

type
  TActionSpeedBarRG03 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;

    SimpleBtn: TSpeedButton;
    NormalBtn: TSpeedButton;
    GrauBtn: TSpeedButton;
    BlauBtn: TSpeedButton;
    MultiBtn: TSpeedButton;
    DisplayBtn: TSpeedButton;
    QuickBtn: TSpeedButton;

    LegendBtn: TSpeedButton;
    LineColorBtn: TSpeedButton;

  private
    procedure ToggleColorModeBtnClick(Sender: TObject);
    procedure ToggleFontSizeBtnClick(Sender: TObject);
  protected
    procedure SpeedButtonClick(Sender: TObject); override;
  public
    procedure InitSpeedButtons; override;
    procedure UpdateSpeedButtonDown; override;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RggTypes,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG03 }

procedure TActionSpeedBarRG03.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;
  case fa of
    faMemoryBtn: FormMain.MemoryBtnClick(Sender);
    faMemoryRecallBtn: FormMain.MemoryRecallBtnClick(Sender);

    faRggBogen: FormMain.BogenBtnClick(Sender);
    faRggKoppel: FormMain.KoppelBtnClick(Sender);

    faSuperSimple: FormMain.SuperSimpleBtnClick(Sender);
    faSuperNormal: FormMain.SuperNormalBtnClick(Sender);
    faSuperGrau: FormMain.SuperGrauBtnClick(Sender);
    faSuperBlau: FormMain.SuperBlauBtnClick(Sender);
    faSuperMulti: FormMain.SuperMultiBtnClick(Sender);
    faSuperDisplay: FormMain.SuperDisplayBtnClick(Sender);
    faSuperQuick: FormMain.SuperQuickBtnClick(Sender);

    faToggleLineColor: FormMain.LineColorBtnClick(Sender);
    faToggleShowLegend: FormMain.RotaForm.LegendBtnClick(Sender);
  end;

  UpdateSpeedButtonDown;
end;

procedure TActionSpeedBarRG03.UpdateSpeedButtonDown;
begin
  MemoryBtn.IsPressed := False;
  MemoryRecallBtn.IsPressed := False;

  BogenBtn.IsPressed := Main.GetChecked(faRggBogen);
  KoppelBtn.IsPressed := Main.GetChecked(faRggKoppel);

  SimpleBtn.IsPressed := Main.GetChecked(faSuperSimple);
  NormalBtn.IsPressed := Main.GetChecked(faSuperNormal);
  GrauBtn.IsPressed := Main.GetChecked(faSuperGrau);
  BlauBtn.IsPressed := Main.GetChecked(faSuperBlau);
  MultiBtn.IsPressed := Main.GetChecked(faSuperMulti);
  DisplayBtn.IsPressed := Main.GetChecked(faSuperDisplay);
  QuickBtn.IsPressed := Main.GetChecked(faSuperQuick);

  LegendBtn.IsPressed := Main.GetChecked(faToggleShowLegend);
  LineColorBtn.IsPressed := Main.GetChecked(faToggleLineColor);
end;

procedure TActionSpeedBarRG03.InitSpeedButtons;
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

  { Rigg Buttons }

  BtnColorValue := clvRigg;

  sb := AddSpeedBtn('SimpleBtn', BtnGroupSpace);
  SimpleBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperSimple;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('NormalBtn', 0);
  NormalBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperNormal;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('GrauBtn', 0);
  GrauBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperGrau;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('BlauBtn', 0);
  BlauBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperBlau;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('MultiBtn', 0);
  MultiBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperMulti;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('DisplayBtn', 0);
  DisplayBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperDisplay;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('QuickBtn', 0);
  QuickBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faSuperQuick;
  InitSpeedButton(sb);

  BtnColorValue := clvGraph;

  sb := AddSpeedBtn('LegendBtn', BtnGroupSpace);
  LegendBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleShowLegend;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LineColorBtn', 0);
  LineColorBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faToggleLineColor;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarRG03.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBarRG03.ToggleFontSizeBtnClick(Sender: TObject);
begin
  ToggleBigMode;
  FormMain.LayoutComponents;
  FormMain.CheckSpaceForMemo;
  FormMain.CheckSpaceForImages;
end;

end.
