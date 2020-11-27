unit RiggVar.RG.Speed04;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  System.UIConsts,
  System.Classes,
  FMX.StdCtrls;

type
  TActionSpeedBarRG04 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    KoppelBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;
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
  RiggVar.App.Main,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG04 }

procedure TActionSpeedBarRG04.SpeedButtonClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;
  Main.ActionHandler.Execute(fa);
end;

procedure TActionSpeedBarRG04.UpdateSpeedButtonDown;
begin
  SeiteBtn.IsPressed := False;
  TopBtn.IsPressed := False;
  AchternBtn.IsPressed := False;
  NullBtn.IsPressed := False;

  ZoomInBtn.IsPressed := False;
  ZoomOutBtn.IsPressed := False;

  BogenBtn.IsPressed := Main.GetChecked(faRggBogen);
  KoppelBtn.IsPressed := Main.GetChecked(faRggKoppel);

  MemoryBtn.IsPressed := False;
  MemoryRecallBtn.IsPressed := False;
end;

procedure TActionSpeedBarRG04.UpdateSpeedButtonEnabled;
begin
end;

procedure TActionSpeedBarRG04.InitSpeedButtons;
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

procedure TActionSpeedBarRG04.ToggleColorModeBtnClick(Sender: TObject);
begin
  Main.ToggleDarkMode;
end;

procedure TActionSpeedBarRG04.ToggleFontSizeBtnClick(Sender: TObject);
begin
  Main.ToggleSpeedPanelFontSize;
end;

end.
