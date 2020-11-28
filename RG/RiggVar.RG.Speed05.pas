unit RiggVar.RG.Speed05;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  FMX.StdCtrls;

type
  TActionSpeedBarRG05 = class(TActionSpeedBar)
  private
    ColorModeBtn: TSpeedButton;
    FontSizeBtn: TSpeedButton;

    MemoryBtn: TSpeedButton;
    MemoryRecallBtn: TSpeedButton;

    BogenBtn: TSpeedButton;
    HullBtn: TSpeedButton;

    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;

    SeiteBtn: TSpeedButton;
    TopBtn: TSpeedButton;
    AchternBtn: TSpeedButton;
    NullBtn: TSpeedButton;
  public
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst;

{ TActionSpeedBarRG05 }

procedure TActionSpeedBarRG05.InitSpeedButtons;
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

  { Bogen and Hull }

  BtnColorValue := clvBogen;

  sb := AddSpeedBtn('BogenBtn', BtnGroupSpace);
  BogenBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faRggBogen;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('HullBtn', 0);
  HullBtn := sb;
  sb.StaysPressed := True;
  sb.Tag := faRggHull;
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

end.
