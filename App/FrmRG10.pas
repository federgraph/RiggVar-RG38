unit FrmRG10;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

{$define WantMenu}

uses
  RiggVar.App.Model,
  RiggVar.App.Strings,
  RiggVar.RG.View,
  RiggVar.FB.Color,
  RiggVar.FB.SpeedColor,
  RiggVar.FB.SpeedBar,
  RiggVar.RG.Report,
  RiggVar.RG.Rota,
  RiggVar.FD.Image,
{$ifdef WantMenu}
  RiggVar.FederModel.Menu,
{$endif}
  RiggVar.RG.Types,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Objects,
{$ifdef WantMenu}
  FMX.Menus,
{$endif}
  FMX.Dialogs,
  FMX.Controls.Presentation;

type
  TFormMain10 = class(TForm, IFormMain)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    FScale: single;
    FormShown: Boolean;
    FShowDataText: Boolean;
    FShowTrimmText: Boolean;
    FShowDiffText: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure FormCreate2(Sender: TObject);
    procedure FormDestroy2(Sender: TObject);
    procedure ToggleLanguage;
  public
    RL: TStrings;
    procedure ShowTrimm;
    procedure UpdateReport;
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    procedure UpdateItemIndexParams;
    procedure UpdateItemIndexReports;
    procedure UpdateItemIndexTrimms;
  public
    function GetShowDataText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowTrimmText: Boolean;
    procedure SetShowDataText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowTrimmText(const Value: Boolean);
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  public
    ComponentsCreated: Boolean;
    procedure CreateComponents;
  public
    Raster: Integer;
    Margin: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    SpeedPanel03: TActionSpeedBar;
    SpeedColorScheme: TSpeedColorScheme;
    procedure InitSpeedButtons;
    procedure LayoutSpeedPanel(SP: TActionSpeedBar);
    procedure ToggleButtonSize;
    procedure SwapSpeedPanel(Value: Integer);
{$ifdef WantMenu}
  public
    MainMenu: TMainMenu;
    MenuItem: TMenuItem;
    FederMenu: TFederMenu;
    function GetMainMenuVisible: Boolean;
    procedure SetMainMenuVisible(const Value: Boolean);
    procedure PopulateMenu;
    property MainMenuVisible: Boolean read GetMainMenuVisible write SetMainMenuVisible;
{$endif}
  public
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    function GetChecked(fa: Integer): Boolean;
    procedure HandleAction(fa: Integer);
    procedure RotaFormRotateZ(Delta: single);
    procedure RotaFormZoom(Delta: single);
  public
    RotaForm: TRotaForm;
  public
    Rigg: IRigg;
    ReportManager: TRggReportManager;
    procedure UpdateOnParamValueChanged;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    property IsUp: Boolean read GetIsUp write SetIsUp;
  public
    BitmapWidth: Integer;
    BitmapHeight: Integer;
    Image: TOriginalImage;
    ImagePositionX: single;
    ImagePositionY: single;
    TextPositionX: single;
    TextPositionY: single;
    procedure UpdateFederText;
    procedure CenterRotaForm;
  protected
    procedure MemoBtnClick(Sender: TObject);
    procedure ActionsBtnClick(Sender: TObject);
  end;

var
  FormMain10: TFormMain10;

implementation

{$R *.fmx}

uses
  FrmMemo,
  FrmAction,
  RiggVar.RG.Speed03,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

const
  HelpCaptionText = 'RG29 - press ? for help';
  ApplicationTitleText = 'RG29';

{ TFormMain }

procedure TFormMain10.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain10.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';

  MainVar.WantFederText := True;
  MainVar.ClientWidth := Round(ClientWidth);
  MainVar.ClientHeight := Round(ClientHeight);

  FormMain10 := self;

  SpeedColorScheme := TSpeedColorScheme.Create;
  SpeedColorScheme.InitDark;
  TActionSpeedBar.SpeedColorScheme := SpeedColorScheme;

  FormCreate2(Sender);
end;

procedure TFormMain10.FormDestroy(Sender: TObject);
begin
  MainVar.AppIsClosing := True;

  FormDestroy2(Sender);

  SpeedColorScheme.Free;
{$ifdef WantMenu}
  FederMenu.Free;
{$endif}

  TModelFactory.ReleaseIfAppropriate(Rigg);
end;

procedure TFormMain10.FormCreate2(Sender: TObject);
begin
{$if defined(Debug) and not defined(WantRotaForm3) }
  ReportMemoryLeaksOnShutdown := True; // there is a small systematic Memory leak when using RotaForm3
{$endif}

  FScale := 1.0;
{$ifdef MSWINDOWS}
  { On MACOS Screen.WorkAreaHeight is not scaled,
    so it would be wrong to div by scale.

    On Windows Screen.WorkAreaHeight is scaled and should be divved. }
  FScale := Handle.Scale;
{$endif}

  Application.OnException := ApplicationEventsException;


  Margin := 2;
  Raster := MainVar.Raster;
  MainVar.Scale := FScale;
  MainVar.ScaledRaster := Raster;

  SpeedPanelHeight := Raster;

  BitmapWidth := Screen.Width;
  BitmapHeight := Screen.Height;

{$ifdef WantMenu}
  FederMenu := TFederMenu.Create;
  { Create MainMenu before setting ClientHeight ? }
  MainMenu := TMainMenu.Create(self);
  MainMenu.Parent := self;
  MenuItem := TMenuItem.Create(self);
  MainMenu.AddObject(MenuItem);
{$endif}

  CreateComponents;

  Rigg := TModelFactory.NewRigg;
  Rigg.ControllerTyp := ctOhne;

  Main := TMain.Create(Rigg, Self, Self);
  Main.Logger.Verbose := True;
  Main.IsUp := True;

  RotaForm := TRotaForm.Create;
  RotaForm.Image := Image;
  RotaForm.Init;
  RotaForm.SwapRota(1);

  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  ReportManager.CurrentReport := rgDiffText;

  { Background }
  Fill.Kind := TBrushKind.Solid; // because it is still TBrushKind.None
  Fill.Color := TRggColors.BackgroundGray;

  Caption := HelpCaptionText;

  Main.Draw;
  Main.MemoryBtnClick;

//  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateColorScheme;

  SwapSpeedPanel(RotaForm.Current);

  Main.InitDefaultData;
  CenterRotaForm;
  Main.FixPoint := ooD0;
  Main.HullVisible := False;
  Main.FederTextCheckState;

  Self.OnMouseWheel := FormMouseWheel;
  Self.OnResize := FormResize;
  Self.OnKeyUp := FormKeyUp;

{$if defined(MACOS) and (CompilerVersion < 34.0) }
  { OnKeyUp does not work well on Mac, RSP-2766 }
  { fixed in Sidney 10.4 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead, in Tokyo 10.2 and Rio 10.3 }
  OnKeyDown := FormKeyUp;
{$endif}

{$ifdef WantMenu}
  PopulateMenu;
{$endif}
end;

procedure TFormMain10.FormDestroy2(Sender: TObject);
begin
  RL.Free;
  ReportManager.Free;
  Main.Free;
  Main := nil;

  Image.Free;

  RotaForm.Free;
end;

procedure TFormMain10.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  fa: Integer;
begin
  fa := GetActionFromKey(Shift, Key);
  if fa = faNoop then
    fa := GetActionFromKeyChar(KeyChar);

  if fa <> faNoop then
  begin
    Main.ActionHandler.Execute(fa);
  end;
  ShowTrimm;
end;

procedure TFormMain10.UpdateOnParamValueChanged;
begin
end;

procedure TFormMain10.UpdateItemIndexParams;
begin

end;

procedure TFormMain10.UpdateItemIndexReports;
begin

end;

procedure TFormMain10.UpdateItemIndexTrimms;
begin
end;

procedure TFormMain10.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Main.DoMouseWheel(Shift, WheelDelta);
  Handled := True;
end;

procedure TFormMain10.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;

    Main.CycleToolSet(0);

    { ClientHeigt is now available }
    LayoutComponents;

    Image.Align := TAlignLayout.Client;

    RotaForm.IsUp := True;
    RotaForm.Draw;
  end;
end;

procedure TFormMain10.FormResize(Sender: TObject);
begin
  if (Main <> nil) and Main.IsUp then
  begin
    MainVar.Scale := Handle.Scale;
    Inc(Main.ResizeCounter);
    Main.UpdateTouch;
    UpdateFederText;
  end;

  if FormShown then
  begin
    CenterRotaForm;
    UpdateReport;
    SpeedPanel.UpdateLayout;
  end;
end;

procedure TFormMain10.HandleAction(fa: Integer);
begin
  case fa of
    faToggleLanguage: ToggleLanguage;

    faShowMemo: MemoBtnClick(nil);
    faShowActions: ActionsBtnClick(nil);

//    faRotaForm1: SwapRota(1);
    faRotaForm2: SwapSpeedPanel(2); // SwapRota(2);
    faRotaForm3: SwapSpeedPanel(3); // SwapRota(3);
  end;
end;

function TFormMain10.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkF12: result := faMemeGotoSquare;
//    vkC: result := faCopyTrimmItem;
//    vkV: result := faPasteTrimmItem;
    vkEscape:
    begin
      if Shift = [ssShift] then
        result := faResetPosition
      else
        result := faReset;
    end;
  end;
end;

function TFormMain10.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  fa := faNoop;
  case KeyChar of
    'a': fa := faSalingA;
    'A': fa := faFixpointA0;

    'b': fa := faBiegung;
    'B': fa := faFixpointB0;

    'c': fa := faMastfallF0C;
    'C': fa := faFixpointC0;

    'd': fa := faFixpointD;
    'D': fa := faFixpointD0;

    'e': fa := faFixpointE;
    'E': fa := faFixpointE0;

    'f': fa := faMastfallF0F;
    'F': fa := faFixpointF0;

    'g': fa := faMastfallVorlauf;

    'h': fa := faSalingH;
    'H': fa := faToggleHelp;

    'i': fa := faWheelRight;
    'I': fa := faWheelLeft;

    'j': fa := faWheelUp;
    'J': fa := faWheelDown;

    'k': ;
    'K': fa := faRggKoppel;

    'l': fa := faToggleShowLegend;
    'L': fa := faMemeGotoLandscape;

    'm': fa := faMemoryBtn;
    'M': fa := faCopyAndPaste;

    'n': fa := faShowNormalKeyInfo;

    'o': fa := faWoben;

    'p': fa := faPan;
    'P': fa := faMemeGotoPortrait;

    'q': fa := faToggleAllText;

    'r': fa := faToggleReport;
    'R': fa := faReadTrimmFile;

    's': fa := faShowSpecialKeyInfo;
    'S': fa := faMemeGotoSquare;

    't': fa := faToggleDarkMode;
    'T': fa := faToggleSpeedPanel;

    'u': fa := faToggleDataText;
    'U': fa := faToggleDiffText;

    'v': fa := faVorstag;

    'w': fa := faWante;

    'z': fa := faShowInfoText;
    'Z': fa := faUpdateTrimm0;

    '0': fa := faTrimm0;
    '1': fa := faTrimm1;
    '2': fa := faTrimm2;
    '3': fa := faTrimm3;
    '4': fa := faTrimm4;
    '5': fa := faTrimm5;
    '6': fa := faTrimm6;
    '7': fa := fa420;
    '8': fa := faLogo;
    '9': ;

    '!': fa := faShowNormalKeyInfo;
    '"': fa := faShowSpecialKeyInfo;
    '§': fa := faShowInfoText;
    '$': fa := faShowDebugInfo;
    '=': fa := faShowZOrderInfo;
    '?': fa := faShowHelpText;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    '#': fa := faActionPage4;

    ',': fa := faRotaForm1;
    '.': fa := faRotaForm2;
    '-': fa := faRotaForm3;

    ';': fa := faRotaForm1;
    ':': fa := faRotaForm2;
    '_': fa := faRotaForm3;

    else fa := faNoop;

  end;
  result := fa;
end;

function TFormMain10.GetIsUp: Boolean;
begin
  if not MainVar.AppIsClosing and Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain10.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
end;

function TFormMain10.GetOpenFileName(dn, fn: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(self);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TFormMain10.GetSaveFileName(dn, fn: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(self);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;

procedure TFormMain10.CreateComponents;
begin
  Image := TOriginalImage.Create(Self, BitmapWidth, BitmapHeight);
  Image.Name := 'Image';
  Image.Parent := Self;

  SpeedPanel03 := TActionSpeedBarRG03.Create(Self);
  SpeedPanel03.Name := 'SpeedPanel03';
  SpeedPanel03.Parent := Self;
  SpeedPanel03.ShowHint := True;
  SpeedPanel03.Visible := False;

  SpeedPanel := SpeedPanel03;

  ComponentsCreated := True;
end;

procedure TFormMain10.SwapSpeedPanel(Value: Integer);
begin
  SpeedPanel.Visible := False;

  case Value of
    1: SpeedPanel := SpeedPanel03;
    2: SpeedPanel := SpeedPanel03;
    3: SpeedPanel := SpeedPanel03;
    else
      SpeedPanel := SpeedPanel03;
  end;

  SpeedPanel.Width := ClientWidth - 3 * Raster - Margin;
  SpeedPanel.Visible := True;
  SpeedPanel.UpdateLayout;;
  SpeedPanel.DarkMode := MainVar.ColorScheme.IsDark;
  SpeedPanel.UpdateColor;
end;

procedure TFormMain10.LayoutSpeedPanel(SP: TActionSpeedBar);
begin
  SP.Anchors := [];
  SP.Position.X := 2 * Raster + Margin;
  SP.Position.Y := Raster + Margin;
  SP.Width := ClientWidth - 3 * Raster - 2 * Margin;
  SP.Height := SpeedPanelHeight;
  SP.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
  SP.UpdateLayout;
end;

procedure TFormMain10.LayoutComponents;
begin
  if not ComponentsCreated then
    Exit;

  { ClientWidth and ClientHeight may still be at DesignTime Values, }
  { when called earlier than FormShow. }
  { Then it only 'works' if these values are big enough, }
  { so that computed values for Height and Width are > 0 }

  LayoutSpeedPanel(SpeedPanel03);
end;

function TFormMain10.GetChecked(fa: Integer): Boolean;
begin
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faRggBogen: result := Main.Bogen;
    faRggKoppel: result := Main.Koppel;

    faSofortBtn: result := Main.SofortBerechnen;
    faGrauBtn: result := Main.BtnGrauDown;
    faBlauBtn: result := Main.BtnBlauDown;
    faMemoryBtn: result := False;
    faMultiBtn: result := RotaForm.WantOverlayedRiggs;

    faToggleReport: result := False; //ReportText.Visible;
    faReportNone..faReportReadme: result := ReportManager.GetChecked(fa);

    faToggleDataText: result := ShowDataText;
    faToggleDiffText: result := ShowDiffText;
    faToggleTrimmText: result := ShowTrimmText;
  end;
end;

procedure TFormMain10.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Parent := self; // needed for Alt-Tab
    FormMemo.Memo.Lines.Clear;
    Main.WriteTrimmItem;
    FormMemo.Memo.Text := Main.FLText;
//    CheckFormBounds(FormMemo);
  end;
  FormMemo.Visible := True;
  FormMemo.Show; //needed on Mac
end;

procedure TFormMain10.ActionsBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
    FormAction.Parent := self; // needed for Alt-Tab
//    CheckFormBounds(FormAction);
  end;
  FormAction.Visible := True;
  FormAction.Show; //needed on Mac
end;

procedure TFormMain10.InitSpeedButtons;
begin
  if SpeedPanel03 <> nil then
    SpeedPanel03.InitSpeedButtons;
end;

procedure TFormMain10.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  Self.Fill.Color := MainVar.ColorScheme.claBackground;
  RotaForm.BackgroundColor := MainVar.ColorScheme.claBackground;

  if MainVar.ColorScheme.IsDark then
    SpeedColorScheme.InitDark
  else
    SpeedColorScheme.InitLight;

  SpeedPanel.DarkMode := MainVar.ColorScheme.IsDark;
  SpeedPanel.UpdateColor;

  RotaForm.DarkMode := SpeedColorScheme.IsDark;
end;

procedure TFormMain10.UpdateFederText;
begin
  Main.FederTextUpdateCaption;
end;

procedure TFormMain10.CenterRotaForm;
begin
  RotaForm.InitPosition(Width, Height, 0, 0);
  if FormShown then
    RotaForm.Draw;
end;

{$ifdef WantMenu}
procedure TFormMain10.PopulateMenu;
begin
  { need dummy MenuItem to set correct client height from beginning }
  if MenuItem.Parent = MainMenu then
  begin
    MainMenu.RemoveObject(MenuItem);
  end;

  if Assigned(MainMenu) and Assigned(Main) then
  begin
    FederMenu.InitMainMenu(MainMenu);
  end;
end;

function TFormMain10.GetMainMenuVisible: Boolean;
begin
  result := MainMenu <> nil;
end;

procedure TFormMain10.SetMainMenuVisible(const Value: Boolean);
begin
  if Value and not Assigned(MainMenu) then
  begin
    MainMenu := TMainMenu.Create(self);
    MainMenu.Tag := -1;
    MainMenu.Parent := self;
    PopulateMenu;
    MainMenu.Tag := 1;
    MainMenu.RecreateOSMenu;
  end
  else if Assigned(MainMenu) and not Value then
  begin
    MainMenu.Free;
    MainMenu := nil;
    Main.UpdateTouch;
  end;
end;
{$endif}

procedure TFormMain10.ShowTrimm;
begin

end;

procedure TFormMain10.ToggleButtonSize;
begin

end;

procedure TFormMain10.UpdateReport;
begin

end;

function TFormMain10.GetShowDataText: Boolean;
begin
  result := FShowDataText;
end;

function TFormMain10.GetShowDiffText: Boolean;
begin
  result := FShowDiffText;
end;

function TFormMain10.GetShowTrimmText: Boolean;
begin
  result := FShowTrimmText;
end;

procedure TFormMain10.SetShowDataText(const Value: Boolean);
begin
  FShowDataText := Value;
end;

procedure TFormMain10.SetShowDiffText(const Value: Boolean);
begin
  FShowDiffText := Value;
end;

procedure TFormMain10.SetShowTrimmText(const Value: Boolean);
begin
  FShowTrimmText := Value;
end;

procedure TFormMain10.RotaFormRotateZ(Delta: single);
begin
  RotaForm.RotateZ(Delta);
end;

procedure TFormMain10.RotaFormZoom(Delta: single);
begin
  RotaForm.Zoom(Delta);
end;

procedure TFormMain10.ToggleLanguage;
begin
  MainVar.WantLocalizedText := not MainVar.WantLocalizedText;
  RggLocalizedStrings.UpdateText;
  FederMenu.UpdateText(MainMenu);
  SpeedPanel.UpdateText;
  Main.CycleToolSet(0);
  Main.ParamCaption := Main.Param2Text(Main.Param);
  Main.FederTextUpdateCaption;
end;

end.
