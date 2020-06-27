unit RiggVar.App.Main1;

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

uses
  System.SysUtils,
  System.Classes,
  RiggVar.App.Main0,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Def,
  RiggVar.RG.Data,
  RiggVar.RG.Main;

type
  TMain1 = class(TMain0)
  private
    FTrimm: Integer;
    function GetCurrentTrimm: TRggData;
    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);
    procedure SetTrimm(const Value: Integer);
    procedure SetTrimmNoChange(const Value: Integer);
    procedure DoReadTrimmFile(fn: string);
    function GetTrimmFilePath: string;
    function GetIsRggParam: Boolean;
    function LogFileNameToInfoFormatted(t1, t2, fn: string): boolean;
  protected
    procedure InitTrimmData;
    procedure PasteTrimm;
  public
    RggMain: TRggMain; // RggMain.Rigg is the model of the app

    RggData: TRggData; // temp object for data transfer

    { slot used as reference for diffing }
    Trimm0: TRggData;

    { user data slots }
    Trimm1: TRggData; // selected with button T1
    Trimm2: TRggData; // selected with button T2
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;

    { example data slots }
    Trimm7: TRggData; // 420
    Trimm8: TRggData; // Logo

    ReportCounter: Integer;
    ResizeCounter: Integer;

    constructor Create(rggm: TRggMain);
    destructor Destroy; override;

    procedure HandleAction(fa: TFederAction); override;
    function GetChecked(fa: TFederAction): Boolean; override;

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    function GetTrimmItem(i: Integer): TRggData;
    function GetTrimmItemReport(ReportID: Integer): string;
    function GetTrimmItemReportData: string;
    function GetTrimmItemReportJson: string;
    function GetTrimmItemReportShort: string;
    function GetTrimmItemReportLong: string;

    procedure WriteTrimmItem;
    procedure WriteTrimmFile;

    procedure CopyAndPaste;
    procedure CopyTrimmFile;
    procedure CopyTrimmItem;
    procedure PasteTrimmItem;

    procedure UpdateTrimm0;
    procedure ReadTrimmFile0;
    procedure ReadTrimmFile;
    procedure SaveTrimmFile;
    procedure ReadTrimmFileAuto;
    procedure SaveTrimmFileAuto;

    procedure ReadText(ML: TStrings);

    procedure DropTargetDropped(fn: string);
    procedure DoReport;
    procedure DoCleanReport;
    procedure ShowDebugData;
    procedure LogFileNameToInfo(fn: string);

    property TrimmNoChange: Integer read FTrimm write SetTrimmNoChange;
    property Trimm: Integer read FTrimm write SetTrimm;
    property CurrentTrimm: TRggData read GetCurrentTrimm;
    property TrimmData: string read GetTrimmItemReportData;
    property TrimmJson: string read GetTrimmItemReportJson;
    property TrimmShort: string read GetTrimmItemReportShort;
    property TrimmLong: string read GetTrimmItemReportLong;

    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;

    property IsRggParam: Boolean read GetIsRggParam;
  end;

implementation

uses
  System.Rtti,
  FrmMain,
  RggTypes,
  FMX.Platform,
  RiggVar.FB.Classes,
  RiggVar.App.Main,
  RiggVar.Util.AppUtils;

{ TMain1 }

constructor TMain1.Create(rggm: TRggMain);
begin
  Main := self;
  MainVar.RG := True;

  inherited Create;

  RggData := TRggData.Create;
  RggData.Name := 'fd';

  Trimm0 := TRggData.Create;
  Trimm0.Name := 'T0';
  Trimm7 := TRggData.Create;
  Trimm7.Name := '420';
  Trimm8 := TRggData.Create;
  Trimm8.Name := 'Logo';

  Trimm1 := TRggData.Create;
  Trimm2 := TRggData.Create;
  Trimm3 := TRggData.Create;
  Trimm4 := TRggData.Create;
  Trimm5 := TRggData.Create;
  Trimm6 := TRggData.Create;

  InitTrimmData;

  RggMain := rggm;

  { this should be done after or when calling RggMain.Init }
//  RggMain.InitLogo; // sets WantLogoData to true
//  RggMain.Init420; // resets WantLogo to false
//  WantLogoData := False;
end;

destructor TMain1.Destroy;
begin
  MainVar.AppIsClosing := True;

  RggMain.Free;

  RggData.Free;
  Trimm0.Free;
  Trimm1.Free;
  Trimm2.Free;
  Trimm3.Free;
  Trimm4.Free;
  Trimm5.Free;
  Trimm6.Free;
  Trimm7.Free;
  Trimm8.Free;

  inherited;
end;

procedure TMain1.SetShowTrimmText(const Value: Boolean);
begin
  FormMain.ShowTrimmText := Value;
end;

procedure TMain1.SetShowDiffText(const Value: Boolean);
begin
  FormMain.ShowDiffText := Value;
end;

procedure TMain1.SetShowDataText(const Value: Boolean);
begin
  FormMain.ShowDataText := Value;
end;

function TMain1.GetShowTrimmText: Boolean;
begin
  result := FormMain.ShowTrimmText;
end;

function TMain1.GetShowDiffText: Boolean;
begin
  result := FormMain.ShowDiffText;
end;

function TMain1.GetShowDataText: Boolean;
begin
  result := FormMain.ShowDataText;
end;

function TMain1.GetTrimmItemReport(ReportID: Integer): string;
begin
  if Assigned(RggMain) and Assigned(RggMain.Rigg) and Assigned(RggData) and Assigned(FL) then
  begin
    RggMain.Rigg.SaveToFederData(RggData);
    FL.Clear;
    case ReportID of
      0: RggData.WriteReport(FL);
      1: RggData.WriteJSon(FL);
      2:
      begin
        RggData.WantAll := False;
        RggData.SaveTrimmItem(FL);
      end;
      3:
      begin
        RggData.WantAll := True;
        RggData.SaveTrimmItem(FL);
      end;

      else
        RggData.WriteReport(FL);
    end;
    result := FL.Text;
    FL.Clear;
  end;
end;

function TMain1.GetTrimmItemReportLong: string;
begin
  result := GetTrimmItemReport(3);
end;

function TMain1.GetTrimmItemReportShort: string;
begin
  result := GetTrimmItemReport(2);
end;

function TMain1.GetTrimmItemReportJson: string;
begin
  result := GetTrimmItemReport(1);
end;

function TMain1.GetTrimmItemReportData: string;
begin
  result := GetTrimmItemReport(0);
end;

procedure TMain1.WriteTrimmItem;
var
  fd: TRggData;
begin
  FL.Clear;
  fd := RggData;
  RggMain.SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);
end;

procedure TMain1.CopyTrimmItem;
begin
  Logger.Info('in CopyTrimmItem');
  WriteTrimmItem;
  CopyText;
  FL.Clear;
end;

procedure TMain1.WriteTrimmFile;
begin
  FL.Clear;
  Trimm0.SaveTrimmFile(FL);
end;

procedure TMain1.CopyTrimmFile;
begin
  WriteTrimmFile;
  CopyText;
  FL.Clear;
end;

procedure TMain1.CopyAndPaste;
var
  fd: TRggData;
begin
  { copy }
  fd := RggData;
  RggMain.SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);

  { paste }
  ReadText(FL);
  FL.Clear;
end;

procedure TMain1.PasteTrimmItem;
begin
  Logger.Info('in PasteTrimmItem');
  PasteTrimm;
  { Note: There is just one paste button (pti), named after the item, }
  { but you can paste a Trimm-Item OR a Trimm-File. }
end;

procedure TMain1.PasteTrimm;
var
  v: TValue;
  s: string;
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    FL.Clear;
    try
      v := cbs.GetClipboard;
      if not v.IsEmpty then
      begin
        s := v.AsString;
        if (s <> '') and (Length(s) < 10000) then
        begin
          FL.Text := s;
          ReadText(FL);
          FL.Clear;
        end
        else
        begin
          Logger.Error('there is no ''data'' string on the clipboard');
        end;
      end;
    except
      Logger.Error('no usable data on clipboard');
    end;
  end;
end;

procedure TMain1.ReadText(ML: TStrings);
var
  i: Integer;
  s: string;
  IsTrimmItem: Boolean;
  IsTrimmFile: Boolean;
begin
  try
    IsTrimmItem := False;
    IsTrimmFile := False;
    for i := 0 to ML.Count-1 do
    begin
      if i > 4 then
        break;
      s := ML[i];
      if Pos('DOCTYPE', s) > 0 then
      begin
        if s.Contains('Trimm-Item' )then
          IsTrimmItem := True;
        if s.Contains('Trimm-File' )then
          IsTrimmFile := True;
      end;
      if IsTrimmItem then
      begin
        RggData.LoadTrimmItem(ML);
        CurrentTrimm.Assign(RggData);
        Trimm := FTrimm;
        Logger.Info(Format('  Trimm %d assigned', [Trimm]));
        break;
      end
      else if IsTrimmFile then
      begin
        RggData.ReadTrimmFile(ML);
        Trimm := FTrimm;
        Logger.Info(Format('  TrimmFile read, Current Trimm is %d.', [Trimm]));
        break;
      end;
    end;
  except
    Trimm := 1;
  end;
end;

procedure TMain1.InitTrimmData;
var
  fd: TRggData;
begin
  Logger.Info('in InitTrimmData ( default data )');
  fd := Trimm0;
  fd.Reset;

  fd := Trimm1;
  fd.Assign(Trimm0);
  fd.Name := 'T1';
  fd.MV := fd.MV + 10;

  fd := Trimm2;
  fd.Assign(Trimm0);
  fd.Name := 'T2';
  fd.WLPos := fd.WLPos + 20;

  fd := Trimm3;
  fd.Assign(Trimm0);
  fd.Name := 'T3';
  fd.VOPos := fd.VOPos + 30;

  fd := Trimm4;
  fd.Assign(Trimm0);
  fd.Name := 'T4';
  fd.SHPos := fd.SHPos + 40;

  fd := Trimm5;
  fd.Assign(Trimm0);
  fd.Name := 'T5';
  fd.SAPos := fd.SAPos + 50;

  fd := Trimm6;
  fd.Assign(Trimm0);
  fd.Name := 'T6';
  fd.VOPos := fd.VOPos + 60;
  fd.WLPos := fd.WLPos - 20;
end;

function TMain1.GetTrimmFilePath: string;
begin
  result := '';
{$ifdef MacOS}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef Win32}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Win64}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Android}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef IOS}
  result  := TAppUtils.GetIOSDataDirectory;
{$endif}
end;

procedure TMain1.ReadTrimmFile0;
var
  fp, sTrimmFileAuto, sTrimmFile: string;
begin
  if not IsSandBoxed then
  begin
    Logger.Info('in ReadTrimmFile0');
    fp := GetTrimmFilePath;
    if fp <> '' then
    begin
      sTrimmFile := fp + TrimmFileName;
      sTrimmFileAuto := fp + TrimmFileNameAuto;
      if FileExists(sTrimmFile) then
      begin
        DoReadTrimmFile(sTrimmFile);
      end
      else if FileExists(sTrimmFileAuto) then
      begin
        DoReadTrimmFile(sTrimmFileAuto);
      end
      else
      begin
        Logger.Info('Trimm-File.txt or Trimm-File-Auto.txt does not exits at path');
        LogFileNameToInfo(fp);
      end;
    end
    else
    begin
      Logger.Info('GetTrimmFilePath is empty');
    end;
  end;
end;

procedure TMain1.ReadTrimmFile;
var
  fp, fn, s: string;
begin
  Logger.Info('in ReadTrimmFile');
  fp := GetTrimmFilePath;

{ By default you try and load the 'manually edited' Trimm-File.txt; }
{ this should make sense on the Desktop, }
{ or on any device where you have access to the Documents folder. }
  fn := TrimmFileName;

{ Maybe you want to have the same behaviour on Windows and iOS }
{ for debugging purpose only... }
{$ifdef MSWINDOWS}
//  fn := TrimmFileNameAuto;
{$endif}

{ On Android and iOS the Trimm-File in the known location cannot be edited, }
{ so it does not make sense to read a 'manually edited' Trimm-File.txt, }
{ but you can manualy read a Trimm-File-Auto.txt if already saved, }
{ e.g. by clicking on a button. }
{$ifdef IOS}
  fn := TrimmFileNameAuto;
{$endif}
{$ifdef Android}
  fn := TrimmFileNameAuto;
{$endif}

  s := fp + fn;
  if IsSandboxed then
  begin
    s := FormMain.GetOpenFileName(fp, fn);
  end;

  if s <> '' then
  begin
    DoReadTrimmFile(s);
  end;
end;

procedure TMain1.ReadTrimmFileAuto;
var
  fp, fn: string;
begin
  if not IsSandboxed then
  begin
    Logger.Info('in ReadTrimmFileAuto');
    fp := GetTrimmFilePath;
    fn := TrimmFileNameAuto;
    if (fp <> '') and (fn <> '') then
      DoReadTrimmFile(fp + fn);
  end;
end;

procedure TMain1.DoBigWheel(Delta: single);
begin
  if not IsRggParam then
    inherited
  else if MainVar.RG then
    RggMain.DoBigWheel(Delta)
  else
    inherited;
end;

procedure TMain1.DoSmallWheel(Delta: single);
begin
  if not IsRggParam then
    inherited
  else if MainVar.RG then
    RggMain.DoSmallWheel(Delta)
  else
    inherited;
end;

procedure TMain1.DoReadTrimmFile(fn: string);
begin
  Logger.Info('in DoReadTrimmFile');
  if (fn <> '') and FileExists(fn) then
  begin
    try
      try
        LogFileNameToInfo(fn);
        FL.LoadFromFile(fn, TEncoding.UTF8);
        RggData.ReadTrimmFile(FL);
        Trimm := FTrimm;
      except
        Logger.Info('  in exeption handler');
        InitTrimmData;
      end;
    finally
      FL.Clear;
    end;
  end
  else
  begin
    Logger.Info('TrimmFile does not exist');
    LogFileNameToInfo(fn);
    Logger.Info('Nothing read.');
    InitTrimmData;
  end;
end;

procedure TMain1.SaveTrimmFile;
begin
  Logger.Info('in SaveTrimmFile');
  SaveTrimmFileAuto;
end;

procedure TMain1.SaveTrimmFileAuto;
var
  fp, fn, s: string;
begin
  Logger.Info('in SaveTrimmFileAuto');
  fp := GetTrimmFilePath;
  fn := TrimmFileNameAuto;

  s := fp + fn;
  if IsSandboxed then
  begin
    s := FormMain.GetSaveFileName(fp, fn);
  end;

  if s <> '' then
  begin
    Trimm0.SaveTrimmFile(FL);
    LogFileNameToInfo(s);
    FL.SaveToFile(s);
    Logger.Info('TrimmFileAuto saved.');
  end
  else
  begin
    Logger.Info('Nothing saved.');
  end;
end;

procedure TMain1.UpdateTrimm0;
begin
  Logger.Info('in UpdateTrimm0');
  RggMain.SaveTrimm(Trimm0);
  FormMain.UpdateReport;
end;

function TMain1.GetTrimmItem(i: Integer): TRggData;
begin
  case i of
    1: result := Trimm1;
    2: result := Trimm2;
    3: result := Trimm3;
    4: result := Trimm4;
    5: result := Trimm5;
    6: result := Trimm6;
    7: result := Trimm7;
    8: result := Trimm8;
    else
      result := Trimm0;
  end;
end;

function TMain1.GetCurrentTrimm: TRggData;
begin
  result := GetTrimmItem(FTrimm);
end;

procedure TMain1.SetTrimmNoChange(const Value: Integer);
begin
  Logger.Info('SetTrimmNoChange: ' + IntToStr(Value));
  FTrimm := Value;
end;

procedure TMain1.SetTrimm(const Value: Integer);
begin
  Logger.Info('SetTrimm: ' + IntToStr(Value));
  FTrimm := Value;
  RggMain.LoadTrimm(CurrentTrimm);
  FormMain.UpdateOnParamValueChanged;
end;

function TMain1.GetIsRggParam: Boolean;
begin
  result := True;
end;

procedure TMain1.HandleAction(fa: TFederAction);
begin
  if IsUp then
  case fa of
    faUpdateReportText: DoCleanReport;
    faToggleDebugText: ShowDebugData;

    faParamValueMinus1, faWheelLeft: DoMouseWheel([ssShift], -1);
    faParamValuePlus1, faWheelRight: DoMouseWheel([ssShift], 1);
    faParamValuePlus10, faWheelUp: DoMouseWheel([ssCtrl], 1);
    faParamValueMinus10, faWheelDown: DoMouseWheel([ssCtrl], -1);

    faController: RggMain.SetParameter(faController);
    faWinkel: RggMain.SetParameter(faWinkel);
    faVorstag: RggMain.SetParameter(faVorstag);
    faWante: RggMain.SetParameter(faWante);
    faWoben: RggMain.SetParameter(faWoben);
    faSalingH: RggMain.SetParameter(faSalingH);
    faSalingA: RggMain.SetParameter(faSalingA);
    faSalingL: RggMain.SetParameter(faSalingL);
    faSalingW: RggMain.SetParameter(faSalingW);
    faMastfallF0C: RggMain.SetParameter(faMastfallF0C);
    faMastfallF0F: RggMain.SetParameter(faMastfallF0F);
    faMastfallVorlauf: RggMain.SetParameter(faMastfallVorlauf);
    faBiegung: RggMain.SetParameter(faBiegung);
    faMastfussD0X: RggMain.SetParameter(faMastfussD0X);

    faParamAPW: RggMain.SetParameter(faParamAPW);
    faParamEAH: RggMain.SetParameter(faParamEAH);
    faParamEAR: RggMain.SetParameter(faParamEAR);
    faParamEI: RggMain.SetParameter(faParamEI);

    faFixpointA0: RggMain.FixPoint := ooA0;
    faFixpointA: RggMain.FixPoint := ooA;
    faFixpointB0: RggMain.FixPoint := ooB0;
    faFixpointB: RggMain.FixPoint := ooB;
    faFixpointC0: RggMain.FixPoint := ooC0;
    faFixpointC: RggMain.FixPoint := ooC;
    faFixpointD0: RggMain.FixPoint := ooD0;
    faFixpointD: RggMain.FixPoint := ooD;
    faFixpointE0: RggMain.FixPoint := ooE0;
    faFixpointE: RggMain.FixPoint := ooE;
    faFixpointF0: RggMain.FixPoint := ooF0;
    faFixpointF: RggMain.FixPoint := ooF;

    faSalingTypOhneStarr,
    faSalingTypOhne,
    faSalingTypDrehbar,
    faSalingTypFest: RggMain.InitSalingTyp(fa);

    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderE,
    faWantRenderS: RggMain.ToggleRenderOption(fa);

    faViewpointS: RggMain.ViewPoint := vpSeite;
    faViewpointA: RggMain.ViewPoint := vpAchtern;
    faViewpointT: RggMain.ViewPoint := vpTop;
    faViewpoint3: RggMain.ViewPoint := vp3D;

    faHull: RggMain.SetOption(faHull);
    faDemo: RggMain.SetOption(faDemo);

    faTrimm0: Trimm := 0;
    faTrimm1: Trimm := 1;
    faTrimm2: Trimm := 2;
    faTrimm3: Trimm := 3;
    faTrimm4: Trimm := 4;
    faTrimm5: Trimm := 5;
    faTrimm6: Trimm := 6;

    fa420:
    begin
      RggMain.Init420;
      FormMain.UpdateOnParamValueChanged;
    end;

    faLogo:
    begin
      RggMain.InitLogo;
      FormMain.UpdateOnParamValueChanged;
    end;

    faUpdateTrimm0: UpdateTrimm0;
    faCopyAndPaste: CopyAndPaste;
    faCopyTrimmItem: CopyTrimmItem;
    faPasteTrimmItem: PasteTrimmItem;

    faReadTrimmFile: ReadTrimmFile;
    faCopyTrimmFile: CopyTrimmFile;
    faSaveTrimmFile: SaveTrimmFile;

    faToggleTrimmText: ShowTrimmText := not ShowTrimmText;
    faToggleDiffText: ShowDiffText := not ShowDiffText;
    faToggleDataText:
    begin
      MainVar.ShowDebugData := False;
      ShowDataText := not ShowDataText;
    end;

    else
      inherited HandleAction(fa);
  end;

  if IsUp then
  begin
    if (fa in ParamsRange) then
      FormMain.UpdateItemIndexParams
    else if (fa in ReportsRange) then
      FormMain.UpdateItemIndexReports
    else if (fa in TrimmsRange) then
      FormMain.UpdateItemIndexTrimms;
  end;
end;

function TMain1.GetChecked(fa: TFederAction): Boolean;
begin
  case fa of
    faController: result := RggMain.Param = fpController;
    faWinkel: result := RggMain.Param = fpWinkel;
    faVorstag: result := RggMain.Param = fpVorstag;
    faWante: result := RggMain.Param = fpWante;
    faWoben: result := RggMain.Param = fpWoben;
    faSalingH: result := RggMain.Param = fpSalingH;
    faSalingA: result := RggMain.Param = fpSalingA;
    faSalingL: result := RggMain.Param = fpSalingL;
    faSalingW: result := RggMain.Param = fpSalingW;
    faMastfallF0C: result := RggMain.Param = fpMastfallF0C;
    faMastfallF0F: result := RggMain.Param = fpMastfallF0F;
    faMastfallVorlauf: result := RggMain.Param = fpMastfallVorlauf;
    faBiegung: result := RggMain.Param = fpBiegung;
    faMastfussD0X: result := RggMain.Param = fpD0X;

    faParamAPW: result := RggMain.Param = fpAPW;
    faParamEAH: result := RggMain.Param = fpEAH;
    faParamEAR: result := RggMain.Param = fpEAR;
    faParamEI: result := RggMain.Param = fpEI;

    faFixpointA0: result := RggMain.FixPoint = ooA0;
    faFixpointA: result := RggMain.FixPoint = ooA;
    faFixpointB0: result := RggMain.FixPoint = ooB0;
    faFixpointB: result := RggMain.FixPoint = ooB;
    faFixpointC0: result := RggMain.FixPoint = ooC0;
    faFixpointC: result := RggMain.FixPoint = ooC;
    faFixpointD0: result := RggMain.FixPoint = ooD0;
    faFixpointD: result := RggMain.FixPoint = ooD;
    faFixpointE0: result := RggMain.FixPoint = ooE0;
    faFixpointE: result := RggMain.FixPoint = ooE;
    faFixpointF0: result := RggMain.FixPoint = ooF0;
    faFixpointF: result := RggMain.FixPoint = ooF;

    faSalingTypFest: result := RggMain.Rigg.SalingTyp = stFest;
    faSalingTypDrehbar: result := RggMain.Rigg.SalingTyp = stDrehbar;
    faSalingTypOhne: result := RggMain.Rigg.SalingTyp = stOhne;

    faTrimm0: result := Trimm = 0;
    faTrimm1: result := Trimm = 1;
    faTrimm2: result := Trimm = 2;
    faTrimm3: result := Trimm = 3;
    faTrimm4: result := Trimm = 4;
    faTrimm5: result := Trimm = 5;
    faTrimm6: result := Trimm = 6;
    fa420: result := Trimm = 7;
    faLogo: result := Trimm = 8;

    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderE,
    faWantRenderS:
    begin
      if RggMain.StrokeRigg <> nil then
        result := RggMain.StrokeRigg.QueryRenderOption(fa)
      else
        result := False;
    end;

    faHull: result := RggMain.HullVisible;
    faDemo: result := RggMain.Demo;

    faSofortBtn: result := RggMain.SofortBerechnen;
    faGrauBtn: result := RggMain.BtnGrauDown;
    faBlauBtn: result := RggMain.BtnBlauDown;
    faMemoryBtn: result := False;

    else
      result := inherited;
  end;
end;

procedure TMain1.DropTargetDropped(fn: string);
var
 ext: string;
begin
  Logger.Info('in DropTargetDropped');
  ext := ExtractFileExt(fn);
  if ext = '.txt' then
  begin
    DoReadTrimmFile(fn);
  end
  else
  begin
    Logger.Info('  .txt file expected');
  end;
end;

procedure TMain1.DoReport;
var
  ML: TStrings;
begin
  Inc(ReportCounter);
  ML := Logger.TL;

  while ML.Count > 24 - 8 do
    ML.Delete(0);

  ML.Add('Report:');
  ML.Add('  ReportCounter = ' + IntToStr(ReportCounter));
  ML.Add('  ColorScheme = ' + IntToStr(MainVar.ColorScheme.Scheme));
  ML.Add('  Scale = ' + FloatToStr(Scale));
  ML.Add('  Retina = ' + BoolStr[IsRetina]);
  ML.Add('  Sandboxed = ' + BoolStr[IsSandboxed]);
  ML.Add('  WantOnResize = ' + BoolStr[MainVar.WantOnResize]);
  ML.Add('  ResizeCounter = ' + IntToStr(ResizeCounter));
  ML.Add(Format('  ClientSize = (%d, %d)', [MainVar.ClientWidth, MainVar.ClientHeight]));
  ML.Add(Format('  Image.Size = (%d, %d)', [Round(FormMain.Image.Width), Round(FormMain.Image.Height)]));
  ML.Add('---');
end;

procedure TMain1.DoCleanReport;
begin
  MainVar.ShowDebugData := True;
  Logger.TL.Clear;
  DoReport;
end;

procedure TMain1.ShowDebugData;
begin
  if not ShowDataText then
  begin
    ShowDataText := true;
    MainVar.ShowDebugData := True;
  end
  else
  begin
    MainVar.ShowDebugData := not MainVar.ShowDebugData;
  end;
end;

procedure TMain1.LogFileNameToInfo(fn: string);
var
  t1: string;
  t2: string;
  r: boolean;
begin
  t1 := 'C:\Users\';
  t2 := 'Trimm';
  r := LogFileNameToInfoFormatted(t1, t2, fn);

  if not r then
  begin
    t1 := '/var/mobile/Containers/Data/Application/';
    t2 := 'Documents/';
    r := LogFileNameToInfoFormatted(t1, t2, fn);
  end;

  if not r then
  begin
    Logger.Info(fn);
  end;
end;

function TMain1.LogFileNameToInfoFormatted(t1: string; t2: string; fn: string): boolean;
var
  p2: Integer;
begin
  result := false;
  if fn.StartsWith(t1) then
  begin
    fn := fn.Substring(t1.Length);
    p2 := fn.LastIndexOf(t2);
    if p2 > -1 then
    begin
      Logger.Info(t1);
      Logger.Info('  ' + fn.Substring(0, p2));
      Logger.Info('  ' + fn.Substring(p2));
      result := True;
    end;
  end;
end;

end.
