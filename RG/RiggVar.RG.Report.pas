unit RiggVar.RG.Report;

interface

uses
  System.Classes,
  System.Generics.Collections,
  RiggVar.RG.Def,
  RiggVar.FB.ActionConst,
  RggReport;

type
  TRggReport = (
    rgLog,
    rgJson,
    rgData,
    rgTrimmText,
    rgDataText,
    rgDiffText,
    rgAusgabeRL,
    rgAusgabeRP,
    rgAusgabeRLE,
    rgAusgabeRPE,
    rgAusgabeDiffL,
    rgAusgabeDiffP,
    rgXML,
    rgDebugReport,
    rgReadme,
    rgNone
  );

  TRggReportManager = class
  private
    FMemo: TStrings;
    ML: TStrings;
    RiggReport: TRiggReport;
    RD: TDictionary<Integer, TRggReport>;
    RDI: TDictionary<TRggReport, Integer>;
    rs: set of TRggReport;
    FCurrentIndex: Integer;
    FCurrentReport: TRggReport;
    FXmlAllTags: Boolean;
    procedure InitRD;
    procedure SetCurrentIndex(const Value: Integer);
    procedure SetXmlAllTags(const Value: Boolean);
    procedure SetCurrentReport(const Value: TRggReport);
  public
    constructor Create(MemoLines: TStrings);
    destructor Destroy; override;
    procedure InitLB(LB: TStrings);
    procedure HA(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    procedure ShowCurrentReport;
    function GetItemIndexOfReport(const Value: TRggReport): Integer;
    function GetReportCaption(r: TRggReport): string;
    function GetCurrentCaption: string;
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property CurrentReport: TRggReport read FCurrentReport write SetCurrentReport;
    property XmlAllTags: Boolean read FXmlAllTags write SetXmlAllTags;
  end;

implementation

uses
//  Winapi.Windows,
//  Winapi.Messages,
  RiggVar.App.Main;

constructor TRggReportManager.Create(MemoLines: TStrings);
begin
  FMemo := MemoLines;
  ML := MemoLines;
  RiggReport := TRiggReport.Create;
  RD := TDictionary<Integer, TRggReport>.Create;
  RDI := TDictionary<TRggReport, Integer>.Create;
  InitRD;
end;

destructor TRggReportManager.Destroy;
begin
  ML := nil; // not owned
  RD.Free;
  RDI.Free;
  RiggReport.Free;
  inherited;
end;

function TRggReportManager.GetCurrentCaption: string;
begin
  result := GetReportCaption(CurrentReport);
end;

function TRggReportManager.GetReportCaption(r: TRggReport): string;
begin
  case r of
    rgLog: result := 'Log';
    rgJson: result := 'RggData.WriteJson';
    rgData: result := 'RggData.WriteReport';
    rgTrimmText: result := 'Trimm Text';
    rgDataText: result := 'Data Text';
    rgDiffText: result := 'Diff Text';
    rgAusgabeRL: result := 'Ausgabe rL';
    rgAusgabeRP: result := 'Ausgabe rP';
    rgAusgabeRLE: result := 'Ausgabe rLE';
    rgAusgabeRPE: result := 'Ausgabe rPE';
    rgAusgabeDiffL: result := 'Ausgabe Diff L';
    rgAusgabeDiffP: result := 'Ausgabe Diff P';
    rgXML: result := 'Write XML';
    rgDebugReport: result := 'Debug Report';
    rgReadme: result := 'Readme';
    rgNone: result := 'Do Nothing';
    else
      result := 'Unknown';
  end;
end;

procedure TRggReportManager.HA(fa: Integer);
var
  rg: TRggReport;
begin
  case fa of
    faReportNone: rg := rgNone;
    faReportLog: rg := rgLog;
    faReportJson: rg := rgJson;
    faReportData: rg := rgData;
    faReportTrimmText: rg := rgTrimmText;
    faReportDataText: rg := rgDataText;
    faReportDiffText: rg := rgDiffText;
    faReportAusgabeRL: rg := rgAusgabeRL;
    faReportAusgabeRP: rg := rgAusgabeRP;
    faReportAusgabeRLE: rg := rgAusgabeRLE;
    faReportAusgabeRPE: rg := rgAusgabeRPE;
    faReportAusgabeDiffL: rg := rgAusgabeDiffL;
    faReportAusgabeDiffP: rg := rgAusgabeDiffP;
    faReportXML: rg := rgXML;
    faReportDebugReport: rg := rgDebugReport;
    faReportReadme: rg := rgReadme;
    else
      rg := FCurrentReport;
  end;
  if rg <> FCurrentReport then
  begin
    FCurrentReport := rg;
//    ShowCurrentReport;
  end;
end;

function TRggReportManager.GetChecked(fa: Integer): Boolean;
begin
  case fa of
    faReportNone: result := CurrentReport = rgNone;
    faReportLog: result := CurrentReport = rgLog;
    faReportJson: result := CurrentReport = rgJson;
    faReportData: result := CurrentReport = rgData;
    faReportTrimmText: result := CurrentReport = rgTrimmText;
    faReportDataText: result := CurrentReport = rgDataText;
    faReportDiffText: result := CurrentReport = rgDiffText;
    faReportAusgabeRL: result := CurrentReport = rgAusgabeRL;
    faReportAusgabeRP: result := CurrentReport = rgAusgabeRP;
    faReportAusgabeRLE: result := CurrentReport = rgAusgabeRLE;
    faReportAusgabeRPE: result := CurrentReport = rgAusgabeRPE;
    faReportAusgabeDiffL: result := CurrentReport = rgAusgabeDiffL;
    faReportAusgabeDiffP: result := CurrentReport = rgAusgabeDiffP;
    faReportXML: result := CurrentReport = rgXML;
    faReportDebugReport: result := CurrentReport = rgDebugReport;
    faReportReadme: result := CurrentReport = rgReadme;
    else
      result := False;
  end;
end;

function TRggReportManager.GetItemIndexOfReport(const Value: TRggReport): Integer;
begin
  if not RDI.TryGetValue(Value, result) then
  begin
    result := -1;
  end;
end;

procedure TRggReportManager.SetCurrentIndex(const Value: Integer);
var
  r: TRggReport;
begin
//  FCurrentReport := RD.Items[Value];
  if RD.TryGetValue(Value, r) then
  begin
    FCurrentIndex := Value;
    FCurrentReport := r;
  end;
end;

procedure TRggReportManager.SetCurrentReport(const Value: TRggReport);
begin
  FCurrentReport := Value;
end;

procedure TRggReportManager.SetXmlAllTags(const Value: Boolean);
begin
  FXmlAllTags := Value;
end;

procedure TRggReportManager.ShowCurrentReport;
//var
//  MemoPosY: LongInt;
begin
  ML.BeginUpdate;
  try
//    MemoPosY := SendMessage(FMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
    ML.Clear;
    case CurrentReport of
      rgNone: ;
      rgReadme:
      begin
        ML.Add('On the desktop - use the scroll wheel of the mouse!');
        ML.Add('');
        ML.Add('Wheel by itself will scroll the text if too long.');
        ML.Add('Shift-Wheel will change current param value (small step)');
        ML.Add('Ctrl-Wheel will change current param value (big step)');
      end;
      rgLog: ML.Text := Main.Logger.TL.Text;
      rgJson: Main.RggData.WriteJSon(ML);
      rgData: Main.RggData.WriteReport(ML);
      rgAusgabeRL:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRL(Main.RggMain.Rigg.rL);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRP:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRP(Main.RggMain.Rigg.rP);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRLE:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRLE(Main.RggMain.Rigg.rLE);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRPE:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRPE(Main.RggMain.Rigg.rPE);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeDiffL:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeDiffL(Main.RggMain.Rigg.rL, Main.RggMain.Rigg.rLE);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeDiffP:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeDiffP(Main.RggMain.Rigg.rP, Main.RggMain.Rigg.rPE);
        ML.Assign(RiggReport.ML);
      end;
{$ifdef MSWindows}
      rgXML:
      begin
        Main.RggMain.Rigg.WriteXml(ML, XmlAllTags);
//        SendMessage(FMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
      end;
{$endif}
      rgDiffText: Main.RggMain.UpdateDiffText(ML);
      rgDataText: Main.RggMain.UpdateDataText(ML);
      rgTrimmText: Main.RggMain.UpdateTrimmText(ML);
      rgDebugReport:
      begin
        Main.DoCleanReport;
        ML.Text := Main.Logger.TL.Text;
      end;
    end;
  finally
    ML.EndUpdate;
  end;

  //ReportLabel.Caption := GetReportCaption(CurrentReport)
end;

procedure TRggReportManager.InitRD;
var
  i: Integer;
  r: TRggReport;
begin
  for r := Low(TRggReport) to High(TRggReport) do
    Include(rs, r);

  Exclude(rs, rgJson);
  Exclude(rs, rgData);

  Exclude(rs, rgTrimmText);
  Exclude(rs, rgAusgabeDiffL);
  Exclude(rs, rgAusgabeDiffP);
  Exclude(rs, rgDebugReport);

  Exclude(rs, rgAusgabeRLE);
  Exclude(rs, rgAusgabeRPE);

//    rgLog,
//    rgJson,
//    rgData,
//    rgTrimmText,
//    rgDataText,
//    rgDiffText,
//    rgAusgabeRL,
//    rgAusgabeRP,
//    rgAusgabeRLE,
//    rgAusgabeRPE,
//    rgAusgabeDiffL,
//    rgAusgabeDiffP,
//    rgXML,
//    rgDebugReport

  i := 0;
  for r in rs do
  begin
    RD.Add(i, r);
    RDI.Add(r, i);
    Inc(i);
  end;
end;

procedure TRggReportManager.InitLB(LB: TStrings);
var
  r: TRggReport;
begin
  for r in rs do
    LB.Add(GetReportCaption(r));
end;

end.
