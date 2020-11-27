unit RiggVar.RG.Report;

interface

{.$define WantUserConfusingReports }
{$define WantXMLReports }

uses
  System.Classes,
  RiggVar.FB.ActionConst,
  RggReport;

type
  TRggReport = (
    rgLog,
    rgJson,
    rgData,
    rgShort,
    rgLong,

    rgTrimmText,
    rgJsonText,
    rgDataText,
    rgDiffText,

    rgAusgabeDetail,
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
    RDR: array[0..Integer(High(TRggReport))] of TRggReport; //TDictionary<Integer, TRggReport>;
    RDI: array[TRggReport] of Integer; //TDictionary<TRggReport, Integer>;
    rs: set of TRggReport;
    FCurrentIndex: Integer;
    FCurrentReport: TRggReport;
    procedure InitRD;
    procedure SetCurrentIndex(const Value: Integer);
    procedure SetCurrentReport(const Value: TRggReport);
  public
    constructor Create(MemoLines: TStrings);
    destructor Destroy; override;
    procedure InitLB(LB: TStrings);
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    procedure ShowCurrentReport;
    function GetItemIndexOfReport(const Value: TRggReport): Integer;
    function GetReportCaption(r: TRggReport): string;
    function GetCurrentCaption: string;
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property CurrentReport: TRggReport read FCurrentReport write SetCurrentReport;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRggReportManager }

constructor TRggReportManager.Create(MemoLines: TStrings);
begin
  FMemo := MemoLines;
  ML := MemoLines;
  RiggReport := TRiggReport.Create;
  InitRD;
end;

destructor TRggReportManager.Destroy;
begin
  ML := nil; // not owned
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
    rgShort: result := 'Trimm-Item Short';
    rgLong: result := 'Trimm-Item Long';
    rgTrimmText: result := 'Trimm Text';
    rgJsonText: result := 'Json Text';
    rgDataText: result := 'Data Text';
    rgDiffText: result := 'Diff Text';
    rgAusgabeDetail: result := 'Ausgabe Detail';
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

procedure TRggReportManager.HandleAction(fa: Integer);
var
  rg: TRggReport;
begin
  case fa of
    faReportNone: rg := rgNone;
    faReportLog: rg := rgLog;
    faReportJson: rg := rgJson;
    faReportData: rg := rgData;
    faReportShort: rg := rgShort;
    faReportLong: rg := rgLong;
    faReportTrimmText: rg := rgTrimmText;
    faReportJsonText: rg := rgJsonText;
    faReportDataText: rg := rgDataText;
    faReportDiffText: rg := rgDiffText;
    faReportAusgabeDetail: rg := rgAusgabeDetail;
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
    faReportShort: result := CurrentReport = rgShort;
    faReportLong: result := CurrentReport = rgLong;
    faReportTrimmText: result := CurrentReport = rgTrimmText;
    faReportJsonText: result := CurrentReport = rgJsonText;
    faReportDataText: result := CurrentReport = rgDataText;
    faReportDiffText: result := CurrentReport = rgDiffText;
    faReportAusgabeDetail: result := CurrentReport = rgAusgabeDetail;
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
  result := RDI[Value];
  if (result = 0) and (Value <> rgLog) then
  begin
    result := RDI[rgNone];
  end;
end;

procedure TRggReportManager.SetCurrentIndex(const Value: Integer);
var
  r: TRggReport;
begin
  if Value < 0 then
    Exit;
  if Value > Integer(High(TRggReport)) then
    Exit;

  r := RDR[Value];
  FCurrentIndex := Value;
  FCurrentReport := r;
end;

procedure TRggReportManager.SetCurrentReport(const Value: TRggReport);
begin
  FCurrentReport := Value;
end;

procedure TRggReportManager.ShowCurrentReport;
begin
  ML.BeginUpdate;
  try
    ML.Clear;
    RiggReport.SofortFlag := Main.SofortBerechnen;
    case CurrentReport of
      rgNone: ;
      rgReadme:
      begin
        ML.Add('On the desktop  - use scroll Wheel of the mouse!');
        ML.Add('On touch screen - use touch bar on button frame');
        ML.Add('');
        ML.Add('Wheel = small step change of current param value');
        ML.Add('Shift-Wheel = big step change of current param value');
      end;
      rgLog: ML.Text := Main.Logger.TL.Text;
      rgJson: Main.RggData.WriteJSon(ML);
      rgData: Main.RggData.WriteReport(ML);
      rgAusgabeDetail:
      begin
        Main.Rigg.AusgabeText(ML, False, Main.SofortBerechnen);
      end;
      rgAusgabeRL:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRL(Main.Rigg.RiggLengths);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRP:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRP(Main.Rigg.RiggPoints);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRLE:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRLE(Main.Rigg.RelaxedRiggLengths);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeRPE:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeRPE(Main.Rigg.RelaxedRiggPoints);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeDiffL:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeDiffL(Main.Rigg.RiggLengths, Main.Rigg.RelaxedRiggLengths);
        ML.Assign(RiggReport.ML);
      end;
      rgAusgabeDiffP:
      begin
        RiggReport.ML.Clear;
        RiggReport.AusgabeDiffP(Main.Rigg.RiggPoints, Main.Rigg.RelaxedRiggPoints);
        ML.Assign(RiggReport.ML);
      end;
{$ifdef MSWindows}
      rgXML:
      begin
        Main.Rigg.WriteXml(ML, MainVar.AllTags);
      end;
{$endif}
      rgShort: ML.Text := Main.TrimmShort;
      rgLong: ML.Text := Main.TrimmLong;
      rgDiffText: Main.UpdateDiffText(ML);
      rgJsonText: Main.UpdateJsonText(ML);
      rgDataText: Main.UpdateDataText(ML);
      rgTrimmText: Main.UpdateTrimmText(ML);
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
  rs := [];

//  { there is not enough space to show all reports in listbox }

{ It is possible to exclude reports. The UI should expect this. }

  Include(rs, rgLog);
{$ifdef WantUserConfusingReports }
  Include(rs, rgJson);
  Include(rs, rgData);
{$endif}
  Include(rs, rgShort);
  Include(rs, rgLong);

  Include(rs, rgTrimmText);
  Include(rs, rgJsonText);
  Include(rs, rgDataText);
  Include(rs, rgDiffText);

  Include(rs, rgAusgabeDetail);
  Include(rs, rgAusgabeRL);
  Include(rs, rgAusgabeRP);
  Include(rs, rgAusgabeRLE);
  Include(rs, rgAusgabeRPE);
  Include(rs, rgAusgabeDiffL);
  Include(rs, rgAusgabeDiffP);
{$ifdef WantXMLReports }
  Include(rs, rgXML);
{$endif}
  Include(rs, rgDebugReport);
  Include(rs, rgReadme);
  Include(rs, rgNone);

  i := 0;
  for r in rs do
  begin
    RDR[i] := r;
    RDI[r] := i;
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
