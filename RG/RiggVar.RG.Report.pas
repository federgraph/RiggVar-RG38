unit RiggVar.RG.Report;

interface

{.$define WantUserConfusingReports}
{$define WantXMLReports}

uses
  System.SysUtils,
  System.Classes,
  RiggVar.RG.Types,
  RiggVar.RG.Fachwerk,
  RiggVar.FB.ActionConst;

type
  ReportConst = record
  const
    { LinkerRand LR: string[10] and Unterstrich }
    LR: string = '          ';
    US: string = '-------------------------------------------------';
  end;

  TFWReport = class
  private
    FML: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AusgabeKoordinaten(KX, KY: TFachwerk.TKnotenVektor; K: Integer);
    procedure AusgabeGeometrie(G: TFachwerk.TGeometrie; S: Integer);
    procedure AusgabeBelastung(FX, FY: TFachwerk.TKnotenVektor; K: Integer);
    procedure AusgabeAuflagerkraefte(Lager: TFachwerk.TAuflager);
    procedure AusgabeStabkraefte(FS: TFachwerk.TStabVektor; S: Integer);
    procedure AusgabeStabQuerschnitte(EA: TFachwerk.TStabVektor; S: Integer);
    procedure AusgabeElastizitaeten(Q: TFachwerk.TStabVektor; S: Integer);
    procedure AusgabeVerschiebungen
                (FOX, FOY, FO, POX, POY: TFachwerk.TKnotenVektor; K: Integer);
    procedure Ausgabe(Fachwerk: TFachwerk);
    property ML: TStrings read FML;
  end;

  TRiggReport = class
  private
    FML: TStrings;
    procedure PrintUnderline;
    procedure PrintUnderlineE;
  public
    IndexAuswahlL: set of TRiggRodIndexRange;
    IndexAuswahlP: set of TRiggPoint;
    SofortFlag: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AusgabeRL(rL: TRiggRods);
    procedure AusgabeRLE(rLe: TRiggRods);
    procedure AusgabeDiffL(rL, rLe: TRiggRods);
    procedure AusgabeRP(rP: TRiggPoints);
    procedure AusgabeRPE(rPe: TRiggPoints);
    procedure AusgabeDiffP(rP, rPe: TRiggPoints);
    procedure AusgabeRF(rF: TRiggRods);
    procedure AusgabeAngle(Value: TRiggAngles);
    procedure AusgabeWinkel(alpha, alpha1, alpha2, beta, gamma,
      delta1, delta2, epsilon, phi, psi: single);
    procedure AusgabeTrimmControls(Ctrls: TTrimmControls);
    procedure AusgabeSalingDaten(SData: TSalingDaten);
    procedure AusgabeLog(Liste: TStringList);
    property ML: TStrings read FML;
  end;

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
    function GetReportCaptionDE(r: TRggReport): string;
    function GetReportCaptionEN(r: TRggReport): string;
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
  if MainVar.WantLocalizedText and MainVar.WantGermanText then
    result := GetReportCaptionDE(r)
  else
    result := GetReportCaptionEN(r);
end;

function TRggReportManager.GetReportCaptionDE(r: TRggReport): string;
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

function TRggReportManager.GetReportCaptionEN(r: TRggReport): string;
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
    rgAusgabeRL: result := 'Output rL';
    rgAusgabeRP: result := 'Output rP';
    rgAusgabeRLE: result := 'Output rLE';
    rgAusgabeRPE: result := 'Output rPE';
    rgAusgabeDiffL: result := 'Output Diff L';
    rgAusgabeDiffP: result := 'Output Diff P';
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
        ML.Add('On the desktop - use scroll Wheel of the mouse!');
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

{ TFWReport }

constructor TFWReport.Create;
begin
  FML := TStringList.Create;
end;

destructor TFWReport.Destroy;
begin
  FML.Free;
end;

procedure  TFWReport.AusgabeKoordinaten(KX, KY: TFachwerk.TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Koordinaten der Knotenpunkte in mm:']));
    Add(Format('%s K  %10s %10s', [ReportConst.LR,'x','y']));
    Add(Format('%s %s', [ReportConst.LR, ReportConst.US]));
    for l:=1 to K do
    begin
      Add(Format('%s K%1d %10.3f %10.3f',
      [ReportConst.LR, l, KX[l], KY[l]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeGeometrie(G: TFachwerk.TGeometrie; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Geometrie:']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d verbunden mit K%1d und K%1d',
      [ReportConst.LR, i, G[0,i], G[1,i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeBelastung(FX, FY: TFachwerk.TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Belastung der Knotenpunkte in N:']));
    Add(Format('%s K  %10s %10s', [ReportConst.LR, 'x', 'y']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for l:=1 to K do
    begin
      Add(Format('%s K%1d %10.3f %10.3f',
      [ReportConst.LR, l, FX[l], FY[l]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeAuflagerkraefte(Lager: TFachwerk.TAuflager);
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Auflagerkräfte in N:']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    Add(Format('%s FAX  %10.3f',[ReportConst.LR, Lager[AX]]));
    Add(Format('%s FAY  %10.3f',[ReportConst.LR, Lager[AY]]));
    Add(Format('%s FBX  %10.3f',[ReportConst.LR, Lager[BX]]));
    Add(Format('%s FBY  %10.3f',[ReportConst.LR, Lager[BY]]));
    Add('');
  end;
end;

procedure  TFWReport.AusgabeStabkraefte(FS: TFachwerk.TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Stabkräfte in N:']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %12.3f', [ReportConst.LR, i, FS[i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeStabQuerschnitte(EA: TFachwerk.TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'EA in KN:']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %10.3f', [ReportConst.LR, i, EA[i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeElastizitaeten(Q: TFachwerk.TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [ReportConst.LR,'Stabelastizitäten in µm/N:']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %6.3f', [ReportConst.LR, i, Q[i]]));
    end;
    Add('');
  end;
end;

procedure TFWReport.AusgabeVerschiebungen(
FOX, FOY, FO, POX, POY: TFachwerk.TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s',[ReportConst.LR,'Verschiebungen der Knotenpunkte:']));
    Add(Format('%s %s',[ReportConst.LR,'(F0X - Verschiebung in 1. Richtung P0X)']));
    Add(Format('%s %s',[ReportConst.LR,'(F0Y - Verschiebung in 2. Richtung P0Y)']));
    Add(Format('%s %s',[ReportConst.LR,'(F0  - Absolutbetrag)']));
    Add(Format('%s %2s %7s %9s %7s %9s %9s',
    [ReportConst.LR,'','P0X','F0X','P0Y','F0Y','F0']));
    Add(Format('%s %2s %7s %9s %7s %9s %9s',
    [ReportConst.LR,'','/Grad','/mm','/Grad','/mm','/mm']));
    Add(Format('%s %s',[ReportConst.LR, ReportConst.US]));
    for l:=1 To K do
      Add(Format('%s K%1d %7.1f %9.2f %7.1f %9.2f %9.2f',
      [ReportConst.LR,l,POX[l],FOX[l],POY[l],FOY[l],FO[l]]));
    Add('');
  end;
end;

procedure TFWReport.Ausgabe(Fachwerk: TFachwerk);
begin
  with Fachwerk do
  begin
    AusgabeGeometrie(G, S);
    AusgabeStabQuerschnitte(vektorEA, S);
    AusgabeElastizitaeten(Q, S);
    AusgabeKoordinaten(KX, KY, K);
    AusgabeBelastung(FXsaved, FYsaved, K);
    AusgabeAuflagerkraefte(Lager);
    AusgabeStabkraefte(FS, S);
    AusgabeVerschiebungen(FO1, FO2, FO, PO1, PO2, K);
  end;
end;

{ TRiggReport }

constructor TRiggReport.Create;
begin
  FML := TStringList.Create;
  IndexAuswahlL := [0,6,8,10,11,13,14];
  IndexAuswahlP := [ooA..ooF];
end;

destructor TRiggReport.Destroy;
begin
  FML.Free;
end;

procedure TRiggReport.AusgabeRL(rL: TRiggRods);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längen belastet in mm (Vektor rL):');
    PrintUnderline;
    for i := 0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rL[%2d] %10.3f  (%s)',
      [i, rL.V[i], AbstandLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeRLE(rLe: TRiggRods);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längen entlastet in mm (Vektor rLe):');
    PrintUnderline;
    for i:=0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rLe[%2d]  %10.3f  (%s)',
      [i, rLe.V[i], AbstandLabels[i]]));
    end;
    PrintUnderlineE;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeDiffL(rL, rLe: TRiggRods);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längenänderungen in mm  (rLe[i]-rL[i]):');
    PrintUnderline;
    for i:=0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  %2d %10.3f  (%s)',
      [i, (rLe.V[i]-rL.V[i]), AbstandLabels[i]]));
    end;
    PrintUnderlineE;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeRP(rP: TRiggPoints);
var
  i: TRiggPoint;
begin
  with FML do
  begin
    Add('  Koordinaten belastet in mm (Vektor rP):');
    Add(Format
       ('  rP[%2s] %8s %8s %8s', ['i ','x','y','z']));
    PrintUnderline;
    for i := ooA0 to ooP do
    begin
      if i in IndexAuswahlP then
      Add(Format('  rP[%s] %8.2f %8.2f %8.2f  (%s)',
      [KoordTexte[i], rP.V[i].X, rP.V[i].Y, rP.V[i].Z, KoordLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeRPE(rPe: TRiggPoints);
var
  i: TRiggPoint;
begin
  with FML do
  begin
    Add('  Koordinaten entlastet in mm (Vektor rPe):');
    Add(Format
       ('  rPe[%2s] %8s %8s %8s', ['i ','x','y','z']));
    PrintUnderline;
    for i := ooA0 to ooP do
    begin
      if i in IndexAuswahlP then
      Add(Format('  rPe[%s] %8.2f %8.2f %8.2f  (%s)',
      [KoordTexte[i], rPe.V[i].X, rPe.V[i].Y, rPe.V[i].Z, KoordLabels[i]]));
    end;
    PrintUnderlineE;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeDiffP(rP, rPe: TRiggPoints);
var
  i: TRiggPoint;
begin
  with FML do
  begin
    Add('  Punktverschiebungen in mm (rPe[i]-rP[i]):');
    Add(Format('  %2s  %8s %8s %8s', ['i ','x','y','z']));
    PrintUnderline;
    for i := ooA0 to ooP do
    begin
      if i in IndexAuswahlP then
      Add(Format('  %s  %8.2f %8.2f %8.2f  (%s)',
      [KoordTexte[i], rPe.V[i].X-rP.V[i].X, rPe.V[i].Y-rP.V[i].Y, rPe.V[i].Z-rP.V[i].Z,
      KoordLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeRF(rF: TRiggRods);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Kräfte in N (Vektor rF):');
    PrintUnderline;
    for i := 0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rF[%2d] %10.0f  (%s)',
      [Ord(i), rF.V[i], AbstandLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeAngle(Value: TRiggAngles);
begin
  with Value do
    AusgabeWinkel(
      alpha, alpha1, alpha2, beta, gamma,
      delta1, delta2, epsilon, phi, psi);
end;

procedure TRiggReport.AusgabeWinkel(alpha, alpha1, alpha2, beta, gamma,
  delta1, delta2, epsilon, phi, psi: single);
var
  t: single;
begin
  t := 180 / pi;
  with FML do
  begin
    Add('  Winkel:');
    PrintUnderline;
    Add(Format('%s phi = %4.2f Grad', [ReportConst.LR, phi * t]));
    Add(Format('%s psi = %4.2f Grad', [ReportConst.LR, psi * t]));
    Add(Format('%s alpha = %4.2f Grad', [ReportConst.LR, alpha * t]));
    Add(Format('%s phi-alpha = %4.2f Grad', [ReportConst.LR, (phi-alpha) * t]));
    Add(Format('%s psi-alpha = %4.2f Grad', [ReportConst.LR, (psi-alpha) * t]));
    Add(Format('%s alpha1 = %4.2f Grad', [ReportConst.LR, alpha1 * t]));
    Add(Format('%s alpha2 = %4.2f Grad', [ReportConst.LR, alpha2 * t]));
    Add(Format('%s delta1 = %4.2f Grad', [ReportConst.LR, delta1 * t]));
    Add(Format('%s delta2 = %4.2f Grad', [ReportConst.LR, delta2 * t]));
    Add(Format('%s gamma = %4.2f Grad', [ReportConst.LR, gamma * t]));
    Add(Format('%s beta = %4.2f Grad', [ReportConst.LR, beta * t]));
    Add(Format('%s epsilon = %4.2f Grad', [ReportConst.LR, epsilon * t]));
    Add('');
  end;
end;

procedure TRiggReport.AusgabeTrimmControls(Ctrls: TTrimmControls);
begin
  with FML do
  begin
    Add('  Einstellungen (TTrimmControls):');
    PrintUnderline;
    Add(Format('%s Controller = %d mm', [ReportConst.LR, Ctrls.Controller]));
    Add(Format('%s Winkel = %d 10E-1 Grad', [ReportConst.LR, Ctrls.Winkel]));
    Add(Format('%s Vorstag = %d mm', [ReportConst.LR, Ctrls.Vorstag]));
    Add(Format('%s Wanten = %d mm', [ReportConst.LR, Ctrls.Wanten]));
    Add(Format('%s Wunten = %d mm', [ReportConst.LR, Ctrls.Wanten-Ctrls.Woben]));
    Add(Format('%s Woben = %d mm', [ReportConst.LR, Ctrls.Woben]));
    Add(Format('%s SalingH = %d mm', [ReportConst.LR, Ctrls.SalingH]));
    Add(Format('%s SalingA = %d mm', [ReportConst.LR, Ctrls.SalingA]));
    Add(Format('%s SalingL = %d mm', [ReportConst.LR, Ctrls.SalingL]));
    Add('');
  end;
end;

procedure TRiggReport.AusgabeSalingDaten(SData: TSalingDaten);
begin
  with FML do
  begin
    Add('  Salinge (TSalingDaten):');
    PrintUnderline;
    Add(Format('%s SalingH = %6.2f mm', [ReportConst.LR, SData.SalingH]));
    Add(Format('%s SalingA = %6.2f mm', [ReportConst.LR, SData.SalingA]));
    Add(Format('%s SalingL = %6.2f mm', [ReportConst.LR, SData.SalingL]));
    Add(Format('%s SalingW = %6.2f Grad', [ReportConst.LR, SData.SalingW]));
    Add(Format('%s WantenWinkel = %6.2f Grad',
    [ReportConst.LR, SData.WantenWinkel]));
    Add(Format('%s KraftWinkel = %6.2f Grad',
    [ReportConst.LR, SData.KraftWinkel]));
    Add('');
  end;
end;

procedure TRiggReport.AusgabeLog(Liste: TStringList);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Log:');
    PrintUnderline;
    for i := 0 to Liste.Count-1 do
    begin
      Add(Format('%s %s', [ReportConst.LR, Liste[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.PrintUnderlineE;
begin
  if not SofortFlag then
  FML.Add('  ---- ( updated only in mode SofortBerechnen ) ---');
end;

procedure TRiggReport.PrintUnderline;
begin
  FML.Add('  -------------------------------------------------');
end;

end.
