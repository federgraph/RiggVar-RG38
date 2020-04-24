unit RggGraph;

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  RggTypes,
  RggZug,
  RggTransformer;

type
  TRggGraph = class
  protected
    FColor: TAlphaColor;
    FColored: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetColored(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetZoom(Value: single);
    function GetFixPoint: TRiggPoint;
    function GetZoom: single;
  protected
    GrafikOK: Boolean; // loaded with data
    Updated: Boolean; // transformed
    KoppelKurveNeedFill: Boolean;
  public
    RaumGraphData: TRaumGraphData;
    RaumGraphProps: TRaumGraphProps;

    Transformer: TRggTransformer; // injected, not owned

    constructor Create;
    destructor Destroy; override;

    procedure Update; virtual;
    procedure DrawToCanvas(Canvas: TCanvas); virtual;
    procedure GetPlotList(ML: TStrings); virtual;

    property FixPoint: TRiggPoint read GetFixPoint write SetFixPoint;
    property Zoom: single read GetZoom write SetZoom;
    property Coloriert: Boolean read FColored write SetColored;
    property Color: TAlphaColor read FColor write SetColor;
  end;

implementation

constructor TRggGraph.Create;
begin
  RaumGraphData := TRaumGraphData.Create;
  RaumGraphProps := TRaumGraphProps.Create;
  FColor := claGray;
  FColored := True;
end;

destructor TRggGraph.Destroy;
begin
  RaumGraphData.Free;
  RaumGraphProps.Free;
  inherited;
end;

procedure TRggGraph.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  RaumGraphProps.Color := Value;
end;

procedure TRggGraph.SetColored(const Value: Boolean);
begin
  FColored := Value;
  RaumGraphProps.Coloriert := FColored;
end;

procedure TRggGraph.SetFixPoint(const Value: TRiggPoint);
begin
  Transformer.FixPoint := Value;
  Updated := False;
end;

procedure TRggGraph.SetZoom(Value: single);
begin
  Transformer.Zoom := Value;
  Updated := False;
  KoppelKurveNeedFill := True;
end;

procedure TRggGraph.Update;
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.DrawToCanvas(Canvas: TCanvas);
begin
  //if GrafikOK then ...
  //virtual
end;

function TRggGraph.GetFixPoint: TRiggPoint;
begin
  result := Transformer.FixPoint;
end;

procedure TRggGraph.GetPlotList(ML: TStrings);
begin
  //if GrafikOK then ...
  //virtual
end;

function TRggGraph.GetZoom: single;
begin
  result := Transformer.Zoom;
end;

end.
