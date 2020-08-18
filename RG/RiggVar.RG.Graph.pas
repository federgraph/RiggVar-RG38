unit RiggVar.RG.Graph;

interface

uses
  System.Math.Vectors,
  RggTypes,
  RggUnit4;

type
  IStrokeRigg = interface
  ['{6BEF1811-8B39-42C7-B04A-694343D7B27C}']
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetControllerTyp(const Value: TControllerTyp);

    procedure SetKoordinaten(const Value: TRealRiggPoints);
    procedure SetKoordinatenE(const Value: TRealRiggPoints);
    procedure SetKoordinatenR(const Value: TRealRiggPoints);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    procedure SetKoppelKurve(const Value: TKoordLine);

    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetKoppel(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);

    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);

    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetGrauZeichnen(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);
    procedure SetSofortBerechnen(const Value: Boolean);

    function GetMastKurvePoint(const Index: Integer): TPoint3D;
    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;

    procedure Draw;

    property Koordinaten: TRealRiggPoints write SetKoordinaten;
    property KoordinatenE: TRealRiggPoints write SetKoordinatenE;
    property KoordinatenR: TRealRiggPoints write SetKoordinatenR;
    property KoppelKurve: TKoordLine write SetKoppelKurve;
    property MastKurve: TMastKurve write SetMastKurve;
    property WanteGestrichelt: Boolean write SetWanteGestrichelt;
    property Bogen: Boolean write SetBogen;
    property Koppel: Boolean write SetKoppel;
    property HullVisible: Boolean write SetHullVisible;
    property FixPoint: TRiggPoint write SetFixPoint;
    property ViewPoint: TViewPoint write SetViewPoint;
    property SalingTyp: TSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp write SetControllerTyp;
    property RiggLED: Boolean write SetRiggLED;
    property SofortBerechnen: Boolean write SetSofortBerechnen;
    property GrauZeichnen: Boolean write SetGrauZeichnen;
    property BtnGrauDown: Boolean write SetBtnGrauDown;
    property BtnBlauDown: Boolean write SetBtnBlauDown;
  end;

  TDummyStrokeRigg = class(TInterfacedObject, IStrokeRigg)
  private
    FRigg: TRigg;
    FBogen: Boolean;
    FWanteGestrichelt: Boolean;
    FViewPoint: TViewPoint;
    FFixPoint: TRiggPoint;
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FHullVisible: Boolean;
    FBtnGrauDown: Boolean;
    FMastKurve: TMastKurve;
    FGrauZeichnen: Boolean;
    FKoppelKurve: TKoordLine;
    FKoordinaten: TRealRiggPoints;
    FBtnBlauDown: Boolean;
    FRiggLED: Boolean;
    FSofortBerechnen: Boolean;
    FKoordinatenR: TRealRiggPoints;
    FKoordinatenE: TRealRiggPoints;
    FKoppel: Boolean;
    procedure SetBogen(const Value: Boolean);
    procedure SetWanteGestrichelt(const Value: Boolean);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetGrauZeichnen(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure SetKoordinatenE(const Value: TRealRiggPoints);
    procedure SetKoordinatenR(const Value: TRealRiggPoints);
    procedure SetKoppel(const Value: Boolean);
  public
    WantRenderH: Boolean;
    WantRenderP: Boolean;
    WantRenderF: Boolean;
    WantRenderE: Boolean;
    WantRenderS: Boolean;

    constructor Create(rgg: TRigg);

    procedure SetKoordinaten(const Value: TRealRiggPoints);
    procedure SetKoppelKurve(const Value: TKoordLine);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    function GetMastKurvePoint(const Index: Integer): TPoint3D;

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;

    procedure Draw;

    property Koordinaten: TRealRiggPoints read FKoordinaten write SetKoordinaten;
    property KoordinatenE: TRealRiggPoints read FKoordinatenE write SetKoordinatenE;
    property KoordinatenR: TRealRiggPoints read FKoordinatenR write SetKoordinatenR;
    property KoppelKurve: TKoordLine read FKoppelKurve write SetKoppelKurve;
    property MastKurve: TMastKurve read FMastKurve write SetMastKurve;
    property WanteGestrichelt: Boolean read FWanteGestrichelt write SetWanteGestrichelt;
    property Bogen: Boolean read FBogen write SetBogen;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property HullVisible: Boolean read FHullVisible write SetHullVisible;
    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property RiggLED: Boolean read FRiggLED write SetRiggLED;
    property SofortBerechnen: Boolean read FSofortBerechnen write SetSofortBerechnen;
    property GrauZeichnen: Boolean read FGrauZeichnen write SetGrauZeichnen;
    property BtnGrauDown: Boolean read FBtnGrauDown write SetBtnGrauDown;
    property BtnBlauDown: Boolean read FBtnBlauDown write SetBtnBlauDown;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

{ TStrokeRigg }

constructor TDummyStrokeRigg.Create(rgg: TRigg);
begin
  FRigg := rgg;
end;

procedure TDummyStrokeRigg.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: single;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - Beta);
  temp2 := cos(beta);
  temp3 := sin(pi / 2 - Beta);
  temp4 := sin(beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    FMastKurve[j].X := FRigg.rP[ooD0].X - tempL * temp1 + Value[k] * temp2;
    FMastKurve[j].Y := 0;
    FMastKurve[j].Z := FRigg.rP[ooD0].Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TDummyStrokeRigg.SetRiggLED(const Value: Boolean);
begin
  FRiggLED := Value;
end;

procedure TDummyStrokeRigg.SetMastKurve(const Value: TMastKurve);
begin
  FMastKurve := Value;
end;

procedure TDummyStrokeRigg.SetSalingTyp(const Value: TSalingTyp);
begin
  FSalingTyp := Value;
end;

procedure TDummyStrokeRigg.SetSofortBerechnen(const Value: Boolean);
begin
  FSofortBerechnen := Value;
end;

procedure TDummyStrokeRigg.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
end;

procedure TDummyStrokeRigg.SetWanteGestrichelt(const Value: Boolean);
begin
  FWanteGestrichelt := Value;
end;

procedure TDummyStrokeRigg.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
end;

procedure TDummyStrokeRigg.SetBtnBlauDown(const Value: Boolean);
begin
  FBtnBlauDown := Value;
end;

procedure TDummyStrokeRigg.SetBtnGrauDown(const Value: Boolean);
begin
  FBtnGrauDown := Value;
end;

procedure TDummyStrokeRigg.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
end;

procedure TDummyStrokeRigg.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
end;

procedure TDummyStrokeRigg.SetGrauZeichnen(const Value: Boolean);
begin
  FGrauZeichnen := Value;
end;

procedure TDummyStrokeRigg.SetHullVisible(const Value: Boolean);
begin
  FHullVisible := Value;
end;

procedure TDummyStrokeRigg.SetKoordinaten(const Value: TRealRiggPoints);
begin
  FKoordinaten := Value;
end;

procedure TDummyStrokeRigg.SetKoordinatenE(const Value: TRealRiggPoints);
begin
  FKoordinatenE := Value;
end;

procedure TDummyStrokeRigg.SetKoordinatenR(const Value: TRealRiggPoints);
begin
  FKoordinatenR := Value;
end;

procedure TDummyStrokeRigg.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
end;

procedure TDummyStrokeRigg.SetKoppelKurve(const Value: TKoordLine);
begin
  // not implemented
end;

procedure TDummyStrokeRigg.ToggleRenderOption(const fa: Integer);
begin
  case fa of
    faWantRenderH: WantRenderH := not WantRenderH;
    faWantRenderP: WantRenderP := not WantRenderP;
    faWantRenderF: WantRenderF := not WantRenderF;
    faWantRenderE: WantRenderE := not WantRenderE;
    faWantRenderS: WantRenderS := not WantRenderS;
  end;
end;

procedure TDummyStrokeRigg.Draw;
begin

end;

function TDummyStrokeRigg.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(FMastKurve)) then
    result := FMastKurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TDummyStrokeRigg.QueryRenderOption(const fa: Integer): Boolean;
begin
  case fa of
    faWantRenderH: result := WantRenderH;
    faWantRenderP: result := WantRenderP;
    faWantRenderF: result := WantRenderF;
    faWantRenderE: result := WantRenderE;
    faWantRenderS: result := WantRenderS;
    else
      result := False;
  end;
end;

end.
