unit RggZug;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math.Vectors,
  FMX.Graphics,
  RggTypes,
  RggDisplay;

type
  TRaumGraphData = class
  public
    xA0, xB0, xC0, xD0, xE0, xF0, xA, xB, xC, xD, xE, xF: single;
    yA0, yB0, yC0, yD0, yE0, yF0, yA, yB, yC, yD, yE, yF: single;
    zA0, zB0, zC0, zD0, zE0, zF0, zA, zB, zC, zD, zE, zF: single;

    xP0, yP0: single;
    xX, yX: single;
    xY, yY: single;
    xZ, yZ: single;
    xM, yM: single;
    xN, yN: single;
    xP, yP: single;
  end;

  TRaumGraphProps = class
  public
    SalingTyp: TSalingTyp;
    ControllerTyp: TControllerTyp;
    BogenIndexD: Integer;
    Bogen: Boolean;
    Coloriert: Boolean;
    Color: TAlphaColor;
    Koppel: Boolean;
    Gestrichelt: Boolean;
    RiggLED: Boolean;
  end;

  TZug0 = class
  protected
    PD: TPathData;
  public
    Data: TRaumGraphData; // injected
    Props: TRaumGraphProps; // injected
    constructor Create;
    destructor Destroy; override;
  end;

  TZug3DBase = class(TZug0)
  public
    ZugRumpf: TPolygon;
    ZugMast: TPolygon;
    ZugMastKurve: TPolygon;
    ZugSalingFS: TPolygon;
    ZugSalingDS: TPolygon;
    ZugWanteStb: TPolygon;
    ZugWanteBb: TPolygon;
    ZugController: TPolygon;
    ZugVorstag: TPolygon;
    ZugKoppelKurve: TPolygon;
    ZugAchsen: TPolygon;
    ZugMastfall: TPolygon;
    ZugRP: TPolygon;

    { no need to call SetLength for these, will be copied via Copy }
    ZugMastKurveD0D: TPolygon;
    ZugMastKurveDC: TPolygon;

    constructor Create;
    procedure FillZug; virtual; abstract;
    procedure DrawToCanvas(g: TCanvas); virtual; abstract;
    procedure GetPlotList(ML: TStrings); virtual;
  end;

implementation

uses
  RiggVar.RG.Def;

{ TZug0 }

constructor TZug0.Create;
begin
  PD := TPathData.Create;
end;

destructor TZug0.Destroy;
begin
  PD.Free;
  inherited;
end;

{ TZug3DBase }

constructor TZug3DBase.Create;
begin
  inherited;
  SetLength(ZugRumpf, 8);
  SetLength(ZugMast, 4);
  SetLength(ZugMastKurve, BogenMax + 2);
  SetLength(ZugSalingFS, 4);
  SetLength(ZugSalingDS, 3);
  SetLength(ZugWanteStb, 3);
  SetLength(ZugWanteBb, 3);
  SetLength(ZugController, 2);
  SetLength(ZugVorstag, 2);
  SetLength(ZugAchsen, 4);
  SetLength(ZugMastfall, 3);
  SetLength(ZugRP, 4);
  SetLength(ZugKoppelKurve, 101);
//  SetLength(ZugMastKurveD0D, ...);
//  SetLength(ZugMastKurveDC, ...);
end;

procedure TZug3DBase.GetPlotList(ML: TStrings);
begin

end;

end.
