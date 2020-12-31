unit RiggVar.FG.StrokeGraph;

interface

uses
  System.Math,
  System.Math.Vectors,
  System.SysUtils,
  System.Classes,
  System.RTLConsts,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Materials,
  FMX.MaterialSources,
  FMX.Types,
  FMX.Objects3D,
  RiggVar.RG.Graph,
  RiggVar.RG.Types,
  RiggVar.RG.Calc,
  RiggVar.FG.Objects;

type
  TStab = class(TStrokeLine)
  private
    o1, o2: TPoint3D;
  public
    oo1: TRiggPoint;
    oo2: TRiggPoint;
    procedure Update; override;
  end;

  TStrokeRigg = class(TControl3D, IStrokeRigg)
  private
    Ver2: TVertexBuffer;
    Idx2: TIndexBuffer;

    Ver3: TVertexBuffer;
    Idx3: TIndexBuffer;

    Ver4: TVertexBuffer;
    Idx4: TIndexBuffer;

    Ver51: TVertexBuffer;
    Idx51: TIndexBuffer;

    MatVorstag: TColorMaterial;
    MatWanteStb: TColorMaterial;
    MatWanteBb: TColorMaterial;
    MatGray: TColorMaterial;
    MatRumpf: TColorMaterial;
    MatController: TColorMaterial;
    MatSaling: TColorMaterial;
    MatMast: TColorMaterial;
    MatFall: TColorMaterial;

    Opacity: single;

    o: TPoint3D;

    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FWanteGestrichelt: Boolean;
    FBogen: Boolean;
    FColor: TColor;

    KugelA0: TCustomMesh;
    KugelA: TCustomMesh;

    KugelB0: TCustomMesh;
    KugelB: TCustomMesh;

    KugelC0: TCustomMesh;
    KugelC: TCustomMesh;

    KugelD0: TCustomMesh;
    KugelD: TCustomMesh;

    KugelE0: TCustomMesh;
    KugelE: TCustomMesh;

    KugelN0: TCustomMesh;
    KugelF0: TCustomMesh;
    KugelF: TCustomMesh;

    KugelP0: TCustomMesh;
    KugelP: TCustomMesh;

    KugelM: TCustomMesh;

    KugelFP: TCustomMesh;

    Vorstag: TStab;
    WanteA0A: TStab;
    WanteAC: TStab;
    WanteB0B: TStab;
    WanteBC: TStab;

    SalingAD: TStab;
    SalingBD: TStab;

    cmsKugelA: TColorMaterialSource;
    cmsKugelB: TColorMaterialSource;
    cmsKugelC: TColorMaterialSource;
    cmsKugelD: TColorMaterialSource;
    cmsKugelE: TColorMaterialSource;
    cmsKugelF: TColorMaterialSource;
    cmsKugelM: TColorMaterialSource;
    cmsKugelP: TColorMaterialSource;
    cmsKugelN: TLightMaterialSource;

    cmsVorstag: TColorMaterialSource;
    cmsWante: TColorMaterialSource;
    cmsSaling: TColorMaterialSource;
    cmsGray: TColorMaterialSource;

    FWantRenderH: Boolean;
    FWantRenderP: Boolean;
    FWantRenderF: Boolean;
    FWantRenderE: Boolean;
    FWantRenderS: Boolean;
    FViewPoint: TViewPoint;
    FFixPoint: TRiggPoint;
    FBtnGrauDown: Boolean;
    FBtnBlauDown: Boolean;
    FRiggLED: Boolean;
    FSofortBerechnen: Boolean;
    FHullVisible: Boolean;
    FGrauZeichnen: Boolean;
    FKoppel: Boolean;

    procedure SetKoordinaten(const Value: TRiggPoints);
    procedure SetWanteGestrichelt(const Value: Boolean);

    procedure DrawPolygon2(p0, p1: TPoint3D; Mat: TColorMaterial);
    procedure DrawPolygon3(p0, p1, p2: TPoint3D; Mat: TColorMaterial);
    procedure DrawPolygon4(p0, p1, p2, p3: TPoint3D; Mat: TColorMaterial);
    procedure DrawPolygon51;

    function Kugel(
      AParent: TFMXObject;
      ms: TMaterialSource;
      oo: TRiggPoint): TCustomMesh;

    function Stab(
      AParent: TFMXObject;
      ms: TMaterialSource;
      oo1, oo2: TRiggPoint;
      adim: single): TStab;

    function GetPoint3D(oo: TRiggPoint): TPoint3D;
    procedure SetSalingTyp(const Value: TSalingTyp);
    procedure SetWantRenderP(const Value: Boolean);
    procedure SetWantRenderF(const Value: Boolean);
    procedure SetWantRenderH(const Value: Boolean);
    procedure SetWantRenderE(const Value: Boolean);
    procedure SetWantRenderS(const Value: Boolean);
    procedure SetBogen(const Value: Boolean);
    procedure SetControllerTyp(const Value: TControllerTyp);
    procedure SetFixPoint(const Value: TRiggPoint);
    procedure SetViewPoint(const Value: TViewPoint);
    function GetKoordinaten: TRiggPoints;
    function GetKoordinatenE: TRiggPoints;
    procedure SetKoordinatenE(const Value: TRiggPoints);
    function GetKoordinatenR: TRiggPoints;
    procedure SetKoordinatenR(const Value: TRiggPoints);
    procedure SetBtnBlauDown(const Value: Boolean);
    procedure SetBtnGrauDown(const Value: Boolean);
    procedure SetHullVisible(const Value: Boolean);
    procedure SetRiggLED(const Value: Boolean);
    procedure SetSofortBerechnen(const Value: Boolean);
    procedure SetGrauZeichnen(const Value: Boolean);
    procedure SetKoppel(const Value: Boolean);
  protected
    InitOK: Boolean;
    GrafikOK: Boolean;

    BogenSize: Integer;

    procedure Render; override;
  public
    Counter: Integer;

    KugelDim: single;
    WanteDim: single;
    SalingDim: single;
    VorstagDim: single;
    MastDim: single;

    WantStab: Boolean;
    WantKugel: Boolean;
    WantSalingStab: Boolean;
    WantMastProfile: Boolean;

    class var
    rP: TRiggPoints;
    Kurve: TMastKurve;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(Dummy: TDummy);

    procedure HideAll;
    procedure ShowAgain;

    procedure UpdateOffset;
    procedure UpdateScene;

    function GetMastKurvePoint(const Index: Integer): TPoint3D;
    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
    procedure SetMastKurve(const Value: TMastKurve);
    procedure SetKoppelKurve(const Value: TKoordLine);

    procedure ToggleRenderOption(const fa: Integer);
    function QueryRenderOption(const fa: Integer): Boolean;
    procedure UpdateHullTexture;
    procedure UpdateCameraX(Delta: single);
    procedure UpdateCameraY(Delta: single);

    procedure Draw;
    procedure DoOnUpdateStrokeRigg;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp read FControllerTyp write SetControllerTyp;
    property Koordinaten: TRiggPoints read GetKoordinaten write SetKoordinaten;
    property KoordinatenE: TRiggPoints read GetKoordinatenE write SetKoordinatenE;
    property KoordinatenR: TRiggPoints read GetKoordinatenR write SetKoordinatenR;

    property WanteGestrichelt: Boolean read FWanteGestrichelt write SetWanteGestrichelt;
    property Bogen: Boolean read FBogen write SetBogen;
    property Koppel: Boolean read FKoppel write SetKoppel;
    property Farbe: TColor read FColor write FColor;

    property WantRenderH: Boolean read FWantRenderH write SetWantRenderH;
    property WantRenderP: Boolean read FWantRenderP write SetWantRenderP;
    property WantRenderF: Boolean read FWantRenderF write SetWantRenderF;
    property WantRenderE: Boolean read FWantRenderE write SetWantRenderE;
    property WantRenderS: Boolean read FWantRenderS write SetWantRenderS;

    property IsInitOK: Boolean read InitOK;

    property FixPoint: TRiggPoint read FFixPoint write SetFixPoint;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;

    property HullVisible: Boolean read FHullVisible write SetHullVisible;
    property BtnBlauDown: Boolean read FBtnBlauDown write SetBtnBlauDown;
    property BtnGrauDown: Boolean read FBtnGrauDown write SetBtnGrauDown;
    property RiggLED: Boolean read FRiggLED write SetRiggLED;
    property SofortBerechnen: Boolean read FSofortBerechnen write SetSofortBerechnen;
    property GrauZeichnen: Boolean read FGrauZeichnen write SetGrauZeichnen;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FG.Rota,
  RiggVar.FB.ActionConst;

{ TStab }

procedure TStab.Update;
begin
  o1 := TStrokeRigg.rP.V[oo1];
  o2 := TStrokeRigg.rP.V[oo2];

  StartPoint := (o1 - TRotaForm3.RggGlobalOffset) * (1.0 / TRotaForm3.RggGlobalScale);
  EndPoint := (o2 - TRotaForm3.RggGlobalOffset) * (1.0 / TRotaForm3.RggGlobalScale);

  inherited;
end;

{ TStrokeRigg }

constructor TStrokeRigg.Create(AOwner: TComponent);
begin
  inherited;

  WantKugel := True;
  WantStab := True;
  WantSalingStab := True;
  WantMastProfile := False;

  KugelDim := 100;
  VorstagDim := 20;
  WanteDim := 20;
  SalingDim := 30;
  MastDim := 50;

  Opacity := 1.0;

  FSalingTyp := stFest;
  FControllerTyp := ctDruck;
  FFixPoint := ooD;
  FViewPoint := vp3D;

  BogenSize := BogenMax + 2;
  {
  BogenSize := BogenMax + 1 + 1;
  BogenSize := last array index + 1 + additional point F
  BogenSize := 52; //<-- actual value
  }

  Ver2 := TVertexBuffer.Create([TVertexFormat.Vertex], 2);
  Idx2 := TIndexBuffer.Create(2);

  Ver3 := TVertexBuffer.Create([TVertexFormat.Vertex], 3);
  Idx3 := TIndexBuffer.Create(4);

  Ver4 := TVertexBuffer.Create([TVertexFormat.Vertex], 4);
  Idx4 := TIndexBuffer.Create(6);

  Ver51 := TVertexBuffer.Create([TVertexFormat.Vertex], BogenSize); //52
  Idx51 := TIndexBuffer.Create((BogenSize - 1) * 2); //102

  MatFall := TColorMaterial.Create;
  MatFall.Color := claYellow;

  MatVorstag := TColorMaterial.Create;
  MatVorstag.Color := claYellow;

  MatWanteStb := TColorMaterial.Create;
  MatWanteStb.Color := claGreen;
  MatWanteBb := TColorMaterial.Create;
  MatWanteBb.Color := claRed;
  MatGray := TColorMaterial.Create;
  MatGray.Color := claGray;

  MatRumpf := TColorMaterial.Create;
  MatRumpf.Color := claGray;

  MatController := TColorMaterial.Create;
  MatController.Color := claAqua;

  MatSaling := TColorMaterial.Create;
  MatSaling.Color := claLime;

  MatMast := TColorMaterial.Create;
  MatMast.Color := claAqua;
end;

destructor TStrokeRigg.Destroy;
begin
  Ver2.Free;
  Idx2.Free;

  Ver3.Free;
  Idx3.Free;

  Ver4.Free;
  Idx4.Free;

  Ver51.Free;
  Idx51.Free;

  MatFAll.Free;
  MatVorstag.Free;
  MatWanteStb.Free;
  MatWanteBb.Free;
  MatGray.Free;
  MatRumpf.Free;
  MatController.Free;
  MatSaling.Free;
  MatMast.Free;

  inherited;
end;

procedure TStrokeRigg.DoOnUpdateStrokeRigg;
begin

end;

procedure TStrokeRigg.SetBogen(const Value: Boolean);
begin
  FBogen := Value;
end;

procedure TStrokeRigg.SetBtnBlauDown(const Value: Boolean);
begin
  FBtnBlauDown := Value;
end;

procedure TStrokeRigg.SetBtnGrauDown(const Value: Boolean);
begin
  FBtnGrauDown := Value;
end;

procedure TStrokeRigg.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
end;

procedure TStrokeRigg.SetFixPoint(const Value: TRiggPoint);
begin
  FFixPoint := Value;
end;

procedure TStrokeRigg.SetGrauZeichnen(const Value: Boolean);
begin
  FGrauZeichnen := Value;
end;

procedure TStrokeRigg.SetHullVisible(const Value: Boolean);
begin
  FHullVisible := Value;
end;

procedure TStrokeRigg.SetKoordinaten(const Value: TRiggPoints);
begin
  TStrokeRigg.rP := Value;
  GrafikOK := True;
end;

procedure TStrokeRigg.SetKoordinatenE(const Value: TRiggPoints);
begin

end;

procedure TStrokeRigg.SetKoordinatenR(const Value: TRiggPoints);
begin

end;

procedure TStrokeRigg.SetKoppel(const Value: Boolean);
begin
  FKoppel := Value;
end;

procedure TStrokeRigg.SetKoppelKurve(const Value: TKoordLine);
begin
  { not implemented }
end;

procedure TStrokeRigg.SetWanteGestrichelt(const Value: Boolean);
begin
  FWanteGestrichelt := Value;
end;

procedure TStrokeRigg.SetWantRenderH(const Value: Boolean);
begin
  FWantRenderH := Value;
end;

procedure TStrokeRigg.SetWantRenderE(const Value: Boolean);
begin
  FWantRenderE := Value;
  KugelE0.Visible := Value;
  KugelE.Visible := Value;
end;

procedure TStrokeRigg.SetWantRenderP(const Value: Boolean);
begin
  FWantRenderP := Value;
  KugelP0.Visible := Value;
  KugelP.Visible := Value;
end;

procedure TStrokeRigg.SetWantRenderF(const Value: Boolean);
begin
  FWantRenderF := Value;
end;

procedure TStrokeRigg.SetWantRenderS(const Value: Boolean);
begin
  FWantRenderS := Value;

  Vorstag.Visible := Value;
  WanteA0A.Visible := Value;
  WanteAC.Visible := Value;
  WanteB0B.Visible := Value;
  WanteBC.Visible := Value;

  SalingAD.Visible := Value;
  SalingBD.Visible := Value;
end;

procedure TStrokeRigg.SetMastKurve(const Value: TMastKurve);
begin
  Kurve := Value;
end;

procedure TStrokeRigg.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
var
  temp1, temp2, temp3, temp4, tempL: double;
  j, k: Integer;
begin
  temp1 := cos(pi / 2 - Beta);
  temp2 := cos(Beta);
  temp3 := sin(pi / 2 - Beta);
  temp4 := sin(Beta);
  for j := 0 to BogenMax do
  begin
    k := Round(100 / BogenMax * j);
    tempL := j * L / BogenMax;
    Kurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    Kurve[j].Y := 0;
    Kurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

procedure TStrokeRigg.SetRiggLED(const Value: Boolean);
begin
  FRiggLED := Value;
end;

procedure TStrokeRigg.SetSalingTyp(const Value: TSalingTyp);
begin
  FSalingTyp := Value;
  if WantSalingStab and Assigned(SalingAD) then
  begin
    if (Value = stOhneBiegt) or (Value = stOhneStarr) then
    begin
      SalingAD.Visible := False;
      SalingBD.Visible := False;
      KugelA.Visible := False;
      KugelB.Visible := False;
    end
    else
    begin
      SalingAD.Visible := True;
      SalingBD.Visible := True;
      KugelA.Visible := True;
      KugelB.Visible := True;
    end;
  end;
end;

procedure TStrokeRigg.SetSofortBerechnen(const Value: Boolean);
begin
  FSofortBerechnen := Value;
end;

procedure TStrokeRigg.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
end;

procedure TStrokeRigg.Draw;
begin
  UpdateOffset;
  UpdateScene;
end;

procedure TStrokeRigg.DrawPolygon2(p0, p1: TPoint3D; Mat: TColorMaterial);
begin
  Ver2.Vertices[0] := Point3D(p0.X-o.X, p0.Y-o.Y, p0.Z-o.Z);
  Ver2.Vertices[1] := Point3D(p1.X-o.X, p1.Y-o.Y, p1.Z-o.Z);
  Idx2[0] := 0;
  Idx2[1] := 1;
  Context.DrawLines(Ver2, Idx2, Mat, Opacity);
end;

procedure TStrokeRigg.DrawPolygon3(p0, p1, p2: TPoint3D; Mat: TColorMaterial);
begin
  Ver3.Vertices[0] := Point3D(p0.X-o.X, p0.Y-o.Y, p0.Z-o.Z);
  Ver3.Vertices[1] := Point3D(p1.X-o.X, p1.Y-o.Y, p1.Z-o.Z);
  Ver3.Vertices[2] := Point3D(p2.X-o.X, p2.Y-o.Y, p2.Z-o.Z);
  Idx3[0] := 0;
  Idx3[1] := 1;
  Idx3[2] := 1;
  Idx3[3] := 2;
  Context.DrawLines(Ver3, Idx3, Mat, Opacity);
end;

procedure TStrokeRigg.DrawPolygon4(p0, p1, p2, p3: TPoint3D; Mat: TColorMaterial);
begin
  Ver4.Vertices[0] := Point3D(p0.X-o.x, p0.Y-o.y, p0.Z-o.z);
  Ver4.Vertices[1] := Point3D(p1.X-o.x, p1.Y-o.y, p1.Z-o.z);
  Ver4.Vertices[2] := Point3D(p2.X-o.x, p2.Y-o.y, p2.Z-o.z);
  Ver4.Vertices[3] := Point3D(p3.X-o.x, p3.Y-o.y, p3.Z-o.z);
  Idx4[0] := 0;
  Idx4[1] := 1;
  Idx4[2] := 1;
  Idx4[3] := 2;
  Idx4[4] := 2;
  Idx4[5] := 3;
  Context.DrawLines(Ver4, Idx4, Mat, Opacity);
end;

procedure TStrokeRigg.DrawPolygon51;
var
  i, j: Integer;
begin
  for i := 0 to BogenMax do
    Ver51.Vertices[i] := Point3D(Kurve[i].X-o.x, Kurve[i].Y-o.y, Kurve[i].Z-o.z);
  Ver51.Vertices[BogenMax+1] := Point3D(rP.F.X-o.x, rP.F.Y-o.y, rP.F.Z-o.z);

  j := 0;
  Idx51[j] := 0;
  for i := 1 to BogenMax do
  begin
    j := 2 * i;
    Idx51[j-1] := i;
    Idx51[j] := i;
  end;
  Inc(j);
  Idx51[j] := Idx51[j-1] + 1;

  Context.DrawLines(Ver51, Idx51, MatMast, Opacity);
end;

function TStrokeRigg.Kugel(
  AParent: TFMXObject;
  ms: TMaterialSource;
  oo: TRiggPoint): TCustomMesh;
begin
  if ms = cmsKugelN then
    result := TCube.Create(Owner)
  else
    result := TSphere.Create(Owner);

//  result.Scale.X := 1;
//  result.Scale.Y := 1;
//  result.Scale.Z := 1;

  result.Width := KugelDim / TRotaForm3.RggGlobalScale;
  result.Height := KugelDim / TRotaForm3.RggGlobalScale;
  result.Depth := KugelDim / TRotaForm3.RggGlobalScale;

  result.MaterialSource := ms;
//  result.OnMouseDown := RotaForm3.Frame3D.ImageMouseDown;
//  result.OnMouseMove := RotaForm3.Frame3D.ImageMouseMove;
//  result.OnMouseWheel := Main.ViewFrame.ViewportMouseWheel;

  AParent.AddObject(result);
end;

function TStrokeRigg.QueryRenderOption(const fa: Integer): Boolean;
begin
  case fa of
    faRggHull: result := FHullVisible;
    faRggBogen: result := FBogen;
    faRggKoppel: result := FKoppel;
    faWantRenderH: result := WantRenderH;
    faWantRenderP: result := WantRenderP;
    faWantRenderF: result := WantRenderF;
    faWantRenderE: result := WantRenderE;
    faWantRenderS: result := WantRenderS;
    else result := False;
  end;
end;

function TStrokeRigg.Stab(
  AParent: TFMXObject;
  ms: TMaterialSource;
  oo1, oo2: TRiggPoint;
  adim: single): TStab;
begin
  result := TStab.Create(Owner);

  result.oo1 := oo1;
  result.oo2 := oo2;

  result.Width := 0.5 * adim  / TRotaForm3.RggGlobalScale;
  result.Height := 2.0 * result.Width;
  result.Depth := 1.0;

  result.MaterialShaftSource := ms;
//  result.OnMouseDown := RotaForm3.Frame3D.ImageMouseDown;
//  result.OnMouseMove := RotaForm3.Frame3D.ImageMouseMove;
{ result.OnMouseWheel := Main.Frame3D.ViewportMouseWheel; }

  AParent.AddObject(result);
end;

procedure TStrokeRigg.ToggleRenderOption(const fa: Integer);
begin
  case fa of
    faWantRenderH: WantRenderH := not WantRenderH;
    faWantRenderP: WantRenderP := not WantRenderP;
    faWantRenderF: WantRenderF := not WantRenderF;
    faWantRenderE: WantRenderE := not WantRenderE;
    faWantRenderS: WantRenderS := not WantRenderS;
  end;
end;

procedure TStrokeRigg.Init(Dummy: TDummy);
begin
  cmsKugelA := TColorMaterialSource.Create(Owner);
  cmsKugelA.Color := claGreen;
  cmsKugelB := TColorMaterialSource.Create(Owner);
  cmsKugelB.Color := claRed;
  cmsKugelC := TColorMaterialSource.Create(Owner);
  cmsKugelC.Color := claYellow;
  cmsKugelD := TColorMaterialSource.Create(Owner);
  cmsKugelD.Color := claBlue;
  cmsKugelE := TColorMaterialSource.Create(Owner);
  cmsKugelE.Color := claAqua;
  cmsKugelF := TColorMaterialSource.Create(Owner);
  cmsKugelF.Color := claLightGray;
  cmsKugelP := TColorMaterialSource.Create(Owner);
  cmsKugelP.Color := claGray;
  cmsKugelM := TColorMaterialSource.Create(Owner);
  cmsKugelM.Color := claOrange;

  cmsKugelN := TLightMaterialSource.Create(Owner);
  cmsKugelN.Ambient := claFuchsia;
//  cmsKugelN.Emissive := claFuchsia;

  cmsVorstag := TColorMaterialSource.Create(Owner);
  cmsVorstag.Color := claOrange;

  cmsWante := TColorMaterialSource.Create(Owner);
  cmsWante.Color := claCrimson;

  cmsSaling := TColorMaterialSource.Create(Owner);
  cmsSaling.Color := claLime;

//  cmsMast := TColorMaterialSource.Create(Owner);
//  cmsMast.Color := claBlue;

  cmsGray := TColorMaterialSource.Create(Owner);
  cmsGray.Color := claGray;

  if WantKugel then
  begin
    KugelN0 := Kugel(Dummy, cmsKugelF, ooN0);

    KugelA0 := Kugel(Dummy, cmsKugelA, ooA0);
    KugelB0 := Kugel(Dummy, cmsKugelB, ooB0);
    KugelC0 := Kugel(Dummy, cmsKugelC, ooC0);
    KugelD0 := Kugel(Dummy, cmsKugelD, ooD0);
    KugelE0 := Kugel(Dummy, cmsKugelE, ooE0);
    KugelF0 := Kugel(Dummy, cmsKugelF, ooF0);
    KugelP0 := Kugel(Dummy, cmsKugelP, ooP0);

    KugelA := Kugel(Dummy, cmsKugelA, ooA);
    KugelB := Kugel(Dummy, cmsKugelB, ooB);
    KugelC := Kugel(Dummy, cmsKugelC, ooC);
    KugelD := Kugel(Dummy, cmsKugelD, ooD);
    KugelE := Kugel(Dummy, cmsKugelE, ooE);
    KugelF := Kugel(Dummy, cmsKugelF, ooF);
    KugelP := Kugel(Dummy, cmsKugelP, ooP);

    KugelM := Kugel(Dummy, cmsKugelM, ooM);

    KugelFP := Kugel(Dummy, cmsKugelN, FixPoint);
    KugelFP.Width := KugelFP.Width * 0.8;
    KugelFP.Height := KugelFP.Height * 0.8;
    KugelFP.Depth := KugelFP.Depth * 0.8;
    //KugelFP.Opacity := 0.7;

    KugelE0.Visible := FWantRenderE;
    KugelE.Visible := FWantRenderE;
    KugelP0.Visible := FWantRenderP;
    KugelP.Visible := FWantRenderP;
  end;

  if WantStab then
  begin
    Vorstag := Stab(Dummy, cmsVorstag, ooC0, ooC, VorstagDim);
    Vorstag.Height := Vorstag.Height * 2;

    WanteA0A := Stab(Dummy, cmsWante, ooA0, ooA, WanteDim);
    WanteB0B := Stab(Dummy, cmsWante, ooB0, ooB, WanteDim);
    WanteAC := Stab(Dummy, cmsWante, ooA, ooC, WanteDim);
    WanteBC := Stab(Dummy, cmsWante, ooB, ooC, WanteDim);

    if WantSalingStab then
    begin
      SalingAD := Stab(Dummy, cmsSaling, ooA, ooD, SalingDim);
      SalingBD := Stab(Dummy, cmsSaling, ooB, ooD, SalingDim);
    end;
  end;

  InitOK := True;

  WantRenderH := True;
  WantRenderP := False;
  WantRenderF := True;
  WantRenderE := False;
  WantRenderS := False;
end;

function TStrokeRigg.GetKoordinaten: TRiggPoints;
begin
  result := rP;
end;

function TStrokeRigg.GetKoordinatenE: TRiggPoints;
begin

end;

function TStrokeRigg.GetKoordinatenR: TRiggPoints;
begin

end;

function TStrokeRigg.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(Kurve)) then
    result := Kurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TStrokeRigg.GetPoint3D(oo: TRiggPoint): TPoint3D;
begin
  result := Point3D(
    (rp.V[oo].X-o.x) / TRotaForm3.RggGlobalScale,
    (rp.V[oo].Y-o.y) / TRotaForm3.RggGlobalScale,
    (rp.V[oo].Z-o.z) / TRotaForm3.RggGlobalScale
    );
end;

procedure TStrokeRigg.Render;
begin
  { Rumpf }
  if WantRenderH then
  begin
  DrawPolygon4(
    rp.A0,
    rp.B0,
    rp.C0,
    rp.A0,
    MatRumpf);
  DrawPolygon3(
    rp.A0,
    rp.D0,
    rp.B0,
    MatRumpf);
  DrawPolygon2(
    rp.C0,
    rp.D0,
    MatRumpf);
   end;

   if WantRenderP then
   begin
    DrawPolygon4(
      rp.D0,
      rp.F0,
      rp.P0,
      rp.D0,
      MatRumpf);
    DrawPolygon2(
      rp.P0,
      rp.C0,
      MatRumpf);
    DrawPolygon3(
      rp.D0,
      rp.N0,
      rp.F0,
      MatRumpf);
    end;

   if WantRenderF then
   begin
    DrawPolygon2(
      rp.F0,
      rp.M,
      MatFall);
    DrawPolygon2(
      rp.M,
      rp.F,
      MatRumpf);
    end;

  { Salinge }
  if (SalingTyp = stOhneBiegt) or (SalingTyp = stOhneStarr) then
  begin

  end
  else if SalingTyp = stDrehbar then
    DrawPolygon3(
      rp.A,
      rp.D,
      rp.B,
      MatSaling)
  else if SalingTyp = stFest then
    DrawPolygon4(
      rp.A,
      rp.D,
      rp.B,
      rp.A,
      MatSaling);

  { Mast }
  if FBogen then
    DrawPolygon51
  else
    DrawPolygon4(
      rp.D0,
      rp.D,
      rp.C,
      rp.F,
      MatMast);

  { Wanten }
  if not WanteGestrichelt then
  begin
    DrawPolygon3(
      rp.A0,
      rp.A,
      rp.C,
      MatWanteStb);
    DrawPolygon3(
      rp.B0,
      rp.B,
      rp.C,
      MatWanteBb);
  end
  else
  begin
    DrawPolygon3(
      rp.A0,
      rp.A,
      rp.C,
      MatGray);
    DrawPolygon3(
      rp.B0,
      rp.B,
      rp.C,
      MatGray);
  end;

  { Controller }
  if ControllerTyp <> ctOhne then
  begin
    DrawPolygon2(
      rp.E0,
      rp.E,
      MatController);
  end;

  { Vorstag }
  DrawPolygon2(
    rp.C0,
    rp.C,
    MatVorstag);

end;

procedure TStrokeRigg.UpdateScene;
begin
  if InitOK and GrafikOK then
  begin
    if WantKugel then
    begin
      KugelN0.Position.Point := GetPoint3D(ooN0);

      KugelA0.Position.Point := GetPoint3D(ooA0);
      KugelB0.Position.Point := GetPoint3D(ooB0);
      KugelC0.Position.Point := GetPoint3D(ooC0);
      KugelD0.Position.Point := GetPoint3D(ooD0);
      KugelF0.Position.Point := GetPoint3D(ooF0);

      KugelA.Position.Point := GetPoint3D(ooA);
      KugelB.Position.Point := GetPoint3D(ooB);
      KugelC.Position.Point := GetPoint3D(ooC);
      KugelD.Position.Point := GetPoint3D(ooD);
      KugelF.Position.Point := GetPoint3D(ooF);

      KugelP0.Position.Point := GetPoint3D(ooP0);
      KugelP.Position.Point := GetPoint3D(ooP);

      KugelE0.Position.Point := GetPoint3D(ooE0);
      KugelE.Position.Point := GetPoint3D(ooE);

      KugelM.Position.Point := GetPoint3D(ooM);

      KugelFP.Position.Point := GetPoint3D(FixPoint);
    end;

    if WantStab then
    begin
      Vorstag.Update;
      WanteA0A.Update;
      WanteAC.Update;
      WanteB0B.Update;
      WanteBC.Update;
      if WantSalingStab then
      begin
        SalingAD.Update;
        SalingBD.Update;
      end;

      if not WanteGestrichelt then
      begin
        WanteA0A.MaterialShaftSource := cmsWante;
        WanteAC.MaterialShaftSource := cmsWante;
        WanteB0B.MaterialShaftSource := cmsWante;
        WanteBC.MaterialShaftSource := cmsWante;
      end
      else
      begin
        WanteA0A.MaterialShaftSource := cmsGray;
        WanteAC.MaterialShaftSource := cmsGray;
        WanteB0B.MaterialShaftSource := cmsGray;
        WanteBC.MaterialShaftSource := cmsGray;
      end;

    end;

    Inc(Counter);
  end;
end;

procedure TStrokeRigg.UpdateCameraX(Delta: single);
begin

end;

procedure TStrokeRigg.UpdateCameraY(Delta: single);
begin

end;

procedure TStrokeRigg.UpdateHullTexture;
begin

end;

procedure TStrokeRigg.UpdateOffset;
begin
  o := TRotaForm3.RggGlobalOffset;
end;

procedure TStrokeRigg.HideAll;
begin
  Visible := False;

  KugelA0.Visible := False;
  KugelA.Visible := False;

  KugelB0.Visible := False;
  KugelB.Visible := False;

  KugelC0.Visible := False;
  KugelC.Visible := False;

  KugelD0.Visible := False;
  KugelD.Visible := False;

  KugelE0.Visible := False;
  KugelE.Visible := False;

  KugelN0.Visible := False;
  KugelF0.Visible := False;
  KugelF.Visible := False;

  KugelP0.Visible := False;
  KugelP.Visible := False;

  KugelM.Visible := False;

  KugelFP.Visible := False;

  Vorstag.Visible := False;
  WanteA0A.Visible := False;
  WanteAC.Visible := False;
  WanteB0B.Visible := False;
  WanteBC.Visible := False;

  SalingAD.Visible := False;
  SalingBD.Visible := False;
end;

procedure TStrokeRigg.ShowAgain;
begin
  Visible := True;

  KugelA0.Visible := True;
  KugelA.Visible := True;

  KugelB0.Visible := True;
  KugelB.Visible := True;

  KugelC0.Visible := True;
  KugelC.Visible := True;

  KugelD0.Visible := True;
  KugelD.Visible := True;

  KugelE0.Visible := True;
  KugelE.Visible := True;

  KugelN0.Visible := True;
  KugelF0.Visible := True;
  KugelF.Visible := True;

  KugelP0.Visible := True;
  KugelP.Visible := True;

  KugelM.Visible := True;

  KugelFP.Visible := True;

  Vorstag.Visible := FWantRenderS;
  WanteA0A.Visible := FWantRenderS;
  WanteAC.Visible := FWantRenderS;
  WanteB0B.Visible := FWantRenderS;
  WanteBC.Visible := FWantRenderS;

  SalingAD.Visible := FWantRenderS;
  SalingBD.Visible := FWantRenderS;

  WantRenderH := WantRenderH;
  WantRenderP := WantRenderP;
  WantRenderF := WantRenderF;
  WantRenderE := WantRenderE;

  SalingTyp := SalingTyp;
end;

end.
