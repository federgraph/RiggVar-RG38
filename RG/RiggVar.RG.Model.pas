unit RiggVar.RG.Model;

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
  System.IniFiles,
  System.Math,
  System.Math.Vectors,
  RiggVar.RG.Def,
  RiggVar.RG.Data,
  RggStrings,
  RggScroll,
  RggTypes,
  RggSchnittKK,
  RggTrimmTab,
  RggCalc,
  RggDoc,
  RggFachwerk,
  RggInter;

type
  TRigg2 = class(TInterfacedObject, IRigg0)
  private
    FSalingTyp: TSalingTyp;
    FControllerTyp: TControllerTyp;
    FCalcTyp: TCalcTyp;

    FManipulatorMode: Boolean;

    FTrimm: TTrimm;

    FGetriebeOK: Boolean;
    FMastOK: Boolean;
    FRiggOK: Boolean;

    FGetriebeStatus: set of TGetriebeStatus;
    FRiggStatus: set of TRiggStatus;

    FrWanteZulang: single;

    SKK: TSchnittKK;
    SplitF: TSplitF;
    TetraF: TTetraF;

    TrimmTab: TTrimmTab;

    GSB: TRggFA; { Getriebe-Scroll-Bars }

    rP: TRiggPoints; { Koordinaten belastet 3d in mm }
    rPe: TRiggPoints; { Koordinaten entlastet 3d in mm }

    rL: TRiggRods; { Längen belastet 3d in mm }
    rLe: TRiggRods; { Längen entlastet 3d in mm }

    rF: TRiggRods; { Stabkräfte 3d in N }
    rEA: TRiggRods; { EA Werte 3d in KN }

    LineData: TLineDataR100; { Durchbiegungswerte in mm }
    FMastKurve: TMastKurve;

    FrPuettingA: single;
    FrBasis: single;

    FrController: single;
    FrWinkel: single;
    FrVorstag: single;
    FrWunten2D: single;
    FrWunten3D: single;
    FrWoben2D: single;
    FrWoben3D: single;
    FrSalingL: single;
    FrSalingH: single;
    FrSalingA: single;
    FrMastUnten: single;
    FrMastOben: single;
    FrMastEnde: single;

    FrPsi: single;
    FrPhi: single;
    FrAlpha: single;
    FrEpsilon: single;

    FiControllerAnschlag: Integer;

    FWinkelDegrees: single;
    FWPowerOS: single;

    FrMastLength: single;
    FrMastfallVorlauf: single;

    FrVorstagDiff: single;
    FrSpannungW: single;

    psiStart: single;
    psiEnde: single;

    l0: single; { in mm }

    FLineCountM: Integer;
    FMastStatus: TMastStatusSet;
    FKorrigiert: Boolean;

    FEx, FEy, FDx, FDy, FD0x, FD0y, FCx, FCy: single;
    FE, FD, FAx, FAy, FALx, FALy, FLvon1, FLvon2, FALvon12: single;

    ControllerFree: Boolean;
    BiegungE: single; { in mm }
    FMastPositionE: single;
    hd, he, lc, ld, le: single; { in mm }
    F1, F2, FA, FB, FC: single; { in N }
    EI: single; { in Nmm^2 }

    { gammaE bedeutet gammaEntlastet }
    Beta, Gamma, gammaE, delta1, delta2, alpha1, alpha2: single; { in rad }
    eps1, eps2, epsA, epsB: single; { in rad }

    FExcenter: single; { in mm, Erfahrungswert }
    FKnicklaenge: single; { in mm }
    FXpos: single; { in mm }
    FSchnittPunktKraft, { in N }
    FwSchnittOhne: single; { in N }
    FwSchnittMit: single; { in N }
    FwSchnittOffset: single; { in mm }
    FControllerWeg: single; { in mm }
    FSalingWeg: single; { in mm }
    FSalingWegKnick: single; { in mm }
    FKoppelFaktor: single; { dimensionslos }
    FKorrekturFaktor: single; { dimensionlos }
    FSalingAlpha: single; { in mm/N }
    FControllerAlpha: single; { in mm/N }

    FOnRegelGrafik: TNotifyEvent;
    FProbe: Boolean;
    FHullIsFlexible: Boolean;
    WantToPlayWithExtendedSearchRange: Boolean;

    KnotenLastD0: TPoint3D;
    KnotenLastC: TPoint3D;
    KnotenLastC0: TPoint3D;

    UpdateGetriebeCounter: Integer;

    ExitCounter1: Integer;
    ExitCounter2: Integer;
    ExitCounter3: Integer;
    ExitCounter4: Integer;
    ExitCounter5: Integer;
    ExitCounter6: Integer;
    ExitCounter7: Integer;

    Temp1: single;
    Temp2: single;
    Temp3: single;
    Temp4: single;

    RGD: TRegelGraphData;

    procedure Kraefte;
    procedure Split;
    procedure MakeRumpfKoord;
    procedure MakeKoord;
    procedure MakeKoordDS;
    procedure KraefteOS;
    procedure SplitOS;
    procedure MakeKoordOS;
    procedure Probe;
    procedure Entlasten;

    function GetProofRequired: Boolean;
    procedure SetProofRequired(const Value: Boolean);

    procedure GetDefaultChartData;
    function GetRiggOK: Boolean;
    function GetEA: TRiggRods;
    procedure SetEA(const Value: TRiggRods);

    function GetRiggStatusText: string;

    function GetKorrigiert: Boolean;
    procedure SetKorrigiert(const Value: Boolean);
    procedure SetEI(const Value: Integer);
    function GetEI: Integer;
    function GetControllerTyp: TControllerTyp;
    procedure SetControllerTyp(const Value: TControllerTyp);
    function GetMastLinie: TLineDataR100;

    procedure Abstaende;

    procedure BerechneF;
    procedure KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single);

    function GetSalingDaten: TSalingDaten;
    procedure GetLogoData;
    procedure GetDefaultData;
    function GetMastLength: single;
    function GetMastOben: single;
    function GetMastUnten: single;

    function GetMastOK: Boolean;
    function GetGetriebeOK: Boolean;
    function GetMastfallVorlauf: single;
    procedure SetMastfallVorlauf(const Value: single);
    function GetManipulatorMode: Boolean;
    procedure SetManipulatorMode(const Value: Boolean);
    procedure SetGlieder(const Values: TTrimmControls);
    function GetGlieder: TTrimmControls;
    function GetRggFA: TRggFA;
    function GetCalcTyp: TCalcTyp;
    procedure SetCalcTyp(const Value: TCalcTyp);

    procedure IntGliederToReal;
    procedure RealGliederToInt;
    procedure Wanten2dTo3d;
    procedure Wanten3dTo2d;
    procedure SetMastLength(const Value: single);
    procedure SetMastUnten(const Value: single);
    procedure SetMastOben(const Value: single);
    function GetRealGlied(Index: TsbName): single;
    procedure SetRealGlied(Index: TsbName; const Value: single);
    function GetSalingTyp: TSalingTyp;
    procedure SetSalingTyp(const Value: TSalingTyp);

    procedure CalcW1W2;
    procedure CalcW1;
    procedure CalcW2;
    procedure CalcWante;
    procedure Clear;
    procedure FanIn;
    procedure FanOut;
    procedure GetEpsilon;
    function GetKoppelFaktor: single;
    procedure SolveKG21(KM, KU1, KU2, KB: TPoint3D; var FU1, FU2, FB: single);

    procedure GetBuiltinData;

    function GetGetriebeStatusText: string;

    procedure ResetStatus;
    procedure UpdateGetriebeDS;
    procedure UpdateGetriebeOSS;
    procedure UpdateGetriebeOSB;
    procedure Rest;
    procedure BerechneM;
    function GetKoppelKurve: TKoordLine;
    function GetMastKurve: TMastKurve;
    procedure GetWantenspannung;
    function WantenKraftvonVorstag(WegSoll: single): single;
    function GetVorstagNull: single;

    procedure CalcWKnick;
    procedure GetSalingWeg;
    procedure GetControllerWeg;
    procedure GetSalingWegKnick;
    function WvonF(f: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
    function FvonW(WSoll: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
    procedure ResetMastStatus;
    procedure UpdateMastPositionE;

    function GetMastStatusText: string;
    function GetMastPositionE: single;

    function GetRealTrimm(Index: TTrimmIndex): single;
    function GetMastLC: single;
    function GetMastBeta: single;
    function GetRiggPoints: TRiggPoints;
    function GetRelaxedRiggPoints: TRiggPoints;
    function GetDurchbiegungHE: single;
    function GetDurchbiegungHD: single;
    function GetRelaxedRiggLengths: TRiggRods;
    function GetRiggLengths: TRiggRods;
    function GetTrimmTabDaten: TTrimmTabDaten;
    function GetStabKraefte: TRiggRods;
    procedure SetRiggPoints(const Value: TRiggPoints);
    function GetTrimmTabelle: TTrimmTab;
    function GetMastLE: single;

    procedure SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
  protected
    procedure ResetExitCounters;
    function GetPlotValue(CurrentParam: TFederParam; PlotID: Integer; x, y: single): single;
    function FindBogenIndexOf(P: TPoint3D): Integer;
    function GetMastKurvePoint(const Index: Integer): TPoint3D;
    function GetAngles: TRiggAngles;
  public
    LogList: TStringList;
    Fachwerk: TFachwerk;

    constructor Create;
    destructor Destroy; override;

    function GetCounterValue(Idx: Integer): Integer;
    function GetTempValue(Idx: Integer): single;

    procedure InitFactArray;
    procedure UpdateFactArray(CurrentParam: TFederParam);
    procedure UpdateMastGraph(Model: TMastGraphModel);

    function Regeln(TrimmSoll: TTrimm): Integer;
    procedure BiegeUndNeigeF1(Mastfall, Biegung: single);
    procedure NeigeF(Mastfall: single);
    procedure BiegeUndNeigeC(MastfallC, Biegung: single);
    procedure BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: single);
    procedure BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: single);
    procedure MakeSalingHBiggerFS(SalingHplus: single);
    procedure MakeSalingLBiggerDS(SalingLplus: single);
    procedure SchnittKraefte;
    procedure ChangeRigg(CurrentParam: TFederParam; Value: single);
    procedure AusgabeText(ML: TStrings; WantAll: Boolean = True; WantForce: Boolean = False);
    procedure ComputeKraftKurven(KK: TKraftKurven);
    procedure AusgabeKommentar(ML: TStrings);

    function GetPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRelaxedPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRiggDistance(Value: TRiggRod): single;
    function GetStabKraft(Value: TRiggRod): single;

    procedure Reset;
    procedure UpdateGSB;
    procedure UpdateGlieder;
    procedure UpdateGetriebe;
    procedure UpdateRigg;

    procedure UpdateGetriebeFS;
    procedure BerechneWinkel;

    procedure Assign(Source: TRigg2);

    procedure LoadFromIniFile(ini: TIniFile);
    procedure WriteToIniFile(ini: TIniFile);

    procedure SaveToFederData(fd: TRggData);
    procedure LoadFromFederData(fd: TRggData);

    procedure WriteToDocFile(FileName: string);
    procedure LoadFromDocFile(FileName: string);

    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);

    procedure SetDefaultDocument;
    procedure GetRealTrimmRecord(var RealTrimm: TRealTrimm);

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    procedure WriteXml(ML: TStrings; AllTags: Boolean = False);

    property SalingTyp: TSalingTyp read GetSalingTyp write SetSalingTyp;
    property ControllerTyp: TControllerTyp read GetControllerTyp write SetControllerTyp;
    property CalcTyp: TCalcTyp read GetCalcTyp write SetCalcTyp;

    property ManipulatorMode: Boolean read GetManipulatorMode write SetManipulatorMode;

    property GetriebeOK: Boolean read GetGetriebeOK;
    property MastOK: Boolean read GetMastOK;
    property RiggOK: Boolean read GetRiggOK;

    property GetriebeStatusText: string read GetGetriebeStatusText;
    property MastStatusText: string read GetMastStatusText;
    property RiggStatusText: string read GetRiggStatusText;

    property MastLength: single read GetMastLength write SetMastLength;
    property MastUnten: single read GetMastUnten write SetMastUnten;
    property MastOben: single read GetMastOben write SetMastOben;
    property MastfallVorlauf: single read GetMastfallVorlauf write SetMastfallVorlauf;
    property DurchbiegungHE: single read GetDurchbiegungHE;
    property DurchbiegungHD: single read GetDurchbiegungHD;
    property MastLE: single read GetMastLE;
    property MastLC: single read GetMastLC;
    property MastBeta: single read GetMastBeta;
    property MastPositionE: single read GetMastPositionE;
    property MastEI: Integer read GetEI write SetEI;
    property MastStatus: TMastStatusSet read FMastStatus;

    property ControllerAnschlag: Integer read FiControllerAnschlag write FiControllerAnschlag;

    property WantenSpannung: single read FWPowerOS write FWPowerOS;

    property LineCountM: Integer read FLineCountM write FLineCountM;
    property Korrigiert: Boolean read GetKorrigiert write SetKorrigiert;
    property ProofRequired: Boolean read GetProofRequired write SetProofRequired;

    property KoppelFaktor: single read FKoppelFaktor;
    property SpannungW: single read FrSpannungW;
    property VorstagDiff: single read FrVorstagDiff;

    property Angles: TRiggAngles read GetAngles;
    property phi: single read FrPhi write FrPhi;
    property psi: single read FrPsi write FrPsi;
    property alpha: single read FrAlpha;
    property epsilon: single read FrEpsilon write FrEpsilon;
    property SalingAlpha: single read FSalingAlpha;

    property RggFA: TRggFA read GetRggFA;
    property Trimm: TTrimm read FTrimm;

    property RegelGraphData: TRegelGraphData read RGD;
    property KoppelKurve: TKoordLine read GetKoppelKurve;
    property MastLinie: TLineDataR100 read GetMastLinie;
    property MastKurve: TMastKurve read GetMastKurve;

    property OnRegelGrafik: TNotifyEvent read FOnRegelGrafik write FOnRegelGrafik;
    property HullFlexible: Boolean read FHullIsFlexible write FHullIsFlexible;

    property RealTrimm[Index: TTrimmIndex]: single read GetRealTrimm;

    property RiggPoints: TRiggPoints read GetRiggPoints write SetRiggPoints;
    property RelaxedRiggPoints: TRiggPoints read GetRelaxedRiggPoints;

    property RiggLengths: TRiggRods read GetRiggLengths;
    property RelaxedRiggLengths: TRiggRods read GetRelaxedRiggLengths;

    property EA: TRiggRods read GetEA write SetEA;
    property StabKraefte: TRiggRods read GetStabKraefte;

    property Glieder: TTrimmControls read GetGlieder write SetGlieder;
    property RealGlied[Index: TsbName]: single read GetRealGlied write SetRealGlied;

    property SalingDaten: TSalingDaten read GetSalingDaten;
    property TrimmtabDaten: TTrimmTabDaten read GetTrimmTabDaten;
    property TrimmTabelle: TTrimmTab read GetTrimmTabelle;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TRigg2.Create;
begin
  RGD := TRegelGraphData.Create;
  GetDefaultChartData;

  SplitF := TSplitF.Create;
  TetraF := TTetraF.Create;
  Fachwerk := TFachwerk.Create;

  FSalingTyp := stFest;
  FControllerTyp := ctDruck;
  FCalcTyp := ctBiegeKnicken;
  FManipulatorMode := False;
  FKorrigiert := True;
  FProbe := True;
  FHullIsFlexible := True;

  FLineCountM := 100;
  EI := 14.7E9; { Nmm^2 }
  FExcenter := 20.0; { in mm }
  FKnicklaenge := 4600.0; { in mm }
  FKorrekturFaktor := 0.8; { dimensionlos }

  rEA.D0C := EAgross;
  rEA.C0D0 := EARumpf;
  rEA.B0C0 := EARumpf;
  rEA.A0C0 := EARumpf;
  rEA.B0D0 := EARumpf;
  rEA.A0D0 := EARumpf;
  rEA.A0B0 := EARumpf;
  rEA.B0B := 13 * EModulStahl;
  rEA.A0A := 13 * EModulStahl;
  rEA.BD := EAgross;
  rEA.AD := EAgross;
  rEA.AB := EASaling;
  rEA.BC := 13 * EModulStahl;
  rEA.AC := 13 * EModulStahl;
  rEA.C0C := 13 * EModulStahl;
  rEA.DC := EAgross;
  rEA.D0D := EAgross;
  rEA.ED := EAgross;
  rEA.D0E := EAgross;
  rEA.E0E := EAgross;

  GSB := TRggFA.Create;
  WantLogoData := false;
  LogList := TStringList.Create;
  SKK := TSchnittKK.Create;
  TrimmTab := TTrimmTab.Create;

  GetBuiltinData;
  IntGliederToReal;
  Reset;
  BerechneWinkel;
end;

destructor TRigg2.Destroy;
begin
  RGD.Free;
  SplitF.Free;
  TetraF.Free;
  Fachwerk.Free;
  SKK.Free;
  TrimmTab.Free;
  GSB.Free;
  LogList.Free;
  inherited Destroy;
end;

procedure TRigg2.SetSalingTyp(const Value: TSalingTyp);
begin
  if Value <> FSalingTyp then
  begin
    FSalingTyp := Value;
    Fachwerk.SalingTyp := Value;
  end;
end;

procedure TRigg2.SetMastUnten(const Value: single);
begin
  if Value <> FrMastUnten then
  begin
    FrMastUnten := Value;
  end;
end;

procedure TRigg2.SetProofRequired(const Value: Boolean);
begin
  FProbe := Value;
end;

procedure TRigg2.SetMastOben(const Value: single);
begin
  if Value <> FrMastOben then
  begin
    FrMastOben := Value;
  end;
end;

procedure TRigg2.SetManipulatorMode(const Value: Boolean);
begin
  FManipulatorMode := Value;
end;

procedure TRigg2.SetMastfallVorlauf(const Value: single);
begin
  FrMastfallVorlauf := Value;
end;

procedure TRigg2.SetMastLength(const Value: single);
begin
  if Value <> FrMastLength then
  begin
    FrMastLength := Value;
    FrMastEnde := Value - FrMastOben - FrMastUnten;
  end;
end;

function TRigg2.GetGetriebeOK: Boolean;
begin
  result := FGetriebeOK;
end;

function TRigg2.GetGlieder: TTrimmControls;
begin
  RealGliederToInt;
  result.Controller := Round(FrController);
  result.Wanten := Round(FrWunten3D + FrWoben3D);
  result.Woben := Round(FrWoben3D);
  result.SalingH := Round(FrSalingH);
  result.SalingA := Round(FrSalingA);
  result.SalingL := Round(FrSalingL);
  result.Vorstag := Round(FrVorstag);
  result.Winkel := Round(FWinkelDegrees);
  result.WPowerOS := Round(FWPowerOS);
end;

procedure TRigg2.SetGlieder(const Values: TTrimmControls);
begin
  FrController := Values.Controller;
  FWinkelDegrees := Values.Winkel;
  FrVorstag := Values.Vorstag;
  FrWunten3D := Values.Wanten - Values.Woben;
  FrWoben3D := Values.Woben;
  FrSalingH := Values.SalingH;
  FrSalingA := Values.SalingA;
  FrSalingL := Values.SalingL;
  FWPowerOS := Values.WPowerOS;
  IntGliederToReal;
end;

function TRigg2.GetRealGlied(Index: TsbName): single;
begin
  result := 0;
  case Index of
    fpController: result := FrController;
    fpWinkel: result := FrWinkel;
    fpVorstag: result := FrVorstag;
    fpWante: result := FrWunten3D + FrWoben3D;
    fpWoben: result := FrWoben3D;
    fpSalingH: result := FrSalingH;
    fpSalingA: result := FrSalingA;
    fpSalingL: result := FrSalingL;
    fpVorstagOS: result := FrVorstag;
    fpWPowerOS: result := FWPowerOS;
  end;
end;

function TRigg2.GetRggFA: TRggFA;
begin
  result := GSB;
end;

procedure TRigg2.SetRealGlied(Index: TsbName; const Value: single);
begin
  case Index of
    fpController: FrController := Value;
    fpWinkel: FrWinkel := Value;
    fpVorstag: FrVorstag := Value;
    fpWante: FrWunten3D := Value - FrWoben3D;
    fpWoben:
    begin
      FrWunten3D := FrWunten3D + FrWoben3D - Value;
      FrWoben3D := Value;
    end;
    fpSalingH: FrSalingH := Value;
    fpSalingA: FrSalingA := Value;
    fpSalingL: FrSalingL := Value;
    fpVorstagOS: FrVorstag := Value;
    fpWPowerOS: FWPowerOS := Round(Value);
  end;
end;

procedure TRigg2.SetRiggPoints(const Value: TRiggPoints);
begin
  rP := Value;
end;

function TRigg2.GetSalingDaten: TSalingDaten;
var
  SD: TSalingDaten;
  ooTempA, ooTempB, ooTempC: TPoint3D;
  EbeneACD, EbeneACA0: TPoint3D;
  tempWW, tempWS: single;
  tempSinus, tempCosinus: single;
  cosWW: single;
begin
  ooTempA := (rP.A - rP.C).Normalize;
  ooTempB := (rP.A0 - rP.A).Normalize;
  cosWW := ooTempA.DotProduct(ooTempB);
  if abs(cosWW) > 0.99 then
    tempWW := 0
  else
    tempWW := arccos(cosWW);

  ooTempB := (rP.A - rP.D).Normalize;
  EbeneACD := ooTempA.CrossProduct(ooTempB);

  ooTempB := (rP.A - rP.A0).Normalize;
  EbeneACA0 := ooTempA.CrossProduct(ooTempB);

  ooTempA := EbeneACD.Normalize;
  ooTempB := EbeneACA0.Normalize;
  ooTempC := ooTempA.CrossProduct(ooTempB);
  tempSinus := ooTempC.Length;
  tempCosinus := ooTempA.DotProduct(ooTempB);

  tempWS := 0;
  try
    tempWS := arctan2(tempSinus, tempCosinus);
  except
    on EMathError do
      Main.Logger.Error(Ebenen_senkrecht_in_GetSalingDaten_String);
  end;

  SD.SalingH := FrSalingH;
  SD.SalingA := FrSalingA;
  SD.SalingL := FrSalingL;
  SD.SalingW := RadToDeg(arctan2(FrSalingA / 2, FrSalingH));
  SD.WantenWinkel := RadToDeg(tempWW);
  SD.KraftWinkel := RadToDeg(tempWS);

  result := SD;
end;

function TRigg2.GetSalingTyp: TSalingTyp;
begin
  result := FSalingTyp;
end;

procedure TRigg2.IntGliederToReal;
begin
  { Integer Glieder have been eliminated. }
  FrWinkel := DegToRad(FWinkelDegrees);
  FrMastEnde := FrMastLength - FrMastOben - FrMastUnten;
end;

procedure TRigg2.RealGliederToInt;
begin
  { Integer Glieder have been eliminated. }
  FWinkelDegrees := RadToDeg(FrWinkel);
  FrMastLength := FrMastUnten + FrMastOben + FrMastEnde;
end;

procedure TRigg2.UpdateGSB;
begin
  RealGliederToInt;
  GSB.Controller.Ist := FrController;
  GSB.Winkel.Ist := FWinkelDegrees;
  GSB.Vorstag.Ist := FrVorstag;
  GSB.Wante.Ist := FrWunten3D + FrWoben3D;
  GSB.Woben.Ist := FrWoben3D;
  GSB.SalingH.Ist := FrSalingH;
  GSB.SalingA.Ist := FrSalingA;
  GSB.SalingL.Ist := FrSalingL;
  GSB.VorstagOS.Ist := FrVorstag;
  GSB.WPowerOS.Ist := FWPowerOS;
end;

procedure TRigg2.UpdateGlieder;
begin
  FrController := GSB.Controller.Ist;
  FWinkelDegrees := GSB.Winkel.Ist;
  FrVorstag := GSB.Vorstag.Ist;
  FrWunten3D := GSB.Wante.Ist - GSB.Woben.Ist;
  FrWoben3D := GSB.Woben.Ist;
  FrSalingH := GSB.SalingH.Ist;
  FrSalingA := GSB.SalingA.Ist;
  FrSalingL := GSB.SalingL.Ist;
  FWPowerOS := GSB.WPowerOS.Ist;
  IntGliederToReal;
end;

procedure TRigg2.Wanten2dTo3d;
begin
  FrWunten3D := sqrt(sqr(FrWunten2D) + sqr((FrPuettingA - FrSalingA) / 2));
  FrWoben3D := sqrt(sqr(FrWoben2D) + sqr(FrSalingA / 2));
end;

procedure TRigg2.Wanten3dTo2d;
var
  u, v: single;
begin
  u := sqr(FrWunten3D) - sqr((FrPuettingA - FrSalingA) / 2);
  v := sqr(FrWoben3D) - sqr(FrSalingA / 2);
  if (u > 0) and (v > 0) then
  begin
    FrWunten2D := sqrt(u);
    FrWoben2D := sqrt(v);
  end;
end;

function TRigg2.GetGetriebeStatusText: string;
var
  s: string;
begin
  s := Status_String_Getriebe;
  if FGetriebeOK then
    s := s + Status_String_OK
  else
  begin
    if gsWanteZukurz in FGetriebeStatus then
      s := s + Status_String_WanteZuKurz
    else if gsWanteZulang in FGetriebeStatus then
      s := s + Format(Status_Format_String_WanteZuLang, [FrWanteZulang])
    else if gsErrorPsivonPhi in FGetriebeStatus then
      s := s + SalingHTooSmallString;
  end;
  result := s;
end;

function TRigg2.GetAngles: TRiggAngles;
begin
  result.alpha := alpha;
  result.alpha1 := alpha1;
  result.alpha2 := alpha2;
  result.beta := beta;
  result.gamma := gamma;
  result.delta1 := delta1;
  result.delta2 := delta2;
  result.epsilon := epsilon;
  result.phi := phi;
  result.psi := psi;
end;

procedure TRigg2.GetBuiltinData;
begin
  if WantLogoData then
    GetLogoData
  else
    GetDefaultData;
end;

procedure TRigg2.GetDefaultData;
{ Initialisierung aller Integerwerte und der TrimmTabelle;
  nachfolgend muß IntGliederToReal und Reset aufgerufen werden,
  um die Gleitkommawerte zu initialiseieren. }
begin
  { see (update) similar code (duplication) in TRggDocument.GetDefaultDoc }

  { Längen im Rigg in mm }
  FiControllerAnschlag := 50;
  FrController := 100; { Controllerposition bzw. Abstand E0-E }
  FrMastLength := 6115; { Gesamtlänge Mast }
  FrMastUnten := 2600; { unterer Teil Mast }
  FrMastOben := 2000; { oberer Teil Mast }
  FrMastfallVorlauf := 5000; { Abstand der Meßmarken }
  FrWunten3D := 2100; { unterer Teil Wante }
  FrWoben3D := 2020; { oberer Teil Wante }
  FrSalingH := 220; { Höhe des Salingdreiecks }
  FrSalingA := 850; { Abstand der Salingnocken }
  FrSalingL := Round(sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2)));
  FrVorstag := 4500; { Vorstaglänge }
  FWinkelDegrees := 95; { Winkel der unteren Wantabschnitte }
  FWPowerOS := 1000; { angenommene Wantenspannung 3d }

  { RumpfKoordinaten in mm }
  rP.A0.X := 2560; { Pütting Stbd }
  rP.A0.Y := -765;
  rP.A0.Z := 430;

  rP.B0.X := 2560; { Püttinge Bb }
  rP.B0.Y := 765;
  rP.B0.Z := 430;

  rP.C0.X := 4140; { Vorstag }
  rP.C0.Y := 0;
  rP.C0.Z := 340;

  rP.D0.X := 2870; { Mastfuß }
  rP.D0.Y := 0;
  rP.D0.Z := -100;

  rP.E0.X := 2970; { Controller }
  rP.E0.Y := 0;
  rP.E0.Z := 450;

  rP.F0.X := -30; { Spiegel }
  rP.F0.Y := 0;
  rP.F0.Z := 300;

  rP.P0 := rP.A0;
  rP.P0.Y := 0;

  GSB.Controller.Ist := FrController;
  GSB.Winkel.Ist := FWinkelDegrees;
  GSB.Vorstag.Ist := FrVorstag;
  GSB.Wante.Ist := FrWunten3D + FrWoben3D;
  GSB.Woben.Ist := FrWoben3D;
  GSB.SalingH.Ist := FrSalingH;
  GSB.SalingA.Ist := FrSalingA;
  GSB.SalingL.Ist := FrSalingL; { oben aus FiSalingH und FiSalingA errechnet }
  GSB.VorstagOS.Ist := FrVorstag;
  GSB.WPowerOS.Ist := FWPowerOS;

  GSB.InitStepDefault;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB.Controller.Min := 50;
  GSB.Controller.Max := 200;
  GSB.Winkel.Min := 85;
  GSB.Winkel.Max := 105;
  GSB.Vorstag.Min := 4400;
  GSB.Vorstag.Max := 4600;
  GSB.Wante.Min := 4050;
  GSB.Wante.Max := 4200;
  GSB.Woben.Min := 2000;
  GSB.Woben.Max := 2070;
  GSB.SalingH.Min := 140;
  GSB.SalingH.Max := 300;
  GSB.SalingA.Min := 780;
  GSB.SalingA.Max := 1000;
  GSB.SalingL.Min := 450;
  GSB.SalingL.Max := 600;
  GSB.VorstagOS.Min := 4200;
  GSB.VorstagOS.Max := 4700;
  GSB.WPowerOS.Min := 100;
  GSB.WPowerOS.Max := 3000;

  TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten;
end;

procedure TRigg2.GetLogoData;
{ Initialisierung aller Integerwerte und der TrimmTabelle;
  nachfolgend muß IntGliederToReal und Reset aufgerufen werden,
  um die Gleitkommawerte zu initialiseieren. }
var
  f, ox, oz: Integer;
begin
  { see similar code (duplication) in TRggDocument.GetLogoDoc }

  ox := 1400;
  oz := -350;

  f := 18;

  { Längen im Rigg in mm }
  FiControllerAnschlag := 50;
  FrController := 100; { Controllerposition bzw. Abstand E0-E }
  FrMastLength := Round((40 + sqrt(250) * 10) * f); { Gesamtlänge Mast }
  FrMastUnten := Round((sqrt(40) + sqrt(10)) * 10 * f); { unterer Teil Mast }
  FrMastOben := Round(sqrt(40) * 10 * f); { oberer Teil Mast }
  FrMastfallVorlauf := Round(FrMastLength * 0.75); { Abstand der Meßmarken }
  FrWunten3D := Round(sqrt(40) * 10 * f); { unterer Teil Wante }
  FrWoben3D := Round(sqrt(56) * 10 * f); { oberer Teil Wante }
  FrSalingH := 40 * f; { Höhe des Salingdreiecks }
  FrSalingA := 80 * f; { Abstand der Salingnocken }
  FrSalingL := Round(sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2)));
  FrVorstag := Round(sqrt(288) * 10 * f); { Vorstaglänge }
  FWinkelDegrees := Round(90 + RadToDeg(arctan2(1, 3))); { Winkel Wunten }
  FWPowerOS := 1000; { angenommene Wantenspannung 3d }

  { RumpfKoordinaten in mm }
  rP.A0.X := 30 * f + ox; { Pütting Stbd }
  rP.A0.Y := -40 * f;
  rP.A0.Z := 40 * f + oz;

  rP.B0.X := 30 * f + ox;
  rP.B0.Y := 40 * f;
  rP.B0.Z := 40 * f + oz;

  rP.C0.X := 150 * f + ox;
  rP.C0.Y := 0;
  rP.C0.Z := 40 * f + oz;

  rP.D0.X := 80 * f + ox;
  rP.D0.Y := 0;
  rP.D0.Z := 10 * f + oz;

  rP.E0.X := 85 * f + ox;
  rP.E0.Y := 0;
  rP.E0.Z := 50 * f + oz;

  rP.F0.X := -85 * f + ox;
  rP.F0.Y := 0;
  rP.F0.Z := 40 * f + oz;

  rP.P0 := rP.A0;
  rP.P0.Y := 0;

  GSB.Controller.Ist := FrController;
  GSB.Winkel.Ist := FrWinkel;
  GSB.Vorstag.Ist := FrVorstag;
  GSB.Wante.Ist := FrWunten3D + FrWoben3D;
  GSB.Woben.Ist := FrWoben3D;
  GSB.SalingH.Ist := FrSalingH;
  GSB.SalingA.Ist := FrSalingA;
  GSB.SalingL.Ist := FrSalingL;
  GSB.VorstagOS.Ist := FrVorstag;
  GSB.WPowerOS.Ist := FWPowerOS;
  GSB.MastfallVorlauf.Ist := FrMastfallVorlauf;

  GSB.InitStepDefault;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB.Controller.Min := 50;
  GSB.Controller.Max := 200;
  GSB.Winkel.Min := 70;
  GSB.Winkel.Max := 120;
  GSB.Vorstag.Min := FrVorstag - 20 * f;
  GSB.Vorstag.Max := FrVorstag + 0 * f;
  GSB.Wante.Min := FrWunten3D + FrWoben3D - 10 * f;
  GSB.Wante.Max := FrWunten3D + FrWoben3D + 10 * f;
  GSB.Woben.Min := FrWoben3D - 10 * f;
  GSB.Woben.Max := FrWoben3D + 10 * f;
  GSB.SalingH.Min := FrSalingH - 10 * f;
  GSB.SalingH.Max := FrSalingH + 10 * f;
  GSB.SalingA.Min := FrSalingA - 10 * f;
  GSB.SalingA.Max := FrSalingA + 10 * f;
  GSB.SalingL.Min := FrSalingL - 10 * f;
  GSB.SalingL.Max := FrSalingL + 10 * f;
  GSB.VorstagOS.Min := FrVorstag - 10 * f;
  GSB.VorstagOS.Max := FrVorstag + 10 * f;
  GSB.WPowerOS.Min := 100;
  GSB.WPowerOS.Max := 3000;
  GSB.MastfallVorlauf.Min := GSB.MastfallVorlauf.Ist - 10 * f;
  GSB.MastfallVorlauf.Max := GSB.MastfallVorlauf.Ist + 10 * f;

  TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten;
end;

function TRigg2.GetManipulatorMode: Boolean;
begin
  result := FManipulatorMode;
end;

function TRigg2.GetMastfallVorlauf: single;
begin
  result := FrMastfallVorlauf;
end;

function TRigg2.GetMastOben: single;
begin
  result := FrMastOben;
end;

function TRigg2.GetMastOK: Boolean;
begin
  result := FMastOK;
end;

procedure TRigg2.Reset;
begin
  { Rumpfkoordinaten }
  rP.P0 := rP.A0;
  rP.P0.Y := 0;
  { Mast }
  FrMastEnde := FrMastLength - FrMastUnten - FrMastOben;
  { Rumpflängen }
  FrPuettingA := rP.B0.Y - rP.A0.Y;
  FrBasis := rP.D0.Distance(rP.P0);
  FrAlpha := SKK.AngleZXM(rP.P0, rP.D0);
end;

procedure TRigg2.WriteToIniFile(ini: TIniFile);
var
  i: Integer;
  s, s1, s2: string;
  tempEI: Integer;
begin
  s := Rigg_IniSectionString;
  ini.WriteInteger(s, SalingTyp_IniString, Ord(FSalingTyp));
  TrimmTab.WriteToIniFile(ini);

  s := Mast_IniSectionString;
  ini.WriteInteger(s, MastL_IniString, Round(FrMastLength));
  ini.WriteInteger(s, Mastunten_IniString, Round(FrMastUnten));
  ini.WriteInteger(s, Mastoben_IniString, Round(FrMastOben));
  ini.WriteInteger(s, MastfallVorlauf_IniString, Round(FrMastfallVorlauf));

  s := Ist_IniSectionString;
  ini.WriteInteger(s, Controller_IniString, Round(FrController));
  ini.WriteInteger(s, WinkelString, Round(FWinkelDegrees));
  ini.WriteInteger(s, VorstagString, Round(FrVorstag));
  ini.WriteInteger(s, WanteString, Round(FrWunten3D + FrWoben3D));
  ini.WriteInteger(s, Woben_IniString, Round(FrWoben3D));
  ini.WriteInteger(s, SalingH_IniString, Round(FrSalingH));
  ini.WriteInteger(s, SalingA_IniString, Round(FrSalingA));
  ini.WriteInteger(s, WPowerOS_IniString, Round(FWPowerOS));

  s := Min_IniSectionString;
  ini.WriteInteger(s, Controller_IniString, Round(GSB.Controller.Min));
  ini.WriteInteger(s, Winkel_IniString, Round(GSB.Winkel.Min));
  ini.WriteInteger(s, Vorstag_IniString, Round(GSB.Vorstag.Min));
  ini.WriteInteger(s, Wante_IniString, Round(GSB.Wante.Min));
  ini.WriteInteger(s, Woben_IniString, Round(GSB.Woben.Min));
  ini.WriteInteger(s, SalingH_IniString, Round(GSB.SalingH.Min));
  ini.WriteInteger(s, SalingA_IniString, Round(GSB.SalingA.Min));
  ini.WriteInteger(s, SalingL_IniString, Round(GSB.SalingL.Min));
  ini.WriteInteger(s, VorstagOS_IniString, Round(GSB.VorstagOS.Min));
  ini.WriteInteger(s, WPowerOS_IniString, Round(GSB.WPowerOS.Min));

  s := Max_IniSectionString;
  ini.WriteInteger(s, Controller_IniString, Round(GSB.Controller.Max));
  ini.WriteInteger(s, Winkel_IniString, Round(GSB.Winkel.Max));
  ini.WriteInteger(s, Vorstag_IniString, Round(GSB.Vorstag.Max));
  ini.WriteInteger(s, Wante_IniString, Round(GSB.Wante.Max));
  ini.WriteInteger(s, Woben_IniString, Round(GSB.Woben.Max));
  ini.WriteInteger(s, SalingH_IniString, Round(GSB.SalingH.Max));
  ini.WriteInteger(s, SalingA_IniString, Round(GSB.SalingA.Max));
  ini.WriteInteger(s, SalingL_IniString, Round(GSB.SalingL.Max));
  ini.WriteInteger(s, VorstagOS_IniString, Round(GSB.VorstagOS.Max));
  ini.WriteInteger(s, WPowerOS_IniString, Round(GSB.WPowerOS.Max));

  s := Koordinaten_Rumpf_IniSectionString;
  ini.WriteInteger(s, A0x_IniString, Round(rP.A0.X));
  ini.WriteInteger(s, A0y_IniString, Round(rP.A0.Y));
  ini.WriteInteger(s, A0z_IniString, Round(rP.A0.Z));
  ini.WriteInteger(s, B0x_IniString, Round(rP.B0.X));
  ini.WriteInteger(s, B0y_IniString, Round(rP.B0.Y));
  ini.WriteInteger(s, B0z_IniString, Round(rP.B0.Z));
  ini.WriteInteger(s, C0x_IniString, Round(rP.C0.X));
  ini.WriteInteger(s, C0y_IniString, Round(rP.C0.Y));
  ini.WriteInteger(s, C0z_IniString, Round(rP.C0.Z));
  ini.WriteInteger(s, D0x_IniString, Round(rP.D0.X));
  ini.WriteInteger(s, D0y_IniString, Round(rP.D0.Y));
  ini.WriteInteger(s, D0z_IniString, Round(rP.D0.Z));
  ini.WriteInteger(s, E0x_IniString, Round(rP.E0.X));
  ini.WriteInteger(s, E0y_IniString, Round(rP.E0.Y));
  ini.WriteInteger(s, E0z_IniString, Round(rP.E0.Z));
  ini.WriteInteger(s, F0x_IniString, Round(rP.F0.X));
  ini.WriteInteger(s, F0y_IniString, Round(rP.F0.Y));
  ini.WriteInteger(s, F0z_IniString, Round(rP.F0.Z));

  s := Koordinaten_Rigg_IniSectionString;
  ini.WriteInteger(s, Ax_IniString, Round(rP.A.X));
  ini.WriteInteger(s, Ay_IniString, Round(rP.A.Y));
  ini.WriteInteger(s, Az_IniString, Round(rP.A.Z));
  ini.WriteInteger(s, Bx_IniString, Round(rP.B.X));
  ini.WriteInteger(s, By_IniString, Round(rP.B.Y));
  ini.WriteInteger(s, Bz_IniString, Round(rP.B.Z));
  ini.WriteInteger(s, Cx_IniString, Round(rP.C.X));
  ini.WriteInteger(s, Cy_IniString, Round(rP.C.Y));
  ini.WriteInteger(s, Cz_IniString, Round(rP.C.Z));
  ini.WriteInteger(s, Dx_IniString, Round(rP.D.X));
  ini.WriteInteger(s, Dy_IniString, Round(rP.D.Y));
  ini.WriteInteger(s, Dz_IniString, Round(rP.D.Z));
  ini.WriteInteger(s, Ex_IniString, Round(rP.E.X));
  ini.WriteInteger(s, Ey_IniString, Round(rP.E.Y));
  ini.WriteInteger(s, Ez_IniString, Round(rP.E.Z));
  ini.WriteInteger(s, Fx_IniString, Round(rP.F.X));
  ini.WriteInteger(s, Fy_IniString, Round(rP.F.Y));
  ini.WriteInteger(s, Fz_IniString, Round(rP.F.Z));

  s := Rigg_IniSectionString;
  ini.WriteInteger(s, ControllerTyp_IniString, Ord(FControllerTyp));
  ini.WriteInteger(s, CalcTyp_IniString, Ord(FCalcTyp));

  s := Mast_IniSectionString;
  tempEI := Round(EI / 1E6);
  ini.WriteInteger(s, EI_IniString, tempEI);

  s := EA_IniString;
  for i := 0 to 19 do
  begin
    s1 := IntToStr(i);
    s2 := Format('%.6g', [rEA.V[i]]);
    ini.WriteString(s, s1, s2);
  end;

end;

procedure TRigg2.LoadFromIniFile(ini: TIniFile);
var
  i: Integer;
  s, s1, s2: string;
begin
  s := Rigg_IniSectionString;
  SalingTyp := TSalingTyp(ini.ReadInteger(s, SalingTyp_IniString, Ord(stFest)));

  TrimmTab.LoadFromIniFile(ini);

  s := Mast_IniSectionString;
  FrMastLength := ini.ReadInteger(s, MastL_IniString, Round(FrMastLength));
  FrMastUnten := ini.ReadInteger(s, Mastunten_IniString, Round(FrMastUnten));
  FrMastOben := ini.ReadInteger(s, Mastoben_IniString, Round(FrMastOben));
  FrMastfallVorlauf := ini.ReadInteger(s, MastfallVorlauf_IniString, Round(FrMastfallVorlauf));

  s := Ist_IniSectionString;
  FrController := ini.ReadInteger(s, Controller_IniString, Round(FrController));
  FrWinkel := ini.ReadInteger(s, Winkel_IniString, Round(FrWinkel));
  FrVorstag := ini.ReadInteger(s, Vorstag_IniString, Round(FrVorstag));
  FrWoben3D := ini.ReadInteger(s, Woben_IniString, Round(FrWoben3D));
  FrWunten3D := ini.ReadInteger(s, Wante_IniString, Round(FrWunten3D + FrWoben3D - FrWoben3D));
  FrSalingH := ini.ReadInteger(s, SalingH_IniString, Round(FrSalingH));
  FrSalingA := ini.ReadInteger(s, SalingA_IniString, Round(FrSalingA));
  FWPowerOS := ini.ReadInteger(s, WPowerOS_IniString, Round(FWPowerOS));

  s := Min_IniSectionString;
  GSB.Controller.Min := ini.ReadInteger(s, Controller_IniString, Round(GSB.Controller.Min));
  GSB.Winkel.Min := ini.ReadInteger(s, Winkel_IniString, Round(GSB.Winkel.Min));
  GSB.Vorstag.Min := ini.ReadInteger(s, Vorstag_IniString, Round(GSB.Vorstag.Min));
  GSB.Wante.Min := ini.ReadInteger(s, Wante_IniString, Round(GSB.Wante.Min));
  GSB.Woben.Min := ini.ReadInteger(s, Woben_IniString, Round(GSB.Woben.Min));
  GSB.SalingH.Min := ini.ReadInteger(s, SalingH_IniString, Round(GSB.SalingH.Min));
  GSB.SalingA.Min := ini.ReadInteger(s, SalingA_IniString, Round(GSB.SalingA.Min));
  GSB.SalingL.Min := ini.ReadInteger(s, SalingL_IniString, Round(GSB.SalingL.Min));
  GSB.VorstagOS.Min := ini.ReadInteger(s, VorstagOS_IniString, Round(GSB.VorstagOS.Min));
  GSB.WPowerOS.Min := ini.ReadInteger(s, WPowerOS_IniString, Round(GSB.WPowerOS.Min));

  s := Max_IniSectionString;
  GSB.Controller.Max := ini.ReadInteger(s, Controller_IniString, Round(GSB.Controller.Max));
  GSB.Winkel.Max := ini.ReadInteger(s, Winkel_IniString, Round(GSB.Winkel.Max));
  GSB.Vorstag.Max := ini.ReadInteger(s, Vorstag_IniString, Round(GSB.Vorstag.Max));
  GSB.Wante.Max := ini.ReadInteger(s, Wante_IniString, Round(GSB.Wante.Max));
  GSB.Woben.Max := ini.ReadInteger(s, Woben_IniString, Round(GSB.Woben.Max));
  GSB.SalingH.Max := ini.ReadInteger(s, SalingH_IniString, Round(GSB.SalingH.Max));
  GSB.SalingA.Max := ini.ReadInteger(s, SalingA_IniString, Round(GSB.SalingA.Max));
  GSB.SalingL.Max := ini.ReadInteger(s, SalingL_IniString, Round(GSB.SalingL.Max));
  GSB.VorstagOS.Max := ini.ReadInteger(s, VorstagOS_IniString, Round(GSB.VorstagOS.Max));
  GSB.WPowerOS.Max := ini.ReadInteger(s, WPowerOS_IniString, Round(GSB.WPowerOS.Max));

  s := Koordinaten_Rumpf_IniSectionString;
  rP.A0.X := ini.ReadInteger(s, A0x_IniString, Round(rP.A0.X));
  rP.A0.Y := ini.ReadInteger(s, A0y_IniString, Round(rP.A0.Y));
  rP.A0.Z := ini.ReadInteger(s, A0z_IniString, Round(rP.A0.Z));
  rP.B0.X := ini.ReadInteger(s, B0x_IniString, Round(rP.B0.X));
  rP.B0.Y := ini.ReadInteger(s, B0y_IniString, Round(rP.B0.Y));
  rP.B0.Z := ini.ReadInteger(s, B0z_IniString, Round(rP.B0.Z));
  rP.C0.X := ini.ReadInteger(s, C0x_IniString, Round(rP.C0.X));
  rP.C0.Y := ini.ReadInteger(s, C0y_IniString, Round(rP.C0.Y));
  rP.C0.Z := ini.ReadInteger(s, C0z_IniString, Round(rP.C0.Z));
  rP.D0.X := ini.ReadInteger(s, D0x_IniString, Round(rP.D0.X));
  rP.D0.Y := ini.ReadInteger(s, D0y_IniString, Round(rP.D0.Y));
  rP.D0.Z := ini.ReadInteger(s, D0z_IniString, Round(rP.D0.Z));
  rP.E0.X := ini.ReadInteger(s, E0x_IniString, Round(rP.E0.X));
  rP.E0.Y := ini.ReadInteger(s, E0y_IniString, Round(rP.E0.Y));
  rP.E0.Z := ini.ReadInteger(s, E0z_IniString, Round(rP.E0.Z));
  rP.F0.X := ini.ReadInteger(s, F0x_IniString, Round(rP.F0.X));
  rP.F0.Y := ini.ReadInteger(s, F0y_IniString, Round(rP.F0.Y));
  rP.F0.Z := ini.ReadInteger(s, F0z_IniString, Round(rP.F0.Z));

  s := Koordinaten_Rigg_IniSectionString;
  rP.A.X := ini.ReadInteger(s, Ax_IniString, Round(rP.A.X));
  rP.A.Y := ini.ReadInteger(s, Ay_IniString, Round(rP.A.Y));
  rP.A.Z := ini.ReadInteger(s, Az_IniString, Round(rP.A.Z));
  rP.B.X := ini.ReadInteger(s, Bx_IniString, Round(rP.B.X));
  rP.B.Y := ini.ReadInteger(s, By_IniString, Round(rP.B.Y));
  rP.B.Z := ini.ReadInteger(s, Bz_IniString, Round(rP.B.Z));
  rP.C.X := ini.ReadInteger(s, Cx_IniString, Round(rP.C.X));
  rP.C.Y := ini.ReadInteger(s, Cy_IniString, Round(rP.C.Y));
  rP.C.Z := ini.ReadInteger(s, Cz_IniString, Round(rP.C.Z));
  rP.D.X := ini.ReadInteger(s, Dx_IniString, Round(rP.D.X));
  rP.D.Y := ini.ReadInteger(s, Dy_IniString, Round(rP.D.Y));
  rP.D.Z := ini.ReadInteger(s, Dz_IniString, Round(rP.D.Z));
  rP.E.X := ini.ReadInteger(s, Ex_IniString, Round(rP.E.X));
  rP.E.Y := ini.ReadInteger(s, Ey_IniString, Round(rP.E.Y));
  rP.E.Z := ini.ReadInteger(s, Ez_IniString, Round(rP.E.Z));
  rP.F.X := ini.ReadInteger(s, Fx_IniString, Round(rP.F.X));
  rP.F.Y := ini.ReadInteger(s, Fy_IniString, Round(rP.F.Y));
  rP.F.Z := ini.ReadInteger(s, Fz_IniString, Round(rP.F.Z));

  s := Rigg_IniSectionString;
  ControllerTyp := TControllerTyp(ini.ReadInteger(S, ControllerTyp_IniString, Ord(ctDruck)));
  CalcTyp := TCalcTyp(ini.ReadInteger(S, CalcTyp_IniString, Ord(ctBiegeKnicken)));

  s := Mast_IniSectionString;
  EI := ini.ReadInteger(S, EI_IniString, 14700) * 1E6;

  s := EA_IniString;
  for i := 0 to 19 do
  begin
    s1 := IntToStr(i);
    s2 := ini.ReadString(s, s1, IntToStr(100000));
    rEA.V[i] := StrToFloat(s2);
  end;

end;

procedure TRigg2.ResetStatus;
begin
  FGetriebeOK := True;
  Exclude(FGetriebeStatus, gsWanteZukurz);
  Exclude(FGetriebeStatus, gsWanteZulang);
end;

procedure TRigg2.ResetExitCounters;
begin
  ExitCounter1 := 0;
  ExitCounter2 := 0;
  ExitCounter3 := 0;
  ExitCounter4 := 0;
  ExitCounter5 := 0;
  ExitCounter6 := 0;
  ExitCounter7 := 0;
end;

procedure TRigg2.UpdateGetriebe;
begin
  Inc(UpdateGetriebeCounter);
  LogList.Clear;
  case SalingTyp of
    stOhneStarr:
      UpdateGetriebeOSS;
    stOhneBiegt:
      UpdateGetriebeOSB;
    stDrehbar:
      UpdateGetriebeDS;
    stFest:
      begin
        if ManipulatorMode then
          UpdateGetriebeFS
        else
          BerechneWinkel;
      end;
  end;
end;

procedure TRigg2.UpdateGetriebeFS;
{ FrWinkel gegeben, FrVorstag ergibt sich }
var
  svar: Boolean;
begin
  ResetStatus;
  FrPhi := FrWinkel + FrAlpha;
  Wanten3dTo2d;

  { Berechnung der Punkte A, B, P und D }
  FrPsi := PsiVonPhi(FrPhi, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
  if FrPsi < 0 then
    FrPsi := FrPsi + 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(LogList_String_InUpdateGetriebeFS);
    LogList.Add(LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter1);
    Exit;
  end;

  rP.A := SKK.AnglePointXZ(rP.A0, FrWunten2D, FrPhi - FrAlpha);
  rP.A.Y := -FrSalingA / 2;

  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;

  rP.P := rP.A;
  rP.P.Y := 0;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);

  rP.C := SKK.IntersectionXZ1(rP.A, rP.D, FrWoben2D, FrMastOben);

  FrVorstag := rP.C0.Distance(rP.C);
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest;
end;

procedure TRigg2.Rest;
begin
  { Berechnung Punkt ooE }
  rP.E.X := rP.E0.X - FrController;
  rP.E.Y := 0;
  rP.E.Z := rP.E0.Z;

  { Berechnung Punkt ooF, ooM }
  BerechneF;
  BerechneM;
end;

procedure TRigg2.BerechneF;
begin
{$ifdef TGetriebeOnly}
  { Berechnung Punkt F - Masttop }
  FrEpsilon := pi / 2 - SKK.AngleXZ(rP.C, rP.D);
  rP.F := SKK.AnglePointXZ(rP.D, FrMastLength - FrMastUnten, FrEpsilon);
{$endif}

  { überschriebene virtuelle Methode von TGetriebeFS }
  { Berechnung Punkt F - Masttop }
  SchnittKraefte;
  GetEpsilon;
  FrEpsilon := pi / 2 - SKK.AngleXZ(rP.C, rP.D0) - epsB;
  rP.F := SKK.AnglePointXZ(rP.C, FrMastEnde, FrEpsilon);
end;

procedure TRigg2.BerechneM;
var
  ooTemp: TPoint3D;
  a, t: single;
begin
  a := rp.F0.Distance(rp.F);
  t := (a - MastfallVorlauf) / a;
  ooTemp := rp.F - rp.F0;
  ooTemp := rp.F0 + ooTemp * t;
  rp.M := ooTemp;
end;

procedure TRigg2.BerechneWinkel;
{ FrVorstag gegeben, FrWinkel gesucht }
var
  Counter: Integer;
  svar: Boolean;
  VorstagIst, Diff: single;
  psiA, psiB: single;
  localC, ooTemp1, ooTemp2: TPoint3D;

  function VorstagLaenge(psi: single): single;
  { Viergelenk P0 P D D0, Koppelpunkt C }
  begin
    rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, psi - FrAlpha);
    rP.P := SKK.IntersectionXZ1(rP.P0, rP.D, FrWunten2D, FrSalingH);
    rP.C := SKK.IntersectionXZ1(rP.P, rP.D, FrWoben2D, FrMastOben);
    Result := rP.C0.Distance(rP.C);
  end;

begin
  ResetStatus;
  Wanten3dTo2d;

  { 1. Startwinkel ermitteln }
  { Durchbiegung Null, Mast gerade,
    linke Totlage für Winkel psi im Viergelenk D0 D C C0 }
  localC := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);

  psiStart := SKK.AngleXZM(rP.D0, localC);
  psiStart := pi / 2 + psiStart + FrAlpha;

  { Test, ob Wante locker bei Mast gerade und Vorstaglänge = FrVorstag.
    Ermittlung der Koordinaten für diesen Fall. }
  FrPsi := psiStart;

  rP.C := localC;
  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);
  rP.P := SKK.IntersectionXZ1(rP.D, rP.C, FrSalingH, FrWoben2D);

  FrWanteZulang := rP.P0.Distance(rP.P) + rP.P.Distance(rP.C) - (FrWunten2D + FrWoben2D);
  if FrWanteZulang < 0 then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end
  else
  begin
    { wenn Wante nicht zu locker dann Suche fortsetzen }

    { may jump to 'other' intersection of circle C0C and KoppelKurve }
    if WantToPlayWithExtendedSearchRange then
    begin
      { Suchbereich erweitern, wenn Durchbiegung negativ
        für psi = psiStart im VierGelenk P0 P D D0 }
      ooTemp1 := rP.D - rP.D0;
      ooTemp2 := rP.C - rP.D;
      localC := ooTemp1.CrossProduct(ooTemp2);
      if (localC.Y > 0) then
        psiStart := psiStart + DegToRad(45);
    end;

    { 2. Endwinkel ermitteln - Mastoben parallel zu Vorstag
      rechte Totlage für Winkel psi im Viergelenk D0 D C C0 }
    localC := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten, FrVorstag - FrMastOben);
    psiEnde := SKK.AngleXZM(rP.D0, localC);
    psiEnde := pi / 2 + psiEnde + FrAlpha;

    { 3. Winkel ermitteln, für den gilt: VorstagIst gleich FrVorstag }
    { Viergelenk P0 P D D0, Koppelpunkt C }
    psiB := psiStart;
    psiA := psiEnde + DegToRad(0.01);

    Temp1 := RadToDeg(psiA);
    Temp2 := RadToDeg(psiB);

    Counter := 0;
    repeat
      Counter := Counter + 1;
      FrPsi := (psiA + psiB) / 2;
      VorstagIst := VorstagLaenge(FrPsi);
      Diff := VorstagIst - FrVorstag;
      if Diff > 0 then
        psiB := FrPsi
      else
        psiA := FrPsi;
    until (abs(Diff) < 0.01) or (Counter = 200);

    Temp3 := Counter;
  end;

  { aktualisieren }
  rP.A := rP.P;
  rP.A.Y := -FrSalingA / 2;
  rP.B := rP.P;
  rP.B.Y := -rP.A.Y;
  { We actually want PhiVonPsi, but we can use function PsiVonPhi;
    imagine we are looking from behind - the mechanism appears mirrored,
    angle Psi needs to be transformed back and forth,
    and member length values passed according to mirrored model. }
  FrPhi := pi - PsiVonPhi(pi - FrPsi, FrBasis, FrMastUnten, FrSalingH, FrWunten2D, svar);
  if FrPhi > 2 * PI then
    FrPhi := FrPhi - 2 * PI;

  if svar = False then
  begin
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsErrorPsivonPhi);
    LogList.Add(LogList_String_InBerechneWinkel);
    LogList.Add(LogList_String_FalseInPsiVonPhi);
    Inc(ExitCounter2);
    Exit;
  end;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  Rest; { think: refactored away with 'extract method refactoring' }
end;

function TRigg2.GetKoppelKurve: TKoordLine;
{ Koppelkurve Viergelenk P0, P, D, D0 }
{ Wanten2d neu bereitgestellt,
  sonst interne Felder nicht verändert! }
var
  svar: Boolean;
  i: Integer;
  phiA, phiE, phiM, psiM, WinkelStep: single;
  ooTemp: TPoint3D;
  oooTemp: TRiggPoints;
begin
  oooTemp := rP; { aktuelle Koordinaten sichern }
  Wanten3dTo2d;

  { 1. Startwinkel }
  ooTemp := SKK.IntersectionXZ1(rP.P0, rP.D0, FrWunten2D + FrSalingH, FrMastUnten);
  phiA := SKK.AngleXZM(rP.P0, ooTemp);
  phiA := phiA + pi / 2 + FrAlpha;

  { 2. Endwinkel }
  ooTemp := SKK.IntersectionXZ1(rP.P0, rP.D0, FrWunten2D, FrSalingH + FrMastUnten);
  if SKK.Status = bmK1inK2 then
    phiE := FrAlpha + DegToRad(130)
  else
  begin
    phiE := SKK.AngleXZM(rP.P0, ooTemp);
    phiE := phiE + pi / 2 + FrAlpha;
  end;

  { 3. Koppelkurve }
  phiA := phiA + DegToRad(1);
  phiE := phiE - DegToRad(1);
  WinkelStep := (phiE - phiA) / 100;
  phiM := phiA;
  for i := 0 to 100 do
  begin
    psiM := PsiVonPhi(phiM, FrBasis, FrWunten2D, FrSalingH, FrMastUnten, svar);
    rP.P.X := rP.P0.X + FrWunten2D * cos(phiM - FrAlpha);
    rP.P.Z := rP.P0.Z + FrWunten2D * sin(phiM - FrAlpha);
    rP.D.X := rP.D0.X + FrMastUnten * cos(psiM - FrAlpha);
    rP.D.Z := rP.D0.Z + FrMastUnten * sin(psiM - FrAlpha);
    { Berechnung Punkt C }
    rP.C := SKK.IntersectionXZ1(rP.P, rP.D, FrWoben2D, FrMastOben);
    Result[i].X := rP.C.X;
    Result[i].Y := 0;
    Result[i].Z := rP.C.Z;
    phiM := phiM + WinkelStep;
  end;

  rP := oooTemp; { aktuelle Koordinaten wiederherstellen }
end;

procedure TRigg2.MakeSalingHBiggerFS(SalingHplus: single);
{ FrSalingH größer machen, FrWoben2d, Neigung und Biegung beibehalten;
  FrWunten2d neu berechnen }
begin
  FrSalingH := SalingHplus;

  rP.P := SKK.IntersectionXZ1(rP.D, rP.C, FrSalingH, FrWoben2D);
  FrWunten2D := rP.P.Distance(rP.P0);

  { aktualisieren }
  rP.A := rP.P;
  rP.A.Y := -FrSalingA / 2;
  rP.B := rP.P;
  rP.B.Y := -FrSalingA / 2;
  FrPhi := SKK.AngleXZM(rP.A0, rP.A);
  FrPhi := FrAlpha + pi / 2 + FrPhi;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingL := sqrt(sqr(FrSalingH) + sqr(FrSalingA / 2));
  FrController := FiControllerAnschlag;
  Wanten2dTo3d;
end;

procedure TRigg2.UpdateGetriebeDS;
{ gegeben: Woben3d, Wunten3d, Mastunten, Mastoben, SalingL,
  Vorstag, Rumpfkoordinaten. }
{ gesucht: Riggkoordinaten ooA, ooB, ooC, ooD, ooP, ooF }
var
  Counter: Integer;
  psiStart, psiEnde, psiEnde2, psiA, psiB: single;
  WobenMin, WobenMax, WobenIst, Diff: single;
  Saling1L, WStrich, W1Strich, Basis, Skalar: single;
  Temp, TempA, TempC, TempD: TPoint3D;

  function WobenIstVonPsi(psi: single): single;
  begin
    { Berechnungen im Vierelenk D0 D C C0 }
    rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, psi - FrAlpha);
    rP.C := SKK.IntersectionXZ1(rP.D, rP.C0, FrMastOben, FrVorstag);

    WStrich := rP.A0.Distance(rP.C);
    Basis := rP.A0.Distance(rP.D);

    { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
    TempD := TPoint3D.Zero;
    TempD.X := Basis;
    TempC := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, WStrich, FrMastOben);
    TempA := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, FrWunten3D, FrSalingL);
    Result := TempA.Distance(TempC);
  end;

begin
  ResetStatus;

  { Vorstag gegeben, Winkel numerisch ermitteln! }
  { Startwinkel }
  Temp := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten, FrVorstag - FrMastOben);
  psiStart := SKK.AngleXZM(rP.D0, Temp);
  psiStart := psiStart + pi / 2 + FrAlpha + DegToRad(0.1);

  { Endwinkel }
  Temp := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
  psiEnde := SKK.AngleXZM(rP.D0, Temp);
  psiEnde := pi / 2 + psiEnde + FrAlpha;
  psiEnde2 := psiEnde + DegToRad(50);

  WobenMin := WobenIstVonPsi(psiStart);
  WobenMax := WobenIstVonPsi(psiEnde);

  psiA := 0;
  psiB := 0;
  if (FrWoben3D < WobenMin) then
  begin
    psiA := psiEnde2;
    psiB := psiEnde;
    WobenMin := WobenIstVonPsi(psiEnde2);
    if (FrWoben3D < WobenMin) then
    begin
      FGetriebeOK := False;
      Include(FGetriebeStatus, gsWanteZukurz);
    end;
  end
  else if (FrWoben3D > WobenMax) then
  begin
    FrWanteZulang := FrWoben3D - WobenMax;
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end
  else
  begin
    psiA := psiStart;
    psiB := psiEnde;
  end;

  { FrPsi ermitteln }
  { Mast gerade zeichnen, wenn Wante zu lang oder zu kurz ist }
  if gsWanteZulang in FGetriebeStatus then
    FrPsi := psiEnde
  else if gsWanteZukurz in FGetriebeStatus then
  begin
    WobenIstVonPsi(psiEnde);
    FrPsi := psiEnde;
  end
  { if GetriebeOK then : den richtigen Winkel FrPsi numerisch ermitteln }
  else
  begin
    Counter := 0;
    repeat
      Counter := Counter + 1;
      FrPsi := (psiA + psiB) / 2;
      WobenIst := WobenIstVonPsi(FrPsi);
      Diff := WobenIst - FrWoben3D;
      if Diff > 0 then
        psiB := FrPsi
      else
        psiA := FrPsi;
    until
      (abs(Diff) < 0.01) or (Counter = 200);
  end;

  { weiter im ebenen Trapez }
  SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := TempD.Distance(temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP.C - rP.A0;
  temp := temp * Skalar;
  temp := rP.A0 + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP.D;
  temp := temp * Skalar;
  rP.A := rP.D + temp;

  { aktualisieren }
  rP.P := rP.A;
  rP.P.Y := 0;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  FrSalingA := 2 * rP.B.Y;
  FrSalingH := rP.P.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  Rest;
end;

procedure TRigg2.MakeSalingLBiggerDS(SalingLplus: single);
var
  TempA, TempC, TempD, temp: TPoint3D;
  Basis, Skalar, WStrich, W1Strich, Saling1L: single;
begin
  temp := TPoint3D.Zero;

  { Punkte D, C und F schon bekannt, FrWoben3d bleibt erhalten }
  FrSalingL := SalingLplus;

  WStrich := rP.A0.Distance(rP.C);
  Basis := rP.A0.Distance(rP.D);
  { weiter mit Koordinatentransformation, ebenes Trapez A0, A, C, D }
  { Berechnung TempD }
  TempD := TPoint3D.Zero;
  TempD.X := Basis;
  TempC := SKK.IntersectionXZ1(TPoint3D.Zero, TempD, WStrich, FrMastOben);
  { TempC bleibt beim Regeln unverändert }

  TempA := SKK.IntersectionXZ1(TempD, TempC, FrSalingL, FrWOben3D);
  { Radius1 = FrSalingL verändert sich beim Regeln
    Radius2 = FrWoben3D bleibt gleich beim Regeln
    TempA = SchnittPunkt1 verändert sich beim Regeln }

  SchnittGG(TPoint3D.Zero, TempC, TempD, TempA, temp);
  { Temp enthält jetzt den Schnittpunkt der Diagonalen }
  W1Strich := temp.Length;
  Saling1L := TempD.Distance(temp);

  { weiter räumlich: }
  Skalar := W1Strich / WStrich;
  temp := rP.C - rP.A0;
  temp := temp * Skalar;
  temp := rP.A0 + temp;
  { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

  { Berechnung Punkt ooA }
  Skalar := FrSalingL / Saling1L;
  temp := temp - rP.D;
  temp := temp * Skalar;
  rP.A := rP.D + temp;

  { FrWunten3d ermitteln und aktualisieren }
  FrWunten3D := TempA.Length;
  rP.P := rP.A;
  rP.P.Y := 0;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  FrPhi := SKK.AngleXZM(rP.A0, rP.A);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrSalingA := 2 * rP.B.Y;
  FrSalingH := rP.P.Distance(rP.D);
  FrController := FiControllerAnschlag;
end;

procedure TRigg2.UpdateGetriebeOSS;
{ FrVorstag und FrWoben2d gegeben }
var
  temp: TPoint3D;
  Skalar: single;
begin
  ResetStatus;
  rP.C := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
  FrWunten2D := rP.P0.Distance(rP.C) - FrWoben2D;
  { Punkt P }
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP.P.X := rP.C.X - Skalar * (rP.C.X - rP.P0.X);
  rP.P.Y := 0;
  rP.P.Z := rP.C.Z - Skalar * (rP.C.Z - rP.P0.Z);
  { Punkte A, B }
  rP.A := rP.P;
  rP.A.Y := Skalar * rP.A0.Y;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  { Punkt D }
  temp := rP.C - rP.D0;
  Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
  temp.X := Skalar * temp.X;
  { Temp[y] := 0; }
  temp.Z := Skalar * temp.Z;
  rP.D := rP.D0 + temp;
  { aktualisieren }
  FrSalingH := rP.P.Distance(rP.D);
  FrSalingA := 2 * rP.B.Y;
  FrSalingL := rP.A.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := SKK.AngleXZM(rP.D0, rP.D);
  FrPsi := pi / 2 + FrPsi + FrAlpha;
  Wanten2dTo3d;
  Rest;
end;

procedure TRigg2.UpdateGetriebeOSB;
{ FrVorstag und FrWoben3d und FrWunten3d gegeben }
var
  TempW, Skalar, TempWunten2d, TempWoben2d: single;
  temp: TPoint3D;
begin
  ResetStatus;
  { Wanten3dto2d }
  TempW := sqrt(sqr(FrWunten3D + FrWoben3D) - sqr(FrPuettingA / 2));
  Skalar := FrWunten3D / (FrWoben3D + FrWunten3D);
  TempWunten2d := TempW * Skalar;
  TempWoben2d := TempW * (1 - Skalar);
  rP.C := SKK.IntersectionXZ1(rP.P0, rP.C0, TempWunten2d + TempWoben2d, FrVorstag);

  { wenn die Wanten nicht straff sind: }
  if rP.D0.Distance(rP.C) > FrMastUnten + FrMastOben then
  begin
    rP.C := SKK.IntersectionXZ1(rP.D0, rP.C0, FrMastUnten + FrMastOben, FrVorstag);
    temp := rP.C- rP.D0;
    Skalar := FrMastUnten / (FrMastUnten + FrMastOben);
    temp.X := Skalar * temp.X;
    temp.Z := Skalar * temp.Z;
    rP.D := rP.D0 + temp;
    FrWanteZulang := FrWunten3D + FrWoben3D - rP.C.Distance(rP.A0);
    FGetriebeOK := False;
    Include(FGetriebeStatus, gsWanteZulang);
  end

  { wenn die Wanten straff sind: }
  else
  begin
    rP.D := SKK.IntersectionXZ1(rP.C, rP.D0, FrMastOben, FrMastUnten);
  end;

  { Punkt P }
  Skalar := FrWoben2D / (FrWunten2D + FrWoben2D);
  rP.P.X := rP.C.X - Skalar * (rP.C.X - rP.P0.X);
  rP.P.Y := 0;
  rP.P.Z := rP.C.Z - Skalar * (rP.C.Z - rP.P0.Z);
  { Punkte A, B }
  rP.A := rP.P;
  rP.A.Y := Skalar * rP.A0.Y;
  rP.B := rP.A;
  rP.B.Y := -rP.A.Y;
  { aktualisieren }
  FrSalingH := rP.P.Distance(rP.D);
  FrSalingA := 2 * rP.B.Y;
  FrSalingL := rP.A.Distance(rP.D);
  FrPhi := SKK.AngleXZM(rP.P0, rP.P);
  FrPhi := pi / 2 + FrPhi + FrAlpha;
  FrWinkel := FrPhi - FrAlpha;
  FrPsi := SKK.AngleXZM(rP.D0, rP.D);
  FrPsi := pi / 2 + FrPsi + FrAlpha;
  { Wanten2dTo3d; Wantenlängen3d bleiben unverändert }
  Rest;
end;

procedure TRigg2.GetWantenspannung;
var
  VorstagNull: single;
begin
  VorstagNull := GetVorstagNull;
  FrVorstagDiff := VorstagNull - FrVorstag;
  if VorstagDiff < 0 then
    FrSpannungW := 0
  else
    FrSpannungW := WantenKraftvonVorstag(VorstagDiff);
  Wantenspannung := SpannungW;
end;

function TRigg2.WantenKraftvonVorstag(WegSoll: single): single;
{ liefert Wantenspannung 3D in Abhängigkeit von der Auslenkung des Vorstags }
begin
  result := TrimmTab.EvalX(WegSoll);
end;

function TRigg2.GetVorstagNull: single;
var
  Temp, TempP, TempD, TempC: TPoint3D;
  s: string;
  WStrich, WStrich2d: single;
begin
  result := 0;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;

      case SalingTyp of
        stFest:
          begin
            { 1. Aufruf SchnittKK: Saling2d und WanteOben2d;
              Schnittpunkt Temp wird im 2. Aufruf benötigt }
            Radius1 := FrSalingH;
            Radius2 := FrWoben2D;
            Temp := TPoint3D.Zero;
            Temp.X := FrMastUnten;
            MittelPunkt1 := Temp;
            { Temp := Null; }
            Temp.X := FrMastUnten + FrMastOben;
            MittelPunkt2 := Temp;
            Temp := SchnittPunkt1;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [1, s]);
            LogList.Add(s);

            { 2. Aufruf SchnittKK: TempP ermitteln }
            Radius1 := FrWunten2D;
            Radius2 := Temp.Length; { Temp unter 1. ermittelt }
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempP := SchnittPunkt1;
            TempP.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [2, s]);
            LogList.Add(s);

            { 3. Aufruf SchnittKK: Saling2d und MastUnten; TempD ermitteln }
            Radius1 := FrSalingH;
            Radius2 := FrMastUnten;
            MittelPunkt1 := TempP;
            MittelPunkt2 := rP.D0;
            TempD := SchnittPunkt1;
            TempD.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [3, s]);
            LogList.Add(s);

            { 4. Aufruf SchnittKK: WanteOben2d und MastOben; TempC ermitteln }
            Radius1 := FrWoben2D;
            Radius2 := FrMastOben;
            MittelPunkt1 := TempP;
            MittelPunkt2 := TempD;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullFest, [4, s]);
            LogList.Add(s);

            result := rP.C0.Distance(TempC);
          end;

        stDrehbar:
          begin
            Radius1 := FrSalingL;
            Radius2 := FrWoben3D;
            TempD := TPoint3D.Zero;
            TempD.X := FrMastUnten;
            MittelPunkt1 := TempD;
            TempC := TPoint3D.Zero;
            TempC.X := FrMastUnten + FrMastOben;
            MittelPunkt2 := TempC;
            TempP := SchnittPunkt1;
            TempP.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [1, s]);
            LogList.Add(s);

            Radius1 := rP.D0.Distance(rP.A0);
            Radius2 := FrWunten3D;
            MittelPunkt1 := TPoint3D.Zero;
            MittelPunkt2 := TempP;
            Temp := SchnittPunkt1;
            Temp.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [2, s]);
            LogList.Add(s);

            WStrich := Temp.Distance(TempC);
            WStrich2d := sqrt(sqr(WStrich) - sqr(rP.A0.Y));

            Radius1 := WStrich2d;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullDrehbar, [3, s]);
            LogList.Add(s);

            result := rP.C0.Distance(TempC);
          end;

        stOhneStarr, stOhneBiegt:
          begin
            { 1. Aufruf SchnittKK: Wante2d und Mast; TempC ermitteln }
            Radius1 := FrWunten2D + FrWoben2D;
            Radius2 := FrMastUnten + FrMastOben;
            MittelPunkt1 := rP.P0;
            MittelPunkt2 := rP.D0;
            TempC := SchnittPunkt1;
            TempC.Y := 0;
            s := Bemerkung;
            s := Format(LogList_Format_String_GetVorstagNullOhne, [1, s]);
            LogList.Add(s);
            result := rP.C0.Distance(TempC);
          end;
      end;
    end;

  except
    on E: EMathError do
    begin
      s := Format(LogList_Format_String_GetVorstagNullException, [E.Message]);
      LogList.Add(s);
    end;
  end;
end;

procedure TRigg2.NeigeF(Mastfall: single);
var
  D0: TPoint3D;

  oldF: TPoint3D;
  oldC: TPoint3D;
  oldD: TPoint3D;

  newF: TPoint3D;
  newC: TPoint3D;
  newD: TPoint3D;

  D0F: single; // k3
  D0C: single; // k1 + k2
  D0D: single; // l4 (FrMastUnten)

  newF0F: single;

  oldPsi: single;
  newPsi: single;
  delta: single;
  w: single;
begin
  oldF := rp.F;
  oldC := rp.C;
  oldD := rp.D;
  D0 := rp.D0;
  D0F := rp.D0.Distance(rp.F);
  D0C := rp.D0.Distance(rp.C);
  D0D := FrMastUnten;

  { compute new Point F }

  newF0F := Mastfall + FrMastfallVorlauf;
  with SKK do
  begin
    SchnittEbene := seXZ;
    Radius1 := newF0F;
    Radius2 := D0F; // unchanged
    MittelPunkt1 := rP.F0;
    MittelPunkt2 := rP.D0;
    rP.F := SchnittPunkt1;
  end;

  { compute new Points C and D }

  newF := rp.F;
  oldPsi := Pi/2 - SKK.AngleXZ(oldF, D0);
  newPsi := Pi/2 - SKK.AngleXZ(newF, D0);
  delta := newPsi - oldPsi;

  w := Pi/2 - SKK.AngleXZ(oldC, D0);
  w := w + delta;
  newC.X := D0.X + D0C * cos(w);
  newC.Y := 0;
  newC.Z := D0.Z + D0C * sin(w);

  w := Pi/2 - SKK.AngleXZ(oldD, D0);
  w := w + delta;
  newD.X := D0.X + D0D * cos(w);
  newD.Y := 0;
  newD.Z := D0.Z + D0D * sin(w);

  rp.C := newC;
  rp.D := newD;

  { continue as in original BiegeUndNeigeF }

  FrVorstag := rP.C0.Distance(rP.C);

  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TRigg2.BiegeUndNeigeF1(Mastfall, Biegung: single);
var
  k1, k2, k3, k4, k5, k6, k7: single;
  tempAlpha, tempBeta, tempGamma: single;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { 1. Berechnung Länge D0F aus Durchbiegung }
  k1 := sqrt((sqr(FrMastUnten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastOben) - sqr(Biegung)));
  tempAlpha := arctan2(Biegung, k1);
  k4 := (k1 + k2) * sin(tempAlpha);
  k6 := (k1 + k2) * cos(tempAlpha);
  tempGamma := arctan2(k4, k6 - FrMastUnten);
  k5 := (FrMastOben + FrMastEnde) * sin(tempGamma);
  k7 := (FrMastOben + FrMastEnde) * cos(tempGamma);
  tempBeta := arctan2(k5, (FrMastUnten + k7));
  k3 := sqrt(sqr(k5) + sqr(FrMastUnten + k7));
  { oder k3 := k5 / sin(tempBeta) }
  { k3 = Abstand D0F }

  { Bessere Werte für k3 und tempBeta bestimmen }
  KorrekturF(Biegung, k1, k2, k3, tempBeta, tempGamma); { virtuelle Methode }

  { 2. Berechnung Punkt F mit Mastfall }
  rP.F := SKK.IntersectionXZ1(rP.F0, rP.D0, Mastfall + FrMastfallVorlauf, k3);

  { 3. psi, D, und C ermitteln }
  FrPsi := SKK.AngleXZM(rP.D0, rP.F);
  FrPsi := pi / 2 + FrPsi + FrAlpha - tempBeta;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);
  rP.C := SKK.AnglePointXZ(rP.D, FrMastOben, FrPsi - FrAlpha + tempGamma);

  FrVorstag := rP.C0.Distance(rP.C);

  { 4. restliche Aktualisierungen vornehmen }
  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;

  BerechneM;
end;

procedure TRigg2.BiegeUndNeigeFS(TrimmSoll: TTrimm; var SalingHStart: single);
{ var Parameter SalingHStart wird vom Regler benötigt }
var
  ooTemp: TPoint3D;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { 4. Startwert für FrSalingH ermitteln }
  ooTemp := (rP.C - rP.P0).Normalize;
  ooTemp := ooTemp * FrWoben2D;
  rP.P := rP.C + ooTemp;
  SalingHStart := rP.P.Distance(rP.D);
  FrSalingH := Trunc(SalingHStart) + 1; { FiSalingH garantiert größer }

  { 5. restliche Aktualisierungen in MakeSalingHBiggerFS vornehmen! }
  MakeSalingHBiggerFS(SalingHStart);
end;

procedure TRigg2.BiegeUndNeigeDS(TrimmSoll: TTrimm; var SalingLStart: single);
{ var Parameter SalingLStart wird vom Regler benötigt }
var
  ooTemp: TPoint3D;
begin
  BiegeUndNeigeF1(TrimmSoll.Mastfall, TrimmSoll.BiegungS);

  { Startwert für SalingL ermitteln }
  ooTemp := (rP.C - rP.A0).Normalize;
  ooTemp := ooTemp * FrWoben3D;
  rP.A := rP.C + ooTemp;
  SalingLStart := rP.A.Distance(rP.D);
  FrSalingL := Trunc(SalingLStart) + 1; { FiSalingL dann garantiert größer! }

  { restliche Aktualisierungen in MakeSalingLBiggerDS vornehmen }
  MakeSalingLBiggerDS(SalingLStart);
end;

procedure TRigg2.BiegeUndNeigeC(MastfallC, Biegung: single);
var
  k1, k2: single;
  tempAlpha: single;
begin
  ResetStatus;
  if SalingTyp = stDrehbar then
    Wanten3dTo2d;

  { Zweischlag ausgehend von Durchbiegung }
  k1 := sqrt((sqr(FrMastUnten) - sqr(Biegung)));
  k2 := sqrt((sqr(FrMastOben) - sqr(Biegung)));
  tempAlpha := arctan2(Biegung, k1);

  rP.C := SKK.IntersectionXZ1(rP.F0, rP.D0, MastfallC, k1 + k2);

  { psi und Punkt D }
  FrPsi := SKK.AngleXZM(rP.D0, rP.C);
  FrPsi := pi / 2 + FrPsi + FrAlpha - tempAlpha;

  rP.D := SKK.AnglePointXZ(rP.D0, FrMastUnten, FrPsi - FrAlpha);

  { Vorstag }
  FrVorstag := rP.C0.Distance(rP.C);

  { Punkt F, M }
  BerechneF;
  BerechneM;

  { restliche Aktualisierungen }
  case SalingTyp of
    stFest:
      MakeSalingHBiggerFS(FrSalingH);
    stDrehbar:
      MakeSalingLBiggerDS(FrSalingL);
  end;
end;

function TRigg2.GetCounterValue(Idx: Integer): Integer;
begin
  result := 0;
  case Idx of
    0: result := UpdateGetriebeCounter;
    1: result := ExitCounter1;
    2: result := ExitCounter2;
    3: result := ExitCounter3;
    4: result := ExitCounter4;
    5: result := ExitCounter5;
    6: result := ExitCounter6;
    7: result := ExitCounter7;
  end;
end;

function TRigg2.GetTempValue(Idx: Integer): single;
begin
  result := 0;
  case Idx of
    1: result := Temp1;
    2: result := Temp2;
    3: result := Temp3;
    4: result := Temp4;
  end;
end;

function TRigg2.GetTrimmTabDaten: TTrimmTabDaten;
begin
  result := TrimmTab.TrimmTabDaten;
end;

function TRigg2.GetTrimmTabelle: TTrimmTab;
begin
  result := TrimmTab;
end;

{ TMast }

procedure TRigg2.SetCalcTyp(const Value: TCalcTyp);
begin
  FCalcTyp := Value;
end;

procedure TRigg2.SetControllerTyp(const Value: TControllerTyp);
begin
  FControllerTyp := Value;
end;

procedure TRigg2.SetEI(const Value: Integer);
begin
  { EI Werte intern in Nmm^2, extern in Nm^2 }
  EI := Value * 1E6;
end;

procedure TRigg2.SetKorrigiert(const Value: Boolean);
begin
  FKorrigiert := Value;
end;

function TRigg2.GetEI: Integer;
begin
  { EI Werte intern in Nmm^2, extern in Nm^2 }
  result := Round(EI / 1E6);
end;

procedure TRigg2.Clear;
var
  i: Integer;
begin
  F1 := 0;
  F2 := 0;
  FA := 0;
  FB := 0;
  FC := 0;
  for i := 0 to FLineCountM do
    LineData[i] := 0;
end;

procedure TRigg2.ResetMastStatus;
begin
  FMastOK := True;
  Exclude(FMastStatus, msBiegungNegativ);
  Exclude(FMastStatus, msControllerJenseits);
  Exclude(FMastStatus, msZugKraftimMast);
  Exclude(FMastStatus, msControllerKraftzuGross);
end;

function TRigg2.GetMastStatusText: string;
var
  s: string;
begin
  s := Status_String_Mast;
  if FMastOK then
    s := s + Status_String_OK
  else
  begin
    if msBiegungNegativ in FMastStatus then
      s := s + Status_String_MastBiegungNegativ
    else if msControllerJenseits in FMastStatus then
      s := s + Status_String_MastControllerBeyondMiddle
    else if msZugKraftimMast in FMastStatus then
      s := s + Status_String_MastControllerTooFarBack
    else if msControllerKraftZuGross in FMastStatus then
      s := s + Status_String_MastControllerTooFar;
  end;
  result := s;
end;

function TRigg2.GetMastUnten: single;
begin
  result := FrMastUnten;
end;

procedure TRigg2.GetEpsilon;
var
  i: Integer;
  DeltaL: single;
begin
  DeltaL := lc / 100;
  epsA := arctan2(LineData[1], DeltaL);
  epsB := -arctan2(LineData[FLineCountM - 1], DeltaL);

  i := Round(le / lc * 100);
  if i < 1 then
  begin
    eps1 := 0;
    eps2 := 0;
    Exit;
  end;
  if i > FLineCountM then
  begin
    eps1 := 0;
    eps2 := 0;
    Exit;
  end;
  eps1 := arctan2((LineData[i] - LineData[i - 1]), DeltaL);

  i := Round(ld / lc * 100);
  if i < 1 then
  begin
    eps2 := 0;
    Exit;
  end;
  if i > FLineCountM then
  begin
    eps2 := 0;
    Exit;
  end;
  eps2 := arctan2((LineData[i] - LineData[i - 1]), DeltaL);
end;

procedure TRigg2.CalcW1W2;
var
  alpha11, alpha22, alpha12, alpha21: single; { in mm/N }
  a01, a02: single; { in mm/N }
  i: Integer;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3;
  alpha12 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  alpha21 := alpha12;

  { Kräfte in N }
  F1 := -(alpha12 * hd - alpha22 * he) / (alpha11 * alpha22 - alpha21 * alpha12);
  F2 := (alpha11 * hd - alpha21 * he) / (alpha11 * alpha22 - alpha21 * alpha12);
  FA := -(F1 * le + F2 * ld - lc * F1 - lc * F2) / lc;
  FB := (F1 * le + F2 * ld) / lc;

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < le then
    begin
      { Durchbiegung in Feld 1 }
      a01 := l0 * (lc - le) * (lc * lc - l0 * l0 - Sqr(lc - le)) / lc / EI / 6;
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6;
    end
    else if l0 < ld then
    begin
      { Durchbiegung in Feld 2 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6;
    end
    else
    begin
      { Durchbiegung in Feld 3 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;
      a02 := (lc - l0) * ld * (lc * lc - Sqr(lc - l0) - ld * ld) / lc / EI / 6;
    end;

    LineData[i] := a01 * F1 + a02 * F2;
  end;
  ControllerFree := False;
end;

procedure TRigg2.CalcW1;
var
  alpha11: single;
  a01: single;
  i: Integer;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;

  { Kräfte in N }
  F1 := he / alpha11;
  F2 := 0;
  FA := F1 * (lc - le) / lc;
  FB := F1 * le / lc;

  a01 := (lc - ld) * le * (lc * lc - Sqr(lc - ld) - le * le) / lc / EI / 6;
  FSalingWeg := a01 * F1;

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < le then
      { Durchbiegung in Feld 1 }
      a01 := l0 * (lc - le) * (lc * lc - l0 * l0 - Sqr(lc - le)) / lc / EI / 6
    else
      { Durchbiegung in Feld 2 }
      a01 := (lc - l0) * le * (lc * lc - Sqr(lc - l0) - le * le) / lc / EI / 6;

    LineData[i] := a01 * F1;
  end;
  ControllerFree := False;
end;

procedure TRigg2.CalcW2;
var
  alpha22: single;
  a02: single;
  i: Integer;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }

  if alpha22 = 0 then
  begin
    { Wenn F0C Position clamped because out of range (not between Min, Max) }
    F1 := 0;
    F2 := 0;
    FA := 0;
    FB := 0;
    BiegungE := 0;
    ControllerFree := True;
    Inc(ExitCounter3);
    Exit;
  end;

  { Kräfte in N }
  F1 := 0;
  F2 := hd / alpha22;
  FA := F2 * (lc - ld) / lc;
  FB := F2 * ld / lc;

  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  FControllerWeg := a02 * F2;
  if (FControllerTyp <> ctOhne) and (FControllerWeg > he) then
    Exit;
  { Weiterrechnen nur, wenn Controller nicht anliegt. }

  for i := 0 to FLineCountM do
  begin
    l0 := i * lc / FLineCountM;
    if l0 < ld then
      { Durchbiegung in Feld 1 }
      a02 := l0 * (lc - ld) * (lc * lc - l0 * l0 - Sqr(lc - ld)) / lc / EI / 6
    else
      { Durchbiegung in Feld 2 }
      a02 := (lc - l0) * ld * (lc * lc - Sqr(lc - l0) - ld * ld) / lc / EI / 6;

    LineData[i] := a02 * F2;
  end;

  { Durchbiegung an der Stelle le }
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  BiegungE := a02 * F2;
  ControllerFree := True;
end;

procedure TRigg2.CalcWante;
var
  FU1, FU2, FBekannt: single;
  l2, h, alpha: single;
begin
  FU1 := 0;
  FU2 := 0;
  GetWantenSpannung;
  { 1. Wantenkraft3Dto2D; FB ermitteln }
  h := rP.P0.Distance(rP.P);
  l2 := rL.A0B0 - rL.AB; { PüttingAbstand - SalingAbstand }
  alpha := arctan2(l2 / 2, h);
  FBekannt := WantenSpannung * cos(alpha) * 2; { Wantenspannung2d }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      { Gleichgewicht am Punkt ooP }
      {         KM    KU1   KU2   KB     FU1  FU2  FB  }
      SolveKG21(rP.P, rP.D, rP.C, rP.P0, FU1, FU2, FBekannt);
      { Winkel alpha2 ermitteln }
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta2 := SKK.AngleZXM(rP.A, rP.D);
      Beta := Gamma - pi / 2;
      alpha2 := Beta + delta2;
      F1 := 0;
      F2 := -FU1 * cos(alpha2);
      FA := F2 * (lc - ld) / lc;
      FB := F2 * ld / lc;
    end;
    stOhneBiegt:
    begin
      { Gleichgewicht am Punkt ooC }
      {         KM    KU1    KU2    KB     FU1  FU2  FB  }
      SolveKG21(rP.C, rP.D0, rP.C0, rP.P0, FU1, FU2, FBekannt);
      F1 := 0;
      F2 := 0;
      FA := 0;
      FB := 0;
      FC := FU1;
    end;
  end;
end;

procedure TRigg2.GetSalingWeg;
{ aus CalcW1 abgeleitet. Ermittelt die Durchbiegung hd, wenn he vorgegeben ist
  und die Salingkraft F2 Null ist. }
var
  alpha11, tempF1, a01: single;
begin
  alpha11 := le * le * Sqr(lc - le) / lc / EI / 3;
  FControllerAlpha := alpha11; { im mm/N, wird in CalcWKnick gebraucht! }
  tempF1 := he / alpha11;
  a01 := (lc - ld) * le * (lc * lc - Sqr(lc - ld) - le * le) / lc / EI / 6;
  FSalingWeg := a01 * tempF1; { in mm }
end;

function TRigg2.GetCalcTyp: TCalcTyp;
begin
  result := FCalcTyp;
end;

function TRigg2.GetControllerTyp: TControllerTyp;
begin
  result := FControllerTyp;
end;

procedure TRigg2.GetControllerWeg;
{ aus CalcW2 abgeleitet. Ermittelt die Durchbiegung he, wenn hd vorgegeben ist
  und die Controllerkraft F1 Null ist. }
var
  alpha22, tempF2, a02: single;
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  FSalingAlpha := alpha22; { im mm/N, wird in WvonF gebraucht! }
  tempF2 := hd / alpha22;
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;
  FControllerWeg := a02 * tempF2; { in mm }
end;

procedure TRigg2.GetSalingWegKnick;
var
  Zaehler: Integer;
  WegDiff, WegIst, WegSoll: single; { in mm } { steht für FControllerWeg }
  Temp, TempA, TempB: single; { in mm } { steht für FSalingWegKnick }
  Kraft, alpha22, a02: single; { Zwischenwerte }
begin
  alpha22 := ld * ld * Sqr(lc - ld) / lc / EI / 3; { in mm/N }
  a02 := le * (lc - ld) * (lc * lc - le * le - Sqr(lc - ld)) / lc / EI / 6;

  if he < 0 then
    Exit;

  WegSoll := he;
  Zaehler := 0;
  TempA := 0; { linke Begrenzung für FSalingWegKnick in mm }
  TempB := 1000; { rechte Begrenzung für FSalingWegKnick in mm }
  repeat
    Zaehler := Zaehler + 1;
    temp := (TempA + TempB) / 2;
    { zwei Zeilen aus GetControllerWeg }
    Kraft := temp / alpha22; { in N }
    WegIst := a02 * Kraft; { in mm } { FControllerWeg }

    WegDiff := WegIst - WegSoll; { Diff in mm }
    if WegDiff > 0 then
      TempB := temp
    else
      TempA := temp;
  until (abs(WegDiff) < 0.1) or (Zaehler = 100); { kleiner 0.1 mm }

  FSalingWegKnick := Temp;
end;

function TRigg2.GetStabKraefte: TRiggRods;
begin
  result := rF;
end;

function TRigg2.GetStabKraft(Value: TRiggRod): single;
begin
  result := rF.Rod[Value];
end;

procedure TRigg2.CalcWKnick;
var
  Kurve: TKurvenTyp;
  WSoll: single; { in mm }
  Mastdruck: single; { in N }
  ControllerKraft: single; { in N }
  SalingKraft: single; { in N }
begin
  ResetMastStatus;
  GetControllerWeg; { berechnet FControllerWeg und FSalingAlpha }
  GetSalingWeg; { berechnet FSalingWeg und FControllerAlpha }

  FKnicklaenge := lc;
  FXpos := ld;
  FKoppelFaktor := GetKoppelFaktor; { wird in FvonW --> WvonF schon gebraucht }
  GetSalingWegKnick;

  if hd < 0 then
  begin
    FMastOK := False;
    Include(FMastStatus, msBiegungNegativ);
  end;

  if ControllerTyp <> ctOhne then
    if ((hd > 0) and (he < 0)) or ((hd < 0) and (he > 0)) then
    begin
      FMastOK := False;
      Include(FMastStatus, msControllerJenseits);
    end;

  { zutreffende Knickkurve bestimmen }
  Kurve := KurveOhneController;
  case FControllerTyp of
    ctOhne:
      Kurve := KurveOhneController;
    ctDruck:
      begin
        Kurve := KurveOhneController;
        if FSalingWegKnick < hd then
          Kurve := KurveMitController;
    end;
    ctZugDruck:
      Kurve := KurveMitController;
  end;

  { FwSchnittOffset für Knickkurve festlegen }
  if Kurve = KurveOhneController then
    FwSchnittOffset := 0
  else if Kurve = KurveMitController then
  begin
    FwSchnittOhne := FSalingWegKnick;
    FSchnittPunktKraft := FvonW(FwSchnittOhne, KurveOhneController, FKorrigiert);
    FwSchnittMit := WvonF(FSchnittPunktKraft, KurveMitController, False);
    FwSchnittOffset := FwSchnittOhne - FwSchnittMit;
  end;

  { MastDruck bestimmen }
  WSoll := hd - FwSchnittOffset;
  if WSoll < 0 then
  begin
    FMastOK := False;
    Include(FMastStatus, msZugKraftimMast);
  end;
  if Kurve = KurveOhneController then
    MastDruck := FvonW(WSoll, Kurve, FKorrigiert)
  else
    MastDruck := FvonW(WSoll, Kurve, False);

  { Controllerkraft ermitteln }
  if Kurve = KurveOhneController then
    ControllerKraft := 0
  else
  begin
    ControllerKraft := (FControllerWeg - he) / FControllerAlpha;
    if abs(ControllerKraft) > 1000 then
    begin
      ControllerKraft := 1000;
      FMastOK := False;
      Include(FMastStatus, msControllerKraftzuGross);
    end;
  end;

  { Salingkraft ermitteln }
  SalingKraft := FKoppelFaktor * MastDruck;

  if SalingTyp <> stOhneBiegt then
  begin
    F1 := -ControllerKraft;
    F2 := SalingKraft;
    FA := -(F1 * le + F2 * ld - lc * F1 - lc * F2) / lc;
    { Mastfuß ohne Einfluß der Druckkraft im Mast }
    FB := (F1 * le + F2 * ld) / lc;
    { Wantangriffspunkt ohne Einfluß der Druckkraft im Mast }
    FC := -MastDruck;
    { Druckkraft ist negativ. FC wird nur bei stOhneBiegt verwendet,
      die Druckkraft im Mast ergibt sich sonst über den Umweg der Salingkraft. }
  end;

  if SalingTyp = stOhneBiegt then
  begin
    F1 := -ControllerKraft;
    F2 := 0; { Salingkraft, hier immer Null }
    FA := F1 * (lc - le) / lc; { Mastfuß ohne Einfluß von FC }
    FB := F1 * le / lc; { Wantangriffspunkt ohne Einfluß von FC }
    FC := -MastDruck; { neg. Vorzeichen, da Druckkraft }
  end;
end;

function TRigg2.FvonW(WSoll: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
{ WSoll in mm, result in N }
var
  Zaehler: Integer;
  Knicklaenge: single; { in mm }
  Diff, WIst: single; { in mm }
  FTemp, FTempA, FTempB: single; { in N }
begin
  result := 0;
  if WSoll < 0 then
    Exit;
  { Die Kraft numerisch ermitteln, da die Umkehrfunktion zu WvonF noch nicht
    explizit vorliegt. }
  Knicklaenge := FKnicklaenge;
  if Kurve = KurveMitController then
    Knicklaenge := FKorrekturFaktor * FKnicklaenge;

  Zaehler := 0;
  FTempA := 0; { in N }
  FTempB := EI * 3.14 * 3.14 / Knicklaenge / Knicklaenge; { Knicklast in N }
  repeat
    Zaehler := Zaehler + 1;
    FTemp := (FTempA + FTempB) / 2;
    WIst := WvonF(FTemp, Kurve, Korrigiert); { WIst in mm }
    Diff := WIst - WSoll; { Diff in mm }
    if Diff > 0 then
      FTempB := FTemp
    else
      FTempA := FTemp;
  until (abs(Diff) < 0.1) or (Zaehler = 100);

  result := FTemp;
end;

function TRigg2.WvonF(f: single; Kurve: TKurvenTyp; Korrigiert: Boolean): single;
{ F in N, result in mm }
var
  k: single;
  Knicklaenge: single;
begin
  Knicklaenge := FKnicklaenge;
  if Kurve = KurveMitController then
    Knicklaenge := FKorrekturFaktor * FKnicklaenge;

  k := sqrt(f / EI); { k in mm^-1, F in N, EI in Nmm^2 }
  result := FExcenter * (tan(k * Knicklaenge / 2) * sin(k * FXpos) + cos(k * FXpos) - 1);

  { Jetzt noch berücksichtigen, daß Auslenkung des Puntes D teilweise durch
    die Salingkraft bedingt ist. }
  if Korrigiert = True then
    result := result + FKoppelFaktor * f * FSalingAlpha;
end;

function TRigg2.GetKoppelFaktor: single;
var
  FU1, FU2, FB: single;
begin
  FU1 := 0;
  FU2 := 0;
  result := 0;
  case SalingTyp of
    stOhneStarr, stOhneBiegt:
      result := 0;
    stFest, stDrehbar:
    begin
      FB := 1; { bekannte Kraft vom Betrag 1 im Mast }
      (*
      KM  betrachteter Knoten
          KU1 Knoten der zur 1. unbekannten Stabkraft FU1 gehört
          KU2 Knoten der zur 2. unbekannten Stabkraft FU2 gehört
          KB  Knoten der zur bekannten Stabkraft FB gehört
                KM    KU1    KU2   KB     FU1  FU2  FB *)
      SolveKG21(rP.C, rP.C0, rP.P, rP.D0, FU1, FU2, FB);

      FB := FU2;
      {         KM    KU1   KU2    KB    FU1  FU2  FB  }
      SolveKG21(rP.P, rP.D, rP.P0, rP.C, FU1, FU2, FB);
        result := FU1; { selbe Einheit wie FB }
    end;
  end;
end;

function TRigg2.GetKorrigiert: Boolean;
begin
  result := FKorrigiert;
end;

procedure TRigg2.SolveKG21(KM, KU1, KU2, KB: TPoint3D; var FU1, FU2, FB: single);
var
  DX1, DY1, W1: single;
  DX2, DY2, W2: single;
  DX3, DY3, W3: single;
  D, D1, D2: single;
  BekanntFX, BekanntFY: single;
  s: string;
begin
  W1 := -1;
  W2 := -1;
  W3 := -1;
  D := -1;
  try
    { unbekannte Kraft Nr.1 }
    DX1 := KU1.X - KM.X; { delta x }
    DY1 := KU1.Z - KM.Z; { delta y }
    W1 := sqrt(Sqr(DX1) + Sqr(DY1)); { Stablänge }
    { unbekannte Kraft Nr.2 }
    DX2 := KU2.X - KM.X; { delta x }
    DY2 := KU2.Z - KM.Z; { delta y }
    W2 := sqrt(Sqr(DX2) + Sqr(DY2)); { Stablänge }
    { bekannte Kraft Nr.3 }
    DX3 := KB.X - KM.X; { delta x }
    DY3 := KB.Z - KM.Z; { delta y }
    W3 := sqrt(Sqr(DX3) + Sqr(DY3)); { Stablänge }
    { Summe der bekannten Kräfte }
    { mit DX/W = cos alpha, DY/W = sin alpha }
    BekanntFX := -FB * DX3 / W3;
    BekanntFY := -FB * DY3 / W3;
    { Ausrechnen der Stabkräfte }
    D := DX1 * DY2 - DX2 * DY1;
    if D = 0 then
    begin
      { we do not want exceptions when running in the debugger }
      FU1 := 0;
      FU2 := 0;
      Inc(ExitCounter4);
      Exit;
    end;
    D1 := BekanntFX * DY2 - BekanntFY * DX2;
    D2 := BekanntFY * DX1 - BekanntFX * DY1;
    FU1 := D1 / D * W1; { 1. neu ermittelte Stabkraft }
    FU2 := D2 / D * W2; { 2. neu ermittelte Stabkraft }
  except
    on EZeroDivide do
    begin
      { D ist Null, wenn FU1 und FU2 auf einer Geraden liegen. }
      FU1 := 0;
      FU2 := 0;
      s := LogString_SolveKG21_Except;
      if W1 = 0 then
        s := s + LogString_W1;
      if W2 = 0 then
        s := s + LogString_W2;
      if W3 = 0 then
        s := s + LogString_W3;
      if D = 0 then
        s := s + LogString_D;
      s := s + LogString_AreNull;
      Main.Logger.Info(s);
    end;
  end;
end;

procedure TRigg2.SchnittKraefte;
begin
  FanIn;

  case SalingTyp of
    stFest, stDrehbar:
    begin
      case ControllerTyp of
        ctOhne: CalcW2;
        ctDruck:
        begin
          CalcW2;
          if (FControllerWeg > he) then
            CalcW1W2;
        end;
        ctZugDruck: CalcW1W2;
      end;
      if CalcTyp = ctBiegeKnicken then
        CalcWKnick;
      if CalcTyp = ctKraftGemessen then
        CalcWante;
    end;

    stOhneStarr:
    begin
      case ControllerTyp of
        ctOhne: ; { do nothing }
        ctDruck:
          if (he < 0) then
            CalcW1;
        ctZugDruck: CalcW1;
      end;
    end;

    stOhneBiegt:
    begin
      case ControllerTyp of
        ctOhne: CalcW2;
        ctDruck:
        begin
          CalcW2;
          if (FControllerWeg > he) then
            CalcW1W2;
        end;
        ctZugDruck: CalcW1W2;
      end;
      if CalcTyp = ctKraftGemessen then
        CalcWante
      else
        CalcWKnick; { sonst immer BiegeKnicken }
    end;

  end;
  FanOut;
end;

procedure TRigg2.FanIn;
var
  SPSaling, SPController: TPoint3D;
  k1, k2, EC: single;
begin
  Abstaende;

  { Geometrie für Mastsystem }
  case SalingTyp of
    stFest, stDrehbar, stOhneBiegt:
    begin
      SchnittGG(rP.D0, rP.C, rP.P, rP.D, SPSaling);
      SchnittGG(rP.D0, rP.C, rP.E, rP.E0, SPController);
      ld := rP.D0.Distance(SPSaling);
      le := rP.D0.Distance(SPController);
      lc := rL.D0C;
      EC := rP.C.Distance(rP.E);
      hd := Hoehe(lc - 0.0001, rL.D0D, rL.DC, k2);
      he := Hoehe(lc - 0.0001, rL.D0E, EC, k1);
      if SPSaling.X - rP.D.X > 0 then
        hd := -hd;
      if SPController.X - rP.E.X > 0 then
        he := -he;
    end;

    stOhneStarr:
    begin
      SchnittGG(rP.D0, rP.C, rP.E, rP.E0, SPController);
      ld := rL.D0D;
      le := rP.D0.Distance(SPController);
      lc := rL.D0C;
      EC := rP.C.Distance(rP.E);
      hd := 0; { Null gesetzt, da nicht relevant }
      he := Hoehe(lc - 0.0001, rL.D0E, EC, k1);
      if SPController.X - rP.E.X > 0 then
        he := -he;
    end;
  end;

  Clear; { bei ctOhne wird hier die Mastlinie genullt }
end;

procedure TRigg2.FanOut;
begin
  { Winkel }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta1 := SKK.AngleZXM(rP.E, rP.C0);
      delta2 := SKK.AngleZXM(rP.A, rP.D);
      Beta := Gamma - pi / 2;
      alpha1 := Beta + delta1;
      alpha2 := Beta + delta2;
    end;

    stOhneStarr, stOhneBiegt:
    begin
      Gamma := pi / 2 - SKK.AngleXZ(rP.C, rP.D0);
      delta1 := SKK.AngleZXM(rP.E, rP.C0);
      delta2 := 0; { Null gesetzt, da nicht relevant }
      Beta := Gamma - pi / 2;
      alpha1 := Beta + delta1;
      alpha2 := 0; { Null gesetzt, da nicht relevant }
    end;
  end;

  UpdateMastPositionE;

  { Kraftkomponenten bereitstellen }
  case SalingTyp of
    stFest, stDrehbar:
    begin
      try
        FE := F1 / cos(alpha1);
        FD := F2 / cos(alpha2);
      except
        on EZeroDivide do
        begin
          FE := 0;
          FD := 0;
          Main.Logger.Info(LogString_ZeroDivideAlpha);
        end;
      end;
      FLvon1 := FE * sin(alpha1);
      FLvon2 := FD * sin(alpha2);
      FALvon12 := FLvon1 + FLvon2;

      FAx := FA * cos(Beta);
      FAy := FA * sin(Beta);
      FALx := FALvon12 * sin(Beta);
      FALy := FALvon12 * cos(Beta);

      FD0x :=  FAx + FALx;
      FD0y := -FAy + FALy;
      FCx := FB * cos(Beta);
      FCy := FB * sin(Beta);
      { Mastdruckkraft FC hier nicht enthalten, im Fachwerkmodul
        wird später spezielle Prozedur für Stabkräfte aufgerufen. }

      FEx := FE * cos(delta1);
      FEy := FE * sin(delta1);
      FDx := FD * cos(delta2);
      FDy := FD * sin(delta2);
    end;

    stOhneStarr, stOhneBiegt:
    begin
      try
        FE := F1 / cos(alpha1);
        FD := 0; { Null gesetzt, da nicht relevant }
      except
        on EZeroDivide do
        begin
          FE := 0;
          FD := 0;
          Main.Logger.Info(LogString_ZeroDivideAlpha);
        end;
      end;
      FLvon1 := FE * sin(alpha1);
      FLvon2 := 0; { Null gesetzt, da nicht relevant }
      FALvon12 := FLvon1 + FLvon2;

      FAx := FA * cos(Beta);
      FAy := FA * sin(Beta);
      FALx := FALvon12 * sin(Beta);
      FALy := FALvon12 * cos(Beta);

      FD0x :=  FAx + FALx;
      FD0y := -FAy + FALy;
      FCx := FB * cos(Beta);
      FCy := FB * sin(Beta);
      { Mastdruckkraft FC hier nicht enthalten,
          im Fachwerkmodul wird später spezielle Prozedur für Stabkräfte aufgerufen. }

      FEx := FE * cos(delta1);
      FEy := FE * sin(delta1);
      FDx := 0; { Null gesetzt, da nicht relevant }
      FDy := 0; { Null gesetzt, da nicht relevant }
    end;
  end;
end;

procedure TRigg2.Abstaende;
begin
  { Abstände ermitteln }
  rL.V[0] := rP.D0.Distance(rP.C);
  rL.V[1] := rP.D0.Distance(rP.C0);
  rL.V[2] := rP.B0.Distance(rP.C0);
  rL.V[3] := rP.A0.Distance(rP.C0);
  rL.V[4] := rP.B0.Distance(rP.D0);
  rL.V[5] := rP.A0.Distance(rP.D0);
  rL.V[6] := rP.A0.Distance(rP.B0);
  rL.V[7] := rP.B0.Distance(rP.B);
  rL.V[8] := rP.A0.Distance(rP.A);
  rL.V[9] := rP.B.Distance(rP.D);
  rL.V[10] := rP.A.Distance(rP.D);
  rL.V[11] := rP.A.Distance(rP.B);
  rL.V[12] := rP.B.Distance(rP.C);
  rL.V[13] := rP.A.Distance(rP.C);
  rL.V[14] := rP.C0.Distance(rP.C);
  rL.V[15] := rP.C.Distance(rP.D);
  rL.V[16] := rP.D0.Distance(rP.D);
  rL.V[17] := rP.D.Distance(rP.E);
  rL.V[18] := rP.D0.Distance(rP.E);
  rL.V[19] := rP.E0.Distance(rP.E);
end;

procedure TRigg2.KorrekturF(tempH, k1, k2: single; var k3, Beta, Gamma: single);
var
  k8, temp: single;
begin
  { überschriebene virtuelle Methode von TGetriebeFS }
  { k3 und Beta näherungsweise(!) neu bestimmen:
    dazu als erstes epsB bestimmen }
  he := 200; { Controller soll nicht anliegen }
  hd := tempH; { Durchbiegung an den Salingen }
  le := 500; { oder alter Wert le, ohne Bedeutung }
  ld := k1; { oder alter Wert ld }
  lc := (k1 + k2); { oder alter Wert lc }
  CalcW2;
  GetEpsilon;
  k8 := sin(-epsB) * FrMastEnde * 0.9; { 0.9 empirisch }
  temp := k8 * sin(Gamma - Beta); { delta k3, siehe Zeichnung }
  k3 := k3 - temp;
  Beta := Beta + k8 * cos(Gamma - Beta) / k3 * 0.6; { 0.6 empirisch }
end;

function TRigg2.GetMastLinie: TLineDataR100;
begin
  result := LineData;
end;

procedure TRigg2.UpdateMastPositionE;
var
  PositionEStrich: single;
begin
  FMastPositionE := rP.E.X - rP.D0.X;
  if not ControllerFree then
    Exit;
  PositionEStrich := -le * sin(Beta) + BiegungE * cos(Beta);
  PositionEStrich := PositionEStrich + BiegungE * tan(alpha1) * sin(Beta);
  FMastPositionE := PositionEStrich;
end;

function TRigg2.GetMastPositionE: single;
begin
  result := FMastPositionE;
end;

procedure TRigg2.UpdateMastGraph(Model: TMastGraphModel);
begin
  Model.LineData := LineData;
  Model.FLineCountM := FLineCountM;
  Model.GetriebeOK := GetriebeOK;
end;

function TRigg2.GetRiggStatusText: string;
var
  s: string;
begin
  s := Status_String_Rigg;
  if RiggOK then
    s := s + Status_String_RiggLetzteRechnungOK;
  if rsNichtEntspannbar in FRiggStatus then
    s := s + Status_String_RiggNichtEntspannbar;
  if rsWanteAufDruck in FRiggStatus then
    s := s + Status_String_RiggWanteAufDruck;
  if rsKraftZuGross in FRiggStatus then
    s := s + Status_String_RiggForceTooBig;
  Result := s;
end;

procedure TRigg2.Entlasten;
var
  i: Integer;
begin
  rLe.D0C := rL.DC + rL.D0D;
  for i := 1 to 14 do
  begin
    rLe.V[i] := rL.V[i] - rF.V[i] * rL.V[i] / rEA.V[i];
  end;
  for i := 15 to 19 do
  begin
    rLe.V[i] := rL.V[i];
  end;
end;

function TRigg2.GetEA: TRiggRods;
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    Result.V[i] := rEA.V[i] / 1000;
end;

function TRigg2.GetRiggOK: Boolean;
begin
  result := FRiggOK;
end;

procedure TRigg2.SetEA(const Value: TRiggRods);
var
  i: Integer;
begin
  { EA Werte intern in N, extern in KN }
  for i := 0 to 19 do
    rEA.V[i] := Value.V[i] * 1000;
  Fachwerk.vektorEA[3] := rEA.A0A;
  Fachwerk.vektorEA[4] := rEA.AC;
  Fachwerk.vektorEA[5] := rEA.D0C;
  Fachwerk.vektorEA[6] := rEA.C0C;
  Fachwerk.vektorEA[7] := rEA.C0D0;
  Fachwerk.vektorEA[8] := rEA.C0D0;
  Fachwerk.vektorEA[9] := rEA.C0D0;
end;

procedure TRigg2.UpdateRigg;
begin
  FRiggOK := True;
  Exclude(FRiggStatus, rsNichtEntspannbar);
  Exclude(FRiggStatus, rsWanteAufDruck);
  Exclude(FRiggStatus, rsKraftZuGross);

  case SalingTyp of
    stFest:
      begin
        Kraefte;
        Split;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoord;
      end;
    stDrehbar:
      begin
        Kraefte;
        Split;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoordDS;
      end;
    stOhneStarr, stOhneBiegt:
      begin
        KraefteOS;
        SplitOS;
        if FProbe then
          Probe;
        Entlasten;
        MakeKoordOS;
      end;
  end;

  { Mastfall }
  FTrimm.Mastfall := Round(rP.F0.Distance(rP.F)); { in mm }
  { Vorstagspannung }
  if abs(rF.C0C) < 32000 then
    FTrimm.Spannung := Round(rF.C0C) { in N }
  else
  begin
    if rF.C0C > 32000 then
      FTrimm.Spannung := 32000;
    if rF.C0C < -32000 then
      FTrimm.Spannung := -32000;
  end;
  { Biegung an den Salingen }
  FTrimm.BiegungS := Round(hd); { in mm }
  { Biegung am Controller }
  FTrimm.BiegungC := Round(he); { in mm }
  { "Elastizität" }
  FTrimm.FlexWert := Round(rP.C.Distance(rPe.C)); { in mm }
end;

procedure TRigg2.Kraefte;
begin
  { Kräfte für Verwendung in Probe speichern: }
  KnotenLastD0.X := FD0x; { in N }
  KnotenLastD0.Y := 0;
  KnotenLastD0.Z := -FD0y;

  KnotenLastC.X := FCx;
  KnotenLastC.Y := 0;
  KnotenLastC.Z := FCy;

  with Fachwerk do
  begin
    { neue Kräfte in N }
    FS1 := ClearVektorS;
    FX := ClearVektorK;
    FY := ClearVektorK;
    FX[1] := -FEx; { Mastcontrollerkraft }
    FY[1] := FEy;
    FX[2] := -FDx; { Salingkraft }
    FY[2] := FDy;
    FX[3] := FCx; { FB vom Mast }
    FY[3] := FCy;
    FX[5] := FD0x; { FA vom Mast }
    FY[5] := -FD0y;

    { neue Geometrie }
    KX[1] := rP.E.X; { Angriffspunkt Mastcontroller }
    KY[1] := rP.E.Z;
    KX[2] := rP.A.X; { Saling }
    KY[2] := rP.A.Z;
    KX[3] := rP.C.X; { Angriffspunkt Wante }
    KY[3] := rP.C.Z;

    KX[4] := rP.C0.X;
    KY[4] := rP.C0.Z;
    KX[5] := rP.D0.X;
    KY[5] := rP.D0.Z;
    KX[6] := rP.A0.X;
    KY[6] := rP.A0.Z;

    ActionF; { Fachwerk berechnen }

    if Fachwerk.FS[3] < 0 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsWanteAufDruck);
      LogList.Add(LogList_String_WanteAufDruck);
    end;
  end;
end;

procedure TRigg2.Split;
var
  P0P, PC, PD, P0C0, P0D0: single;
  j: Integer;
begin
  { Laengen bereitstellen }
  P0C0 := rP.P0.Distance(rP.C0);
  PC := rP.P.Distance(rP.C);
  PD := rP.P.Distance(rP.D);
  P0P := rP.P0.Distance(rP.P);
  P0D0 := rP.P0.Distance(rP.D0);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF.B0C0 := F1;
    rF.A0C0 := F1;
    if abs(l1 - rL.B0C0) > 0.01 then
      LogList.Add(LogList_String_LengthDeviation);

    { Punkt D0 }
    h := P0D0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF.B0D0 := F1;
    rF.A0D0 := F1;

    { Punkte A, B }
    h := P0P;
    l2 := rL.A0B0 - rL.AB; { PüttingAbstand - SalingAbstand }
    F := Fachwerk.FS[3];
    SplitCalc;
    rF.B0B := F1;
    rF.A0A := F1;

    { Punkt D }
    h := PD;
    l2 := rL.AB; { SalingAbstand }
    F := -FD;
    SplitCalc;
    rF.BD := F1;
    rF.AD := F1;

    { Punkt C }
    h := PC;
    l2 := rL.AB; { SalingAbstand }
    F := Fachwerk.FS[4];
    SplitCalc;
    rF.BC := F1;
    rF.AC := F1;
  end;

  with Fachwerk do
  begin
    rF.D0C := FS[5];
    rF.C0D0 := FS[8];
    rF.C0C := FS[6];
    rF.DC := 0;
    rF.D0D := 0;
    rF.ED := 0;
    rF.D0E := FS[2];
    rF.E0E := FS[1];
  end;

  with TetraF do
  begin
    d1 := rP.A - rP.A0;
    d2 := rP.C0 - rP.A0;
    d3 := rP.D0 - rP.A0;
    d4 := rP.B0 - rP.A0;
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL.A0A;
    l2 := rL.A0C0;
    l3 := rL.A0D0;
    F1 := rF.A0A;
    F2 := rF.A0C0;
    F3 := rF.A0D0;
    VierteKraft;
    rF.A0B0 := F4;

    d1 := rP.A0 - rP.A;
    d2 := rP.C - rP.A;
    d3 := rP.D - rP.A;
    d4 := rP.B - rP.A;
    l1 := rL.A0A;
    l2 := rL.AC;
    l3 := rL.AD;
    F1 := rF.A0A;
    F2 := rF.AC;
    F3 := rF.AD;
    VierteKraft;
    rF.AB := F4;
  end;

  for j := 0 to 19 do
  begin
    if abs(rF.V[j]) > 32000 then
    begin
      FRiggOK := False;
      Include(FRiggStatus, rsKraftZuGross);
      LogList.Add(Format(LogList_Format_String_BetragTooBig, [j]));
    end;
  end;
end;

procedure TRigg2.Probe;

  function Probe(o, a, b, c, d: TRiggPoint; al, bl, cl, dl: Integer): Boolean;
  begin
    TetraF.d1 := rP.V[a] - rP.V[o];
    TetraF.d2 := rP.V[b] - rP.V[o];
    TetraF.d3 := rP.V[c] - rP.V[o];
    TetraF.d4 := rP.V[d] - rP.V[o];

    TetraF.l1 := rL.V[al];
    TetraF.l2 := rL.V[bl];
    TetraF.l3 := rL.V[cl];
    TetraF.l4 := rL.V[dl];

    TetraF.F1 := rF.V[al];
    TetraF.F2 := rF.V[bl];
    TetraF.F3 := rF.V[cl];
    TetraF.F4 := rF.V[dl];
    Result := TetraF.Probe; { Aufruf von Probe in class TetraF }
  end;

var
  tempResult: single;
  temptest: Boolean;
  test: Boolean;

begin
  test := True;
  if (SalingTyp = stFest) or (SalingTyp = stDrehbar) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA0], tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    temptest := Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB0], tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0.X := rF.E0E * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF.E0E * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooD0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt A }
    temptest := Probe(ooA, ooA0, ooB, ooC, ooD, 8, 11, 13, 10);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA], tempResult]));
    test := test and temptest;
    { Probe Punkt B }
    temptest := Probe(ooB, ooA, ooB0, ooC, ooD, 11, 7, 12, 9);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB], tempResult]));
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add(LogList_String_ProbeFalsch);
      Main.Logger.Info(LogList_String_ProbeFalsch);
    end
    else
      LogList.Add(LogList_String_ProbeOK);
  end;

  if (SalingTyp = stOhneStarr) or (SalingTyp = stOhneBiegt) then
  begin
    { Probe Punkt A0 }
    temptest := Probe(ooA0, ooA, ooB0, ooC0, ooD0, 8, 6, 3, 5);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooA0], tempResult]));
    test := test and temptest;
    { Probe Punkt B0 }
    test := test and Probe(ooB0, ooA0, ooB, ooC0, ooD0, 6, 7, 2, 4);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooB0], tempResult]));
    test := test and temptest;
    { Probe Punkt C0 }
    KnotenLastC0.X := rF.E0E * -cos(delta1);
    KnotenLastC0.Y := 0;
    KnotenLastC0.Z := rF.E0E * sin(delta1);
    TetraF.KnotenLast := KnotenLastC0;
    temptest := Probe(ooC0, ooA0, ooB0, ooD0, ooC, 3, 2, 1, 14);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt D0 }
    TetraF.KnotenLast := KnotenLastD0;
    temptest := Probe(ooD0, ooA0, ooB0, ooC0, ooC, 5, 4, 1, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooD0], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;
    { Probe Punkt C }
    TetraF.KnotenLast := KnotenLastC;
    temptest := Probe(ooC, ooA, ooB, ooC0, ooD0, 13, 12, 14, 0);
    tempResult := TetraF.ProbeErgebnis;
    LogList.Add(Format(LogList_Format_String_ProbeOfPoint, [KoordTexteXML[ooC], tempResult]));
    TetraF.KnotenLast := TPoint3D.Zero;
    test := test and temptest;

    if test = False then
    begin
      FRiggOK := False;
      LogList.Add(LogList_String_ProbeFalsch);
      Main.Logger.Info(LogList_String_ProbeFalsch);
    end
    else
      LogList.Add(LogList_String_ProbeOK);
  end;
end;

procedure TRigg2.MakeRumpfKoord;
var
  r1, r2: single;
begin
  { Festpunkte übernehmen }
  rPe.D0 := rP.D0;
  rPe.E0 := rP.E0;
  rPe.F0 := rP.F0;

  if not FHullIsFlexible then
  begin
    { Rumpf steif angenommen }
    rPe.A0 := rP.A0;
    rPe.B0 := rP.B0;
    rPe.C0 := rP.C0;
    rPe.P0 := rP.P0;
    Exit;
  end;

  r2 := sqr(rLe.V[5]) - sqr(rLe.V[6] / 2);
  if (r2 < 0.1) then
  begin
    Inc(ExitCounter7);
    Exit;
  end;

  r2 := sqrt(sqr(rLe.V[5]) - sqr(rLe.V[6] / 2));
  r1 := rP.P0.Length;
  if (r1 < 0.1) or (r2 < 0.1) then
  begin
    Inc(ExitCounter5);
    Exit;
  end;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: ooP0, ooA0, ooB0 ermitteln }
      Radius1 := r1;
      Radius2 := r2;
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := rPe.D0;
      rPe.P0 := SchnittPunkt1;
      rPe.A0 := rPe.P0;
      rPe.A0.Y := -rLe.V[6] / 2;
      rPe.B0 := rPe.P0;
      rPe.B0.Y := rLe.V[6] / 2;
    end;

    r1 := sqr(rLe.V[3]) - sqr(rLe.V[6] / 2);
    if (r1 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    r2 := rLe.V[1];
    if (r2 < 0.1) then
    begin
      Inc(ExitCounter7);
      Exit;
    end;

    with SKK do
    begin
      SchnittEbene := seXZ;
      { 2. Aufruf SchnittKK: ooC0 ermitteln }
      Radius1 := sqrt(r1);
      Radius2 := r2;
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C0 := SchnittPunkt1;
    end;
  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeRumpfKoordExcept + E.Message);
  end;
end;

procedure TRigg2.MakeKoord;
var
  Temp: TPoint3D;
  s: string;
  s1, s2: single;
  r1, r2: single;
begin
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    s1 := sqr(rLe.V[10]) - sqr(rLe.V[11] / 2);
    s2 := sqr(rLe.V[13]) - sqr(rLe.V[11] / 2);
    if (s1 < 0.1) or (s2 < 0.1) then
    begin
      Inc(ExitCounter6);
      Exit;
    end;
    r1 := sqrt(s1);
    r2 := sqrt(s2);
    with SKK do
    begin
      SchnittEbene := seXZ;
      { 1. Aufruf SchnittKK: Saling2d und WanteOben2d;
        Schnittpunkt Temp wird im 2. Aufruf benötigt }
      Radius1 := r1;
      Radius2 := r2;
      Temp := TPoint3D.Zero;
      Temp.X := rL.D0D;
      MittelPunkt1 := Temp;
      Temp := TPoint3D.Zero;
      Temp.X := rL.D0D + rL.DC;
      MittelPunkt2 := Temp;
      Temp := SchnittPunkt1;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [1, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { 2. Aufruf SchnittKK: WanteUnten2d und Abstand rPe.D0]..rPe.P];
        ooA, ooB, ooP ermitteln }
      Radius1 := sqrt(sqr(rLe.V[8]) - sqr((rLe.V[6] - rLe.V[11]) / 2));
      Radius2 := Temp.Length; { Temp unter 1. ermittelt }
      MittelPunkt1 := rPe.A0;
      MittelPunkt2 := rPe.D0;
      rPe.A := SchnittPunkt1;
      rPe.A.Y := -rLe.V[11] / 2;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [2, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      rPe.B := rPe.A;
      rPe.B.Y := -rPe.A.Y;
      rPe.P := rPe.A;
      rPe.P.Y := 0;

      { 3. Aufruf SchnittKK: Saling2d und MastUnten; ooD ermitteln }
      Radius1 := sqrt(sqr(rLe.V[10]) - sqr(rLe.V[11] / 2));
      Radius2 := rLe.V[16];
      MittelPunkt1 := rPe.A;
      MittelPunkt2 := rPe.D0;
      rPe.D := SchnittPunkt1;
      rPe.D.Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [3, s]);
      LogList.Add(s);

      { 4. Aufruf SchnittKK: WanteOben2d und MastOben; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe.V[13]) - sqr(rLe.V[11] / 2));
      Radius2 := rLe.V[15];
      MittelPunkt1 := rPe.A;
      MittelPunkt2 := rPe.D;
      rPe.C := SchnittPunkt1;
      rPe.C.Y := 0;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoord, [4, s]);
      LogList.Add(s);
    end;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
    rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordExept + E.Message);
  end;
end;

procedure TRigg2.MakeKoordDS;
var
  s: string;
  Temp, TempA0, TempA, TempC, TempD: TPoint3D;
  WStrich3d, WStrich2d, W1Strich, Saling1L, Skalar: single;
begin
  Temp := TPoint3D.Zero;
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;

      Radius1 := rLe.V[10]; { FrSalingL; }
      Radius2 := rLe.V[13]; { FrWoben3d; }
      TempD := TPoint3D.Zero;
      TempD.X := rLe.V[16]; { FrMastunten }
      MittelPunkt1 := TempD;
      TempC := TPoint3D.Zero;
      TempC.X := rLe.V[16] + rLe.V[15]; { FrMastunten+FrMastoben; }
      MittelPunkt2 := TempC;
      TempA := SchnittPunkt1;
      TempA.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [1, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      Radius1 := rLe.V[5]; { Abstand(rPe.D0],rPe.A0]); }
      Radius2 := rLe.V[8]; { FrWunten3d; }
      MittelPunkt1 := TPoint3D.Zero;
      MittelPunkt2 := TempA;
      TempA0 := SchnittPunkt1;
      TempA0.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [2, s]);
      LogList.Add(s);

      if Status = bmEntfernt then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      WStrich3d := TempA0.Distance(TempC);
      WStrich2d := sqrt(sqr(WStrich3d) - sqr(rPe.A0.Y));

      Radius1 := WStrich2d;
      Radius2 := rLe.V[16] + rLe.V[15]; { FrMastunten + FrMastoben; }
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C := SchnittPunkt1;
      rPe.C.Y := 0;
      s := Bemerkung;
      s := Format(LogList_FormatString_MakeKoordDS, [3, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;

      { weiter in der Ebene }
      SchnittGG(TempA0, TempC, TempD, TempA, Temp);
      { Temp enthält jetzt den Schnittpunkt der Diagonalen }
      W1Strich := TempA0.Distance(Temp);
      Saling1L := TempD.Distance(Temp);

      { weiter räumlich: }
      Skalar := W1Strich / WStrich3d;
      Temp := rPe.C - rPe.A0;
      Temp := Temp * Skalar;
      Temp := rPe.A0 + Temp;
      { Temp enthält jetzt den räumlichen Schnittpunkt der Diagonalen }

      Skalar := rLe.V[16] / (rLe.V[16] + rLe.V[15]);
      rPe.D := rPe.C - rPe.D0;
      rPe.D := rPe.D * Skalar;
      rPe.D := rPe.D0 + rPe.D;

      { Berechnung Punkt ooA }
      Skalar := rLe.V[10] / Saling1L;
      Temp := Temp - rPe.D;
      Temp := Temp * Skalar;
      rPe.A := rPe.D + Temp;

      { aktualisieren }
      rPe.P := rPe.A;
      rPe.P.Y := 0;
      rPe.B := rPe.A;
      rPe.B.Y := -rPe.A.Y;

      { Berechnung für Punkt ooF - Masttop }
      gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
      rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);
    end;

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordDSExept + E.Message);
  end;
end;

procedure TRigg2.KraefteOS;
var
  temp: single;
begin
  { für Verwendung in Probe speichern }
  KnotenLastD0.X := FD0x; { in N }
  KnotenLastD0.Y := 0;
  KnotenLastD0.Z := -FD0y;

  KnotenLastC.X := FCx;
  KnotenLastC.Y := 0;
  KnotenLastC.Z := FCy;

  with Fachwerk do
  begin
    { neue Kräfte in N }
    FS1 := ClearVektorS;
    FX := ClearVektorK;
    FY := ClearVektorK;
    FX[1] := -FEx; { Mastcontrollerkraft }
    FY[1] := FEy;
    FX[2] := 0; { Null gesetzt, da nicht relevant }
    FY[2] := 0; { Null gesetzt, da nicht relevant }
    FX[3] := FCx; { FB vom Mast }
    FY[3] := FCy;
    FX[5] := FD0x; { FA vom Mast }
    FY[5] := -FD0y;

    { neue Geometrie }
    KX[1] := rP.E.X; { Angriffspunkt Mastcontroller }
    KY[1] := rP.E.Z;
    KX[2] := rP.P.X; { Saling }
    KY[2] := rP.P.Z;
    KX[3] := rP.C.X; { Angriffspunkt Wante }
    KY[3] := rP.C.Z;

    KX[4] := rP.C0.X;
    KY[4] := rP.C0.Z;
    KX[5] := rP.D0.X;
    KY[5] := rP.D0.Z;
    KX[6] := rP.A0.X;
    KY[6] := rP.A0.Z;

    if SalingTyp = stOhneStarr then
    begin
      temp := sqrt(sqr(rL.A0A + rL.AC) - sqr(rL.A0B0 / 2));
      temp := arctan2(rL.A0B0 / 2, temp);
      WantenPower := cos(temp) * WantenSpannung * 2;
    end;
    if SalingTyp = stOhneBiegt then
      MastDruck := FC;
    ActionF;
  end;
end;

procedure TRigg2.SplitOS;
var
  P0D0, P0C, P0C0: single;
begin
  { Laengen bereitstellen }
  P0D0 := rP.P0.Distance(rP.D0);
  P0C0 := rP.P0.Distance(rP.C0);
  P0C := rP.P0.Distance(rP.C);

  { Kräfte ermitteln }
  with SplitF do
  begin
    { Punkt C0 }
    h := P0C0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[7];
    SplitCalc;
    rF.B0C0 := F1;
    rF.A0C0 := F1;

    { Punkt D0 }
    h := P0D0;
    l2 := rL.A0B0; { PüttingAbstand }
    F := Fachwerk.FS[9];
    SplitCalc;
    rF.B0D0 := F1;
    rF.A0D0 := F1;

    { Punkt C }
    h := P0C;
    l2 := rL.A0B0; { PuettingAbstand }
    if SalingTyp = stOhneStarr then
      F := Fachwerk.WantenPower;
    if SalingTyp = stOhneBiegt then
      F := Fachwerk.FS[4];
    SplitCalc;
    rF.B0B := F1;
    rF.A0A := F1;
    rF.BC := F1;
    rF.AC := F1;
  end;

  with Fachwerk do
  begin
    rF.D0C := FS[5];
    rF.C0D0 := FS[8];
    rF.BD := 0;
    rF.AD := 0;
    rF.AB := 0;
    rF.C0C := FS[6];
    rF.DC := 0;
    rF.D0D := 0;
    rF.ED := 0;
    rF.D0E := FS[2];
    rF.E0E := FS[1];
  end;

  with TetraF do
  begin
    d1 := rP.C - rP.A0;
    d2 := rP.C0 - rP.A0;
    d3 := rP.D0 - rP.A0;
    d4 := rP.B0 - rP.A0;
    { d4 wird zur Vorzeichenermittlung benötigt }
    l1 := rL.A0A + rL.AC;
    l2 := rL.A0C0;
    l3 := rL.A0D0;
    F1 := rF.A0A;
    F2 := rF.A0C0;
    F3 := rF.A0D0;
    VierteKraft;
    rF.A0B0 := F4;
  end;
end;

procedure TRigg2.MakeKoordOS;
var
  Temp: TPoint3D;
  Skalar: single;
  s: String;
begin
  MakeRumpfKoord;
  rPe.E := rP.E;
  try
    with SKK do
    begin
      SchnittEbene := seXZ;

      { 1. Aufruf SchnittKK: Wante2d und Mast; ooC ermitteln }
      Radius1 := sqrt(sqr(rLe.V[8] + rLe.V[13]) - sqr(rLe.V[6] / 2));
      Radius2 := rLe.V[15] + rLe.V[16];
      MittelPunkt1 := rPe.P0;
      MittelPunkt2 := rPe.D0;
      rPe.C := SchnittPunkt1;
      s := Bemerkung;
      s := Format(LogList_Format_String_MakeKoordOS, [1, s]);
      LogList.Add(s);

      if Status = bmK1inK2 then
      begin
        FRiggOK := False;
        Include(FRiggStatus, rsNichtEntspannbar);
      end;
    end;

    { Punkte ooA, ooB, ooD und ooP ermitteln }
    Temp := rPe.C - rPe.D0;
    Skalar := rLe.V[16] / (rLe.V[15] + rLe.V[16]); { Mastunten / Mast }
    Temp.X := Skalar * Temp.X;
    Temp.Z := Skalar * Temp.Z;
    rPe.D := rPe.D0 + Temp;

    Skalar := rLe.V[13] / (rLe.V[8] + rLe.V[13]); { Woben3d / Wante3d }
    rPe.P.X := rPe.C.X - Skalar * (rPe.C.X - rPe.P0.X);
    rPe.P.Y := 0;
    rPe.P.Z := rPe.C.Z - Skalar * (rPe.C.Z - rPe.P0.Z);

    rPe.A := rPe.P;
    rPe.A.Y := Skalar * rPe.A0.Y;
    rPe.B := rPe.A;
    rPe.B.Y := -rPe.A.Y;

    { Berechnung für Punkt ooF - Masttop }
    gammaE := pi / 2 - SKK.AngleXZ(rPe.C, rPe.D0);
    rPe.F := SKK.AnglePointXZ(rPe.D0, FrMastLength, gammaE);

  except
    on E: EMathError do
      LogList.Add(LogList_String_MakeKoordExeptOS + E.Message);
  end;
end;

{ Regeln funktioniert auch bei Durchbiegung Null, weil die
  Ermittlung der Höhe hd immer(!) eine kleine Auslenkung
  zurückliefert. }
function TRigg2.Regeln(TrimmSoll: TTrimm): Integer;

  procedure Berechnen(Antrieb: single);
  begin
    case SalingTyp of
      stFest:
        MakeSalingHBiggerFS(Antrieb);
      stDrehbar:
        MakeSalingLBiggerDS(Antrieb);
    end;
    SchnittKraefte;
    UpdateRigg;
  end;

var
  Diff, Schranke: single;
  Zaehler, ZaehlerMax: Integer;
  i: Integer;
  KraftMin, KraftMax: single;
  tempFProbe, TrimmSollOut: Boolean;
begin
  Result := 0;
  { Biegen und Neigen }
  case SalingTyp of
    stFest:
      BiegeUndNeigeFS(TrimmSoll, RGD.LimitA);
    stDrehbar:
      BiegeUndNeigeDS(TrimmSoll, RGD.LimitA);
    stOhneStarr, stOhneBiegt:
      Exit; { Regeln nur für stFest und stDrehbar }
  end;

  { mit Salinglhöhe oder Salinglänge die Kraft einstellen: }
  { Zunächst limitA und limitB bestimmen }
  case CalcTyp of
    ctBiegeKnicken:
      begin
        case SalingTyp of
          stFest:
            begin
              RGD.LimitA := GSB.SalingH.Min; { in mm }
              RGD.LimitB := GSB.SalingH.Max; { in mm }
            end;
          stDrehbar:
            begin
              RGD.LimitA := GSB.SalingL.Min; { in mm }
              RGD.LimitB := GSB.SalingL.Max; { in mm }
            end;
        end;
      end;
    ctQuerKraftBiegung:
      begin
        { limitA := limitA; } { in mm, wird in BiegeUndNeigeF ermittelt }
        RGD.LimitB := RGD.LimitA + 400; { in mm }
      end;
    ctKraftGemessen:
      begin
        { Hier nur Biegen und Neigen }
        case SalingTyp of
          stFest:
            Berechnen(FrSalingH); { Saling wiederherstellen }
          stDrehbar:
            Berechnen(FrSalingL);
        end;
        Exit; { weil Kraft schon bekannt }
      end;
  end;

  { Anfang und Ende bestimmen }
  if RGD.LimitA < RGD.LimitB then
  begin { normalerweise immer limitA < limitB }
    RGD.Anfang := RGD.LimitA;
    RGD.Ende := RGD.LimitB;
  end
  else
  begin
    RGD.Anfang := RGD.LimitB;
    RGD.Ende := RGD.LimitA;
  end;

  tempFProbe := FProbe;
  FProbe := False;

  { Mittleren Kraftwert ermitteln }
  RGD.Antrieb := RGD.Anfang + (RGD.Ende - RGD.Anfang) / 2;
  Berechnen(RGD.Antrieb);
  KraftMin := FTrimm.Spannung;
  KraftMax := KraftMin;

  { Daten für Kurve sowie KraftMin und KraftMax ermitteln }
  RGD.KurveF[0] := 0;
  RGD.KurveF[CLMax] := 2000; { Bereich zwischen 0 und 1700 immer sichtbar }
  for i := CLMax - 2 downto 6 do
  begin
    RGD.Antrieb := RGD.Anfang + (RGD.Ende - RGD.Anfang) * i / CLMax;
    Berechnen(RGD.Antrieb);
    { FTrimm.Spannung schon begrenzt auf +/- 32000 }
    if rF.C0C < KraftMin then
      KraftMin := FTrimm.Spannung;
    if rF.C0C > KraftMax then
      KraftMax := FTrimm.Spannung;
    RGD.KurveF[i] := FTrimm.Spannung;
    if Assigned(FOnRegelGrafik) then
      FOnRegelGrafik(Self);
  end;

  (*
    if TrimmSoll.Spannung < KraftMin then TrimmSoll.Spannung := Round(KraftMin)+1;
    if TrimmSoll.Spannung > KraftMax then TrimmSoll.Spannung := Round(KraftMax)-1;
  *)
  TrimmSollOut := False;
  if TrimmSoll.Spannung < KraftMin then
  begin
    RGD.TrySalingH := RGD.Anfang + (RGD.Ende - RGD.Anfang) * (CLMax - 2) / CLMax;
    TrimmSollOut := True;
  end;
  if TrimmSoll.Spannung > KraftMax then
  begin
    RGD.TrySalingH := RGD.Anfang + (RGD.Ende - RGD.Anfang) * 6 / CLMax;
    TrimmSollOut := True;
  end;
  if TrimmSollOut then
  begin
    Berechnen(RGD.TrySalingH);
    FOnRegelGrafik(Self);
    FProbe := tempFProbe;
    Exit;
  end;

  { Regelungsschleife }
  Schranke := 1; { in N }
  Zaehler := 0;
  ZaehlerMax := 20; { max. ZaehlerMax Schleifendurchläufe }
  repeat
    Inc(Zaehler);
    RGD.TrySalingH := (RGD.LimitA + RGD.LimitB) / 2;
    Berechnen(RGD.TrySalingH);
    Diff := rF.C0C - TrimmSoll.Spannung; { Abweichung der Vorstagspannung in N }
    if Diff > 0 then
      RGD.LimitA := RGD.TrySalingH
    else
      RGD.LimitB := RGD.TrySalingH;
    FOnRegelGrafik(Self);
  until (abs(Diff) < Schranke) or (Zaehler = ZaehlerMax);
  Result := Zaehler;

  FProbe := tempFProbe;
end;

procedure TRigg2.GetDefaultChartData;
var
  i: Integer;
begin
  RGD.Anfang := 0;
  RGD.Antrieb := 50;
  RGD.Ende := 100;
  RGD.LimitA := 20;
  RGD.LimitB := 80;
  for i := 0 to CLMax do
    RGD.KurveF[i] := i * (2000 div CLMax);
end;

{$ifdef MSWindows}
procedure TRigg2.WriteXml(ML: TStrings; AllTags: Boolean);
var
  Document: TRggDocument;
begin
  Document := TRggDocument.Create;
  Document.WantFestigkeitsWerteInXml := AllTags;
  Document.WantTrimmTabInXml := AllTags;
  try
    GetDocument(Document);
    Document.WriteXML(ML);
  finally
    Document.Free;
  end;
end;
{$else}
procedure TRigg2.WriteXml(ML: TStrings; AllTags: Boolean);
begin
end;
{$endif}

procedure TRigg2.WriteToDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    GetDocument(Document);
    s := ExtractFileExt(FileName);
    if s = RGI_File_Extension then
    begin
      Document.WriteToIniFile(FileName);
    end
    else if s = RGG_File_Extension then
    begin
      { write as .rgi, .rgg no longer supported }
      s := ChangeFileExt(FileName, RGI_File_Extension);
      Document.WriteToIniFile(s);
      // Document.SaveToFile(FileName);
    end;
  finally
    Document.Free;
  end;
end;

procedure TRigg2.LoadFromDocFile(FileName: string);
var
  Document: TRggDocument;
  s: string;
begin
  Document := TRggDocument.Create;
  try
    s := ExtractFileExt(FileName);
    if s = RGI_File_Extension then
    begin
      Document.LoadFromIniFile(FileName);
      SetDocument(Document);
    end;
    // if S = RGG_File_Extension then
    // begin
    //   Document.LoadFromFile(FileName);
    //   SetDocument(Document);
    // end;
  finally
    Document.Free;
  end;
end;

procedure TRigg2.Assign(Source: TRigg2);
var
  Document: TRggDocument;
begin
  if Source is TRigg2 then
  begin
    Document := TRggDocument.Create;
    try
      (Source as TRigg2).GetDocument(Document);
      SetDocument(Document);
    finally
      Document.Free;
    end;
    Exit;
  end;
//  inherited Assign(Source);
end;

procedure TRigg2.GetDocument(Doc: TRggDocument);
begin
  UpdateGSB;
  { Rigg: Typ }
  Doc.SalingTyp := SalingTyp;
  Doc.ControllerTyp := ControllerTyp;
  Doc.CalcTyp := CalcTyp;
  { Mast: Abmessungen }
  Doc.FiMastL := Round(FrMastLength);
  Doc.FiMastunten := Round(FrMastUnten);
  Doc.FiMastoben := Round(FrMastOben);
  Doc.FiMastfallVorlauf := Round(FrMastfallVorlauf);
  Doc.FiControllerAnschlag := Round(FiControllerAnschlag);
  { Rumpf: Koordinaten }
  Doc.iP := rP;
  { Festigkeitswerte }
  Doc.rEA := rEA;
  Doc.EI := EI;
  { Grenzwerte und Istwerte }
  Doc.GSB.Assign(GSB);
  { Trimmtabelle }
  Doc.TrimmTabDaten := TrimmTab.TrimmTabDaten;
end;

function TRigg2.GetDurchbiegungHD: single;
begin
  result := hd;
end;

function TRigg2.GetDurchbiegungHE: single;
begin
  result := BiegungE;
end;

function TRigg2.GetMastBeta: single;
begin
  result := Beta;
end;

function TRigg2.GetMastLC: single;
begin
  result := lc;
end;

function TRigg2.GetMastLE: single;
begin
  result := le;
end;

function TRigg2.GetMastLength: single;
begin
  result := FrMastLength;
end;

procedure TRigg2.SetDocument(Doc: TRggDocument);
var
  InputRec: TTrimmControls;
  tempManipulatorMode: Boolean;
begin
  { Mast: Abmessungen }
  FrMastLength := Doc.FiMastL;
  FrMastUnten := Doc.FiMastunten;
  FrMastOben := Doc.FiMastoben;
  FrMastfallVorlauf := Doc.FiMastfallVorlauf;
  FiControllerAnschlag := Doc.FiControllerAnschlag;
  { Rumpf: Koordinaten }
  rP := Doc.iP;
  { Festigkeitswerte }
  rEA := Doc.rEA;
  EI := Doc.EI;
  { Grenzwerte }
  GSB.Assign(Doc.GSB);
  { Trimmtabelle }
  TrimmTab.TrimmTabDaten := Doc.TrimmTabDaten;
  { Istwerte }
  InputRec.Controller := Round(Doc.GSB.Controller.Ist);
  InputRec.Winkel := Round(Doc.GSB.Winkel.Ist);
  InputRec.Vorstag := Round(Doc.GSB.Vorstag.Ist);
  InputRec.Wanten := Round(Doc.GSB.Wante.Ist);
  InputRec.Woben := Round(Doc.GSB.Woben.Ist);
  InputRec.Wunten := Round(InputRec.Wanten - InputRec.Woben);
  InputRec.SalingH := Round(Doc.GSB.SalingH.Ist);
  InputRec.SalingA := Round(Doc.GSB.SalingA.Ist);
  InputRec.SalingL := Round(Doc.GSB.SalingL.Ist);
  InputRec.Vorstag := Round(Doc.GSB.VorstagOS.Ist);
  InputRec.WPowerOS := Round(Doc.GSB.WPowerOS.Ist);
  Glieder := InputRec; { --> IntGliederToReal }
  Reset; { restliche Gleitkommawerte für Rumpf und Mast aktualisieren }

  { Rigg: Typ }
  SalingTyp := Doc.SalingTyp;
  ControllerTyp := Doc.ControllerTyp;
  CalcTyp := Doc.CalcTyp;

  tempManipulatorMode := ManipulatorMode;
  ManipulatorMode := false;
  UpdateGetriebe;
  UpdateRigg;
  ManipulatorMode := tempManipulatorMode;

  UpdateGSB;
end;

procedure TRigg2.SetDefaultDocument;
var
  Document: TRggDocument;
begin
  Document := TRggDocument.Create;
  try
   Document.GetDefaultDocument;
   SetDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TRigg2.GetRealTrimmRecord(var RealTrimm: TRealTrimm);
{ Die Funktion überprüft nicht, ob die Werte in Rigg aktualisiert sind.
  Einige Werte stehen schon nach Aufruf von UpdateGetriebe() zur Verfügung.
  Andere erst nach Aufruf von UpdateRigg(). }
begin
  { Auslenkung und Wantenspannung }
  RealTrimm.VorstagDiff := VorstagDiff;
  RealTrimm.SpannungW := SpannungW;
  with RealTrimm do
   begin
    { Mastfall }
    MastfallF0F := rP.F0.Distance(rP.F); { in mm }
    { Vorstagspannung }
    if abs(rF.C0C) < 32000 then
      SpannungV := rF.C0C { in N }
    else
    begin
      if rF.C0C > 32000 then
        SpannungV := 32000;
      if rF.C0C < -32000 then
        SpannungV := -32000;
    end;
    { Biegung an den Salingen }
    BiegungS := hd; { in mm }
    { Biegung am Controller }
    BiegungC := he; { in mm }
    { "Elastizität" }
    FlexWert := rP.C.Distance(rPe.C); { in mm }
  end;
end;

function TRigg2.GetRelaxedRiggLengths: TRiggRods;
begin
  result := rLE;
end;

function TRigg2.GetRelaxedRiggPoints: TRiggPoints;
begin
  result := rPE;
end;

function TRigg2.GetRiggPoints: TRiggPoints;
begin
  result := rP;
end;

function TRigg2.GetRealTrimm(Index: TTrimmIndex): single;
var
  temp: single;
begin
  temp := 0;
  case Index of
    tiMastfallF0F: temp := rP.F0.Distance(rP.F);
    tiMastfallF0C: temp := rP.F0.Distance(rP.C);
    tiVorstagDiff: temp := VorstagDiff;
    tiVorstagDiffE: temp := rPe.C0.Distance(rPe.C) - rL.C0C;
    tiSpannungW: temp := SpannungW;
    tiSpannungV:
      begin
        if abs(rF.C0C) < 32000 then
          temp := rF.C0C { in N }
        else
        begin
          if rF.C0C > 32000 then
            temp := 32000;
          if rF.C0C < -32000 then
            temp := -32000;
      end;
    end;
    tiBiegungS: temp := hd;
    tiBiegungC: temp := he;
    tiFlexWert: temp := rP.C.Distance(rPe.C); { in mm }
  end;
  result := temp;
end;

procedure TRigg2.SaveToFederData(fd: TRggData);
begin
  fd.A0X := Round(rP.A0.X);
  fd.A0Y := -Round(rP.A0.Y);
  fd.A0Z := Round(rP.A0.Z);

  fd.C0X := Round(rP.C0.X);
  fd.C0Y := Round(rP.C0.Y);
  fd.C0Z := Round(rP.C0.Z);

  fd.D0X := Round(rP.D0.X);
  fd.D0Y := Round(rP.D0.Y);
  fd.D0Z := Round(rP.D0.Z);

  fd.E0X := Round(rP.E0.X);
  fd.E0Y := Round(rP.E0.Y);
  fd.E0Z := Round(rP.E0.Z);

  fd.F0X := Round(rP.F0.X);
  fd.F0Y := Round(rP.F0.Y);
  fd.F0Z := Round(rP.F0.Z);

  fd.MU := Round(FrMastUnten);
  fd.MO := Round(FrMastOben);
  fd.ML := Round(FrMastLength);
  fd.MV := Round(MastfallVorlauf);
  fd.CA := Round(FiControllerAnschlag);

  fd.CPMin := Round(GSB.Controller.Min);
  fd.CPPos := Round(GSB.Controller.Ist);
  fd.CPMax := Round(GSB.Controller.Max);

  fd.SHMin := Round(GSB.SalingH.Min);
  fd.SHPos := Round(GSB.SalingH.Ist);
  fd.SHMax := Round(GSB.SalingH.Max);

  fd.SAMin := Round(GSB.SalingA.Min);
  fd.SAPos := Round(GSB.SalingA.Ist);
  fd.SaMax := Round(GSB.SalingA.Max);

  fd.SLMin := Round(GSB.SalingL.Min);
  fd.SLPos := Round(GSB.SalingL.Ist);
  fd.SLMax := Round(GSB.SalingL.Max);

  fd.VOMin := Round(GSB.Vorstag.Min);
  fd.VOPos := Round(GSB.Vorstag.Ist);
  fd.VOMax := Round(GSB.Vorstag.Max);

  fd.WIMin := Round(GSB.Winkel.Min);
  fd.WIPos := Round(GSB.Winkel.Ist);
  fd.WIMax := Round(GSB.Winkel.Max);

  fd.WLMin := Round(GSB.Wante.Min);
  fd.WLPos := Round(GSB.Wante.Ist);
  fd.WLMax := Round(GSB.Wante.Max);

  fd.WOMin := Round(GSB.Woben.Min);
  fd.WOPos := Round(GSB.Woben.Ist);
  fd.WOMax := Round(GSB.Woben.Max);

  fd.F0C := Round(RealTrimm[tiMastfallF0C]);
  fd.F0F := Round(RealTrimm[tiMastfallF0F]);
  fd.Bie := Round(RealTrimm[tiBiegungS]);
end;

procedure TRigg2.LoadFromFederData(fd: TRggData);
var
  InputRec: TTrimmControls;
  tempManipulatorMode: Boolean;
begin
  { Mast: Abmessungen }
  FrMastLength := fd.ML;
  FrMastUnten := fd.MU;
  FrMastOben := fd.MO;
  FiControllerAnschlag := fd.CA;
  MastfallVorlauf := fd.MV;

  { Rumpf: Koordinaten }

  //rP := Doc.iP;
  rP.A0.X := fd.A0X;
  rP.A0.Y := -fd.A0Y;
  rP.A0.Z := fd.A0Z;
  rP.B0.X := fd.A0X;
  rP.B0.Y := fd.A0Y;
  rP.B0.Z := fd.A0Z;
  rP.C0.X := fd.C0X;
  rP.C0.Y := fd.C0Y;
  rP.C0.Z := fd.C0Z;
  rP.D0.X := fd.D0X;
  rP.D0.Y := fd.D0Y;
  rP.D0.Z := fd.D0Z;
  rP.E0.X := fd.E0X;
  rP.E0.Y := fd.E0Y;
  rP.E0.Z := fd.E0Z;
  rP.F0.X := fd.F0X;
  rP.F0.Y := fd.F0Y;
  rP.F0.Z := fd.F0Z;
  rP.P0.X := fd.A0X;
  rP.P0.Y := 0;
  rP.P0.Z := fd.A0Z;

  rP.P.X := fd.A0X;
  rP.P.Y := 0;
  rP.P.Z := fd.A0Z;

  { Festigkeitswerte }
//  rEA := Doc.rEA;
//  EI := Doc.EI;

  { Grenzwerte }
  GSB.Controller.Min := fd.CPMin;
  GSB.Controller.Ist := fd.CPPos;
  GSB.Controller.Max := fd.CPMax;
  GSB.Winkel.Min := fd.WIMin;
  GSB.Winkel.Ist := fd.WIPos;
  GSB.Winkel.Max := fd.WIMax;
  GSB.Vorstag.Min := fd.VOMin;
  GSB.Vorstag.Ist := fd.VOPos;
  GSB.Vorstag.Max := fd.VOMax;
  GSB.Wante.Min := fd.WLMin;
  GSB.Wante.Ist := fd.WLPos;
  GSB.Wante.Max := fd.WLMax;
  GSB.Woben.Min := fd.WOMin;
  GSB.Woben.Ist := fd.WOPos;
  GSB.Woben.Max := fd.WOMax;
  GSB.SalingH.Min := fd.SHMin;
  GSB.SalingH.Ist := fd.SHPos;
  GSB.SalingH.Max := fd.SHMax;
  GSB.SalingA.Min := fd.SAMin;
  GSB.SalingA.Ist := fd.SAPos;
  GSB.SalingA.Max := fd.SAMax;
  GSB.SalingL.Min := fd.SLMin;
  GSB.SalingL.Ist := fd.SLPos;
  GSB.SalingL.Max := fd.SLMax;
  GSB.VorstagOS.Min := fd.VOMin;
  GSB.VorstagOS.Ist := fd.VOPos;
  GSB.VorstagOS.Max := fd.VOMax;
//  GSB.WPowerOS.Min := 0;
//  GSB.WPowerOS.Ist := 0;
//  GSB.WPowerOS.Max := 0;

  { Trimmtabelle }
//  TrimmTab.TrimmTabDaten := Doc.TrimmTabDaten;
  { Istwerte }
  InputRec.Controller := fd.CPPos;
  InputRec.Winkel := fd.WIPos;
  InputRec.Vorstag := fd.VOPos;
  InputRec.Wanten := fd.WLPos;
  InputRec.Woben := fd.WOPos;
  InputRec.Wunten := fd.WLPos - fd.WOPos;
  InputRec.SalingH := fd.SHPos;
  InputRec.SalingA := fd.SAPos;
  InputRec.SalingL := fd.SLPos;
  InputRec.Vorstag := fd.VOPos;
  InputRec.WPowerOS := 0;
  Glieder := InputRec; { --> IntGliederToReal }
  Reset; { restliche Gleitkommawerte für Rumpf und Mast aktualisieren }

  { Rigg: Typ }
  SalingTyp := stFest;
  ControllerTyp := ctOhne;
  CalcTyp := ctKraftGemessen;

  tempManipulatorMode := ManipulatorMode;
  ManipulatorMode := false;
  UpdateGetriebe;
  UpdateRigg;
  ManipulatorMode := tempManipulatorMode;

  UpdateGSB;
end;

procedure TRigg2.AusgabeText(ML: TStrings; WantAll: Boolean = True; WantForce: Boolean = False);
var
  tempSalingDaten: TSalingDaten;
begin
  tempSalingDaten := SalingDaten;

//  MemoPosY := SendMessage(OutputForm.DisplayMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
//  ML := OutputForm.DisplayMemo.Lines;
//  ML.BeginUpdate;
//  ML.Clear;

  { Text setzen }
//  lbMastFall := Format('Mastfall = %5.1f cm', [Rigg.Trimm.Mastfall / 10]);
//  lbSpannung := Format('Spannung = %5.0f N', [Rigg.rF[14]]);
//  lbBiegung := Format('Biegung  = %5.1f cm', [Rigg.hd / 10]);

  ML.Add('Trimm:');
  ML.Add(Format('  Mastfall F0F     = %8.0f mm', [rP.F0.Distance(rP.F)]));
  if WantForce then
  ML.Add(Format('  Vorstagspannung  = %8.0f N', [rF.C0C]));
  ML.Add(Format('  Durchbiegung hd  = %8.0f mm', [hd]));

  ML.Add('');
  ML.Add('Saling:');
  ML.Add(Format('  Saling Länge   = %6.2f mm', [tempSalingDaten.SalingL]));
  ML.Add(Format('  Saling Höhe    = %6.2f mm', [tempSalingDaten.SalingH]));
  ML.Add(Format('  Saling Abstand = %6.2f mm', [tempSalingDaten.SalingA]));
  ML.Add(Format('  Saling Winkel  = %6.2f Grad', [tempSalingDaten.SalingW]));
  ML.Add(Format('  Wanten Winkel  = %6.2f Grad', [tempSalingDaten.WantenWinkel]));
  ML.Add(Format('  Kraft Winkel   = %6.2f Grad', [tempSalingDaten.KraftWinkel]));

  ML.Add('');
  ML.Add('Winkel:');
  ML.Add(Format('  phi       = %6.2f Grad', [RadToDeg(Phi)]));
  ML.Add(Format('  psi       = %6.2f Grad', [RadToDeg(psi)]));
  ML.Add(Format('  alpha     = %6.2f Grad', [RadToDeg(alpha)]));
  ML.Add(Format('  phi-alpha = %6.2f Grad (Mast-Neigung)', [RadToDeg(Phi-alpha)]));
  ML.Add(Format('  psi-alpha = %6.2f Grad (Wanten-Neigung)', [RadToDeg(psi-alpha)]));

  ML.Add('');
  ML.Add('MastWinkel:');
  ML.Add(Format('  epsB = %6.2f Grad', [RadToDeg(epsB)]));
  ML.Add(Format('  eps2 = %6.2f Grad', [RadToDeg(eps2)]));
  ML.Add(Format('  eps1 = %6.2f Grad', [RadToDeg(eps1)]));
  ML.Add(Format('  epsA = %6.2f Grad', [RadToDeg(epsA)]));
  ML.Add(Format('  Epsilon  = %6.2f Grad', [RadToDeg(epsilon)]));

  ML.Add('');
  ML.Add('SchnittWinkel:');
  ML.Add(Format('  alpha1 = %6.2f Grad', [RadToDeg(alpha1)]));
  ML.Add(Format('  alpha2 = %6.2f Grad', [RadToDeg(alpha2)]));
  ML.Add(Format('  delta1 = %6.2f Grad', [RadToDeg(delta1)]));
  ML.Add(Format('  delta2 = %6.2f Grad', [RadToDeg(delta2)]));
  ML.Add(Format('  gamma  = %6.2f Grad', [RadToDeg(gamma)]));
  ML.Add(Format('  beta   = %6.2f Grad', [RadToDeg(beta)]));

  if not WantAll then
    Exit;

  ML.Add('');
  ML.Add('SchnittKräfte:');
  ML.Add(Format('  FC  = %8.2f N    (Mastdruckkraft)', [FC]));
  ML.Add(Format('  FB  = %8.2f N    (Wanten/Vorstag)', [FB]));
  ML.Add(Format('  F2  = %8.2f N    (Saling)', [F2]));
  ML.Add(Format('  F1  = %8.2f N    (Controller)', [F1]));
  ML.Add(Format('  FA  = %8.2f N    (Mastfuß)', [FA]));
  ML.Add(Format('  hd  = %8.2f mm   (Saling Durchbiegung)', [hd]));
  ML.Add(Format('  he  = %8.2f mm   (Controller Durchbiegung)', [he]));
  ML.Add(Format('  sd  = %8.2f mm   (hd-FSalingWegKnick)', [hd-FSalingWegKnick]));

  ML.Add('');
  ML.Add('BiegeKnicken:');
  ML.Add(Format('  KoppelFaktor       = %8.5f', [FKoppelFaktor]));
  ML.Add(Format('  SalingAlpha        = %8.5f mm/N', [FSalingAlpha]));
  ML.Add(Format('  ControllerAlpha    = %8.5f mm/N', [FControllerAlpha]));
  ML.Add(Format('  SalingWeg          = %8.2f mm', [FSalingWeg]));
  ML.Add(Format('  SalingWegKnick     = %8.2f mm', [FSalingWegKnick]));
  ML.Add(Format('  ControllerWeg      = %8.2f mm', [FControllerWeg]));
  ML.Add(Format('  FSchnittPunktKraft = %8.2f N', [FSchnittPunktKraft]));
  ML.Add(Format('  FwSchnittOhne      = %8.2f mm', [FwSchnittOhne]));
  ML.Add(Format('  FwSchnittMit       = %8.2f mm', [FwSchnittMit]));
  ML.Add(Format('  FwSchnittOffset    = %8.2f mm', [FwSchnittOffset]));

//  SendMessage(OutputForm.DisplayMemo.Handle, EM_LINESCROLL, 0, MemoPosY);
//  ML.EndUpdate;
end;

procedure TRigg2.AusgabeKommentar(ML: TStrings);
var
  temp: single;
begin
//  ML := OutputForm.KommentarMemo.Lines;
//  ML.BeginUpdate;
//  ML.Clear;

  temp := hd / 10; { Biegung in cm }
  if temp < 0 then
    ML.Add('Mastbiegung negativ!');
  if temp < 2 then
    ML.Add('Mast hat fast keine Vorbiegung.');
  if temp > 10 then
    ML.Add('Mastbiegung zu groß.');

  temp := rF.C0C; { Vorstagspannung in N }
  if temp < 800 then
    ML.Add('Vorstagspannung zu gering.');
  if temp > 2000 then
    ML.Add('Vorstagspannung zu groß.');

//  ML.EndUpdate;
end;

procedure TRigg2.InitFactArray;
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
    tempH := RggFA.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
    tempA := RggFA.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  GSB.SalingW.Ist := Round(RadToDeg(arctan2(tempH * 2, tempA)));

  GSB.MastfallF0C.Ist := RealTrimm[tiMastfallF0C];
  GSB.MastfallF0F.Ist := RealTrimm[tiMastfallF0F];
  GSB.Biegung.Ist := RealTrimm[tiBiegungS];
  GSB.D0X.Ist := rP.D0.X;

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  { speziell überschreiben }
  if WantLogoData then
  begin
    GSB.Controller.Min := 50;
    GSB.Winkel.Min := 70;
    GSB.Winkel.Max := 120;
    // GSB.Woben.Min := 2000;
    // GSB.Woben.Max := 2100;
    GSB.SalingW.Min := 40;
    GSB.SalingW.Max := 60;
    // GSB.MastfallF0F.Max := 6400;
    GSB.Biegung.Min := 0;
    GSB.Biegung.Max := 120;
  end
  else
  begin
    GSB.Controller.Min := 50;

    GSB.Wante.Min := 4020;
    GSB.Wante.Max := 4220;

    GSB.Vorstag.Min := 4200;
    GSB.Vorstag.Max := 5000;

    GSB.Winkel.Min := 80;
    GSB.Winkel.Max := 115;

    GSB.Woben.Min := 2000;
    GSB.Woben.Max := 2100;

    GSB.SalingH.Min := 170;
    GSB.SalingH.Max := 1020;

    GSB.SalingA.Min := 250;
    GSB.SalingA.Max := 1550;

    GSB.SalingL.Ist := 480;
    GSB.SalingL.Min := 240;
    GSB.SalingL.Max := 1200;

    GSB.SalingW.Min := 15;
    GSB.SalingW.Max := 87;

    GSB.D0X.Min := 2600;
    GSB.D0X.Ist := 2870;
    GSB.D0X.Max := 3300;

    GSB.MastfallF0C.Min := 4000;
    GSB.MastfallF0C.Ist := 4800;
    GSB.MastfallF0C.Max := 5100;

    GSB.MastfallF0F.Min := 5370;
    GSB.MastfallF0F.Ist := 6070;
    GSB.MastfallF0F.Max := 6570;

    GSB.MastfallVorlauf.Min := 4950;
    GSB.MastfallVorlauf.Ist := 5000;
    GSB.MastfallVorlauf.Max := 5150;

    GSB.Biegung.Min := 0;
    GSB.Biegung.Max := 500;

    GSB.ResetVolatile;
  end;
end;

procedure TRigg2.UpdateFactArray(CurrentParam: TFederParam);
var
  i: TFederParam;
  sb: TRggSB;
begin
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    case i of
      fpController:
        sb.Ist := RealGlied[fpController];
      fpWinkel:
        sb.Ist := RadToDeg(RealGlied[fpWinkel]);
      fpVorstag:
        sb.Ist := RealGlied[fpVorstag];
      fpWante:
        sb.Ist := RealGlied[fpWante];
      fpWoben:
        sb.Ist := RealGlied[fpWoben];
      fpSalingH:
        sb.Ist := RealGlied[fpSalingH];
      fpSalingA:
        sb.Ist := RealGlied[fpSalingA];
      fpSalingL:
        sb.Ist := RealGlied[fpSalingL];
      fpSalingW:
        sb.Ist := RadToDeg(arctan2(RealGlied[fpSalingH] * 2, RealGlied[fpSalingA]));
      fpMastfallF0C:
        sb.Ist := rP.F0.Distance(rP.C);
      fpMastfallF0F:
        sb.Ist := rP.F0.Distance(rP.F);
      fpBiegung:
        sb.Ist := DurchbiegungHD;
      fpD0X:
        sb.Ist := rP.D0.X;
    end;
  end;

  if CurrentParam <> fpWinkel then
  begin
    sb := GSB.Find(fpWinkel);
    sb.Ist := RadToDeg(RealGlied[fpWinkel]);
  end;
end;

procedure TRigg2.ChangeRigg(CurrentParam: TFederParam; Value: single);
var
  tempH, tempA, tempL, tempW: single;
begin
  case CurrentParam of
    fpController: RealGlied[fpController] := Value;

    fpWinkel: RealGlied[fpWinkel] := DegToRad(Value);

    fpVorstag: RealGlied[fpVorstag] := Value;
    fpWante: RealGlied[fpWante] := Value;
    fpWoben: RealGlied[fpWoben] := Value;

    fpSalingH:
    begin
      tempH := GSB.SalingH.Ist;
      tempA := GSB.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := RadToDeg(arctan2(tempH * 2, tempA));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      // SalingH no change (just changed)
      // SalingA no change (kept unchanged)
      GSB.SalingL.Ist := tempL;
      GSB.SalingW.Ist := tempW;
    end;

    fpSalingA:
    begin
      tempH := GSB.SalingH.Ist;
      tempA := GSB.SalingA.Ist;
      tempL := sqrt(sqr(tempA / 2) + sqr(tempH));
      tempW := RadToDeg(arctan2(tempH * 2, tempA));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      // SalingH no change (kept unchanged)
      // SalingA no change (just changed)
      GSB.SalingL.Ist := tempL;
      GSB.SalingW.Ist := tempW;
    end;

    fpSalingL:
    begin
      tempW := GSB.SalingW.Ist;
      tempL := GSB.SalingL.Ist;
      tempH := tempL * sin(DegToRad(tempW));
      tempA := 2 * tempL * cos(DegToRad(tempW));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      GSB.SalingH.Ist := tempH;
      GSB.SalingA.Ist := tempA;
      // SalingL no change (just changed)
      // SalingW no change (kept unchanged)
    end;

    fpSalingW:
    begin
      tempW := GSB.SalingW.Ist;
      tempL := GSB.SalingL.Ist;
      tempH := tempL * sin(DegToRad(tempW));
      tempA := 2 * tempL * cos(DegToRad(tempW));

      RealGlied[fpSalingH] := tempH;
      RealGlied[fpSalingA] := tempA;
      RealGlied[fpSalingL] := tempL;

      GSB.SalingH.Ist := tempH;
      GSB.SalingA.Ist := tempA;
      // SalingL no change
      // SalingW no change
    end;

    fpMastfallF0F:
      NeigeF(Value - GSB.MastfallVorlauf.Ist);

    fpMastfallF0C:
      BiegeUndNeigeC(Value, GSB.Biegung.Ist);

    fpMastfallVorlauf:
      MastfallVorlauf := Value;

    fpBiegung:
      BiegeUndNeigeC(GSB.MastfallF0C.Ist, Value);

    fpD0X:
      rP.D0.X := Round(Value);
  end;
end;

function TRigg2.GetPlotValue(CurrentParam: TFederParam; PlotID: Integer; x, y: single): single;
var
  tx, ty: single;
begin
  case PlotID of
    1..12:
    begin
      tx := GSB.Vorstag.Ist;
      ty := GSB.SalingL.Ist;
      RealGlied[fpVorstag] := tx + x;
      RealGlied[fpSalingA] := ty + y / 10;
      UpdateGetriebe;
      if GetriebeOK then
      begin
        result := rP.F0.Distance(rP.F);
        UpdateFactArray(CurrentParam);
      end
      else
        result := 0;
    end;
    else
      result := 0;
  end;
end;

function TRigg2.GetPoint3D(Value: TRiggPoint): TPoint3D;
begin
  result := rP.V[Value];
end;

function TRigg2.GetRelaxedPoint3D(Value: TRiggPoint): TPoint3D;
begin
  result := rPe.V[Value];
end;

function TRigg2.GetProofRequired: Boolean;
begin
  result := FProbe;
end;

function TRigg2.GetRiggDistance(Value: TRiggRod): single;
begin
  result := rL.Rod[Value];
end;

function TRigg2.GetRiggLengths: TRiggRods;
begin
  result := rL;
end;

procedure TRigg2.LoadTrimm(fd: TRggData);
var
  temp, tempH, tempA: single;
  i: TFederParam;
  sb: TRggSB;
begin
  SetDefaultDocument;
  LoadFromFederData(fd);

//  FactArray.Controller.Ist := Rigg.GSB.Controller.Ist;
//  FactArray.Winkel.Ist := Rigg.GSB.Winkel.Ist;
//  FactArray.Vorstag.Ist := Rigg.GSB.Vorstag.Ist;
//  FactArray.Wante.Ist := Rigg.GSB.Wante.Ist;
//  FactArray.Woben.Ist := Rigg.GSB.Woben.Ist;
  tempH := RggFA.SalingH.Ist;
//  FactArray.SalingH.Ist := tempH;
  tempA := RggFA.SalingA.Ist;
//  FactArray.SalingA.Ist := tempA;
//  FactArray.SalingL.Ist := Rigg.GSB.SalingL.Ist;
  GSB.SalingW.Ist := Round(RadToDeg(arctan2(tempH * 2, tempA)));

  GSB.MastfallF0C.Ist := RealTrimm[tiMastfallF0C];
  GSB.MastfallF0F.Ist := RealTrimm[tiMastfallF0F];
  GSB.Biegung.Ist := RealTrimm[tiBiegungS];
  GSB.D0X.Ist := rP.D0.X;

  fd.F0C := Round(GSB.MastfallF0C.Ist);
  fd.F0F := Round(GSB.MastfallF0F.Ist);
  fd.Bie := Round(GSB.Biegung.Ist);

  { allgemein setzen }
  for i := fpController to fpD0X do
  begin
    sb := GSB.Find(i);
    sb.SmallStep := 1;
    sb.BigStep := 10;
    temp := sb.Ist;
    sb.Min := temp - 100;
    sb.Max := temp + 100;
  end;

  GSB.Controller.Min := fd.CPMin;
  GSB.Controller.Max := fd.CPMax;

  GSB.Wante.Min := fd.WLMin;
  GSB.Wante.Max := fd.WLMax;

  GSB.Vorstag.Min := fd.VOMin;
  GSB.Vorstag.Max := fd.VOMax;

  GSB.Winkel.Min := fd.WIMin;
  GSB.Winkel.Max := fd.WIMax;

  GSB.Woben.Min := fd.WOMin;
  GSB.Woben.Max := fd.WOMax;

  GSB.SalingH.Min := fd.SHMin;
  GSB.SalingH.Max := fd.SHMax;

  GSB.SalingA.Min := fd.SAMin;
  GSB.SalingA.Max := fd.SAMax;

  GSB.SalingL.Min := fd.SLMin;
  GSB.SalingL.Max := fd.SLMax;

  GSB.SalingW.Min := fd.SWMin;
  GSB.SalingW.Max := fd.SWMax;

  GSB.D0X.Min := fd.D0X - 100;
  GSB.D0X.Max := fd.D0X + 100;

  GSB.MastfallVorlauf.Ist := fd.MV;
  GSB.MastfallVorlauf.Min := fd.MV - 100;
  GSB.MastfallVorlauf.Max := fd.MV + 100;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooC]);
//  tempH := GSB.MastfallF0C.Ist;
//  temp := tempA - tempH; // = 0
  temp := GSB.MastfallF0C.Ist;
  GSB.MastfallF0C.Min := temp - 700;
  GSB.MastfallF0C.Max := temp + 500;

//  tempA := Abstand(Rigg.rP[ooF0], Rigg.rP[ooF]);
//  tempH := GSB.MastfallF0F.Ist;
//  temp := tempA - tempH; // = 0
  temp := GSB.MastfallF0F.Ist;
  GSB.MastfallF0F.Min := temp - 700;
  GSB.MastfallF0F.Max := temp + 500;

  GSB.Biegung.Min := 0;
  GSB.Biegung.Max := 500;

  GSB.ResetVolatile;
end;

procedure TRigg2.SaveTrimm(fd: TRggData);
begin
  SaveToFederData(fd);

  fd.CPMin := Round(GSB.Controller.Min);
  fd.CPPos := Round(GSB.Controller.Ist);
  fd.CPMax := Round(GSB.Controller.Max);

  fd.SHMin := Round(GSB.SalingH.Min);
  fd.SHPos := Round(GSB.SalingH.Ist);
  fd.SHMax := Round(GSB.SalingH.Max);

  fd.SAMin := Round(GSB.SalingA.Min);
  fd.SAPos := Round(GSB.SalingA.Ist);
  fd.SaMax := Round(GSB.SalingA.Max);

  fd.SLMin := Round(GSB.SalingL.Min);
  fd.SLPos := Round(GSB.SalingL.Ist);
  fd.SLMax := Round(GSB.SalingL.Max);

  fd.SWMin := Round(GSB.SalingW.Min);
  fd.SWPos := Round(GSB.SalingW.Ist);
  fd.SWMax := Round(GSB.SalingW.Max);

  fd.VOMin := Round(GSB.Vorstag.Min);
  fd.VOPos := Round(GSB.Vorstag.Ist);
  fd.VOMax := Round(GSB.Vorstag.Max);

  fd.WIMin := Round(GSB.Winkel.Min);
  fd.WIPos := Round(GSB.Winkel.Ist);
  fd.WIMax := Round(GSB.Winkel.Max);

  fd.WLMin := Round(GSB.Wante.Min);
  fd.WLPos := Round(GSB.Wante.Ist);
  fd.WLMax := Round(GSB.Wante.Max);

  fd.WOMin := Round(GSB.Woben.Min);
  fd.WOPos := Round(GSB.Woben.Ist);
  fd.WOMax := Round(GSB.Woben.Max);
end;

procedure TRigg2.SetMastLineData(const Value: TLineDataR100; L: single; Beta: single);
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
    FMastKurve[j].X := rP.D0.X - tempL * temp1 + Value[k] * temp2;
    FMastKurve[j].Y := 0;
    FMastKurve[j].Z := rP.D0.Z + tempL * temp3 + Value[k] * temp4;
  end;
end;

function TRigg2.GetMastKurve: TMastKurve;
begin
  SetMastLineData(MastLinie, MastLC, MastBeta);
  result := FMastKurve;
end;

function TRigg2.GetMastKurvePoint(const Index: Integer): TPoint3D;
begin
  if (Index >= 0) and (Index < Length(FMastKurve)) then
    result := FMastKurve[Index]
  else
  begin
    result := TPoint3D.Zero;
  end;
end;

function TRigg2.FindBogenIndexOf(P: TPoint3D): Integer;
var
  i, j: Integer;
  MinIndex: Integer;
  MinAbstand: single;
  a: single;
begin
  j := Length(FMastKurve);
  MinIndex := j div 2;
  MinAbstand := 1000;
  for i := 0 to j - 1 do
  begin
    a := (P - FMastKurve[i]).Length;
    if a < MinAbstand then
    begin
      MinAbstand := a;
      MinIndex := i;
    end;
  end;
  result := MinIndex;
end;

procedure TRigg2.ComputeKraftKurven(KK: TKraftKurven);
var
  i: Integer;
  tempHD: single;
  tempKorrigiert: Boolean;
  tempControllerTyp: TControllerTyp;
  Knicklaenge, KnickLast, Kraft, Weg: single;
begin
  { mit FSalingAlpha wird in FvonW korrigiert, daher auch in WvonF gebraucht;
    mit FControllerWeg wird in SchnittKraefte getestet, ob Controller anliegt }
  GetControllerWeg; { FSalingAlpha und FControllerWeg }
  { mit FContollerAlpha wird in CalcWKnick die ControllerKraft ausgerechnet }
  GetSalingWeg; { FControllerAlpha und FSalingWeg }
  { mit SalingWegKnick wird in CalcWKnick KurvenTyp und Offset bestimmt }
  GetSalingWegKnick; { FSalingWegKnick }
  { FKurveOhne und FKurveMit }
  for i := 0 to 150 do
  begin
    FwSchnittOhne := i; { in mm }
    FSchnittPunktKraft := FvonW(FwSchnittOhne, TKurvenTyp.KurveOhneController, False);
    KK.KurveOhne[i] := FSchnittPunktKraft; { in N }
    FwSchnittMit := i; { in mm }
    FSchnittPunktKraft := FvonW(FwSchnittMit, TKurvenTyp.KurveMitController, False);
    KK.KurveMit[i] := FSchnittPunktKraft; { in N }
  end;
  { FKurveOhneKorrigiert }
  FKnicklaenge := lc;
  KnickLast := EI * PI * PI / FKnicklaenge / FKnicklaenge; { Knicklast in N }
  for i := 0 to 100 do
  begin
    Kraft := i * 100;
    if Kraft > 0.9 * KnickLast then
      KK.KurveOhneKorrigiert[i] := 150
    else
    begin
      Weg := WvonF(Kraft, TKurvenTyp.KurveOhneController, True);
      if Weg < 150 then
        KK.KurveOhneKorrigiert[i] := Weg
      else
        KK.KurveOhneKorrigiert[i] := 150;
    end;
  end;

  { FKurveMitKorrigiert }
  Knicklaenge := FKnicklaenge * FKorrekturFaktor;
  KnickLast := EI * PI * PI / Knicklaenge / Knicklaenge; { Knicklast in N }
  for i := 0 to 100 do
  begin
    Kraft := i*100;
    if Kraft > 0.9 * KnickLast then
      KK.KurveMitKorrigiert[i] := 150
    else
    begin
      Weg := WvonF(Kraft, TKurvenTyp.KurveMitController, True);
      if Weg < 150 then
        KK.KurveMitKorrigiert[i] := Weg
      else
        KK.KurveMitKorrigiert[i] := 150;
    end;
  end;

  tempHD := hd; { hd sichern }
  tempControllerTyp := ControllerTyp;
  tempKorrigiert := Korrigiert;

  for i := 0 to 150 do
  begin
    { FVerschoben }
    Korrigiert := False;
    ControllerTyp := ctZugDruck;
    hd := i;
    CalcWKnick;
    if MastOK or KK.ShowAll then
      KK.KurveVerschoben[i] := Round(-FC) { in N }
    else
      KK.KurveVerschoben[i] := 0;

    { FVerschobenKorrigiert }
    Korrigiert := True;
    ControllerTyp := ctZugDruck;
    hd := i;
    CalcWKnick;
    if MastOK or KK.ShowAll then
      KK.KurveVerschobenKorrigiert[i] := Round(-FC) { in N }
    else
      KK.KurveVerschobenKorrigiert[i] := 0;
  end;

  { hd und FKorrigiert restaurieren }
  hd := tempHD;
  Korrigiert := tempKorrigiert;
  ControllerTyp := tempControllerTyp;
  CalcWKnick;
end;

end.

