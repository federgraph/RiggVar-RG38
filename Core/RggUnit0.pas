unit RggUnit0;

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
  System.UITypes,
  System.IniFiles,
  System.Math,
  RiggVar.RG.Def,
  RggScroll,
  RggTypes,
  RggCalc,
  RggSchnittKK,
  RggTrimmTab;

type
  TGetriebe = class(TPersistent)
  private
    FSalingTyp: TSalingTyp;
    FManipulatorMode: Boolean;
    procedure SetGlieder(Values: TTrimmControls);
    function GetGlieder: TTrimmControls;
    function GetSalingDaten: TSalingDaten;
    procedure GetLogoData;
    procedure GetDefaultData;
    procedure SetFrWinkel(const Value: double);
  protected
    FTrimm: TTrimm;
    FGetriebeOK: Boolean;
    FMastOK: Boolean;
    FGetriebeStatus: set of TGetriebeStatus;
    FrWanteZulang: double;

    FrPuettingA: double;
    FrBasis: double;
    FrController: double;
    _FrWinkel: double;
    FrVorstag: double;
    FrWunten2d: double;
    FrWunten3d: double;
    FrWoben2d: double;
    FrWoben3d: double;
    FrSalingL: double;
    FrSalingH: double;
    FrSalingA: double;
    FrMastunten: double;
    FrMastoben: double;
    FrMastEnde: double;
    FrPsi: double;
    FrPhi: double;
    FrAlpha: double;
    FrEpsilon: double;

    FiZaehler: Integer;
    FiControllerAnschlag: Integer;

    FiController: double;
    FiWinkel: double;
    FiVorstag: double;
    FiWunten3d: double;
    FiWoben3d: double;
    FiSalingL: double;
    FiSalingH: double;
    FiSalingA: double;
    FiWPowerOS: double;

    FiMastL: double;
    FiMastUnten: double;
    FiMastOben: double;
    FiMastfallVorlauf: double;

    procedure IntGliederToReal;
    procedure RealGliederToInt;
    procedure Wanten2dTo3d;
    procedure Wanten3dTo2d;
    procedure SetMastL(Value: double);
    procedure SetMastunten(Value: double);
    procedure SetMastoben(Value: double);
    function GetRealGlied(Index: TsbName): double;
    procedure SetRealGlied(Index: TsbName; Value: double);
    procedure SetSalingTyp(Value: TSalingTyp); virtual;

    procedure LoadFromIniFile(ini: TIniFile); virtual;
    procedure WriteToIniFile(ini: TIniFile); virtual;
    procedure LoadFromStream(S: TStream); virtual;
    procedure SaveToStream(S: TStream); virtual;
  public
    LogList: TStringList;
    SchnittKK: TSchnittKK;
    TrimmTab: TTrimmTab;
    GSB: TRggFA;
    iP: TIntRiggPoints;
    rP: TRealRiggPoints;

    constructor Create;
    destructor Destroy; override;

    procedure GetBuiltinData; { Integerwerte initialisieren }
    procedure Reset; { Gleitkommawerte initialisieren }
    procedure UpdateGSB;
    procedure UpdateGlieder;

    function GetriebeStatusText: string;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ManipulatorMode: Boolean read FManipulatorMode write FManipulatorMode;
    property GetriebeOK: Boolean read FGetriebeOK;

    property MastLaenge: double read FiMastL write SetMastL;
    property MastUnten: double read FiMastunten write SetMastunten;
    property MastOben: double read FiMastoben write SetMastoben;
    property MastfallVorlauf: double read FiMastfallVorlauf write FiMastfallVorlauf;

    property phi: double read FrPhi write FrPhi;
    property psi: double read FrPsi write FrPsi;
    property alpha: double read FrAlpha;
    property epsilon: double read FrEpsilon write FrEpsilon;
    property WantenSpannung: double read FiWPowerOS write FiWPowerOS;
    property ControllerAnschlag: Integer read FiControllerAnschlag write FiControllerAnschlag;

    property SalingDaten: TSalingDaten read GetSalingDaten;
    property Glieder: TTrimmControls read GetGlieder write SetGlieder;
    property RealGlied[Index: TsbName]: double read GetRealGlied write SetRealGlied;

    property FrWinkel: double read _FrWinkel write SetFrWinkel;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TGetriebe.Create;
begin
  inherited Create;
  GSB := TRggFA.Create;
  WantLogoData := false;
  LogList := TStringList.Create;
  SchnittKK := TSchnittKK.Create;
  TrimmTab := TTrimmTab.Create;
  FSalingTyp := stFest;
  FManipulatorMode := false;
  GetBuiltinData;
  IntGliederToReal;
  Reset;
end;

destructor TGetriebe.Destroy;
begin
  LogList.Free;
  SchnittKK.Free;
  TrimmTab.Free;
  GSB.Free;
  inherited Destroy;
end;

procedure TGetriebe.SetSalingTyp(Value: TSalingTyp);
begin
  if Value <> FSalingTyp then
  begin
    FSalingTyp := Value;
  end;
end;

procedure TGetriebe.SetMastunten(Value: double);
begin
  if Value <> FiMastunten then
  begin
    FiMastunten := Value;
    FrMastunten := Value;
  end;
end;

procedure TGetriebe.SetMastoben(Value: double);
begin
  if Value <> FiMastoben then
  begin
    FiMastoben := Value;
    FrMastoben := Value;
  end;
end;

procedure TGetriebe.SetMastL(Value: double);
begin
  if Value <> FiMastL then
  begin
    FiMastL := Value;
    FrMastEnde := Value - FiMastoben - FiMastunten;
  end;
end;

function TGetriebe.GetGlieder: TTrimmControls;
var
  Trimm: TTrimmControls;
begin
  RealGliederToInt;
  with Trimm do
  begin
    Controller := Round(FiController);
    Wanten := Round(FiWunten3d + FiWoben3d);
    Woben := Round(FiWoben3d);
    SalingH := Round(FiSalingH);
    SalingA := Round(FiSalingA);
    SalingL := Round(FiSalingL);
    Vorstag := Round(FiVorstag);
    Winkel := Round(FiWinkel);
    WPowerOS := Round(FiWPowerOS);
  end;
  result := Trimm;
end;

procedure TGetriebe.SetFrWinkel(const Value: double);
begin
  _FrWinkel := Value;

  if _FrWinkel > 2 * PI then
    _FrWinkel := _FrWinkel - (2 * PI);

//  while _FrWinkel > 2 * PI do
//    _FrWinkel := _FrWinkel - (2 * PI);

//  if _FrWinkel < -(2 * PI) then
//    _FrWinkel := _FrWinkel + 2 * PI;
end;

procedure TGetriebe.SetGlieder(Values: TTrimmControls);
begin
  with Values do
  begin
    FiController := Controller;
    FiWinkel := Winkel;
    FiVorstag := Vorstag;
    FiWunten3d := Wanten - Woben;
    FiWoben3d := Woben;
    FiSalingH := SalingH;
    FiSalingA := SalingA;
    FiSalingL := SalingL;
    FiWPowerOS := WPowerOS;
  end;
  IntGliederToReal;
end;

function TGetriebe.GetRealGlied(Index: TsbName): double;
begin
  result := 0;
  case Index of
    fpController: result := FrController;
    fpWinkel: result := FrWinkel;
    fpVorstag: result := FrVorstag;
    fpWante: result := FrWunten3d + FrWoben3d;
    fpWoben: result := FrWoben3d;
    fpSalingH: result := FrSalingH;
    fpSalingA: result := FrSalingA;
    fpSalingL: result := FrSalingL;
    fpVorstagOS: result := FrVorstag;
    fpWPowerOS: result := FiWPowerOS;
  end;
end;

procedure TGetriebe.SetRealGlied(Index: TsbName; Value: double);
begin
  case Index of
    fpController: FrController := Value;
    fpWinkel: FrWinkel := Value;
    fpVorstag: FrVorstag := Value;
    fpWante: FrWunten3d := Value - FrWoben3d;
    fpWoben:
      begin
        FrWunten3d := FrWunten3d + FrWoben3d - Value;
        FrWoben3d := Value;
      end;
    fpSalingH: FrSalingH := Value;
    fpSalingA: FrSalingA := Value;
    fpSalingL: FrSalingL := Value;
    fpVorstagOS: FrVorstag := Value;
    fpWPowerOS: FiWPowerOS := Round(Value);
  end;
end;

function TGetriebe.GetSalingDaten: TSalingDaten;
var
  SD: TSalingDaten;
  ooTempA, ooTempB, ooTempC: TRealPoint;
  EbeneACD, EbeneACA0: TRealPoint;
  tempWW, tempWS: double;
  tempSinus, tempCosinus: double;
begin
  ooTempA := EVektor(rP[ooA], rP[ooC]);
  ooTempB := EVektor(rP[ooA0], rP[ooA]);
  tempWW := sprod(ooTempA, ooTempB);
  try
    tempWW := arctan2(sqrt(1 - sqr(tempWW)), tempWW);
    { Wurzel ev. Null bei stOhne }
  except
    on EMathError do
      tempWW := 0;
  end;
  { ooTempA := Evektor(rP[ooA],rP[ooC]); }
  ooTempB := EVektor(rP[ooA], rP[ooD]);
  EbeneACD := vprod(ooTempA, ooTempB);

  { ooTempA := Evektor(rP[ooA],rP[ooC]); }
  ooTempB := EVektor(rP[ooA], rP[ooA0]);
  EbeneACA0 := vprod(ooTempA, ooTempB);

  ooTempA := EVektor(Null, EbeneACD);
  ooTempB := EVektor(Null, EbeneACA0);
  ooTempC := vprod(ooTempA, ooTempB);
  tempSinus := Abstand(Null, ooTempC);
  tempCosinus := sprod(ooTempA, ooTempB);

  tempWS := 0;
  try
    tempWS := arctan2(tempSinus, tempCosinus);
  except
    on EMathError do
      Main.Logger.Error('Ebenen senkrecht in GetSalingDaten!');
  end;

  with SD do
  begin
    SalingH := FrSalingH;
    SalingA := FrSalingA;
    SalingL := FrSalingL;
    SalingW := arctan2(FrSalingA / 2, FrSalingH) * 180 / pi;
    WantenWinkel := tempWW * 180 / pi;
    KraftWinkel := tempWS * 180 / pi;
  end;
  result := SD;
end;

procedure TGetriebe.IntGliederToReal;
begin
  FrController := FiController;
  FrWinkel := FiWinkel * pi / 180; { FrWinkel in Grad }
  FrVorstag := FiVorstag;
  FrWunten3d := FiWunten3d;
  FrWoben3d := FiWoben3d;
  FrSalingH := FiSalingH;
  FrSalingA := FiSalingA;
  FrSalingL := FiSalingL;

  FrMastunten := FiMastunten;
  FrMastoben := FiMastoben;
  FrMastEnde := FiMastL - FiMastoben - FiMastunten;
end;

procedure TGetriebe.RealGliederToInt;
begin
  FiController := FrController;
  FiWinkel := FrWinkel * 180 / pi; { FiWinkel in Grad }
  FiVorstag := FrVorstag;
  FiWunten3d := FrWunten3d;
  FiWoben3d := FrWoben3d;
  FiSalingH := FrSalingH;
  FiSalingA := FrSalingA;
  FiSalingL := FrSalingL;

  FiMastunten := FrMastunten;
  FiMastoben := FrMastoben;
  FiMastL := FrMastunten + FrMastoben + FrMastEnde;
end;

procedure TGetriebe.UpdateGSB;
begin
  RealGliederToInt;
  GSB.Controller.Ist := FiController;
  GSB.Winkel.Ist := FiWinkel;
  GSB.Vorstag.Ist := FiVorstag;
  GSB.Wante.Ist := FiWunten3d + FiWoben3d;
  GSB.Woben.Ist := FiWoben3d;
  GSB.SalingH.Ist := FiSalingH;
  GSB.SalingA.Ist := FiSalingA;
  GSB.SalingL.Ist := FiSalingL;
  GSB.VorstagOS.Ist := FiVorstag;
  GSB.WPowerOS.Ist := FiWPowerOS;
end;

procedure TGetriebe.UpdateGlieder;
begin
  FiController := GSB.Controller.Ist;
  FiWinkel := GSB.Winkel.Ist;
  FiVorstag := GSB.Vorstag.Ist;
  FiWunten3d := GSB.Wante.Ist - GSB.Woben.Ist;
  FiWoben3d := GSB.Woben.Ist;
  FiSalingH := GSB.SalingH.Ist;
  FiSalingA := GSB.SalingA.Ist;
  FiSalingL := GSB.SalingL.Ist;
  FiWPowerOS := GSB.WPowerOS.Ist;
  IntGliederToReal;
end;

procedure TGetriebe.Wanten2dTo3d;
begin
  FrWunten3d := sqrt(sqr(FrWunten2d) + sqr((FrPuettingA - FrSalingA) / 2));
  FrWoben3d := sqrt(sqr(FrWoben2d) + sqr(FrSalingA / 2));
end;

procedure TGetriebe.Wanten3dTo2d;
begin
  FrWunten2d := sqrt(sqr(FrWunten3d) - sqr((FrPuettingA - FrSalingA) / 2));
  FrWoben2d := sqrt(sqr(FrWoben3d) - sqr(FrSalingA / 2));
end;

function TGetriebe.GetriebeStatusText: string;
var
  S: string;
begin
  S := '  Getriebe:';
  if FGetriebeOK then
    S := S + ' O.K.'
  else
  begin
    if gsWanteZukurz in FGetriebeStatus then
      S := S + ' Wante zu kurz.'
    else if gsWanteZulang in FGetriebeStatus then
      S := S + Format(' Wante um %5.2f mm zu lang!', [FrWanteZulang])
    else if gsErrorPsivonPhi in FGetriebeStatus then
      S := S + ' Salinghöhe zu klein!';
  end;
  result := S;
end;

procedure TGetriebe.GetBuiltinData;
begin
  if WantLogoData then
    GetLogoData
  else
    GetDefaultData;
end;

procedure TGetriebe.GetDefaultData;
{ Initialisierung aller Integerwerte und der TrimmTabelle;
  nachfolgend muß IntGliederToReal und Reset aufgerufen werden, um die
  Gleitkommawerte zu initialiseieren. }
begin
  // see (update) similar code (duplication) in TRggDocument.GetDefaultDoc

  { Längen im Rigg in mm }
  FiControllerAnschlag := 50;
  FiController := 100; { Controllerposition bzw. Abstand E0-E }
  FiMastL := 6115; { Gesamtlänge Mast }
  FiMastunten := 2600; { unterer Teil Mast }
  FiMastoben := 2000; { oberer Teil Mast }
  FiMastfallVorlauf := 5000; { Abstand der Meßmarken }
  FiWunten3d := 2100; { unterer Teil Wante }
  FiWoben3d := 2020; { oberer Teil Wante }
  FiSalingH := 220; { Höhe des Salingdreiecks }
  FiSalingA := 850; { Abstand der Salingnocken }
  FiSalingL := Round(sqrt(sqr(FiSalingH) + sqr(FiSalingA / 2)));
  FiVorstag := 4500; { Vorstaglänge }
  FiWinkel := 95; { Winkel der unteren Wantabschnitte Winkel in Grad }
  FiWPowerOS := 1000; { angenommene Wantenspannung 3d }

  { RumpfKoordinaten in mm }
  iP[ooA0, x] := 2560; { Pütting Stbd }
  iP[ooA0, y] := 765;
  iP[ooA0, z] := 430;

  iP[ooB0, x] := 2560; { Püttinge Bb }
  iP[ooB0, y] := -765;
  iP[ooB0, z] := 430;

  iP[ooC0, x] := 4140; { Vorstag }
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 340;

  iP[ooD0, x] := 2870; { Mastfuß }
  iP[ooD0, y] := 0;
  iP[ooD0, z] := -100;

  iP[ooE0, x] := 2970; { Controller }
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 450;

  iP[ooF0, x] := -30; { Spiegel }
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 300;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  GSB.Controller.Ist := FiController;
  GSB.Winkel.Ist := FiWinkel;
  GSB.Vorstag.Ist := FiVorstag;
  GSB.Wante.Ist := FiWunten3d + FiWoben3d;
  GSB.Woben.Ist := FiWoben3d;
  GSB.SalingH.Ist := FiSalingH;
  GSB.SalingA.Ist := FiSalingA;
  GSB.SalingL.Ist := FiSalingL; { oben aus FiSalingH und FiSalingA errechnet }
  GSB.VorstagOS.Ist := FiVorstag;
  GSB.WPowerOS.Ist := FiWPowerOS;

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

procedure TGetriebe.GetLogoData;
{ Initialisierung aller Integerwerte und der TrimmTabelle;
  nachfolgend muß IntGliederToReal und Reset aufgerufen werden, um die
  Gleitkommawerte zu initialiseieren. }
var
  f, ox, oz: Integer;
begin
  // see similar code (duplication) in TRggDocument.GetLogoDoc

  ox := 1400;
  oz := -350;

  f := 18;

  { Längen im Rigg in mm }
  FiControllerAnschlag := 50;
  FiController := 100; { Controllerposition bzw. Abstand E0-E }
  FiMastL := Round((40 + sqrt(250) * 10) * f); { Gesamtlänge Mast }
  FiMastunten := Round((sqrt(40) + sqrt(10)) * 10 * f); { unterer Teil Mast }
  FiMastoben := Round(sqrt(40) * 10 * f); { oberer Teil Mast }
  FiMastfallVorlauf := Round(FiMastL * 0.75); { Abstand der Meßmarken }
  FiWunten3d := Round(sqrt(40) * 10 * f); { unterer Teil Wante }
  FiWoben3d := Round(sqrt(56) * 10 * f); { oberer Teil Wante }
  FiSalingH := 40 * f; { Höhe des Salingdreiecks }
  FiSalingA := 80 * f; { Abstand der Salingnocken }
  FiSalingL := Round(sqrt(sqr(FiSalingH) + sqr(FiSalingA / 2)));
  FiVorstag := Round(sqrt(288) * 10 * f); { Vorstaglänge }
  FiWinkel := Round(90 + arctan2(1, 3) * 180 / pi);
  { Winkel der unteren Wantabschnitte Winkel in Grad }
  FiWPowerOS := 1000; { angenommene Wantenspannung 3d }

  { RumpfKoordinaten in mm }
  iP[ooA0, x] := 30 * f + ox; { Pütting Stbd }
  iP[ooA0, y] := 40 * f;
  iP[ooA0, z] := 40 * f + oz;

  iP[ooB0, x] := 30 * f + ox;
  iP[ooB0, y] := -40 * f;
  iP[ooB0, z] := 40 * f + oz;

  iP[ooC0, x] := 150 * f + ox;
  iP[ooC0, y] := 0;
  iP[ooC0, z] := 40 * f + oz;

  iP[ooD0, x] := 80 * f + ox;
  iP[ooD0, y] := 0;
  iP[ooD0, z] := 10 * f + oz;

  iP[ooE0, x] := 85 * f + ox;
  iP[ooE0, y] := 0;
  iP[ooE0, z] := 50 * f + oz;

  iP[ooF0, x] := -85 * f + ox;
  iP[ooF0, y] := 0;
  iP[ooF0, z] := 40 * f + oz;

  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0;

  GSB.Controller.Ist := FiController;
  GSB.Winkel.Ist := FiWinkel;
  GSB.Vorstag.Ist := FiVorstag;
  GSB.Wante.Ist := FiWunten3d + FiWoben3d;
  GSB.Woben.Ist := FiWoben3d;
  GSB.SalingH.Ist := FiSalingH;
  GSB.SalingA.Ist := FiSalingA;
  GSB.SalingL.Ist := FiSalingL; { oben aus FiSalingH und FiSalingA errechnet }
  GSB.VorstagOS.Ist := FiVorstag;
  GSB.WPowerOS.Ist := FiWPowerOS;
  GSB.MastfallVorlauf.Ist := FiMastfallVorlauf;

  GSB.InitStepDefault;

  { Bereichsgrenzen einstellen:
    Woben2d.Min + SalingH.Min > Mastoben
    Mastunten + SalingH.Min > Abstand D0-P, daraus Winkel.Max }
  GSB.Controller.Min := 50;
  GSB.Controller.Max := 200;
  GSB.Winkel.Min := 70;
  GSB.Winkel.Max := 120;
  GSB.Vorstag.Min := FiVorstag - 20 * f;
  GSB.Vorstag.Max := FiVorstag + 0 * f;
  GSB.Wante.Min := FiWunten3d + FiWoben3d - 10 * f;
  GSB.Wante.Max := FiWunten3d + FiWoben3d + 10 * f;
  GSB.Woben.Min := FiWoben3d - 10 * f;
  GSB.Woben.Max := FiWoben3d + 10 * f;
  GSB.SalingH.Min := FiSalingH - 10 * f;
  GSB.SalingH.Max := FiSalingH + 10 * f;
  GSB.SalingA.Min := FiSalingA - 10 * f;
  GSB.SalingA.Max := FiSalingA + 10 * f;
  GSB.SalingL.Min := FiSalingL - 10 * f;
  GSB.SalingL.Max := FiSalingL + 10 * f;
  GSB.VorstagOS.Min := FiVorstag - 10 * f;
  GSB.VorstagOS.Max := FiVorstag + 10 * f;
  GSB.WPowerOS.Min := 100;
  GSB.WPowerOS.Max := 3000;
  GSB.MastfallVorlauf.Min := GSB.MastfallVorlauf.Ist - 10 * f;
  GSB.MastfallVorlauf.Max := GSB.MastfallVorlauf.Ist + 10 * f;

  TrimmTab.TrimmTabDaten := DefaultTrimmTabDaten;
end;

procedure TGetriebe.Reset;
{ Wenn die Integerwerte für Rumpf und Mast verändert wurden, dann muß Reset
  aufgerufen werden, um die Gleitkommawerte zu aktualisieren. }
var
  i: TRiggPoint;
  j: TKoord;
begin
  { Rumpfkoordinaten }
  iP[ooP0] := iP[ooA0];
  iP[ooP0, y] := 0; { diesen Integerwert hier aktualisieren }
  for i := ooA0 to ooP0 do
  begin
    for j := x to z do
    begin
      rP[i, j] := iP[i, j];
    end;
  end;
  { Mast }
  FrMastEnde := FiMastL - FiMastunten - FiMastoben;
  { Rumpflängen }
  FrPuettingA := rP[ooA0, y] - rP[ooB0, y];
  FrBasis := Abstand(rP[ooP0], rP[ooD0]);
  FrAlpha := arctan2((rP[ooP0, z] - rP[ooD0, z]), (rP[ooD0, x] - rP[ooP0, x]));
end;

procedure TGetriebe.WriteToIniFile(ini: TIniFile);
var
  S: string;
begin
  S := 'Rigg';
  ini.WriteInteger(S, 'SalingTyp', Ord(FSalingTyp));
  TrimmTab.WriteToIniFile(ini);

  S := 'Mast';
  ini.WriteInteger(S, 'MastL', Round(FiMastL));
  ini.WriteInteger(S, 'Mastunten', Round(FiMastunten));
  ini.WriteInteger(S, 'Mastoben', Round(FiMastoben));
  ini.WriteInteger(S, 'MastfallVorlauf', Round(FiMastfallVorlauf));

  S := 'Ist';
  ini.WriteInteger(S, 'Controller', Round(FiController));
  ini.WriteInteger(S, 'Winkel', Round(FiWinkel));
  ini.WriteInteger(S, 'Vorstag', Round(FiVorstag));
  ini.WriteInteger(S, 'Wante', Round(FiWunten3d + FiWoben3d));
  ini.WriteInteger(S, 'Woben', Round(FiWoben3d));
  ini.WriteInteger(S, 'SalingH', Round(FiSalingH));
  ini.WriteInteger(S, 'SalingA', Round(FiSalingA));
  ini.WriteInteger(S, 'WPowerOS', Round(FiWPowerOS));

  S := 'Min';
  ini.WriteInteger(S, 'Controller', Round(GSB.Controller.Min));
  ini.WriteInteger(S, 'Winkel', Round(GSB.Winkel.Min));
  ini.WriteInteger(S, 'Vorstag', Round(GSB.Vorstag.Min));
  ini.WriteInteger(S, 'Wante', Round(GSB.Wante.Min));
  ini.WriteInteger(S, 'Woben', Round(GSB.Woben.Min));
  ini.WriteInteger(S, 'SalingH', Round(GSB.SalingH.Min));
  ini.WriteInteger(S, 'SalingA', Round(GSB.SalingA.Min));
  ini.WriteInteger(S, 'SalingL', Round(GSB.SalingL.Min));
  ini.WriteInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Min));
  ini.WriteInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Min));

  S := 'Max';
  ini.WriteInteger(S, 'Controller', Round(GSB.Controller.Max));
  ini.WriteInteger(S, 'Winkel', Round(GSB.Winkel.Max));
  ini.WriteInteger(S, 'Vorstag', Round(GSB.Vorstag.Max));
  ini.WriteInteger(S, 'Wante', Round(GSB.Wante.Max));
  ini.WriteInteger(S, 'Woben', Round(GSB.Woben.Max));
  ini.WriteInteger(S, 'SalingH', Round(GSB.SalingH.Max));
  ini.WriteInteger(S, 'SalingA', Round(GSB.SalingA.Max));
  ini.WriteInteger(S, 'SalingL', Round(GSB.SalingL.Max));
  ini.WriteInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Max));
  ini.WriteInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Max));

  S := 'Koordinaten Rumpf';
  ini.WriteInteger(S, 'A0x', Round(iP[ooA0, x]));
  ini.WriteInteger(S, 'A0y', Round(iP[ooA0, y]));
  ini.WriteInteger(S, 'A0z', Round(iP[ooA0, z]));
  ini.WriteInteger(S, 'B0x', Round(iP[ooB0, x]));
  ini.WriteInteger(S, 'B0y', Round(iP[ooB0, y]));
  ini.WriteInteger(S, 'B0z', Round(iP[ooB0, z]));
  ini.WriteInteger(S, 'C0x', Round(iP[ooC0, x]));
  ini.WriteInteger(S, 'C0y', Round(iP[ooC0, y]));
  ini.WriteInteger(S, 'C0z', Round(iP[ooC0, z]));
  ini.WriteInteger(S, 'D0x', Round(iP[ooD0, x]));
  ini.WriteInteger(S, 'D0y', Round(iP[ooD0, y]));
  ini.WriteInteger(S, 'D0z', Round(iP[ooD0, z]));
  ini.WriteInteger(S, 'E0x', Round(iP[ooE0, x]));
  ini.WriteInteger(S, 'E0y', Round(iP[ooE0, y]));
  ini.WriteInteger(S, 'E0z', Round(iP[ooE0, z]));
  ini.WriteInteger(S, 'F0x', Round(iP[ooF0, x]));
  ini.WriteInteger(S, 'F0y', Round(iP[ooF0, y]));
  ini.WriteInteger(S, 'F0z', Round(iP[ooF0, z]));

  S := 'Koordinaten Rigg';
  ini.WriteInteger(S, 'Ax', Round(iP[ooA, x]));
  ini.WriteInteger(S, 'Ay', Round(iP[ooA, y]));
  ini.WriteInteger(S, 'Az', Round(iP[ooA, z]));
  ini.WriteInteger(S, 'Bx', Round(iP[ooB, x]));
  ini.WriteInteger(S, 'By', Round(iP[ooB, y]));
  ini.WriteInteger(S, 'Bz', Round(iP[ooB, z]));
  ini.WriteInteger(S, 'Cx', Round(iP[ooC, x]));
  ini.WriteInteger(S, 'Cy', Round(iP[ooC, y]));
  ini.WriteInteger(S, 'Cz', Round(iP[ooC, z]));
  ini.WriteInteger(S, 'Dx', Round(iP[ooD, x]));
  ini.WriteInteger(S, 'Dy', Round(iP[ooD, y]));
  ini.WriteInteger(S, 'Dz', Round(iP[ooD, z]));
  ini.WriteInteger(S, 'Ex', Round(iP[ooE, x]));
  ini.WriteInteger(S, 'Ey', Round(iP[ooE, y]));
  ini.WriteInteger(S, 'Ez', Round(iP[ooE, z]));
  ini.WriteInteger(S, 'Fx', Round(iP[ooF, x]));
  ini.WriteInteger(S, 'Fy', Round(iP[ooF, y]));
  ini.WriteInteger(S, 'Fz', Round(iP[ooF, z]));
end;

procedure TGetriebe.LoadFromIniFile(ini: TIniFile);
var
  S: string;
begin
  S := 'Rigg';
  SalingTyp := TSalingTyp(ini.ReadInteger(S, 'SalingTyp', Ord(stFest)));

  TrimmTab.LoadFromIniFile(ini);

  S := 'Mast';
  FiMastL := ini.ReadInteger(S, 'MastL', Round(FiMastL));
  FiMastunten := ini.ReadInteger(S, 'Mastunten', Round(FiMastunten));
  FiMastoben := ini.ReadInteger(S, 'Mastoben', Round(FiMastoben));
  FiMastfallVorlauf := ini.ReadInteger(S, 'MastfallVorlauf', Round(FiMastfallVorlauf));

  S := 'Ist';
  FiController := ini.ReadInteger(S, 'Controller', Round(FiController));
  FiWinkel := ini.ReadInteger(S, 'Winkel', Round(FiWinkel));
  FiVorstag := ini.ReadInteger(S, 'Vorstag', Round(FiVorstag));
  FiWoben3d := ini.ReadInteger(S, 'Woben', Round(FiWoben3d));
  FiWunten3d := ini.ReadInteger(S, 'Wante', Round(FiWunten3d + FiWoben3d - FiWoben3d));
  FiSalingH := ini.ReadInteger(S, 'SalingH', Round(FiSalingH));
  FiSalingA := ini.ReadInteger(S, 'SalingA', Round(FiSalingA));
  FiWPowerOS := ini.ReadInteger(S, 'WPowerOS', Round(FiWPowerOS));

  S := 'Min';
  GSB.Controller.Min := ini.ReadInteger(S, 'Controller', Round(GSB.Controller.Min));
  GSB.Winkel.Min := ini.ReadInteger(S, 'Winkel', Round(GSB.Winkel.Min));
  GSB.Vorstag.Min := ini.ReadInteger(S, 'Vorstag', Round(GSB.Vorstag.Min));
  GSB.Wante.Min := ini.ReadInteger(S, 'Wante', Round(GSB.Wante.Min));
  GSB.Woben.Min := ini.ReadInteger(S, 'Woben', Round(GSB.Woben.Min));
  GSB.SalingH.Min := ini.ReadInteger(S, 'SalingH', Round(GSB.SalingH.Min));
  GSB.SalingA.Min := ini.ReadInteger(S, 'SalingA', Round(GSB.SalingA.Min));
  GSB.SalingL.Min := ini.ReadInteger(S, 'SalingL', Round(GSB.SalingL.Min));
  GSB.VorstagOS.Min := ini.ReadInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Min));
  GSB.WPowerOS.Min := ini.ReadInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Min));

  S := 'Max';
  GSB.Controller.Max := ini.ReadInteger(S, 'Controller', Round(GSB.Controller.Max));
  GSB.Winkel.Max := ini.ReadInteger(S, 'Winkel', Round(GSB.Winkel.Max));
  GSB.Vorstag.Max := ini.ReadInteger(S, 'Vorstag', Round(GSB.Vorstag.Max));
  GSB.Wante.Max := ini.ReadInteger(S, 'Wante', Round(GSB.Wante.Max));
  GSB.Woben.Max := ini.ReadInteger(S, 'Woben', Round(GSB.Woben.Max));
  GSB.SalingH.Max := ini.ReadInteger(S, 'SalingH', Round(GSB.SalingH.Max));
  GSB.SalingA.Max := ini.ReadInteger(S, 'SalingA', Round(GSB.SalingA.Max));
  GSB.SalingL.Max := ini.ReadInteger(S, 'SalingL', Round(GSB.SalingL.Max));
  GSB.VorstagOS.Max := ini.ReadInteger(S, 'VorstagOS', Round(GSB.VorstagOS.Max));
  GSB.WPowerOS.Max := ini.ReadInteger(S, 'WPowerOS', Round(GSB.WPowerOS.Max));

  S := 'Koordinaten Rumpf';
  iP[ooA0, x] := ini.ReadInteger(S, 'A0x', Round(iP[ooA0, x]));
  iP[ooA0, y] := ini.ReadInteger(S, 'A0y', Round(iP[ooA0, y]));
  iP[ooA0, z] := ini.ReadInteger(S, 'A0z', Round(iP[ooA0, z]));
  iP[ooB0, x] := ini.ReadInteger(S, 'B0x', Round(iP[ooB0, x]));
  iP[ooB0, y] := ini.ReadInteger(S, 'B0y', Round(iP[ooB0, y]));
  iP[ooB0, z] := ini.ReadInteger(S, 'B0z', Round(iP[ooB0, z]));
  iP[ooC0, x] := ini.ReadInteger(S, 'C0x', Round(iP[ooC0, x]));
  iP[ooC0, y] := ini.ReadInteger(S, 'C0y', Round(iP[ooC0, y]));
  iP[ooC0, z] := ini.ReadInteger(S, 'C0z', Round(iP[ooC0, z]));
  iP[ooD0, x] := ini.ReadInteger(S, 'D0x', Round(iP[ooD0, x]));
  iP[ooD0, y] := ini.ReadInteger(S, 'D0y', Round(iP[ooD0, y]));
  iP[ooD0, z] := ini.ReadInteger(S, 'D0z', Round(iP[ooD0, z]));
  iP[ooE0, x] := ini.ReadInteger(S, 'E0x', Round(iP[ooE0, x]));
  iP[ooE0, y] := ini.ReadInteger(S, 'E0y', Round(iP[ooE0, y]));
  iP[ooE0, z] := ini.ReadInteger(S, 'E0z', Round(iP[ooE0, z]));
  iP[ooF0, x] := ini.ReadInteger(S, 'F0x', Round(iP[ooF0, x]));
  iP[ooF0, y] := ini.ReadInteger(S, 'F0y', Round(iP[ooF0, y]));
  iP[ooF0, z] := ini.ReadInteger(S, 'F0z', Round(iP[ooF0, z]));

  S := 'Koordinaten Rigg';
  iP[ooA, x] := ini.ReadInteger(S, 'Ax', Round(iP[ooA, x]));
  iP[ooA, y] := ini.ReadInteger(S, 'Ay', Round(iP[ooA, y]));
  iP[ooA, z] := ini.ReadInteger(S, 'Az', Round(iP[ooA, z]));
  iP[ooB, x] := ini.ReadInteger(S, 'Bx', Round(iP[ooB, x]));
  iP[ooB, y] := ini.ReadInteger(S, 'By', Round(iP[ooB, y]));
  iP[ooB, z] := ini.ReadInteger(S, 'Bz', Round(iP[ooB, z]));
  iP[ooC, x] := ini.ReadInteger(S, 'Cx', Round(iP[ooC, x]));
  iP[ooC, y] := ini.ReadInteger(S, 'Cy', Round(iP[ooC, y]));
  iP[ooC, z] := ini.ReadInteger(S, 'Cz', Round(iP[ooC, z]));
  iP[ooD, x] := ini.ReadInteger(S, 'Dx', Round(iP[ooD, x]));
  iP[ooD, y] := ini.ReadInteger(S, 'Dy', Round(iP[ooD, y]));
  iP[ooD, z] := ini.ReadInteger(S, 'Dz', Round(iP[ooD, z]));
  iP[ooE, x] := ini.ReadInteger(S, 'Ex', Round(iP[ooE, x]));
  iP[ooE, y] := ini.ReadInteger(S, 'Ey', Round(iP[ooE, y]));
  iP[ooE, z] := ini.ReadInteger(S, 'Ez', Round(iP[ooE, z]));
  iP[ooF, x] := ini.ReadInteger(S, 'Fx', Round(iP[ooF, x]));
  iP[ooF, y] := ini.ReadInteger(S, 'Fy', Round(iP[ooF, y]));
  iP[ooF, z] := ini.ReadInteger(S, 'Fz', Round(iP[ooF, z]));
end;

procedure TGetriebe.LoadFromStream(S: TStream);
var
  temp: Integer;
begin
  S.ReadBuffer(temp, SizeOf(Integer));
  SalingTyp := TSalingTyp(temp);
  TrimmTab.LoadFromStream(S);
  { Mast }
  S.ReadBuffer(FiMastL, SizeOf(Integer));
  S.ReadBuffer(FiMastunten, SizeOf(Integer));
  S.ReadBuffer(FiMastoben, SizeOf(Integer));
  S.ReadBuffer(FiMastfallVorlauf, SizeOf(Integer));
  { Ist }
  S.ReadBuffer(FiController, SizeOf(Integer));
  S.ReadBuffer(FiWinkel, SizeOf(Integer));
  S.ReadBuffer(FiVorstag, SizeOf(Integer));
  S.ReadBuffer(FiWoben3d, SizeOf(Integer));
  S.ReadBuffer(FiWunten3d, SizeOf(Integer));
  S.ReadBuffer(FiSalingH, SizeOf(Integer));
  S.ReadBuffer(FiSalingA, SizeOf(Integer));
  S.ReadBuffer(FiWPowerOS, SizeOf(Integer));
  { GSB }
  GSB.LoadFromStream(S);
  { Koordinaten }
  S.ReadBuffer(iP, SizeOf(TIntRiggPoints));
end;

procedure TGetriebe.SaveToStream(S: TStream);
begin
  S.WriteBuffer(SalingTyp, SizeOf(Integer));
  TrimmTab.SaveToStream(S);
  { Mast }
  S.WriteBuffer(FiMastL, SizeOf(Integer));
  S.WriteBuffer(FiMastunten, SizeOf(Integer));
  S.WriteBuffer(FiMastoben, SizeOf(Integer));
  S.WriteBuffer(FiMastfallVorlauf, SizeOf(Integer));
  { Ist }
  S.WriteBuffer(FiController, SizeOf(Integer));
  S.WriteBuffer(FiWinkel, SizeOf(Integer));
  S.WriteBuffer(FiVorstag, SizeOf(Integer));
  S.WriteBuffer(FiWoben3d, SizeOf(Integer));
  S.WriteBuffer(FiWunten3d, SizeOf(Integer));
  S.WriteBuffer(FiSalingH, SizeOf(Integer));
  S.WriteBuffer(FiSalingA, SizeOf(Integer));
  S.WriteBuffer(FiWPowerOS, SizeOf(Integer));
  { GSB }
  GSB.SaveToStream(S);
  { Koordinaten }
  S.WriteBuffer(iP, SizeOf(TIntRiggPoints));
end;

end.

