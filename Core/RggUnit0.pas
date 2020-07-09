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
  System.IniFiles,
  System.Math,
  RiggVar.RG.Def,
  RggStrings,
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
    _FrWinkel: double; { in radians }
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

    FiControllerAnschlag: Integer;

    FiController: double;
    FiWinkel: double; { in degrees }
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
  cosWW: double;
begin
  ooTempA := EVektor(rP[ooA], rP[ooC]);
  ooTempB := EVektor(rP[ooA0], rP[ooA]);
  cosWW := sprod(ooTempA, ooTempB);
  tempWW := arccos(cosWW);

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
      Main.Logger.Error(Ebenen_senkrecht_in_GetSalingDaten_String);
  end;

  SD.SalingH := FrSalingH;
  SD.SalingA := FrSalingA;
  SD.SalingL := FrSalingL;
  SD.SalingW := arctan2(FrSalingA / 2, FrSalingH) * 180 / pi;
  SD.WantenWinkel := tempWW * 180 / pi;
  SD.KraftWinkel := tempWS * 180 / pi;

  result := SD;
end;

procedure TGetriebe.IntGliederToReal;
begin
  FrController := FiController;
  FrWinkel := FiWinkel * pi / 180; { FrWinkel im Bogenmass }
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
var
  u, v: double;
begin
  u := sqr(FrWunten3d) - sqr((FrPuettingA - FrSalingA) / 2);
  v := sqr(FrWoben3d) - sqr(FrSalingA / 2);
  if (u > 0) and (v > 0) then
  begin
    FrWunten2d := sqrt(u);
    FrWoben2d := sqrt(v);
  end;
end;

function TGetriebe.GetriebeStatusText: string;
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
  s: string;
begin
  s := Rigg_IniSectionString;
  ini.WriteInteger(s, SalingTyp_IniString, Ord(FSalingTyp));
  TrimmTab.WriteToIniFile(ini);

  s := Mast_IniSectionString;
  ini.WriteInteger(s, MastL_IniString, Round(FiMastL));
  ini.WriteInteger(s, Mastunten_IniString, Round(FiMastunten));
  ini.WriteInteger(s, Mastoben_IniString, Round(FiMastoben));
  ini.WriteInteger(s, MastfallVorlauf_IniString, Round(FiMastfallVorlauf));

  s := Ist_IniSectionString;
  ini.WriteInteger(s, Controller_IniString, Round(FiController));
  ini.WriteInteger(s, WinkelString, Round(FiWinkel));
  ini.WriteInteger(s, VorstagString, Round(FiVorstag));
  ini.WriteInteger(s, WanteString, Round(FiWunten3d + FiWoben3d));
  ini.WriteInteger(s, Woben_IniString, Round(FiWoben3d));
  ini.WriteInteger(s, SalingH_IniString, Round(FiSalingH));
  ini.WriteInteger(s, SalingA_IniString, Round(FiSalingA));
  ini.WriteInteger(s, WPowerOS_IniString, Round(FiWPowerOS));

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
  ini.WriteInteger(s, A0x_IniString, Round(iP[ooA0, x]));
  ini.WriteInteger(s, A0y_IniString, Round(iP[ooA0, y]));
  ini.WriteInteger(s, A0z_IniString, Round(iP[ooA0, z]));
  ini.WriteInteger(s, B0x_IniString, Round(iP[ooB0, x]));
  ini.WriteInteger(s, B0y_IniString, Round(iP[ooB0, y]));
  ini.WriteInteger(s, B0z_IniString, Round(iP[ooB0, z]));
  ini.WriteInteger(s, C0x_IniString, Round(iP[ooC0, x]));
  ini.WriteInteger(s, C0y_IniString, Round(iP[ooC0, y]));
  ini.WriteInteger(s, C0z_IniString, Round(iP[ooC0, z]));
  ini.WriteInteger(s, D0x_IniString, Round(iP[ooD0, x]));
  ini.WriteInteger(s, D0y_IniString, Round(iP[ooD0, y]));
  ini.WriteInteger(s, D0z_IniString, Round(iP[ooD0, z]));
  ini.WriteInteger(s, E0x_IniString, Round(iP[ooE0, x]));
  ini.WriteInteger(s, E0y_IniString, Round(iP[ooE0, y]));
  ini.WriteInteger(s, E0z_IniString, Round(iP[ooE0, z]));
  ini.WriteInteger(s, F0x_IniString, Round(iP[ooF0, x]));
  ini.WriteInteger(s, F0y_IniString, Round(iP[ooF0, y]));
  ini.WriteInteger(s, F0z_IniString, Round(iP[ooF0, z]));

  s := Koordinaten_Rigg_IniSectionString;
  ini.WriteInteger(s, Ax_IniString, Round(iP[ooA, x]));
  ini.WriteInteger(s, Ay_IniString, Round(iP[ooA, y]));
  ini.WriteInteger(s, Az_IniString, Round(iP[ooA, z]));
  ini.WriteInteger(s, Bx_IniString, Round(iP[ooB, x]));
  ini.WriteInteger(s, By_IniString, Round(iP[ooB, y]));
  ini.WriteInteger(s, Bz_IniString, Round(iP[ooB, z]));
  ini.WriteInteger(s, Cx_IniString, Round(iP[ooC, x]));
  ini.WriteInteger(s, Cy_IniString, Round(iP[ooC, y]));
  ini.WriteInteger(s, Cz_IniString, Round(iP[ooC, z]));
  ini.WriteInteger(s, Dx_IniString, Round(iP[ooD, x]));
  ini.WriteInteger(s, Dy_IniString, Round(iP[ooD, y]));
  ini.WriteInteger(s, Dz_IniString, Round(iP[ooD, z]));
  ini.WriteInteger(s, Ex_IniString, Round(iP[ooE, x]));
  ini.WriteInteger(s, Ey_IniString, Round(iP[ooE, y]));
  ini.WriteInteger(s, Ez_IniString, Round(iP[ooE, z]));
  ini.WriteInteger(s, Fx_IniString, Round(iP[ooF, x]));
  ini.WriteInteger(s, Fy_IniString, Round(iP[ooF, y]));
  ini.WriteInteger(s, Fz_IniString, Round(iP[ooF, z]));
end;

procedure TGetriebe.LoadFromIniFile(ini: TIniFile);
var
  s: string;
begin
  s := Rigg_IniSectionString;
  SalingTyp := TSalingTyp(ini.ReadInteger(s, SalingTyp_IniString, Ord(stFest)));

  TrimmTab.LoadFromIniFile(ini);

  s := Mast_IniSectionString;
  FiMastL := ini.ReadInteger(s, MastL_IniString, Round(FiMastL));
  FiMastunten := ini.ReadInteger(s, Mastunten_IniString, Round(FiMastunten));
  FiMastoben := ini.ReadInteger(s, Mastoben_IniString, Round(FiMastoben));
  FiMastfallVorlauf := ini.ReadInteger(s, MastfallVorlauf_IniString, Round(FiMastfallVorlauf));

  s := Ist_IniSectionString;
  FiController := ini.ReadInteger(s, Controller_IniString, Round(FiController));
  FiWinkel := ini.ReadInteger(s, Winkel_IniString, Round(FiWinkel));
  FiVorstag := ini.ReadInteger(s, Vorstag_IniString, Round(FiVorstag));
  FiWoben3d := ini.ReadInteger(s, Woben_IniString, Round(FiWoben3d));
  FiWunten3d := ini.ReadInteger(s, Wante_IniString, Round(FiWunten3d + FiWoben3d - FiWoben3d));
  FiSalingH := ini.ReadInteger(s, SalingH_IniString, Round(FiSalingH));
  FiSalingA := ini.ReadInteger(s, SalingA_IniString, Round(FiSalingA));
  FiWPowerOS := ini.ReadInteger(s, WPowerOS_IniString, Round(FiWPowerOS));

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
  iP[ooA0, x] := ini.ReadInteger(s, A0x_IniString, Round(iP[ooA0, x]));
  iP[ooA0, y] := ini.ReadInteger(s, A0y_IniString, Round(iP[ooA0, y]));
  iP[ooA0, z] := ini.ReadInteger(s, A0z_IniString, Round(iP[ooA0, z]));
  iP[ooB0, x] := ini.ReadInteger(s, B0x_IniString, Round(iP[ooB0, x]));
  iP[ooB0, y] := ini.ReadInteger(s, B0y_IniString, Round(iP[ooB0, y]));
  iP[ooB0, z] := ini.ReadInteger(s, B0z_IniString, Round(iP[ooB0, z]));
  iP[ooC0, x] := ini.ReadInteger(s, C0x_IniString, Round(iP[ooC0, x]));
  iP[ooC0, y] := ini.ReadInteger(s, C0y_IniString, Round(iP[ooC0, y]));
  iP[ooC0, z] := ini.ReadInteger(s, C0z_IniString, Round(iP[ooC0, z]));
  iP[ooD0, x] := ini.ReadInteger(s, D0x_IniString, Round(iP[ooD0, x]));
  iP[ooD0, y] := ini.ReadInteger(s, D0y_IniString, Round(iP[ooD0, y]));
  iP[ooD0, z] := ini.ReadInteger(s, D0z_IniString, Round(iP[ooD0, z]));
  iP[ooE0, x] := ini.ReadInteger(s, E0x_IniString, Round(iP[ooE0, x]));
  iP[ooE0, y] := ini.ReadInteger(s, E0y_IniString, Round(iP[ooE0, y]));
  iP[ooE0, z] := ini.ReadInteger(s, E0z_IniString, Round(iP[ooE0, z]));
  iP[ooF0, x] := ini.ReadInteger(s, F0x_IniString, Round(iP[ooF0, x]));
  iP[ooF0, y] := ini.ReadInteger(s, F0y_IniString, Round(iP[ooF0, y]));
  iP[ooF0, z] := ini.ReadInteger(s, F0z_IniString, Round(iP[ooF0, z]));

  s := Koordinaten_Rigg_IniSectionString;
  iP[ooA, x] := ini.ReadInteger(s, Ax_IniString, Round(iP[ooA, x]));
  iP[ooA, y] := ini.ReadInteger(s, Ay_IniString, Round(iP[ooA, y]));
  iP[ooA, z] := ini.ReadInteger(s, Az_IniString, Round(iP[ooA, z]));
  iP[ooB, x] := ini.ReadInteger(s, Bx_IniString, Round(iP[ooB, x]));
  iP[ooB, y] := ini.ReadInteger(s, By_IniString, Round(iP[ooB, y]));
  iP[ooB, z] := ini.ReadInteger(s, Bz_IniString, Round(iP[ooB, z]));
  iP[ooC, x] := ini.ReadInteger(s, Cx_IniString, Round(iP[ooC, x]));
  iP[ooC, y] := ini.ReadInteger(s, Cy_IniString, Round(iP[ooC, y]));
  iP[ooC, z] := ini.ReadInteger(s, Cz_IniString, Round(iP[ooC, z]));
  iP[ooD, x] := ini.ReadInteger(s, Dx_IniString, Round(iP[ooD, x]));
  iP[ooD, y] := ini.ReadInteger(s, Dy_IniString, Round(iP[ooD, y]));
  iP[ooD, z] := ini.ReadInteger(s, Dz_IniString, Round(iP[ooD, z]));
  iP[ooE, x] := ini.ReadInteger(s, Ex_IniString, Round(iP[ooE, x]));
  iP[ooE, y] := ini.ReadInteger(s, Ey_IniString, Round(iP[ooE, y]));
  iP[ooE, z] := ini.ReadInteger(s, Ez_IniString, Round(iP[ooE, z]));
  iP[ooF, x] := ini.ReadInteger(s, Fx_IniString, Round(iP[ooF, x]));
  iP[ooF, y] := ini.ReadInteger(s, Fy_IniString, Round(iP[ooF, y]));
  iP[ooF, z] := ini.ReadInteger(s, Fz_IniString, Round(iP[ooF, z]));
end;

procedure TGetriebe.LoadFromStream(S: TStream);
var
  temp: Integer;
begin
  temp := 0;
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

