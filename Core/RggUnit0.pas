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
    _FrWinkel: double;
    FrVorstag: double;
    FrWunten2D: double;
    FrWunten3D: double;
    FrWoben2D: double;
    FrWoben3D: double;
    FrSalingL: double;
    FrSalingH: double;
    FrSalingA: double;
    FrMastUnten: double;
    FrMastOben: double;
    FrMastEnde: double;

    FrPsi: double;
    FrPhi: double;
    FrAlpha: double;
    FrEpsilon: double;

    FiControllerAnschlag: Integer;

    FWinkelDegrees: double;
    FWPowerOS: double;

    FrMastLength: double;
    FrMastfallVorlauf: double;

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
  public
    LogList: TStringList;
    SchnittKK: TSchnittKK;
    TrimmTab: TTrimmTab;
    GSB: TRggFA;
    rP: TRealRiggPoints;

    constructor Create;
    destructor Destroy; override;

    procedure GetBuiltinData;
    procedure Reset;
    procedure UpdateGSB;
    procedure UpdateGlieder;

    function GetriebeStatusText: string;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
    property ManipulatorMode: Boolean read FManipulatorMode write FManipulatorMode;
    property GetriebeOK: Boolean read FGetriebeOK;

    property MastLaenge: double read FrMastLength write SetMastL;
    property MastUnten: double read FrMastUnten write SetMastunten;
    property MastOben: double read FrMastOben write SetMastoben;
    property MastfallVorlauf: double read FrMastfallVorlauf write FrMastfallVorlauf;

    property phi: double read FrPhi write FrPhi;
    property psi: double read FrPsi write FrPsi;
    property alpha: double read FrAlpha;
    property epsilon: double read FrEpsilon write FrEpsilon;
    property WantenSpannung: double read FWPowerOS write FWPowerOS;
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
  if Value <> FrMastUnten then
  begin
    FrMastUnten := Value;
    FrMastUnten := Value;
  end;
end;

procedure TGetriebe.SetMastoben(Value: double);
begin
  if Value <> FrMastOben then
  begin
    FrMastOben := Value;
    FrMastOben := Value;
  end;
end;

procedure TGetriebe.SetMastL(Value: double);
begin
  if Value <> FrMastLength then
  begin
    FrMastLength := Value;
    FrMastEnde := Value - FrMastOben - FrMastUnten;
  end;
end;

function TGetriebe.GetGlieder: TTrimmControls;
var
  Trimm: TTrimmControls;
begin
  RealGliederToInt;
  with Trimm do
  begin
    Controller := Round(FrController);
    Wanten := Round(FrWunten3D + FrWoben3D);
    Woben := Round(FrWoben3D);
    SalingH := Round(FrSalingH);
    SalingA := Round(FrSalingA);
    SalingL := Round(FrSalingL);
    Vorstag := Round(FrVorstag);
    Winkel := Round(FWinkelDegrees);
    WPowerOS := Round(FWPowerOS);
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
    FrController := Controller;
    FWinkelDegrees := Winkel;
    FrVorstag := Vorstag;
    FrWunten3D := Wanten - Woben;
    FrWoben3D := Woben;
    FrSalingH := SalingH;
    FrSalingA := SalingA;
    FrSalingL := SalingL;
    FWPowerOS := WPowerOS;
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
    fpWante: result := FrWunten3D + FrWoben3D;
    fpWoben: result := FrWoben3D;
    fpSalingH: result := FrSalingH;
    fpSalingA: result := FrSalingA;
    fpSalingL: result := FrSalingL;
    fpVorstagOS: result := FrVorstag;
    fpWPowerOS: result := FWPowerOS;
  end;
end;

procedure TGetriebe.SetRealGlied(Index: TsbName; Value: double);
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
  if abs(cosWW) > 0.99 then
    tempWW := 0
  else
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
//  FrController := FiController;
  FrWinkel := FWinkelDegrees * pi / 180;
//  FrVorstag := FiVorstag;
//  FrWunten3d := FiWunten3d;
//  FrWoben3d := FiWoben3d;
//  FrSalingH := FiSalingH;
//  FrSalingA := FiSalingA;
//  FrSalingL := FiSalingL;

//  FrMastunten := FiMastunten;
//  FrMastoben := FiMastoben;
  FrMastEnde := FrMastLength - FrMastOben - FrMastUnten;
end;

procedure TGetriebe.RealGliederToInt;
begin
  { Integer Glieder have been eliminated. }
//  FiController := FrController;
  FWinkelDegrees := FrWinkel * 180 / pi;
//  FiVorstag := FrVorstag;
//  FiWunten3d := FrWunten3d;
//  FiWoben3d := FrWoben3d;
//  FiSalingH := FrSalingH;
//  FiSalingA := FrSalingA;
//  FiSalingL := FrSalingL;

//  FiMastunten := FrMastunten;
//  FiMastoben := FrMastoben;
  FrMastLength := FrMastUnten + FrMastOben + FrMastEnde;
end;

procedure TGetriebe.UpdateGSB;
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

procedure TGetriebe.UpdateGlieder;
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

procedure TGetriebe.Wanten2dTo3d;
begin
  FrWunten3D := sqrt(sqr(FrWunten2D) + sqr((FrPuettingA - FrSalingA) / 2));
  FrWoben3D := sqrt(sqr(FrWoben2D) + sqr(FrSalingA / 2));
end;

procedure TGetriebe.Wanten3dTo2d;
var
  u, v: double;
begin
  u := sqr(FrWunten3D) - sqr((FrPuettingA - FrSalingA) / 2);
  v := sqr(FrWoben3D) - sqr(FrSalingA / 2);
  if (u > 0) and (v > 0) then
  begin
    FrWunten2D := sqrt(u);
    FrWoben2D := sqrt(v);
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
  rP[ooA0, x] := 2560; { Pütting Stbd }
  rP[ooA0, y] := 765;
  rP[ooA0, z] := 430;

  rP[ooB0, x] := 2560; { Püttinge Bb }
  rP[ooB0, y] := -765;
  rP[ooB0, z] := 430;

  rP[ooC0, x] := 4140; { Vorstag }
  rP[ooC0, y] := 0;
  rP[ooC0, z] := 340;

  rP[ooD0, x] := 2870; { Mastfuß }
  rP[ooD0, y] := 0;
  rP[ooD0, z] := -100;

  rP[ooE0, x] := 2970; { Controller }
  rP[ooE0, y] := 0;
  rP[ooE0, z] := 450;

  rP[ooF0, x] := -30; { Spiegel }
  rP[ooF0, y] := 0;
  rP[ooF0, z] := 300;

  rP[ooP0] := rP[ooA0];
  rP[ooP0, y] := 0;

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

procedure TGetriebe.GetLogoData;
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
  FWinkelDegrees := Round(90 + arctan2(1, 3) * 180 / pi); { Winkel Wunten }
  FWPowerOS := 1000; { angenommene Wantenspannung 3d }

  { RumpfKoordinaten in mm }
  rP[ooA0, x] := 30 * f + ox; { Pütting Stbd }
  rP[ooA0, y] := 40 * f;
  rP[ooA0, z] := 40 * f + oz;

  rP[ooB0, x] := 30 * f + ox;
  rP[ooB0, y] := -40 * f;
  rP[ooB0, z] := 40 * f + oz;

  rP[ooC0, x] := 150 * f + ox;
  rP[ooC0, y] := 0;
  rP[ooC0, z] := 40 * f + oz;

  rP[ooD0, x] := 80 * f + ox;
  rP[ooD0, y] := 0;
  rP[ooD0, z] := 10 * f + oz;

  rP[ooE0, x] := 85 * f + ox;
  rP[ooE0, y] := 0;
  rP[ooE0, z] := 50 * f + oz;

  rP[ooF0, x] := -85 * f + ox;
  rP[ooF0, y] := 0;
  rP[ooF0, z] := 40 * f + oz;

  rP[ooP0] := rP[ooA0];
  rP[ooP0, y] := 0;

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

procedure TGetriebe.Reset;
begin
  { Rumpfkoordinaten }
  rP[ooP0] := rP[ooA0];
  rP[ooP0, y] := 0;
  { Mast }
  FrMastEnde := FrMastLength - FrMastUnten - FrMastOben;
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
  ini.WriteInteger(s, A0x_IniString, Round(rP[ooA0, x]));
  ini.WriteInteger(s, A0y_IniString, Round(rP[ooA0, y]));
  ini.WriteInteger(s, A0z_IniString, Round(rP[ooA0, z]));
  ini.WriteInteger(s, B0x_IniString, Round(rP[ooB0, x]));
  ini.WriteInteger(s, B0y_IniString, Round(rP[ooB0, y]));
  ini.WriteInteger(s, B0z_IniString, Round(rP[ooB0, z]));
  ini.WriteInteger(s, C0x_IniString, Round(rP[ooC0, x]));
  ini.WriteInteger(s, C0y_IniString, Round(rP[ooC0, y]));
  ini.WriteInteger(s, C0z_IniString, Round(rP[ooC0, z]));
  ini.WriteInteger(s, D0x_IniString, Round(rP[ooD0, x]));
  ini.WriteInteger(s, D0y_IniString, Round(rP[ooD0, y]));
  ini.WriteInteger(s, D0z_IniString, Round(rP[ooD0, z]));
  ini.WriteInteger(s, E0x_IniString, Round(rP[ooE0, x]));
  ini.WriteInteger(s, E0y_IniString, Round(rP[ooE0, y]));
  ini.WriteInteger(s, E0z_IniString, Round(rP[ooE0, z]));
  ini.WriteInteger(s, F0x_IniString, Round(rP[ooF0, x]));
  ini.WriteInteger(s, F0y_IniString, Round(rP[ooF0, y]));
  ini.WriteInteger(s, F0z_IniString, Round(rP[ooF0, z]));

  s := Koordinaten_Rigg_IniSectionString;
  ini.WriteInteger(s, Ax_IniString, Round(rP[ooA, x]));
  ini.WriteInteger(s, Ay_IniString, Round(rP[ooA, y]));
  ini.WriteInteger(s, Az_IniString, Round(rP[ooA, z]));
  ini.WriteInteger(s, Bx_IniString, Round(rP[ooB, x]));
  ini.WriteInteger(s, By_IniString, Round(rP[ooB, y]));
  ini.WriteInteger(s, Bz_IniString, Round(rP[ooB, z]));
  ini.WriteInteger(s, Cx_IniString, Round(rP[ooC, x]));
  ini.WriteInteger(s, Cy_IniString, Round(rP[ooC, y]));
  ini.WriteInteger(s, Cz_IniString, Round(rP[ooC, z]));
  ini.WriteInteger(s, Dx_IniString, Round(rP[ooD, x]));
  ini.WriteInteger(s, Dy_IniString, Round(rP[ooD, y]));
  ini.WriteInteger(s, Dz_IniString, Round(rP[ooD, z]));
  ini.WriteInteger(s, Ex_IniString, Round(rP[ooE, x]));
  ini.WriteInteger(s, Ey_IniString, Round(rP[ooE, y]));
  ini.WriteInteger(s, Ez_IniString, Round(rP[ooE, z]));
  ini.WriteInteger(s, Fx_IniString, Round(rP[ooF, x]));
  ini.WriteInteger(s, Fy_IniString, Round(rP[ooF, y]));
  ini.WriteInteger(s, Fz_IniString, Round(rP[ooF, z]));
end;

procedure TGetriebe.LoadFromIniFile(ini: TIniFile);
var
  s: string;
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
  rP[ooA0, x] := ini.ReadInteger(s, A0x_IniString, Round(rP[ooA0, x]));
  rP[ooA0, y] := ini.ReadInteger(s, A0y_IniString, Round(rP[ooA0, y]));
  rP[ooA0, z] := ini.ReadInteger(s, A0z_IniString, Round(rP[ooA0, z]));
  rP[ooB0, x] := ini.ReadInteger(s, B0x_IniString, Round(rP[ooB0, x]));
  rP[ooB0, y] := ini.ReadInteger(s, B0y_IniString, Round(rP[ooB0, y]));
  rP[ooB0, z] := ini.ReadInteger(s, B0z_IniString, Round(rP[ooB0, z]));
  rP[ooC0, x] := ini.ReadInteger(s, C0x_IniString, Round(rP[ooC0, x]));
  rP[ooC0, y] := ini.ReadInteger(s, C0y_IniString, Round(rP[ooC0, y]));
  rP[ooC0, z] := ini.ReadInteger(s, C0z_IniString, Round(rP[ooC0, z]));
  rP[ooD0, x] := ini.ReadInteger(s, D0x_IniString, Round(rP[ooD0, x]));
  rP[ooD0, y] := ini.ReadInteger(s, D0y_IniString, Round(rP[ooD0, y]));
  rP[ooD0, z] := ini.ReadInteger(s, D0z_IniString, Round(rP[ooD0, z]));
  rP[ooE0, x] := ini.ReadInteger(s, E0x_IniString, Round(rP[ooE0, x]));
  rP[ooE0, y] := ini.ReadInteger(s, E0y_IniString, Round(rP[ooE0, y]));
  rP[ooE0, z] := ini.ReadInteger(s, E0z_IniString, Round(rP[ooE0, z]));
  rP[ooF0, x] := ini.ReadInteger(s, F0x_IniString, Round(rP[ooF0, x]));
  rP[ooF0, y] := ini.ReadInteger(s, F0y_IniString, Round(rP[ooF0, y]));
  rP[ooF0, z] := ini.ReadInteger(s, F0z_IniString, Round(rP[ooF0, z]));

  s := Koordinaten_Rigg_IniSectionString;
  rP[ooA, x] := ini.ReadInteger(s, Ax_IniString, Round(rP[ooA, x]));
  rP[ooA, y] := ini.ReadInteger(s, Ay_IniString, Round(rP[ooA, y]));
  rP[ooA, z] := ini.ReadInteger(s, Az_IniString, Round(rP[ooA, z]));
  rP[ooB, x] := ini.ReadInteger(s, Bx_IniString, Round(rP[ooB, x]));
  rP[ooB, y] := ini.ReadInteger(s, By_IniString, Round(rP[ooB, y]));
  rP[ooB, z] := ini.ReadInteger(s, Bz_IniString, Round(rP[ooB, z]));
  rP[ooC, x] := ini.ReadInteger(s, Cx_IniString, Round(rP[ooC, x]));
  rP[ooC, y] := ini.ReadInteger(s, Cy_IniString, Round(rP[ooC, y]));
  rP[ooC, z] := ini.ReadInteger(s, Cz_IniString, Round(rP[ooC, z]));
  rP[ooD, x] := ini.ReadInteger(s, Dx_IniString, Round(rP[ooD, x]));
  rP[ooD, y] := ini.ReadInteger(s, Dy_IniString, Round(rP[ooD, y]));
  rP[ooD, z] := ini.ReadInteger(s, Dz_IniString, Round(rP[ooD, z]));
  rP[ooE, x] := ini.ReadInteger(s, Ex_IniString, Round(rP[ooE, x]));
  rP[ooE, y] := ini.ReadInteger(s, Ey_IniString, Round(rP[ooE, y]));
  rP[ooE, z] := ini.ReadInteger(s, Ez_IniString, Round(rP[ooE, z]));
  rP[ooF, x] := ini.ReadInteger(s, Fx_IniString, Round(rP[ooF, x]));
  rP[ooF, y] := ini.ReadInteger(s, Fy_IniString, Round(rP[ooF, y]));
  rP[ooF, z] := ini.ReadInteger(s, Fz_IniString, Round(rP[ooF, z]));
end;

end.

