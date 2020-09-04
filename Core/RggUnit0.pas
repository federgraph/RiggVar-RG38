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
  System.Math.Vectors,
  RiggVar.RG.Def,
  RggStrings,
  RggScroll,
  RggTypes,
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
  protected
    FTrimm: TTrimm;
    FGetriebeOK: Boolean;
    FMastOK: Boolean;
    FGetriebeStatus: set of TGetriebeStatus;
    FrWanteZulang: single;

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

    procedure IntGliederToReal;
    procedure RealGliederToInt;
    procedure Wanten2dTo3d;
    procedure Wanten3dTo2d;
    procedure SetMastL(Value: single);
    procedure SetMastunten(Value: single);
    procedure SetMastoben(Value: single);
    function GetRealGlied(Index: TsbName): single;
    procedure SetRealGlied(Index: TsbName; Value: single);
    procedure SetSalingTyp(Value: TSalingTyp); virtual;

    procedure LoadFromIniFile(ini: TIniFile); virtual;
    procedure WriteToIniFile(ini: TIniFile); virtual;
  public
    LogList: TStringList;
    SKK: TSchnittKK;
    TrimmTab: TTrimmTab;
    GSB: TRggFA;
    rP: TRiggPoints;

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

    property MastLaenge: single read FrMastLength write SetMastL;
    property MastUnten: single read FrMastUnten write SetMastunten;
    property MastOben: single read FrMastOben write SetMastoben;
    property MastfallVorlauf: single read FrMastfallVorlauf write FrMastfallVorlauf;

    property phi: single read FrPhi write FrPhi;
    property psi: single read FrPsi write FrPsi;
    property alpha: single read FrAlpha;
    property epsilon: single read FrEpsilon write FrEpsilon;
    property WantenSpannung: single read FWPowerOS write FWPowerOS;
    property ControllerAnschlag: Integer read FiControllerAnschlag write FiControllerAnschlag;

    property SalingDaten: TSalingDaten read GetSalingDaten;
    property Glieder: TTrimmControls read GetGlieder write SetGlieder;
    property RealGlied[Index: TsbName]: single read GetRealGlied write SetRealGlied;
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
  SKK := TSchnittKK.Create;
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
  SKK.Free;
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

procedure TGetriebe.SetMastunten(Value: single);
begin
  if Value <> FrMastUnten then
  begin
    FrMastUnten := Value;
  end;
end;

procedure TGetriebe.SetMastoben(Value: single);
begin
  if Value <> FrMastOben then
  begin
    FrMastOben := Value;
  end;
end;

procedure TGetriebe.SetMastL(Value: single);
begin
  if Value <> FrMastLength then
  begin
    FrMastLength := Value;
    FrMastEnde := Value - FrMastOben - FrMastUnten;
  end;
end;

function TGetriebe.GetGlieder: TTrimmControls;
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

procedure TGetriebe.SetGlieder(Values: TTrimmControls);
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

function TGetriebe.GetRealGlied(Index: TsbName): single;
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

procedure TGetriebe.SetRealGlied(Index: TsbName; Value: single);
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

procedure TGetriebe.IntGliederToReal;
begin
  { Integer Glieder have been eliminated. }
  FrWinkel := DegToRad(FWinkelDegrees);
  FrMastEnde := FrMastLength - FrMastOben - FrMastUnten;
end;

procedure TGetriebe.RealGliederToInt;
begin
  { Integer Glieder have been eliminated. }
  FWinkelDegrees := RadToDeg(FrWinkel);
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

procedure TGetriebe.Reset;
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
end;

end.

