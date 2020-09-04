unit RggReport;

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
  RggTypes,
  RggFachwerk;

const
  LinkerRand: string[10] = ' ';
  Unterstrich: string = '-------------------------------------------------';

type
  TFWReport = class
  private
    FML: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AusgabeKoordinaten(KX, KY: TKnotenVektor; K: Integer);
    procedure AusgabeGeometrie(G: TGeometrie; S: Integer);
    procedure AusgabeBelastung(FX, FY: TKnotenVektor; K: Integer);
    procedure AusgabeAuflagerkraefte(Lager: TAuflager);
    procedure AusgabeStabkraefte(FS: TStabVektor; S: Integer);
    procedure AusgabeStabQuerschnitte(EA: TStabVektor; S: Integer);
    procedure AusgabeElastizitaeten(Q: TStabVektor; S: Integer);
    procedure AusgabeVerschiebungen
                (FOX, FOY, FO, POX, POY: TKnotenVektor; K: Integer);
    procedure Ausgabe(Fachwerk: TFachwerk);
    property ML: TStrings read FML;
  end;

  TRiggReport = class
  private
    FML: TStrings;
  public
    IndexAuswahlL: set of TRiggLIndexRange;
    IndexAuswahlP: set of TRiggPoint;
    constructor Create;
    destructor Destroy; override;
    procedure AusgabeRL(rL: TRiggLvektor);
    procedure AusgabeRLE(rLe: TRiggLvektor);
    procedure AusgabeDiffL(rL, rLe: TRiggLvektor);
    procedure AusgabeRP(rP: TRiggPoints);
    procedure AusgabeRPE(rPe: TRiggPoints);
    procedure AusgabeDiffP(rP, rPe: TRiggPoints);
    procedure AusgabeRF(rF: TRiggLvektor);
    procedure AusgabeWinkel(alpha, alpha1, alpha2, beta, gamma,
      delta1, delta2, epsilon, phi, psi: single);
    procedure AusgabeTrimmControls(Ctrls: TTrimmControls);
    procedure AusgabeSalingDaten(SData: TSalingDaten);
    procedure AusgabeLog(Liste: TStringList);
    property ML: TStrings read FML;
  end;

implementation

{ TFWReport }

constructor TFWReport.Create;
begin
  FML := TStringList.Create;
end;

destructor TFWReport.Destroy;
begin
  FML.Free;
end;

procedure  TFWReport.AusgabeKoordinaten(KX, KY: TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Koordinaten der Knotenpunkte in mm:']));
    Add(Format('%s K  %10s %10s', [LinkerRand,'x','y']));
    Add(Format('%s %s', [LinkerRand, Unterstrich]));
    for l:=1 to K do
    begin
      Add(Format('%s K%1d %10.3f %10.3f',
      [LinkerRand, l, KX[l], KY[l]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeGeometrie(G: TGeometrie; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Geometrie:']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d verbunden mit K%1d und K%1d',
      [LinkerRand, i, G[0,i], G[1,i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeBelastung(FX, FY: TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Belastung der Knotenpunkte in N:']));
    Add(Format('%s K  %10s %10s', [LinkerRand, 'x', 'y']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for l:=1 to K do
    begin
      Add(Format('%s K%1d %10.3f %10.3f',
      [LinkerRand, l, FX[l], FY[l]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeAuflagerkraefte(Lager: TAuflager);
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Auflagerkräfte in N:']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    Add(Format('%s FAX  %10.3f',[LinkerRand, Lager[AX]]));
    Add(Format('%s FAY  %10.3f',[LinkerRand, Lager[AY]]));
    Add(Format('%s FBX  %10.3f',[LinkerRand, Lager[BX]]));
    Add(Format('%s FBY  %10.3f',[LinkerRand, Lager[BY]]));
    Add('');
  end;
end;

procedure  TFWReport.AusgabeStabkraefte(FS: TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Stabkräfte in N:']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %12.3f', [LinkerRand, i, FS[i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeStabQuerschnitte(EA: TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'EA in KN:']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %10.3f', [LinkerRand, i, EA[i]]));
    end;
    Add('');
  end;
end;

procedure  TFWReport.AusgabeElastizitaeten(Q: TStabVektor; S: Integer);
var
  i: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s', [LinkerRand,'Stabelastizitäten in µm/N:']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for i:=1 to S do
    begin
      Add(Format('%s S%1d %6.3f', [LinkerRand, i, Q[i]]));
    end;
    Add('');
  end;
end;

procedure TFWReport.AusgabeVerschiebungen(
FOX, FOY, FO, POX, POY: TKnotenVektor; K: Integer);
var
  l: Integer;
begin
  with FML do
  begin
    Add(Format('%s %s',[LinkerRand,'Verschiebungen der Knotenpunkte:']));
    Add(Format('%s %s',[LinkerRand,'(F0X - Verschiebung in 1. Richtung P0X)']));
    Add(Format('%s %s',[LinkerRand,'(F0Y - Verschiebung in 2. Richtung P0Y)']));
    Add(Format('%s %s',[LinkerRand,'(F0  - Absolutbetrag)']));
    Add(Format('%s %2s %7s %9s %7s %9s %9s',
    [LinkerRand,'','P0X','F0X','P0Y','F0Y','F0']));
    Add(Format('%s %2s %7s %9s %7s %9s %9s',
    [LinkerRand,'','/Grad','/mm','/Grad','/mm','/mm']));
    Add(Format('%s %s',[LinkerRand, Unterstrich]));
    for l:=1 To K do
      Add(Format('%s K%1d %7.1f %9.2f %7.1f %9.2f %9.2f',
      [LinkerRand,l,POX[l],FOX[l],POY[l],FOY[l],FO[l]]));
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

procedure TRiggReport.AusgabeRL(rL: TRiggLvektor);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längen belastet in mm (Vektor rL):');
    Add('  -------------------------------------------------');
    for i := 0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rL[%2d] %10.3f  (%s)',
      [i, rL.V[i], AbstandLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeRLE(rLe: TRiggLvektor);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längen entlastet in mm (Vektor rLe):');
    Add('  -------------------------------------------------');
    for i:=0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rLe[%2d]  %10.3f  (%s)',
      [i, rLe.V[i], AbstandLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeDiffL(rL, rLe: TRiggLvektor);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Längenänderungen in mm  (rLe[i]-rL[i]):');
    Add('  -------------------------------------------------');
    for i:=0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  %2d %10.3f  (%s)',
      [i, (rLe.V[i]-rL.V[i]), AbstandLabels[i]]));
    end;
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
    Add('  -------------------------------------------------');
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
    Add('  -------------------------------------------------');
    for i := ooA0 to ooP do
    begin
      if i in IndexAuswahlP then
      Add(Format('  rPe[%s] %8.2f %8.2f %8.2f  (%s)',
      [KoordTexte[i], rPe.V[i].X, rPe.V[i].Y, rPe.V[i].Z, KoordLabels[i]]));
    end;
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
    Add('  -------------------------------------------------');
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

procedure TRiggReport.AusgabeRF(rF: TRiggLvektor);
var
  i: Integer;
begin
  with FML do
  begin
    Add('  Kräfte in N (Vektor rF):');
    Add('  -------------------------------------------------');
    for i := 0 to 19 do
    begin
      if i in IndexAuswahlL then
      Add(Format('  rF[%2d] %10.0f  (%s)',
      [Ord(i), rF.V[i], AbstandLabels[i]]));
    end;
    Add('');
  end;
end;

procedure TRiggReport.AusgabeWinkel(alpha, alpha1, alpha2, beta, gamma,
  delta1, delta2, epsilon, phi, psi: single);
begin
  with FML do
  begin
    Add('  Winkel:');
    Add('  -------------------------------------------------');
    Add(Format('%s phi = %4.2f Grad', [LinkerRand, phi*180/pi]));
    Add(Format('%s psi = %4.2f Grad', [LinkerRand, psi*180/pi]));
    Add(Format('%s alpha = %4.2f Grad', [LinkerRand, alpha*180/pi]));
    Add(Format('%s phi-alpha = %4.2f Grad', [LinkerRand, (phi-alpha)*180/pi]));
    Add(Format('%s psi-alpha = %4.2f Grad', [LinkerRand, (psi-alpha)*180/pi]));
    Add(Format('%s alpha1 = %4.2f Grad', [LinkerRand, alpha1*180/pi]));
    Add(Format('%s alpha2 = %4.2f Grad', [LinkerRand, alpha2*180/pi]));
    Add(Format('%s delta1 = %4.2f Grad', [LinkerRand, delta1*180/pi]));
    Add(Format('%s delta2 = %4.2f Grad', [LinkerRand, delta2*180/pi]));
    Add(Format('%s gamma = %4.2f Grad', [LinkerRand, gamma*180/pi]));
    Add(Format('%s beta = %4.2f Grad', [LinkerRand, beta*180/pi]));
    Add(Format('%s epsilon = %4.2f Grad', [LinkerRand, epsilon*180/pi]));
    Add('');
  end;
end;

procedure TRiggReport.AusgabeTrimmControls(Ctrls: TTrimmControls);
begin
  with FML do
  begin
    Add('  Einstellungen (TTrimmControls):');
    Add('  -------------------------------------------------');
    Add(Format('%s Controller = %d mm', [LinkerRand, Ctrls.Controller]));
    Add(Format('%s Winkel = %d 10E-1 Grad', [LinkerRand, Ctrls.Winkel]));
    Add(Format('%s Vorstag = %d mm', [LinkerRand, Ctrls.Vorstag]));
    Add(Format('%s Wanten = %d mm', [LinkerRand, Ctrls.Wanten]));
    Add(Format('%s Wunten = %d mm', [LinkerRand, Ctrls.Wanten-Ctrls.Woben]));
    Add(Format('%s Woben = %d mm', [LinkerRand, Ctrls.Woben]));
    Add(Format('%s SalingH = %d mm', [LinkerRand, Ctrls.SalingH]));
    Add(Format('%s SalingA = %d mm', [LinkerRand, Ctrls.SalingA]));
    Add(Format('%s SalingL = %d mm', [LinkerRand, Ctrls.SalingL]));
    Add('');
  end;
end;

procedure TRiggReport.AusgabeSalingDaten(SData: TSalingDaten);
begin
  with FML do
  begin
    Add('  Salinge (TSalingDaten):');
    Add('  -------------------------------------------------');
    Add(Format('%s SalingH = %6.2f mm', [LinkerRand, SData.SalingH]));
    Add(Format('%s SalingA = %6.2f mm', [LinkerRand, SData.SalingA]));
    Add(Format('%s SalingL = %6.2f mm', [LinkerRand, SData.SalingL]));
    Add(Format('%s SalingW = %6.2f Grad', [LinkerRand, SData.SalingW]));
    Add(Format('%s WantenWinkel = %6.2f Grad',
    [LinkerRand, SData.WantenWinkel]));
    Add(Format('%s KraftWinkel = %6.2f Grad',
    [LinkerRand, SData.KraftWinkel]));
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
    Add('  -------------------------------------------------');
    for i := 0 to Liste.Count-1 do
    begin
      Add(Format('%s %s', [LinkerRand, Liste[i]]));
    end;
    Add('');
  end;
end;

end.
