unit RggFachwerk;

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
  RggTypes;

type
  TGeometrie = array [0 .. 1, 1 .. 9] of Integer;
  TKnotenVektor = array [1 .. 6] of single;
  TStabVektor = array [1 .. 9] of single;
  Lagerkraefte = (AX, AY, BX, BY);
  TAufLager = array [Lagerkraefte] of single;

const
  { Geometriematrix, Stab in Spalte verbindet die beiden Knoten }
  constantG: TGeometrie =
   ((1, 1, 2, 2, 3, 3, 4, 4, 5),
    (4, 5, 6, 3, 5, 4, 6, 5, 6));

  { EA in N }
  constantEA: TStabVektor = (1E6, 1E6, 2E6, 2E6, 2E6, 2E6, 10E6, 10E6, 10E6);

  { Anfangswerte für Knotenpunkt-Koordinaten in mm }
  InitX: TKnotenVektor = (2870, 2300, 2800, 4140, 2870, 2560);
  InitY: TKnotenVektor = ( 400, 2600, 4600, 340, -100, 430);

  { Anfangswert für äußere Belastung in N }
  BelastungX: TKnotenVektor = (0.00, 0.00, 0.00, 0, 0.00, 0);
  BelastungY: TKnotenVektor = (0.00, 0.00, 0.00, 0, 0.00, 0);

  { Richtungen der Kraft "1" für Ermittlung der Knotenverschiebungen in Grad }
  PO1: TKnotenVektor = (  0,   0,   0,   0,   0,   0);
  PO2: TKnotenVektor = (-90, -90, -90, -90, -90, -90);

  { Vektoren zum bequemen Löschen der Arrays }
  ClearVektorK: TKnotenVektor = (0, 0, 0, 0, 0, 0);
  ClearVektorS: TStabVektor = (0, 0, 0, 0, 0, 0, 0, 0, 0);

type
  TFachwerk = class
  public
    K: Integer; { Anzahl der Knoten }
    K1: Integer; { KnotenNr. des Festlagers A }
    K2: Integer; { KnotenNr. des Loslagers B }
    Phi: single; { Winkel von FB in Grad }
    S: Integer; { Anzahl der Staebe }
    G: TGeometrie; { Geometriematrix }
    vektorEA: TStabVektor; { Vektor EA }
    KX, KY: TKnotenVektor; { Koordinaten }
    FX, FY: TKnotenVektor; { Belastung }
    FXsaved, FYsaved: TKnotenVektor; { Kopie des Belastungsvektors }
    FO, FO1, FO2: TKnotenVektor; { Verschiebungen }
    Lager: TAufLager; { Auflagerreaktionen }
    P: single; { Winkel der Auflagekraft im Loslager B in Rad }
    Q: TStabVektor; { l/EA Vektor }
    FS1: TStabVektor; { Puffer für Stabkräfte }
    FS: TStabVektor; { Speicher-Vektor der Stabkräfte bei äußerer Last }
    SummeFX, SummeFY, SummeMO: single; { summierte Belastungen }
    DX1, DY1, W1: single;
    DX2, DY2, W2: single;
    DX3, DY3, W3: single;
    DX4, DY4, W4: single;
    D, D1, D2, W: single;
    BekanntFX, BekanntFY: single;
    WantenPower: single;
    MastDruck: single;
    SalingTyp: TSalingTyp;
    BerechneVerschiebungen: Boolean;
    constructor Create;
    procedure KG22(l, l1, l2, l3, l4, i1, i2, i3, i4: Integer);
    procedure KG20(l, l1, l2, i1, i2: Integer);
    procedure KG21(l, l1, l2, l3, i1, i2, i3: Integer);
    procedure Stabkraefte;
    procedure StabkraefteOSB;
    procedure Auflagerkraefte(SumFX, SumFY, SumMO: single; out Lager: TAuflager);
    procedure Verschiebungen;
    procedure ActionF;
  end;

implementation

constructor TFachwerk.Create;
begin
  BerechneVerschiebungen := False;
  SalingTyp := stFest;
  K := 6; { Anzahl der Knoten }
  K1 := 6; { KnotenNr. des Festlagers A }
  K2 := 5; { KnotenNr. des Loslagers B }
  Phi := 30; { Winkel von FB in Grad }
  S := 2 * K - 3; { Anzahl der Staebe }
  G := constantG;
  vektorEA := constantEA;
  KX := InitX;
  KY := InitY;
  FX := BelastungX;
  FY := BelastungY;
  P := Phi * pi / 180;
  FS1 := ClearVektorS;
end;

procedure TFachwerk.KG22(l, l1, l2, l3, l4, i1, i2, i3, i4: Integer);
begin
  { unbekannte Kraft Nr.1 }
  DX1 := KX[l1] - KX[l]; { delta x }
  DY1 := KY[l1] - KY[l]; { delta y }
  W1 := Sqrt(sqr(DX1) + sqr(DY1)); { Stablänge }
  { unbekannte Kraft Nr.2 }
  DX2 := KX[l2] - KX[l]; { delta x }
  DY2 := KY[l2] - KY[l]; { delta y }
  W2 := Sqrt(sqr(DX2) + sqr(DY2)); { Stablänge }
  { bekannte Kraft Nr.3 }
  DX3 := KX[l3] - KX[l]; { delta x }
  DY3 := KY[l3] - KY[l]; { delta y }
  W3 := Sqrt(sqr(DX3) + sqr(DY3)); { Stablänge }
  { bekannte Kraft Nr.4 }
  DX4 := KX[l4] - KX[l]; { delta x }
  DY4 := KY[l4] - KY[l]; { delta y }
  W4 := Sqrt(sqr(DX4) + sqr(DY4)); { Stablänge }
  { Summe der bekannten Kräfte }
  { mit DX/W = cos alpha, DY/W = sin alpha }
  BekanntFX := -FX[l] - FS1[i3] * DX3 / W3 - FS1[i4] * DX4 / W4;
  BekanntFY := -FY[l] - FS1[i3] * DY3 / W3 - FS1[i4] * DY4 / W4;
  { Ausrechnen der Stabkräfte }
  D := DX1 * DY2 - DX2 * DY1;
  D1 := BekanntFX * DY2 - BekanntFY * DX2;
  D2 := BekanntFY * DX1 - BekanntFX * DY1;
  FS1[i1] := D1 / D * W1; { 1. neu ermittelte Stabkraft }
  FS1[i2] := D2 / D * W2; { 2. neu ermittelte Stabkraft }
end; { KG22 }

procedure TFachwerk.KG20(l, l1, l2, i1, i2: Integer);
begin
  { unbekannte Kraft Nr.1 }
  DX1 := KX[l1] - KX[l]; { delta x }
  DY1 := KY[l1] - KY[l]; { delta y }
  W1 := Sqrt(sqr(DX1) + sqr(DY1)); { Stablänge }
  { unbekannte Kraft Nr.2 }
  DX2 := KX[l2] - KX[l]; { delta x }
  DY2 := KY[l2] - KY[l]; { delta y }
  W2 := Sqrt(sqr(DX2) + sqr(DY2)); { Stablänge }
  { Summe der bekannten Kräfte }
  BekanntFX := -FX[l];
  BekanntFY := -FY[l];
  { Ausrechnen der Stabkräfte }
  D := DX1 * DY2 - DX2 * DY1;
  D1 := BekanntFX * DY2 - BekanntFY * DX2;
  D2 := BekanntFY * DX1 - BekanntFX * DY1;
  if D <> 0 then
  begin
    FS1[i1] := D1 / D * W1; { 1. neu ermittelte Stabkraft }
    FS1[i2] := D2 / D * W2; { 2. neu ermittelte Stabkraft }
  end
  else
  begin
    FS1[i1] := 0;
    FS1[i2] := 0;
  end;
end; { KG20 }

procedure TFachwerk.KG21(l, l1, l2, l3, i1, i2, i3: Integer);
begin
  { unbekannte Kraft Nr.1 }
  DX1 := KX[l1] - KX[l]; { delta x }
  DY1 := KY[l1] - KY[l]; { delta y }
  W1 := Sqrt(sqr(DX1) + sqr(DY1)); { Stablänge }
  { unbekannte Kraft Nr.2 }
  DX2 := KX[l2] - KX[l]; { delta x }
  DY2 := KY[l2] - KY[l]; { delta y }
  W2 := Sqrt(sqr(DX2) + sqr(DY2)); { Stablänge }
  { bekannte Kraft Nr.3 }
  DX3 := KX[l3] - KX[l]; { delta x }
  DY3 := KY[l3] - KY[l]; { delta y }
  W3 := Sqrt(sqr(DX3) + sqr(DY3)); { Stablänge }
  { Summe der bekannten Kräfte }
  { mit DX/W = cos alpha, DY/W = sin alpha }
  if W3 = 0 then
  begin
    BekanntFX := 0;
    BekanntFY := 0;
    FS1[i1] := 0; { 1. neu ermittelte Stabkraft }
    FS1[i2] := 0; { 2. neu ermittelte Stabkraft }
    Exit;
  end;
  BekanntFX := -FX[l] - FS1[i3] * DX3 / W3;
  BekanntFY := -FY[l] - FS1[i3] * DY3 / W3;
  { Ausrechnen der Stabkräfte }
  D := DX1 * DY2 - DX2 * DY1;
  D1 := BekanntFX * DY2 - BekanntFY * DX2;
  D2 := BekanntFY * DX1 - BekanntFX * DY1;
  FS1[i1] := D1 / D * W1; { 1. neu ermittelte Stabkraft }
  FS1[i2] := D2 / D * W2; { 2. neu ermittelte Stabkraft }
end; { KG21 }

procedure TFachwerk.Stabkraefte;
{ Puffer der ermittelten Stabkräfte ist Vektor FS1 }
begin
  KG20(1, 5, 4, 2, 1);

  if SalingTyp = stOhneStarr then
  begin
    { Eine Kraft vorgeben! }
    FS1[4] := WantenPower;
    FS1[3] := WantenPower;
  end
  else
    KG20(2, 3, 6, 4, 3);

  KG21(3, 4, 5, 2, 6, 5, 4);
  KG22(4, 5, 6, 3, 1, 8, 7, 6, 1);
  KG21(6, 5, 4, 2, 9, 7, 3);
end;

procedure TFachwerk.StabkraefteOSB;
begin
  { Controller-Zweischlag }
  KG20(1, 5, 4, 2, 1);

  { Punkt C, MastDruck vorgegeben }
  FS1[5] := MastDruck;
  KG21(3, 4, 2, 5, 6, 4, 5);

  { Punkt P, weil Ohne Saling: }
  FS1[3] := FS1[4];

  KG22(4, 5, 6, 3, 1, 8, 7, 6, 1);
  KG21(6, 5, 4, 2, 9, 7, 3);
end;

procedure TFachwerk.Auflagerkraefte(SumFX, SumFY, SumMO: single; out Lager: TAuflager);
var
  FB: single;
begin
  FB := -SumMO / ((KX[K2] - KX[K1]) * sin(P) - (KY[K2] - KY[K1]) * cos(P));
  Lager[BX] := FB * cos(P);
  Lager[BY] := FB * sin(P);
  Lager[AX] := -SumFX - Lager[BX];
  Lager[AY] := -SumFY - Lager[BY];
end;

procedure TFachwerk.Verschiebungen;
var
  i, j, l: Integer;
  Sum1FX, Sum1FY, Sum1MO: single;
  Lager1: TAufLager;
  PORad: single;
  F: single;
begin
  PORad := 0;
  for l := 1 To K do
  begin
    for j := 0 To 1 do
    begin
      { 1. neue Richtung aus Vektor holen. }
      case j of
        0:
          PORad := PO1[l] * pi / 180; { Richtung in Rad }
        1:
          PORad := PO2[l] * pi / 180; { Richtung in Rad }
      end;

      { 2. FW nur mit Kraft "1" auf Knoten l belasten. }
      Sum1FX := cos(PORad); { x-Komponenten der Last "1" }
      Sum1FY := sin(PORad); { y-Komponenten der Last "1" }
      Sum1MO := Sum1FY * (KX[l] - KX[K1]) - Sum1FX * (KY[l] - KY[K1]);

      { 3. Auflagerkräfte für Belatung mit "1" ausrechnen. }
      Auflagerkraefte(Sum1FX, Sum1FY, Sum1MO, Lager1);

      { 4. Belastungs-Vektor für "1" - Belastung aktualisieren. }
      FX := ClearVektorK;
      FY := ClearVektorK;
      FX[l] := Sum1FX;
      FY[l] := Sum1FY;
      FX[K1] := FX[K1] + Lager1[AX];
      FY[K1] := FY[K1] + Lager1[AY];
      FX[K2] := FX[K2] + Lager1[BX];
      FY[K2] := FY[K2] + Lager1[BY];

      { 5. Stabkräfte bei "1" - Belastung berechnen. }
      Stabkraefte;

      { 6. Verschiebungsanteile summieren. }

      F := 0;
      for i := 1 to S do
      begin
        Q[i] := Sqrt(sqr(KX[G[0, i]] - KX[G[1, i]]) +
          sqr(KY[G[0, i]] - KY[G[1, i]]));
        Q[i] := Q[i] / vektorEA[i];
        F := F + FS[i] * FS1[i] * Q[i];
      end;

      { 7. Verschiebungskomponente des Knotens k im Vektor speichern. }
      case j of
        0:
          FO1[l] := F;
        1:
          FO2[l] := F;
      end;
    end;

    { Speichern des absoluten Verschiebungsbetrages des Knotens l }
    FO[l] := Sqrt(sqr(FO1[l]) + sqr(FO2[l]));
  end;
end;

procedure TFachwerk.ActionF;
var
  i, l: Integer;
begin
  { Belastungsvektor für die Ausgabe sichern; FX und FY werden bei der
    Berechnung der Verschiebungen überschrieben }
  FXsaved := FX;
  FYsaved := FY;

  { Aufsummieren der äußeren Belastung }
  SummeFX := 0;
  SummeFY := 0;
  SummeMO := 0;
  for l := 1 To K do
  begin
    SummeFX := SummeFX + FX[l];
    SummeFY := SummeFY + FY[l];
    SummeMO := SummeMO + FY[l] * (KX[l] - KX[K1]) 
                       - FX[l] * (KY[l] - KY[K1]);
  end;

  { Auflagerkräfte ausrechnen }
  Auflagerkraefte(SummeFX, SummeFY, SummeMO, Lager);

  { Die Auflagerkräfte werden zu den Knotenlasten addiert: }
  FX[K1] := FX[K1] + Lager[AX];
  FY[K1] := FY[K1] + Lager[AY];
  FX[K2] := FX[K2] + Lager[BX];
  FY[K2] := FY[K2] + Lager[BY];

  { Stabkräfte ausrechnen }
  if SalingTyp = stOhneBiegt then
    StabkraefteOSB
  else
    Stabkraefte;

  { Stabkräfte von FS1 nach FS kopieren. Die Prozedur Stabkräfte
    speichert die ermittelten Stabkräfte immer in FS1! }
  for i := 1 to S do
  begin
    FS[i] := FS1[i];
  end;

  if (SalingTyp = stOhneStarr)  or (SalingTyp = stOhneBiegt) and
    (BerechneVerschiebungen = True) then
    { Verschiebungen ausrechnen }
    Verschiebungen; { jetzt sind die Stabkräfte unter Last "1" im
    Vektor FS1 gespeichert, die Stabkräfte unter der wirklichen Last
    immer noch in Vektor FS }
end;

end.
