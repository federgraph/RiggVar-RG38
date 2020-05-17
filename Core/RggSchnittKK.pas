unit RggSchnittKK;

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
  System.Types,
  System.Math,
  RggTypes,
  RggCalc;

type
  TBemerkungKK = (
    bmKonzentrisch,
    bmZwei,
    bmEinerAussen,
    bmEntfernt,
    bmEinerK1inK2,
    bmEinerK2inK1,
    bmK1inK2,
    bmK2inK1,
    bmRadiusFalsch
  );

  TSchnittEbene = (seXY, seYZ, seXZ);

  TSchnittKK = class
  private
    R1: double;
    R2: double;
    FM1: TRealPoint;
    FM2: TRealPoint;
    S1: TRealPoint;
    S2: TRealPoint;
    Ebene: TSchnittEbene;
    Bem: TBemerkungKK;
    NeedCalc: Boolean;
    sv: Boolean;
    procedure SetRadius1(Value: double);
    procedure SetRadius2(Value: double);
    procedure SetMittelPunkt1(Value: TRealPoint);
    procedure SetMittelPunkt2(Value: TRealPoint);
    function GetSchnittPunkt1: TRealPoint;
    function GetSchnittPunkt2: TRealPoint;
    function GetBem: TBemerkungKK;
    function GetBemerkung: string;
    function Vorhanden: Boolean;
    procedure SetM1(const Value: TPointF);
    procedure SetM2(const Value: TPointF);
//    function GetM1: TPointF;
//    function GetM2: TPointF;
  protected
    procedure Schnitt; virtual;
  public
    property Radius1: double read R1 write SetRadius1;
    property Radius2: double read R2 write SetRadius2;
    property M1: TPointF write SetM1;
    property M2: TPointF write SetM2;
    property MittelPunkt1: TRealPoint read FM1 write SetMittelPunkt1;
    property MittelPunkt2: TRealPoint read FM2 write SetMittelPunkt2;
    property SchnittPunkt1: TRealPoint read GetSchnittPunkt1;
    property SchnittPunkt2: TRealPoint read GetSchnittPunkt2;
    property Status: TBemerkungKK read GetBem;
    property Bemerkung: string read GetBemerkung;
    property SPVorhanden: Boolean read Vorhanden;
    property SchnittEbene: TSchnittEbene read Ebene write Ebene;
  end;

  TSchnittKKR = class(TSchnittKK)
  protected
    procedure Schnitt; override;
  public
    RectangleMode: Boolean;
  end;

  TSplitF = class
  public
    l1, l2, h: double;
    F, F1, F2: double;
    alpha: double;
    procedure SplitCalc;
  end;

  TTetraF = class
  public
    d1, d2, d3, d4: TRealPoint;
    l1, l2, l3, l4: double;
    F1, F2, F3, F4: double;
    FR: TRealPoint;
    SkalarProdukt: double;
    Toleranz: double;
    KnotenLast: TRealPoint;
    ProbeErgebnis: double;
    constructor Create;
    procedure VierteKraft;
    function SkalarProduktPositiv: Boolean;
    function Probe: Boolean;
  end;

implementation

{ TSchnittKK }

procedure TSchnittKK.SetRadius1(Value: double);
begin
  if Value <> R1 then
  begin
    R1 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetRadius2(Value: double);
begin
  if Value <> R2 then
  begin
    R2 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetM1(const Value: TPointF);
begin
  FM1[x] := Value.X;
  FM1[y] := Value.Y;
  FM1[z] := 0;
  NeedCalc := True;
end;

procedure TSchnittKK.SetM2(const Value: TPointF);
begin
  FM2[x] := Value.X;
  FM2[y] := Value.Y;
  FM2[z] := 0;
  NeedCalc := True;
end;

procedure TSchnittKK.SetMittelPunkt1(Value: TRealPoint);
begin
  if (Value[x] <> FM1[x]) or (Value[y] <> FM1[y]) or (Value[z] <> FM1[z]) then
  begin
    FM1 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetMittelPunkt2(Value: TRealPoint);
begin
  if (Value[x] <> FM2[x]) or (Value[y] <> FM2[y]) or (Value[z] <> FM2[z]) then
  begin
    FM2 := Value;
    NeedCalc := True;
  end;
end;

function TSchnittKK.GetSchnittPunkt1: TRealPoint;
begin
  if NeedCalc = True then
    Schnitt;
  result := S1;
end;

function TSchnittKK.GetSchnittPunkt2: TRealPoint;
begin
  if NeedCalc = True then
    Schnitt;
  result := S2;
end;

function TSchnittKK.GetBem: TBemerkungKK;
begin
  if NeedCalc = True then
    Schnitt;
  result := Bem;
end;

function TSchnittKK.Vorhanden: Boolean;
begin
  if NeedCalc = True then
    Schnitt;
  result := sv;
end;

function TSchnittKK.GetBemerkung: string;
begin
  if NeedCalc = True then
    Schnitt;
  case Bem of
    bmKonzentrisch:
      result := 'konzentrische Kreise';
    bmZwei:
      result := 'zwei Schnittpunkte';
    bmEntfernt:
      result := 'zwei entfernte Kreise';
    bmEinerAussen:
      result := 'Berührung außen';
    bmEinerK1inK2:
      result := 'Berührung innen, K1 in K2';
    bmEinerK2inK1:
      result := 'Berührung innen, K2 in K1';
    bmK1inK2:
      result := 'K1 innerhalb K2';
    bmK2inK1:
      result := 'K2 innerhalb K1';
    bmRadiusFalsch:
      result := 'Radius Ungültig';
  end;
end;

//function TSchnittKK.GetM1: TPointF;
//begin
//  result.X := Mittelpunkt1[x];
//  result.Y := Mittelpunkt1[y];
//end;
//
//function TSchnittKK.GetM2: TPointF;
//begin
//  result.X := Mittelpunkt2[x];
//  result.Y := Mittelpunkt2[y];
//end;

procedure TSchnittKK.Schnitt;
label
  M;
var
  a, b, h1, h2, h3, p, q, Entfernung: Extended;
  DeltaNullx, DeltaNully: Boolean;
  M1M2, M1S1, KreuzProd: TRealPoint;
  M1, M2, SP: TRealPoint;
begin
  NeedCalc := False;
  sv := False;

  S1 := Null;
  S2 := Null;

  if Ebene = seXY then
  begin
    M1[x] := FM1[x];
    M1[y] := FM1[y];
    M1[z] := 0;
    M2[x] := FM2[x];
    M2[y] := FM2[y];
    M2[z] := 0;
  end
  else if Ebene = seXZ then
  begin
    M1[x] := FM1[x];
    M1[y] := FM1[z];
    M1[z] := 0;
    M2[x] := FM2[x];
    M2[y] := FM2[z];
    M2[z] := 0;
  end
  else if Ebene = seYZ then
  begin
    M1[x] := FM1[y];
    M1[y] := FM1[z];
    M1[z] := 0;
    M2[x] := FM2[y];
    M2[y] := FM2[z];
    M2[z] := 0;
  end;

  { Radien sollen größer Null sein }
  if (R1 <= 0) or (R2 <= 0) then
  begin
    Bem := bmRadiusFalsch;
    Exit;
  end;

  DeltaNullx := M2[x] - M1[x] = 0;
  DeltaNully := M2[y] - M1[y] = 0;
  { Spezialfall konzentrische Kreise }
  if DeltaNullx and DeltaNully then
  begin
    Bem := bmKonzentrisch;
    Exit;
  end;

  h1 := (R1 * R1 - R2 * R2) + (M2[x] * M2[x] - M1[x] * M1[x]) + (M2[y] * M2[y] - M1[y] * M1[y]);
  { Spezialfall Mittelpunkte auf gleicher Höhe }
  if DeltaNully then { Rechnung vermeidet Division durch Null }
  begin
    S1[x] := h1 / (2 * (M2[x] - M1[x]));
    S2[x] := S1[x];
    h3 := R1 * R1 - sqr(S1[x] - M1[x]);
    if h3 < 0 then { kein Schnittpunkt }
    begin
      S1 := Null;
      S2 := Null;
      goto M;
    end;
    if h3 = 0 then { ein Schnittpunkt bzw. zwei identische }
    begin
      S1[y] := M1[y];
      S2[y] := S1[y];
      sv := True;
      goto M;
    end;
    if h3 > 0 then { zwei verschiedene Schnittpunkte }
    begin
      S1[y] := M1[y] + sqrt(h3);
      S2[y] := M1[y] - sqrt(h3);
      sv := True;
      goto M;
    end;
  end; { if DeltaNully }

  { Rechnung im Normalfall }
  a := (-1) * (M2[x] - M1[x]) / (M2[y] - M1[y]);
  b := h1 / (2 * (M2[y] - M1[y]));
  p := 2 * (a * b - M1[x] - a * M1[y]) / (1 + a * a);
  q := (M1[x] * M1[x] + b * b - 2 * b * M1[y] + M1[y] * M1[y] - R1 * R1) / (1 + a * a);
  h2 := p * p / 4 - q;
  if h2 >= 0 then
  begin
    h2 := sqrt(h2);
    S1[x] := -p / 2 + h2;
    S2[x] := -p / 2 - h2;
    S1[y] := a * S1[x] + b;
    S2[y] := a * S2[x] + b;
    sv := True;
  end;

M:
  Entfernung := Abstand(M1, M2);

  if sv = False then
  begin
    if Entfernung > R1 + R2 then
      Bem := bmEntfernt
    else if Entfernung + R1 < R2 then
      Bem := bmK1inK2
    else if Entfernung + R2 < R1 then
      Bem := bmK2inK1;
    Exit;
  end;

  if sv = True then
  begin
    Bem := bmZwei;
    if Entfernung + R1 = R2 then
      Bem := bmEinerK1inK2
    else if Entfernung + R2 = R1 then
      Bem := bmEinerK2inK1
    else if Entfernung = R1 + R2 then
      Bem := bmEinerAussen;
  end;

  { den "richtigen" SchnittPunkt ermitteln }
  if Bem = bmZwei then
  begin
    M1M2 := vsub(M2, M1);
    M1S1 := vsub(S1, M1);
    KreuzProd := vprod(M1M2, M1S1);
    if KreuzProd[z] < 0 then
    begin
      SP := S2;
      S2 := S1;
      S1 := SP;
    end;
  end;

  if Ebene = seXZ then
  begin
    S1[z] := S1[y];
    S1[y] := 0;
    S2[z] := S2[y];
    S2[y] := 0;
  end
  else if Ebene = seYZ then
  begin
    S1[x] := S1[y];
    S1[y] := S1[z];
    S1[z] := 0;
    S2[x] := S2[y];
    S2[y] := S2[z];
    S2[z] := 0;
  end;

end;

{ TSplitF }

procedure TSplitF.SplitCalc;
begin
  alpha := arctan2(l2 / 2, h);
  F1 := F / 2 / cos(alpha);
  F2 := F1;
  l1 := h / cos(alpha);
end;

{ TTetraF }

constructor TTetraF.Create;
begin
  inherited Create;
  Toleranz := 2;
  KnotenLast := Null;
end;

procedure TTetraF.VierteKraft;
begin
  d1[x] := d1[x] / l1;
  d2[x] := d2[x] / l2;
  d3[x] := d3[x] / l3;

  d1[y] := d1[y] / l1;
  d2[y] := d2[y] / l2;
  d3[y] := d3[y] / l3;

  d1[z] := d1[z] / l1;
  d2[z] := d2[z] / l2;
  d3[z] := d3[z] / l3;

  FR[x] := F1 * d1[x] + F2 * d2[x] + F3 * d3[x];
  FR[y] := F1 * d1[y] + F2 * d2[y] + F3 * d3[y];
  FR[z] := F1 * d1[z] + F2 * d2[z] + F3 * d3[z];

  F4 := Abstand(FR, Null);
  if FR[y] < 0 then
    F4 := -F4;
end;

function TTetraF.SkalarProduktPositiv: Boolean;
begin
  result := False;
  SkalarProdukt := FR[x] * d4[x] + FR[y] * d4[y] + FR[z] * d4[z];
  if SkalarProdukt >= 0 then
    result := True;
end;

function TTetraF.Probe: Boolean;
begin
  result := True;

  d1[x] := d1[x] / l1;
  d2[x] := d2[x] / l2;
  d3[x] := d3[x] / l3;
  d4[x] := d4[x] / l4;

  d1[y] := d1[y] / l1;
  d2[y] := d2[y] / l2;
  d3[y] := d3[y] / l3;
  d4[y] := d4[y] / l4;

  d1[z] := d1[z] / l1;
  d2[z] := d2[z] / l2;
  d3[z] := d3[z] / l3;
  d4[z] := d4[z] / l4;

  FR[x] := F1 * d1[x] + F2 * d2[x] + F3 * d3[x] + F4 * d4[x];
  FR[y] := F1 * d1[y] + F2 * d2[y] + F3 * d3[y] + F4 * d4[y];
  FR[z] := F1 * d1[z] + F2 * d2[z] + F3 * d3[z] + F4 * d4[z];

  FR := vadd(FR, KnotenLast);
  ProbeErgebnis := Abstand(FR, Null);
  if ProbeErgebnis > Toleranz then
    result := False;
end;

{ TSchnittKKR }

procedure TSchnittKKR.Schnitt;
var
  t1, t2, t: TRect;
  ret: Boolean;
begin
  if RectangleMode then
  begin
    S1 := Null; //vcopy(S1, Null);
    S2 := Null; //vcopy(S2, Null);
    try
      t1.Left := Round(FM1[x] - R1);
      t1.Right := Round(FM1[x] + R1);
      t1.Top := Round(FM1[y] - R1);
      t1.Bottom := Round(FM1[y] + R1);

      t2.Left := Round(FM2[x] - R2);
      t2.Right := Round(FM2[x] + R2);
      t2.Top := Round(FM2[y] - R2);
      t2.Bottom := Round(FM2[y] + R2);

      { fpc compiler warned that t does not seem to be initialized }
      t.Left := 0;
      t.Right := 0;
      t.Top := 0;
      t.Bottom := 0;

      ret := IntersectRect(t, t1, t2);
      if ret then
      begin
        S1[x] := t.Left;
        S1[y] := t.Top;
        S2[x] := t.Right;
        S2[y] := t.Bottom;
      end;
    except
    end;
  end
  else
    inherited Schnitt;
end;

end.
