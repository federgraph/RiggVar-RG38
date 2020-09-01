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
  System.Math.Vectors,
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
    R1: single;
    R2: single;
    FM1: TPoint3D;
    FM2: TPoint3D;
    S1: TPoint3D;
    S2: TPoint3D;
    Ebene: TSchnittEbene;
    Bem: TBemerkungKK;
    NeedCalc: Boolean;
    sv: Boolean;
    procedure SetRadius1(Value: single);
    procedure SetRadius2(Value: single);
    procedure SetMittelPunkt1(Value: TPoint3D);
    procedure SetMittelPunkt2(Value: TPoint3D);
    function GetSchnittPunkt1: TPoint3D;
    function GetSchnittPunkt2: TPoint3D;
    function GetBem: TBemerkungKK;
    function GetBemerkung: string;
    function Vorhanden: Boolean;
    procedure SetM1(const Value: TPointF);
    procedure SetM2(const Value: TPointF);
  protected
    procedure Schnitt; virtual;
  public
    Watch1: Integer;
    Watch2: Integer;
    property Radius1: single read R1 write SetRadius1;
    property Radius2: single read R2 write SetRadius2;
    property M1: TPointF write SetM1;
    property M2: TPointF write SetM2;
    property MittelPunkt1: TPoint3D read FM1 write SetMittelPunkt1;
    property MittelPunkt2: TPoint3D read FM2 write SetMittelPunkt2;
    property SchnittPunkt1: TPoint3D read GetSchnittPunkt1;
    property SchnittPunkt2: TPoint3D read GetSchnittPunkt2;
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
    l1, l2, h: single;
    F, F1, F2: single;
    alpha: single;
    procedure SplitCalc;
  end;

  TTetraF = class
  public
    d1, d2, d3, d4: TPoint3D;
    l1, l2, l3, l4: single;
    F1, F2, F3, F4: single;
    FR: TPoint3D;
    SkalarProdukt: single;
    Toleranz: single;
    KnotenLast: TPoint3D;
    ProbeErgebnis: single;
    constructor Create;
    procedure VierteKraft;
    function SkalarProduktPositiv: Boolean;
    function Probe: Boolean;
  end;

implementation

{ TSchnittKK }

procedure TSchnittKK.SetRadius1(Value: single);
begin
  if Value <> R1 then
  begin
    R1 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetRadius2(Value: single);
begin
  if Value <> R2 then
  begin
    R2 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetM1(const Value: TPointF);
begin
  FM1.X := Value.X;
  FM1.Y := Value.Y;
  FM1.Z := 0;
  NeedCalc := True;
end;

procedure TSchnittKK.SetM2(const Value: TPointF);
begin
  FM2.X := Value.X;
  FM2.Y := Value.Y;
  FM2.Z := 0;
  NeedCalc := True;
end;

procedure TSchnittKK.SetMittelPunkt1(Value: TPoint3D);
begin
//  if not Value.EqualsTo(FM1) then
  if (Value.X <> FM1.X) or (Value.Y <> FM1.Y) or (Value.Z <> FM1.Z) then
  begin
    FM1 := Value;
    NeedCalc := True;
  end;
end;

procedure TSchnittKK.SetMittelPunkt2(Value: TPoint3D);
begin
//  if not Value.EqualsTo(FM2) then
  if (Value.X <> FM2.X) or (Value.Y <> FM2.Y) or (Value.Z <> FM2.Z) then
  begin
    FM2 := Value;
    NeedCalc := True;
  end;
end;

function TSchnittKK.GetSchnittPunkt1: TPoint3D;
begin
  if NeedCalc = True then
    Schnitt;
  result := S1;
end;

function TSchnittKK.GetSchnittPunkt2: TPoint3D;
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

procedure TSchnittKK.Schnitt;
var
  a, b, h1, h2, p, q, Entfernung: single;
  DeltaX, DeltaY: single;
  AbsDeltaX, AbsDeltaY: single;
  DeltaNullx, DeltaNully: Boolean;
  M1M2, M1S1, KreuzProd: TPoint3D;
  M1, M2, SP: TPoint3D;
begin
  NeedCalc := False;
  sv := False;
  Watch1 := 0;
  Watch2 := 0;

  S1 := TPoint3D.Zero;
  S2 := TPoint3D.Zero;

  if Ebene = seXY then
  begin
    M1.X := FM1.X;
    M1.Y := FM1.Y;
    M1.Z := 0;
    M2.X := FM2.X;
    M2.Y := FM2.Y;
    M2.Z := 0;
  end
  else if Ebene = seXZ then
  begin
    M1.X := FM1.X;
    M1.Y := FM1.Z;
    M1.Z := 0;
    M2.X := FM2.X;
    M2.Y := FM2.Z;
    M2.Z := 0;
  end
  else if Ebene = seYZ then
  begin
    M1.X := FM1.Y;
    M1.Y := FM1.Z;
    M1.Z := 0;
    M2.X := FM2.Y;
    M2.Y := FM2.Z;
    M2.Z := 0;
  end;

  { Radien sollen größer Null sein }
  if (R1 <= 0) or (R2 <= 0) then
  begin
    Bem := bmRadiusFalsch;
    Exit;
  end;

  DeltaX := M2.X - M1.X;
  DeltaY := M2.Y - M1.Y;
  DeltaNullx := DeltaX = 0;
  DeltaNully := DeltaY = 0;
  AbsDeltaX := abs(DeltaX);
  AbsDeltaY := abs(DeltaY);

  { Spezialfall konzentrische Kreise }
  if DeltaNullx and DeltaNully then
  begin
    Bem := bmKonzentrisch;
    Exit;
  end;

  h1 := (R1 * R1 - R2 * R2) + (M2.X * M2.X - M1.X * M1.X) + (M2.Y * M2.Y - M1.Y * M1.Y);

  { Rechnung im Normalfall }

  if AbsDeltaY > AbsDeltaX then
  begin
    Watch1 := 1;
    a := - DeltaX / DeltaY;
    b := h1 / (2 * DeltaY);
    p := 2 * (a * b - M1.X - a * M1.Y) / (1 + a * a);
    q := (M1.X * M1.X + b * b - 2 * b * M1.Y + M1.Y * M1.Y - R1 * R1) / (1 + a * a);
    h2 := p * p / 4 - q;
    if h2 >= 0 then
    begin
      h2 := sqrt(h2);
      S1.X := -p / 2 + h2;
      S2.X := -p / 2 - h2;
      S1.Y := a * S1.X + b;
      S2.Y := a * S2.X + b;
      sv := True;
    end;
  end
  else
  begin
    Watch1 := 2;
    a := - DeltaY / DeltaX;
    b := h1 / (2 * DeltaX);
    p := 2 * (a * b - M1.Y - a * M1.X) / (1 + a * a);
    q := (M1.Y * M1.Y + b * b - 2 * b * M1.X + M1.X * M1.X - R1 * R1) / (1 + a * a);
    h2 := p * p / 4 - q;
    if h2 >= 0 then
    begin
      h2 := sqrt(h2);
      S1.Y := -p / 2 + h2;
      S2.Y := -p / 2 - h2;
      S1.X := a * S1.Y + b;
      S2.X := a * S2.Y + b;
      sv := True;
    end;
  end;

  Entfernung := (M2 - M1).Length;

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
    Watch2 := 1;
    M1M2 := M2 - M1;
    M1S1 := S1 - M1;
    KreuzProd := M1M2.CrossProduct(M1S1);
    if KreuzProd.Z < 0 then
    begin
      Watch2 := 2;
      SP := S2;
      S2 := S1;
      S1 := SP;
    end;
  end;

  if Ebene = seXZ then
  begin
    S1.Z := S1.Y;
    S1.Y := 0;
    S2.Z := S2.Y;
    S2.Y := 0;
  end
  else if Ebene = seYZ then
  begin
    S1.X := S1.Y;
    S1.Y := S1.Z;
    S1.Z := 0;
    S2.X := S2.Y;
    S2.Y := S2.Z;
    S2.Z := 0;
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
  KnotenLast := TPoint3D.Zero;
end;

procedure TTetraF.VierteKraft;
begin
  d1.X := d1.X / l1;
  d2.X := d2.X / l2;
  d3.X := d3.X / l3;

  d1.Y := d1.Y / l1;
  d2.Y := d2.Y / l2;
  d3.Y := d3.Y / l3;

  d1.Z := d1.Z / l1;
  d2.Z := d2.Z / l2;
  d3.Z := d3.Z / l3;

  FR.X := F1 * d1.X + F2 * d2.X + F3 * d3.X;
  FR.Y := F1 * d1.Y + F2 * d2.Y + F3 * d3.Y;
  FR.Z := F1 * d1.Z + F2 * d2.Z + F3 * d3.Z;

  F4 := FR.Length;
  { > or < depending on the use of left handed system or right handed system }
  if FR.Y > 0 then
    F4 := -F4;
end;

function TTetraF.SkalarProduktPositiv: Boolean;
begin
  result := False;
  SkalarProdukt := FR.X * d4.X + FR.Y * d4.Y + FR.Z * d4.Z;
  if SkalarProdukt >= 0 then
    result := True;
end;

function TTetraF.Probe: Boolean;
begin
  result := True;

  d1.X := d1.X / l1;
  d2.X := d2.X / l2;
  d3.X := d3.X / l3;
  d4.X := d4.X / l4;

  d1.Y := d1.Y / l1;
  d2.Y := d2.Y / l2;
  d3.Y := d3.Y / l3;
  d4.Y := d4.Y / l4;

  d1.Z := d1.Z / l1;
  d2.Z := d2.Z / l2;
  d3.Z := d3.Z / l3;
  d4.Z := d4.Z / l4;

  FR.X := F1 * d1.X + F2 * d2.X + F3 * d3.X + F4 * d4.X;
  FR.Y := F1 * d1.Y + F2 * d2.Y + F3 * d3.Y + F4 * d4.Y;
  FR.Z := F1 * d1.Z + F2 * d2.Z + F3 * d3.Z + F4 * d4.Z;

  FR := FR + KnotenLast;
  ProbeErgebnis := FR.Length;
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
    S1 := TPoint3D.Zero;
    S2 := TPoint3D.Zero;
    try
      t1.Left := Round(FM1.X - R1);
      t1.Right := Round(FM1.X + R1);
      t1.Top := Round(FM1.Y - R1);
      t1.Bottom := Round(FM1.Y + R1);

      t2.Left := Round(FM2.X - R2);
      t2.Right := Round(FM2.X + R2);
      t2.Top := Round(FM2.Y - R2);
      t2.Bottom := Round(FM2.Y + R2);

      { fpc compiler warned that t does not seem to be initialized }
      t.Left := 0;
      t.Right := 0;
      t.Top := 0;
      t.Bottom := 0;

      ret := IntersectRect(t, t1, t2);
      if ret then
      begin
        S1.X := t.Left;
        S1.Y := t.Top;
        S2.X := t.Right;
        S2.Y := t.Bottom;
      end;
    except
    end;
  end
  else
    inherited Schnitt;
end;

end.
