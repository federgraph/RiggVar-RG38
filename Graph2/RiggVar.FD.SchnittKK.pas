unit RiggVar.FD.SchnittKK;

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
  System.Math,
  System.Math.Vectors,
  RiggVar.RG.Calc,
  RiggVar.FD.Elements;

type
  TRggSchnittKK = class(TSchnittKK)
  private
  public
    function AngleM(P1, P2: TPoint3D): single;
    function Angle(P1, P2: TPoint3D): single;
    function OuterAngle(P1, P2: TPoint3D): single;

    function AnglePoint(P: TRggPoint3D; R: single; AngleInRad: single): TPoint3D; overload;
    function AnglePoint(P: TPoint3D; R: single; AngleInRad: single): TPoint3D; overload;

    function Intersection1(AM1, AM2: TRggPoint3D; AR1, AR2: single): TPoint3D; overload;
    function Intersection2(AM1, AM2: TRggPoint3D; AR1, AR2: single): TPoint3D; overload;

    function Intersection1(AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D; overload;
    function Intersection2(AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D; overload;

    function Intersection(ASelector: Integer; AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D; overload;
  end;

implementation

{ TRggSchnittKK }

function TRggSchnittKK.AnglePoint(P: TRggPoint3D; R, AngleInRad: single): TPoint3D;
begin
  result := AnglePoint(P.C, R, AngleInRad);
end;

function TRggSchnittKK.AnglePoint(P: TPoint3D; R, AngleInRad: single): TPoint3D;
begin
  result.X := P.X + R * cos(AngleInRad);
  result.Y := P.Y - R * sin(AngleInRad);
  result.Z := 0;
end;

function TRggSchnittKK.Intersection(ASelector: Integer; AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D;
begin
  SchnittEbene := seXY;
  Radius1 := AR1;
  Radius2 := AR2;
  MittelPunkt1 := AM1;
  MittelPunkt2 := AM2;
  if ASelector = 2 then
    result := SchnittPunkt2
  else
    result := SchnittPunkt1
end;

function TRggSchnittKK.Intersection1(AM1, AM2: TRggPoint3D; AR1, AR2: single): TPoint3D;
begin
  result := Intersection1(AM1.C, AM2.C, AR1, AR2);
end;

function TRggSchnittKK.Intersection2(AM1, AM2: TRggPoint3D; AR1, AR2: single): TPoint3D;
begin
  result := Intersection2(AM1.C, AM2.C, AR1, AR2);
end;

function TRggSchnittKK.Intersection1(AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D;
begin
  result := Intersection(1, AM1, AM2, AR1, AR2);
end;

function TRggSchnittKK.Intersection2(AM1, AM2: TPoint3D; AR1, AR2: single): TPoint3D;
begin
  result := Intersection(2, AM1, AM2, AR1, AR2);
end;

function TRggSchnittKK.AngleM(P1, P2: TPoint3D): single;
begin
  result := arctan2(P1.Y - P2.Z, P2.X - P1.Y);
end;

function TRggSchnittKK.Angle(P1, P2: TPoint3D): single;
begin
  result := arctan2(P1.Y - P2.Y, P1.X - P2.Y);
end;

function TRggSchnittKK.OuterAngle(P1, P2: TPoint3D): single;
begin
  result := arctan2((P1.X - P2.X), (P1.Y - P2.Y));
  result := Pi / 2 + result;
end;

end.
