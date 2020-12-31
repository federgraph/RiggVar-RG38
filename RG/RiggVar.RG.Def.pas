unit RiggVar.RG.Def;

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
  System.UITypes,
  System.UIConsts,
  System.Classes,
  FMX.Graphics;

type
  TSelectedCircle = (
    scC1,
    scC2
  );

  TCircleParam = (
    fpR1,
    fpR2,
    fpM1X,
    fpM1Y,
    fpM1Z,
    fpM2X,
    fpM2Y,
    fpM2Z,
    fpA1,
    fpA2,
    fpE1,
    fpE2
  );

const
  claRumpf = claSilver;
  claMast = claBlue;
  claWanten = claRed;
  claVorstag = claYellow;
  claSaling = claLime;
  claController = claAqua;
  claEntspannt = claGray;
  claNullStellung = claAqua;
  claKoppelKurve = claYellow;
  claGestrichelt = TAlphaColors.Antiquewhite;
  claFixPoint = claYellow;

implementation

end.
