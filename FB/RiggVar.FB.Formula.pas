unit RiggVar.FB.Formula;

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

type
  TFederFormulaBase = class
  private
    FModelID: Integer;
  protected
    FSpringCount: Integer;
  public
    rmin: single;
    rmax: single;
    constructor Create(AModelID: Integer = 1); virtual;
    function GetValue(x, y: single): single; virtual;
    procedure PrepareCalc; virtual;
    property SpringCount: Integer read FSpringCount write FSpringCount;
    property ModelID: Integer read FModelID;
  end;

implementation

constructor TFederFormulaBase.Create(AModelID: Integer = 1);
begin
  FModelID := AModelID;
  FSpringCount := 3;
end;

procedure TFederFormulaBase.PrepareCalc;
begin
  // virtual, do nothing here
end;

function TFederFormulaBase.GetValue(x, y: single): single;
begin
  result := sqr(x) * sqr(y);
end;

end.
