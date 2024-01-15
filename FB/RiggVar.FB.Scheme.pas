unit RiggVar.FB.Scheme;

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
  System.UIConsts;

type
  TColorScheme = record
  public
    Scheme: Integer;
    SchemeDefault: Integer;
    claBackground: TAlphaColor;
    claHint: TAlphaColor;

    claToolBtnFill: TAlphaColor;
    claTouchBtnFill: TAlphaColor;
    claCornerScrollbar: TAlphaColor;
    claCornerBtnText: TAlphaColor;
    claTouchbarText: TAlphaColor;

    IsDark: Boolean;

    Dark: Integer;
    Light: Integer;

    constructor Create(cs: Integer);

    procedure BlackText;
    procedure GrayText;
    procedure WhiteText;

    procedure Init(cs: Integer);
  end;

implementation

{ TColorScheme }

procedure TColorScheme.BlackText;
begin
  claToolBtnFill := claGray;
  claTouchBtnFill := claGray;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claBlue;
  claHint := claYellow;
end;

procedure TColorScheme.GrayText;
begin
  claToolBtnFill := claGray;
  claTouchBtnFill := claGray;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claBlue;
  claHint := claYellow;
end;

procedure TColorScheme.WhiteText;
begin
  claToolBtnFill := claWhite;
  claTouchBtnFill := claWhite;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claWhite;
  claHint := claBlue;
end;

constructor TColorScheme.Create(cs: Integer);
begin
  Dark := 5;
  Light := 2;
  claTouchbarText := claBlack;
  SchemeDefault := cs;
  Scheme := SchemeDefault;
  Init(Scheme);
end;

procedure TColorScheme.Init(cs: Integer);
begin
  Scheme := cs;
  IsDark := True;
  case cs of
    1:
    begin
      IsDark := False;
      claBackground := claBisque;
      claToolBtnFill := claBlack;
      claTouchBtnFill := claSilver;
      claCornerScrollbar := claBurlywood;
      claCornerBtnText:= claDimgray;
      claHint := claCornflowerblue;
    end;
    2:
    begin
      IsDark := False;
      claBackground := claAntiquewhite;
      claToolBtnFill := claBlue;
      claTouchBtnFill := claLightblue;
      claCornerScrollbar := claLightblue;
      claCornerBtnText:= claBlue;
      claHint := claCornflowerblue;
    end;
    3:
    begin
      claBackground := claDarkslategray;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claBlue;
      claHint := claYellow;
    end;
    4:
    begin
      claBackground := StringToAlphaColor('#FF372E69');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
      claHint := claYellow;
    end;
    5:
    begin
      claBackground := StringToAlphaColor('#FF333333');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claHint := claYellow;
    end;
    6:
    begin
      claBackground := StringToAlphaColor('#FF444444');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claHint := claYellow;
    end;
    7:
    begin
      claBackground := StringToAlphaColor('#FF555555');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claHint := claYellow;
    end;
  end;
end;

end.
