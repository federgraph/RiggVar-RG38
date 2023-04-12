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
    JokerColor: TAlphaColor;
    WantBlackText: Boolean;

    Scheme: Integer;
    SchemeDefault: Integer;
    claBackground: TAlphaColor;

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
end;

procedure TColorScheme.GrayText;
begin
  claToolBtnFill := claGray;
  claTouchBtnFill := claGray;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claBlue;
end;

procedure TColorScheme.WhiteText;
begin
  claToolBtnFill := claWhite;
  claTouchBtnFill := claWhite;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claWhite;
end;

constructor TColorScheme.Create(cs: Integer);
begin
  Dark := 5;
  Light := 2;
  WantBlackText := True;
  JokerColor := claCornflowerblue;
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
      if WantBlackText then
      begin
        claBackground := claSlateblue;
        claToolBtnFill := claGray;
        claTouchBtnFill := claGray;
        claCornerScrollbar := claGray;
        claCornerBtnText:= claBlue;
      end
      else
      begin
        claBackground := claLavender;
        claToolBtnFill := claGray;
        claTouchBtnFill := claGray;
        claCornerScrollbar := claGray;
        claCornerBtnText:= claBlue;
      end;
    end;
    2:
    begin
      IsDark := False;
      claBackground := StringToAlphaColor('#FFF9F9F9');
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claLavender;
      claCornerBtnText:= claBlue;
    end;
    3:
    begin
      claBackground := JokerColor;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
    end;
    4:
    begin
      claBackground := StringToAlphaColor('#FF372E69');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
    end;
    5:
    begin
      claBackground := StringToAlphaColor('#FF333333');
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
    6:
    begin
      claBackground := claBlack;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
    7:
    begin
      claBackground := claPurple; //claNull;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
  end;
end;

end.
