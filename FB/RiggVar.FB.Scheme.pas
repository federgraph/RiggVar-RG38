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
    WantBlackText: Boolean;

    Scheme: Integer;
    SchemeDefault: Integer;
    claBackground: TAlphaColor;

    claLabelText: TAlphaColor;
    claSampleText: TAlphaColor;
    claOptionText: TAlphaColor;
    claToolBtnFill: TAlphaColor;
    claTouchBtnFill: TAlphaColor;
    claCornerScrollbar: TAlphaColor;
    claCornerBtnText: TAlphaColor;
    claEquationFill: TAlphaColor;
    claEquationText: TAlphaColor;
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
  claLabelText := claBlack;
  claSampleText := claBlack;
  claToolBtnFill := claGray;
  claTouchBtnFill := claGray;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claBlue;
  claEquationFill := claNull;
  claEquationText := claBlack;

  claOptionText := claSampleText;
end;

procedure TColorScheme.GrayText;
begin
  claLabelText := claGray;
  claSampleText := claGray;
  claToolBtnFill := claGray;
  claTouchBtnFill := claGray;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claBlue;
  claEquationFill := claNull;
  claEquationText := claBlack;

  claOptionText := claSampleText;
end;

procedure TColorScheme.WhiteText;
begin
  claLabelText := claWhite;
  claSampleText := claWhite;
  claToolBtnFill := claWhite;
  claTouchBtnFill := claWhite;
  claCornerScrollbar := claGray;
  claCornerBtnText:= claWhite;
  claEquationFill := claNull;
  claEquationText := claWhite;

  claOptionText := claSampleText;
end;

constructor TColorScheme.Create(cs: Integer);
begin
  Dark := 5;
  Light := 2;
  WantBlackText := True;
  claTouchbarText := claBlack;
  SchemeDefault := cs;
  Scheme := SchemeDefault;
  Init(Scheme);
end;

procedure TColorScheme.Init(cs: Integer);
begin
  Scheme := cs;
  case cs of
    1:
    begin
      IsDark := True;
      if WantBlackText then
      begin
        claBackground := claSlateblue;
        claLabelText := claBlack;
        claSampleText := claBlack;
        claToolBtnFill := claGray;
        claTouchBtnFill := claGray;
        claCornerScrollbar := claGray;
        claCornerBtnText:= claBlue;
        claEquationFill := claNull;
        claEquationText := claBlack;
      end
      else
      begin
        claBackground := claLavender;
        claLabelText := claGray;
        claSampleText := claGray;
        claToolBtnFill := claGray;
        claTouchBtnFill := claGray;
        claCornerScrollbar := claGray;
        claCornerBtnText:= claBlue;
        claEquationFill := claNull;
        claEquationText := claBlack;
      end;
    end;
    2:
    begin
      IsDark := False;
      claBackground := StringToAlphaColor('#FFF9F9F9');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claLavender;
      claCornerBtnText:= claBlue;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
    3:
    begin
      IsDark := True;
      claBackground := claCornflowerblue;
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
    4:
    begin
      IsDark := True;
      claBackground := StringToAlphaColor('#FF372E69');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    5:
    begin
      IsDark := True;
      claBackground := StringToAlphaColor('#FF333333');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    6:
    begin
      IsDark := True;
      claBackground := claBlack;
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    7:
    begin
      IsDark := True;
      claBackground := claPurple; //claNull;
      claLabelText := claBlack;
      claSampleText := claBlack;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
  end;
  claOptionText := claSampleText;
end;

end.

