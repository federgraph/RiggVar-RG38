unit RiggVar.FederModel.Menu;

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

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types,
  FMX.Menus,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action,
  RiggVar.FB.ActionLong;

type
  TFederActions = TList<Integer>;

  TFederActionGroup = class
  public
    Items: TFederActions;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Add(fa: Integer);
  end;

  TFederMenuBase = class (TList<TFederActionGroup>)
  public
    TestName: string;
    constructor Create;
    destructor Destroy; override;
    procedure AddGroup(Caption: string; fag: TFederActionGroup);

    procedure CollectOne(fa: Integer; ML: TStrings);
    procedure CollectAll(ML: TStrings);
  end;

  TFederMenu = class(TFederMenuBase)
  private
    function AddMenu(M: TMainMenu; Caption: string; fas: TFederActions): TMenuItem;
    procedure InitItem(I: TMenuItem; fa: TFederAction);
  public
    procedure InitMainMenu(M: TMainMenu);
    procedure UpdateText(M: TMainMenu);
  end;

implementation

uses
  RiggVar.App.Main;

{ TFederMenu }

procedure TFederMenu.InitMainMenu(M: TMainMenu);
var
  ag: TFederActionGroup;
begin
  for ag in self do
  begin
    AddMenu(M, ag.Name, ag.Items);
  end;
end;

procedure TFederMenu.UpdateText(M: TMainMenu);
var
  i, j: Integer;
  fmxMenu, fmxItem: TFmxObject;
  mm: TMenuItem;
  mi: TMenuItem;
begin
  for i := 0 to M.ItemsCount-1 do
  begin
    fmxMenu := M.Items[i];
    if fmxMenu is TMenuItem then
    begin
      mm := fmxMenu as TMenuItem;
      for j := 0 to mm.ItemsCount-1 do
      begin
        fmxItem := mm.Items[j];
        if fmxItem is TMenuItem then
        begin
          mi := fmxItem as TMenuItem;
//          mi.Text := Main.ActionHandler.GetCaption(mi.Tag);
          mi.Action := Main.ActionList.GetFederAction(mi.Tag, MainVar.WantLocalizedText, False);
        end;
      end;
    end;
  end;
end;

function TFederMenu.AddMenu(M: TMainMenu; Caption: string; fas: TFederActions): TMenuItem;
var
  fa: TFederAction;
begin
  result := TMenuItem.Create(M);
  result.Text := Caption;
  M.AddObject(result);
  for fa in fas do
    InitItem(result, fa);
end;

procedure TFederMenu.InitItem(I: TMenuItem; fa: TFederAction);
var
  t: TMenuItem;
begin
  t := TMenuItem.Create(I);
  t.Width := 50;
  t.Height := 50;
  t.Opacity := 1.0;
  t.Font.Size := 24;
  t.Text := GetFederActionLong(fa);
  t.Enabled := True;
  t.Visible := True;
  t.Tag := Ord(fa);
  t.Action := Main.ActionList.GetFederAction(fa, MainVar.WantLocalizedText, False);
  I.AddObject(t);
end;

{ TFederMenuBase }

procedure TFederMenuBase.AddGroup(Caption: string; fag: TFederActionGroup);
begin
  fag.Name := Caption;
  Add(fag);
end;

procedure TFederMenuBase.CollectAll(ML: TStrings);
var
  ag: TFederActionGroup;
  fa: Integer;
begin
  for ag in self do
    for fa in ag.Items do
      ML.Add(Format('%s %s: %s', [TestName, ag.Name, GetFederActionLong(fa)]));
end;

procedure TFederMenuBase.CollectOne(fa: Integer; ML: TStrings);
var
  ag: TFederActionGroup;
  i: Integer;
begin
  for ag in self do
    for i in ag.Items do
      if i = fa then
        ML.Add(Format('%s %s', [TestName, ag.Name]));
end;

constructor TFederMenuBase.Create;
var
  fag: TFederActionGroup;
begin
  inherited;

  TestName := 'MEN';

  fag := TFederActionGroup.Create;
  fag.Add(faRotaForm1);
  fag.Add(faRotaForm2);
//  fag.Add(faRotaForm3);
  fag.Add(faShowActions);
  fag.Add(faShowMemo);
  fag.Add(faShowConfig);
  fag.Add(faShowTrimmTab);
  fag.Add(faShowDrawings);
//  fag.Add(faShowOptions);
//  fag.Add(faShowKreis);
//  fag.Add(faShowInfo);
//  fag.Add(faShowSplash);
//  fag.Add(faShowForce);
//  fag.Add(faShowTabelle);
//  fag.Add(faShowDetail);
//  fag.Add(faShowSaling);
//  fag.Add(faShowController);
//  fag.Add(faShowText);
//  fag.Add(faShowChart);
//  fag.Add(faShowDiagA);
  fag.Add(faShowDiagC);
  fag.Add(faShowDiagE);
  fag.Add(faShowDiagQ);
//  if MainVar.AppTitle = '' then
//    MainVar.AppTitle := 'App';
//  AddGroup(MainVar.AppTitle, fag);
  AddGroup('Forms', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faController);
  fag.Add(faWinkel);
  fag.Add(faVorstag);
  fag.Add(faWante);
  fag.Add(faWoben);
  fag.Add(faSalingH);
  fag.Add(faSalingA);
  fag.Add(faSalingL);
  fag.Add(faSalingW);
  fag.Add(faMastfallF0C);
  fag.Add(faMastfallF0F);
  fag.Add(faMastfallVorlauf);
  fag.Add(faBiegung);
  fag.Add(faMastfussD0X);
//  fag.Add(faVorstagOS);
//  fag.Add(faWPowerOS);
//  fag.Add(faParamAPW);
//  fag.Add(faParamEAH);
//  fag.Add(faParamEAR);
//  fag.Add(faParamEI);
  AddGroup('Param', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faFixpointA0);
  fag.Add(faFixpointA);
  fag.Add(faFixpointB0);
  fag.Add(faFixpointB);
  fag.Add(faFixpointC0);
  fag.Add(faFixpointC);
  fag.Add(faFixpointD0);
  fag.Add(faFixpointD);
  fag.Add(faFixpointE0);
  fag.Add(faFixpointE);
  fag.Add(faFixpointF0);
  fag.Add(faFixpointF);
  AddGroup('Fixpoint', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faViewpointS);
  fag.Add(faViewpointA);
  fag.Add(faViewpointT);
  fag.Add(faViewpoint3);
  AddGroup('Viewpoint', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faTrimm0);
  fag.Add(faTrimm1);
  fag.Add(faTrimm2);
  fag.Add(faTrimm3);
  fag.Add(faTrimm4);
  fag.Add(faTrimm5);
  fag.Add(faTrimm6);
  fag.Add(fa420);
  fag.Add(faLogo);
  AddGroup('Trimm', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faSuperSimple);
  fag.Add(faSuperNormal);
  fag.Add(faSuperGrau);
  fag.Add(faSuperBlau);
  fag.Add(faSuperMulti);
  fag.Add(faSuperDisplay);
  fag.Add(faSuperQuick);
  AddGroup('Super', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faRggBogen);
  fag.Add(faRggKoppel);
  fag.Add(faRggHull);
  AddGroup('Graph', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faReportNone);
  fag.Add(faReportLog);
  fag.Add(faReportJson);
  fag.Add(faReportData);
  fag.Add(faReportShort);
  fag.Add(faReportLong);
  fag.Add(faReportTrimmText);
  fag.Add(faReportJsonText);
  fag.Add(faReportDataText);
  fag.Add(faReportDiffText);
  fag.Add(faReportAusgabeDetail);
  fag.Add(faReportAusgabeRL);
  fag.Add(faReportAusgabeRP);
  fag.Add(faReportAusgabeRLE);
  fag.Add(faReportAusgabeRPE);
  fag.Add(faReportAusgabeDiffL);
  fag.Add(faReportAusgabeDiffP);
  fag.Add(faReportXML);
  fag.Add(faReportDebugReport);
  fag.Add(faReportReadme);
  AddGroup('Report', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faReset);
  fag.Add(faResetPosition);
  fag.Add(faResetRotation);
  fag.Add(faResetZoom);
  AddGroup('Reset', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faMemeGotoLandscape);
  fag.Add(faMemeGotoSquare);
  fag.Add(faMemeGotoPortrait);
//  fag.Add(faMemeFormat0);
//  fag.Add(faMemeFormat1);
//  fag.Add(faMemeFormat2);
//  fag.Add(faMemeFormat3);
//  fag.Add(faMemeFormat4);
//  fag.Add(faMemeFormat5);
//  fag.Add(faMemeFormat6);
//  fag.Add(faMemeFormat7);
//  fag.Add(faMemeFormat8);
//  fag.Add(faMemeFormat9);
  AddGroup('Format', fag);

  fag := TFederActionGroup.Create;
  fag.Add(faToggleLanguage);
  fag.Add(faToggleButtonSize);
  fag.Add(faToggleDarkMode);
  AddGroup('Option', fag);
end;

destructor TFederMenuBase.Destroy;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited;
end;

{ TFederActionGroup }

constructor TFederActionGroup.Create;
begin
  inherited;
  Items := TList<Integer>.Create;
end;

destructor TFederActionGroup.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure TFederActionGroup.Add(fa: Integer);
begin
  Items.Add(fa);
end;

end.
