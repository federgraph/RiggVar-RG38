unit FrmMemo;

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

{.$define WantDriverTest}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView;

type
  TAutoUpdateReport = (
    Nothing,
    TrimmItem
  );

  TMemoAction = record
  public
    Tag: Integer;
    Caption: string;
    Handler: TNotifyEvent;
  end;

  TMemoActionList = class (TList<TMemoAction>)
  public
    procedure AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
    function FindByTag(ATag: Integer): TMemoAction;
  end;

  TFormMemo = class(TForm)
    ToolbarPanel: TPanel;
    ListView: TListView;
    Memo: TMemo;
    ReadBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReadBtnClick(Sender: TObject);
    procedure ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    SL: TStringList;
    MemoActionList: TMemoActionList;
    procedure InitItems;
    procedure InitList;
    procedure MemoBeginUpdate;
    procedure MemoEndUpdate;
  protected
    procedure WriteTrimmItem(Sender: TObject);
    procedure WriteTrimmFile(Sender: TObject);
    procedure WriteHullPoints(Sender: TObject);

    procedure InfoText(Sender: TObject);
    procedure HelpText(Sender: TObject);
    procedure HelpTextForIO(Sender: TObject);
    procedure WriteShortcuts(Sender: TObject);

    procedure DeviceReportBtnClick(Sender: TObject);

    procedure ActionTestBtnClick(Sender: TObject);
    procedure WriteNewActionConstBtnClick(Sender: TObject);
    procedure WriteActionConstBtnClick(Sender: TObject);
    procedure WriteActionNamesBtnClick(Sender: TObject);

    procedure NotImplemented;
    procedure AutoUpdateOff;
  public
    CounterNotImplemented: Integer;
    AutoUpdateID: TAutoUpdateReport;
    procedure AutoUpdate;
  end;

var
  FormMemo: TFormMemo;

implementation

uses
{$ifdef WantDriverTest}
  RiggVar.FG.DriverTest,
{$endif}
  RiggVar.App.Main;

{$R *.fmx}

procedure TFormMemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TFormMemo.FormCreate(Sender: TObject);
begin
  Caption := 'Form Memo';
  Left := 10;
  Top := 120;
  Width := 800;
  Height := 750;
  SL := TStringList.Create;

  ListView.Align := TAlignLayout.Left;
  ListView.ItemAppearanceName := 'ListItem';
  ListView.ItemAppearance.ItemHeight := 24;
  ListView.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
  ListView.ItemAppearanceObjects.ItemObjects.Text.Font.Family := 'Consolas';
  ListView.ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  ListView.ItemAppearanceObjects.ItemObjects.Text.TextColor := TAlphaColors.Dodgerblue;
  ListView.ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  ListView.ItemAppearanceObjects.FooterObjects.Text.Visible := False;

  Memo.Align := TAlignLayout.Client;
  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 14;
  Memo.TextSettings.FontColor := claBlack;

  MemoActionList := TMemoActionList.Create;
  InitList;
  InitItems;
end;

procedure TFormMemo.FormDestroy(Sender: TObject);
begin
  SL.Free;
  MemoActionList.Free;
end;

procedure TFormMemo.ReadBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  SL.Text := Memo.Lines.Text;
  Main.ReadText(SL);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteTrimmItem(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.WriteTrimmItem;
  Memo.Lines.Text := Main.FLText;
  MemoEndUpdate;
  AutoUpdateID := TrimmItem;
end;

procedure TFormMemo.WriteTrimmFile(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.WriteTrimmFile;
  Memo.Lines.Text := Main.FLText;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteHullPoints(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.FederBinding.InitHullPoints(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.InfoText(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.FederBinding.InitInfoText(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.HelpText(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.FederBinding.InitHelpText(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.HelpTextForIO(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.FederBinding.InitHelpTextForIO(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.DeviceReportBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
{$ifdef WantDriverTest}
  { DeviceCheck instance should already have been created in dpr file. }
  { Viewport instance should have been injected into DeviceCheck in FormMain }
  if not Assigned(DeviceCheck) then
    DeviceCheck := TDeviceCheck.Create;
  DeviceCheck.GetDeviceReport(Memo.Lines);
{$else}
  Memo.Lines.Add('DeviceReport not implemented');
{$endif}
  MemoEndUpdate;
end;

procedure TFormMemo.ActionTestBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.TestAll;
  Memo.Lines.Text := Main.ActionTest.SL.Text;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteNewActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteNewActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionNamesBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionNames(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteShortcuts(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionHelper.GetShortcutReport(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.InitList;
begin
  MemoActionList.AddMemoAction('Show Trimm Item', WriteTrimmItem);
  MemoActionList.AddMemoAction('Show Trimm File', WriteTrimmFile);
  MemoActionList.AddMemoAction('Show Hull Points', WriteHullPoints);
  MemoActionList.AddMemoAction('Info Text', InfoText);
  MemoActionList.AddMemoAction('Manual Shortcuts', HelpText);

{$ifdef MSWINDOWS}
//  MemoActionList.AddMemoAction('Help Text for IO', HelpTextForIO);
  MemoActionList.AddMemoAction('Auto Shortcuts', WriteShortcuts);
{$ifdef WantDriverTest}
  MemoActionList.AddMemoAction('Device Report', DeviceReportBtnClick);
{$endif}

  MemoActionList.AddMemoAction('Action Test', ActionTestBtnClick);
  MemoActionList.AddMemoAction('Write Action Const', WriteActionConstBtnClick);
  MemoActionList.AddMemoAction('Write New Action Const', WriteNewActionConstBtnClick);
  MemoActionList.AddMemoAction('Write Action Names', WriteActionNamesBtnClick);
{$endif}
end;

procedure TFormMemo.InitItems;
var
  cr: TMemoAction;
  lvi: TListViewItem;
begin
  ListView.Items.Clear;
  for cr in MemoActionList do
  begin
    lvi := ListView.Items.Add;
    lvi.Text := cr.Caption;
    lvi.Tag := cr.Tag;
    lvi.Height := 24;
  end;
end;

procedure TFormMemo.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  cr: TMemoAction;
begin
  cr := MemoActionList.FindByTag(AItem.Tag);
  if cr.Tag > -1 then
    cr.Handler(nil);
end;

{ TMemoActionList }

procedure TMemoActionList.AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
var
  ma: TMemoAction;
begin
  ma.Caption := ACaption;
  ma.Handler := AHandler;
  ma.Tag := Self.Count;
  Self.Add(ma);
end;

function TMemoActionList.FindByTag(ATag: Integer): TMemoAction;
var
  cr: TMemoAction;
begin
  result.Caption := 'not found';
  for cr in Self do
    if cr.Tag = ATag then
    begin
      result := cr;
      break;
    end;
end;

procedure TFormMemo.MemoBeginUpdate;
begin
  AutoUpdateID := Nothing;
  Memo.Lines.BeginUpdate;
  Memo.Lines.Clear;
end;

procedure TFormMemo.MemoEndUpdate;
begin
  Memo.Lines.EndUpdate;
{$ifdef MSWINDOWS}
  Memo.ContentBounds := TRectF.Empty;
{$endif}
end;

procedure TFormMemo.NotImplemented;
begin
  Memo.Lines.Clear;
  Inc(CounterNotImplemented);
  Memo.Lines.Add(Format('Not implemented (%d) [%s]', [CounterNotImplemented, DateTimeToStr(Now)]));
  AutoUpdateID := Nothing;
end;

procedure TFormMemo.AutoUpdateOff;
begin
  AutoUpdateID := Nothing;
end;

procedure TFormMemo.AutoUpdate;
begin
  case AutoUpdateID of
    TrimmItem: WriteTrimmItem(nil);
  end;
end;

end.
