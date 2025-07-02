unit RiggVar.FB.ColorListBox;

interface

{ This is a 'runtime' color-list-box component for TRggColors. }

uses
  RiggVar.FB.Color,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Rtti,
  System.UITypes,
  FMX.Objects,
  FMX.Controls,
  FMX.ListBox;

type
  TRggColorListBox = class(TCustomListBox)
  private
    FColorPool: TRggColorPoolBase;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure DoItemApplyStyleLookup(Sender: TObject);
    procedure SetColorPool(const Value: TRggColorPoolBase);
  protected
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure RebuildList;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorPool: TRggColorPoolBase read FColorPool write SetColorPool;
  published
    property Color: TAlphaColor read GetColor write SetColor default TAlphaColorRec.Null;
    property OnChange;
  end;

implementation

{ TColorListBox }

constructor TRggColorListBox.Create(AOwner: TComponent);
begin
  inherited;
  FColorPool := ColorVar.RggColorPool;
  RebuildList;
  SetAcceptsControls(False);
end;

destructor TRggColorListBox.Destroy;
begin
  inherited;
end;

procedure TRggColorListBox.RebuildList;
var
  I, SaveIndex: Integer;
  Item: TListBoxItem;
begin
  if (FUpdating > 0) or (csDestroying in ComponentState) then
    Exit;

  BeginUpdate;
  SaveIndex := ItemIndex;
  Clear;
  for I := 0 to FColorPool.Count - 1 do
  begin
    Item := TListBoxItem.Create(nil);
    Item.Parent := Self;
    Item.Width := Item.DefaultSize.Width;
    Item.Height := Item.DefaultSize.Height;
    Item.Stored := False;
    Item.Locked := True;
    Item.Text := FColorPool.ColorMap[I].Name;
    Item.Tag := I;
    Item.StyleLookup := 'colorlistboxitemstyle';
    Item.OnApplyStyleLookup := DoItemApplyStyleLookup;
  end;
  SelectionController.SetCurrent(SaveIndex);
  EndUpdate;
end;

procedure TRggColorListBox.SetColor(const Value: TAlphaColor);
var
  I: Integer;
begin
  if Value = TAlphaColorRec.Null then
    ItemIndex := -1
  else
    for I := 0 to FColorPool.Count - 1 do
      if FColorPool.ColorMap[I].Value = Value then
      begin
        ItemIndex := I;
        Break;
      end;
end;

procedure TRggColorListBox.SetColorPool(const Value: TRggColorPoolBase);
begin
  FColorPool := Value;
  RebuildList;
end;

function TRggColorListBox.GetColor: TAlphaColor;
begin
  if (ItemIndex >= 0) and (ItemIndex < Count) then
    Result := FColorPool.ColorMap[ItemIndex].Value
  else
    Result := TAlphaColorRec.Null;
end;

procedure TRggColorListBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsType<TAlphaColor> then
    Color := Value.AsType<TAlphaColor>
  else
    Color := FColorPool.ColorFromName(Value.ToString);
end;

procedure TRggColorListBox.DoItemApplyStyleLookup(Sender: TObject);
var
  ColorObj: TShape;
begin
  if TListBoxItem(Sender).FindStyleResource<TShape>('color', ColorObj) then
    ColorObj.Fill.Color := FColorPool.ColorMap[TListBoxItem(Sender).Tag].Value;
end;

function TRggColorListBox.GetData: TValue;
begin
  Result := TValue.From<TAlphaColor>(Color);
end;

function TRggColorListBox.GetDefaultStyleLookupName: string;
begin
  Result := 'listboxstyle';
end;

end.
