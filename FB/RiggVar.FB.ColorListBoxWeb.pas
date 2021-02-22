unit RiggVar.FB.ColorListBoxWeb;

interface

{ This is a very slim FMX 'runtime' color list box component.
  It can show colors from a group of web colors only, if you want. }

uses
  RiggVar.FB.Color,
  RiggVar.FB.ColorGroup,
  RiggVar.FB.ColorList,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Objects,
  FMX.ListBox;

type
  TRggWebColorListBox = class(TCustomListBox)
  private
    FColorList: TRggColorList;
    FColorGroup: TRggColorGroup;
    function GetColor: TRggColor;
    procedure DoItemApplyStyleLookup(Sender: TObject);
    procedure SetColorGroup(const Value: TRggColorGroup);
    procedure SetColor(const Value: TRggColor);
  protected
    procedure RebuildList;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorGroup: TRggColorGroup read FColorGroup write SetColorGroup;
    property Color: TRggColor read GetColor write SetColor;
    property OnChange;
  end;

implementation

{ TRggWebColorListBox }

constructor TRggWebColorListBox.Create(AOwner: TComponent);
begin
  inherited;
  FColorGroup := TRggColorGroup.PinkGroup;
  FColorList := TRggColorList.Create;
  FColorList.AddColorsFromGroup(FColorGroup);
  RebuildList;
  SetAcceptsControls(False);
end;

destructor TRggWebColorListBox.Destroy;
begin
  FColorList.Free;
  inherited;
end;

procedure TRggWebColorListBox.RebuildList;
var
  I, SaveIndex: Integer;
  Item: TListBoxItem;
begin
  if (FUpdating > 0) or (csDestroying in ComponentState) then
    Exit;

  BeginUpdate;
  SaveIndex := ItemIndex;
  Clear;
  for I := 0 to FColorList.Count - 1 do
  begin
    Item := TListBoxItem.Create(nil);
    Item.Parent := Self;
    Item.Width := Item.DefaultSize.Width;
    Item.Height := Item.DefaultSize.Height;
    Item.Stored := False;
    Item.Locked := True;
    Item.Text := FColorList[I].ColorName;
    Item.Tag := I;
    Item.StyleLookup := 'colorlistboxitemstyle';
    Item.OnApplyStyleLookup := DoItemApplyStyleLookup;
  end;
  SelectionController.SetCurrent(SaveIndex);
  EndUpdate;
end;

procedure TRggWebColorListBox.SetColor(const Value: TRggColor);
var
  I: Integer;
begin
  if Value = TAlphaColorRec.Null then
    ItemIndex := -1
  else
    for I := 0 to Items.Count-1 do
      if Value = FColorList[I].Color then
      begin
        ItemIndex := I;
        Break;
      end;
end;

procedure TRggWebColorListBox.SetColorGroup(const Value: TRggColorGroup);
begin
  FColorGroup := Value;
  FColorList.Clear;
    FColorList.AddColorsFromGroup(FColorGroup);
  RebuildList;
end;

function TRggWebColorListBox.GetColor: TRggColor;
begin
  if (ItemIndex >= 0) and (ItemIndex < Count) then
    Result := FColorList[ItemIndex].Color
  else
    Result := TAlphaColorRec.Null;
end;

procedure TRggWebColorListBox.DoItemApplyStyleLookup(Sender: TObject);
var
  ColorObj: TShape;
begin
  if TListBoxItem(Sender).FindStyleResource<TShape>('color', ColorObj) then
    ColorObj.Fill.Color := FColorList[TListBoxItem(Sender).Tag].Color;
end;

function TRggWebColorListBox.GetDefaultStyleLookupName: string;
begin
  Result := 'listboxstyle';
end;

end.
