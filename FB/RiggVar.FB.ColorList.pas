unit RiggVar.FB.ColorList;

interface

{ this is much leaner than the color list in
    https://github.com/jackdp/JPLib/blob/master/Graphics/JPL.Colors.List.pas }

uses
  RiggVar.FB.Color,
  RiggVar.FB.ColorGroup;

type
  TRggColorListItem = record
    Color: TRggColor;
    ColorName: string;
  end;

  TRggColorListArray = array of TRggColorListItem;

  TRggColorList = class
  private
    FArray: TRggColorListArray;

    function GetCount: Integer;

    function GetItem(Index: Integer): TRggColorListItem;
    procedure SetItem(Index: Integer; const Value: TRggColorListItem);

    function GetColor(Index: Integer): TRggColor;
    procedure SetColor(Index: Integer; const Value: TRggColor);

    function GetName(Index: Integer): string;
    procedure SetName(Index: Integer; const Value: string);

    function AddColor(const Item: TRggColorListItem): Integer; overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function AddColor(const Color: TRggColor; const ColorName: string): Integer; overload;

    procedure AddColorsFromArray(const Arr: TRggColorListArray);

    procedure AddColorsFromColorArray(const Arr: array of TRggColor);
    procedure AddColorsFromGroup(g: TRggColorGroup);
    procedure AddColorsFromAllGroups;

    property ColorArray: TRggColorListArray read FArray write FArray;
    property Count: Integer read GetCount;

    property Items[Index: Integer]: TRggColorListItem read GetItem write SetItem; default;
    property Colors[Index: Integer]: TRggColor read GetColor write SetColor;
    property Names[Index: Integer]: string read GetName write SetName;
  end;

implementation

{ TRggColorList }

constructor TRggColorList.Create;
begin
  SetLength(FArray, 0);
end;

destructor TRggColorList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TRggColorList.Clear;
var
  i: Integer;
begin
  for i := 0 to High(FArray) do
    FArray[i].ColorName := '';
  SetLength(FArray, 0);
end;

function TRggColorList.AddColor(const Color: TRggColor; const ColorName: string): Integer;
begin
  SetLength(FArray, Length(FArray) + 1);
  FArray[High(FArray)].Color := Color;
  FArray[High(FArray)].ColorName := ColorName;
  Result := High(FArray);
end;

function TRggColorList.AddColor(const Item: TRggColorListItem): Integer;
begin
  SetLength(FArray, Length(FArray) + 1);
  FArray[High(FArray)] := Item;
  Result := High(FArray);
end;

procedure TRggColorList.AddColorsFromArray(const Arr: TRggColorListArray);
var
  i: Integer;
  Item: TRggColorListItem;
begin
  for i := 0 to High(Arr) do
  begin
    Item := Arr[i];
    AddColor(Item);
  end;
end;

function TRggColorList.GetCount: Integer;
begin
  Result := Length(FArray);
end;

function TRggColorList.GetItem(Index: Integer): TRggColorListItem;
begin
  Result := FArray[Index];
end;

function TRggColorList.GetColor(Index: Integer): TRggColor;
begin
  Result := FArray[Index].Color;
end;

procedure TRggColorList.SetColor(Index: Integer; const Value: TRggColor);
begin
  FArray[Index].Color := Value;
end;

procedure TRggColorList.SetItem(Index: Integer; const Value: TRggColorListItem);
begin
  FArray[Index] := Value;
end;

function TRggColorList.GetName(Index: Integer): string;
begin
  Result := FArray[Index].ColorName;
end;

procedure TRggColorList.SetName(Index: Integer; const Value: string);
begin
  FArray[Index].ColorName := Value;
end;

procedure TRggColorList.AddColorsFromColorArray(const Arr: array of TRggColor);
var
  c: TRggColor;
begin
  for c in Arr do
    AddColor(c, TRggColorPool.ColorToString(c));
end;

procedure TRggColorList.AddColorsFromGroup(g: TRggColorGroup);
begin
  case g of
    CombinedGroup: AddColorsFromAllGroups;
    PinkGroup: AddColorsFromColorArray(PinkWebColorArray);
    PurpleGroup: AddColorsFromColorArray(PurpleWebColorArray);
    RedGroup: AddColorsFromColorArray(RedWebColorArray);
    GreenGroup: AddColorsFromColorArray(GreenWebColorArray);
    BlueGroup: AddColorsFromColorArray(BlueWebColorArray);
    OrangeGroup: AddColorsFromColorArray(OrangeWebColorArray);
    YellowGroup: AddColorsFromColorArray(YellowWebColorArray);
    BrownGroup: AddColorsFromColorArray(BrownWebColorArray);
    WhiteGroup: AddColorsFromColorArray(WhiteWebColorArray);
    GrayGroup: AddColorsFromColorArray(GrayWebColorArray);
    CyanGroup: AddColorsFromColorArray(CyanWebColorArray);
    CustomGroup: AddColorsFromColorArray(CustomColorArray);
  end;
end;

procedure TRggColorList.AddColorsFromAllGroups;
var
  g: TRggColorGroup;
begin
  for g := PinkGroup to High(TRggColorGroup) do
  begin
    AddColorsFromGroup(g);
  end;
end;

end.
