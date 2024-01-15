unit RiggVar.FD.Image;

interface

uses
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  FMX.Forms,
  FMX.Controls,
  FMX.Graphics;

type
  TScaledSize = record
    cx: FixedInt;
    cy: FixedInt;
    ss: single;
  public
    constructor Create(const X, Y : Integer; AScale: single); overload;

    class operator Equal(const Lhs, Rhs : TScaledSize) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TScaledSize): Boolean;

    property Width: Integer read cx write cx;
    property Height: Integer read cy write cy;
    property Scale: single read ss write ss;
  end;

  TBitmapDict = TDictionary<TScaledSize, TBitmap>;

  TBitmapCollection = class
  private
    FBitmaps: TBitmapDict;
  public
    constructor Create;
    destructor Destroy; override;
    function ItemBySize(AScaledSize: TScaledSize): TBitmap;
    function Add(AScaledSize: TScaledSize): TBitmap;
  end;

  { This control does not even have a WrapMode property. }
  { But it behaves similar to a TImage with WrapMode set to Original }
  TOriginalImage = class(TControl)
  private
    FWantWorkArea: Boolean;
    FNominalSize: TSize;
    FCurrentSize: TScaledSize;
    FCurrentScreenScale: single;
    FCurrentBitmap: TBitmap;
    FOnSizeChanged: TNotifyEvent;
    FHasChanged: Boolean;
    FBitmapCollection: TBitmapCollection;
    function GetBitmap: TBitmap;
    function ItemForCurrentWorkArea: TBitmap;
    function ItemForCurrentScale: TBitmap;
    procedure SetOnSizeChanged(const Value: TNotifyEvent);
    procedure NotifyChanged;
  protected
    procedure Paint; override;
  public
    IR: TRectF;
    R1: TRectF;
    R2: TRectF;
    { Use reintroduce to hide an inherited virtual method with a new one. }
    constructor Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer); reintroduce;
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
    property Scale;
    property ScreenScale: single read FCurrentScreenScale;
    property OnSizeChanged: TNotifyEvent read FOnSizeChanged write SetOnSizeChanged;
    property WantWorkArea: Boolean read FWantWorkArea write FWantWorkArea;
  end;

implementation

{ TOriginalImage }

constructor TOriginalImage.Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer);
begin
  inherited Create(AOwner);
  FNominalSize := TSize.Create(AWidth, AHeight);
  FBitmapCollection := TBitmapCollection.Create;
  SetAcceptsControls(False);
end;

destructor TOriginalImage.Destroy;
begin
  FBitmapCollection.Free;
  inherited;
end;

function TOriginalImage.ItemForCurrentWorkArea: TBitmap;
var
  t: single;
  NewSize: TScaledSize;
begin
  if Scene <> nil then
    t := Scene.GetSceneScale
  else
    t := 1.0;
  if t < 1 then
    t := 1.0;

  if t <> FCurrentScreenScale then
  begin
    FCurrentScreenScale := t;
    FHasChanged := True;
  end;

  NewSize.cx := Ceil(Screen.WorkAreaWidth);
  NewSize.cy := Ceil(Screen.WorkAreaHeight);
  NewSize.ss := FCurrentScreenScale;

  result := FBitmapCollection.ItemBySize(NewSize);
  if result = nil then
    result := FBitmapCollection.Add(NewSize);

  if FCurrentSize <> NewSize then
  begin
    FHasChanged := True;
    FCurrentSize := NewSize;
  end;
end;

function TOriginalImage.ItemForCurrentScale: TBitmap;
var
  t: single;
  NewSize: TScaledSize;
begin
  if Scene <> nil then
    t := Scene.GetSceneScale
  else
    t := 1.0;
  if t < 1 then
    t := 1.0;

  if t <> FCurrentScreenScale then
  begin
    FCurrentScreenScale := t;
    FHasChanged := True;
  end;

  NewSize.cx := FNominalSize.Width;
  NewSize.cy := FNominalSize.Height;
  NewSize.ss := FCurrentScreenScale;

  result := FBitmapCollection.ItemBySize(NewSize);
  if result = nil then
    result := FBitmapCollection.Add(NewSize);
end;

function TOriginalImage.GetBitmap: TBitmap;
begin
  if WantWorkArea then
    result := ItemForCurrentWorkArea
  else
    result := ItemForCurrentScale
end;

procedure TOriginalImage.Paint;
var
  LR: TRectF;
  ScalingValue: single;
begin
  FCurrentBitmap := GetBitmap;
  ScalingValue := FCurrentScreenScale;
  if FCurrentBitmap <> nil then
  begin
    R1 := LocalRect;
    R2 := TRectF.Create(
      R1.Left,
      R1.Top,
      R1.Left + FCurrentBitmap.Width,
      R1.Top + FCurrentBitmap.Height);

    LR := TRectF.Create(
      R1.Left * ScalingValue,
      R1.Top * ScalingValue,
      R1.Right * ScalingValue,
      R1.Bottom * ScalingValue);

    IntersectRect(IR, LR, R2);

    R1 := TRectF.Create(0, 0, IR.Width, IR.Height);
    R2 := TRectF.Create(
      R2.Left,
      R2.Top,
      R2.Left + IR.Width / ScalingValue,
      R2.Top + IR.Height / ScalingValue);

    Canvas.DrawBitmap(FCurrentBitmap, R1, R2, 1.0, True);
  end;

  if FHasChanged then
  begin
    FHasChanged := False;
    NotifyChanged;
  end;
end;

procedure TOriginalImage.NotifyChanged;
begin
  if Assigned(FOnSizeChanged) then
    FOnSizeChanged(Self);
end;

procedure TOriginalImage.SetOnSizeChanged(const Value: TNotifyEvent);
begin
  FOnSizeChanged := Value;
end;

{ TBitmapCollection }

constructor TBitmapCollection.Create;
begin
  FBitmaps := TBitmapDict.Create(2);
end;

destructor TBitmapCollection.Destroy;
var
  cr: TBitmap;
begin
  for cr in FBitmaps.Values do
    cr.Free;
  FBitmaps.Free;
  inherited;
end;

function TBitmapCollection.ItemBySize(AScaledSize: TScaledSize): TBitmap;
begin
  FBitmaps.TryGetValue(AScaledSize, result);
end;

function TBitmapCollection.Add(AScaledSize: TScaledSize): TBitmap;
var
  w, h: Integer;
begin
  if not FBitmaps.ContainsKey(AScaledSize) then
  begin
    w := Round(AScaledSize.Width * AScaledSize.Scale);
    h := Round(AScaledSize.Height * AScaledSize.Scale);
    result := TBitmap.Create(w, h);
    FBitmaps.Add(AScaledSize, result);
  end
  else
  begin
    FBitmaps.TryGetValue(AScaledSize, result);
  end;
  Assert(result <> nil);
end;

{ TScaledSize }

constructor TScaledSize.Create(const X, Y: Integer; AScale: single);
begin
  cx := X;
  cy := Y;
  ss := AScale;
end;

class operator TScaledSize.Equal(const Lhs, Rhs: TScaledSize): Boolean;
begin
  Result := (Lhs.cx = Rhs.cx) and (Lhs.cy = Rhs.cy) and (Lhs.ss = Rhs.ss);
end;

class operator TScaledSize.NotEqual(const Lhs, Rhs: TScaledSize): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

end.
