unit RiggVar.FD.Image;

interface

uses
  System.Types,
  System.Classes,
  System.Generics.Collections,
  FMX.Controls,
  FMX.Graphics;

type
  TBitmapDict = TDictionary<single, TBitmap>;

  TBitmapCollection = class
  private
    FNominalSize: TSize;
    FBitmaps: TBitmapDict;
  public
    constructor Create(ANominalSize: TSize);
    destructor Destroy; override;
    function ItemByScale(AScale: single): TBitmap;
    function Add(AScale: single): TBitmap;
  end;

  TOriginalImage = class(TControl)
  private
    FCurrentBitmap: TBitmap;
    FNominalSize: TSize;
    BitmapCollection: TBitmapCollection;
    FSS: Single;
    FOnScreenScaleChaned: TNotifyEvent;
    ScreenScaleHasChanged: Boolean;
    function GetBitmap: TBitmap;
    function ItemForCurrentScale: TBitmap;
    procedure SetOnScreenScaleChaned(const Value: TNotifyEvent);
    procedure NotifyScreenScaleChanged;
  protected
    procedure Paint; override;
  public
    IR: TRectF;
    R1: TRectF;
    R2: TRectF;
    { Use reintroduce when you want to hide an inherited virtual method with a new one. }
    constructor Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer); reintroduce;
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
  public
    property Scale;
    property ScreenScale: single read FSS;
    property OnScreenScaleChaned: TNotifyEvent read FOnScreenScaleChaned write SetOnScreenScaleChaned;
  end;

implementation

{ TOriginalImage }

constructor TOriginalImage.Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer);
begin
  inherited Create(AOwner);
  FNominalSize := TSize.Create(AWidth, AHeight);
  BitmapCollection := TBitmapCollection.Create(FNominalSize);
  SetAcceptsControls(False);
end;

destructor TOriginalImage.Destroy;
begin
  BitmapCollection.Free;
  inherited;
end;

function TOriginalImage.ItemForCurrentScale: TBitmap;
var
  t: single;
begin
  if Scene <> nil then
    t := Scene.GetSceneScale
  else
    t := 1.0;
  if t < 1 then
    t := 1.0;

  if t <> FSS then
  begin
    FSS := t;
    ScreenScaleHasChanged := True
  end;

  result := BitmapCollection.ItemByScale(FSS);
  if result = nil then
    result := BitmapCollection.Add(FSS);
end;

function TOriginalImage.GetBitmap: TBitmap;
begin
  result := ItemForCurrentScale;
end;

procedure TOriginalImage.Paint;
var
  LR: TRectF;
begin
  FCurrentBitmap := GetBitmap;
  if FCurrentBitmap <> nil then
  begin
    R1 := LocalRect;
//    R2 := FCurrentBitmap.BoundsF;
    R2 := TRectF.Create(R1.Left, R1.Top, R1.Left + FCurrentBitmap.Width, R1.Top + FCurrentBitmap.Height);

    LR := TRectF.Create(R1.Left * FSS, R1.Top * FSS, R1.Right * FSS, R1.Bottom * FSS);
    IntersectRect(IR, LR, R2);

    R1 := TRectF.Create(0, 0, IR.Width, IR.Height);
    R2 := TRectF.Create(R2.Left, R2.Top, R2.Left + IR.Width / FSS, R2.Top + IR.Height / FSS);

    Canvas.DrawBitmap(FCurrentBitmap, R1, R2, 1.0, True);
  end;

  if ScreenScaleHasChanged then
  begin
    ScreenScaleHasChanged := False;
    NotifyScreenScaleChanged;
  end;
end;

procedure TOriginalImage.NotifyScreenScaleChanged;
begin
  if Assigned(FOnScreenScaleChaned) then
    FOnScreenScaleChaned(Self);
end;

procedure TOriginalImage.SetOnScreenScaleChaned(const Value: TNotifyEvent);
begin
  FOnScreenScaleChaned := Value;
end;

{ TBitmapCollection }

constructor TBitmapCollection.Create(ANominalSize: TSize);
begin
  FNominalSize := ANominalSize;
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

function TBitmapCollection.ItemByScale(AScale: single): TBitmap;
begin
  FBitmaps.TryGetValue(AScale, result);
end;

function TBitmapCollection.Add(AScale: single): TBitmap;
var
  w, h: Integer;
begin
  if not FBitmaps.ContainsKey(AScale) then
  begin
    w := Round(FNominalSize.cx * AScale);
    h := Round(FNominalSize.cy * AScale);
    result := TBitmap.Create(w, h);
    FBitmaps.Add(AScale, result);
  end
  else
  begin
    FBitmaps.TryGetValue(AScale, result);
  end;
  Assert(result <> nil);
end;

end.
