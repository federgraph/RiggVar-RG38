unit RiggVar.FD.Image;

interface

uses
  System.Types,
  System.Classes,
  System.Messaging,
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
    FMultiResBitmap: TBitmapCollection;
    FScaleChangedId: Integer;
    FScreenScale: Single;
    function GetBitmap: TBitmap;
    function ItemForCurrentScale: TBitmap;
    procedure ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure UpdateCurrentBitmap;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer); reintroduce;
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
  public
    property Scale;
    property ScreenScale: single read FScreenScale;
  end;

implementation

uses
  FMX.Forms;

{ TOriginalImage }

constructor TOriginalImage.Create(AOwner: TComponent; AWidth: Integer; AHeight: Integer);
begin
  inherited Create(AOwner);
  FNominalSize := TSize.Create(AWidth, AHeight);
  FMultiResBitmap := TBitmapCollection.Create(FNominalSize);
  SetAcceptsControls(False);
  FScaleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
end;

destructor TOriginalImage.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, FScaleChangedId);
  FMultiResBitmap.Free;
  inherited;
end;

function TOriginalImage.ItemForCurrentScale: TBitmap;
begin
  if Scene <> nil then
    FScreenScale := Scene.GetSceneScale
  else
    FScreenScale := 1.0;

  if FScreenScale < 1 then
    FScreenScale := 1.0;

  result := FMultiResBitmap.ItemByScale(FScreenScale);
  if result = nil then
    result := FMultiResBitmap.Add(FScreenScale);
end;

function TOriginalImage.GetBitmap: TBitmap;
begin
  result := ItemForCurrentScale;
end;

procedure TOriginalImage.UpdateCurrentBitmap;
begin
  FCurrentBitmap := GetBitmap;
end;

procedure TOriginalImage.Paint;
var
  IR: TRectF;
begin
  UpdateCurrentBitmap;
  if FCurrentBitmap <> nil then
  begin
    IntersectRect(IR, LocalRect, FCurrentBitmap.BoundsF);
    Canvas.DrawBitmap(FCurrentBitmap, IR, IR, 1.0, True);
  end;
end;

procedure TOriginalImage.ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  Repaint;
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
