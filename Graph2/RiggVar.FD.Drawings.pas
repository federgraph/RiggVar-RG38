unit RiggVar.FD.Drawings;

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
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Math.Vectors,
  RiggVar.RG.Calc,
  FMX.Graphics,
  FMX.StdCtrls,
  RiggVar.FD.TransformHelper,
  RiggVar.FD.Elements,
  RiggVar.FD.SchnittKK;

type
  TRggButtonGroup = class
  public
    Btn1: TSpeedButton;
    Btn2: TSpeedButton;
    Btn3: TSpeedButton;
    Btn4: TSpeedButton;
    Btn5: TSpeedButton;
    Btn6: TSpeedButton;
    Btn7: TSpeedButton;
    Btn8: TSpeedButton;
    Btn9: TSpeedButton;
    Btn0: TSpeedButton;

    BtnA: TSpeedButton;
    BtnB: TSpeedButton;
    BtnC: TSpeedButton;
    BtnD: TSpeedButton;
    BtnE: TSpeedButton;
    BtnF: TSpeedButton;

    procedure Reset;

    class var OnUpdateDrawing: TNotifyEvent;
    class procedure UpdateDrawing;
  end;

  TRggElementList = TList<TRggElement>;
  TRggCircleList = TList<TRggCircle>;
  TRggLineList = TList<TRggLine>;

  TRggCircleComparer = class(TInterfacedObject, IComparer<TRggCircle>)
  public
    function Compare(const Left, Right: TRggCircle): Integer;
  end;

  TRggLineComparer = class(TInterfacedObject, IComparer<TRggLine>)
  public
    function Compare(const Left, Right: TRggLine): Integer;
  end;

  TRggDrawing = class(TRggDrawingBase)
  private
    FName: string;
    FWantSort: Boolean;
    ElementList: TRggElementList;
    function GetElement(Index: Integer): TRggElement;
    procedure SetName(const Value: string);
    procedure SortedDraw(g: TCanvas);
    procedure UnsortedDraw(g: TCanvas);
    function GetIsValid: Boolean;
    function GetDefaultElementIndex: Integer;
    procedure SetWantSort(const Value: Boolean);
  protected
    CircleComparer: IComparer<TRggCircle>;
    LineComparer: IComparer<TRggLine>;
    SortedCircleList: TRggCircleList;
    SortedLineList: TRggLineList;
    function Find(ACaption: string): TRggCircle;
  public
    CircleList: TRggCircleList;
    LineList: TRggLineList;

    DefaultElement: TRggElement;

    ML: TStrings;
    WantMemoLines: Boolean;

    class var TH: TTransformHelper;

    constructor Create;
    destructor Destroy; override;

    procedure Add(Value: TRggElement);

    procedure SortElements;

    procedure InitItems(ML: TStrings);
    procedure WriteCode(ML: TStrings);
    procedure Reset; override;
    procedure Transform(M: TMatrix3D); override;
    procedure InitDefaultPos; virtual;
    procedure SaveAll;
    procedure Compute; virtual;
    procedure InitButtons(BG: TRggButtonGroup); virtual;

    procedure Draw(g: TCanvas);
    procedure GetInfo(ML: TStrings);

    procedure UpdateDrawing;

    property Name: string read FName write SetName;
    property Element[Index: Integer]: TRggElement read GetElement;
    property IsValid: Boolean read GetIsValid;
    property DefaultElementIndex: Integer read GetDefaultElementIndex;
    property MemoLines: TStrings read ML;
    property WantSort: Boolean read FWantSort write SetWantSort;
  end;

  TRggDrawingList = TList<TRggDrawing>;

  TRggDrawings = class
  public
    UseDarkColorScheme: Boolean;
    DrawingList: TRggDrawingList;
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value: TRggDrawing);
    procedure InitItems(ML: TStrings);
  end;

  TRggDrawingKK = class(TRggDrawing)
  protected
    SKK: TRggSchnittKK;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRggDrawing }

procedure TRggDrawing.Add(Value: TRggElement);
var
  cr: TRggCircle;
  cl: TRggLine;
begin
  ElementList.Add(Value);

  if Value is TRggCircle then
  begin
    cr := Value as TRggCircle;
    CircleList.Add(cr);
    cr.SpecialDraw := True;
    cr.Save;
  end;

  if Value is TRggLine then
  begin
    cl := Value as TRggLine;
    cl.SpecialDraw := True;
    LineList.Add(cl);
  end;
end;

function TRggDrawing.Find(ACaption: string): TRggCircle;
var
  c: TRggCircle;
begin
  result := nil;
  for c in CircleList do
    if c.Caption = aCaption then
        result := c;
end;

procedure TRggDrawing.Compute;
begin

end;

constructor TRggDrawing.Create;
begin
  Colors.GoLight;
  FName := 'Empty Drawing';
  DefaultShowCaption := True;
  ElementList := TRggElementList.Create;
  CircleList := TRggCircleList.Create;
  LineList := TRggLineList.Create;
  CircleComparer := TRggCircleComparer.Create;
  LineComparer := TRggLineComparer.Create;
  SortedCircleList := TRggCircleList.Create;
  SortedLineList := TRggLineList.Create;
  ML := TStringList.Create;
  WantOffset := True;
  WantSort := True;
end;

procedure TRggDrawing.InitButtons(BG: TRggButtonGroup);
begin
  if BG = nil then
    Exit;
  BG.Reset;
end;

procedure TRggDrawing.InitDefaultPos;
begin

end;

destructor TRggDrawing.Destroy;
var
  i: Integer;
begin
  for i := ElementList.Count-1 downto 0 do
  begin
    ElementList[i].Free;
  end;
  ElementList.Clear;
  ElementList.Free;
  CircleList.Free;
  LineList.Free;
  SortedCircleList.Free;
  SortedLineList.Free;
  ML.Free;
  inherited;
end;

procedure TRggDrawing.Draw(g: TCanvas);
begin
  if WantSort then
    SortedDraw(g)
  else
    UnsortedDraw(g);
end;

procedure TRggDrawing.UnsortedDraw(g: TCanvas);
var
  e: TRggElement;
begin
  for e in ElementList do
  begin
    e.Draw(g);
  end;
end;

procedure TRggDrawing.UpdateDrawing;
begin
  TRggButtonGroup.UpdateDrawing;
end;

procedure TRggDrawing.SetWantSort(const Value: Boolean);
begin
  FWantSort := Value;
end;

procedure TRggDrawing.SortedDraw(g: TCanvas);
var
  cr: TRggCircle;
  cl: TRggLine;
  e: TRggElement;
begin
  SortElements;

  for e in ElementList do
  begin
    if e is TRggTriangle then
    begin
      e.Draw(g);
      e.Painted := True;
    end;
  end;

  for cl in SortedLineList do
  begin
    cl.Draw(g);
    cl.Point1.Draw(g);
    cl.Point2.Draw(g);
    cl.Point1.Painted := True;
    cl.Point2.Painted := True;
  end;

  for cr in SortedCircleList do
  begin
    if not cr.Painted then
      cr.Draw(g);
  end;

  for e in ElementList do
  begin
    if e.Painted or e.SpecialDraw then
      Continue;

    e.Draw(g);
  end;
end;

function TRggDrawing.GetDefaultElementIndex: Integer;
begin
  if DefaultElement = nil then
    result := -1
  else
    result := ElementList.IndexOf(DefaultElement);
end;

function TRggDrawing.GetElement(Index: Integer): TRggElement;
begin
  result := ElementList[Index];
end;

function TRggDrawing.GetIsValid: Boolean;
var
  e: TRggElement;
begin
  result := True;
  for e in ElementList do
  begin
    if not e.GetValid then
    begin
      result := False;
      break;
    end;
  end;
end;

procedure TRggDrawing.GetInfo(ML: TStrings);
var
  e: TRggElement;
begin
  for e in ElementList do
    e.GetInfo(ML);
end;

procedure TRggDrawing.InitItems(ML: TStrings);
var
  d: TRggElement;
begin
  ML.Clear;
  for d in ElementList do
  begin
    ML.Add(d.GetListCaption)
  end;
end;

procedure TRggDrawing.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TRggDrawing.SortElements;
var
  i: Integer;
begin
  if WantSort then
  begin
    TRggLine.ResetCounter;

    SortedCircleList.Clear;
    for i := 0 to CircleList.Count-1 do
      SortedCircleList.Add(CircleList[i]);
    SortedCircleList.Sort(CircleComparer);

    SortedLineList.Clear;
    for i := 0 to LineList.Count-1 do
    begin
      if LineList[i].Visible then
        SortedLineList.Add(LineList[i]);
    end;
    SortedLineList.Sort(LineComparer);
  end;
end;

procedure TRggDrawing.SaveAll;
var
  c: TRggCircle;
begin
  for c in CircleList do
    c.Save;
end;

procedure TRggDrawing.Reset;
var
  c: TRggCircle;
begin
  InitDefaultPos;
  Compute;
  SaveAll;
  for c in CircleList do
    c.Reset;
end;

procedure TRggDrawing.Transform(M: TMatrix3D);
var
  c: TRggCircle;
begin
  TRggCircle.Matrix := M;
  for c in CircleList do
    c.Transform;
end;

procedure TRggDrawing.WriteCode(ML: TStrings);
var
  c: TRggCircle;
begin
  for c in CircleList do
  begin
    c.WriteCode(ML);
  end;
end;

{ TRggDrawings }

procedure TRggDrawings.Add(Value: TRggDrawing);
var
  e: TRggElement;
begin
  DrawingList.Add(Value);

  Value.IsDark := UseDarkColorScheme;

  for e in Value.ElementList do
  begin
    e.Drawing := Value;
  end;
end;

constructor TRggDrawings.Create;
begin
  DrawingList := TRggDrawingList.Create;
end;

destructor TRggDrawings.Destroy;
var
  i: Integer;
begin
  for i := DrawingList.Count-1 downto 0 do
  begin
    DrawingList[i].Free;
  end;
  DrawingList.Clear;
  DrawingList.Free;
  inherited;
end;

procedure TRggDrawings.InitItems(ML: TStrings);
var
  d: TRggDrawing;
begin
  ML.Clear;
  for d in DrawingList do
  begin
    ML.Add(d.Name)
  end;
end;

{ TRggLineComparer }

function TRggLineComparer.Compare(const Left, Right: TRggLine): Integer;
begin
  result := TRggLine.Compare(Left, Right);
end;

{ TRggCircleComparer }

function TRggCircleComparer.Compare(const Left, Right: TRggCircle): Integer;
begin
  result := TRggCircle.Compare(Left, Right);
end;

{ TRggDrawingKK }

constructor TRggDrawingKK.Create;
begin
  inherited;
  SKK := TRggSchnittKK.Create;
  SKK.SchnittEbene := TSchnittEbene.seXY;
end;

destructor TRggDrawingKK.Destroy;
begin
  SKK.Free;
  inherited;
end;

{ TRggButtonGroup }

procedure TRggButtonGroup.Reset;
begin
  Btn1.OnClick := nil;
  Btn2.OnClick := nil;
  Btn3.OnClick := nil;
  Btn4.OnClick := nil;
  Btn5.OnClick := nil;
  Btn6.OnClick := nil;
  Btn7.OnClick := nil;
  Btn8.OnClick := nil;
  Btn9.OnClick := nil;
  Btn0.OnClick := nil;

  BtnA.OnClick := nil;
  BtnB.OnClick := nil;
  BtnC.OnClick := nil;
  BtnD.OnClick := nil;
  BtnE.OnClick := nil;
  BtnF.OnClick := nil;

  Btn1.Text := '1';
  Btn2.Text := '2';
  Btn3.Text := '3';
  Btn4.Text := '4';
  Btn5.Text := '5';
  Btn6.Text := '6';
  Btn7.Text := '7';
  Btn8.Text := '8';
  Btn9.Text := '9';
  Btn0.Text := '0';

  BtnA.Text := 'A';
  BtnB.Text := 'B';
  BtnC.Text := 'C';
  BtnD.Text := 'D';
  BtnE.Text := 'E';
  BtnF.Text := 'F';

  Btn1.Hint := 'Btn 1';
  Btn2.Hint := 'Btn 2';
  Btn3.Hint := 'Btn 3';
  Btn4.Hint := 'Btn 4';
  Btn5.Hint := 'Btn 5';
  Btn6.Hint := 'Btn 6';
  Btn7.Hint := 'Btn 7';
  Btn8.Hint := 'Btn 8';
  Btn9.Hint := 'Btn 9';
  Btn0.Hint := 'Btn 0';

  BtnA.Hint := 'Btn A';
  BtnB.Hint := 'Btn B';
  BtnC.Hint := 'Btn C';
  BtnD.Hint := 'Btn D';
  BtnE.Hint := 'Btn E';
  BtnF.Hint := 'Btn F';
end;

class procedure TRggButtonGroup.UpdateDrawing;
begin
  if Assigned(TRggButtonGroup.OnUpdateDrawing) then
    TRggButtonGroup.OnUpdateDrawing(nil);
end;

end.
