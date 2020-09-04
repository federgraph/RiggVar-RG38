unit RggDisplay;

interface

uses
  System.UITypes,
  System.UIConsts,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types,
  FMX.Graphics,
  RggTypes,
  RggCalc,
  RggDisplayTypes,
  RggDisplayOrder;

type
  TDisplayItem = class
  public
    Name: string;
    Edge: TDisplayEdge;
    ItemType: TDisplayItemType;
    P1: TPoint3D;
    P2: TPoint3D;

    StrokeWidth: single;
    StrokeColor: TAlphaColor;

    LineStart: TPointF;
    LineEnd: TPointF;

    CenterPoint: TPointF;
    Radius: single;

    Polygon: TPolygon;

    Bemerkung: TLineSegmentCompareCase;

    IsRod: Boolean;

    procedure DrawLegend(g: TCanvas; j: Integer);
    procedure Draw(g: TCanvas);
    procedure Assign(Value: TDisplayItem);
  public
    class var
    CounterLeftNil: Integer;
    CounterRightNil: Integer;
    CounterHardCodedAbove: Integer;
    CounterHardCodedBelow: Integer;
    CounterSame: Integer;
    CounterTotallyAbove: Integer;
    CounterTotallyBelow: Integer;
    CounterCommon: Integer;
    CounterParallel: Integer;
    CounterNoVisibleCrossing: Integer;
    CounterSPY: Integer;
    CounterZero: Integer;
    NullpunktOffset: TPointF;
    class function CounterSum: Integer;
    class procedure ResetCounter;
    class function Compare(const Left, Right: TDisplayItem): Integer;
  end;

  TDisplayItemComparer = class(TInterfacedObject, IComparer<TDisplayItem>)
  public
    function Compare(const Left, Right: TDisplayItem): Integer;
  end;

  TDisplayList = TList<TDisplayItem>;

  TRggDisplayList = class
  private
    FList: TDisplayList;
    FIndex: Integer;
    FCapacity: Integer;
    FNeedSort: Boolean;
    function Add: TDisplayItem;
    procedure CheckCount;
  protected
    DisplayItemComparer: IComparer<TDisplayItem>;
  public
    IL: TStringList;
    DI: TDisplayItem;
    DF: TRggFrame; // injected
    WantLineColors: Boolean;
    CounterDraw: Integer;
    CounterCompareItems: Integer;
    FReportList: TStringList;
    WantLegend: Boolean;
    Verbose: Boolean;
    UseQuickSort: Boolean;
    ErrorCode: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Ellipse(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; CenterPoint: TPointF; Radius: single = 10);
    procedure Line(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; A, B: TPointF; Color: TAlphaColor);
    procedure PolyLine(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; A: TPolygon; Color: TAlphaColor);
    procedure Draw(Canvas: TCanvas);
    procedure Draw1(Canvas: TCanvas);
    procedure Draw2(Canvas: TCanvas);
    function CompareItems(i1, i2: Integer): Integer;
    function CheckLinePairs: Boolean;
    procedure DoCounterReport(ML: TStrings);
    procedure DoLinePairReport(ML: TStrings);
    procedure GetItemNameList(ML: TStrings);
    function FindItem(const AName: string): TDisplayItem;
    function FindItemByEdge(const Value: TDisplayEdge): TDisplayItem;
  end;

implementation

var
  PD: TPathData;

{ TDisplayItem }

procedure TDisplayItem.Assign(Value: TDisplayItem);
begin
  StrokeWidth := Value.StrokeWidth;
  StrokeColor := Value.StrokeColor;
end;

procedure TDisplayItem.DrawLegend(g: TCanvas; j: Integer);
var
  x, w, h: single;
  R: TRectF;

  procedure TextOut(s: string);
  begin
    g.FillText(
      R,
      s, // Text
      false, // WordWrap
      1.0, // Opacity
      [], // empty set means use 'LeftToRight', [TFillTextFlag.RightToLeft]
      TTextAlign.Center, // horizontal alignment
      TTextAlign.Center); // vertical alignment
  end;

begin
  w := 100;
  h := 25;
  x := 0;
  if IsRod then
    x := 20;
  R := TRectF.Create(x, j * h, x + w, (j + 1) * h);
  R.Offset(-NullpunktOffset.X, -NullpunktOffset.Y);
  g.Stroke.Thickness := 1.0;
  g.Stroke.Color := claBlack;
  g.Fill.Color := StrokeColor;
  g.FillRect(R, 0, 0, [], 0.7);
  g.DrawRect(R, 0, 0, [], 0.7);

  g.Fill.Color := claWhite;
  g.Font.Family := 'Consolas';
  g.Font.Size := 16;
  TextOut(Name);
end;

procedure TDisplayItem.Draw(g: TCanvas);
var
  R: TRectF;
  i: Integer;
begin
  g.Stroke.Thickness := StrokeWidth;
  g.Stroke.Color := StrokeColor;

  case ItemType of
    diLine:
    begin
      g.DrawLine(LineStart, LineEnd, 1.0);
    end;

    diPolyLine:
    begin
      PD.Clear;
      PD.MoveTo(Polygon[0]);
      for i := 1 to Length(Polygon) - 1 do
        PD.LineTo(Polygon[i]);
      g.DrawPath(PD, 1.0);
    end;

    diEllipse:
    begin
      R := TRectF.Create(CenterPoint);
      R.Inflate(Radius, Radius);
      g.DrawEllipse(R, 1.0);
    end;

  end;
end;

class procedure TDisplayItem.ResetCounter;
begin
  CounterLeftNil := 0;
  CounterRightNil := 0;
  CounterHardcodedAbove := 0;
  CounterHardcodedBelow := 0;
  CounterSame := 0;
  CounterTotallyAbove := 0;
  CounterTotallyBelow := 0;
  CounterCommon := 0;
  CounterParallel := 0;
  CounterNoVisibleCrossing := 0;
  CounterSPY := 0;
  CounterZero := 0;
end;

class function TDisplayItem.Compare(const Left, Right: TDisplayItem): Integer;
var
  LP: TRggLinePair;
  r: Integer;
begin
  if Left = nil then
  begin
    Left.Bemerkung := ccNil;
    Inc(CounterLeftNil);
    result := 0;
    Exit;
  end;

  if Right = nil then
  begin
    Left.Bemerkung := ccNil;
    Inc(CounterRightNil);
    result := 0;
    Exit;
  end;

  LP.SP := TPoint3D.Zero;
  LP.L1.A.P := Left.P1;
  LP.L1.B.P := Left.P2;
  LP.L1.Name := Left.Name;
  LP.L2.A.P := Right.P1;
  LP.L2.B.P := Right.P2;
  LP.L2.Name := Right.Name;

  if False then

  else if LP.L1.IsSame(LP.L2) then
  begin
    Inc(CounterSame);
    Left.Bemerkung := ccTotallySame;
    r := 0;
    Dec(CounterZero); // compensate for Inc below
  end

  else if Left.ItemType = diEllipse then
  begin
    Inc(CounterHardCodedBelow);
    Left.Bemerkung := ccHardcodedBelow;
    r := -1;
  end

  else if Right.ItemType = diEllipse then
  begin
    Inc(CounterHardCodedAbove);
    Left.Bemerkung := ccHardcodedAbove;
    r := 1;
  end

  else if LP.L1.IsTotallyAbove(LP.L2) then
  begin
    Inc(CounterTotallyAbove);
    Left.Bemerkung := ccTotallyAbove;
    r := 1;
  end

  else if LP.L1.IsTotallyBelow(LP.L2) then
  begin
    Inc(CounterTotallyBelow);
    Left.Bemerkung := ccTotallyBelow;
    r := -1;
  end

  else if LP.HasCommonPoint then
  begin
    Inc(CounterCommon);
    r := LP.CompareCommon;
    case r of
      0: Left.Bemerkung := ccCommonSame;
      1: Left.Bemerkung := ccCommonAbove;
      -1: Left.Bemerkung := ccCommonBelow;
      else
        Left.Bemerkung := ccCommonNone;
    end;
  end

  { As a side effect, this call to IsParallel will set SP }
  else if LP.IsParallel then
  begin
    Inc(CounterParallel);
    Left.Bemerkung := ccParallel;
    r := 0;
  end

  { returning r = 0 does help at all with sort algorithm }
//  else if LP.DoesNotHaveVisibleCrossing then
//  begin
//    Inc(CounterNoVisibleCrossing);
//    Left.Bemerkung := ccNoVisibleCrossing;
//    r := 0;
//  end

  else
  begin
    Inc(CounterSPY);
    r := LP.CompareSPY;
    case r of
      0: Left.Bemerkung := ccSame;
      1: Left.Bemerkung := ccAbove;
      -1: Left.Bemerkung := ccBelow;
      else
        Left.Bemerkung := ccNone;
    end;
  end;

  if r = 0 then
  begin
    Inc(CounterZero);
  end;

  result := r;
end;

class function TDisplayItem.CounterSum: Integer;
begin
  result :=
    CounterLeftNil +
    CounterRightNil +
    CounterHardCodedAbove +
    CounterHardCodedBelow +
    CounterSame +
    CounterTotallyAbove +
    CounterTotallyBelow +
    CounterCommon +
    CounterParallel +
    CounterNoVisibleCrossing +
    CounterSPY;
end;

{ TRggDisplayList }

constructor TRggDisplayList.Create;
begin
  FCapacity := 300;
  FList := TDisplayList.Create;
  DI := TDisplayItem.Create;
  IL := TStringList.Create;
  Clear;

  DisplayItemComparer := TDisplayItemComparer.Create;
  FReportList := TStringList.Create;

  UseQuickSort := False;
end;

destructor TRggDisplayList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
    FList[i].Free;
  FList.Clear;
  FList.Free;
  DI.Free;
  IL.Free;
  FReportList.Free;
  inherited;
end;

procedure TRggDisplayList.Clear;
begin
  FIndex := -1;
  FNeedSort := True;
  IL.Clear;
end;

function TRggDisplayList.CheckLinePairs: Boolean;
var
  i: Integer;
  i1, i2: Integer;
  r: Integer;
begin
  result := False;
  for i := 1 to FList.Count-1 do
  begin
    i1 := i-1;
    i2 := i;
    r := CompareItems(i1, i2);
    if r = 1 then
      break;
    result := True;
  end;
end;

function TRggDisplayList.CompareItems(i1, i2: Integer): Integer;
var
  cr1, cr2: TDisplayItem;
  LP: TRggLinePair;

  procedure Add(verb: string);
  begin
    FReportList.Add(Format('  %s "%s" %s.', [cr1.Name, verb, cr2.Name]));
  end;

begin
  result := 0;
  Inc(CounterCompareItems);

  TDisplayItem.ResetCounter;
  if (i1 < 0) or (i2 < 0) then
  begin
    { Can happen when Hull Visiblility is toggled and UI not updated }
    Exit;
  end;

  if (i1 > FList.Count-1) or (i2 > FList.Count-1) then
  begin
    Exit;
  end;

  cr1 := FList.Items[i1];
  cr2 := FList.Items[i2];
  result := TDisplayItem.Compare(cr1, cr2);

  FReportList.Clear;
  if Verbose then
  begin
    FReportList.Add('Bemerkung:');
    FReportList.Add('');
  end;
  case cr1.Bemerkung of
    ccNone: Add('has none');
    ccNil: Add('is nil');
    ccHardcodedAbove: Add('hardcoded above');
    ccHardcodedBelow: Add('hardcoded below');
    ccParallel: Add('is parallel to');
    ccNoVisibleCrossing: Add('no visible crossing');
    ccTotallyAbove: Add('totally above');
    ccTotallyBelow: Add('totally below');
    ccTotallySame: Add('totally same');
    ccCommonNone: Add('common none');
    ccCommonAbove: Add('common above');
    ccCommonBelow: Add('common below');
    ccCommonSame: Add('common same');
    ccAbove: Add('sp above');
    ccBelow: Add('sp below');
    ccSame: Add('sp same');
    ccUnknown: Add('unknown');
  end;

  LP.L1.A.P := cr1.P1;
  LP.L1.B.P := cr1.P2;
  LP.L1.Name := cr1.Name;
  LP.L2.A.P := cr2.P1;
  LP.L2.B.P := cr2.P2;
  LP.L2.Name := cr2.Name;

  FReportList.Add('');

  if Verbose then
  begin
    FReportList.Add('LinePair.ReportData:');
  end;
  LP.ReportData(FReportList);
end;

procedure TRggDisplayList.DoCounterReport(ML: TStrings);
begin
  if Verbose then
  begin
    ML.Add('--- DL.DoCounterReport ---');
    ML.Add(Format('DrawCoutner = %d', [CounterDraw]));
    ML.Add(Format('FList.Count = %d', [FList.Count]));
  end;

  if TDisplayItem.CounterLeftNil > 0 then
    ML.Add(Format('C LeftNil = %d', [TDisplayItem.CounterLeftNil]));
  if TDisplayItem.CounterRightNil > 0 then
    ML.Add(Format('C RightNil = %d', [TDisplayItem.CounterRightNil]));

  ML.Add(Format('C EllipseAbove = %d', [TDisplayItem.CounterHardCodedAbove]));
  ML.Add(Format('C EllipseBelow = %d', [TDisplayItem.CounterHardCodedBelow]));
  ML.Add(Format('C Same = %d', [TDisplayItem.CounterSame]));
  ML.Add(Format('C Totally Above = %d', [TDisplayItem.CounterTotallyAbove]));
  ML.Add(Format('C Totally Below = %d', [TDisplayItem.CounterTotallyBelow]));
  ML.Add(Format('C Common = %d', [TDisplayItem.CounterCommon]));
  ML.Add(Format('C Parallel = %d', [TDisplayItem.CounterParallel]));
  ML.Add(Format('C SP Y NVC = %d', [TDisplayItem.CounterNoVisibleCrossing]));
  ML.Add(Format('C SP Y = %d', [TDisplayItem.CounterSPY]));
  ML.Add(Format('C Sum = %d', [TDisplayItem.CounterSum]));
  ML.Add(Format('C Zero = %d', [TDisplayItem.CounterZero]));
end;

procedure TRggDisplayList.DoLinePairReport(ML: TStrings);
var
  i: Integer;
begin
  if Verbose then
  begin
    ML.Add('--- DL.DoLinePairReport ---');
    ML.Add(Format('DrawCoutner = %d', [CounterDraw]));
    ML.Add(Format('CompareItemsCounter = %d', [CounterCompareItems]));
    ML.Add('');
    if TDisplayItem.CounterNoVisibleCrossing = 1 then
      ML.Add('  No visible crossing')
    else if TDisplayItem.CounterTotallyAbove = 1 then
      ML.Add('  Totally Above')
    else if TDisplayItem.CounterTotallyBelow = 1 then
      ML.Add('  Totally Below')
    else if TDisplayItem.CounterSPY = 1 then
      ML.Add('  SPY')
    else if TDisplayItem.CounterSame = 1 then
      ML.Add('  Same')
    else if TDisplayItem.CounterParallel = 1 then
      ML.Add('  Parallel');
  end;

  ML.Add('');
  for i := 0 to FReportList.Count-1 do
  begin
    ML.Add(FReportList[i]);
  end;
end;

procedure TRggDisplayList.CheckCount;
var
  i: Integer;
begin
  if FList.Count > FIndex + 1 then
  begin
    for i := FIndex + 1 to FList.Count - 1 do
      FList[i].Free;
    FList.Count := FIndex + 1;
  end;
end;

function TRggDisplayList.Add: TDisplayItem;
begin
  Inc(FIndex);
  if FIndex < FList.Count then
  begin
    result := FList[FIndex];
  end
  else if (FIndex >= FList.Count) and (FIndex < FCapacity) then
  begin
    result := TDisplayItem.Create;
    FList.Add(result); // returns Index in List
  end
  else
  begin
    FIndex := FList.Count - 1;
    result := FList.Last;
  end;
  result.Assign(DI);
end;

procedure TRggDisplayList.Ellipse(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; CenterPoint: TPointF; Radius: single = 10);
var
  cr: TDisplayItem;
begin
  IL.Add(Name);
  cr := Add;
  cr.Name := Name;
  cr.Edge := EdgeName;
  cr.ItemType := diEllipse;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.P1.X := cr.P1.X - Radius;
  cr.P2.X := cr.P2.X + Radius;
  cr.CenterPoint := CenterPoint;
  cr.Radius := Radius;
end;

procedure TRggDisplayList.Line(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; A, B: TPointF; Color: TAlphaColor);
var
  cr: TDisplayItem;
begin
  IL.Add(Name);
  if WantLineColors then
    DI.StrokeColor := Color;
  cr := Add;
  cr.Name := Name;
  cr.Edge := EdgeName;
  cr.ItemType := diLine;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.LineStart := A;
  cr.LineEnd := B;

  case EdgeName of
    deA0A, deB0B, deC0C, deD0D, deE0E, deCF:
      cr.IsRod := True;
    else
      cr.IsRod := False;
  end;
end;

procedure TRggDisplayList.PolyLine(const Name: string; EdgeName: TDisplayEdge; P1, P2: TPoint3D; A: TPolygon; Color: TAlphaColor);
var
  cr: TDisplayItem;
begin
  IL.Add(Name);
  if WantLineColors then
    DI.StrokeColor := Color;
  cr := Add;
  cr.Name := Name;
  cr.Edge := EdgeName;
  cr.ItemType := diPolyLine;
  cr.P1 := P1;
  cr.P2 := P2;
  cr.Polygon := A;

  cr.IsRod := False;
  if EdgeName = deD0D then
    cr.IsRod := True;
end;

procedure TRggDisplayList.GetItemNameList(ML: TStrings);
var
  cr: TDisplayItem;
begin
  for cr in FList do
  begin
    ML.Add(cr.Name);
  end;
end;

function TRggDisplayList.FindItem(const AName: string): TDisplayItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
  begin
     if FList[i].Name = AName then
     begin
       result := FList[i];
       break;
     end;
  end;
end;

function TRggDisplayList.FindItemByEdge(const Value: TDisplayEdge): TDisplayItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
  begin
    if FList[i].Edge = Value then
    begin
      result := FList[i];
      break;
    end;
  end;
end;

procedure TRggDisplayList.Draw(Canvas: TCanvas);
begin
  if DF = nil then
  begin
    Draw1(Canvas)
  end
  else
  begin
    if UseQuickSort then
      Draw1(Canvas)
    else
      Draw2(Canvas);
  end;
end;

procedure TRggDisplayList.Draw2(Canvas: TCanvas);
var
  de: TDisplayEdge;
  cr: TDisplayItem;
  j: Integer;
begin
  Inc(CounterDraw);
  CheckCount;

  if FNeedSort then
  begin
    TDisplayItem.ResetCounter;
  end;

  j := 0;
  for de in DF.TempList do
  begin
    cr := FindItemByEdge(de);
    if cr <> nil then
    begin
      if WantLegend then
      begin
        cr.DrawLegend(Canvas, j);
        Inc(j);
      end;
      cr.Draw(Canvas);
    end;
  end;

  FNeedSort := False;
end;

procedure TRggDisplayList.Draw1(Canvas: TCanvas);
var
  cr: TDisplayItem;
  j: Integer;
begin
  Inc(CounterDraw);
  CheckCount;

  if FNeedSort then
  begin
    TDisplayItem.ResetCounter;
    FList.Sort(DisplayItemComparer);
  end;

  if WantLegend then
  begin
    j := 0;
    for cr in FList do
    begin
      cr.DrawLegend(Canvas, j);
      Inc(j);
    end;
  end;

  for cr in FList do
  begin
    cr.Draw(Canvas);
  end;

  FNeedSort := False;
end;

{ TDisplayItemComparer }

function TDisplayItemComparer.Compare(const Left, Right: TDisplayItem): Integer;
begin
  result := TDisplayItem.Compare(Left, Right);
end;

initialization
  PD := TPathData.Create;

finalization
  PD.Free;
  PD := nil;

end.
