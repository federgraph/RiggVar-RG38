unit RggScroll;

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
  System.Classes,
  RiggVar.RG.Def,
  RggTypes;

type
//  TRggParams = (
//    fpController,
//    fpWinkel,
//    fpVorstag,
//    fpWante,
//    fpWoben,
//    fpSalingH,
//    fpSalingA,
//    fpSalingL,
//    fpVorstagOS,
//    fpWPowerOS
//    fpSalingW,
//    fpMastfallF0C,
//    fpMastfallF0F,
//    fpMastfallVorlauf,
//    fpBiegung,
//    fpD0X,
//    fpT1,
//    fpT2
//    );

  TRggSB = class
  public
    Ist: double;
    Min: double;
    Max: double;
    TinyStep: Integer;
    BigStep: Integer;

    procedure Assign(Value: TRggSB);
    procedure SaveToStream(s: TStream);
    procedure LoadFromStream(s: TStream);
    function GetValue(n: TsbParam): double;
  end;

  TRggFA = class
  public
    Dummy: TRggSB;

    Controller: TRggSB;
    Winkel: TRggSB;
    Vorstag: TRggSB;
    Wante: TRggSB;
    Woben: TRggSB;
    SalingH: TRggSB;
    SalingA: TRggSB;
    SalingL: TRggSB;
    VorstagOS: TRggSB;
    WPowerOS: TRggSB;
    T1: TRggSB;
    T2: TRggSB;
    SalingW: TRggSB;
    MastfallF0C: TRggSB;
    MastfallF0F: TRggSB;
    MastfallVorlauf: TRggSB;
    Biegung: TRggSB;
    D0X: TRggSB;

    constructor Create;
    destructor Destroy; override;

    procedure Assign(Value: TRggFA);

    function GetSB(sbn: TsbName): TRggSB;
    function Find(Value: TFederParam): TRggSB;

    procedure InitStepDefault;
    procedure InitTinyStep(Value: Integer);
    procedure InitBigStep(Value: Integer);

    procedure SaveToStream(s: TStream);
    procedure LoadFromStream(s: TStream);
  end;

implementation

{ TRggSB }

procedure TRggSB.Assign(Value: TRggSB);
begin
  Ist := Value.Ist;
  Min := Value.Min;
  Max := Value.Max;
  TinyStep := Value.TinyStep;
  BigStep := Value.BigStep;
end;

function TRggSB.GetValue(n: TsbParam): double;
begin
  case n of
    TsbParam.Ist: result := Ist;
    TsbParam.Min: result := Min;
    TsbParam.Max: result := Max;
    TsbParam.TinyStep: result := TinyStep;
    TsbParam.BigStep: result := BigStep;
    else
      result := 0;
  end;
end;

procedure TRggSB.LoadFromStream(s: TStream);
var
  temp: Integer;
begin
  temp := 0;
  s.ReadBuffer(temp, SizeOf(Integer));
  Ist := temp;
  s.ReadBuffer(temp, SizeOf(Integer));
  Min := temp;
  s.WriteBuffer(temp, SizeOf(Integer));
  Max := temp;
  s.WriteBuffer(TinyStep, SizeOf(Integer));
  s.WriteBuffer(BigStep, SizeOf(Integer));
end;

procedure TRggSB.SaveToStream(s: TStream);
var
  temp: Integer;
begin
  temp := Round(Ist);
  s.WriteBuffer(temp, SizeOf(Integer));
  temp := Round(Min);
  s.WriteBuffer(temp, SizeOf(Integer));
  temp := Round(Max);
  s.WriteBuffer(temp, SizeOf(Integer));

  s.WriteBuffer(TinyStep, SizeOf(Integer));
  s.WriteBuffer(BigStep, SizeOf(Integer));
end;

{ TRggFA }

constructor TRggFA.Create;
begin
  Dummy := TRggSB.Create;
  Controller := TRggSB.Create;
  Winkel := TRggSB.Create;
  Vorstag := TRggSB.Create;
  Wante := TRggSB.Create;
  Woben := TRggSB.Create;
  SalingH := TRggSB.Create;
  SalingA := TRggSB.Create;
  SalingL := TRggSB.Create;
  VorstagOS := TRggSB.Create;
  WPowerOS := TRggSB.Create;
  SalingW := TRggSB.Create;
  MastfallF0C := TRggSB.Create;
  MastfallF0F := TRggSB.Create;
  MastfallVorlauf := TRggSB.Create;
  Biegung := TRggSB.Create;
  D0X := TRggSB.Create;
  T1 := TRggSB.Create;
  T2 := TRggSB.Create;
  T2.Ist := 1;
end;

destructor TRggFA.Destroy;
begin
  Dummy.Free;
  Controller.Free;
  Winkel.Free;
  Vorstag.Free;
  Wante.Free;
  Woben.Free;
  SalingH.Free;
  SalingA.Free;
  SalingL.Free;
  VorstagOS.Free;
  WPowerOS.Free;
  SalingW.Free;
  MastfallF0C.Free;
  MastfallF0F.Free;
  MastfallVorlauf.Free;
  Biegung.Free;
  D0X.Free;
  T1.Free;
  T2.Free;
  inherited;
end;

procedure TRggFA.InitStepDefault;
begin
  InitTinyStep(1);
  InitTinyStep(10)
end;

procedure TRggFA.InitTinyStep(Value: Integer);
begin
  Controller.TinyStep := Value;
  Winkel.TinyStep := Value;
  Vorstag.TinyStep := Value;
  Wante.TinyStep := Value;
  Woben.TinyStep := Value;
  SalingH.TinyStep := Value;
  SalingA.TinyStep := Value;
  SalingL.TinyStep := Value;
  VorstagOS.TinyStep := Value;
  WPowerOS.TinyStep := Value;
  T1.TinyStep := Value;
  T2.TinyStep := Value;
end;

procedure TRggFA.Assign(Value: TRggFA);
begin
  Controller.Assign(Value.Controller);
  Winkel.Assign(Value.Winkel);
  Vorstag.Assign(Value.Vorstag);
  Wante.Assign(Value.Wante);
  Woben.Assign(Value.Woben);
  SalingH.Assign(Value.SalingH);
  SalingA.Assign(Value.SalingA);
  SalingL.Assign(Value.SalingL);
  VorstagOS.Assign(Value.VorstagOS);
  WPowerOS.Assign(Value.WPowerOS);
  SalingW.Assign(Value.SalingW);
  MastfallF0C.Assign(Value.MastfallF0C);
  MastfallF0F.Assign(Value.MastfallF0F);
  MastfallVorlauf.Assign(Value.MastfallVorlauf);
  Biegung.Assign(Value.Biegung);
  D0X.Assign(Value.D0X);
  T1.Assign(Value.T1);
  T2.Assign(Value.T2);
end;

function TRggFA.GetSB(sbn: TsbName): TRggSB;
begin
  result := nil;
  case sbn of
    fpController: result := Controller;
    fpWinkel: result := Winkel;
    fpVorstag: result := Vorstag;
    fpWante: result := Wante;
    fpWoben: result := Woben;
    fpSalingH: result := SalingH;
    fpSalingA: result := SalingA;
    fpSalingL: result := SalingL;
    fpVorstagOS: result := VorstagOS;
    fpWPowerOS: result := WPowerOS;
  end;
end;

procedure TRggFA.InitBigStep(Value: Integer);
begin
  Controller.BigStep := Value;
  Winkel.BigStep := Value;
  Vorstag.BigStep := Value;
  Wante.BigStep := Value;
  Woben.BigStep := Value;
  SalingH.BigStep := Value;
  SalingA.BigStep := Value;
  SalingL.BigStep := Value;
  VorstagOS.BigStep := Value;
  WPowerOS.BigStep := Value;
  T1.BigStep := Value;
  T2.BigStep := Value;
end;

function TRggFA.Find(Value: TFederParam): TRggSB;
begin
  case Value of
    TFederParam.fpController: result := Controller;
    TFederParam.fpWinkel: result := Winkel;
    TFederParam.fpVorstag: result := Vorstag;
    TFederParam.fpWante: result := Wante;
    TFederParam.fpWoben: result := Woben;
    TFederParam.fpSalingH: result := SalingH;
    TFederParam.fpSalingA: result := SalingA;
    TFederParam.fpSalingL: result := SalingL;
    TFederParam.fpSalingW: result := SalingW;
    TFederParam.fpMastfallF0C: result := MastfallF0C;
    TFederParam.fpMastfallF0F: result := MastfallF0F;
    TFederParam.fpMastfallVorlauf: result := MastfallVorlauf;
    TFederParam.fpBiegung: result := Biegung;
    TFederParam.fpD0X: result := D0X;
    TFederParam.fpT1: result := T1;
    TFederParam.fpT2: result := T2;
    TFederParam.fpVorstagOS: result := VorstagOS;
    TFederParam.fpWPowerOS: result := WPowerOS;
    else
      result := Dummy;
  end;
end;

procedure TRggFA.LoadFromStream(s: TStream);
begin
  Controller.LoadFromStream(s);
  Winkel.LoadFromStream(s);
  Vorstag.LoadFromStream(s);
  Wante.LoadFromStream(s);
  Woben.LoadFromStream(s);
  SalingH.LoadFromStream(s);
  SalingA.LoadFromStream(s);
  SalingL.LoadFromStream(s);
  VorstagOS.LoadFromStream(s);
  WPowerOS.LoadFromStream(s);
  SalingW.LoadFromStream(s);
  MastfallF0C.LoadFromStream(s);
  MastfallF0F.LoadFromStream(s);
  MastfallVorlauf.LoadFromStream(s);
  Biegung.LoadFromStream(s);
  D0X.LoadFromStream(s);
  T1.LoadFromStream(s);
  T2.LoadFromStream(s);
end;

procedure TRggFA.SaveToStream(s: TStream);
begin
  Controller.SaveToStream(s);
  Winkel.SaveToStream(s);
  Vorstag.SaveToStream(s);
  Wante.SaveToStream(s);
  Woben.SaveToStream(s);
  SalingH.SaveToStream(s);
  SalingA.SaveToStream(s);
  SalingL.SaveToStream(s);
  VorstagOS.SaveToStream(s);
  WPowerOS.SaveToStream(s);
  SalingW.SaveToStream(s);
  MastfallF0C.SaveToStream(s);
  MastfallF0F.SaveToStream(s);
  MastfallVorlauf.SaveToStream(s);
  Biegung.SaveToStream(s);
  D0X.SaveToStream(s);
  T1.SaveToStream(s);
  T2.SaveToStream(s);
end;

end.
