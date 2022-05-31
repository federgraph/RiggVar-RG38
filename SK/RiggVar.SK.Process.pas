unit RiggVar.SK.Process;

interface

uses
  System.Classes,
  FMX.Types,
  RiggVar.RG.Def,
  RiggVar.RG.Types;

type
  TIdleHandler = class
  private
    FDelta: single;
    FParam: TCircleParam;
    procedure SetDelta(const Value: single);
    procedure SetParam(const Value: TCircleParam);
  public
    ParamChanged: Boolean;
    ParamValueChanged: Boolean;

    TextUpdateNeeded: Boolean;
    GraphUpdateNeeded: Boolean;

    Counter: Integer;

    procedure DoOnIdle;

    procedure UpdateParam;
    procedure UpdateParamValue;
    procedure UpdateGraph;
    procedure UpdateText;

    property Param: TCircleParam read FParam write SetParam;
    property Delta: single read FDelta write SetDelta;
  end;

implementation

uses
  FrmSK03,
  RiggVar.SK.Main;

{ TIdleHandler }

procedure TIdleHandler.SetDelta(const Value: single);
begin
  if Value < 0 then
    FDelta := -1
  else if Value > 0 then
    FDelta := 1
  else
  begin
    FDelta := 0;
    Exit;
  end;
  ParamValueChanged := True;
end;

procedure TIdleHandler.SetParam(const Value: TCircleParam);
begin
  if Value <> FParam then
  begin
    FParam := Value;
    ParamChanged := True;
  end;
end;

procedure TIdleHandler.DoOnIdle;
begin
  if ParamChanged then
  begin
    ParamChanged := False;
    ParamValueChanged := False;
    TextUpdateNeeded := True;
    UpdateParam;
  end;

  if ParamValueChanged then
  begin
    Inc(Counter);
    ParamValueChanged := False;
    TextUpdateNeeded := True;
    GraphUpdateNeeded := True;
    UpdateParamValue;
  end;

  if GraphUpdateNeeded then
  begin
    GraphUpdateNeeded := False;
    UpdateGraph;
  end;

  if TextUpdateNeeded then
  begin
    TextUpdateNeeded := False;
    UpdateText;
  end;
end;

procedure TIdleHandler.UpdateParam;
begin
  Main.Param := FParam;
end;

procedure TIdleHandler.UpdateParamValue;
begin
  Main.DoWheel(delta);
end;

procedure TIdleHandler.UpdateGraph;
begin
end;

procedure TIdleHandler.UpdateText;
begin
  FormMain.UpdateReport;
end;

end.
