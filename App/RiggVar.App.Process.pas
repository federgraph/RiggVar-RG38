unit RiggVar.App.Process;

interface

uses
  System.Classes,
  FMX.Types,
  RiggVar.RG.Types;

type
  TIdleHandler = class
  private
    FDelta: single;
    FParam: TFederParam;
    procedure SetDelta(const Value: single);
    procedure SetParam(const Value: TFederParam);
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

    property Param: TFederParam read FParam write SetParam;
    property Delta: single read FDelta write SetDelta;
  end;

implementation

uses
  FrmRG03,
  RiggVar.App.Main;

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

procedure TIdleHandler.SetParam(const Value: TFederParam);
begin
  if Value <> FParam then
  begin
    FParam := Value;
    ParamChanged := True;
  end;
end;

procedure TIdleHandler.DoOnIdle;
begin
  { Do one thing at a time? }

  if ParamChanged then
  begin
    ParamChanged := False;
    ParamValueChanged := False;
    TextUpdateNeeded := True;
    UpdateParam;
    Exit;
  end;

  if ParamValueChanged then
  begin
    Inc(Counter);
    ParamValueChanged := False;
    TextUpdateNeeded := True;
    GraphUpdateNeeded := True;
    UpdateParamValue;
    Exit;
  end;

  if TextUpdateNeeded then
  begin
    TextUpdateNeeded := False;
    UpdateText;
    Exit;
  end;

  if GraphUpdateNeeded then
  begin
    GraphUpdateNeeded := False;
    UpdateGraph;
    Exit;
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
  if Main.UseTimedDrawing then
    Main.Draw;
end;

procedure TIdleHandler.UpdateText;
begin
  FormMain.ShowTrimm;
end;

end.
