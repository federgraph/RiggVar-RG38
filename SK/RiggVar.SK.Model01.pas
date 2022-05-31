unit RiggVar.SK.Model01;

interface

uses
  System.Types,
  FMX.Objects,
  FMX.Graphics,
  RiggVar.RG.Types,
  RiggVar.RG.Calc,
  RiggVar.RG.SchnittGG,
  RiggVar.SK.Graph;

type
  TSKModel01 = class
  private
    SchnittKK: TSchnittKK;
    SchnittGG: TSchnittGGEx;
    M1: TPointF;
    M2: TPointF;
    FR1: single;
    FR2: single;
    FA1: single;
    FA2: single;
    FS1: TPointF;
    FS2: TPointF;
    FS3: TPointF;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetA1(const Value: single);
    procedure SetA2(const Value: single);
    procedure SetR1(const Value: single);
    procedure SetR2(const Value: single);
    procedure SetM1X(const Value: single);
    procedure SetM1Y(const Value: single);
    procedure SetM2X(const Value: single);
    procedure SetM2Y(const Value: single);
    function GetM1X: single;
    function GetM1Y: single;
    function GetM2X: single;
    function GetM2Y: single;
    function GetSP1: TPointF;
    function GetSP2: TPointF;
    function GetSP3: TPointF;
    function GetBemerkungKK: string;
    function GetBemerkungGG: string;
    procedure UpdateModelKK;
    procedure UpdateModelGG;
  private
    PD: TPathData;
  public
    Width: Integer;
    Height: Integer;

    Clock1: TClockImage;
    Clock2: TClockImage;
    S1: TCircle;
    S2: TCircle;
    S3: TCircle;

    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure UpdateWH(AWidth, AHeight: Integer);
    procedure Draw;
    procedure Load;
    procedure ResetCircles;

    property A1: single read FA1 write SetA1;
    property A2: single read FA2 write SetA2;
    property R1: single read FR1 write SetR1;
    property R2: single read FR2 write SetR2;
    property M1X: single read GetM1X write SetM1X;
    property M1Y: single read GetM1Y write SetM1Y;
    property M2X: single read GetM2X write SetM2X;
    property M2Y: single read GetM2Y write SetM2Y;
    property SP1: TPointF read GetSP1;
    property SP2: TPointF read GetSP2;
    property SP3: TPointF read GetSP3;
    property BemerkungKK: string read GetBemerkungKK;
    property BemerkungGG: string read GetBemerkungGG;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

implementation

{ TSKModel01 }

constructor TSKModel01.Create;
begin
  SchnittKK := TSchnittKK.Create;
  SchnittGG := TSchnittGGEx.Create;

  Width := 300;
  Height := 300;

  PD := TPathData.Create;

  Reset;
end;

destructor TSKModel01.Destroy;
begin
  SchnittKK.Free;
  SchnittGG.Free;
  PD.Free;
  inherited;
end;

function TSKModel01.GetBemerkungGG: string;
begin
  result := SchnittGG.Bemerkung;
end;

function TSKModel01.GetBemerkungKK: string;
begin
  result := SchnittKK.Bemerkung;
end;

function TSKModel01.GetM1X: single;
begin
  result := M1.X;
end;

function TSKModel01.GetM1Y: single;
begin
  result := M1.Y;
end;

function TSKModel01.GetM2X: single;
begin
  result := M2.X;
end;

function TSKModel01.GetM2Y: single;
begin
  result := M2.Y;
end;

function TSKModel01.GetSP1: TPointF;
begin
  result := FS1;
end;

function TSKModel01.GetSP2: TPointF;
begin
  result := FS2;
end;

function TSKModel01.GetSP3: TPointF;
begin
  result := FS3;
end;

procedure TSKModel01.SetA1(const Value: single);
begin
  FA1 := Value;
  SchnittGG.Angle1 := Value;
  UpdateModelGG;
end;

procedure TSKModel01.SetA2(const Value: single);
begin
  FA2 := Value;
  SchnittGG.Angle2 := Value;
  UpdateModelGG;
end;

procedure TSKModel01.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSKModel01.SetM1X(const Value: single);
begin
  if (Value > 0) and (Value < Width) then
  begin
    M1.X := Value;
    SchnittKK.M1 := M1;
    SchnittGG.M1 := M1;
    UpdateModelKK;
    UpdateModelGG;
  end;
end;

procedure TSKModel01.SetM1Y(const Value: single);
begin
  if (Value > 0) and (Value < Height) then
  begin
    M1.Y := Value;
    SchnittKK.M1 := M1;
    SchnittGG.M1 := M1;
    UpdateModelKK;
    UpdateModelGG;
  end;
end;

procedure TSKModel01.SetM2X(const Value: single);
begin
  if (Value > 0) and (Value < Width) then
  begin
    M2.X := Value;
    SchnittKK.M2 := M2;
    SchnittGG.M2 := M2;
    UpdateModelKK;
    UpdateModelGG;
  end;
end;

procedure TSKModel01.SetM2Y(const Value: single);
begin
  if (Value > 0) and (Value < Height) then
  begin
    M2.Y := Value;
    SchnittKK.M2 := M2;
    SchnittGG.M2 := M2;
    UpdateModelKK;
    UpdateModelGG;
  end;
end;

procedure TSKModel01.SetR1(const Value: single);
begin
  FR1 := Value;
  SchnittKK.Radius1 := Value;
  UpdateModelKK;
end;

procedure TSKModel01.SetR2(const Value: single);
begin
  FR2 := Value;
  SchnittKK.Radius2 := Value;
  UpdateModelKK;
end;

procedure TSKModel01.UpdateModelKK;
begin
  FS1.X := SchnittKK.SchnittPunkt1.X;
  FS1.Y := SchnittKK.SchnittPunkt1.Y;
  FS2.X := SchnittKK.SchnittPunkt2.X;
  FS2.Y := SchnittKK.SchnittPunkt2.Y;
end;

procedure TSKModel01.UpdateModelGG;
begin
  FS3.X := SchnittGG.SchnittPunkt.X;
  FS3.Y := SchnittGG.SchnittPunkt.Z;
end;

procedure TSKModel01.UpdateWH(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

procedure TSKModel01.Load;
begin
  R1 := Clock1.Radius;
  M1X := Clock1.CenterPoint.X;
  M1Y := Clock1.CenterPoint.Y;

  R2 := Clock2.Radius;
  M2X := Clock2.CenterPoint.X;
  M2Y := Clock2.CenterPoint.Y;

  A1 := -45;
  A2 := 45;

  UpdateModelKK;
  UpdateModelGG;
  Draw;

  if Enabled then
  begin
    S1.Visible := True;
    S2.Visible := True;
    S3.Visible := True;
  end;
end;

procedure TSKModel01.Draw;
var
  r: single;
begin
  if Enabled then
  begin
    Clock1.CenterPoint := TPointF.Create(M1X, M1Y);
    Clock1.Radius := R1;
    Clock1.Angle := A1;
    Clock1.Draw;

    Clock2.CenterPoint := TPointF.Create(M2X, M2Y);
    Clock2.Radius := R2;
    Clock2.Angle := A2;
    Clock2.Draw;

    r := S1.Width / 2;
    S1.Position.X := SP1.X - r;
    S1.Position.Y := SP1.Y - r;

    r := S2.Width / 2;
    S2.Position.X := SP2.X - r;
    S2.Position.Y := SP2.Y - r;

    r := S3.Width / 2;
    S3.Position.X := SP3.X - r;
    S3.Position.Y := SP3.Y - r;
  end;
end;

procedure TSKModel01.Reset;
begin
  M1 := TPointF.Zero;
  M2 := TPointF.Zero;
  FS1 := TPointF.Zero;
  FS2 := TPointF.Zero;

  R1 := 80;
  R2 := 80;

  M1X := 50;
  M1Y := 50;

  M2X := 150;
  M2Y := 150;

  A1 := -45;
  A2 := 45;
end;

procedure TSKModel01.ResetCircles;
begin
  Reset;

  Clock1.CenterPoint := TPointF.Create(176+160, 280+160);
  Clock1.Radius := 160;
  Clock1.Angle := 0;
  Clock1.Elevation := 1;

  Clock2.CenterPoint := TPointF.Create(408+120, 200+120);
  Clock2.Radius := 120;
  Clock2.Angle := -45;
  Clock2.Elevation := 1;

  Load;
end;

end.
