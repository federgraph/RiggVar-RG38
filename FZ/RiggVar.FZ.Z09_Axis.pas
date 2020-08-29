unit RiggVar.FZ.Z09_Axis;

interface

uses
  System.UIConsts,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ09 = class(TRggDrawing)
  private
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure Btn3Click(Sender: TObject);
    procedure Btn4Click(Sender: TObject);
    procedure Btn5Click(Sender: TObject);
    procedure Btn6Click(Sender: TObject);
  public
    Origin: TRggCircle;
    AX: TRggCircle;
    AY: TRggCircle;
    AZ: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
  end;

implementation

uses
  FrmDrawing;

{ TRggDrawingZ09 }

procedure TRggDrawingZ09.InitButtons(BG: TRggButtonGroup);
begin
  { Will only be called if Buttons have been created. }
  inherited; { will call Reset }

  BG.Btn1.OnClick := Btn1Click;
  BG.Btn2.OnClick := Btn2Click;
  BG.Btn3.OnClick := Btn3Click;
  BG.Btn4.OnClick := Btn4Click;
  BG.Btn5.OnClick := Btn5Click;
  BG.Btn6.OnClick := Btn6Click;

  BG.Btn1.Text := '-X';
  BG.Btn2.Text := '+X';

  BG.Btn3.Text := '-Y';
  BG.Btn4.Text := '+Y';

  BG.Btn5.Text := '-Z';
  BG.Btn6.Text := '+Z';
end;

procedure TRggDrawingZ09.InitDefaultPos;
var
  ox, oy, oz: single;
begin
  ox := 400;
  oy := 400;
  oz := 0;

  Origin.Center.X := ox;
  Origin.Center.Y := oy;
  Origin.Center.Z := 0;

  AX.Center.X := ox + 200;
  AX.Center.Y := oy;
  AX.Center.Z := oz;

  AY.Center.X := ox;
  AY.Center.Y := oy + 200;
  AY.Center.Z := oz;

  AZ.Center.X := ox;
  AZ.Center.Y := oy;
  AZ.Center.Z := oz + 200;
end;

constructor TRggDrawingZ09.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z09-Axis';

  { Points }

  AX := TRggCircle.Create;
  AX.Caption := 'X';
  AX.StrokeColor := claRed;

  AY := TRggCircle.Create;
  AY.Caption := 'Y';
  AY.StrokeColor := claGreen;

  AZ := TRggCircle.Create;
  AZ.Caption := 'Z';
  AZ.StrokeColor := claBlue;

  Origin := TRggCircle.Create;
  Origin.Caption := 'Origin';
  Origin.StrokeColor := claYellow;
  Origin.ShowCaption := False;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AX');
  L.StrokeColor := claRed;
  L.Point1 := Origin;
  L.Point2 := AX;
  Add(L);

  L := TRggLine.Create('AY');
  L.StrokeColor := claGreen;
  L.Point1 := Origin;
  L.Point2 := AY;
  Add(L);

  L := TRggLine.Create('AZ');
  L.StrokeColor := claBlue;
  L.Point1 := Origin;
  L.Point2 := AZ;
  Add(L);

  Add(Origin);
  Add(AX);
  Add(AY);
  Add(AZ);

  FixPoint := Origin.Center.C;
  WantRotation := True;
  WantSort := True;
end;

procedure TRggDrawingZ09.Btn1Click(Sender: TObject);
begin
  TH.Rotation.X := 10 * PI / 180;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ09.Btn2Click(Sender: TObject);
begin
  TH.Rotation.X := -10 * PI / 180;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ09.Btn3Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 10 * PI / 180;
  TH.Rotation.Z:= 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ09.Btn4Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := -10 * PI / 180;
  TH.Rotation.Z:= 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ09.Btn5Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 10 * PI / 180;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ09.Btn6Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := -10 * PI / 180;
  TH.Draw;
  TH.GetEulerAngles;
end;

end.
