unit RiggVar.FZ.Z21_Rotations;


interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  RiggVar.FB.Color,
  RiggVar.FD.TransformHelper,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ21 = class(TRggDrawing)
  private
    LAX: TRggLine;
    LAY: TRggLine;
    LAZ: TRggLine;
    HT: TRggLabel;
    procedure Btn0Click(Sender: TObject);
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure Btn3Click(Sender: TObject);
    procedure Btn4Click(Sender: TObject);
    procedure Btn5Click(Sender: TObject);
    procedure Btn6Click(Sender: TObject);
    procedure Btn7Click(Sender: TObject);
    procedure Btn8Click(Sender: TObject);
    procedure Btn9Click(Sender: TObject);
    procedure BtnAClick(Sender: TObject);
    procedure BtnBClick(Sender: TObject);
    procedure BtnCClick(Sender: TObject);
    procedure BtnDClick(Sender: TObject);
    procedure BtnEClick(Sender: TObject);
    procedure BtnFClick(Sender: TObject);
    procedure DoGlobalRotation(aRotX, aRotY, aRotZ: single);
    procedure RotaQuo(aRotX, aRotY, aRotZ: single);
    procedure RotaMat(aRotX, aRotY, aRotZ: single);
    procedure RotaHPB(aRotX, aRotY, aRotZ: single);
    procedure RotaYPR(aRotX, aRotY, aRotZ: single);
    function GetHelpText: string;
  public
    Origin: TRggCircle;
    AX: TRggCircle;
    AY: TRggCircle;
    AZ: TRggCircle;
    constructor Create;
    procedure InitDefaultPos; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
    procedure GoDark; override;
    procedure GoLight; override;
  end;

implementation

{ TRggDrawingZ21 }

procedure TRggDrawingZ21.InitDefaultPos;
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

constructor TRggDrawingZ21.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z21-Rotations';

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  Add(HT);

  { Points }

  AX := TRggCircle.Create;
  AX.Caption := 'X';
  AX.StrokeColor := TRggColors.Red;

  AY := TRggCircle.Create;
  AY.Caption := 'Y';
  AY.StrokeColor := TRggColors.Green;

  AZ := TRggCircle.Create;
  AZ.Caption := 'Z';
  AZ.StrokeColor := TRggColors.Blue;

  Origin := TRggCircle.Create;
  Origin.Caption := 'Origin';
  Origin.StrokeColor := TRggColors.Yellow;
  Origin.ShowCaption := False;

  InitDefaultPos;

  { Lines }

  DefaultShowCaption := False;

  L := TRggLine.Create('AX');
  L.StrokeColor := TRggColors.Red;
  L.Point1 := Origin;
  L.Point2 := AX;
  Add(L);
  LAX := L;

  L := TRggLine.Create('AY');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := Origin;
  L.Point2 := AY;
  Add(L);
  LAY := L;

  L := TRggLine.Create('AZ');
  L.StrokeColor := TRggColors.Blue;
  L.Point1 := Origin;
  L.Point2 := AZ;
  Add(L);
  LAZ := L;

  Add(Origin);
  Add(AX);
  Add(AY);
  Add(AZ);

  FixPoint3D := Origin.Center.C;
  WantRotation := True;
  WantSort := True;
end;

procedure TRggDrawingZ21.DoGlobalRotation(aRotX, aRotY, aRotZ: single);
begin
  TH.Rotation.X := DegToRad(aRotX);
  TH.Rotation.Y := DegToRad(aRotY);
  TH.Rotation.Z := DegToRad(aRotZ);
  TH.Draw;
  TH.GetEulerAngles;
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRggDrawingZ21.Btn1Click(Sender: TObject);
begin
  DoGlobalRotation(10, 0, 0);
end;

procedure TRggDrawingZ21.Btn2Click(Sender: TObject);
begin
  DoGlobalRotation(-10, 0, 0);
end;

procedure TRggDrawingZ21.Btn3Click(Sender: TObject);
begin
  DoGlobalRotation(0, 10, 0);
end;

procedure TRggDrawingZ21.Btn4Click(Sender: TObject);
begin
  DoGlobalRotation(0, -10, 0);
end;

procedure TRggDrawingZ21.Btn5Click(Sender: TObject);
begin
  DoGlobalRotation(0, 0, 10);
end;

procedure TRggDrawingZ21.Btn6Click(Sender: TObject);
begin
  DoGlobalRotation(0, 0, -10);
end;

procedure TRggDrawingZ21.Btn7Click(Sender: TObject);
begin
  { UseDarkColorScheme := not UseDarkColorScheme }
  if not IsDark then
    GoDark
  else
    GoLight;
  TH.DrawToCanvas
end;

procedure TRggDrawingZ21.Btn8Click(Sender: TObject);
var
  RotR: TPoint3D;
  mr: TMatrix3D;
begin
  RotR := TH.RotationHelper.EulerAnglesFromMatrix(TH.AccuMatrix);
  TH.Reset;
  TH.Rotation := RotR;

  if WantRotation then
  begin
    mr := TH.RotationHelper.EulerAnglesToMatrix(TH.Rotation.X, TH.Rotation.Y, TH.Rotation.Z);
    TH.BuildMatrix(mr);
    TH.UpdateTransform;
  end;

  TH.DrawToCanvas;
  TH.GetEulerAngles;
  TH.ShowRotation(TH.RotationHelper.RotD(TH.Rotation), False);
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRggDrawingZ21.Btn9Click(Sender: TObject);
var
  RotR: TPoint3D;
  M: TMatrix3D;
begin
  RotR := TH.RotationHelper.GetRotationInfoHPB(TH.AccuMatrix);
  TH.Reset;
  TH.Rotation := RotR;

  if WantRotation then
  begin
    M := TMatrix3D.CreateRotationHeadingPitchBank(TH.Rotation.X, TH.Rotation.Y, TH.Rotation.Z);
    TH.BuildMatrix(M);
    TH.UpdateTransform;
  end;

  TH.DrawToCanvas;
  TH.GetEulerAngles;
  TH.ShowRotation(TH.RotationHelper.RotD(TH.Rotation), False);
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRggDrawingZ21.Btn0Click(Sender: TObject);
var
  RotR: TPoint3D;
  Q: TQuaternion3D;
begin
  Q := TQuaternion3D.Create(TH.AccuMatrix);
  RotR := TH.RotationHelper.EulerAnglesFromQuaternion(Q);
  TH.Reset;
  TH.Rotation := RotR;

  if WantRotation then
  begin
    TH.BuildMatrix(Q);
    TH.UpdateTransform;
  end;

  TH.DrawToCanvas;
  TH.GetEulerAngles;
  TH.ShowRotation(TH.RotationHelper.RotD(TH.Rotation), False);
  TH.Rotation := TPoint3D.Zero;
end;

procedure TRggDrawingZ21.BtnAClick(Sender: TObject);
begin
  RotaMat(10, 20, 30);
end;

procedure TRggDrawingZ21.BtnBClick(Sender: TObject);
begin
  RotaQuo(10, 20, 30);
end;

procedure TRggDrawingZ21.BtnCClick(Sender: TObject);
begin
  RotaHPB(10, 20, 30);
end;

procedure TRggDrawingZ21.BtnDClick(Sender: TObject);
begin
  RotaYPR(10, 20, 30);
end;

procedure TRggDrawingZ21.BtnEClick(Sender: TObject);
begin
  RotaQuo(10, 10, 0);
end;

procedure TRggDrawingZ21.BtnFClick(Sender: TObject);
begin
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.InitButtons(BG: TRggButtonGroup);
begin
  inherited InitButtons(BG);

  BG.Btn1.OnClick := Btn1Click;
  BG.Btn2.OnClick := Btn2Click;
  BG.Btn3.OnClick := Btn3Click;
  BG.Btn4.OnClick := Btn4Click;
  BG.Btn5.OnClick := Btn5Click;
  BG.Btn6.OnClick := Btn6Click;
  BG.Btn7.OnClick := Btn7Click;
  BG.Btn8.OnClick := Btn8Click;
  BG.Btn9.OnClick := Btn9Click;
  BG.Btn0.OnClick := Btn0Click;

  BG.BtnA.OnClick := BtnAClick;
  BG.BtnB.OnClick := BtnBClick;
  BG.BtnC.OnClick := BtnCClick;
  BG.BtnD.OnClick := BtnDClick;
  BG.BtnE.OnClick := BtnEClick;
  BG.BtnF.OnClick := BtnFClick;

  BG.Btn1.Text := '-X';
  BG.Btn2.Text := '+X';

  BG.Btn3.Text := '-Y';
  BG.Btn4.Text := '+Y';

  BG.Btn5.Text := '-Z';
  BG.Btn6.Text := '+Z';

  BG.Btn7.Hint := 'Toggle Color Scheme';
  BG.Btn8.Hint := 'Test EulerAnglesToMatrix';
  BG.Btn9.Hint := 'Test GetRotationInfoHPB';
  BG.Btn0.Hint := 'Test EulerAnglesFromQuaternion';

  BG.BtnA.Hint := 'RotaMat 10-20-30';
  BG.BtnB.Hint := 'RotaQuo 10-20-30';
  BG.BtnC.Hint := 'RotaHPB 10-20-30';
  BG.BtnD.Hint := 'RotaYPR 10-20-30';
  BG.BtnE.Hint := 'RotaQuo 10-10';
  BG.BtnF.Hint := 'TH.GetEulerAngles;';
end;

procedure TRggDrawingZ21.GoLight;
begin
  inherited;
  AX.StrokeColor := TRggColors.Red;
  AY.StrokeColor := TRggColors.Green;
  AZ.StrokeColor := TRggColors.Blue;

  Origin.StrokeColor := TRggColors.Orange;

  LAX.StrokeColor := TRggColors.Red;
  LAY.StrokeColor := TRggColors.Green;
  LAZ.StrokeColor := TRggColors.Blue;
end;

procedure TRggDrawingZ21.GoDark;
begin
  inherited;
  AX.StrokeColor := TRggColors.Orangered;
  AY.StrokeColor := TRggColors.Seagreen;
  AZ.StrokeColor := TRggColors.DodgerBlue;

  Origin.StrokeColor := TRggColors.Goldenrod;

  LAX.StrokeColor := TRggColors.Orangered;
  LAY.StrokeColor := TRggColors.Seagreen;
  LAZ.StrokeColor := TRggColors.Dodgerblue;
end;

procedure TRggDrawingZ21.RotaMat(aRotX, aRotY, aRotZ: single);
var
  x, y, z: single;
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  { Rotation um globale Achsen }

  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  mx := TMatrix3D.CreateRotationX(x);
  my := TMatrix3D.CreateRotationY(y);
  mz := TMatrix3D.CreateRotationZ(z);

  mr := mx * my * mz;

  TH.Reset;
  TH.InitTransform(mr);
  TH.Rotation := TPoint3D.Zero;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.RotaQuo(aRotX, aRotY, aRotZ: single);
var
  x, y, z: single;
  Q: TQuaternion3D;
begin
  { Rotation um lokale Achsen ? }

  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  Q := TQuaternion3D.Identity;
  if x <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirX, x);
  if y <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirY, y);
  if z <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirZ, z);

  TH.Reset;
  TH.InitTransform(Q);
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.RotaHPB(aRotX, aRotY, aRotZ: single);
var
  x, y, z: single;
  mr: TMatrix3D;
begin
  { Rotation um ? }

  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  mr := TMatrix3D.CreateRotationHeadingPitchBank(x, y, z);

  TH.Reset;
  TH.InitTransform(mr);
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.RotaYPR(aRotX, aRotY, aRotZ: single);
var
  x, y, z: single;
  mr: TMatrix3D;
begin
  { Rotation um ? }

  x := DegToRad(aRotX);
  y := DegToRad(aRotY);
  z := DegToRad(aRotZ);

  mr := TMatrix3D.CreateRotationYawPitchRoll(x, y, z);

  TH.Reset;
  TH.InitTransform(mr);
  TH.GetEulerAngles;
end;

function TRggDrawingZ21.GetHelpText: string;
begin
  ML.Add('Rotation Test - for testing out Euler angles.');
  ML.Add('  Hover mouse over buttons 8 and 9 to see hints.');
  ML.Add('');
  ML.Add('Steps:');
  ML.Add('1. Rotate the graph in 3D.');
  ML.Add('2. Click speed button 8 or 9.');
  ML.Add('3. Graph should NOT jump away.');
  ML.Add('     All is good if it does not, see code.');
  ML.Add('');
  ML.Add('Otherwise - work in progress.');

  result := ML.Text;
  ML.Clear;
end;

end.
