unit RiggVar.FZ.Z21_Rotations;


interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
//  RiggVar.FD.RotationHelper,
  RiggVar.FB.Color,
  RiggVar.FD.TransformHelper,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ21 = class(TRggDrawing)
  private
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

  L := TRggLine.Create('AY');
  L.StrokeColor := TRggColors.Green;
  L.Point1 := Origin;
  L.Point2 := AY;
  Add(L);

  L := TRggLine.Create('AZ');
  L.StrokeColor := TRggColors.Blue;
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

procedure TRggDrawingZ21.Btn1Click(Sender: TObject);
begin
  TH.Rotation.X := 10 * PI / 180;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn2Click(Sender: TObject);
begin
  TH.Rotation.X := -10 * PI / 180;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn3Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 10 * PI / 180;
  TH.Rotation.Z:= 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn4Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := -10 * PI / 180;
  TH.Rotation.Z:= 0;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn5Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := 10 * PI / 180;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn6Click(Sender: TObject);
begin
  TH.Rotation.X := 0;
  TH.Rotation.Y := 0;
  TH.Rotation.Z := -10 * PI / 180;
  TH.Draw;
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn7Click(Sender: TObject);
begin
//  FormDrawing.SwapThickLines;
end;

procedure TRggDrawingZ21.Btn8Click(Sender: TObject);
//var
//  RotR: TPoint3D;
//  mr: TMatrix3D;
begin
//  RotR := TH.RotationHelper.EulerAnglesFromMatrix(TH.AccuMatrix);
//  TH.Reset;
//  TH.Rotation := RotR;
//
//  if WantRotation then
//  begin
//    mr := TH.RotationHelper.EulerAnglesToMatrix(TH.Rotation.X, TH.Rotation.Y, TH.Rotation.Z);
//    TH.BuildMatrix(mr);
//    TH.UpdateTransform;
//  end;
//
//  TH.DrawToCanvas;
//  TH.ShowRotation(TH.Rotation, False);
//  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn9Click(Sender: TObject);
//var
//  RotR: TPoint3D;
//  M: TMatrix3D;
begin
//  RotR := TH.RotationHelper.GetRotationInfoHPB(TH.AccuMatrix);
//  TH.Reset;
//  TH.Rotation := RotR;
//
//  if WantRotation then
//  begin
//    M := TMatrix3D.CreateRotationHeadingPitchBank(TH.Rotation.X, TH.Rotation.Y, TH.Rotation.Z);
//    TH.BuildMatrix(M);
//    TH.UpdateTransform;
//  end;
//
//  TH.DrawToCanvas;
//  TH.ShowRotation(TH.Rotation, False);
//  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.Btn0Click(Sender: TObject);
//var
//  RotR: TPoint3D;
//  Q: TQuaternion3D;
begin
//  Q := TQuaternion3D.Create(TH.AccuMatrix);
//  RotR := TH.RotationHelper.EulerAnglesFromQuaternion(Q);
//  TH.Reset;
//  TH.Rotation := RotR;
//
//  if WantRotation then
//  begin
//    TH.BuildMatrix(Q);
//    TH.UpdateTransform;
//  end;
//
//  TH.DrawToCanvas;
//  TH.ShowRotation(TH.Rotation, False);
//  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.BtnAClick(Sender: TObject);
var
  x, y, z: single;
  mx, my, mz: TMatrix3D;
  mr: TMatrix3D;
begin
  { Rotation um globale Achsen }

  x := DegToRad(10);
  y := DegToRad(20);
  z := DegToRad(30);

  mx := TMatrix3D.CreateRotationX(x);
  my := TMatrix3D.CreateRotationY(y);
  mz := TMatrix3D.CreateRotationZ(z);

  mr := mx * my * mz;

  TH.Reset;
  TH.InitTransform(mr);
end;

procedure TRggDrawingZ21.BtnBClick(Sender: TObject);
var
  x, y, z: single;
  Q: TQuaternion3D;
begin
  { Rotation um lokale Achsen ? }

  x := DegToRad(10);
  y := DegToRad(20);
  z := DegToRad(30);

  Q := TQuaternion3D.Identity;
  if x <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirX, x);
  if y <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirY, y);
  if z <> 0 then
    Q := Q * TQuaternion3D.Create(TH.DirZ, z);

  TH.Reset;
  TH.InitTransform(Q);
end;

procedure TRggDrawingZ21.BtnCClick(Sender: TObject);
var
  x, y, z: single;
  mr: TMatrix3D;
begin
  { Rotation um ? }

  x := DegToRad(10);
  y := DegToRad(20);
  z := DegToRad(30);

  mr := TMatrix3D.CreateRotationHeadingPitchBank(x, y, z);

  TH.Reset;
  TH.InitTransform(mr);
end;

procedure TRggDrawingZ21.BtnDClick(Sender: TObject);
var
  x, y, z: single;
  mr: TMatrix3D;
begin
  { Rotation um ? }

  x := DegToRad(10);
  y := DegToRad(20);
  z := DegToRad(30);

  mr := TMatrix3D.CreateRotationYawPitchRoll(x, y, z);

  TH.Reset;
  TH.InitTransform(mr);
end;

procedure TRggDrawingZ21.BtnEClick(Sender: TObject);
begin
  TH.GetEulerAngles;
end;

procedure TRggDrawingZ21.BtnFClick(Sender: TObject);
begin
//  FormDrawing.ShowInfo;
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
end;

end.
