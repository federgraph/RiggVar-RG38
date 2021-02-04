unit RiggVar.FZ.Z23_Federgraph;

interface

uses
  System.SysUtils,
  System.Math,
  RiggVar.FederModel.Circle,
  RiggVar.FB.Color,
  RiggVar.FB.Equation,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

{$define WantDiameter}

type
  TRggDrawingZ23 = class(TRggDrawing)
  private
    Raster: single;
    FederModel: TFederPoly;
    WantLC: Boolean;
    function GetHelpText: string;
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
  public
    A: TRggCircle;
    B: TRggCircle;
    C: TRggCircle;
    D: TRggCircle;
    AD: TRggFederLine;
    BD: TRggFederLine;
    CD: TRggFederLine;
    HT: TRggLabel;

    Circle: TRggBigCircle;

{$ifdef WantDiameter}
    D1: TRggCircle;
    D2: TRggCircle;
    Diameter: TRggLine;
{$endif}

    Poly: TRggPolyCurve;

    ParamA: TRggParam;
    ParamR: TRggParam;

    constructor Create;
    destructor Destroy; override;

    procedure InitDefaultPos; override;
    procedure Compute; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
  end;

implementation

{ TRggDrawingZ23 }

procedure TRggDrawingZ23.InitDefaultPos;
var
  ox, oy, oz: single;
  r, t: single;
begin
  ParamA.Scale := 1;
  ParamA.BaseValue := 0;

  ParamR.Scale := 1;
  ParamR.BaseValue := 100;

  ox := 400;
  oy := 400;
  oz := 0;

  r := 200;
  t := r * tan(pi/3) / 2;
  A.Center.X := ox - r;
  A.Center.Y := oy + t;
  A.Center.Z := oz;

  B.Center.X := ox + r;
  B.Center.Y := oy + t;
  B.Center.Z := oz;

  C.Center.X := ox;
  C.Center.Y := oy - t;
  C.Center.Z := oz;

  D.Center.X := ox;
  D.Center.Y := oy;
  D.Center.Z := oz;

  Circle.Center := D.Center;

  FederModel.ParamBahnRadius := 100;
  FederModel.ParamBahnPosition := D.Center.C;
  FederModel.ParamBahnAngle := 0;

{$ifdef WantDiameter}
  D1.Center := D.Center;
  D1.Center.X := D1.Center.X - FederModel.ParamBahnRadius;

  D2.Center := D.Center;
  D2.Center.X := D2.Center.X + FederModel.ParamBahnRadius;
{$endif}
end;

constructor TRggDrawingZ23.Create;
begin
  inherited;
  Name := 'Z23-Federgraph';

  Raster := 25;

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Red;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Green;

  C := TRggCircle.Create('C');
  C.StrokeColor := TRggColors.Blue;

  D := TRggCircle.Create('D');
  D.StrokeColor := TRggColors.Dodgerblue;

  AD := TRggFederLine.Create('AD');
  AD.Point1 := A;
  AD.Point2 := D;
  AD.ShowCaption := False;
  AD.StrokeThickness := 2;
  AD.StrokeColor := TRggColors.Aquamarine;

  BD := TRggFederLine.Create('BD');
  BD.Point1 := B;
  BD.Point2 := D;
  BD.ShowCaption := False;
  BD.StrokeThickness := 2;
  BD.StrokeColor := TRggColors.Aquamarine;

  CD := TRggFederLine.Create('CD');
  CD.Point1 := C;
  CD.Point2 := D;
  CD.ShowCaption := False;
  CD.StrokeThickness := 2;
  CD.StrokeColor := TRggColors.Aquamarine;

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  Add(HT);

  FederModel := TFederPoly.Create;

  Circle := TRggBigCircle.Create('Path');
  Circle.StrokeThickness := 1;
  Circle.StrokeColor := TRggColors.Plum;
  Circle.Radius := FederModel.ParamBahnRadius;
  Circle.IsComputed := True;
  Circle.ShowCaption := False;
  Add(Circle);

{$ifdef WantDiameter}
  D1 := TRggCircle.Create('D1');
  D1.StrokeColor := TRggColors.Dodgerblue;
  D1.ShowCaption := False;
  D1.IndentItem := True;
  Add(D1);

  D2 := TRggCircle.Create('D2');
  D2.StrokeColor := TRggColors.Dodgerblue;
  D2.ShowCaption := False;
  D2.IndentItem := True;
  Add(D2);

  Diameter := TRggLine.Create('Diameter');
  Diameter.Point1 := D1;
  Diameter.Point2 := D2;
  Diameter.ShowCaption := False;
  Diameter.IndentItem := True;
  Diameter.StrokeThickness := 1;
  Diameter.StrokeColor := TRggColors.Dodgerblue;
  Add(Diameter);
{$endif}

  Poly := TRggPolyCurve.Create('Poly', Length(FederModel.LC));
  Poly.Caption := 'Poly';
  Poly.StrokeThickness := 3;
  Poly.StrokeColor := TRggColors.Plum;
  Poly.Opacity := 1.0;
  Add(Poly);

  ParamA := TRggParam.Create;
  ParamA.Caption := 'Probe Angle';
  ParamA.StrokeColor := TRggColors.Teal;
  ParamA.StartPoint.Y := 3 * Raster;
  Add(ParamA);

  ParamR := TRggParam.Create;
  ParamR.Caption := 'Probe Radius';
  ParamR.StrokeColor := TRggColors.Teal;
  ParamR.StartPoint.Y := 5 * Raster;
  Add(ParamR);

  InitDefaultPos;

  Add(A);
  Add(B);
  Add(C);
  Add(D);
  Add(AD);
  Add(BD);
  Add(CD);

  FixPoint3D := D.Center.C;
  WantRotation := False;
  WantSort := False;
  WantLC := True;

  DefaultElement := D;
end;

destructor TRggDrawingZ23.Destroy;
begin
  FederModel.Free;
  inherited;
end;

function TRggDrawingZ23.GetHelpText: string;
begin
  ML.Add('Federgraph Example (2D).');

  result := ML.Text;
  ML.Clear;
end;

procedure TRggDrawingZ23.Compute;
begin
  FederModel.ParamBahnAngle := ParamA.ParamValue;
  FederModel.ParamBahnRadius := ParamR.ParamValue;

  Circle.Center := D.Center;
  Circle.Radius := FederModel.ParamBahnRadius;

  FederModel.EQ.x1 := A.Center.X;
  FederModel.EQ.y1 := A.Center.Y;

  FederModel.EQ.x2 := B.Center.X;
  FederModel.EQ.y2 := B.Center.Y;

  FederModel.EQ.x3 := C.Center.X;
  FederModel.EQ.y3 := C.Center.Y;

  FederModel.ParamBahnPosition := D.Center.C;

  FederModel.Compute;

{$ifdef WantDiameter}
  Diameter.Point1.Center.P := FederModel.D1;
  Diameter.Point2.Center.P := FederModel.D2;
{$endif}

  if WantLC then
  begin
    Poly.AssignPoly(FederModel.LC);
  end
  else
  begin
    Poly.AssignPoly(FederModel.LL);
  end;

  ParamA.Text := Format('Param A = %.2f', [ParamA.ParamValue]);
  ParamR.Text := Format('Param R = %.2f', [ParamR.ParamValue]);
end;

procedure TRggDrawingZ23.InitButtons(BG: TRggButtonGroup);
begin
  { Will only be called if Buttons have been created. }
  inherited; { will call Reset }

  BG.Btn1.OnClick := Btn1Click;
  BG.Btn2.OnClick := Btn2Click;

  BG.Btn1.Text := 'LC';
  BG.Btn2.Text := 'LL';
end;

procedure TRggDrawingZ23.Btn1Click(Sender: TObject);
begin
  WantLC := True;
  Compute;
  TH.Draw;
end;

procedure TRggDrawingZ23.Btn2Click(Sender: TObject);
begin
  WantLC := False;
  Compute;
  TH.Draw;
end;

end.
