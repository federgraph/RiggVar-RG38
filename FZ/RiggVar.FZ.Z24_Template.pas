unit RiggVar.FZ.Z24_Template;

interface

uses
  System.SysUtils,
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ24 = class(TRggDrawing)
  private
    function GetHelpText: string;
    procedure BtnAClick(Sender: TObject);
    procedure BtnBClick(Sender: TObject);
  public
    A: TRggCircle;
    B: TRggCircle;
    HT: TRggLabel;

    Param: TRggParam;


    constructor Create;
    procedure InitDefaultPos; override;
    procedure Compute; override;
    procedure InitButtons(BG: TRggButtonGroup); override;
  end;

implementation

{ TRggDrawingZ24 }

procedure TRggDrawingZ24.InitDefaultPos;
begin
  A.Center.X := 100;
  A.Center.Y := 200;
  A.Center.Z := 0;

  B.Center.X := 400;
  B.Center.Y := 200;
  B.Center.Z := 0;

  Param.ParamValue := 3;
end;

procedure TRggDrawingZ24.InitButtons(BG: TRggButtonGroup);
begin
  inherited;
  BG.BtnA.OnClick := BtnAClick;
  BG.BtnB.OnClick := BtnBClick;

  BG.BtnA.Text := 'A*';
  BG.BtnB.Text := 'B*';
end;

procedure TRggDrawingZ24.BtnAClick(Sender: TObject);
begin
  ML.Text := 'Btn A clicked.';
  UpdateDrawing;
end;

procedure TRggDrawingZ24.BtnBClick(Sender: TObject);
begin
  ML.Text := 'Btn B clicked.';
  UpdateDrawing;
end;

constructor TRggDrawingZ24.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z24-Template';
  WantSort := False;

  { Parameter }

  Param := TRggParam.Create;
  Param.Caption := 'Test';
  Param.StrokeColor := TRggColors.Teal;
  Param.StartPoint.Y := 50;
  Param.BaseValue := 3;
  Param.Scale := 2 / Param.OriginValue;
  Add(Param);

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Tomato;
  HT.IsMemoLabel := True;
  HT.Position.Y := 500;
  Add(HT);

  { Points }

  A := TRggCircle.Create('A');
  A.StrokeColor := TRggColors.Orangered;

  B := TRggCircle.Create('B');
  B.StrokeColor := TRggColors.Blue;

  InitDefaultPos;

  { Lines }

  L := TRggLine.Create('AB');
  L.StrokeColor := TRggColors.Lime;
  L.Point1 := A;
  L.Point2 := B;
  Add(L);

  { Add points last so that they stay in front. }
  Add(A);
  Add(B);

  WantMemoLines := True;
  DefaultElement := Param;
end;

function TRggDrawingZ24.GetHelpText: string;
begin
  ML.Add('First select a drawing element, Circle or Line, and play with Params.');
  ML.Add('');
  ML.Add('Change Param 1 with mouse wheel.');
  ML.Add('Change Param 2 with Shift-Wheel');
  ML.Add('  Make sure the mouse is over the drawing when you wheel.');
  ML.Add('');
  ML.Add('A Circle element can be moved in x and y direction.');
  ML.Add('A Line element maps length and angle to params 1 and 2.');
  ML.Add('');
  ML.Add('Drag a Selected circle with the mouse.');
  ML.Add('');
  ML.Add('Press Esc key to reset the drawing.');
  ML.Add('  For other keyboard shortcuts see drawing Z10-Lager.');
  result := ML.Text;
  ML.Clear;
end;

procedure TRggDrawingZ24.Compute;
begin
  inherited;
  Param.Text := Format('ParamValue = %.2f', [Param.ParamValue]);
end;

end.
