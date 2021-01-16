unit RiggVar.FZ.Z10_Lager;

interface

{$define WantMultipleLists}

uses
  RiggVar.FB.Color,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ10 = class(TRggDrawing)
  private
    function GetHelpText: string;
  public
    Origin: TRggCircle;
    AX: TRggCircle;
    AY: TRggCircle;
    AZ: TRggCircle;
    HT: TRggLabel;
    constructor Create;
    procedure InitDefaultPos; override;
  end;

implementation

{ TRggDrawingZ10 }

procedure TRggDrawingZ10.InitDefaultPos;
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

constructor TRggDrawingZ10.Create;
var
  L: TRggLine;
begin
  inherited;
  Name := 'Z10-Lager';

  { Help Text }

  HT := TRggLabel.Create;
  HT.Caption := 'HelpText';
  HT.Text := GetHelpText;
  HT.StrokeColor := TRggColors.Teal;
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

  L := TRggLagerLine.Create('AX');
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

  FixPoint3D := Origin.Center.C;
  WantRotation := True;
  WantSort := True;
end;

function TRggDrawingZ10.GetHelpText: string;
begin
  ML.Add('Lager = Bearing (Festlager und Loslager),');
  ML.Add('  to be used later - when I will do real examples.');
  ML.Add('');
  ML.Add('Meanwhile, I have some Keyboard shortcuts from FormDrawing:');
  ML.Add('');
  ML.Add('case Key of');
  ML.Add('  vkEscape: DoReset;');
  ML.Add('  vkF11: SwapLayout;');
{$ifdef WantMultipleLists}
  ML.Add('  vkF6 : SwapDrawingLists;');
{$endif}
  ML.Add('  vkF3 : SwapThickLines;');
  ML.Add('  vkF1 : ShowInfo;');
  ML.Add('  vkF2 : SwapColorScheme;');
  ML.Add('');
  ML.Add('case KeyChar of');
  ML.Add('  C: SwapColorScheme;');
  ML.Add('  G: GlobalShowCaptionBtnClick(nil);');
  ML.Add('  R: DoReset;');
{$ifdef WantMultipleLists}
  ML.Add('  L: SwapDrawingLists;');
{$endif}
  ML.Add('  T: SwapLayout;');
  ML.Add('  W: SwapThickLines;');
  ML.Add('');
  ML.Add('  c: CodeBtnClick(nil);');
  ML.Add('  h: UpdateLayout(True);');
  ML.Add('  i: ShowInfo;');
  ML.Add('  t: ToggleShowCaptionBtnClick(nil);');
  ML.Add('  v: UpdateLayout(False);');
  result := ML.Text;
  ML.Clear;
end;

end.
