unit RiggVar.RG.Rota;

interface

{$define UseImage}
{.$define UseViewport}

{$define WantRotaForm1}
{.$define WantRotaForm2}
{.$define WantRotaForm3}

uses
  RggTypes,
  RiggVar.RG.Graph,
{$ifdef WantRotaForm1}
  RggRota,
{$endif}
{$ifdef WantRotaForm2}
  RiggVar.FD.Rota,
{$endif}
{$ifdef WantRotaForm3}
  FMX.Viewport3D,
  RiggVar.App.Rota,
{$endif}
{$ifdef UseImage }
  RiggVar.FD.Image,
{$endif}
  System.UITypes;

type
  TRotaForm = class
  private
    FCurrent: Integer;
    FIsUp: Boolean;
    FUseDisplayList: Boolean;
    FWantLineColors: Boolean;
    FMatrixItemChecked: Boolean;
    FLegendItemChecked: Boolean;
    FWantOverlayedRiggs: Boolean;
    FUseQuickSort: Boolean;
    FBackgroundColor: TAlphaColor;
    FViewPoint: TViewPoint;
    FDarkMode: Boolean;
    procedure SetIsUp(const Value: Boolean);
    procedure SetLegendItemChecked(const Value: Boolean);
    procedure SetMatrixItemChecked(const Value: Boolean);
    procedure SetUseDisplayList(const Value: Boolean);
    procedure SetUseQuickSort(const Value: Boolean);
    procedure SetWantLineColors(const Value: Boolean);
    procedure SetWantOverlayedRiggs(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetViewPoint(const Value: TViewPoint);
    procedure SetDarkMode(const Value: Boolean);
  public
{$ifdef WantRotaForm1}
    RotaForm1: TRotaForm1;
    StrokeRigg1: IStrokeRigg;
{$endif}
{$ifdef WantRotaForm2}
    RotaForm2: TRotaForm2;
    StrokeRigg2: IStrokeRigg;
{$endif}
{$ifdef WantRotaForm3}
    RotaForm3: TRotaForm3;
    StrokeRigg3: IStrokeRigg;
{$endif}

{$ifdef UseImage}
    Image: TOriginalImage;
{$endif}
{$ifdef WantRotaForm3}
    Viewport: TViewport3D;
{$endif}

    constructor Create;
    destructor Destroy; override;

    procedure RotateZ(delta: single);
    procedure Zoom(delta: single);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);

    procedure LegendBtnClick(Sender: TObject);
    procedure UseDisplayListBtnClick(Sender: TObject);
    procedure UseQuickSortBtnClick(Sender: TObject);
    procedure MatrixItemClick(Sender: TObject);

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    procedure SetChecked(fa: Integer; Value: Boolean);

    procedure Init;
    procedure Draw;

    procedure SwapRota(Selected: Integer);
    procedure DoOnIdle;

    property IsUp: Boolean read FIsUp write SetIsUp;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property LegendItemChecked: Boolean read FLegendItemChecked write SetLegendItemChecked;
    property WantLineColors: Boolean read FWantLineColors write SetWantLineColors;
    property UseDisplayList: Boolean read FUseDisplayList write SetUseDisplayList;
    property UseQuickSort: Boolean read FUseQuickSort write SetUseQuickSort;
    property MatrixItemChecked: Boolean read FMatrixItemChecked write SetMatrixItemChecked;
    property WantOverlayedRiggs: Boolean read FWantOverlayedRiggs write SetWantOverlayedRiggs;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property DarkMode: Boolean read FDarkMode write SetDarkMode;
    property Current: Integer read FCurrent;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main;

{ TRotaFormContainer }

constructor TRotaForm.Create;
begin
{$ifdef WantRotaForm1}
  RotaForm1 := TRotaForm1.Create;
  StrokeRigg1 := RotaForm1;
{$endif}

{$ifdef WantRotaForm2}
  RotaForm2 := TRotaForm2.Create;
  StrokeRigg2 := RotaForm2;
{$endif}

{$ifdef WantRotaForm3}
  RotaForm3 := TRotaForm3.Create;
  StrokeRigg3 := RotaForm3;
{$endif}

  FCurrent := 1;
end;

destructor TRotaForm.Destroy;
begin
{$ifdef WantRotaForm3}
  RotaForm3.Frame3D.Camera.FG := nil;
  RotaForm3.Frame3D.FG := nil;
{$endif}

  inherited;
  { RotaForm instances are automatically freed
    when StrokeRigg instances go out of scope. }
end;

procedure TRotaForm.Init;
begin
{$ifdef WantRotaForm1}
  RotaForm1.Image := Image;
  RotaForm1.Init;
{$endif}

{$ifdef WantRotaForm2}
  RotaForm2.Image := Image;
  RotaForm2.Init;
{$endif}

{$ifdef WantRotaForm3}
  RotaForm3.Viewport := Viewport;
  RotaForm3.Init;
{$endif}

{$ifdef WantRotaForm1}
  RotaForm1.ViewPoint := vp3D;
  RotaForm1.ZoomIndex := 8;
  RotaForm1.FixPoint := ooD0;
{$endif}

{$ifdef WantRotaForm2}
  RotaForm2.ViewPoint := vp3D;
  RotaForm2.ZoomIndex := 8;
  RotaForm2.FixPoint := ooD0;
{$endif}

{$ifdef WantRotaForm3}
  RotaForm3.ViewPoint := vpSeite;
//  RotaForm3.ZoomIndex := 8;
  RotaForm3.FixPoint := ooD0;
{$endif}
end;

procedure TRotaForm.Draw;
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.Draw;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.Draw;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Draw;
{$endif}
    else
      ;
  end;
end;

function TRotaForm.GetChecked(fa: Integer): Boolean;
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: result := RotaForm1.GetChecked(fa);
{$endif}
{$ifdef WantRotaForm2}
    2: result := RotaForm2.GetChecked(fa);
{$endif}
{$ifdef WantRotaForm3}
    3: result := RotaForm3.GetChecked(fa);
{$endif}
    else
      result := false;
  end;
end;

procedure TRotaForm.HandleAction(fa: Integer);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
//    1: RotaForm1.HandleAction(fa);
{$endif}
{$ifdef WantRotaForm2}
//    2: RotaForm2.HandleAction(fa);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.HandleAction(fa);
{$endif}
  end;
end;

procedure TRotaForm.LegendBtnClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.LegendBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.LegendBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.MatrixItemClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.MatrixItemClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.MatrixItemClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.RotateZ(delta: single);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.RotateZ(delta * 0.3);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.RotateZ(delta * 0.3);
{$endif}
{$ifdef WantRotaForm3}
    3:
    begin
      RotaForm3.RotateZ(Delta);
      //Main.UpdateText;
    end;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.BackgroundColor := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.BackgroundColor := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.BackgroundColor := Value;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetChecked(fa: Integer; Value: Boolean);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.SetChecked(fa, Value);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.SetChecked(fa, Value);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.DarkMode := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.DarkMode := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.DarkMode := Value;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
{$ifdef WantRotaForm1}
  RotaForm1.IsUp := Value;
{$endif}
{$ifdef WantRotaForm2}
  RotaForm2.IsUp := Value;
{$endif}
{$ifdef WantRotaForm3}
//  RotaForm3.IsUp := Value;
{$endif}
end;

procedure TRotaForm.SetLegendItemChecked(const Value: Boolean);
begin
  FLegendItemChecked := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.LegendItemChecked := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.LegendItemChecked := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetMatrixItemChecked(const Value: Boolean);
begin
  FMatrixItemChecked := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.MatrixItemChecked := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.MatrixItemChecked := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetUseDisplayList(const Value: Boolean);
begin
  FUseDisplayList := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.UseDisplayList := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.UseDisplayList := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetUseQuickSort(const Value: Boolean);
begin
  FUseQuickSort := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.UseQuickSort := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.UseQuickSort := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.ViewPoint := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ViewPoint := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.ViewPoint := Value;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetWantLineColors(const Value: Boolean);
begin
  FWantLineColors := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.WantLineColors := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.WantLineColors := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SetWantOverlayedRiggs(const Value: Boolean);
begin
  FWantOverlayedRiggs := Value;
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.WantOverlayedRiggs := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.WantOverlayedRiggs := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.UseDisplayListBtnClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.UseDisplayListBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.UseDisplayListBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.UseQuickSortBtnClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.UseQuickSortBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.UseQuickSortBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.Zoom(delta: single);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.Zoom(delta);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.Zoom(delta);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Zoom(delta);
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.ZoomInBtnClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.ZoomInBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ZoomInBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.ZoomOutBtnClick(Sender: TObject);
begin
  case FCurrent of
{$ifdef WantRotaForm1}
    1: RotaForm1.ZoomOutBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ZoomOutBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
    else
      ;
  end;
end;

procedure TRotaForm.SwapRota(Selected: Integer);
begin
  case Selected of
{$ifdef WantRotaForm1}
    1:
    begin
      FCurrent := 1;
      Main.StrokeRigg := RotaForm1;
{$ifdef UseViewport}
      Viewport.Visible := False;
{$endif}
      Image.Visible := True;
      RotaForm1.Swap;
      Main.FederText1.Parent := FormMain;
      Main.FederText2.Parent := FormMain;
    end;
{$endif}

{$ifdef WantRotaForm2}
    2:
    begin
      FCurrent := 2;
      Main.StrokeRigg := RotaForm2;
{$ifdef UseViewport}
      Viewport.Visible := False;
{$endif}
      Image.Visible := True;
      RotaForm2.Swap;
      Main.FederText1.Parent := FormMain;
      Main.FederText2.Parent := FormMain;
    end;
{$endif}

{$ifdef WantRotaForm3}
    3:
    begin
      FCurrent := 3;
      Main.StrokeRigg := RotaForm3;
      Viewport.Visible := True;
{$ifdef UseImage}
      Image.Visible := False;
{$endif}
      Main.FederText1.Parent := Viewport;
      Main.FederText2.Parent := Viewport;
      Viewport.SetFocus;
    end;
{$endif}

  end;

  Main.UpdateStrokeRigg;
  Draw;
end;

procedure TRotaForm.DoOnIdle;
begin
{$ifdef WantRotaForm3}
  RotaForm3.Frame3D.DoOnIdle;
{$endif}
end;

end.
