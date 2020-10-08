unit RiggVar.RG.Rota;

interface

uses
  System.UITypes,
  RggTypes,
  RiggVar.RG.Graph,
  RggRota,
//  RiggVar.FD.Rota,
  RiggVar.FD.Image;

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
    RotaForm1: TRotaForm1;
//    RotaForm2: TRotaForm2;
    StrokeRigg1: IStrokeRigg;
//    StrokeRigg2: IStrokeRigg;

    Image: TOriginalImage;

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

    function GetChecked(fa: Integer): Boolean;
    procedure SetChecked(fa: Integer; Value: Boolean);

    procedure Init;
    procedure Draw;

    procedure SwapRota(Selected: Integer);

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
  end;

implementation

uses
  RiggVar.App.Main;

{ TRotaFormContainer }

constructor TRotaForm.Create;
begin
  RotaForm1 := TRotaForm1.Create;
//  RotaForm2 := TRotaForm2.Create;
  StrokeRigg1 := RotaForm1;
//  StrokeRigg2 := RotaForm2;

  FCurrent := 1;
end;

destructor TRotaForm.Destroy;
begin
  inherited;
  { RotaForm instances are automatically freed
    when StrokeRigg instances go out of scope. }
end;

procedure TRotaForm.Init;
begin
  RotaForm1.Image := Image;
  RotaForm1.Init;

//  RotaForm2.Image := Image;
//  RotaForm2.Init;

  case FCurrent of
    1: RotaForm1.Swap;
//    2: RotaForm2.Swap;
    else
      ;
  end;

  RotaForm1.ViewPoint := vp3D;
  RotaForm1.ZoomIndex := 8;
  RotaForm1.FixPoint := ooD0;

//  RotaForm2.ViewPoint := vp3D;
//  RotaForm2.ZoomIndex := 8;
//  RotaForm2.FixPoint := ooD0;
end;

procedure TRotaForm.Draw;
begin
  case FCurrent of
    1: RotaForm1.Draw;
//    2: RotaForm2.Draw;
    else
      ;
  end;
end;

function TRotaForm.GetChecked(fa: Integer): Boolean;
begin
  case FCurrent of
    1: result := RotaForm1.GetChecked(fa);
//    2: result := RotaForm2.GetChecked(fa);
    else
      result := false;
  end;
end;

procedure TRotaForm.LegendBtnClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.LegendBtnClick(Sender);
//    2: RotaForm2.LegendBtnClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.MatrixItemClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.MatrixItemClick(Sender);
//    2: RotaForm2.MatrixItemClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.RotateZ(delta: single);
begin
  case FCurrent of
    1: RotaForm1.RotateZ(delta);
//    2: RotaForm2.RotateZ(delta);
    else
      ;
  end;
end;

procedure TRotaForm.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  case FCurrent of
    1: RotaForm1.BackgroundColor := Value;
//    2: RotaForm2.BackgroundColor := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetChecked(fa: Integer; Value: Boolean);
begin
  case FCurrent of
    1: RotaForm1.SetChecked(fa, Value);
//    2: RotaForm2.SetChecked(fa, Value);
    else
      ;
  end;
end;

procedure TRotaForm.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  case FCurrent of
    1: RotaForm1.DarkMode := Value;
//    2: RotaForm2.DarkMode := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
  RotaForm1.IsUp := Value;
//  RotaForm2.IsUp := Value;
end;

procedure TRotaForm.SetLegendItemChecked(const Value: Boolean);
begin
  FLegendItemChecked := Value;
  case FCurrent of
    1: RotaForm1.LegendItemChecked := Value;
//    2: RotaForm2.LegendItemChecked := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetMatrixItemChecked(const Value: Boolean);
begin
  FMatrixItemChecked := Value;
  case FCurrent of
    1: RotaForm1.MatrixItemChecked := Value;
//    2: RotaForm2.MatrixItemChecked := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetUseDisplayList(const Value: Boolean);
begin
  FUseDisplayList := Value;
  case FCurrent of
    1: RotaForm1.UseDisplayList := Value;
//    2: RotaForm2.UseDisplayList := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetUseQuickSort(const Value: Boolean);
begin
  FUseQuickSort := Value;
  case FCurrent of
    1: RotaForm1.UseQuickSort := Value;
//    2: RotaForm2.UseQuickSort := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  case FCurrent of
    1: RotaForm1.ViewPoint := Value;
//    2: RotaForm2.ViewPoint := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetWantLineColors(const Value: Boolean);
begin
  FWantLineColors := Value;
  case FCurrent of
    1: RotaForm1.WantLineColors := Value;
//    2: RotaForm2.WantLineColors := Value;
    else
      ;
  end;
end;

procedure TRotaForm.SetWantOverlayedRiggs(const Value: Boolean);
begin
  FWantOverlayedRiggs := Value;
  case FCurrent of
    1: RotaForm1.WantOverlayedRiggs := Value;
//    2: RotaForm2.WantOverlayedRiggs := Value;
    else
      ;
  end;
end;

procedure TRotaForm.UseDisplayListBtnClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.UseDisplayListBtnClick(Sender);
//    2: RotaForm2.UseDisplayListBtnClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.UseQuickSortBtnClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.UseQuickSortBtnClick(Sender);
//    2: RotaForm2.UseQuickSortBtnClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.Zoom(delta: single);
begin
  case FCurrent of
    1: RotaForm1.Zoom(delta);
//    2: RotaForm2.Zoom(delta);
    else
      ;
  end;
end;

procedure TRotaForm.ZoomInBtnClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.ZoomInBtnClick(Sender);
//    2: RotaForm2.ZoomInBtnClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.ZoomOutBtnClick(Sender: TObject);
begin
  case FCurrent of
    1: RotaForm1.ZoomOutBtnClick(Sender);
//    2: RotaForm2.ZoomOutBtnClick(Sender);
    else
      ;
  end;
end;

procedure TRotaForm.SwapRota(Selected: Integer);
begin
  case Selected of
    1:
    begin
      FCurrent := 1;
      Main.StrokeRigg := RotaForm1;
      RotaForm1.Swap;
    end;

//    2:
//    begin
//      FCurrent := 2;
//      Main.StrokeRigg := RotaForm2;
//      RotaForm2.Swap;
//    end;
  end;
end;

end.
