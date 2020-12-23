unit RiggVar.RG.Rota;

interface

{$define WantRotaForm1}
{$define WantRotaForm2}
{.$define WantRotaForm3}

{$if defined(WantRotaForm1) or defined(WantRotaForm2) }
{$define UseImage}
{$endif}

{$ifdef WantRotaForm3}
{$define UseViewport}
{$endif}

uses
  RiggVar.RG.Types,
  RiggVar.RG.Graph,
{$ifdef WantRotaForm1}
  RiggVar.Graph1.Rota,
{$endif}
{$ifdef WantRotaForm2}
  RiggVar.FD.Rota,
{$endif}
{$ifdef WantRotaForm3}
  FMX.Viewport3D,
  RiggVar.FG.Rota,
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
    function GetUseDisplayList: Boolean;
    function GetLegendItemChecked: Boolean;
    function GetMatrixItemChecked: Boolean;
    function GetUseQuickSort: Boolean;
    function GetWantLineColors: Boolean;
    function GetWantOverlayedRiggs: Boolean;
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
    procedure InitPosition(w, h, x, y: single);
    procedure Draw;

    procedure SwapRota(Selected: Integer);
    procedure DoOnIdle;
    procedure DoOnResizeEnd;

    property IsUp: Boolean read FIsUp write SetIsUp;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property LegendItemChecked: Boolean read GetLegendItemChecked write SetLegendItemChecked;
    property WantLineColors: Boolean read GetWantLineColors write SetWantLineColors;
    property UseDisplayList: Boolean read GetUseDisplayList write SetUseDisplayList;
    property UseQuickSort: Boolean read GetUseQuickSort write SetUseQuickSort;
    property MatrixItemChecked: Boolean read GetMatrixItemChecked write SetMatrixItemChecked;
    property WantOverlayedRiggs: Boolean read GetWantOverlayedRiggs write SetWantOverlayedRiggs;
    property ViewPoint: TViewPoint read FViewPoint write SetViewPoint;
    property DarkMode: Boolean read FDarkMode write SetDarkMode;
    property Current: Integer read FCurrent;
  end;

implementation

uses
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
  RotaForm2.FixPoint := ooD;
{$endif}

{$ifdef WantRotaForm3}
  RotaForm3.ViewPoint := vp3D;
  RotaForm3.FixPoint := ooD;
{$endif}
end;

procedure TRotaForm.InitPosition(w, h, x, y: single);
begin
{$ifdef WantRotaForm1}
  RotaForm1.InitPosition(w, h, x, y);
{$endif}

{$ifdef WantRotaForm2}
  RotaForm2.InitPosition(w, h, x, y);
{$endif}

{$ifdef WantRotaForm3}
  RotaForm3.InitPosition(w, h, x, y);
{$endif}
end;

procedure TRotaForm.Draw;
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.Draw;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.Draw;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Draw;
{$endif}
  end;
end;

function TRotaForm.GetChecked(fa: Integer): Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.GetChecked(fa);
{$endif}
{$ifdef WantRotaForm2}
    2: result := RotaForm2.GetChecked(fa);
{$endif}
{$ifdef WantRotaForm3}
    3: result := RotaForm3.GetChecked(fa);
{$endif}
  end;
end;

procedure TRotaForm.HandleAction(fa: Integer);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.HandleAction(fa);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.HandleAction(fa);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.HandleAction(fa);
{$endif}
  end;
end;

procedure TRotaForm.LegendBtnClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.LegendBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.MatrixItemClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.MatrixItemClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.RotateZ(delta: single);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.RotateZ(delta);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.RotateZ(delta);
{$endif}
{$ifdef WantRotaForm3}
    3:
    begin
      RotaForm3.RotateZ(Delta);
      //Main.UpdateText;
    end;
{$endif}
  end;
end;

procedure TRotaForm.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.BackgroundColor := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.BackgroundColor := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.BackgroundColor := Value;
{$endif}
  end;
end;

procedure TRotaForm.SetChecked(fa: Integer; Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.SetChecked(fa, Value);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.SetChecked(fa, Value);
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.SetDarkMode(const Value: Boolean);
begin
  FDarkMode := Value;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.DarkMode := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.DarkMode := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.DarkMode := Value;
{$endif}
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
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.LegendItemChecked := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetLegendItemChecked: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.LegendItemChecked;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: result := False;
{$endif}
  end;
end;

procedure TRotaForm.SetMatrixItemChecked(const Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.MatrixItemChecked := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetMatrixItemChecked: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.MatrixItemChecked;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: result := False;
{$endif}
  end;
end;
procedure TRotaForm.SetUseDisplayList(const Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.UseDisplayList := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetUseDisplayList: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.UseDisplayList;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.SetUseQuickSort(const Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.UseQuickSort := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetUseQuickSort: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.UseQuickSort;
{$endif}
{$ifdef WantRotaForm2}
    2: result := False;
{$endif}
{$ifdef WantRotaForm3}
    3: result := False;
{$endif}
  end;
end;

procedure TRotaForm.SetViewPoint(const Value: TViewPoint);
begin
  FViewPoint := Value;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.ViewPoint := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ViewPoint := Value;
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.ViewPoint := Value;
{$endif}
  end;
end;

procedure TRotaForm.SetWantLineColors(const Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.WantLineColors := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetWantLineColors: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.WantLineColors;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.SetWantOverlayedRiggs(const Value: Boolean);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.WantOverlayedRiggs := Value;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

function TRotaForm.GetWantOverlayedRiggs: Boolean;
begin
  result := False;
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: result := RotaForm1.WantOverlayedRiggs;
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.UseDisplayListBtnClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.UseDisplayListBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.UseQuickSortBtnClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.UseQuickSortBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: ;
{$endif}
{$ifdef WantRotaForm3}
    3: ;
{$endif}
  end;
end;

procedure TRotaForm.Zoom(delta: single);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.Zoom(delta);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.Zoom(delta);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Zoom(delta);
{$endif}
  end;
end;

procedure TRotaForm.ZoomInBtnClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.ZoomInBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ZoomInBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Zoom(1.0);
{$endif}
  end;
end;

procedure TRotaForm.ZoomOutBtnClick(Sender: TObject);
begin
  case FCurrent of
    0: ;
{$ifdef WantRotaForm1}
    1: RotaForm1.ZoomOutBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm2}
    2: RotaForm2.ZoomOutBtnClick(Sender);
{$endif}
{$ifdef WantRotaForm3}
    3: RotaForm3.Zoom(-1.0);
{$endif}
  end;
end;

procedure TRotaForm.SwapRota(Selected: Integer);
begin
  case Selected of
    0: ;
{$ifdef WantRotaForm1}
    1:
    begin
      FCurrent := 1;
      Main.CurrentRotaForm := 1;
      Main.StrokeRigg := RotaForm1;
{$ifdef UseViewport}
      Viewport.Visible := False;
{$endif}
      Image.Visible := True;
      RotaForm1.Swap;
      Main.FederTextUpdateParent;
      Main.UpdateStrokeRigg;
      RotaForm1.FixPoint := Main.FixPoint;
    end;
{$endif}

{$ifdef WantRotaForm2}
    2:
    begin
      FCurrent := 2;
      Main.CurrentRotaForm := 2;
      Main.StrokeRigg := RotaForm2;
{$ifdef UseViewport}
      Viewport.Visible := False;
{$endif}
      Image.Visible := True;
      RotaForm2.Swap;
      Main.FederTextUpdateParent;
      Main.UpdateStrokeRigg;
      RotaForm2.FixPoint := Main.FixPoint;
    end;
{$endif}

{$ifdef WantRotaForm3}
    3:
    begin
      FCurrent := 3;
      Main.CurrentRotaForm := 3;
      Main.StrokeRigg := RotaForm3;
      Viewport.Visible := True;
{$ifdef UseImage}
      Image.Visible := False;
{$endif}
      Main.FederText1.Parent := Viewport;
      Main.FederText2.Parent := Viewport;
      Main.UpdateStrokeRigg;
      RotaForm3.FixPoint := Main.FixPoint;
      Viewport.SetFocus;
    end;
{$endif}

  end;
end;

procedure TRotaForm.DoOnIdle;
begin
{$ifdef WantRotaForm3}
  RotaForm3.DoOnIdle;
{$endif}
end;

procedure TRotaForm.DoOnResizeEnd;
begin
{$ifdef WantRotaForm3}
  RotaForm3.DoOnResize;
{$endif}
end;

end.
