unit RiggVar.FZ.Z13_SchnittKK;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  RggSchnittKK,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawingZ13 = class(TRggDrawing)
  private
    Radius: single;
    SchnittKK: TSchnittKK;
  public
    M1: TRggCircle;
    M2: TRggCircle;
    S1: TRggCircle;
    S2: TRggCircle;

    Bem: TRggLabel;
    Watch: TRggLabel;

    C1: TRggBigCircle;
    C2: TRggBigCircle;

    constructor Create;
    destructor Destroy; override;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

uses
  Math;

{ TRggDrawingZ13 }

procedure TRggDrawingZ13.InitDefaultPos;
var
  ox, oy: single;
begin
  ox := 300;
  oy := 400;

  M1.Center.X := ox;
  M1.Center.Y := oy;
  M1.Center.Z := 0;

  M2.Center.X := ox + 200;
  M2.Center.Y := oy;
  M2.Center.Z := 0;
end;

procedure TRggDrawingZ13.Compute;
begin
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := Radius;
  SchnittKK.Radius2 := Radius;
  SchnittKK.MittelPunkt1 := M1.Center.C;
  SchnittKK.MittelPunkt2 := M2.Center.C;
  S1.Center.C := SchnittKK.SchnittPunkt1;
  S2.Center.C := SchnittKK.SchnittPunkt2;

  Bem.Text := SchnittKK.Bemerkung;
  Watch.Text := Format('Watch1 = %d, Watch2 = %d', [SchnittKK.Watch1, SchnittKK.Watch2]);

  C1.Center := M1.Center;
  C2.Center := M2.Center;
end;

constructor TRggDrawingZ13.Create;
begin
  inherited;
  Name := 'Z13-SchnittKK';

  Radius := 150;
  SchnittKK := TSchnittKK.Create;

  { Points }

  M1 := TRggCircle.Create('M1');
  M1.StrokeColor := claRed;

  M2 := TRggCircle.Create('M2');
  M2.StrokeColor := claBlue;

  S1 := TRggCircle.Create('S1');
  S1.StrokeColor := claYellow;
  S1.IsComputed := True;

  S2 := TRggCircle.Create('S2');
  S2.StrokeColor := claLime;
  S2.IsComputed := True;

  InitDefaultPos;

  { Other Elements }

  Bem := TRggLabel.Create;
  Bem.Caption := 'Bemerkung';
  Bem.Text := 'Bemerkung';
  Bem.Position.X := 20;
  Bem.Position.Y := 20;
  Bem.StrokeColor := claTomato;

  Watch := TRggLabel.Create;
  Watch.Caption := 'Watch';
  Watch.Text := 'Watch';
  Watch.Position.X := 20;
  Watch.Position.Y := 55;
  Watch.StrokeColor := claTomato;

  C1 := TRggBigCircle.Create('C1');
  C1.StrokeThickness := 1.0;
  C1.StrokeColor := claPlum;
  C1.Radius := Radius;
  C1.IsComputed := True;
  Add(C1);

  C2 := TRggBigCircle.Create('C2');
  C2.StrokeThickness := 1.0;
  C2.StrokeColor := claDodgerblue;
  C2.Radius := Radius;
  C2.IsComputed := True;
  Add(C2);

  Add(M1);
  Add(M2);
  Add(S1);
  Add(S2);

  Add(Bem);
  Add(Watch);

  FixPoint := M1.Center.C;
  WantRotation := False;
  WantSort := False;

  DefaultElement := M2;
end;

destructor TRggDrawingZ13.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

end.
