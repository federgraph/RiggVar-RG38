unit RiggVar.FZ.Z13_SchnittKK;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.Generics.Collections,
  System.UITypes,
  RggSchnittKK,
  FMX.Graphics,
  RiggVar.FD.Elements,
  RiggVar.FD.Drawings;

type
  TRggDrawing13 = class(TRggDrawing)
  private
    SchnittKK: TSchnittKK;
  public
    M1: TRggCircle;
    M2: TRggCircle;
    S1: TRggCircle;
    S2: TRggCircle;
    Bem: TRggLabel;
    constructor Create;
    destructor Destroy; override;
    procedure InitDefaultPos; override;
    procedure Compute; override;
  end;

implementation

uses
  Math;

{ TRggDrawing13 }

procedure TRggDrawing13.InitDefaultPos;
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

procedure TRggDrawing13.Compute;
begin
  SchnittKK.SchnittEbene := seXY;
  SchnittKK.Radius1 := 150;
  SchnittKK.Radius2 := 150;
  SchnittKK.MittelPunkt1 := M1.Center.C;
  SchnittKK.MittelPunkt2 := M2.Center.C;
  S1.Center.C := SchnittKK.SchnittPunkt1;
  S2.Center.C := SchnittKK.SchnittPunkt2;

  Bem.Text := SchnittKK.Bemerkung;
end;

constructor TRggDrawing13.Create;
begin
  inherited;
  Name := '13-SchnittKK';

  SchnittKK := TSchnittKK.Create;

  { Points }

  M1 := TRggCircle.Create('M1');
  M1.StrokeColor := claRed;

  M2 := TRggCircle.Create('M2');
  M2.StrokeColor := claBlue;

  S1 := TRggCircle.Create('S1');
  S1.StrokeColor := claYellow;

  S2 := TRggCircle.Create('S2');
  S2.StrokeColor := claLime;

  InitDefaultPos;

  { Other Elements }

  Bem := TRggLabel.Create;
  Bem.Caption := 'Bemerkung';
  Bem.Text := 'Bemerkung';
  Bem.Position.X := 100;
  Bem.Position.X := 50;
  Bem.StrokeColor := TAlphaColors.Tomato;

  Add(M1);
  Add(M2);
  Add(S1);
  Add(S2);

  Add(Bem);

  FixPoint := M1.Center.C;
  WantRotation := False;
  WantSort := False;
end;

destructor TRggDrawing13.Destroy;
begin
  SchnittKK.Free;
  inherited;
end;

end.
