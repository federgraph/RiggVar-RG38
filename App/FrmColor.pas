unit FrmColor;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  RiggVar.FB.Color,
  RiggVar.FB.ColorListBox,
  RiggVar.FB.ColorListBoxWeb,
  RiggVar.FB.ListboxPatcher,
  FMX.Colors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.StdCtrls;

type
  TFormColor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    ML: TStrings;
    FColor: TRggColor;
    FColorGroup: TRggColorGroup;
    Text: TText;
    Margin: Integer;
    ColorListBox: TRggColorListBox;
    WebColorListBox: TRggWebColorListBox;
    Rectangle: TRectangle;
    CurrentGroup: TRggColorGroup;
    procedure ColorListBoxChange(Sender: TObject);
    procedure WebColorListBoxChange(Sender: TObject);
    procedure UpdateText;
  end;

var
  FormColor: TFormColor;

implementation

{$R *.fmx}

procedure TFormColor.FormCreate(Sender: TObject);
begin
  Margin := 10;
  FColor := TRggColors.Windowgray;
  FColorGroup := TRggColorGroup.CombinedGroup;

  Caption := 'TRggColorListbox Test';

  TListBoxPatcher.PatchListBox;

  ML := TStringList.Create;

  ColorListBox := TRggColorListBox.Create(Self);
  ColorListBox.Parent := Self;
  ColorListBox.Align := TAlignLayout.Left;
  ColorListBox.OnChange := ColorListBoxChange;

  WebColorListBox := TRggWebColorListBox.Create(Self);
  WebColorListBox.Parent := Self;
  WebColorListBox.Align := TAlignLayout.Right;
  WebColorListBox.OnChange := WebColorListBoxChange;
  WebColorListBox.ColorGroup := OrangeGroup;

  Rectangle := TRectangle.Create(Self);
  Rectangle.Parent := Self;
  Rectangle.Position.X := ColorListBox.Position.X + ColorListBox.Width + Margin;
  Rectangle.Position.Y := Margin;
  Rectangle.Width := 145;
  Rectangle.Height := 145;

  Text := TText.Create(Self);
  Text.Parent := Self;
  Text.TextSettings.Font.Size := 16;
  Text.TextSettings.Font.Family := 'Consolas';
  Text.TextSettings.FontColor := TAlphaColors.Dodgerblue;
  Text.TextSettings.HorzAlign := TTextAlign.Leading;
  Text.WordWrap := False;
  Text.AutoSize := True;
  Text.Text := 'Text';
  Text.Position.X := Rectangle.Position.X;
  Text.Position.Y := Rectangle.Position.Y + Rectangle.Height + Margin;

  UpdateText;
end;

procedure TFormColor.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormColor.ColorListBoxChange(Sender: TObject);
var
  c: TRggColor;
  g: TRggColorGroup;
begin
  c := ColorListBox.Color;
  Rectangle.Fill.Color := c;

  g := TRggColorPool.ColorToGroup(c);
  if g <> CurrentGroup then
  begin
    CurrentGroup := g;
    WebColorListbox.ColorGroup := g;
    Caption := TRggColorPool.ColorGroupToGroupName(g);
  end;

  WebColorListbox.Color := c;

  FColor := c;
  FColorGroup := g;
  UpdateText;
end;

procedure TFormColor.WebColorListBoxChange(Sender: TObject);
begin
  FColor := WebColorListBox.Color;
  Rectangle.Fill.Color := FColor;
  UpdateText;
end;

procedure TFormColor.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case KeyChar of
    'a': WebColorListBox.ColorGroup := CombinedGroup;

    'b': WebColorListBox.ColorGroup := BrownGroup;
    'c': WebColorListBox.ColorGroup := CyanGroup;
    'g': WebColorListBox.ColorGroup := GrayGroup;
    'o': WebColorListBox.ColorGroup := OrangeGroup;
    'p': WebColorListBox.ColorGroup := PinkGroup;
    'r': WebColorListBox.ColorGroup := RedGroup;
    'w': WebColorListBox.ColorGroup := WhiteGroup;
    'y': WebColorListBox.ColorGroup := YellowGroup;

    'B': WebColorListBox.ColorGroup := BlueGroup;
    'G': WebColorListBox.ColorGroup := GreenGroup;
    'P': WebColorListBox.ColorGroup := PurpleGroup;
  end;
end;

procedure TFormColor.UpdateText;
var
  cr: TAlphaColorRec;
begin
  cr.Color := FColor;

  ML.Clear;

  ML.Add(Format('Index = %d', [TRggColorPool.GetColorIndex(FColor)]));
  ML.Add(Format('Name  = %s', [TRggColorPool.ColorToString(FColor)]));
  ML.Add(Format('Group = %s', [TRggColorPool.ColorGroupToGroupName(FColorGroup)]));
  ML.Add(Format('Kind  = %s', [TRggColorPool.GetColorKindString(FColor)]));
  ML.Add(Format('Hex   = (%3.2x, %3.2x, %3.2x)', [cr.R, cr.G, cr.B]));
  ML.Add(Format('RBG   = (%3d, %3d, %3d)', [cr.R, cr.G, cr.B]));

  Text.Text := ML.Text;
end;

end.
