unit RiggVar.RG.LocalizedStrings;

interface

uses
  RiggVar.RG.LocalizedStrings00;

type
  TRggLocalizedStrings = class(TRggLocalizedStrings00)
  public
    class constructor Create;
    class procedure UpdateText;
  end;

implementation

uses
  RiggVar.RG.LocalizedStringsDE,
  RiggVar.RG.LocalizedStringsEN,
  RiggVar.App.Main;

{ TRggLocalizedStrings }

class constructor TRggLocalizedStrings.Create;
begin
  TRggLocalizedStrings.Init00;
end;

class procedure TRggLocalizedStrings.UpdateText;
begin
  if not MainVar.WantLocalizedText then
  begin
    TRggLocalizedStrings.Init00
  end
  else
  begin
    if MainVar.WantGermanText then
      TRggLocalizedStringsDE.InitDE
    else
      TRggLocalizedStringsEN.InitEN;
  end;
end;

end.
