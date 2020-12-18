unit RiggVar.App.Model;

interface

{
  I want to be able to
  1. use old or new implementation
  2. use interface or object references

  Using object references will allow to control click to the implementation.
  IRigg may just be an alias to the chosen TRigg.
}

{.$define WantOriginalLayout}
{.$define WantInterface}

uses
{$ifdef WantInterface}
  RiggVar.RG.Inter,
{$endif}
{$ifdef WantOriginalLayout}
  RggUnit4;
{$else}
  RiggVar.RG.Model;
{$endif}

type
{$ifdef WantOriginalLayout}
  TRigg = TRigg1;
{$else}
  TRigg = TRigg2;
{$endif}

{$ifdef WantInterface}
  IRigg = IRigg0;
{$else}
  IRigg = TRigg;
{$endif}

  TModelFactory = class
  public
    class function NewRigg: TRigg;
    class procedure ReleaseIfAppropriate(ARigg: IRigg);
  end;

implementation

{ TModelFactory }

class function TModelFactory.NewRigg: TRigg;
begin
  result := TRigg.Create;
end;

class procedure TModelFactory.ReleaseIfAppropriate(ARigg: IRigg);
begin
{$ifdef WantInterface}
  { do nothing }
{$else}
  ARigg.Free;
{$endif}
end;

end.
