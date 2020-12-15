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
  RggInter,
{$endif}
{$ifdef WantOriginalLayout}
  RggUnit4;
{$else}
  RiggVar.RG.Model;
{$endif}

type
{$ifdef WantOriginalLayout}
  TRigg = RggUnit4.TRigg1;
{$else}
  TRigg = RiggVar.RG.Model.TRigg2;
{$endif}

{$ifdef WantInterface}
  IRigg = RggInter.IRigg0;
{$else}
  IRigg = TRigg;
{$endif}

  TModelFactory = class
  public
    class function NewRigg: TRigg;
    class procedure ReleaseIfAppropriate(ARigg: TRigg);
  end;

implementation

{ TModelFactory }

class function TModelFactory.NewRigg: TRigg;
begin
  result := TRigg.Create;
end;

class procedure TModelFactory.ReleaseIfAppropriate(ARigg: TRigg);
begin
{$ifdef WantInterface}
  { do nothing }
{$else}
  ARigg.Free;
{$endif}
end;

end.
