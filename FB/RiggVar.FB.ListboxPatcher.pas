unit RiggVar.FB.ListboxPatcher;

interface

type
  TListBoxPatcher = class
  private
    const MSF_MethodName = 'MouseSelectFinish';
    class var IsPatched: Boolean;
    class function GetVirtualMethodIndex(CT: TClass): Integer;
    class procedure PatchMouseSelectFinish(CT: TClass);
  public
    class procedure PatchListBox;
  end;

implementation

uses
  Windows,
  FMX.Listbox,
  System.UITypes,
  System.Classes,
  System.Rtti;

type
  TListBoxAccess = class(TListBox);

  TMouseSelectProc = procedure(
    const Item: TListBoxItem;
    const Button: TMouseButton;
    const Shift: TShiftState) of object;

procedure FixedMouseSelectFinish(Self: TListBoxSelector;
  const Item: TListBoxItem;
  const Button: TMouseButton;
  const Shift: TShiftState);
begin
  if Button <> TMouseButton.mbLeft then
    Self.DoMouseSelectStart(Item, Shift);
  Self.DoMouseSelectFinish(Item, Shift);
end;

class procedure TListBoxPatcher.PatchListBox;
var
  LB: TListBox;
  SC: TListBoxSelector;
begin
  if not IsPatched then
  begin
    IsPatched := True;
    LB := TListBox.Create(nil);
    LB.MultiSelectStyle := TMultiSelectStyle.None;
    { we need to know the class type of the SelectionController }
    SC := TListBoxAccess(LB).SelectionController;
    PatchMouseSelectFinish(SC.ClassType); // of the actual SC
    LB.Free;
  end;
end;

class procedure TListBoxPatcher.PatchMouseSelectFinish(CT: TClass);
var
  p: Pointer;
  n: UINT_PTR;
  vi: Integer;
begin
//  vi := 7; { known value in 10.3.3 }
  vi := GetVirtualMethodIndex(CT);

  if vi > 0 then
  begin
  { see https://en.delphipraxis.net/topic/1922-patch-a-private-virtual-method/ }
{$POINTERMATH ON}
    p := @FixedMouseSelectFinish;
    WriteProcessMemory(
      GetCurrentProcess,
      @PPointer(CT)[vi],
      @p,
      SizeOf(Pointer),
      n);
{$POINTERMATH OFF}
  end;
end;

class function TListBoxPatcher.GetVirtualMethodIndex(CT: TClass): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethods: TArray<TRttiMethod>;
  l: Integer;
  m: TRttiMethod;
begin
  { get the index of the public virtual method in VMT,
    of the class that is actually used,
    a descendent of TListBoxSelector }
  result := -1;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(CT);
    LMethods := LType.GetMethods('MouseSelectFinish');
    l := Length(LMethods);
    if l > 0 then
    begin
      { we want the first one }
      m := LMethods[0];
      result := m.VirtualIndex;
    end;
  finally
    LContext.Free;
  end;
end;

end.
