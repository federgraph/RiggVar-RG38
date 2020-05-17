unit RiggVar.RG.Track;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.SysUtils,
  System.Classes;

type
  TFederTrackbar = class
  private
    FValueOld: single;
    FValue: single;
    procedure SetValue(const AValue: single);
  public
    Min: single;
    Max: single;
    Frequency: single;
    LineSize: single;
    PageSize: single;
    OnChange: TNotifyEvent;
    Tracking: Boolean;
    property Value: single read FValue write SetValue;
    property ValueNoChange: single read FValue write FValue;
  end;

implementation

{ TFederTrackbar }

procedure TFederTrackbar.SetValue(const AValue: single);
begin
  if (AValue <= Min) then
    FValue := Min
  else if AValue >= Max then
    FValue := Max
  else
    FValue := AValue;

  if (FValueOld <> FValue) and Assigned(OnChange) then
  begin
    FValueOld := FValue;
    OnChange(self);
  end;
end;

end.
