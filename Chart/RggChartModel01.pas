unit RggChartModel01;

interface

{.$define AuswahlDlgSupported}

uses
  SysUtils,
  Classes,
  Math,
  FMX.Forms,
  Types,
  FMX.Graphics,
  UITypes,
  UIConsts,
  RggStrings,
  RggChartModel,
  RggScroll,
  RggTypes;

type
  TRggChartModel01 = class(TChartModel)
  private
    FSalingTyp: TSalingTyp;
    procedure SetSalingTyp(Value: TSalingTyp);
  public
    BereichBtnDown: Boolean;
    APBtnDown: Boolean;

    constructor Create;

    procedure UpdateXMinMax; override;
    procedure UpdatePMinMax; override;

    procedure YAuswahlClick;
    procedure RebuildYCombo;

    property SalingTyp: TSalingTyp read FSalingTyp write SetSalingTyp;
  end;

implementation

uses
{$ifdef AuswahlDlgSupported}
  FrmAuswahl,
{$endif}
  RiggVar.RG.Def;

{ TRggChartModel01 }

constructor TRggChartModel01.Create;
begin
  WantRectangles := True;

  BereichBtnDown := False;
  APBtnDown := True;

  inherited;

  IsUp := True;
end;

procedure TRggChartModel01.UpdateXMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := XComboSelectedText;
  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(XMinEditText);
        Maximum := StrToInt(XMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    XMinEditText := IntToStr(Minimum);
    XMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      XMinEditText := IntToStr(0);
      XMaxEditText := IntToStr(100);
      Valid := False;
      XLEDFillColor := claRed;
    end;
  end;
end;

procedure TRggChartModel01.UpdatePMinMax;
var
  s: string;
  xp: TxpName;
  tempMin, tempMax, tempIst, Minimum, Maximum: Integer;
  f: TRggSB;
begin
  s := PComboSelectedText;
  if s = NoParamString then
  begin
    PMinEditText := IntToStr(0);
    PMaxEditText := IntToStr(0);
    KurvenZahlSpinnerValue := 1;
  end
  else if KurvenZahlSpinnerValue = 1 then
  begin
    KurvenzahlSpinnerValue := UserSelectedKurvenZahl;
  end;

  xp := GetTsbName(s);
  if not (xp in XSet) then
    Exit;

  if (SalingTyp = stFest) and (xp = xpSalingL) then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_LMin);
    tempMax := Floor(SalingDreieck.Saling_LMax);
    tempIst := Round(SalingDreieck.Saling_L);
  end
  else if xp = xpSalingW then
  begin
    SalingDreieck.CopyFromRigg(Rigg);
    tempMin := Ceil(SalingDreieck.Saling_WMin * D180);
    tempMax := Floor(SalingDreieck.Saling_WMax * D180);
    tempIst := Round(SalingDreieck.Saling_W * D180);
  end
  else
  begin
    f := Rigg.GSB.GetSB(TsbName(xp));
    tempMin := Round(f.Min);
    tempMax := Round(f.Max);
    tempIst := Round(f.Ist);
  end;

  try
    if BereichBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
    end
    else if APBtnDown then
    begin
      Minimum := tempMin;
      Maximum := tempMax;
      if (tempIst - APWidth) > tempMin then
        Minimum := tempIst - APWidth;
      if (tempIst + APWidth) < tempMax then
        Maximum := tempIst + APWidth;
    end
    else
    begin
      try
        Minimum := StrToInt(PMinEditText);
        Maximum := StrToInt(PMaxEditText);
        if Minimum < tempMin then Minimum := tempMin;
        if Maximum > tempMax then Maximum := tempMax;
        if Minimum > Maximum then Minimum := tempMin;
        if Maximum < Minimum then Maximum := tempMax;
      except
        on EConvertError do
        begin
          Minimum := tempMin;
          Maximum := tempMax;
        end;
      end;
    end;
    PMinEditText := IntToStr(Minimum);
    PMaxEditText := IntToStr(Maximum);
  except
    on ERangeError do
    begin
      PMinEditText := IntToStr(0);
      PMaxEditText := IntToStr(100);
      Valid := False;
      PLEDFillColor := claRed;
    end;
  end;
end;

procedure TRggChartModel01.RebuildYCombo;
var
  YAV: TYAchseValue;
begin
  YComboItems.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if YAV in YAchseSet then
      YComboItems.Add(YAchseRecordList[YAV].ComboText);
  if YComboItems.Count > 0 then YComboItemIndex := 0;
  UpdateYAchseList;
{$ifdef AuswahlDlgSupported}
  YAuswahlDlg.DstList.Items := YComboItems;
  YAuswahlDlg.SrcList.Clear;
  for YAV := Low(TYAchseValue) to High(TYAchseValue) do
    if not (YAV in YAchseSet) then
      YAuswahlDlg.SrcList.Items.Add(YAchseRecordList[YAV].ComboText);
{$endif}
end;

procedure TRggChartModel01.SetSalingTyp(Value: TSalingTyp);
begin
  if FSalingTyp <> Value then
  begin
    FSalingTyp := Value;
    FValid := False;
    YLEDFillColor := claRed;
    UpdateXCombo(SalingTyp);
  end;
end;

procedure TRggChartModel01.YAuswahlClick;
{$ifdef AuswahlDlgSupported}
var
  i: Integer;
{$endif}
begin
{$ifdef AuswahlDlgSupported}
  if YAuswahlDlg = nil then
  begin
    YAuswahlDlg := TYAuswahlDlg.Create(Application);
  end;
  with YAuswahlDlg do
  begin
    if not (DstList.ItemIndex = -1) then
    begin
      { clear selection if any }
      for i := 0 to DstList.Items.Count-1 do
      begin
        DstList.Selected[i] := False;
      end;
      { In DstList den gleichen Eintrag wie in YComboBox selektieren }
      DstList.ItemIndex := YComboItemIndex;
      DstList.Selected[YComboItemIndex] := True;
    end;

    if ShowModal = mrOK then
    begin
      if (DstList.Items.Count = 0) then
      begin
        { mindestens ein Eintrag muﬂ sich in DestList befinden }
        DstList.Items.AddObject(SrcList.Items[0], SrcList.Items.Objects[0]);
        SrcList.Items.Delete(0);
      end;
      YComboItems.Assign(DstList.Items);
      YComboItemIndex := DstList.ItemIndex;
      UpdateYAchseList;
      YComboChange(nil);
    end;
  end;
{$endif}
end;

end.
