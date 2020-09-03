unit RiggVar.RG.Data;

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
  System.Classes,
  System.Math,
  RiggVar.RG.Def;

type
  TRggData = class
  private
    DoctypeRgg: string;
    DoctypeTrimm: string;
    DoctypeTrimmFile: string;

    NamespaceRgg: string;
    NamespaceTrimm: string;
    NamespaceTrimmFile: string;

    VersionRgg: Integer;
    VersionTrimm: Integer;
    VersionTrimmFile: Integer;

    Version: Integer;
    FModified: Boolean;
    procedure Check(AML: TStrings);
    function IsCode(AML: TStrings): Boolean;
    procedure LoadCode(AML: TStrings);
    procedure ProcessH;
    procedure ProcessW;
    procedure DoSave(fs: string; AML: TStrings);
    procedure SaveJava(AML: TStrings);
    procedure SavePascal(AML: TStrings);
    procedure SaveProps(AML: TStrings);
  protected
    procedure Reset1;
    procedure Reset2;
  public

    { helper values for spreader trapeze }
    h0: Integer;
    h1: Integer;

    h2: Integer;
    l2: Integer;

    l3: Integer;
    w3: Integer;
    h3: single;

    { computed value cache - initialized later, when loaded }
    F0C: Integer;
    F0F: Integer;
    Bie: Integer;

    { Root Attributes }

    Name: string;

    Faktor: Integer;
    OffsetX: Integer;
    OffsetZ: Integer;

    { RK: Rumpfkoordinaten }

    A0X: Integer;
    A0Y: Integer;
    A0Z: Integer;

    C0X: Integer;
    C0Y: Integer;
    C0Z: Integer;

    D0X: Integer;
    D0Y: Integer;
    D0Z: Integer;

    E0X: Integer;
    E0Y: Integer;
    E0Z: Integer;

    F0X: Integer;
    F0Y: Integer;
    F0Z: Integer;

    { RL: Rigglängen }

    MU: Integer;
    MO: Integer;
    ML: Integer;
    MV: Integer;
    CA: Integer;

    { SB: Scroll Bars }

    CPMin: Integer;
    CPMax: Integer;
    CPPos: Integer;

    SHMin: Integer;
    SHPos: Integer;
    SHMax: Integer;

    SAMin: Integer;
    SAPos: Integer;
    SAMax: Integer;

    SLMin: Integer;
    SLPos: Integer;
    SLMax: Integer;

    SWMin: Integer;
    SWMax: Integer;
    SWPos: Integer;

    VOMin: Integer;
    VOPos: Integer;
    VOMax: Integer;

    WIMin: Integer;
    WIPos: Integer;
    WIMax: Integer;

    WLMin: Integer;
    WLPos: Integer;
    WLMax: Integer;

    WOMin: Integer;
    WOPos: Integer;
    WOMax: Integer;

    { VP: View Params }

    AngleX: single;
    AngleY: single;
    AngleZ: single;

    PosZ: single;

    class var
      WantJava: Boolean;
      WantPascal: Boolean;
      WantComment: Boolean;
      WantName: Boolean;
      WantAll: Boolean;
      WantSpace: Boolean;

    constructor Create;
    procedure Reset;
    procedure Assign(fd: TRggData);

    procedure ReadTestFile(FL: TStrings);

    procedure SaveTrimmFile(AML: TStrings);
    procedure ReadTrimmFile(AML: TStrings);

    procedure SaveTrimmItem(AML: TStrings);
    procedure LoadTrimmItem(AML: TStrings);

    procedure Save(AML: TStrings);
    procedure Load(AML: TStrings);

    procedure WriteJSon(AML: TStrings);
    procedure WriteReport(AML: TStrings);
  end;

implementation

uses
  RiggVar.App.Main;

{ ML - Memo Lines, often used as a name for a parameter of type TStrings }
{ AML - needed to avoid naming conflict with field ML! }
{ Maybe a better Name for AML would be TL - Text Lines }

{ TRggData }

constructor TRggData.Create;
begin
  WantPascal := True;

  DoctypeRgg := 'Rgg';
  DoctypeTrimm := 'Trimm-Item';
  DoctypeTrimmFile := 'Trimm-File';

  NamespaceRgg := 'http://www.riggvar.de/2011/rgg';
  NamespaceTrimm := 'http://www.riggvar.de/2015/rgg/trimm-item';
  NamespaceTrimmFile := 'http://www.riggvar.de/2015/rgg/trimm-file';

  VersionRgg := 1;
  VersionTrimm := 1;
  VersionTrimmFile := 1;

  Reset;
end;

procedure TRggData.Assign(fd: TRggData);
begin
  Faktor := fd.Faktor;
  OffsetX := fd.OffsetX;
  OffsetZ := fd.OffsetZ;

  { RK: Rumpfkoordinaten }

  A0X := fd.A0X;
  A0Y := fd.A0Y;
  A0Z := fd.A0Z;

  C0X := fd.C0X;
  C0Y := fd.C0Y;
  C0Z := fd.C0Z;

  D0X := fd.D0X;
  D0Y := fd.D0Y;
  D0Z := fd.D0Z;

  E0X := fd.E0X;
  E0Y := fd.E0Y;
  E0Z := fd.E0Z;

  F0X := fd.F0X;
  F0Y := fd.F0Y;
  F0Z := fd.F0Z;

  { RL: Rigglängen }

  MU := fd.MU;
  MO := fd.MO;
  ML := fd.ML;
  MV := fd.MV;
  CA := fd.CA;

  { SB: Scroll Bars }

  CPMin := fd.CPMin;
  CPPos := fd.CPPos;
  CPMax := fd.CPMax;

  SHMin := fd.SHMin;
  SHPos := fd.SHPos;
  SHMax := fd.SHMax;

  SAMin := fd.SAMin;
  SAPos := fd.SAPos;
  SAMax := fd.SAMax;

  SLMin := fd.SLMin;
  SLPos := fd.SLPos;
  SLMax := fd.SLMax;

  SWMin := fd.SWMin;
  SWPos := fd.SWPos;
  SWMax := fd.SWMax;

  VOMin := fd.VOMin;
  VOPos := fd.VOPos;
  VOMax := fd.VOMax;

  WIMin := fd.WIMin;
  WIPos := fd.WIPos;
  WIMax := fd.WIMax;

  WLMin := fd.WLMin;
  WLPos := fd.WLPos;
  WLMax := fd.WLMax;

  WOMin := fd.WOMin;
  WOPos := fd.WOPos;
  WOMax := fd.WOMax;
end;

procedure TRggData.Reset;
begin
  Reset1;
end;

procedure TRggData.Reset1;
begin
//  Name := '420';

  Faktor := 1;
  OffsetX := 0;
  OffsetZ := 0;

  A0X := 2560;
  A0Y := 765;
  A0Z := 430;

  C0X := 4140;
  C0Y := 0;
  C0Z := 340;

  D0X := 2870;
  D0Y := 0;
  D0Z := -100;

  E0X := 2970;
  E0Y := 0;
  E0Z := 450;

  F0X := -30;
  F0Y := 0;
  F0Z := 300;

  { Rigglängen }
  MU := 2600; // Mast Unten  (D0D), or lower part of the mast
  MO := 2000; // Mast Oben (DC), or uppe part of the mast
  ML := 6115; // Mast Länge (D0F), or mast length
  MV := 5000; // Mastfallvorlauf
  CA := 50; // Controller Anschlag

  { Controller Position (E0E ) }
  CPMin := 50;
  CPPos := 100;
  CPMax := 200;

  { Saling Abstand (AB) }
  SAMin := 780;
  SAPos := 850;
  SAMax := 1000;

  { Saling Höhe (PD), or spreader height }
  SHMin := 140;
  SHPos := 220;
  SHMax := 300;

  { Saling Länge (AB), or spreader length }
  SLMin := 450;
  SLPos := Round(Sqrt(Sqr(SHPos) + Sqr(SAPos / 2)));
  SLMax := 600;

  SWMin := 0;
  SWPos := Round(RadToDeg(ArcTan2(SHPos, SAPos / 2)));
  SWMax := 89;

  { Vorstag (C0C), or forestay, headstay }
  VOMin := 4400;
  VOPos := 4500;
  VOMax := 4600;

  { Winkel, or Angle of D0D }
  WIMin := 85;
  WIPos := 95;
  WIMax := 105;

  { Wante Länge (A0A+AC und B0B+BC), or shroud length }
  WLMin := 4050;
  WLPos := 4120;
  WLMax := 4200;

  { Wante Oben (AC und BC), or upper part of shroud }
  WOMin := 2000;
  WOPos := 2020;
  WOMax := 2070;

  { for viewing the model }
  AngleX := 0;
  AngleY := 0;
  AngleZ := 0;
  PosZ := 0; // camera position

  h0 := 56;
  l2 := 100;
  h2 := 0;

  h1 := 0;
  l3 := 0;
  w3 := 0;
  h3 := 0;
end;

procedure TRggData.Reset2;
begin
//  Name := '420';

  Faktor := 1;
  OffsetX := 0;
  OffsetZ := 0;

//  RK.A0.Init(2560, 765, 430);
//  RK.C0.Init(4140, 0, 340);
//  RK.D0.Init(2870, 0, -100);
//  RK.E0.Init(2970, 0, 450);
//  RK.F0.Init(-30, 0, 300);
//
//  RL.MU = 2600;
//  RL.MO = 2000;
//  RL.ML = 6115;
//  RL.MV = 5000;
//  RL.CA = 50;
//
//  SB.CP.Min = 50;
//  SB.CP.Pos = 100;
//  SB.CP.Max = 200;
//
//  SB.SA.Min = 780;
//  SB.SA.Pos = 850;
//  SB.SA.Max = 1000;
//
//  SB.SH.Min = 140;
//  SB.SH.Pos = 220;
//  SB.SH.Max = 300;
//
//  SB.SWMin := 0;
//  SB.SWPos := Round(RadToDeg(ArcTan2(SHPos, SAPos / 2)));
//  SB.SWMax := 89;
//
//  SB.SL.Min = 450;
//  SB.SL.Pos = Round(Sqrt(Sqr(SHPos) + Sqr(SAPos / 2)));
//  SB.SL.Max = 600;
//
//  SB.VO.Min = 4400;
//  SB.VO.Pos = 4500;
//  SB.VO.Max = 4600;
//
//  SB.WI.Min = 85;
//  SB.WI.Pos = 95;
//  SB.WI.Max = 105;
//
//  SB.WL.Min = 4050;
//  SB.WL.Pos = 4120;
//  SB.WL.Max = 4200;
//
//  SB.WO.Min = 2000;
//  SB.WO.Pos = 2020;
//  SB.WO.Max = 2070;
end;

function TRggData.IsCode(AML: TStrings): Boolean;
var
  i: Integer;
begin
  result := false;
  if AML.Count > 0 then
  begin
    for i := 0 to AML.Count-1 do
      if Pos(':=', AML[i]) > 0 then
      begin
        result := True;
        break;
      end;
  end
end;

procedure TRggData.LoadCode(AML: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to AML.Count-1 do
  begin
    s := Trim(AML[i]);
    if s = '' then
      Continue;
    if Pos('with', s) > 0 then
      Continue;
    if Pos('begin', s) > 0 then
      Continue;
    if Pos('end;', s) > 0 then
      Continue;
    s := StringReplace(s, ':=', '=', []);
    s := StringReplace(s, ';', '', []);
    AML[i] := s;
  end;
  if AML.Count > 0 then
  begin
    Version := 1;
    Check(AML);
    Load(AML);
  end;
end;

procedure TRggData.Check(AML: TStrings);
var
  s: string;
  temp: string;
  i, l: Integer;
begin
  for l := 0 to AML.Count-1 do
  begin
    s := AML[l];
    i := Pos('=', s);
    if i > 0 then
    begin
      temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)));
      AML[l] := temp;
    end
    else
      temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);
  end;
end;

procedure TRggData.LoadTrimmItem(AML: TStrings);
var
  s: string;
begin
  if IsCode(AML) then
  begin
    LoadCode(AML);
  end
  else if AML.Count > 0 then
  begin
    s := AML.Values['version'];
    Version := StrToIntDef(s, 1);
    Check(AML);
    Load(AML)
  end;
  FModified := False;
end;

procedure TRggData.SaveTrimmItem(AML: TStrings);
begin
  if WantPascal then
  begin
    AML.Add(Format('DOCTYPE := %s;', [DoctypeTrimm]));
    AML.Add(Format('Namespace := %s ;', [NamespaceTrimm]));
    AML.Add('Version := 1;');
    AML.Add('');
    AML.Add(Format(dss, [cName, Name]));
  end
  else if WantJava then
  begin
    AML.Add(Format('DOCTYPE = %s;', [DoctypeTrimm]));
    AML.Add(Format('Namespace = %s;', [NamespaceTrimm]));
    AML.Add('Version = 1;');
    AML.Add('');
    AML.Add(Format(jss, [cName, Name]));
  end
  else
  begin
    AML.Add('//Trimm-Item');
    AML.Add(Format('DOCTYPE = %s', [DoctypeTrimm]));
    AML.Add(Format('Namespace = %s', [NamespaceTrimm]));
    AML.Add('Version = 1');
    AML.Add('');
    AML.Add(Format(fss, [cName, Name]));
  end;
  AML.Add('');
  WantName := False;
  Save(AML);
end;

procedure TRggData.Save(AML: TStrings);
begin
  if WantPascal then
    SavePascal(AML)
  else if WantJava then
    SaveJava(AML)
  else
    SaveProps(AML);
end;

procedure TRggData.SavePascal(AML: TStrings);
begin
  if WantName then
    AML.Add(Format(dss, [cName, Name]));
  DoSave(dsd, AML);
end;

procedure TRggData.SaveJava(AML: TStrings);
begin
  if WantName then
    AML.Add(Format(jss, [cName, Name]));
  DoSave(jsd, AML);
end;

procedure TRggData.SaveProps(AML: TStrings);
begin
  if WantName then
    AML.Add(Format(fss, [cName, Name]));
  DoSave(fsd, AML);
end;

procedure TRggData.DoSave(fs: string; AML: TStrings);
var
  fd: TRggData;
begin
  fd := Main.Trimm0;

//  if WantName then
//    AML.Add(Format(dss, [cName, Name]));

//  AML.Add(Format(dsd, [cFaktor, Faktor]));
//  AML.Add(Format(dsd, [cOffsetX, OffsetX]));
//  AML.Add(Format(dsd, [cOffsetZ, OffsetZ]));

  if WantSpace then
    AML.Add('');
  if WantAll or (A0X <> fd.A0X) then
    AML.Add(Format(fs, [cA0X, A0X]));
  if WantAll or (A0Y <> fd.A0Y) then
    AML.Add(Format(fs, [cA0Y, A0Y]));
  if WantAll or (A0Z <> fd.A0Z) then
    AML.Add(Format(fs, [cA0Z, A0Z]));

  if WantAll or (C0X <> fd.C0X) then
    AML.Add(Format(fs, [cC0X, C0X]));
  if WantAll or (C0Y <> fd.C0Y) then
    AML.Add(Format(fs, [cC0Y, C0Y]));
  if WantAll or (C0Z <> fd.C0Z) then
    AML.Add(Format(fs, [cC0Z, C0Z]));

  if WantAll or (D0X <> fd.D0X) then
    AML.Add(Format(fs, [cD0X, D0X]));
  if WantAll or (D0Y <> fd.D0Y) then
    AML.Add(Format(fs, [cD0Y, D0Y]));
  if WantAll or (D0Z <> fd.D0Z) then
    AML.Add(Format(fs, [cD0Z, D0Z]));

  if WantAll or (E0X <> fd.E0X) then
    AML.Add(Format(fs, [cE0X, E0X]));
  if WantAll or (E0Y <> fd.E0Y) then
    AML.Add(Format(fs, [cE0Y, E0Y]));
  if WantAll or (E0Z <> fd.E0Z) then
    AML.Add(Format(fs, [cE0Z, E0Z]));

  if WantAll or (F0X <> fd.F0X) then
    AML.Add(Format(fs, [cF0X, F0X]));
  if WantAll or (F0Y <> fd.F0Y) then
    AML.Add(Format(fs, [cF0Y, F0Y]));
  if WantAll or (F0Z <> fd.F0Z) then
    AML.Add(Format(fs, [cF0Z, F0Z]));

  if WantSpace then
    AML.Add('');
  if WantAll or (MU <> fd.MU) then
    AML.Add(Format(fs, [cMU, MU]));
  if WantAll or (MO <> fd.MO) then
    AML.Add(Format(fs, [cMO, MO]));
  if WantAll or (ML <> fd.ML) then
    AML.Add(Format(fs, [cML, ML]));
  if WantAll or (MV <> fd.MV) then
    AML.Add(Format(fs, [cMV, MV]));
  if WantAll or (CA <> fd.CA) then
    AML.Add(Format(fs, [cCA, CA]));

  if WantSpace then
    AML.Add('');
  if WantAll or (h0 <> fd.h0) then
    AML.Add(Format(fs, [ch0, h0]));
  if WantAll or (h2 <> fd.h2) then
    AML.Add(Format(fs, [ch2, h2]));
  if WantAll or (l2 <> fd.l2) then
    AML.Add(Format(fs, [cl2, l2]));

  if WantSpace then
    AML.Add('');
  if WantAll or (CPMin <> fd.CPMin) then
    AML.Add(Format(fs, [cCPMin, CPMin]));
  if WantAll or (CPPos <> fd.CPPos) then
    AML.Add(Format(fs, [cCPPos, CPPos]));
  if WantAll or (CPMax <> fd.CPMax) then
    AML.Add(Format(fs, [cCPMax, CPMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (SHMin <> fd.SHMin) then
    AML.Add(Format(fs, [cSHMin, SHMin]));
  if WantAll or (SHPos <> fd.SHPos) then
    AML.Add(Format(fs, [cSHPos, SHPos]));
  if WantAll or (SHMax <> fd.SHMax) then
    AML.Add(Format(fs, [cSHMax, SHMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (SAMin <> fd.SAMin) then
    AML.Add(Format(fs, [cSAMin, SAMin]));
  if WantAll or (SAPos <> fd.SAPos) then
    AML.Add(Format(fs, [cSAPos, SAPos]));
  if WantAll or (SAMax <> fd.SAMax) then
    AML.Add(Format(fs, [cSAMax, SAMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (SLMin <> fd.SLMin) then
    AML.Add(Format(fs, [cSLMin, SLMin]));
  if WantAll or (SLPos <> fd.SLPos) then
    AML.Add(Format(fs, [cSLPos, SLPos]));
  if WantAll or (SLMax <> fd.SLMax) then
    AML.Add(Format(fs, [cSLMax, SLMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (SWMin <> fd.SWMin) then
    AML.Add(Format(fs, [cSWMin, SWMin]));
  if WantAll or (SWPos <> fd.SWPos) then
    AML.Add(Format(fs, [cSWPos, SWPos]));
  if WantAll or (SWMax <> fd.SWMax) then
    AML.Add(Format(fs, [cSWMax, SWMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (VOMin <> fd.VOMin) then
    AML.Add(Format(fs, [cVOMin, VOMin]));
  if WantAll or (VOPos <> fd.VOPos) then
    AML.Add(Format(fs, [cVOPos, VOPos]));
  if WantAll or (VOMax <> fd.VOMax) then
    AML.Add(Format(fs, [cVOMax, VOMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (WIMin <> fd.WIMin) then
    AML.Add(Format(fs, [cWIMin, WIMin]));
  if WantAll or (WIPos <> fd.WIPos) then
    AML.Add(Format(fs, [cWIPos, WIPos]));
  if WantAll or (WIMax <> fd.WIMax) then
    AML.Add(Format(fs, [cWIMax, WIMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (WLMin <> fd.WLMin) then
    AML.Add(Format(fs, [cWLMin, WLMin]));
  if WantAll or (WLPos <> fd.WLPos) then
    AML.Add(Format(fs, [cWLPos, WLPos]));
  if WantAll or (WLMax <> fd.WLMax) then
    AML.Add(Format(fs, [cWLMax, WLMax]));
  if WantSpace then
    AML.Add('');
  if WantAll or (WOMin <> fd.WOMin) then
    AML.Add(Format(fs, [cWOMin, WOMin]));
  if WantAll or (WOPos <> fd.WOPos) then
    AML.Add(Format(fs, [cWOPos, WOPos]));
  if WantAll or (WOMax <> fd.WOMax) then
    AML.Add(Format(fs, [cWOMax, WOMax]));
end;

procedure TRggData.Load(AML: TStrings);
var
  s: string;
begin
//  Reset;

  s := AML.Values[cVersion];
  Version := StrToIntDef(s, VersionTrimm);

  s := AML.Values[cName];
  Name := s;

  s := AML.Values[cFaktor];
  Faktor := StrToIntDef(s, Faktor);

  s := AML.Values[cOffsetX];
  OffsetX := StrToIntDef(s, OffsetX);

  s := AML.Values[cOffsetZ];
  OffsetZ := StrToIntDef(s, OffsetZ);

  s := AML.Values[cA0X];
  A0X := StrToIntDef(s, A0X);
  s := AML.Values[cA0Y];
  A0Y := StrToIntDef(s, A0Y);
  s := AML.Values[cA0Z];
  A0Z := StrToIntDef(s, A0Z);

  s := AML.Values[cC0X];
  C0X := StrToIntDef(s, C0X);
  s := AML.Values[cC0Y];
  C0Y := StrToIntDef(s, C0Y);
  s := AML.Values[cC0Z];
  C0Z := StrToIntDef(s, C0Z);

  s := AML.Values[cD0X];
  D0X := StrToIntDef(s, D0X);
  s := AML.Values[cD0Y];
  D0Y := StrToIntDef(s, D0Y);
  s := AML.Values[cD0Z];
  D0Z := StrToIntDef(s, D0Z);

  s := AML.Values[cE0X];
  E0X := StrToIntDef(s, E0X);
  s := AML.Values[cE0Y];
  E0Y := StrToIntDef(s, E0Y);
  s := AML.Values[cE0Z];
  E0Z := StrToIntDef(s, E0Z);

  s := AML.Values[cF0X];
  F0X := StrToIntDef(s, F0X);
  s := AML.Values[cF0Y];
  F0Y := StrToIntDef(s, F0Y);
  s := AML.Values[cF0Z];
  F0Z := StrToIntDef(s, F0Z);

  s := AML.Values[cMU];
  MU := StrToIntDef(s, MU);
  s := AML.Values[cMO];
  MO := StrToIntDef(s, MO);
  s := AML.Values[cML];
  ML := StrToIntDef(s, ML);
  s := AML.Values[cCA];
  CA := StrToIntDef(s, CA);

  s := AML.Values[cMV];
  MV := StrToIntDef(s, MV);

  s := AML.Values[cCPMin];
  CPMin := StrToIntDef(s, CPMin);
  s := AML.Values[cCPPos];
  CPPos := StrToIntDef(s, CPPos);
  s := AML.Values[cCPMax];
  CPMax := StrToIntDef(s, CPMax);

  s := AML.Values[cSHMin];
  SHMin := StrToIntDef(s, SHMin);
  s := AML.Values[cSHPos];
  SHPos := StrToIntDef(s, SHPos);
  s := AML.Values[cSHMax];
  SHMax := StrToIntDef(s, SHMax);

  s := AML.Values[cSAMin];
  SAMin := StrToIntDef(s, SAMin);
  s := AML.Values[cSAPos];
  SAPos := StrToIntDef(s, SAPos);
  s := AML.Values[cSAMax];
  SAMax := StrToIntDef(s, SAMax);

  s := AML.Values[cSLMin];
  SLMin := StrToIntDef(s, SLMin);
//  s := AML.Values[cSLPos];
//  SLPos := StrToIntDef(s, SLPos);
  s := AML.Values[cSLMax];
  SLMax := StrToIntDef(s, SLMax);

  s := AML.Values[cSWMin];
  SWMin := StrToIntDef(s, SWMin);
//  s := AML.Values[cSWPos];
//  SWPos := StrToIntDef(s, SWPos);
  s := AML.Values[cSWMax];
  SWMax := StrToIntDef(s, SWMax);

  s := AML.Values[cVOMin];
  VOMin := StrToIntDef(s, VOMin);
  s := AML.Values[cVOPos];
  VOPos := StrToIntDef(s, VOPos);
  s := AML.Values[cVOMax];
  VOMax := StrToIntDef(s, VOMax);

  s := AML.Values[cWIMin];
  WIMin := StrToIntDef(s, WIMin);
  s := AML.Values[cWIPos];
  WIPos := StrToIntDef(s, WIPos);
  s := AML.Values[cWIMax];
  WIMax := StrToIntDef(s, WIMax);

  s := AML.Values[cWLMin];
  WLMin := StrToIntDef(s, WLMin);
  s := AML.Values[cWLPos];
  WLPos := StrToIntDef(s, WLPos);
  s := AML.Values[cWLMax];
  WLMax := StrToIntDef(s, WLMax);

  s := AML.Values[cWOMin];
  WOMin := StrToIntDef(s, WOMin);
  s := AML.Values[cWOPos];
  WOPos := StrToIntDef(s, WOPos);
  s := AML.Values[cWOMax];
  WOMax := StrToIntDef(s, WOMax);

  s := AML.Values[cCP];
  CPPos := CPPos + StrToIntDef(s, 0);

  s := AML.Values[cSH];
  SHPos := SHPos + StrToIntDef(s, 0);

  s := AML.Values[cSA];
  SAPos := SAPos + StrToIntDef(s, 0);

//  s := AML.Values[cSL];
//  SLPos := SLPos + StrToIntDef(s, 0);

//  s := AML.Values[cSW];
//  SWPos := SWPos + StrToIntDef(s, 0);

  s := AML.Values[cVO];
  VOPos := VOPos + StrToIntDef(s, 0);

  s := AML.Values[cWI];
  WIPos := WIPos + StrToIntDef(s, 0);

  s := AML.Values[cWL];
  WLPos := WLPos + StrToIntDef(s, 0);

  s := AML.Values[cWO];
  WOPos := WOPos + StrToIntDef(s, 0);

  s := AML.Values[ch0];
  h0 := StrToIntDef(s, h0);

  s := AML.Values[ch1];
  h1 := StrToIntDef(s, h1);

  s := AML.Values[ch2];
  h2 := StrToIntDef(s, h2);

  s := AML.Values[cl2];
  l2 := StrToIntDef(s, l2);

  s := AML.Values[cl3];
  l3 := StrToIntDef(s, l3);

  s := AML.Values[cw3];
  w3 := StrToIntDef(s, w3);

  ProcessH;
  ProcessW;
end;

procedure TRggData.WriteJSon(AML: TStrings);
begin
  AML.Add(Format('{"Name":"%s","Faktor":%d,"OffsetX":%d,"OffsetZ":%d,', [Name, Faktor, OffsetX, OffsetZ]));

  AML.Add('"RK":{');
  AML.Add(Format('"A0":{"x":%d,"y":%d,"z":%d},', [A0X, A0Y, A0Z]));
  AML.Add(Format('"C0":{"x":%d,"y":%d,"z":%d},', [C0X, C0Y, C0Z]));
  AML.Add(Format('"D0":{"x":%d,"y":%d,"z":%d},', [D0X, D0Y, D0Z]));
  AML.Add(Format('"E0":{"x":%d,"y":%d,"z":%d},', [E0X, E0Y, E0Z]));
  AML.Add(Format('"F0":{"x":%d,"y":%d,"z":%d}},', [F0X, F0Y, F0Z]));

  AML.Add(Format('"RL":{"MU":%d,"MO":%d,"ML":%d,"MV":%d,"CA":%d},', [MU, MO, ML, MV, CA]));

  AML.Add('"SB":{');
  AML.Add(Format('"CP":{"Min":%d,"Pos":%d,"Max":%d},', [CPMin, CPPos, CPMax]));
  AML.Add(Format('"VO":{"Min":%d,"Pos":%d,"Max":%d},', [VOMin, VOPos, VOMax]));
  AML.Add(Format('"WI":{"Min":%d,"Pos":%d,"Max":%d},', [WIMin, WIPos, WIMax]));
  AML.Add(Format('"WL":{"Min":%d,"Pos":%d,"Max":%d},', [WLMin, WLPos, WLMax]));
  AML.Add(Format('"WO":{"Min":%d,"Pos":%d,"Max":%d},', [WOMin, WOPos, WOMax]));
  AML.Add(Format('"SH":{"Min":%d,"Pos":%d,"Max":%d},', [SHMin, SHPos, SHMax]));
  AML.Add(Format('"SA":{"Min":%d,"Pos":%d,"Max":%d},', [SAMin, SAPos, SAMax]));
  AML.Add(Format('"SL":{"Min":%d,"Pos":%d,"Max":%d}}}', [SLMin, SLPos, SLMax]));
end;

procedure TRggData.WriteReport(AML: TStrings);
begin
  AML.Add(Format('A0 (%d, %d, %d)', [A0X, A0Y, A0Z]));
  AML.Add(Format('C0 (%d, %d, %d)', [C0X, C0Y, C0Z]));
  AML.Add(Format('D0 (%d, %d, %d)', [D0X, D0Y, D0Z]));
  AML.Add(Format('E0 (%d, %d, %d)', [E0X, E0Y, E0Z]));
  AML.Add(Format('F0 (%d, %d, %d)', [F0X, F0Y, F0Z]));
  AML.Add('');
  AML.Add(Format('ML, MV (%d, %d)', [ML, MV]));
  AML.Add(Format('MU, MO (%d, %d)', [MU, MO]));
  AML.Add(Format('CA (%d)', [CA]));
  AML.Add(Format('h0, h2, l2 (%d, %d, %d)', [h0, h2, l2]));
  AML.Add('');
  AML.Add(Format('CP (%d, %d, %d)', [CPMin, CPPos, CPMax]));
  AML.Add(Format('VO (%d, %d, %d)', [VOMin, VOPos, VOMax]));
  AML.Add(Format('WI (%d, %d, %d)', [WIMin, WIPos, WIMax]));
  AML.Add(Format('WL (%d, %d, %d)', [WLMin, WLPos, WLMax]));
  AML.Add(Format('WO (%d, %d, %d)', [WOMin, WOPos, WOMax]));
  AML.Add(Format('SH (%d, %d, %d)', [SHMin, SHPos, SHMax]));
  AML.Add(Format('SA (%d, %d, %d)', [SAMin, SAPos, SAMax]));
  AML.Add(Format('SL (%d, %d, %d)', [SLMin, SLPos, SLMax]));
end;

procedure TRggData.ProcessW;
begin
  if (l3 > 0) and (w3 > 0) then
  begin
    h3 := l3 * sin(RadToDeg(w3));
    SHPos := Round(h2 + h3);
    SAPos := Round(2 * (l3 * cos(RadToDeg(w3)) + l2));
  end;
end;

procedure TRggData.ProcessH;
begin
  if (h1 > 0) then
    SHPos := h0 + h1;
end;

procedure TRggData.SaveTrimmFile(AML: TStrings);
begin
  Main.Logger.Info('in TRggData.SaveTrimmFile ( to TStrings )');
  WantSpace := True;
  WantComment := True;

  if WantPascal then
  begin
    AML.Add(Format('DOCTYPE := %s;', [DoctypeTrimmFile]));
    AML.Add(Format('Namespace := %s ;', [NamespaceTrimmFile]));
    AML.Add('Version := 1;');
  end
  else if WantJava then
  begin
    AML.Add(Format('DOCTYPE = %s;', [DoctypeTrimmFile]));
    AML.Add(Format('Namespace = %s ;', [NamespaceTrimmFile]));
    AML.Add('Version = 1;');
  end
  else
  begin
    AML.Add(Format('DOCTYPE=%s', [DoctypeTrimmFile]));
    AML.Add(Format('Namespace=%s', [NamespaceTrimmFile]));
    AML.Add('Version=1');
  end;

  if WantSpace then
    AML.Add('');
  if WantComment then
    AML.Add('//Basis-Trimm (Trimm 0)');

  WantAll := True;
  WantName := False;
  WantSpace := True;

  Main.Trimm0.Save(AML);

  WantAll := False;
  WantName := True;
  WantSpace := False;

  AML.Add('');
  AML.Add('//Trimm1');
  Main.Trimm1.Save(AML);

  AML.Add('');
  AML.Add('//Trimm2');
  Main.Trimm2.Save(AML);

  AML.Add('');
  AML.Add('//Trimm3');
  Main.Trimm3.Save(AML);

  AML.Add('');
  AML.Add('//Trimm4');
  Main.Trimm4.Save(AML);

  AML.Add('');
  AML.Add('//Trimm5');
  Main.Trimm5.Save(AML);

  AML.Add('');
  AML.Add('//Trimm6');
  Main.Trimm6.Save(AML);

end;

procedure TRggData.ReadTrimmFile(AML: TStrings);
var
  c: Integer;
  i, j: Integer;
  s: string;
  fd: TRggData;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    c := 0;
    for i := 0 to AML.Count-1 do
    begin
      s := AML[i];
      if s = '' then
        Continue;

      { ignore comment lines }
      if s.Trim.StartsWith('//') then
        Continue;

      { remove comments }
      if Pos('//', s) > 0 then
      begin
        { allow for namespace schema http:// }
        if not s.Contains('://') then
        begin
          j := s.IndexOf('//');
          s := s.Substring(0, j);
        end;
      end;

      if Pos('Name', s) > 0 then
      begin
        if Pos('Namespace', s) > 0 then
        begin
          SL.Add(s);
          Continue;
        end;

        { start new trimm }
        Inc(c);
        if c > 6 then
          break;

        { process old trimm }
        if c = 1 then
        begin
          { Trimm 0 }
          fd := Main.RggData;
          fd.Reset;
          fd.LoadTrimmItem(SL);
          Main.Trimm0.Assign(fd);
        end
        else if c > 1 then
        begin
          { Trimm 1 - 6 }
          fd := Main.GetTrimmItem(c - 1);
          fd.Reset;
          fd.Assign(Main.Trimm0);
          fd.LoadTrimmItem(SL);
        end;
        SL.Clear;

        SL.Add(s); // line with name for new trimm
      end
      else
      begin
        SL.Add(s); // normal lines with data (after line with name)
      end;
    end;

    { process current (last) trimm }
    fd := Main.GetTrimmItem(c);
    fd.Assign(Main.Trimm0);
    fd.LoadTrimmItem(SL);

  finally
    SL.Free;
  end;
end;

procedure TRggData.ReadTestFile(FL: TStrings);
begin
  FL.Clear;
  FL.Add(' //comment line');
  FL.Add('Name := T1;//abc');
  FL.Add('');
  FL.Add('Name := T2;');
  FL.Add('vo := 2;');
  FL.Add('');
  FL.Add('Name := T3;');
  FL.Add('vo := -3;');
  FL.Add('');
  FL.Add('Name := T4;');
  FL.Add('vo := 15;');
  FL.Add('wl := 4;');

  FL.Add('Name := T5;');
  FL.Add('vo := 5;');
  FL.Add('sa := 5;');

  FL.Add('Name := T6;');
  FL.Add('sh := 6;');
  FL.Add('');
  ReadTrimmFile(FL);
  FL.Clear;
end;

end.
