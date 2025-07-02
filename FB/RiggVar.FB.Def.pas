unit RiggVar.FB.Def;

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
  RiggVar.FB.DefConst;

//{$I App\RiggVar.App.Defs.inc}

const
  BoolStr: array[Boolean] of string = ('False', 'True');
  BoolInt: array[Boolean] of Integer = (0, 1);
  BoolFloat: array[Boolean] of single = (0.0, 1.0);

type
  TSizeName = (
    ExtraSmall,
    Small,
    Medium,
    Large,
    ExtraLarge
  );

const
  SizeName: array[TSizeName] of string = ('XS', 'S', 'M',  'L', 'XL');

type
  TConnectionStatus = (csRed, csYellow, csGreen);

{$ifdef OriginalFMX}
  TViewType = (Perspective, Orthographic);
{$endif}

  TGuiAction = (
    gaAnimationStop,
    gaConnectionStatus
  );

  TFederMessageValue = (
    fmvNone,
    fmvActionReset,
    fmvActionExecute,
    fmvMessageValue,
    fmvParamValue,
    fmvActionDownload,
    fmvActionPlay
  );

  TFederMessageType = (
    fmtInt,
    fmtDouble,
    fmtBool
  );

  TBandWidthOption = (
    bwAbsolute,
    bwRelative,
    bwContour
  );

  TFederMessageKind = (
    fmkNoop,
    fmkAction,
    fmkParam,
    fmkParamValue,

    fmkPalCol,

    fmkAnimNewStory,
    fmkAnimNewEntry,
    fmkAnimParam,
    fmkAnimStartValue,
    fmkAnimStopValue,
    fmkAnimGO,
    fmkAnimAT,
    fmkAnimIT,
    fmkAnimDU,
    fmkAnimAR,
    fmkAnimLP,
    fmkAnimFC,

    fmkTX,
    fmkTY,
    fmkRX,
    fmkRY,
    fmkRZ,
    fmkCZ,

    fmkX1,
    fmkY1,
    fmkX2,
    fmkY2,
    fmkX3,
    fmkY3,
    fmkX4,
    fmkY4,

    fmkScene,
    fmkGraph,
    fmkPlot,
    fmkFigure,
    fmkBitmap,
    fmkColor,

    fmkMinusCap,
    fmkPlusCap,
    fmkMeshSize,
    fmkLevel,
    fmkHub,
    fmkSample
  );

  TFederParam = (
    fpController,
    fpWinkel,
    fpVorstag,
    fpWante,
    fpWoben,
    fpSalingH,
    fpSalingA,
    fpSalingL,
    fpVorstagOS,
    fpWPowerOS,
    fpSalingW,
    fpMastfallF0C,
    fpMastfallF0F,
    fpMastfallVorlauf,
    fpBiegung,
    fpD0X,
    fpAPW,
    fpEAH,
    fpEAR,
    fpEI,

    fpDiscreteValue,

    fpx1,
    fpx2,
    fpx3,
    fpx4,

    fpy1,
    fpy2,
    fpy3,
    fpy4,

    fpZ,
    fpz1,
    fpz2,
    fpz3,
    fpz4,

    fpL,
    fpl1,
    fpl2,
    fpl3,
    fpl4,

    fpK,
    fpk1,
    fpk2,
    fpk3,
    fpk4,

    fpox,
    fpoy,
    fpoz,

    fpAttenuation,
    fpGrenze,
    fpRange,
    fpAbsoluteRange,

    fpT,
    fpt1,
    fpt2,
    fpt3,
    fpt4,

    fptrx,
    fptry,
    fptrt,
    fptrs,

    fprx,
    fpry,
    fprz,

    fptx,
    fpty,
    fpcz,

    fph, //Hub
    fps, //Sample
    fppa, //Parallel Animation

    fpiv,
    fpiw,
    fpjv,
    fpjw,

    fppx,
    fppy,
    fpva,
    fpnp,
    fpfp,

    fp0,
    fp1,
    fp2,
    fp3,
    fp4,
    fp5,
    fp6,
    fp7,
    fp8,
    fp9,

    fpbpr,
    fpbpx,
    fpbpy,
    fpbpa,
    fpbs1,
    fpbs2,
    fpbcf,
    fpbcd,
    fpbcz,

    fporx,
    fpory,
    fporz,

    fpl1x,
    fpl1y,
    fpl1z,

    fpl2x,
    fpl2y,
    fpl2z,

    fpl3x,
    fpl3y,
    fpl3z,

    fpl4x,
    fpl4y,
    fpl4z,

    fpBandSelected,

    fpBandCount,
    fpBandDistributionX,
    fpBandDistributionY,

    fpBandWidth,

    fpLabelTextX,
    fpLabelTextY,
    fpLabelTextZ,

    fpPan,

    fpx12,
    fpy12,
    fpz12,

    fpParamSliceHeight,
    fpParamCapValue,

    fpParamY3f,
    fpParamL3f,
    fpParamLf,

    fpCoord0,
    fpCoord1,
    fpCoord2,
    fpCoord3,

    fpUnknown
  );

  TTrackingEvent = procedure(k: TFederParam; v: Integer) of object;
  TAbsoluteEvent = procedure(k: TFederParam; v: Integer) of object;
  TRelativeEvent = procedure(k: TFederMessageKind; v: Integer) of object;
  TRecorderEvent = procedure(k: string; v: Integer) of object;
  TMessageEvent = procedure(s: string) of object;

  TFederUtils = class
  public
    class function StrToParamInt(s: string): Integer;
    class function StrToParamIndex(s: string): Integer;
    class function StrToColorIndex(s: string): Integer;

    class function GetParamCaption(fp: TFederParam): string;
    class function GetParamInt(fp: TFederParam): Integer;
    class function GetParamIndex(fp: TFederParam): Integer;
    class function IsViewParam(fp: TFederParam): Boolean; overload;
    class function IsViewParam(fp: Integer): Boolean; overload;
    class function GetFederParam(idx: Integer): TFederParam;
    class function IsRggParam(fp: Integer): Boolean; overload;

    class function IsDiscreteParam(fmk: TFederMessageKind): Boolean;
    class function GetMessageKindInt(fmk: TFederMessageKind): Integer;
    class function GetMessageKindString(mk: TFederMessageKind): string;
    class function GetMessageKindLabel(mk: TFederMessageKind): string;
  end;

  TFederMeshType =
  (
    fmtLinearK,
    fmtLinearP,
    fmtNonlinearK,
    fmtNonlinearP,
    fmtFilteredK
  );

implementation

{ TFederUtils }

class function TFederUtils.StrToColorIndex(s: string): Integer;
var
  c: char;
begin
  if s = '' then
    result := 0
  else
  begin
    c := LowerCase(s)[1];
    if c = 'w' then
      result := 0 //white
    else if c = 'r' then
      result := 1 //red
    else if c = 'g' then
      result := 2 //green
    else if c = 'b' then
      result := 3 //blue
    else if c = 'm' then
      result := 4 //magenta
    else if c = 'c' then
      result := 5 //cyan
    else if c = 'y' then
      result := 6 //yellow
    else
      result := 0;
  end;
end;

class function TFederUtils.StrToParamInt(s: string): Integer;
var
  fp: TFederParam;
begin
  result := 0;
  for fp := Low(TFederParam) to High(TFederParam) do
    if Lowercase(GetParamCaption(fp)) = Lowercase(s) then
    begin
      result := GetParamIndex(fp);
      break;
    end;
end;

class function TFederUtils.StrToParamIndex(s: string): Integer;
begin
  if s = 'x' then
    result := GetParamIndex(fprx)
  else if s = 'y' then
    result := GetParamIndex(fpry)
  else if s = 'z' then
    result := GetParamIndex(fpcz)
  else
    result := StrToParamInt(s);
end;

class function TFederUtils.IsDiscreteParam(fmk: TFederMessageKind): Boolean;
begin
  case fmk of
    fmkScene,
    fmkGraph,
    fmkPlot,
    fmkFigure,
    fmkBitmap,
    fmkColor,
    fmkMinusCap,
    fmkPlusCap,
    fmkMeshSize,
    fmkAction,
    fmkLevel,
    fmkHub,
    fmkSample: result := True;
    else
      result := False;
  end;
end;

class function TFederUtils.IsRggParam(fp: Integer): Boolean;
begin
  result := TFederParam(fp) in [fpController .. fpD0X];
end;

class function TFederUtils.IsViewParam(fp: Integer): Boolean;
begin
  result := IsViewParam(GetFederParam(fp));
end;

class function TFederUtils.IsViewParam(fp: TFederParam): Boolean;
begin
  case fp of
    fprx,
    fpry,
    fprz,
    fptx,
    fpty,
    fpcz,
    fppx,
    fppy,
    fpva,
    fpnp,
    fpfp,
    fporx,
    fpory,
    fporz
    : result := True;

    else
      result := false;
  end;
end;

class function TFederUtils.GetFederParam(idx: Integer): TFederParam;
var
  j: SmallInt;
begin
  j := Integer(High(TFederParam));
  if idx > j then
    result := fpUnknown
  else if idx < 0 then
    result := fpUnknown
  else
    result := TFederParam(idx);

end;

class function TFederUtils.GetParamInt(fp: TFederParam): Integer;
begin
  result := Integer(fp);
end;

class function TFederUtils.GetParamIndex(fp: TFederParam): Integer;
begin
  result := GetParamInt(fp) - 1;
end;

class function TFederUtils.GetMessageKindInt(fmk: TFederMessageKind): Integer;
begin
  result := Integer(fmk);
end;

class function TFederUtils.GetMessageKindString(mk: TFederMessageKind): string;
begin
  case mk of
    fmkNoop: result := cNoop;
    fmkAction: result := cAction;
    fmkPalCol: result := cPalCol;
    fmkParam: result := cParam;
    fmkParamValue: result := cParamValue;

    fmkAnimNewStory: result := cAnimNewStory;
    fmkAnimNewEntry: result := cAnimNewEntry;
    fmkAnimParam: result := cAnimParam;
    fmkAnimStartValue: result := cAnimStartValue;
    fmkAnimStopValue: result := cAnimStopValue;
    fmkAnimGO: result := cAnimGO;
    fmkAnimAT: result := cAnimAnimationType;
    fmkAnimIT: result := cAnimInterpolationType;
    fmkAnimDU: result := cAnimDuration;
    fmkAnimAR: result := cAnimAutoReverse;
    fmkAnimLP: result := cAnimLoop;
    fmkAnimFC: result := cAnimFromCurrent;

    fmkTX: result := cTX;
    fmkTY: result := cTY;
    fmkRX: result := cRX;
    fmkRY: result := cRY;
    fmkRZ: result := cRZ;
    fmkCZ: result := cCZ;

    fmkX1: result := cX1;
    fmkY1: result := cY1;
    fmkX2: result := cX2;
    fmkY2: result := cY2;
    fmkX3: result := cX3;
    fmkY3: result := cY3;
    fmkX4: result := cX4;
    fmkY4: result := cY4;

    fmkScene: result := cScene;
    fmkGraph: result := cGraph;
    fmkPlot: result := cPlot;
    fmkFigure: result := cFigure;
    fmkBitmap: result := cBitmap;
    fmkColor: result := cColor;

    fmkMinusCap: result := cMinusCap;
    fmkPlusCap: result := cPlusCap;
    fmkMeshSize: result := cMeshSize;
    fmkLevel: result := cLevel;
    fmkHub: result := cHub;
    fmkSample: result := cSample;
  end;
end;

class function TFederUtils.GetMessageKindLabel(mk: TFederMessageKind): string;
begin
  case mk of
    fmkNoop: result := 'NO';

    fmkLevel: result := 'LE';
    fmkHub: result := 'HU';
    fmkSample: result := 'SA';

    fmkMeshSize: result := 'MS';

    fmkScene: result := 'SC';
    fmkGraph: result := 'GR';
    fmkPlot: result := 'PL';
    fmkFigure: result := 'FI';
    fmkBitmap: result := 'BM';
    fmkColor: result := 'CL';
    fmkParam: result := 'PA';

    else
      result := '--'
  end;
end;

class function TFederUtils.GetParamCaption(fp: TFederParam): string;
begin
  case fp of
    fpL: result := 'L';
    fpl1: result := cl1;
    fpl2: result := cl2;
    fpl3: result := cl3;
    fpl4: result := cl4;

    fpK: result := 'K';
    fpk1: result := ck1;
    fpk2: result := ck2;
    fpk3: result := ck3;
    fpk4: result := ck4;

    fpox: result := 'ox';
    fpoy: result := 'oy';
    fpoz: result := 'oz';

    fpx1: result := cx1;
    fpx2: result := cx2;
    fpx3: result := cx3;
    fpx4: result := cx4;

    fpy1: result := cy1;
    fpy2: result := cy2;
    fpy3: result := cy3;
    fpy4: result := cy4;

    fpz: result := 'Z';
    fpz1: result := cz1;
    fpz2: result := cz2;
    fpz3: result := cz3;
    fpz4: result := cz4;

    fpAttenuation: result := 'A';
    fpGrenze: result := 'G';
    fpRange: result := 'R';
    fpAbsoluteRange: result := 'AR';

    fpT: result := 'T';
    fps: result := 's';

    fpt1: result := 't1';
    fpt2: result := 't2';
    fpt3: result := 't3';
    fpt4: result := 't4';

    fptrt: result := 'trt';
    fptrx: result := 'trx';
    fptry: result := 'try';
    fptrs: result := 'trs';

    fprx: result := cRX;
    fpry: result := cRY;
    fprz: result := cRZ;
    fptx: result := cTX;
    fpty: result := cTY;
    fpcz: result := cCZ;
    fppa: result := cPA;

    fpiv: result := civ;
    fpiw: result := ciw;
    fpjv: result := cjv;
    fpjw: result := cjw;

    fppx: result := cpx;
    fppy: result := cpy;
    fpva: result := cva;
    fpnp: result := cnp;
    fpfp: result := cfp;

    fp0: result := cp0;
    fp1: result := cp1;
    fp2: result := cp2;
    fp3: result := cp3;
    fp4: result := cp4;
    fp5: result := cp5;
    fp6: result := cp6;
    fp7: result := cp7;
    fp8: result := cp8;
    fp9: result := cp9;

    fpbpr: result := cbpr;
    fpbpx: result := cbpx;
    fpbpy: result := cbpy;
    fpbpa: result := cbpa;
    fpbs1: result := cbs1;
    fpbs2: result := cbs2;
    fpbcf: result := cbpf;
    fpbcd: result := cbpd;
    fpbcz: result := cbpz;

    fporx: result := corx;
    fpory: result := cory;
    fporz: result := corz;

    fpl1x: result := cl1x;
    fpl1y: result := cl1y;
    fpl1z: result := cl1z;

    fpl2x: result := cl2x;
    fpl2y: result := cl2y;
    fpl2z: result := cl2z;

    fpl3x: result := cl3x;
    fpl3y: result := cl3y;
    fpl3z: result := cl3z;

    fpl4x: result := cl4x;
    fpl4y: result := cl4y;
    fpl4z: result := cl4z;

    fpBandSelected: result := cParamBandSelected;

    fpBandCount: result := cParamBandCount;
    fpBandDistributionX: result := cParamBandDistributionX;
    fpBandDistributionY: result := cParamBandDistributionY;
    fpBandWidth: result := cParamBandWidth;

    fpLabelTextX: result := cParamLabelTextX;
    fpLabelTextY: result := cParamLabelTextY;
    fpLabelTextZ: result := cParamLabelTextZ;

    fpPan: result := cPan;

    fpVorstag..fpD0X: result := 'rgg';

    fpx12: result := cx12;
    fpy12: result := cy12;
    fpz12: result := cz12;

    fpParamY3f: result := 'y3f';
    fpParamL3f: result := 'l3f';
    fpParamLf: result := 'Lf';

    fpCoord0: result := 'oo0';
    fpCoord1: result := 'oo1';
    fpCoord2: result := 'oo2';
    fpCoord3: result := 'oo3';

    fpParamSliceHeight: result := 'psh';
    fpParamCapValue: result := 'pcv';

    else
      result := 'ukn';
  end;
end;

end.
