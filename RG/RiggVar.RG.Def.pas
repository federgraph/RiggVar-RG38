unit RiggVar.RG.Def;

interface

uses
  System.UITypes,
  System.UIConsts,
  FMX.Graphics;

type
  TSelectedCircle = (
    scC1,
    scC2
  );

  TCircleParam = (
    fpR1,
    fpR2,
    fpM1X,
    fpM1Y,
    fpM1Z,
    fpM2X,
    fpM2Y,
    fpM2Z,
    fpA1,
    fpA2,
    fpE1,
    fpE2
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
    fpT1,
    fpT2
    );

const
  cFaktor = 'Faktor';
  cName = 'Name';

  cA0X = 'A0X';
  cA0Y = 'A0Y';
  cA0Z = 'A0Z';

  cC0X = 'C0X';
  cC0Y = 'C0Y';
  cC0Z = 'C0Z';

  cD0X = 'D0X';
  cD0Y = 'D0Y';
  cD0Z = 'D0Z';

  cE0X = 'E0X';
  cE0Y = 'E0Y';
  cE0Z = 'E0Z';

  cF0X = 'F0X';
  cF0Y = 'F0Y';
  cF0Z = 'F0Z';

  cMU = 'MU';
  cMO = 'MO';
  cML = 'ML';
  cMV = 'MV';
  cCA = 'CA';

  cCPMin = 'CPMin';
  cCPPos = 'CPPos';
  cCPMax = 'CPMax';

  cSHMin = 'SHMin';
  cSHPos = 'SHPos';
  cSHMax = 'SHMax';

  cSAMin = 'SAMin';
  cSAPos = 'SAPos';
  cSAMax = 'SAMax';

  cSLMin = 'SLMin';
  cSLPos = 'SLPos';
  cSLMax = 'SLMax';

  cSWMin = 'SWMin';
  cSWPos = 'SWPos';
  cSWMax = 'SWMax';

  cVOMin = 'VOMin';
  cVOPos = 'VOPos';
  cVOMax = 'VOMax';

  cWIMin = 'WIMin';
  cWIPos = 'WIPos';
  cWIMax = 'WIMax';

  cWLMin = 'WLMin';
  cWLPos = 'WLPos';
  cWLMax = 'WLMax';

  cWOMin = 'WOMin';
  cWOPos = 'WOPos';
  cWOMax = 'WOMax';

  cCP = 'cp';
  cSH = 'sh';
  cSA = 'sa';
//  cSL = 'sl';
//  cSW = 'sw';
  cVO = 'vo';
  cWI = 'wi';
  cWL = 'wl';
  cWO = 'wo';

// gespeicherte Basiswerte
  ch0 = 'h0';
  cl2 = 'l2'; //schon in FC
  ch2 = 'h2';

  //nicht gespeichert
  ch1 = 'h1';
  ch3 = 'h3';
  cl3 = 'l3'; //schon in FC
  cw3 = 'w3';

  //Delphi code format strings
  dsg = '%s := %g;';
  dsd = '%s := %d;';
  dss = '%s := %s;';
  dsf = '%s := %2.2f;';

  dbg = '%d := %g;';
  dbd = '%d := %d;';
  dbs = '%d := %s;';
  dbf = '%d := %2.2f;';

  //Java code format strings
  jsg = '%s = %g;';
  jsd = '%s = %d;';
  jss = '%s = %s;';
  jsf = '%s = %2.2f;';

  jbg = '%d = %g;';
  jbd = '%d = %d;';
  jbs = '%d = %s;';
  jbf = '%d = %2.2f;';

  //normal properties file format strings
  fsg = '%s=%g';
  fsd = '%s=%d';
  fss = '%s=%s';
  fsf = '%s=%2.2f';

  fbg = '%d=%g';
  fbd = '%d=%d';
  fbs = '%d=%s';
  fbf = '%d=%2.2f';

  cVersion = 'version';
  nVersion=0;

  cOffsetX = 'OffsetX';
  cOffsetY = 'OffsetY';
  cOffsetZ = 'OffsetZ';
  nOffsetX = 27;
  nOffsetY = 28;
  nOffsetZ = 29;

  claRumpf = claSilver;
  claMast = claBlue;
  claWanten = claRed;
  claVorstag = claYellow;
  claSaling = claLime;
  claController = claAqua;
  claEntspannt = claGray;
  claNullStellung = claAqua;
  claKoppelKurve = claYellow;
  claGestrichelt = TAlphaColors.Antiquewhite;
  claFixPoint = claYellow;

implementation

end.
