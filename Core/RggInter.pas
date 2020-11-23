unit RggInter;

interface

uses
  System.Math.Vectors,
  System.Classes,
  RggTypes,
  RggScroll,
  RggDoc,
  RggTrimmTab,
  RiggVar.RG.Data;

type
  IRigg = interface
  ['{CA6D68C5-B9EC-4C80-AF51-56777A8963DA}']
    function GetRggFA: TRggFA;

    procedure SetRiggPoints(const Value: TRiggPoints);
    function GetRiggPoints: TRiggPoints;
    function GetRelaxedRiggPoints: TRiggPoints;

    function GetRiggLengths: TRiggRods;
    function GetRelaxedRiggLengths: TRiggRods;
    function GetStabKraefte: TRiggRods;

    function GetKoppelKurve: TKoordLine;
    function GetRealTrimm(Index: TTrimmIndex): single;

    function GetPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRelaxedPoint3D(Value: TRiggPoint): TPoint3D;
    function GetRiggDistance(Value: TRiggRod): single;
    function GetStabKraft(Value: TRiggRod): single;

    function GetCounterValue(Idx: Integer): Integer;
    function GetTempValue(Idx: Integer): single;

    function GetGlieder: TTrimmControls;
    procedure SetGlieder(const Value: TTrimmControls);

    function GetEA: TRiggRods;
    procedure SetEA(const Value: TRiggRods);

    function GetEI: Integer;
    procedure SetEI(const Value: Integer);

    function GetRealGlied(Index: TsbName): single;
    procedure SetRealGlied(Index: TsbName; const Value: single);

    function GetTrimmTabDaten: TTrimmTabDaten;
    function GetTrimmTabelle: TTrimmTab;

    function GetControllerTyp: TControllerTyp;
    procedure SetControllerTyp(const Value: TControllerTyp);
    function GetSalingTyp: TSalingTyp;
    procedure SetSalingTyp(const Value: TSalingTyp);
    function GetCalcTyp: TCalcTyp;
    procedure SetCalcTyp(const Value: TCalcTyp);

    function GetKorrigiert: Boolean;
    procedure SetKorrigiert(const Value: Boolean);
    function GetMastfallVorlauf: single;
    procedure SetMastfallVorlauf(const Value: single);
    function GetManipulatorMode: Boolean;
    procedure SetManipulatorMode(const Value: Boolean);

    function GetProofRequired: Boolean;
    procedure SetProofRequired(const Value: Boolean);

    function GetGetriebeOK: Boolean;
    function GetMastOK: Boolean;
    function GetRiggOK: Boolean;

    function GetGetriebeStatusText: string;
    function GetMastStatusText: string;
    function GetRiggStatusText: string;

    function GetDurchBiegungHD: single;
    function GetMastLinie: TLineDataR100;
    function GetMastBeta: single;
    function GetMastLC: single;
    function GetMastLength: single;
    function GetMastOben: single;
    function GetMastUnten: single;
    procedure SetMastLength(const Value: single);
    procedure SetMastUnten(const Value: single);
    procedure SetMastOben(const Value: single);

    procedure SetDefaultDocument;
    procedure Schnittkraefte; // ComputeSchnittKraefte or DoSchnittKraefter
    procedure Reset;
    procedure NeigeF(Mastfall: single);
    procedure BiegeUndNeigeC(MastfallC, Biegung: single);
    procedure MakeSalingHBiggerFS(SalingHplus: single);
    procedure MakeSalingLBiggerDS(SalingLplus: single);
    procedure UpdateGetriebe;
    procedure UpdateGetriebeFS;
    procedure BerechneWinkel;
    procedure UpdateRigg;
    procedure LoadFromFederData(fd: TRggData);
    procedure SaveToFederData(fd: TRggData);
    procedure GetDocument(Doc: TRggDocument);
    procedure SetDocument(Doc: TRggDocument);
    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    procedure InitFactArray;
    procedure UpdateFactArray(CurrentParam: TFederParam);
    procedure ChangeRigg(CurrentParam: TFederParam; Value: single);

    procedure WriteXml(ML: TStrings; AllTags: Boolean = False);
    procedure AusgabeText(ML: TStrings; WantAll: Boolean = True; WantForce: Boolean = False);

    property RggFA: TRggFA read GetRggFA;
    property RiggPoints: TRiggPoints read GetRiggPoints write SetRiggPoints;
    property RelaxedRiggPoints: TRiggPoints read GetRelaxedRiggPoints;
    property RiggLengths: TRiggRods read GetRiggLengths;
    property RelaxedRiggLengths: TRiggRods read GetRelaxedRiggLengths;
    property StabKraefte: TRiggRods read GetStabKraefte;

    property KoppelKurve: TKoordLine read GetKoppelKurve;
    property RealTrimm[Index: TTrimmIndex]: single read GetRealTrimm;
    property Glieder: TTrimmControls read GetGlieder write SetGlieder;
    property EA: TRiggRods read GetEA write SetEA;

    property ControllerTyp: TControllerTyp read GetControllerTyp write SetControllerTyp;
    property SalingTyp: TSalingTyp read GetSalingTyp write SetSalingTyp;
    property CalcTyp: TCalcTyp read GetCalcTyp write SetCalcTyp;

    property ManipulatorMode: Boolean read GetManipulatorMode write SetManipulatorMode;
    property Korrigiert: Boolean read GetKorrigiert write SetKorrigiert;
    property MastEI: Integer read GetEI write SetEI;
    property RealGlied[Index: TsbName]: single read GetRealGlied write SetRealGlied;
    property MastfallVorlauf: single read GetMastfallVorlauf write SetMastfallVorlauf;

    property GetriebeOK: Boolean read GetGetriebeOK;
    property MastOK: Boolean read GetMastOK;
    property RiggOK: Boolean read GetRiggOK;

    property GetriebeStatusText: string read GetGetriebeStatusText;
    property MastStatusText: string read GetMastStatusText;
    property RiggStatusText: string read GetRiggStatusText;

    property MastLinie: TLineDataR100 read GetMastLinie;
    property MastLC: single read GetMastLC;
    property MastBeta: single read GetMastBeta;
    property MastLength: single read GetMastLength write SetMastLength;
    property MastUnten: single read GetMastUnten write SetMastUnten;
    property MastOben: single read GetMastOben write SetMastOben;

    property DurchbiegungHD: single read GetDurchbiegungHD;

    property TrimmtabDaten: TTrimmTabDaten read GetTrimmTabDaten;
    property TrimmTabelle: TTrimmTab read GetTrimmTabelle;

    property ProofRequired: Boolean read GetProofRequired write SetProofRequired;
  end;

implementation

end.
