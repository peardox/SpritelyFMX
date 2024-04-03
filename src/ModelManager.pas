unit ModelManager;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  CastleDebugTransform, CastleColors,
  CastleScene, CastleQuaternions, CastleVectors,
  SpritelyTypes,
  CastleModel;

type
  TModelManager = class(TComponent)
    fModels: TObjectList<TModelInfo>;
  private
    fHashDict: TObjectDictionary<String, TModelInfo>;
    fPackScale: Single;
    fDebugColor: TCastleColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddModel(const AFileName: String): TCastleModel;
    function AddHash(const AHash: String): Boolean;
    procedure SetDebugBoxColour(const AValue: TCastleColor);
    property Models: TObjectList<TModelInfo> read fModels write fModels;
    property PackScale: Single read fPackScale write fPackScale;
  end;


implementation

{ TModelManager }

uses
  CastleLog;

function TModelManager.AddHash(const AHash: String): Boolean;
var
  AModelInfo: TModelInfo;
begin
  if fHashDict.TryGetValue(AHash, AModelInfo) then
    begin
      WriteLnLog('Dupicate Key : ' + AHash);
      Result := True;
    end
  else
    begin
      fHashDict.Add(AHash, Nil);
    end;
  Result := False;
end;

function TModelManager.AddModel(const AFileName: String): TCastleModel;
begin
  Result := Nil;
end;

constructor TModelManager.Create(AOwner: TComponent);
begin
  inherited;
  { Create Dictionary - it doesn't own anything ATM }
  fHashDict := TObjectDictionary<String, TModelInfo>.Create([]);

end;

destructor TModelManager.Destroy;
begin
  WriteLnLog('Key Count : ' + IntToStr(fHashDict.Count));
  FreeAndNil(fHashDict);
  inherited;
end;

procedure TModelManager.SetDebugBoxColour(const AValue: TCastleColor);
begin

end;

end.
