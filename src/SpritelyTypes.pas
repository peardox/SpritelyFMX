unit SpritelyTypes;

interface
uses
  System.SysUtils, CastleVectors, CastleBoxes;

type
  TPDXMessageEvent = procedure (Sender: TObject; const Msg: String) of object;
  TOptionLanguage = ( enUS, ptBR, frFR, itIT, deDE, esES, ruRU, jaJP, koKR, zhCN, zhTW);

  { TExtents }
  TExtents = record
    isValid: Boolean;
    corners: TBoxCorners;
    Min: TVector3;
    Max: TVector3;
    Size: TVector3;
    Pixels: TVector2;
    Aspect: Single;
  end;

const
  Pi2 = (Pi * 2);
  AppId = UInt32(2275430);

function WrapRot(const AValue: Single): Single;


implementation

function WrapRot(const AValue: Single): Single;
begin
  Result := AValue;
  while(Result > Pi) do
    begin
      Result := Result - Pi2;
    end;
  while(Result < -Pi) do
    begin
      Result := Result + Pi2;
    end;
end;


end.
