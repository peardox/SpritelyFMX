unit SpritelyTypes;

interface
uses
  System.SysUtils, CastleVectors, CastleBoxes, CastleModel, CastleRectangles;

type
  TPDXMessageEvent = procedure (Sender: TObject; const Msg: String) of object;
  TPDXModelEvent = procedure (Sender: TObject; const AModel: TCastleModel) of object;
  TPDXRotationEvent = procedure (Sender: TObject; const Rotation: Single) of object;
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

  { TViewBox }
  TViewBox = record
    View2D: TFloatRectangle;
    View3D: TFloatRectangle;
  end;

  { TOutputSize }
  TUnitConfig = record
    Width: Integer;
    Height: Integer;
    OriginX: Integer;
    OriginY: Integer;
    Aspect: Single;
  end;

  TViewStats = record
    isValid: Boolean;
    Diagonal: TVector2;
    DyDx: Single;
    Box: TViewBox;
    GroundRect: array[0..3] of TVector2;
  end;

const
  Pi2 = (Pi * 2);
  AppId = UInt32(2275430);

function UnitConfig(const AWidth: Integer = 256; const AHeight: Integer = 256; const AOriginX: Integer = 0; const AOriginY: Integer = 0): TUnitConfig;
function WrapRot(const AValue: Single): Single;

implementation

function UnitConfig(const AWidth: Integer; const AHeight: Integer; const AOriginX: Integer; const AOriginY: Integer): TUnitConfig;
begin
  with Result do
    begin
      Width := AWidth;
      Height := AHeight;
      OriginX := AOriginX;
      OriginY := AOriginY;
      if (AWidth > 0) and (AHeight > 0) then
        begin
          Aspect := AWidth / AHeight;
        end
      else
        begin
          Raise Exception.Create('Tried creating an invalid UnitConfig');
        end;
    end;
end;

function WrapRot(const AValue: Single): Single;
begin
  Result := AValue;
  while(Result > Pi2) do
    begin
      Result := Result - Pi2;
    end;
  while(Result < 0) do
    begin
      Result := Result + Pi2;
    end;
end;


end.
