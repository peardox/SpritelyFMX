unit CastleModel;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  CastleScene, CastleQuaternions, CastleVectors;

type
  TModelXAlign = (modelXLeft, modelXCenter, modelXRight);
  TModelYAlign = (modelYBottom, modelYCenter, modelYTop);
  TModelZAlign = (modelZBack, modelZCenter, modelZFront);

  TModelAlign = record
    X: TModelXAlign;
    Y: TModelYAlign;
    Z: TModelZAlign;
  end;

  TModelPack = class;

  TGimbal = class(TComponent)
  private
    fOffsetScale: TVector3;
    fOffsetTranslation: TVector3;
    fOffsetRotation: TVector4;
    fScale: Single;
    fTranslation: TVector3;
    fRotation: TVector4;
    procedure SetScale(const AValue: Single);
    procedure SetTranslation(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector4);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply;
    property Scale: Single read fScale write SetScale;
    property Translation: TVector3 read fTranslation write SetTranslation;
    property Rotation: TVector4 read fRotation write SetRotation;
  end;

  TCastleModel = class(TCastleScene)
  private
    fChildOf: TModelPack;
    fScale: Single;
    fGimbal: TGimbal;
    fAlign: TModelAlign;
  public
    Frame: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NormalScale: Single read fScale write fScale;
    function Normalize: Boolean;
    procedure LoadModel(filename: String);
    property Gimbal: TGimbal read fGimbal write fGimbal;
    property Align: TModelAlign read fAlign write fAlign;
  end;

  TModelPack = class(TComponent)
    fChildren: TObjectList<TCastleModel>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddModel(const AFileName: String): TCastleModel;
    property Kids: TObjectList<TCastleModel> read fChildren write fChildren;
  end;

implementation

uses
  Math, CastleUriUtils;

{ TCastleModel }

constructor TCastleModel.Create(AOwner: TComponent);
begin
  inherited;
  fScale := 1.0;
  fAlign.X := modelXCenter;
  fAlign.Y := modelYBottom;
  fAlign.Z := modelZCenter;
  fGimbal := TGimbal.Create(Self);
end;

destructor TCastleModel.Destroy;
begin
  fGimbal.Free;
  inherited;
end;

procedure TCastleModel.LoadModel(filename: String);
begin
  try
    if UriFileExists(filename) then
      begin
        Load(filename);
        fChildOf := Nil;
        Normalize;
      end;
  except
    on E : Exception do
      begin
        Raise Exception.Create('Error in LoadScene : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

function TCastleModel.Normalize: Boolean;
var
  BBMax: Single;
  CX, CY, CZ: Single;
begin
  Result := False;
  if not(RootNode = nil) then
    begin
    if not LocalBoundingBox.IsEmptyOrZero then
      begin
        if LocalBoundingBox.MaxSize > 0 then
          begin
            CX := 1;
            CY := 1;
            CZ := 1;

            case fAlign.X of
              modelXLeft: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X);
              modelXCenter: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2);
              modelXRight: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + LocalBoundingBox.SizeX;
            end;

            case fAlign.Y of
              modelYBottom: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y);
              modelYCenter: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2);
              modelYTop: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + LocalBoundingBox.SizeY;
            end;

            case fAlign.Z of
              modelZBack: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z);
              modelZCenter: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2);
              modelZFront: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + LocalBoundingBox.SizeZ;
            end;

            Center := Vector3(CX, CY, CZ);

            if Assigned(fGimbal) then
              begin
                fGimbal.fOffsetTranslation := Vector3(-Center.X, -Center.Y, -Center.Z);
                BBMax := LocalBoundingBox.MaxSize;
                fGimbal.fOffsetScale := Vector3(NormalScale / BBMax,
                                                NormalScale / BBMax,
                                                NormalScale / BBMax);
                fGimbal.Apply;
                Result := True;
              end
            else
              begin
                Translation := -Center;
                BBMax := LocalBoundingBox.MaxSize;
                Scale := Vector3(NormalScale / BBMax,
                                 NormalScale / BBMax,
                                 NormalScale / BBMax);
                Result := True;
              end;
          end;
      end;
    end;
end;

{ TModelPack }

function TModelPack.AddModel(const AFileName: String): TCastleModel;
var
  model: TCastleModel;
begin
  Result := Nil;
  if UriFileExists(AFileName) then
    begin
      model := TCastleModel.Create(Owner);
      try
        model.Load(AFileName);
        if Assigned(model) then
          begin
            fChildren.Add(model);
            model.Normalize;
            model.fChildOf := Self;
            Result := model;
          end;
      except
        on E : Exception do
          begin
            Raise Exception.Create('Error in LoadScene : ' + E.ClassName + ' - ' + E.Message);
           end;
      end;
    end;

end;

constructor TModelPack.Create(AOwner: TComponent);
begin
  inherited;
  fChildren := TObjectList<TCastleModel>.Create(True);
end;

destructor TModelPack.Destroy;
begin
  fChildren.Free;
  inherited;
end;

{ TGimbal }

procedure TGimbal.Apply;
begin
  if Owner is TCastleModel then
    begin
      TCastleModel(Owner).Rotation := fOffsetRotation + fRotation;
      TCastleModel(Owner).Scale := fOffsetScale * fScale;
      TCastleModel(Owner).Translation := fOffsetTranslation + fTranslation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Rotation when owner is not TCastleModel');
end;

constructor TGimbal.Create(AOwner: TComponent);
begin
  inherited;
  fScale := 1.0;
end;

destructor TGimbal.Destroy;
begin
  inherited;
end;

procedure TGimbal.SetRotation(const AValue: TVector4);
begin
  if Owner is TCastleModel then
    begin
      fRotation := AValue;
      TCastleModel(Owner).Rotation := fOffsetRotation + fRotation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Rotation when owner is not TCastleModel');
end;

procedure TGimbal.SetScale(const AValue: Single);
begin
  if Owner is TCastleModel then
    begin
      fScale := AValue;
      TCastleModel(Owner).Scale := fOffsetScale * fScale;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Scale when owner is not TCastleModel');
end;

procedure TGimbal.SetTranslation(const AValue: TVector3);
begin
  if Owner is TCastleModel then
    begin
      fTranslation := AValue;
      TCastleModel(Owner).Translation := fOffsetTranslation + fTranslation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Translation when owner is not TCastleModel');
end;

end.

