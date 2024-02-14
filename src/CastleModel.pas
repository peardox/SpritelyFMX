unit CastleModel;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  CastleScene, CastleQuaternions, CastleVectors;

type
  TModelPack = class;

  TCastleModel = class(TCastleScene)
    constructor Create(AOwner: TComponent); override;
  private
    fScale: Single;
    fOrientation: TVector4;
    fRotation: TVector4;
    fChildOf: TModelPack;
  protected
    procedure ReOrient(X, Y, Z: Integer); overload;
    procedure ReOrient(X, Y, Z: Single); overload;
    procedure SetCombiRotation(const Value: TVector4);
    procedure SetModelRotation(const Theta: Double; const AnAxis: Integer);
  public
    Frame: Integer;
    property NormalScale: Single read fScale write fScale;
    property Orientation: TVector4 read fOrientation write fOrientation;
    property CombiRotation: TVector4 read fRotation write SetCombiRotation;
    function Normalize: Boolean;
    procedure LoadModel(filename: String);
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

procedure TCastleModel.SetModelRotation(const Theta: Double; const AnAxis: Integer);
begin
  case AnAxis of
    1: Rotation :=  Vector4(1, 0, 0, Theta);
    2: Rotation :=  Vector4(0, 1, 0, Theta);
    3: Rotation :=  Vector4(0, 0, 1, Theta);
  end;
end;

procedure TCastleModel.SetCombiRotation(const Value: TVector4);
var
  Q: TQuaternion;
begin
  fRotation := Value;
  Q := QuatFromAxisAngle(fOrientation, True);
  Q := Q * QuatFromAxisAngle(fRotation);
  Rotation := Q.ToAxisAngle;
end;

procedure TCastleModel.ReOrient(X, Y, Z: Integer);
const
  Deg90: Extended = (Pi / 2);
var
  Q: TQuaternion;
begin
  Q := QuatFromAxisAngle(Vector4(0, 1, 0, 0), True);
  if X <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(1, 0, 0, X * Deg90));
  if Y <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, Y * Deg90));
  if Z <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(0, 0, 1, Z * Deg90));
  Orientation := Q.ToAxisAngle;
end;

procedure TCastleModel.ReOrient(X, Y, Z: Single);
var
  Q: TQuaternion;
begin
  Q := QuatFromAxisAngle(Vector4(0, 1, 0, 0), True);
  if X <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(1, 0, 0, X));
  if Y <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, Y));
  if Z <> 0 then
    Q := Q * QuatFromAxisAngle(Vector4(0, 0, 1, Z));
  Orientation := Q.ToAxisAngle;
end;

function TCastleModel.Normalize: Boolean;
var
  BBMax: Single;
begin
  Result := False;
  if not(RootNode = nil) then
    begin
    if not LocalBoundingBox.IsEmptyOrZero then
      begin
        if LocalBoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2),
                              Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2),
                              Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2));
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

end.

