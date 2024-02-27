unit SphericalCamera;

interface

uses  System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  CastleViewport, CastleCameras, CastleProjection, CastleTransform,
  SpritelyTypes;

type
  TSphericalCamera = class(TCastleTransform)
  private
    fInclinationTransform: TCastleTransform;
    fCamera: TCastleCamera;
    FProjectionType: TProjectionType;
    fLookAt: TVector3;
    procedure SetProjectionType(const AProjectionType: TProjectionType);
    function GetProjectionType: TProjectionType;
    function GetPerspective: TCastlePerspective;
    function GetOrthographic: TCastleOrthographic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure Add(const Item: TCastleTransform);
    procedure ViewFromSphere(const ARadius, AzimuthValue, InclinationValue: Single);
    property ProjectionType: TProjectionType read GetProjectionType write SetProjectionType default ptPerspective;
    property Perspective: TCastlePerspective read GetPerspective;
    property Orthographic: TCastleOrthographic read GetOrthographic;
    property LookAt: TVector3 read fLookAt write fLookAt;
    property Camera: TCastleCamera read fCamera write fCamera;
  end;

implementation
  uses CastleLog;

{ TSphericalCamera }
{
procedure TSphericalCamera.Add(const Item: TCastleTransform);
begin
  fCamera.Add(Item);
end;
}
constructor TSphericalCamera.Create(AOwner: TComponent);
begin
  inherited;
  fInclinationTransform := TCastleTransform.Create(Self);
  fCamera := TCastleCamera.Create(Self);
  fInclinationTransform.Add(fCamera);
  Add(fInclinationTransform);
  fCamera.Translation := Vector3(0,0,2);
  fLookAt := Vector3(0,0,0);
end;

destructor TSphericalCamera.Destroy;
begin

  inherited;
end;

function TSphericalCamera.GetOrthographic: TCastleOrthographic;
begin
  Result := fCamera.Orthographic;
end;

function TSphericalCamera.GetPerspective: TCastlePerspective;
begin
  Result := fCamera.Perspective;
end;

function TSphericalCamera.GetProjectionType: TProjectionType;
begin
  Result := fCamera.ProjectionType;
end;

procedure TSphericalCamera.SetProjectionType(
  const AProjectionType: TProjectionType);
begin
  if fCamera.ProjectionType <> AProjectionType then
    fCamera.ProjectionType := AProjectionType;
end;

procedure TSphericalCamera.ViewFromSphere(const ARadius: Single; const AzimuthValue: Single; const InclinationValue : Single);
begin
//  WriteLnLog('Radius = ' + FloatToStr(ARadius) + ', Azi = ' + FloatToStr(AzimuthValue) + ', Inc = ' + FloatToStr(InclinationValue));
  fCamera.Translation := Vector3(0, 0, ARadius);
  Rotation := Vector4(0,1,0,AzimuthValue);
  fInclinationTransform.Rotation := Vector4(1,0,0,InclinationValue);
end;


end.
