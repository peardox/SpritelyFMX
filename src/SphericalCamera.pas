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
    fPan: TVector2;
    procedure SetProjectionType(const AProjectionType: TProjectionType);
    function GetProjectionType: TProjectionType;
    function GetPerspective: TCastlePerspective;
    function GetOrthographic: TCastleOrthographic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ViewFromSphere(const ARadius, AzimuthValue, InclinationValue: Single); overload;
    procedure ViewFromSphere(const ARadius: Single; const AzimuthValue: Single; const InclinationValue : Single; const ALookAt: TVector3); overload;
    property ProjectionType: TProjectionType read GetProjectionType write SetProjectionType default ptPerspective;
    property Perspective: TCastlePerspective read GetPerspective;
    property Orthographic: TCastleOrthographic read GetOrthographic;
    property LookAt: TVector3 read fLookAt write fLookAt;
    property Camera: TCastleCamera read fCamera write fCamera;
    property Pan: TVector2 read fPan write fPan;
  end;

implementation
  uses CastleLog;

constructor TSphericalCamera.Create(AOwner: TComponent);
begin
  inherited;
  fInclinationTransform := TCastleTransform.Create(Self);
  fCamera := TCastleCamera.Create(Self);
  fInclinationTransform.Add(fCamera);
  Add(fInclinationTransform);
  fCamera.Translation := Vector3(0,0,2);
  fPan := Vector2(0,0);
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

procedure TSphericalCamera.ViewFromSphere(const ARadius: Single; const AzimuthValue: Single; const InclinationValue : Single; const ALookAt: TVector3);
begin
  fLookAt := ALookAt; // Vector3(0,0,0);
  ViewFromSphere(ARadius, AzimuthValue, InclinationValue);
end;

procedure TSphericalCamera.ViewFromSphere(const ARadius: Single; const AzimuthValue: Single; const InclinationValue : Single);
var
  pPos, pDir, pUp, pSide: TVector3;
begin
//  WriteLnLog('Radius = ' + FloatToStr(ARadius) + ', Azi = ' + FloatToStr(AzimuthValue) + ', Inc = ' + FloatToStr(InclinationValue));

  Rotation := Vector4(0,1,0,AzimuthValue);
  fInclinationTransform.Rotation := Vector4(1,0,0,InclinationValue);
  fCamera.Translation := Vector3(0, 0, ARadius) + fLookAt;
  fCamera.Direction := fLookAt - fCamera.Translation;
// fCamera.Scale := Vector3(1.5,1.5,1.5);
{
  fCamera.Position := Vector3(0, 0, ARadius);
//  fCamera.Rotation := Vector4(0,1,0,AzimuthValue);
//  fCamera.Rotation := fCamera.Rotation * Vector4(1,0,0,InclinationValue);
  fCamera.Direction := fLookAt - fCamera.Translation;
  WriteLnLog(Format('D = %s,T = %s,R =  %s,U = %s,A = %2.8f,I = %2.8f',[fCamera.Direction.ToString, fCamera.Translation.ToString, fCamera.Rotation.ToString, fCamera.Up.ToString,AzimuthValue,InclinationValue]));
}
  fCamera.GetWorldView(pPos, pDir, pUp);
  pSide := TVector3.CrossProduct(pDir, pUp);
  fCamera.SetWorldView(pPos + fPan.X * pSide + fPan.Y * pUp, pDir, pUp);
end;


end.
