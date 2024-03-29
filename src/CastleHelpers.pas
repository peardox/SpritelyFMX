unit CastleHelpers;

interface

uses  System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  CastleViewport, CastleCameras, CastleProjection, CastleTransform,
  SpritelyTypes,
  CastleModel;


type
  { TCastleViewHelper }
  TCastleViewportHelper = class helper for TCastleViewport
  public
    function CalcAngles(const AScene: TCastleModel): TExtents;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector2): TVector2; overload;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector3): TVector2; overload;
  end;

function CreateDirectionalLight(const AOwner: TComponent; const LightPos: TVector3): TCastleDirectionalLight; overload;
function SummariseModel(const AFilename: String): TModelInfo;

implementation

uses Math, CastleLog;

function SummariseModel(const AFilename: String): TModelInfo;
var
  model: TCastleModel;
begin
  model := TCastleModel.Create(Nil);
  model.LoadModel(AFilename, False);
  if Assigned(model) then
    begin
      Result := model.CreateInfo;
    end
  else
    Result := Nil;
//  model.Free;
end;

function TCastleViewportHelper.CalcAngles(const AScene: TCastleModel): TExtents;
var
  OutputMatrix:TMatrix4;
  OutputPoint3D: TVector3;
  i: Integer;
  Extents: TExtents;
begin
  Extents.isValid := False;
  Extents.Min := Vector3(Infinity, Infinity, Infinity);
  Extents.Max := Vector3(-Infinity, -Infinity, -Infinity);
  Extents.Pixels.X := EffectiveWidth;
  Extents.Pixels.Y := EffectiveHeight;

  if ((EffectiveWidth > 0) and (EffectiveHeight > 0) and Assigned(AScene) and not AScene.BoundingBox.IsEmptyOrZero) then
	begin
	  AScene.LocalBoundingBox.Corners(Extents.corners);
    OutputMatrix := Camera.ProjectionMatrix * Camera.Matrix * AScene.WorldTransform;
	  for i := Low(Extents.corners) to High(Extents.corners) do
		begin
		  OutputPoint3D := OutputMatrix.MultPoint(Extents.corners[i]);
		  if OutputPoint3D.X < Extents.Min.X then
		  	Extents.Min.X := OutputPoint3D.X;
		  if OutputPoint3D.Y < Extents.Min.Y then
	  		Extents.Min.Y := OutputPoint3D.Y;
		  if OutputPoint3D.Z < Extents.Min.Z then
	  		Extents.Min.Z := OutputPoint3D.Z;
		  if OutputPoint3D.X > Extents.Max.X then
  			Extents.Max.X := OutputPoint3D.X;
		  if OutputPoint3D.Y > Extents.Max.Y then
			  Extents.Max.Y := OutputPoint3D.Y;
		  if OutputPoint3D.Z > Extents.Max.Z then
			  Extents.Max.Z := OutputPoint3D.Z;
      Extents.corners[i] := Vector3(OutputPoint3D.X, OutputPoint3D.Y, OutputPoint3D.Z);
		end;

    Extents.Aspect := EffectiveWidth / EffectiveHeight;

	  Extents.Size.X := (Extents.Max.X - Extents.Min.X);
	  Extents.Size.Y := (Extents.Max.Y - Extents.Min.Y);
	  Extents.Size.Z := (Extents.Max.Z - Extents.Min.Z);
	  Extents.Aspect := Extents.Size.X / Extents.Size.Y;

    Extents.isValid := True;

	end;

  Result := Extents;
end;

function TCastleViewportHelper.WorldToViewport(AModel: TCastleModel; AVec: TVector2): TVector2;
begin
  if(Camera.ProjectionType = ptOrthographic) then
    Result := Vector2(
      Container.UnscaledWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
      Container.UnscaledHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
    )
  else
    Result := Vector2(
      Container.UnscaledWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
      Container.UnscaledHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
    )

end;

function TCastleViewportHelper.WorldToViewport(AModel: TCastleModel; AVec: TVector3): TVector2;
begin
  if(Camera.ProjectionType = ptOrthographic) then
    Result := Vector2(
      Container.UnscaledWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
      Container.UnscaledHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
    )
  else
    Result := Vector2(
      Container.UnscaledWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
      Container.UnscaledHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
    )
end;

function CreateDirectionalLight(const AOwner: TComponent; const LightPos: TVector3): TCastleDirectionalLight;
var
  Light: TCastleDirectionalLight;
begin
  Light := TCastleDirectionalLight.Create(AOwner);

  Light.Direction := LightPos;
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;

  Result := Light;
end;


end.
