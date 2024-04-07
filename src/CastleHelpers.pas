unit CastleHelpers;

interface

uses  System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  CastleViewport, CastleCameras, CastleProjection, CastleTransform, CastleBoxes,
  Sprite3DTypes, CastleShapes,
  CastleModel;


type
  { TCastleViewHelper }
  TCastleSceneHelper = class helper for TCastleScene
  private
    function CalculateFrameBoundingBox: TBox3D;
  public
    function FrameBoundingBox: TBox3D;
  end;

  TCastleViewportHelper = class helper for TCastleViewport
  private
  public
    function CalcAngles(const AScene: TCastleScene): TExtents;
    function FrameCalcAngles(const AScene: TCastleScene): TExtents;
    function CenterViewPan(const AModel: TCastleModel; const APan: TVector2): TVector2; overload;
    function CenterViewPan(const AModel: TCastleModel; const APan: TVector2; const ContainerWidth: Single; const ContainerHeight: Single): TVector2; overload;
    function GetAxis(const AModel: TCastleModel): TViewStats; overload;
    function GetAxis(const AModel: TCastleModel; const ContainerWidth: Single; const ContainerHeight: Single): TViewStats; overload;
    function GetAxisUnScaled(const AModel: TCastleModel; const ContainerWidth: Single; const ContainerHeight: Single): TViewBox;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector2): TVector2; overload;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector2; const ContainerWidth: Single;  const ContainerHeight: Single): TVector2; overload;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector3): TVector2; overload;
    function WorldToViewport(AModel: TCastleModel; AVec: TVector3; const ContainerWidth: Single; const ContainerHeight: Single): TVector2; overload;
    function WorldToViewportUnScaled(AModel: TCastleModel; AVec: TVector3; const ContainerWidth: Single; const ContainerHeight: Single): TVector2;
  end;

function CreateDirectionalLight(const AOwner: TComponent; const LightPos: TVector3): TCastleDirectionalLight; overload;
function LeftPad(const AValue: Integer; const Length: Integer=3; const Pad: Char = '0'): String;

implementation

uses Math, CastleRectangles, System.StrUtils, CastleLog;

function LeftPad(const AValue: Integer; const Length: Integer=3; const Pad: Char = '0'): String;
begin
   Result := RightStr(StringOfChar(Pad,Length) + IntToStr(AValue), Length );
end;

function TCastleViewportHelper.GetAxis(const AModel: TCastleModel): TViewStats;
begin
  Result := GetAxis(AModel, Container.UnscaledWidth, Container.UnscaledHeight);
end;

function TCastleViewportHelper.GetAxis(const AModel: TCastleModel; const ContainerWidth: Single; const ContainerHeight: Single): TViewStats;
var
  Points: array[0..3] of TVector2;
  TR, BL: TVector2;
  SX, SY: Single;
  Extents: TExtents;
  RX, RY: TVector2;
  I: Integer;
  LeftGround, BottomGround: Integer;
  tmp: Single;
begin
  Result := Default(TViewStats);

  Points[0] := Vector2(0, ContainerHeight / 2);
  Points[1] := Vector2(ContainerWidth, ContainerHeight / 2);
  Points[2] := Vector2(ContainerWidth / 2, 0);
  Points[3] := Vector2(ContainerWidth / 2, ContainerHeight);

  if Assigned(AModel) and not(AModel.BoundingBox.IsEmptyOrZero) then
    begin
    Extents := CalcAngles(AModel);
    if Extents.isValid then
      begin
        BL := WorldToViewport(AModel, Extents.Min, ContainerWidth, ContainerHeight);
        TR := WorldToViewport(AModel, Extents.Max, ContainerWidth, ContainerHeight);
        SX := TR.X - BL.X;
        SY := TR.Y - BL.Y;

        Result.Box.View2D := FloatRectangle(BL, SX, SY);
        Result.Box.View3D := FloatRectangle(
          Vector2(Extents.Min.X, Extents.Min.Y),
          Extents.Max.X - Extents.Min.X,
          Extents.Max.Y - Extents.Min.Y);

        Result.GroundRect[0] := WorldToViewport(AModel, Extents.corners[0], ContainerWidth, ContainerHeight);
        Result.GroundRect[1] := WorldToViewport(AModel, Extents.corners[1], ContainerWidth, ContainerHeight);
        Result.GroundRect[2] := WorldToViewport(AModel, Extents.corners[5], ContainerWidth, ContainerHeight);
        Result.GroundRect[3] := WorldToViewport(AModel, Extents.corners[4], ContainerWidth, ContainerHeight);

        RX := Vector2(9999999, 9999999);
        RY := Vector2(-9999999, -9999999);
        LeftGround := -1;
        BottomGround := -1;

        for I := 0 to Length(Result.GroundRect) - 1 do
            begin
              if Result.GroundRect[I].X < RX.X then
                RX := Result.GroundRect[I];
              if Result.GroundRect[I].Y > RY.Y then
                RY := Result.GroundRect[I];
              if I = 0 then
                begin
                  LeftGround := 0;
                  BottomGround := 0;
                end
              else
                begin
                  if LeftGround <> -1 then
                    begin
                      if Result.GroundRect[LeftGround].X > Result.GroundRect[I].X then
                        LeftGround := I
                      else if Result.GroundRect[LeftGround].X > Result.GroundRect[I].X then
                        LeftGround := -1;
                    end;

                  if BottomGround <> -1 then
                    begin
                      if Result.GroundRect[BottomGround].Y > Result.GroundRect[I].Y then
                        BottomGround := I
                      else if Result.GroundRect[BottomGround].Y > Result.GroundRect[I].Y then
                        BottomGround := -1;
                    end;

                end;
            end;
        Result.Diagonal := Vector2(abs(RX.X - RY.X),abs(RX.Y - RY.Y));
        if(LeftGround <> -1) and (BottomGround <> -1) and (LeftGround <> BottomGround) then
          begin
            tmp := abs(Result.GroundRect[LeftGround].X - Result.GroundRect[BottomGround].X);

            if tmp = 0 then
              Result.DyDx := 0
            else
              Result.DyDx := (abs(Result.GroundRect[LeftGround].Y - Result.GroundRect[BottomGround].Y) / tmp);
          end
        else
          begin
            Result.DyDx := 0;
          end;

        if (Result.Box.View2D.Width > 1) and (Result.Box.View2D.Height > 1) then
          Result.isValid := True;
      end;
    end;
end;

function TCastleViewportHelper.CenterViewPan(const AModel: TCastleModel; const APan: TVector2): TVector2;
begin
  Result := CenterViewPan(AModel, APan, Container.UnscaledWidth, Container.UnscaledHeight);
end;

function TCastleViewportHelper.CenterViewPan(const AModel: TCastleModel; const APan: TVector2; const ContainerWidth: Single; const ContainerHeight: Single): TVector2;
var
  V: TViewStats;
  OX: Single;
  NewPan: TVector2;
  PX, PY: Single;
begin
  Result := Vector2(0,0);
  V := GetAxis(AModel, ContainerWidth, ContainerHeight);
  if Camera.Orthographic.EffectiveRect.Width <> 0 then
    begin
      OX := EffectiveRect.Width / Camera.Orthographic.EffectiveRect.Width;
      if OX <> 0 then
        begin
          PX := APan.X + ((V.Box.View2D.Left - ((EffectiveRect.Width - V.Box.View2D.Width)/2)) / OX);
          PY := APan.Y + ((V.Box.View2D.Bottom - ((EffectiveRect.Height - V.Box.View2D.Height)/2)) / OX);
          NewPan := Vector2(PX, PY);
          if not NewPan.IsZero(SingleEpsilon) then
            Result := NewPan;
        end;
    end;
end;

function TCastleViewportHelper.GetAxisUnScaled(const AModel: TCastleModel; const ContainerWidth: Single; const ContainerHeight: Single): TViewBox;
var
  TR, BL: TVector2;
  SX, SY: Single;
  Extents: TExtents;
begin
  if Assigned(AModel) and not(AModel.BoundingBox.IsEmptyOrZero) then
    begin
    Extents := CalcAngles(AModel);
    if Extents.isValid then
      begin
        BL := WorldToViewportUnScaled(AModel, Extents.Min, ContainerWidth, ContainerHeight);
        TR := WorldToViewportUnScaled(AModel, Extents.Max, ContainerWidth, ContainerHeight);
        SX := TR.X - BL.X;
        SY := TR.Y - BL.Y;

        Result.View2D := FloatRectangle(BL, SX, SY);
        Result.View3D := FloatRectangle(
          Vector2(Extents.Min.X, Extents.Min.Y),
          Extents.Max.X - Extents.Min.X,
          Extents.Max.Y - Extents.Min.Y);
      end;
    end;

end;

function TCastleViewportHelper.CalcAngles(const AScene: TCastleScene): TExtents;
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

function TCastleViewportHelper.FrameCalcAngles(const AScene: TCastleScene): TExtents;
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
	  AScene.FrameBoundingBox.Corners(Extents.corners);
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
  Result := WorldToViewport(AModel, AVec, Container.UnscaledWidth, Container.UnscaledHeight);
end;

function TCastleViewportHelper.WorldToViewport(AModel: TCastleModel; AVec: TVector2; const ContainerWidth: Single; const ContainerHeight: Single): TVector2;
begin
  Result := Vector2(
    ContainerWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
    ContainerHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
  )
end;

function TCastleViewportHelper.WorldToViewport(AModel: TCastleModel; AVec: TVector3): TVector2;
begin
  Result := WorldToViewport(AModel, AVec, Container.UnscaledWidth, Container.UnscaledHeight);
end;

function TCastleViewportHelper.WorldToViewport(AModel: TCastleModel; AVec: TVector3; const ContainerWidth: Single; const ContainerHeight: Single): TVector2;
begin
  Result := Vector2(
    ContainerWidth  * ((AVec.X * AModel.NormalScale) + Camera.Orthographic.Origin.X),
    ContainerHeight * ((AVec.Y * AModel.NormalScale) + Camera.Orthographic.Origin.Y)
  )
end;

function TCastleViewportHelper.WorldToViewportUnScaled(AModel: TCastleModel; AVec: TVector3; const ContainerWidth: Single; const ContainerHeight: Single): TVector2;
begin
  Result := Vector2(
    ContainerWidth  * (AVec.X + Camera.Orthographic.Origin.X),
    ContainerHeight * (AVec.Y + Camera.Orthographic.Origin.Y)
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


{ TCastleSceneHelper }

function TCastleSceneHelper.CalculateFrameBoundingBox: TBox3D;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Result := TBox3D.Empty;
  ShapeList := Shapes.TraverseList(true);
  for Shape in ShapeList do
    Result.Include(Shape.BoundingBox);
end;

function TCastleSceneHelper.FrameBoundingBox: TBox3D;
begin
    Result := CalculateFrameBoundingBox.Transform(Transform);
end;

end.
