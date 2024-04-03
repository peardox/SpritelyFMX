unit SpritelyAxisGrid;

interface

uses System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  X3DNodes, CastleColors;
type

  TAxis = class(TCastleScene)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    FMaterial: TUnlitMaterialNode;
    FAppearance: TAppearanceNode;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridScale: Single = 1); reintroduce; overload;
    destructor Destroy; override;
    procedure SetGround(const AValue: Single); overload;
    procedure SetGround(const AModel: TCastleScene); overload;
  end;

  TGrid = class(TCastleScene)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    FMaterial: TUnlitMaterialNode;
    FAppearance: TAppearanceNode;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridScale: Single = 1; const AGridStep: Single = 1); reintroduce; overload;
    destructor Destroy; override;
    procedure SetGround(const AValue: Single); overload;
    procedure SetGround(const AModel: TCastleScene); overload;
  end;

  TCameraWidget = class(TCastleScene)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    FMaterial: TUnlitMaterialNode;
    FAppearance: TAppearanceNode;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AHorizontalSize: Integer = 1; const AVerticalSize: Integer = 1; const ADepth: Single = 1); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses Math, CastleLog;

constructor TAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pickable := False;
end;

destructor TAxis.Destroy;
begin
  WriteLnLog('Freeing TAxisGrid');
  inherited;
end;

procedure TAxis.SetGround(const AModel: TCastleScene);
begin
  if assigned(AModel) and not(AModel.RootNode = nil) then
    begin
    if not AModel.BoundingBox.IsEmptyOrZero then
      begin
        Translation := Vector3(0, Min(AModel.BoundingBox.Data[0].Y, AModel.BoundingBox.Data[1].Y), 0);
      end;
    end;
end;

procedure TAxis.SetGround(const AValue: Single);
begin
  Translation := Vector3(0, AValue, 0);
end;

constructor TAxis.Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridScale: Single = 1);
var
  X3DTree: TX3DRootNode;
  Points: Array of TVector3;
begin
  Create(AOwner);
  try
    FCoord := TCoordinateNode.Create;
    FCoord.SetPoint([
        Vector3(-AGridSize,  0,  0), Vector3(AGridSize, 0, 0),
        Vector3( 0, -AGridSize,  0), Vector3(0, AGridSize, 0),
        Vector3( 0,  0, -AGridSize), Vector3(0, 0, AGridSize)
      ]);

    FGeometry := TLineSetNode.Create;
    FGeometry.Mode := lmPair;
    FGeometry.Coord := FCoord;

    FMaterial := TUnlitMaterialNode.Create;
    FMaterial.EmissiveColor := Color;

    FAppearance := TAppearanceNode.Create;
    FAppearance.ShadowCaster := false;
    FAppearance.Material := FMaterial;

    FShape := TShapeNode.Create;
    FShape.Geometry := FGeometry;
    FShape.Appearance := FAppearance;

    FTransform := TTransformNode.Create;
    FTransform.AddChildren(FShape);

    FTransform.Scale := Vector3(AGridScale, AGridScale, AGridScale);

    X3DTree := TX3DRootNode.Create;
    X3DTree.AddChildren(FTransform);
    Load(X3DTree, True);
  except
    on E : Exception do
      begin
        raise Exception.Create('Error in TAxisGrid.Create : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

{ TGrid }

constructor TGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pickable := False;
end;

destructor TGrid.Destroy;
begin
  WriteLnLog('Freeing TAxisGrid');
  inherited;
end;

procedure TGrid.SetGround(const AModel: TCastleScene);
begin
  if assigned(AModel) and not(AModel.RootNode = nil) then
    begin
    if not AModel.BoundingBox.IsEmptyOrZero then
      begin
        Translation := Vector3(0, Min(AModel.BoundingBox.Data[0].Y, AModel.BoundingBox.Data[1].Y), 0);
      end;
    end;
end;

procedure TGrid.SetGround(const AValue: Single);
begin
  Translation := Vector3(0, AValue, 0);
end;

constructor TGrid.Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridScale: Single = 1; const AGridStep: Single = 1);
var
  X3DTree: TX3DRootNode;
  Points: Array of TVector3;
  I: Integer;
begin
  Create(AOwner);
  try
    SetLength(Points, (4 * ((2 * AGridSize) + 1)));
    for I := 0 to (2 * AGridSize) do
      begin
        Points[(I*2)] := Vector3(-AGridSize * AGridStep,  0,  (AGridSize - I) * AGridStep);
        Points[(I*2) + 1] := Vector3(AGridSize * AGridStep, 0, (AGridSize - I) * AGridStep);
        Points[((4 * AGridSize) + 2) + (I*2)] := Vector3((AGridSize - I) * AGridStep, 0, -AGridSize * AGridStep);
        Points[((4 * AGridSize) + 2) + (I*2) + 1] := Vector3((AGridSize - I) * AGridStep, 0, AGridSize * AGridStep);
      end;

    FCoord := TCoordinateNode.Create;
    FCoord.SetPoint(Points);

    FGeometry := TLineSetNode.Create;
    FGeometry.Mode := lmPair;
    FGeometry.Coord := FCoord;

    FMaterial := TUnlitMaterialNode.Create;
    FMaterial.EmissiveColor := Color;

    FAppearance := TAppearanceNode.Create;
    FAppearance.ShadowCaster := false;
    FAppearance.Material := FMaterial;

    FShape := TShapeNode.Create;
    FShape.Geometry := FGeometry;
    FShape.Appearance := FAppearance;

    FTransform := TTransformNode.Create;
    FTransform.AddChildren(FShape);

    FTransform.Scale := Vector3(AGridScale, AGridScale, AGridScale);

    X3DTree := TX3DRootNode.Create;
    X3DTree.AddChildren(FTransform);
    Load(X3DTree, True);
  except
    on E : Exception do
      begin
        raise Exception.Create('Error in TAxisGrid.Create : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

{ TCameraWidget }

constructor TCameraWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TCameraWidget.Create(const AOwner: TComponent;
  const Color: TCastleColorRGB; const AHorizontalSize, AVerticalSize: Integer;
  const ADepth: Single);
var
  X3DTree: TX3DRootNode;
  Aspect, HalfWidth, HalfHeight: Single;
begin
  Create(AOwner);

  if (AHorizontalSize <= 0) or (AVerticalSize <= 0) then
    raise Exception.Create('TCameraWidget must have a non-zero size');

  Aspect := AHorizontalSize / AVerticalSize;

  if Aspect > 1 then // Landscape
    begin
      HalfWidth := 0.5;
      HalfHeight := 0.5 / Aspect;
    end
  else // Portrait
    begin
      HalfHeight := 0.5;
      HalfWidth := 0.5 * Aspect;
    end;

  try
    FCoord := TCoordinateNode.Create;
    FCoord.SetPoint([
        Vector3(-HalfWidth, -HalfHeight,  0), Vector3( HalfWidth, -HalfHeight, 0),
        Vector3(-HalfWidth,  HalfHeight,  0), Vector3( HalfWidth,  HalfHeight, 0),
        Vector3(-HalfWidth, -HalfHeight,  0), Vector3(-HalfWidth,  HalfHeight, 0),
        Vector3( HalfWidth, -HalfHeight,  0), Vector3( HalfWidth,  HalfHeight, 0),

        Vector3(-HalfWidth, -HalfHeight,  0), Vector3( 0, 0, ADepth),
        Vector3(-HalfWidth,  HalfHeight,  0), Vector3( 0, 0, ADepth),
        Vector3( HalfWidth,  HalfHeight,  0), Vector3( 0, 0, ADepth),
        Vector3( HalfWidth, -HalfHeight,  0), Vector3( 0, 0, ADepth)
      ]);

    FGeometry := TLineSetNode.Create;
    FGeometry.Mode := lmPair;
    FGeometry.Coord := FCoord;
    FGeometry.Solid := True;

    FMaterial := TUnlitMaterialNode.Create;
    FMaterial.EmissiveColor := Color;

    FAppearance := TAppearanceNode.Create;
    FAppearance.ShadowCaster := false;
    FAppearance.Material := FMaterial;

    FShape := TShapeNode.Create;
    FShape.Geometry := FGeometry;
    FShape.Appearance := FAppearance;

    FTransform := TTransformNode.Create;
    FTransform.AddChildren(FShape);

    X3DTree := TX3DRootNode.Create;
    X3DTree.AddChildren(FTransform);
    Load(X3DTree, True);
  except
    on E : Exception do
      begin
        raise Exception.Create('Error in TAxisGrid.Create : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

destructor TCameraWidget.Destroy;
begin

  inherited;
end;

end.
