unit SpritelyAxisGrid;

interface

uses System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  X3DNodes, CastleColors;
type

  TAxisGrid = class(TCastleScene)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    FMaterial: TUnlitMaterialNode;
    FAppearance: TAppearanceNode;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridStep: Integer = 1; const AGridScale: Single = 1); reintroduce; overload;
    destructor Destroy; override;
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

constructor TAxisGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAxisGrid.Destroy;
begin
  inherited;
end;

constructor TAxisGrid.Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridStep: Integer = 1; const AGridScale: Single = 1);
var
  X3DTree: TX3DRootNode;
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
