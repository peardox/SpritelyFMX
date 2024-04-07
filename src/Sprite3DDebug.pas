unit Sprite3DDebug;

interface

uses System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  CastleDebugTransform, X3DNodes, CastleBoxes, CastleColors;

type
  TDebugVariableAxis = class(TComponent)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure SetPosition(const Value: TVector3);
    procedure SetScaleFromBox(const Value: TBox3D);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB); reintroduce;
    property Root: TTransformNode read FTransform;
    property Visible: boolean read GetVisible write SetVisible;
    property Position: TVector3 {read GetPosition} {} write SetPosition;
    property ScaleFromBox: TBox3D {read GetScale} {} write SetScaleFromBox;
  end;

  TDebugTransformHexahedron = class(TDebugTransformBox)
  strict private
    FOriginAxis: TDebugVariableAxis;
  strict protected
    procedure InitializeNodes; override;
    procedure Update; override;
  end;

  TDebugGrid = class(TComponent)
  strict private
    FShape: TShapeNode;
    FGeometry: TLineSetNode;
    FCoord: TCoordinateNode;
    FTransform: TTransformNode;
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure SetPosition(const Value: TVector3);
    procedure SetScaleFromBox(const Value: TBox3D);
  public
    constructor Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer = 1; const AGridStep: Integer = 1; const AGridScale: Single = 1); reintroduce;
    property Root: TTransformNode read FTransform;
    property Visible: boolean read GetVisible write SetVisible;
    property Render: Boolean read GetVisible write SetVisible; {$ifdef FPC} deprecated 'use Visible'; {$endif}
    property Position: TVector3 {read GetPosition} {} write SetPosition;
    property ScaleFromBox: TBox3D {read GetScale} {} write SetScaleFromBox;
  end;

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

constructor TDebugGrid.Create(const AOwner: TComponent; const Color: TCastleColorRGB; const AGridSize: Integer; const AGridStep: Integer; const AGridScale: Single);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
//  GridLine: Integer;
//  GridPoint: Array of TVector3;
begin
  inherited Create(AOwner);

  if AGridSize < 1 then
    raise Exception.Create('TDebugGrid mulst have AGridSize >= 1');
{
  SetLength(GridPoint, ((AGridSize * 2) + 1) * 2 * 2);
  for GridLine := -AGridSize to AGridSize do
    begin
//      GridPoint[(GridSize*2)+GridLine] := Vector3(-AGridSize, 0, AGridLine);
//      GridPoint[(GridSize*2)+GridLine+1] := Vector3(AGridSize, 0, AGridLine);
    end;
}
  FCoord := TCoordinateNode.Create;
  FCoord.SetPoint([
    Vector3(-1,  0,  0), Vector3(1, 0, 0),
    Vector3( 0, -1,  0), Vector3(0, 1, 0),
    Vector3( 0,  0, -1), Vector3(0, 0, 1)
  ]);

  FGeometry := TLineSetNode.Create;
  FGeometry.Mode := lmPair;
  FGeometry.Coord := FCoord;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Appearance := Appearance;

  FTransform := TTransformNode.Create;
  FTransform.AddChildren(FShape);
end;

function TDebugGrid.GetVisible: boolean;
begin
  Result := FShape.Visible;
end;

procedure TDebugGrid.SetVisible(const Value: boolean);
begin
  FShape.Visible := Value;
end;

procedure TDebugGrid.SetPosition(const Value: TVector3);
begin
  FTransform.Translation := Value;
end;

procedure TDebugGrid.SetScaleFromBox(const Value: TBox3D);
var
  ScaleFactor: Single;
begin
  ScaleFactor := Value.AverageSize(true, 1) / 2;
  FTransform.Scale := Vector3(ScaleFactor, ScaleFactor, ScaleFactor);
end;

{ TDebugTransformHexahedron }

procedure TDebugTransformHexahedron.InitializeNodes;
begin
  inherited;
  FOriginAxis := TDebugVariableAxis.Create(Self, GreenRGB);
  ParentSpace.AddChildren(FOriginAxis.Root);
end;

procedure TDebugTransformHexahedron.Update;
var
  NewVisible: Boolean;
begin
  inherited;

  NewVisible := Parent.World <> nil;

  FOriginAxis.Visible := NewVisible;

  if NewVisible then
  begin
    // show FParent.Center
    FOriginAxis.Position := Parent.Center + Parent.Translation;
    FOriginAxis.ScaleFromBox := Parent.BoundingBox;
  end;
end;

{ TDebugVariableAxis }

constructor TDebugVariableAxis.Create(const AOwner: TComponent; const Color: TCastleColorRGB);
var
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
begin
  inherited Create(AOwner);

  FCoord := TCoordinateNode.Create;
  FCoord.SetPoint([
    Vector3(-0.5,  0,  0), Vector3(0.5, 0, 0),
    Vector3( 0, -0.5,  0), Vector3(0, 0.5, 0),
    Vector3( 0,  0, -0.5), Vector3(0, 0, 0.5)
  ]);

  FGeometry := TLineSetNode.Create;
  FGeometry.Mode := lmPair;
  FGeometry.Coord := FCoord;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := Color;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  FShape := TShapeNode.Create;
  FShape.Geometry := FGeometry;
  FShape.Appearance := Appearance;

  FTransform := TTransformNode.Create;
  FTransform.AddChildren(FShape);
end;

function TDebugVariableAxis.GetVisible: boolean;
begin
  Result := FShape.Visible;
end;

procedure TDebugVariableAxis.SetVisible(const Value: boolean);
begin
  FShape.Visible := Value;
end;

procedure TDebugVariableAxis.SetPosition(const Value: TVector3);
begin
  FTransform.Translation := Value;
end;

procedure TDebugVariableAxis.SetScaleFromBox(const Value: TBox3D);
begin
  FTransform.Scale := Vector3(Value.SizeX, Value.SizeY, Value.SizeZ);
end;

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
