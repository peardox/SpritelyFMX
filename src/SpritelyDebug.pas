unit SpritelyDebug;

interface

uses System.SysUtils, System.Classes, System.Types, CastleScene, CastleVectors,
  CastleDebugTransform, X3DNodes, CastleBoxes, CastleColors, CastleLog;

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

implementation

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



end.
