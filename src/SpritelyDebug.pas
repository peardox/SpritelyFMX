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


implementation

{ TDebugTransformHexahedron }

procedure TDebugTransformHexahedron.InitializeNodes;
begin
  inherited;
  FOriginAxis := TDebugVariableAxis.Create(Self, YellowRGB);
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
