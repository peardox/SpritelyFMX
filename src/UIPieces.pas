unit UIPieces;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes,
  CastleVectors, CastleScene, CastleImages, CastleUIControls, X3DNodes;

type
  TUIPoly = class(TComponent)
    private
      fWidth: Single;
      fHeight: Single;
      fDepth: Single;
      procedure SetSize(const ASize: TVector3);
      function GetSize: TVector3;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function BuildBox(const AWidth: Single; const AHeight: Single; const ADepth: Single): TCastleScene; overload;
      function BuildBox(const ASize: TVector3): TCastleScene; overload;
      function BuildBox: TCastleScene; overload;
      property Width: Single read fWidth write fWidth;
      property Height: Single read fHeight write fHeight;
      property Depth: Single read fDepth write fDepth;
      property Size: TVector3 read GetSize write SetSize;
    end;

implementation

uses Math,
  CastleColors,
  CastleTransform,
  CastleURIUtils, { for URIFileExists }
  CastleRenderOptions, { for stFragment }
  X3DFields; {for TSF..... }


constructor TUIPoly.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWidth := 0.5;
  fHeight := 0.5;
  fDepth := 0.5;
end;

destructor TUIPoly.Destroy;
begin
  inherited;
end;

function TUIPoly.GetSize: TVector3;
begin
  Result := Vector3(fWidth, fHeight, fDepth);
end;

procedure TUIPoly.SetSize(const ASize: TVector3);
begin
  fWidth := ASize.X;
  fHeight := ASize.Y;
  fDepth := ASize.Z;
end;

function TUIPoly.BuildBox: TCastleScene;
begin
  Result := BuildBox(fWidth, fHeight, FDepth);
end;

function TUIPoly.BuildBox(const ASize: TVector3): TCastleScene;
begin
  Result := BuildBox(Vector3(ASize.X, ASize.Y, ASize.Z));
end;

function TUIPoly.BuildBox(const AWidth: Single; const AHeight: Single; const ADepth: Single): TCastleScene;
var
  X3DTree: TX3DRootNode;
  Shape: TShapeNode;
  Geometry: TIndexedFaceSetNode;
  Coordinate: TCoordinateNode;
  Appearance: TAppearanceNode;
  Translated: TTransformNode;
  UnlitMaterialNode: TUnlitMaterialNode;
  MatInfo: TMaterialInfo;
  vWidth, vHeight, vDepth: Single;
begin
  Result := Nil;
  vWidth := AWidth / 2;
  vHeight := AHeight / 2;
  vDepth := ADepth / 2;

  try
    { This will be used as appearance for both sphere and IndexedFaceSet }
    UnlitMaterialNode := TUnlitMaterialNode.Create;
    UnlitMaterialNode.EmissiveColor := YellowRGB;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := UnlitMaterialNode;

    { Define the front }
    Coordinate := TCoordinateNode.Create;

    Coordinate.SetPoint([
      Vector3(-vWidth, -vHeight, -vDepth),
      Vector3( vWidth, -vHeight, -vDepth),
      Vector3( vWidth,  vHeight, -vDepth),
      Vector3(-vWidth,  vHeight, -vDepth),
      Vector3(-vWidth, -vHeight,  vDepth),
      Vector3( vWidth, -vHeight,  vDepth),
      Vector3( vWidth,  vHeight,  vDepth),
      Vector3(-vWidth,  vHeight,  vDepth)
    ]);

    Geometry := TIndexedFaceSetNode.Create;
    Geometry.SetCoordIndex(
      { Two quad faces. These are just indexes for
        the array placed in Geometry.Coordinate.SetPoint . }
      [  0, 3, 2, 1, 0, -1
       , 0, 1, 5, 4, 0, -1
       , 0, 4, 7, 3, 0, -1
       , 1, 2, 6, 5, 1, -1
       , 2, 3, 7, 6, 2, -1
       , 4, 5, 6, 7, 4, -1
       ]);
    Geometry.Coord := Coordinate;

    Shape := TShapeNode.Create;
    Shape.Appearance := Appearance;
    Shape.Geometry := Geometry;
//    Shape.Geometry.Solid := False;

    Translated := TTransformNode.Create;
    Translated.Translation := Vector3(0, 0, 0);
    Translated.AddChildren(Shape);

    X3DTree := TX3DRootNode.Create;
    { Add front to tree }
    X3DTree.AddChildren(Translated);
    Result := TCastleScene.Create(Self);
    Result.Load(X3DTree, True);
  except
    on E : Exception do
      begin
        raise Exception.Create('Error in BuildCard : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;


end.

