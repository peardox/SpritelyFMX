unit CastleApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation,
  fmx.castlecontrol, CastleUIControls, CastleVectors,
  CastleGLUtils, CastleColors, FMX.Layouts,
  CastleViewport,
  CastleTransform,
  CastleScene,
  CastleHelpers,
  CastleModel;

type
  TCastleApp = class(TCastleView)
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override; // TCastleUserInterface
    procedure Start; override; // TCastleView
    procedure Stop; override; // TCastleView
    procedure Resize; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
  private
    { Private declarations }
    Camera: TCastleCamera;
    CameraLight: TCastleDirectionalLight;
    Viewport: TCastleViewport;
    fModels: TModelPack;
    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure LoadViewport;
  public
    { Public declarations }
    Stage: TCastleModel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwitchView3D(const Use3D: Boolean);
    property Models: TModelPack read fModels write fModels;
  end;

implementation

uses
  CastleProjection;

procedure TCastleApp.Start;
var
  model: TCastleModel;
begin
  inherited;
  LoadViewport;
  Stage := TCastleModel.Create(Self);
//  Stage.LoadModel('castle-data:/KayKit/KayKit_Adventurers_1.0_FREE/Characters/gltf/Knight.glb');
  model := fModels.AddModel('castle-data:/KayKit/KayKit_Adventurers_1.0_FREE/Characters/gltf/Knight.glb');
  Stage.Add(model);
  model := fModels.AddModel('castle-data:/KayKit/KayKit_Adventurers_1.0_FREE/Characters/gltf/Barbarian.glb');
  model.Translation := model.Translation + Vector3(-1, 0, 0);
  Stage.Add(model);

  if Assigned(Stage) then
    begin
//      Stage.Normalize;
      Viewport.Items.Add(Stage);
    end;
end;

procedure TCastleApp.Stop;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
begin
  inherited;

end;

procedure TCastleApp.LoadViewport;
begin
  Viewport := TCastleViewport.Create(Self);
  Viewport.FullSize := False;
  Viewport.Width := Container.UnscaledWidth;
  Viewport.Height := Container.UnscaledHeight;
  Viewport.Transparent := True;

  Camera := TCastleCamera.Create(Viewport);
  Camera.ProjectionType := ptOrthographic;
//  Camera.ProjectionType := ptPerspective;
//  Camera.ViewFromRadius(2, Vector3(1, 1, 1));

  CameraLight := CreateDirectionalLight(Vector3(0,0,1));
  Camera.Add(CameraLight);

  Viewport.Items.Add(Camera);
  Viewport.Camera := Camera;

  InsertFront(Viewport);
end;

procedure TCastleApp.Resize;
begin
  inherited;

  Viewport.Width := Container.UnscaledWidth;
  Viewport.Height := Container.UnscaledHeight;
  if Camera.ProjectionType = ptOrthographic then
    begin
      if Viewport.Width > Viewport.Height then
        Camera.Orthographic.Height := 1
      else
        Camera.Orthographic.Width := 1;
    end;
end;

procedure TCastleApp.SwitchView3D(const Use3D: Boolean);
begin
  if Use3D then
    begin
      Camera.ProjectionType := ptPerspective;
      Camera.ViewFromRadius(8, Vector3(1, 1, 1));
    end
  else
    begin
      Viewport.Setup2D;
      Camera.ProjectionType := ptOrthographic;
      Camera.Orthographic.Width := 8;
//      Camera.Position := Vector3(0,0,0);
      Camera.ViewFromRadius(1, Vector3(0, 0, 1));
      Camera.Orthographic.Origin := Vector2(0.5, 0.5);
    end;
  Resize;
end;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
  fModels := TModelPack.Create(Self);
end;

function TCastleApp.CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
var
  Light: TCastleDirectionalLight;
begin
  Light := TCastleDirectionalLight.Create(Self);

  Light.Direction := LightPos;
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;

  Result := Light;
end;

destructor TCastleApp.Destroy;
begin
  fModels.Free;
  inherited;
end;

procedure TCastleApp.Render;
//var
//  Points: array[0..3] of TVector2;
begin
  inherited;
  {
  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  DrawPrimitive2D(pmLines, Points, Green);
  }
end;

end.

