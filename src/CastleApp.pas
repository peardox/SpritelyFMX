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
  CastleModel,
  SpritelyTypes;

type
  TCastleApp = class(TCastleView)
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override; // TCastleUserInterface
    procedure Start; override; // TCastleView
    procedure Stop; override; // TCastleView
    procedure Resize; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
  private
    { Private declarations }
    fZoomFactor: Single;
    fUse3D: Boolean;
    Camera: TCastleCamera;
    CameraLight: TCastleDirectionalLight;
    Viewport: TCastleViewport;
    fModels: TModelPack;
    FDoExtMessage: TPDXMessageEvent;
    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure LoadViewport;
  public
    { Public declarations }
    Stage: TCastleModel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnExtMessage: TPDXMessageEvent read FDoExtMessage write SetDoExtMessage;
    procedure SwitchView3D(const Use3D: Boolean);
    procedure ApplyView;
    procedure ZoomOut(const factor: Integer = 1);
    procedure ZoomIn(const factor: Integer = 1);
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

  model := fModels.AddModel('castle-data:/KayKit/KayKit_Adventurers_1.0_FREE/Characters/gltf/Knight.glb');
  Stage.Add(model);

  model := fModels.AddModel('castle-data:/KayKit/KayKit_Skeletons_1.0_FREE/characters/gltf/Skeleton_Mage.glb');
  model.gimbal.Translation := Vector3(-1, 0, 0);
  Stage.Add(model);

  model := fModels.AddModel('C:\work\Assets\3D\JoseDiaz\german_shepherd\scene.gltf');
  model.gimbal.Translation := Vector3(1, 0, 0);
  model.gimbal.Scale := 0.75;
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

procedure TCastleApp.ZoomIn(const factor: Integer);
begin
  fZoomFactor := fZoomFactor + (0.1 * factor);
  ApplyView;
end;

procedure TCastleApp.ZoomOut(const factor: Integer);
begin
  fZoomFactor := fZoomFactor - (0.1 * factor);
  if fZoomFactor < 0 then
    fZoomFactor := 0.00001;
  ApplyView;
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

  CameraLight := CreateDirectionalLight(Vector3(0,0,1));
  Camera.Add(CameraLight);

  Viewport.Items.Add(Camera);
  Viewport.Camera := Camera;

  InsertFront(Viewport);
  ApplyView;
end;

procedure TCastleApp.Resize;
begin
  inherited;

  Viewport.Width := Container.UnscaledWidth;
  Viewport.Height := Container.UnscaledHeight;
  {
  if Camera.ProjectionType = ptOrthographic then
    begin
      if Viewport.Width > Viewport.Height then
        Camera.Orthographic.Height := 1
      else
        Camera.Orthographic.Width := 1;
    end;
  }
end;

procedure TCastleApp.SwitchView3D(const Use3D: Boolean);
begin
  fUse3D := Use3D;
  ApplyView;
end;


procedure TCastleApp.ApplyView;
begin
  if fUse3D then
    begin
      Camera.ProjectionType := ptPerspective;
      Camera.ViewFromRadius(fZoomFactor, Vector3(1, 1, 1));
    end
  else
    begin
      Viewport.Setup2D;
      Camera.ProjectionType := ptOrthographic;
      Camera.Orthographic.Width := fZoomFactor;
      Camera.ViewFromRadius(1, Vector3(0, 0, 1));
      Camera.Orthographic.Origin := Vector2(0.5, 0.5);
      SendMessage('Zoom = ' + FloatToStr(fZoomFactor));
      {
      SendMessage('Width  = ' + FloatToStr(Camera.Orthographic.Width));
      SendMessage('Height = ' + FloatToStr(Camera.Orthographic.Height));
      SendMessage('oE Width  = ' + FloatToStr(Camera.Orthographic.EffectiveRect.Width));
      SendMessage('oE Height = ' + FloatToStr(Camera.Orthographic.EffectiveRect.Height));
      SendMessage('Effective Width  = ' + FloatToStr(Viewport.EffectiveRect.Width));
      SendMessage('Effective Height = ' + FloatToStr(Viewport.EffectiveRect.Height));
      }
    end;
  Resize;
end;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
  fZoomFactor := 4.0;
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

procedure TCastleApp.SendMessage(const AMsg: String);
begin
  if Assigned(FDoExtMessage) then
    FDoExtMessage(Self, AMsg);
end;

procedure TCastleApp.SetDoExtMessage(const AProc: TPDXMessageEvent);
begin
  FDoExtMessage := AProc;
end;

procedure TCastleApp.Render;
var
  Points: array[0..3] of TVector2;
begin
  inherited;

  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  DrawPrimitive2D(pmLines, Points, Green);

end;

end.

