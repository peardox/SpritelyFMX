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
  SphericalCamera,
  SpritelyTypes;

type
  TCastleApp = class(TCastleView)
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override; // TCastleUserInterface
    procedure Start; override; // TCastleView
    procedure Stop; override; // TCastleView
    procedure Resize; override; // TCastleUserInterface
    procedure RenderOverChildren; override; // TCastleUserInterface
    procedure Render; override;
    procedure BeforeRender; override;
  private
    { Private declarations }
    fZoomFactor2D: Single;
    fZoomFactor3D: Single;
    fStage: TCastleModel;
    fUse3D: Boolean;
    fCamera: TSphericalCamera;
    fAzimuth: Single;
    fInclination: Single;
    CameraLight: TCastleDirectionalLight;
    fViewport: TCastleViewport;
    fModels: TModelPack;
    FDoExtMessage: TPDXMessageEvent;
    FDoOnModel: TPDXModelEvent;
    FSelectedModel: TCastleModel;
    fCamWidth: Single;
    fCamHeight: Single;
    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    procedure SetDoOnModel(const AProc: TPDXModelEvent);
    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure DoOnModel(const AModel: TCastleModel);
    procedure LoadViewport;
    function GetSelectedModel: TCastleModel;
    procedure SetSelectedModel(const AModel: TCastleModel);
    procedure DrawAxis(const ViewStats: TViewStats);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwitchView3D(const Use3D: Boolean);
    procedure AddModel(const AFilename: String);
    procedure RemoveModel(const AModel: TCastleModel);
    procedure RemoveModels;
    function UnderMouse: TCastleTransform;
    function FitCam: Single;
    procedure DeselectModels;
    procedure ZoomOut(const factor: Integer = 1);
    procedure ZoomIn(const factor: Integer = 1);
    function GetAxis(const AModel: TCastleModel): TViewStats;
    property Models: TModelPack read fModels write fModels;
    property OnExtMessage: TPDXMessageEvent read FDoExtMessage write SetDoExtMessage;
    property OnModel: TPDXModelEvent read FDoOnModel write SetDoOnModel;
    property SelectedModel: TCastleModel read GetSelectedModel write SetSelectedModel;
    property Camera: TSphericalCamera read fCamera write fCamera;
    property Azimuth: Single read fAzimuth write fAzimuth;
    property Inclination: Single read fInclination write fInclination;
    property Zoom: Single read fZoomFactor2D write fZoomFactor2D;
    property Viewport: TCastleViewport read fViewport write fViewport;
    property Stage: TCastleModel read fStage write fStage;
    property CamHeight: Single read fCamHeight write fCamHeight;
    property CamWidth: Single read fCamWidth write fCamWidth;
  end;


implementation

uses
  Math,
  X3DLoad,
  CastleLog,
  CastleDebugTransform,
  CastleRectangles,
  SpritelySettings,
  CastleProjection;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
  fZoomFactor2D := 0.5;
  fZoomFactor3D := 2.0;
  fModels := TModelPack.Create(Self);
  fAzimuth := 0;
  fInclination := 0;
  fCamWidth := 0.8;
  fCamHeight := 1;
end;

procedure TCastleApp.Start;
var
  model: TCastleModel;
begin
  inherited;
  LoadViewport;
  if(SystemSettings.LastModel <> EmptyStr) and (FileExists(SystemSettings.LastModel)) then
    begin
      model := fModels.AddModel(SystemSettings.LastModel, ModelAlignCenter);
//      model.ShowDebugBox(False);
      fStage.Add(model);
      model.SelectModel;
      DoOnModel(model);
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


function TCastleApp.UnderMouse: TCastleTransform;
var
  model: TCastleTransform;
begin
  Result := Nil;
  model := fViewport.TransformUnderMouse;
  if model is TCastleModel then
    begin
      Result := model;
    end;
end;

procedure TCastleApp.ZoomIn(const factor: Integer);
begin

  if fCamera.ProjectionType = ptPerspective then
    begin
      fZoomFactor3D := fZoomFactor3D + (0.1 * factor);
      SendMessage('Zoom = ' + FloatToStr(fZoomFactor3D));
    end
  else
    begin
      fZoomFactor2D := fZoomFactor2D + (0.1 * factor);
      SendMessage('Zoom = ' + FloatToStr(fZoomFactor2D));
    end;
end;

procedure TCastleApp.ZoomOut(const factor: Integer);
begin

  if fCamera.ProjectionType = ptPerspective then
    begin
      fZoomFactor3D := fZoomFactor3D - (0.1 * factor);
      if fZoomFactor3D < 0 then
        fZoomFactor3D := 0.00001;
      SendMessage('Zoom = ' + FloatToStr(fZoomFactor3D));
    end
  else
    begin
      fZoomFactor2D := fZoomFactor2D - (0.1 * factor);
      if fZoomFactor2D < 0 then
        fZoomFactor2D := 0.00001;
      SendMessage('Zoom = ' + FloatToStr(fZoomFactor2D));
    end;
end;

procedure TCastleApp.LoadViewport;
var
  bb: TDebugTransformBox;
begin
  fViewport := TCastleViewport.Create(Self);
  fViewport.FullSize := False;
  fViewport.Width := Container.UnscaledWidth;
  fViewport.Height := Container.UnscaledHeight;
  fViewport.Transparent := True;

  fStage := TCastleModel.Create(Self);

  bb := TDebugTransformBox.Create(Self);
  bb.Parent := fStage;
  bb.BoxColor := Orange;
  bb.Exists := True;

  fViewport.Items.Add(fStage);

  fCamera := TSphericalCamera.Create(fViewport);
//  fCamera.ProjectionType := ptPerspective; // Default

  CameraLight := CreateDirectionalLight(Vector3(0,0,1));
  fCamera.Camera.Add(CameraLight);

  fViewport.Items.Add(fCamera);
  fViewport.Camera := fCamera.Camera;

  InsertFront(fViewport);

end;

procedure TCastleApp.Resize;
begin
  inherited;
  fViewport.Width := Container.UnscaledWidth;
  fViewport.Height := Container.UnscaledHeight;
end;

procedure TCastleApp.SwitchView3D(const Use3D: Boolean);
begin
  fUse3D := Use3D;
end;


procedure TCastleApp.AddModel(const AFilename: String);
var
  model: TCastleModel;
begin
  if Assigned(fStage) and Assigned(fModels) and FileExists(AFilename) then
    begin
      model := fModels.AddModel(AFilename);
//      model.ShowDebugBox(True);
      fStage.Add(model);
      model.SelectModel;
    end;
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

procedure TCastleApp.DeselectModels;
begin
  FSelectedModel := Nil;
end;

destructor TCastleApp.Destroy;
begin
  fModels.Free;
  inherited;
end;

procedure TCastleApp.DoOnModel(const AModel: TCastleModel);
begin
  if Assigned(FDoOnModel) then
    FDoOnModel(Self, AModel);
end;

function TCastleApp.GetSelectedModel: TCastleModel;
begin
  Result := Nil;
  if Assigned(FSelectedModel) then
    Result := FSelectedModel;
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

procedure TCastleApp.SetDoOnModel(const AProc: TPDXModelEvent);
begin
  FDoOnModel := AProc;
end;

procedure TCastleApp.SetSelectedModel(const AModel: TCastleModel);
begin
  FSelectedModel := AModel;
end;

procedure TCastleApp.RemoveModel(const AModel: TCastleModel);
begin
  fStage.Remove(AModel);
end;

procedure TCastleApp.RemoveModels;
var
 I: Integer;
begin
  DeselectModels;
  if Assigned(fStage) and Assigned(fModels) then
    begin
      for I := 0 to fModels.kids.Count - 1 do
        begin
          RemoveModel(fModels.kids[I]);
        end;
      fModels.kids.Clear;
    end;
end;

procedure TCastleApp.BeforeRender;
var
  fc: Single;
begin
  inherited;
  Resize;
  if fUse3D then
    begin
      if fCamera.ProjectionType <> ptPerspective then
        begin
          fCamera.ProjectionType := ptPerspective;
        end;
      fCamera.ViewFromSphere(fZoomFactor3D, fAzimuth, fInclination);
    end
  else
    begin
      if fCamera.ProjectionType <> ptOrthographic then
        begin
          fviewport.Setup2D;
          fCamera.ProjectionType := ptOrthographic;
//          fc := FitCam;
//          fCamera.Orthographic.Height := fCamHeight;
//          fCamera.Orthographic.Width := fZoomFactor2D;
          fCamera.Orthographic.Origin := Vector2(0.5, 0.5);
//          fCamera.Translation := Vector3(0.5, 0.5, 0.5);
        end;
      fCamera.Orthographic.Width := fZoomFactor2D;
      fCamera.Orthographic.Height := fZoomFactor2D;
      if fStage.BoundingBox.IsEmptyOrZero then
        fCamera.ViewFromSphere(1, fAzimuth, fInclination)
      else
        fCamera.ViewFromSphere(1, fAzimuth, fInclination, fStage.BoundingBox.Center);
    end;
end;

function TCastleApp.FitCam: Single;
var
  E: TViewStats;
  nff: Single;
begin
  nff := 1;
  if (fModels.Kids.Count > 0) then
    begin
      E := GetAxis(fStage);
      if E.IsValid then
        begin
//            fZoomFactor2D := fZoomFactor2D / (Max(E.Size.X, E.Size.Y) * 1);
//        fZoomFactor2D := (Min(E.Size.X, E.Size.Y) / 2) * fStage.NormalScale;
//          fZoomFactor2D := (E.Box.View2D.Height / Container.UnscaledHeight) * 1;
//          nff := (Container.UnscaledHeight / E.Box.View2D.Height);
          nff := (Container.UnscaledWidth / E.Box.View2D.Width);
          WriteLnLog(Format('SH = %2.8f,SW = %2.8f,CH =  %2.8f,CW = %2.8f,N = %2.8f',[E.Box.View2D.Height, E.Box.View2D.Width, Container.UnscaledHeight, Container.UnscaledWidth,nff]));
//          fZoomFactor2D := nff;
        end;
    end;
    Result := nff;
end;

procedure TCastleApp.Render;
begin
  inherited;

  if Assigned(fStage) then
    DrawAxis(GetAxis(fStage));
end;

procedure TCastleApp.RenderOverChildren;
begin
  inherited;
{
  if Assigned(fStage) then
    DrawAxis(GetAxis(fStage));

//    WriteLnLog(Format('Model = S = %s, NS = %s',[fStage.Scale.ToString, fStage.NormalScale.ToString]));

  if Assigned(fSelectedModel) then
    DrawAxis(GetAxis(fSelectedModel));
}
end;

function TCastleApp.GetAxis(const AModel: TCastleModel): TViewStats;
var
  Points: array[0..3] of TVector2;
  TR, BL: TVector2;
  SX, SY: Single;
  Extents: TExtents;
  RX, RY: TVector2;
  I: Integer;
begin
  Result := Default(TViewStats);

  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  if Assigned(AModel) then
    begin
    Extents := Viewport.CalcAngles(AModel);
    if Extents.isValid then
      begin
        BL := Viewport.WorldToViewport(AModel, Extents.Min);
        TR := Viewport.WorldToViewport(AModel, Extents.Max);
        SX := TR.X - BL.X;
        SY := TR.Y - BL.Y;

        Result.Box.View2D := FloatRectangle(BL, SX, SY);
        Result.Box.View3D := FloatRectangle(
          Vector2(Extents.Min.X, Extents.Min.Y),
          Extents.Max.X - Extents.Min.X,
          Extents.Max.Y - Extents.Min.Y);

        Result.GroundRect[0] := Viewport.WorldToViewport(AModel, Extents.corners[0]);
        Result.GroundRect[1] := Viewport.WorldToViewport(AModel, Extents.corners[1]);
        Result.GroundRect[2] := Viewport.WorldToViewport(AModel, Extents.corners[5]);
        Result.GroundRect[3] := Viewport.WorldToViewport(AModel, Extents.corners[4]);

        RX := Vector2(9999999, 9999999);
        RY := Vector2(-9999999, -9999999);

        for I := 0 to Length(Result.GroundRect) - 1 do
            begin
              if Result.GroundRect[I].X < RX.X then
                RX := Result.GroundRect[I];
              if Result.GroundRect[I].Y > RY.Y then
                RY := Result.GroundRect[I];
            end;
        Result.Diagonal := Vector2(abs(RX.X - RY.X),abs(RX.Y - RY.Y));
        if(Result.Diagonal.X > 1) and (Result.Diagonal.Y > 1) then
          begin
            Result.DyDx := (Result.Diagonal.Y / Result.Diagonal.X);
          end
        else
          Result.DyDx := 0;

        if (Result.Box.View2D.Width > 1) and (Result.Box.View2D.Height > 1) then
          Result.isValid := True;
      end;
    end;
end;

procedure TCastleApp.DrawAxis(const ViewStats: TViewStats);
begin
  if ViewStats.isValid then
    begin
      DrawPrimitive2D(pmLineLoop, ViewStats.GroundRect, Red);
      DrawRectangleOutline(ViewStats.Box.View2D, Yellow);
    end;
end;

end.

