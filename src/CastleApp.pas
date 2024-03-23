unit CastleApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  CastleUIControls, CastleVectors,
  CastleGLUtils, CastleColors,
  CastleViewport,
  CastleTransform,
  CastleDebugTransform,
  CastleScene,
  CastleHelpers,
  CastleModel,
  SphericalCamera,
  SpritelyAxisGrid,
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
    fUniverse: TCastleScene;
    fStage: TCastleModel;
    fUse3D: Boolean;
    fStageMultipleModels: Boolean;
    fCamera: TSphericalCamera;
    fAzimuth: Single;
    fInclination: Single;
    fCameraLight: TCastleDirectionalLight;
    fViewport: TCastleViewport;
    fModels: TModelPack;
    FDoExtMessage: TPDXMessageEvent;
    FDoOnModel: TPDXModelEvent;
    FSelectedModel: TCastleModel;
    fWaitingModel: TCastleModel;
    fCamWidth: Single;
    fCamHeight: Single;
    fAxis: TAxisGrid;
    fIsReady: Boolean;
    fWaitingToFit: Boolean;
    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    procedure SetDoOnModel(const AProc: TPDXModelEvent);
//    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure DoOnModel(const AModel: TCastleModel);
    procedure LoadViewport;
    function GetSelectedModel: TCastleModel;
    procedure SetSelectedModel(const AModel: TCastleModel);
    procedure DrawAxis(const ViewStats: TViewStats);
    function GetZoom: Single;
    procedure SetZoom(const AValue: Single);
    function GetFOV: Single;
    procedure SetFOV(const AValue: Single);
    procedure StageApplyBox;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwitchView3D(const Use3D: Boolean);
    function AddModel(const AFilename: String): TCastleModel;
    function CloneModel(const AModel: TCastleModel): TCastleModel;
    procedure RemoveModel(const AModel: TCastleModel);
    procedure RemoveModels;
    procedure ResizeView;
    function  GetExtents: TExtents;
    function UnderMouse: TCastleTransform;
    procedure FitViewToModel(const Model: TCastleModel);
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
    property Zoom: Single read GetZoom write SetZoom;
    property Viewport: TCastleViewport read fViewport write fViewport;
    property Stage: TCastleModel read fStage write fStage;
    property CamHeight: Single read fCamHeight write fCamHeight;
    property CamWidth: Single read fCamWidth write fCamWidth;
    property FieldOfView: Single read GetFOV write SetFOV;
    property IsReady: Boolean read fIsReady;

  end;

const
  DEFAULT_MODEL: String = 'castle-data:/up.glb';

implementation

uses
  Math,
  X3DLoad,
  CastleLog,
  CastleUriUtils,
  CastleRectangles,
  SpritelySettings,
  CastleProjection;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
  fZoomFactor2D := 2.0;
  fZoomFactor3D := 75.0;
  fIsReady := False;
  fWaitingToFit := False;
  fWaitingModel := Nil;
  fModels := TModelPack.Create(Self);
  fStageMultipleModels := False;
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
      model := fModels.AddModel(SystemSettings.LastModel);
      fStage.Add(model);
      fAxis.SetGround(model);
      model.SelectModel;
      DoOnModel(model);
      FitViewToModel(model);
    end
  else if UriFileExists(DEFAULT_MODEL) then
    begin
      model := fModels.AddModel(DEFAULT_MODEL);
      fStage.Add(model);
      fAxis.SetGround(model);
      model.SelectModel;
      DoOnModel(model);
      FitViewToModel(model);
    end;
end;

function TCastleApp.GetExtents: TExtents;
var
  E: TExtents;
begin
  if Assigned(fStage) and not(fStage.BoundingBox.IsEmptyOrZero) then
    E := fViewport.CalcAngles(fStage)
  else Result := Default(TExtents);
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


function TCastleApp.GetZoom: Single;
begin
  if fUse3D then
    Result := fZoomFactor3D
  else
    Result := fZoomFactor2D;
end;

procedure TCastleApp.SetZoom(const AValue: Single);
begin
  if fUse3D then
    fZoomFactor3D := AValue
  else
    fZoomFactor2D := AValue;
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
    end
  else
    begin
      fZoomFactor2D := fZoomFactor2D + (0.1 * factor);
    end;
end;

procedure TCastleApp.ZoomOut(const factor: Integer);
begin

  if fCamera.ProjectionType = ptPerspective then
    begin
      fZoomFactor3D := fZoomFactor3D - (0.1 * factor);
      if fZoomFactor3D < 0 then
        fZoomFactor3D := 0.00001;
    end
  else
    begin
      fZoomFactor2D := fZoomFactor2D - (0.1 * factor);
      if fZoomFactor2D < 0 then
        fZoomFactor2D := 0.00001;
    end;
end;

procedure TCastleApp.StageApplyBox;
var
  bb: TDebugTransformBox;
begin
  bb := TDebugTransformBox.Create(Self);
  bb.Parent := fStage;
  bb.BoxColor := Orange;
  bb.Exists := True;
end;

procedure TCastleApp.LoadViewport;
begin
  fViewport := TCastleViewport.Create(Self);
  fViewport.FullSize := False;
  fViewport.Width := Container.UnscaledWidth;
  fViewport.Height := Container.UnscaledHeight;
  fViewport.Transparent := True;

  fUniverse := TCastleScene.Create(Self);

  fAxis := TAxisGrid.Create(Self, Vector3(0,0,1),2);
  fUniverse.Add(fAxis);
{
  fCamWidget := TCameraWidget.Create(Self, Vector3(1,0,0),1920,1080);
  fCamWidget.Translation := Vector3(0,0,2);
  fCamWidget.Center := Vector3(0,0,-2);
  fUniverse.Add(fCamWidget);

  bbw := TDebugTransformBox.Create(Self);
  bbw.Parent := fUniverse;
  bbw.BoxColor := Vector4(1,0,1, 1);
  bbw.Exists := True;

}

  fStage := TCastleModel.Create(Self);
  StageApplyBox;

  fUniverse.Add(fStage);

  fViewport.Items.Add(fUniverse);

  fCamera := TSphericalCamera.Create(fViewport);
  fCamera.ProjectionType := ptOrthographic;
  fCamera.Orthographic.Origin := Vector2(0.5, 0.5);

  fCameraLight := CreateDirectionalLight(Self, Vector3(0,0,1));
  fCamera.Camera.Add(fCameraLight);

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

procedure TCastleApp.ResizeView;
begin
end;

procedure TCastleApp.SwitchView3D(const Use3D: Boolean);
begin
  fUse3D := Use3D;
end;


function TCastleApp.AddModel(const AFilename: String): TCastleModel;
var
  model: TCastleModel;
begin
  model := Nil;
  if Assigned(fStage) and Assigned(fModels) and FileExists(AFilename) then
    begin
      if not fStageMultipleModels then
        begin
          fStage.Clear;
          fSelectedModel := Nil;
          StageApplyBox;
        end;
      model := fModels.AddModel(AFilename);
      if fStageMultipleModels and not model.BoundingBox.IsEmptyOrZero then
        begin
          model.Translation := model.Translation + model.AlignTo(FSelectedModel, ModelAlignYBottom, Vector3(0,0,0));
        end;
      fStage.Add(model);
      model.SelectModel;
      if not fStageMultipleModels then
        begin
          fAxis.SetGround(model);
          FitViewToModel(fStage);
        end
      else
        begin
          fAxis.SetGround(fStage);
          FitViewToModel(fStage);
        end;
    end;
  Result := model;
end;

function TCastleApp.CloneModel(const AModel: TCastleModel): TCastleModel;
var
  model: TCastleModel;
begin
  Result := Nil;
  if Assigned(fStage) and Assigned(AModel) then
    begin
      if not fStageMultipleModels then
        begin
          fStage.Clear;
          fSelectedModel := Nil;
          StageApplyBox;
        end;

      model := AModel.Clone(Self) as TCastleModel;
      model.UpdateModel(AModel);
      model.Normalize;
      Result := model;
      if fStageMultipleModels and not model.BoundingBox.IsEmptyOrZero then
        begin
          model.Translation := model.Translation + model.AlignTo(FSelectedModel, ModelAlignYBottom, Vector3(0,0,0));
        end;
      fStage.Add(model);
      model.SelectModel;
      if not fStageMultipleModels then
        begin
          fAxis.SetGround(model);
          FitViewToModel(fStage);
        end
      else
        begin
          fAxis.SetGround(fStage);
          FitViewToModel(fStage);
        end;
    end;
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

procedure TCastleApp.SetFOV(const AValue: Single);
begin
  if fUse3D then
    fZoomFactor3D := fCamera.Perspective.FieldOfView
  else
    fZoomFactor2D := fCamera.Perspective.FieldOfView; // Temp duff value
end;

procedure TCastleApp.SetSelectedModel(const AModel: TCastleModel);
begin
  FSelectedModel := AModel;
  DoOnModel(FSelectedModel);
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
begin
  inherited;
  Resize;
  if fUse3D then
    begin
      if fCamera.ProjectionType <> ptPerspective then
        begin
          fCamera.ProjectionType := ptPerspective;
          fCamera.Perspective.FieldOfView := Pi / 4; // 360);
        end;
      fCamera.ViewFromSphere(fZoomFactor3D, fAzimuth, fInclination);
    end
  else
    begin
      if fCamera.ProjectionType <> ptOrthographic then
        begin
          fCamera.ProjectionType := ptOrthographic;
        end;
      fCamera.Orthographic.Width := fZoomFactor2D;
      fCamera.Orthographic.Height := 0; // fZoomFactor2D;
      fCamera.ViewFromSphere(1, fAzimuth, fInclination);
    end;

    fIsReady := True;
    if Assigned(fWaitingModel) then
      begin
        FitViewToModel(fWaitingModel);
      end;
end;

procedure TCastleApp.FitViewToModel(const Model: TCastleModel);
var
  V: TViewStats;
  vertScale: Single;
  horizScale: Single;
  Scale: Single;
begin
  if fIsReady then
    begin
      V := GetAxis(Model);

      vertScale := V.Box.View2D.Height / fViewport.Height;
      horizScale := V.Box.View2D.Width / fViewport.Width;
      Scale := Max(horizScale, vertScale);

      SetZoom(Scale * Zoom);
      fWaitingToFit := False;
      fWaitingModel := Nil;
    end
  else
    begin
      fWaitingToFit := True;
      fWaitingModel := Model
    end;
end;

procedure TCastleApp.Render;
begin
  inherited;

  if Assigned(fStage) then
    begin
      DrawAxis(GetAxis(fStage));
{
      fIsReady := True;
      if Assigned(fWaitingModel) then
        begin
          FitViewToModel(fWaitingModel);
        end;
}
    end;
end;

procedure TCastleApp.RenderOverChildren;
begin
  inherited;
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

  if Assigned(AModel) and not(AModel.BoundingBox.IsEmptyOrZero) then
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

function TCastleApp.GetFOV: Single;
begin
  if fUse3D then
    Result := fCamera.Perspective.FieldOfView
  else
    Result := fCamera.Perspective.FieldOfView; // Temp duff value
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

