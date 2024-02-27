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
    procedure Render; override; // TCastleUserInterface
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
    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    procedure SetDoOnModel(const AProc: TPDXModelEvent);
    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure DoOnModel(const AModel: TCastleModel);
    procedure LoadViewport;
    function GetSelectedModel: TCastleModel;
    function GetAxis(const AModel: TCastleModel): TViewStats;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwitchView3D(const Use3D: Boolean);
    procedure AddModel(const AFilename: String);
    procedure ApplyView;
    procedure DeselectModels;
    procedure ZoomOut(const factor: Integer = 1);
    procedure ZoomIn(const factor: Integer = 1);
    property Models: TModelPack read fModels write fModels;
    property OnExtMessage: TPDXMessageEvent read FDoExtMessage write SetDoExtMessage;
    property OnModel: TPDXModelEvent read FDoOnModel write SetDoOnModel;
    property SelectedModel: TCastleModel read GetSelectedModel write fSelectedModel;
    property Camera: TSphericalCamera read fCamera write fCamera;
    property Azimuth: Single read fAzimuth write fAzimuth;
    property Inclination: Single read fInclination write fInclination;
    property Viewport: TCastleViewport read fViewport write fViewport;
    property Stage: TCastleModel read fStage write fStage;
  end;


implementation

uses
  Math,
  UIPieces,
  X3DLoad,
  CastleLog,
  CastleRectangles,
  SpritelySettings,
  CastleProjection;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited;
  fZoomFactor2D := 1.0;
  fZoomFactor3D := 2.0;
  fModels := TModelPack.Create(Self);
  fAzimuth := 0;
  fInclination := 0;
end;

procedure TCastleApp.Start;
var
  model: TCastleModel;
//  rod: TUIPoly;
begin
  inherited;
  LoadViewport;
  if(SystemSettings.LastModel <> EmptyStr) and (FileExists(SystemSettings.LastModel)) then
    begin
      model := fModels.AddModel(SystemSettings.LastModel, ModelAlignCenter);
      model.ShowDebugBox(False);
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

procedure TCastleApp.ZoomIn(const factor: Integer);
begin
  if fCamera.ProjectionType = ptPerspective then
    fZoomFactor3D := fZoomFactor3D + (0.1 * factor)
  else
    fZoomFactor2D := fZoomFactor2D + (0.1 * factor);
  ApplyView;
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

  ApplyView;

end;

procedure TCastleApp.LoadViewport;
begin
  fViewport := TCastleViewport.Create(Self);
  fViewport.FullSize := False;
  fViewport.Width := Container.UnscaledWidth;
  fViewport.Height := Container.UnscaledHeight;
  fViewport.Transparent := True;

  fStage := TCastleModel.Create(Self);
  fViewport.Items.Add(fStage);

  fCamera := TSphericalCamera.Create(fViewport);
  fCamera.ProjectionType := ptOrthographic;

  CameraLight := CreateDirectionalLight(Vector3(0,0,1));
  fCamera.Camera.Add(CameraLight);

  fViewport.Items.Add(fCamera);
  fViewport.Camera := fCamera.Camera;

  InsertFront(fViewport);

  ApplyView;
end;

procedure TCastleApp.Resize;
begin
  inherited;

  fViewport.Width := Container.UnscaledWidth;
  fViewport.Height := Container.UnscaledHeight;
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


procedure TCastleApp.AddModel(const AFilename: String);
var
  model: TCastleModel;
begin
  if Assigned(fStage) and Assigned(fModels) and FileExists(AFilename) then
    begin
      model := fModels.AddModel(AFilename);
      model.ShowDebugBox(True);
      fStage.Add(model);
      model.SelectModel;
    end;
end;

procedure TCastleApp.ApplyView;
begin
  if fUse3D then
    begin
      fCamera.ProjectionType := ptPerspective;
      fCamera.ViewFromSphere(fZoomFactor3D, fAzimuth, fInclination);
    end
  else
    begin
 //     fViewport.Setup2D;

      fCamera.ProjectionType := ptOrthographic;
      fCamera.Orthographic.Width := fZoomFactor2D / 2;
      fCamera.ViewFromSphere(fZoomFactor2D, fAzimuth, fInclination);
      fCamera.Orthographic.Origin := Vector2(0.5, 0.5);
    end;
  Resize;
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

procedure TCastleApp.Render;
var
  d: TViewStats;
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
  if Assigned(fSelectedModel) then
    d := GetAxis(fSelectedModel);
  if Assigned(fStage) then
    d := GetAxis(fStage);
  {
  if(d.X > 1) and (d.Y > 1) then
    begin
      dydx := (d.Y / d.X);
      if (dydx > 0.495) and (dydx < 0.505) then
        begin
          WriteLnLog('Ground Tan = ' + d.ToString + ' = ' + FloatToStr(d.Y / d.X) + ', Inc = ' + FloatToStr(fInclination));
          SendMessage('Ground Inc = ' + FloatToStr(fInclination));
        end;
    end;
  }
end;

function TCastleApp.GetAxis(const AModel: TCastleModel): TViewStats;
var
  Points: array[0..3] of TVector2;
  GroundRect: array[0..3] of TVector2;
  BoundingRect: TFloatRectangle;
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
 // DrawPrimitive2D(pmLines, Points, Red);

  if Assigned(AModel) then
    begin
    Extents := Viewport.CalcAngles(AModel);
    if Extents.isValid then
      begin
        BL := Viewport.WorldToViewport(AModel, Extents.Min);
        TR := Viewport.WorldToViewport(AModel, Extents.Max);
        SX := TR.X - BL.X;
        SY := TR.Y - BL.Y;
        BoundingRect := FloatRectangle(BL, SX, SY);
        DrawRectangleOutline(BoundingRect, Yellow);

        GroundRect[0] := Viewport.WorldToViewport(AModel, Extents.corners[0]);
        GroundRect[1] := Viewport.WorldToViewport(AModel, Extents.corners[1]);
        GroundRect[2] := Viewport.WorldToViewport(AModel, Extents.corners[5]);
        GroundRect[3] := Viewport.WorldToViewport(AModel, Extents.corners[4]);

        RX := Vector2(9999999, 9999999);
        RY := Vector2(-9999999, -9999999);

        for I := 0 to Length(GroundRect) - 1 do
            begin
              Result.GroundRect[I] := GroundRect[I];
              if GroundRect[I].X < RX.X then
                RX := GroundRect[I];
              if GroundRect[I].Y > RY.Y then
                RY := GroundRect[I];
            end;
        Result.Diagonal := Vector2(abs(RX.X - RY.X),abs(RX.Y - RY.Y));
        Result.Box := BoundingRect;
        if(Result.Diagonal.X > 1) and (Result.Diagonal.Y > 1) then
          begin
            Result.DyDx := (Result.Diagonal.Y / Result.Diagonal.X);
          end
        else
          Result.DyDx := 0;
        Result.isValid := True;

        DrawPrimitive2D(pmLineLoop, GroundRect, Red);
      end;
    end;
end;

end.

