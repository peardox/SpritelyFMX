unit FrameToImage;

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
  CastleImages,
  CastleApp,
  SpritelyTypes;

type

  TSizeAndPan = record
    Size: Single;
    Pan: TVector2;
  end;

  TFrameExport = class(TComponent)
  private
    fWidth: Integer;
    fHeight: Integer;
    fViewport: TCastleViewport;
    fStage: TCastleModel;
    fCamera: TSphericalCamera;
    fCameraLight: TCastleDirectionalLight;
    fAzimuth: Single;
    fInclination: Single;
    fTransparent: Boolean;
    fImageBuffer: TCastleImage;
    function FitViewToModel(const Model: TCastleModel): Single;
  public
    procedure CreateViewport;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AWidth: Integer; const AHeight: Integer); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GrabFromCastleApp(ACastleApp: TCastleApp);
    procedure ThumbFromCastleApp(ACastleApp: TCastleApp; AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
    procedure CloneModel(const AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
    function Grab(AContainer: TCastleContainer; APresetSize: TSizeAndPan; const ShowInfo: Boolean = False): TSizeAndPan;
    procedure Save(const AFilename: String);
    property Azimuth: Single read fAzimuth write fAzimuth;
    property Inclination: Single read fInclination write fInclination;
    property Transparent: Boolean read fTransparent write fTransparent;
    property Image: TCastleImage read fImageBuffer write fImageBuffer;
  end;

implementation

uses CastleProjection, CastleGLImages, CastleRectangles, CastleLog, Math, X3DLoad;

constructor TFrameExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTransparent := True;
end;

procedure TFrameExport.CloneModel(const AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
var
{$ifdef showdebug}
  bb: TDebugTransformBox;
{$endif}
  ClonedModel: TCastleModel;
  AniName: String;
begin
  ClonedModel := AModel.Clone(fStage) as TCastleModel;
  ClonedModel.UpdateModel(AModel);
  ClonedModel.Normalize;
  ClonedModel.Translation := -ClonedModel.GetOriginOffset;
  if AniIndex >= 0 then
    begin
      AniName := ClonedModel.AnimationsList[AniIndex];
      if AniTime < 0 then
        ClonedModel.ForceAnimationPose(AniName, ClonedModel.AnimationDuration(AniName) * -AniTime, True)
      else
        ClonedModel.ForceAnimationPose(AniName, AniTime, True);
    end;
{$ifdef showdebug}
  bb := TDebugTransformBox.Create(Self);
  bb.Parent := ClonedModel;
  bb.BoxColor := Green;
  bb.Exists := True;
{$endif}
  fStage.Add(ClonedModel);
end;

procedure TFrameExport.Clear;
begin
  fStage.ClearAndFreeItems;
end;

constructor TFrameExport.Create(AOwner: TComponent; const AWidth: Integer; const AHeight: Integer);
begin
  Create(AOwner);
  fWidth := AWidth;
  fHeight := AHeight;
  CreateViewport;
end;


procedure TFrameExport.CreateViewport;
begin
  fViewport := TCastleViewport.Create(Self);
  fViewport.FullSize := False;
  fViewport.Width := fWidth;
  fViewport.Height := fHeight;
  fViewport.Transparent := True;

  fStage := TCastleModel.Create(Self);
  fViewport.Items.Add(fStage);
  fStage.Clear;

  fCamera := TSphericalCamera.Create(Self);
  fCamera.ProjectionType := ptOrthographic;
  fCamera.Orthographic.Origin := Vector2(0.5, 0.5);

  fCameraLight := CreateDirectionalLight(Self, Vector3(0,0,1));
  fCamera.Camera.Add(fCameraLight);

  fViewport.Items.Add(fCamera);

  fViewport.Camera := fCamera.Camera;
end;

destructor TFrameExport.Destroy;
begin
{$ifdef showfree}
  WriteLnLog('Freeing TFrameExport');
{$endif}
  if Assigned(fStage) then
    FreeAndNil(fStage);
  if Assigned(fCameraLight) then
    FreeAndNil(fCameraLight);
  if Assigned(fCamera) then
    FreeAndNil(fCamera);
  if Assigned(fViewport) then
    FreeAndNil(fViewport);
  if Assigned(fImageBuffer) then
    FreeAndNil(fImageBuffer);
  inherited;
end;

function TFrameExport.FitViewToModel(const Model: TCastleModel): Single;
var
  V: TViewStats;
  vertScale: Single;
  horizScale: Single;
  Scale: Single;
begin
  V := fViewport.GetAxis(Model, fWidth, fHeight);

  vertScale := V.Box.View2D.Height / fViewport.Height;
  horizScale := V.Box.View2D.Width / fViewport.Width;
  Scale := Max(horizScale, vertScale);
  Model.Translation := -Model.GetOriginOffset;
  WriteLnLog(Format('vertScale - %f, horizScale - %f, Scale - %f, VWidth - %f, VHeight - %f, BWidth - %f, BHeight - %f',[vertScale, horizScale, Scale, fViewport.Width, fViewport.Height, V.Box.View2D.Width, V.Box.View2D.Height]));
  Result := Scale;
end;


procedure TFrameExport.GrabFromCastleApp(ACastleApp: TCastleApp);
begin
  fAzimuth := ACastleApp.Azimuth;
  fInclination := ACastleApp.Inclination;
  if Assigned(ACastleApp.SelectedModel) then
    CloneModel(ACastleApp.SelectedModel);
  Grab(ACastleApp.Container, Default(TSizeAndPan));
end;

procedure TFrameExport.ThumbFromCastleApp(ACastleApp: TCastleApp; AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
begin
  fAzimuth := 0.785398185253143;
  fInclination := -0.615088999271393;
  if Assigned(AModel) then
    CloneModel(AModel, AniIndex, AniTime);
  {
  WriteLnLog('AniIndex = ' + IntToStr(AniIndex));

  if AniIndex = 16 then
    Grab(ACastleApp.Container, Default(TSizeAndPan), True)
  else
    Grab(ACastleApp.Container, Default(TSizeAndPan), True);
  }
    Grab(ACastleApp.Container, Default(TSizeAndPan));
end;

function TFrameExport.Grab(AContainer: TCastleContainer; APresetSize: TSizeAndPan; const ShowInfo: Boolean = False): TSizeAndPan;
var
  Image: TDrawableImage;
  RGBA: TRGBAlphaImage;
  ViewportRect: TRectangle;
  E: TExtents;
  S: Single;
  P: TVector2;
  Res: TSizeAndPan;
begin
  try
    Res := Default(TSizeAndPan);
    RGBA := TRGBAlphaImage.Create(fWidth, fHeight);
    RGBA.ClearAlpha(0);
    Image := TDrawableImage.Create(RGBA, true, true);

    try
      fViewport.Transparent := fTransparent;

      if fCamera.ProjectionType <> ptOrthographic then
        begin
          fCamera.ProjectionType := ptOrthographic;
        end;

      fCamera.ViewFromSphere(1, fAzimuth, fInclination);
      E := fViewport.CalcAngles(fStage);

      if APresetSize.Size = 0 then
        S := Max(E.Size.X, E.Size.Y)
      else
        S := APresetSize.Size;

      if APresetSize.Pan.IsZero then
        P := Vector2((E.Size.X / 2) + E.Min.X, (E.Size.Y / 2) + E.Min.Y)
      else
        P := APresetSize.Pan;

      Res.Size := S;
      Res.Pan := P;

//      WriteLnLog('NewSiz = ' + FloatToStr(S) + ' (' + E.Size.ToString + ')');
      fCamera.Orthographic.Width := S; // FitViewToModel(fStage); // fZoomFactor2D;
      fCamera.Orthographic.Height := S; // 0;
      fCamera.ReCenter(P);

      if ShowInfo then
        begin
          WriteLnLog('Min = ' + E.Min.ToString);
          WriteLnLog('Max = ' + E.Max.ToString);
          WriteLnLog('Siz = ' + E.Size.ToString);
          WriteLnLog('Pan = ' + P.ToString);
          WriteLnLog('Pix = ' + E.Pixels.ToString);
          WriteLnLog('Asp = ' + FloatToStr(E.Aspect));
        end;

      Image.RenderToImageBegin;
      ViewportRect := Rectangle(0, 0, fWidth, fHeight);
      AContainer.RenderControl(fViewport,ViewportRect);
      Image.RenderToImageEnd;

      try
        if fTransparent then
          fImageBuffer := Image.GetContents(TRGBAlphaImage)
        else
          fImageBuffer := Image.GetContents(TRGBImage);
      except
        on E : Exception do
          raise Exception.Create('Inner Exception ' + E.ClassName + ' - ' + E.Message);
      end;
    except
      on E : Exception do
        raise Exception.Create('Outer Exception ' + E.ClassName + ' - ' + E.Message);
    end;
  finally
    FreeAndNil(Image);
    Result := Res;
  end;

end;

procedure TFrameExport.Save(const AFilename: String);
begin
  if Assigned(fImageBuffer) then
    SaveImage(fImageBuffer, AFilename);
end;

end.
