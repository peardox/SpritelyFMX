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
  Sprite3DTypes;

type
  TMinMaxSize = record
    Min: TVector3;
    Max: TVector3;
    Size: TVector3;
  end;

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
    function Analyse(const AModel: TCastleModel): TMinMaxSize;
  public
    procedure CreateViewport;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AWidth: Integer; const AHeight: Integer); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GrabFromCastleApp(const ACastleApp: TCastleApp); overload;
    procedure GrabFromCastleApp(const ACastleApp: TCastleApp; const ASizeAndPan: TSizeAndPan); overload;
    procedure GrabFromCastleApp(const ACastleApp: TCastleApp; const ASizeAndPan: TSizeAndPan; const AModel: TCastleModel); overload;
    procedure ThumbFromCastleApp(const ACastleApp: TCastleApp; const AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
    procedure CloneModel(const AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
    function Grab(AContainer: TCastleContainer; AModel: TCastleModel; APresetSize: TSizeAndPan; const ShowInfo: Boolean = False): TSizeAndPan;
    procedure Save(const AFilename: String);
    property Azimuth: Single read fAzimuth write fAzimuth;
    property Inclination: Single read fInclination write fInclination;
    property Transparent: Boolean read fTransparent write fTransparent;
    property Image: TCastleImage read fImageBuffer write fImageBuffer;
    function AnalyseModel(AModel: TCastleModel; const Rotations: Integer;  const AniIndex: Integer = -1; const AniTime: Single = 0): TSizeAndPan;
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


procedure TFrameExport.GrabFromCastleApp(const ACastleApp: TCastleApp);
begin
  GrabFromCastleApp(ACastleApp, Default(TSizeAndPan));
end;

procedure TFrameExport.GrabFromCastleApp(const ACastleApp: TCastleApp; const ASizeAndPan: TSizeAndPan);
begin
  GrabFromCastleApp(ACastleApp, Default(TSizeAndPan), fStage);
end;

procedure TFrameExport.GrabFromCastleApp(const ACastleApp: TCastleApp; const ASizeAndPan: TSizeAndPan; const AModel: TCastleModel);
begin
  fAzimuth := ACastleApp.Azimuth;
  fInclination := ACastleApp.Inclination;
  if Assigned(ACastleApp.SelectedModel) then
    CloneModel(ACastleApp.SelectedModel);
  Grab(ACastleApp.Container, AModel, ASizeAndPan);
end;

procedure TFrameExport.ThumbFromCastleApp(const ACastleApp: TCastleApp; const AModel: TCastleModel; const AniIndex: Integer = -1; const AniTime: Single = 0);
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
    Grab(ACastleApp.Container, fStage, Default(TSizeAndPan));
end;

function TFrameExport.AnalyseModel(AModel: TCastleModel; const Rotations: Integer;  const AniIndex: Integer = -1; const AniTime: Single = 0): TSizeAndPan;
var
  SP: TMinMaxSize;
  I: Integer;
  res: TMinMaxSize;
begin
  fAzimuth := 0.785398185253143;
  fInclination := -0.615088999271393;
  if Assigned(AModel) then
    CloneModel(AModel, AniIndex, AniTime);

  for I := 0 to Rotations - 1 do
    begin
      fAzimuth := WrapRot(0.785398185253143 + (((2*PI) / Rotations) * I));
      SP := Analyse(AModel);
      if I = 0 then
        Res := SP
      else
        begin
          Res.Min := Vector3(Min(Res.Min.X, SP.Min.X),
                             Min(Res.Min.Y, SP.Min.Y),
                             Min(Res.Min.Z, SP.Min.Z));
          Res.Max := Vector3(Min(Res.Max.X, SP.Max.X),
                             Min(Res.Max.Y, SP.Max.Y),
                             Min(Res.Max.Z, SP.Max.Z));
        end;
      WriteLnLog('SP[' + IntToStr(I) + '] = ' + SP.Min.ToString + ' : ' + SP.Max.ToString);
    end;
    Res.Size := Vector3(Res.Max.X - Res.Min.X,
                        Res.Max.Y - Res.Min.Y,
                        Res.Max.Z - Res.Min.Z);
    WriteLnLog('Res = ' + Res.Min.ToString + ' : ' + Res.Max.ToString + ' : ' + Res.Size.ToString);

    Result.Size := Max(Res.Size.X, Res.Size.Y);
    Result.Pan := Vector2((Res.Size.X / 2) + Res.Min.X, (Res.Size.Y / 2) + Res.Min.Y);
end;

function TFrameExport.Grab(AContainer: TCastleContainer; AModel: TCastleModel; APresetSize: TSizeAndPan; const ShowInfo: Boolean = False): TSizeAndPan;
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

function TFrameExport.Analyse(const AModel: TCastleModel): TMinMaxSize;
var
  ViewportRect: TRectangle;
  E: TExtents;
  S: Single;
  P: TVector2;
begin
  Result := Default(TMinMaxSize);

  fCamera.ProjectionType := ptOrthographic;

  fCamera.ViewFromSphere(1, fAzimuth, fInclination);
  WriteLnLog('Azi = ' + FloatToStr(fAzimuth));

  E := fViewport.FrameCalcAngles(AModel); // fStage);
  S := Max(E.Size.X, E.Size.Y);
  P := Vector2((E.Size.X / 2) + E.Min.X, (E.Size.Y / 2) + E.Min.Y);

  Result.Min := E.Min;
  Result.Max := E.Max;
  Result.Size := E.Size;;
end;

procedure TFrameExport.Save(const AFilename: String);
begin
  if Assigned(fImageBuffer) then
    SaveImage(fImageBuffer, AFilename);
end;

end.
