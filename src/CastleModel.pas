unit CastleModel;

 {$define normal}
// {$define showfree}
interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  CastleDebugTransform, CastleColors,
  CastleScene, CastleQuaternions, CastleVectors,
  SpritelyTypes,
  SpritelyDebug;

type
  TModelPack = class;
  TModelInfo = class;

  { CastleModel }
  TCastleModel = class(TCastleScene)
  private
    fChildOf: TObject; // TModelPack;
    fScale: Single;
    fInstanceCount: Integer;
    fAlign: TModelAlign;
    fDebugBox: TDebugTransformHexahedron;
    fInfo: TModelInfo;
    fOffset: TVector3;
    function GetHasDebugBox: Boolean;
    function GetOwnerApp: TObject;
  public
    Frame: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNormalScale: Single;
    property NormalScale: Single read GetNormalScale write fScale;
    function Normalize: Boolean;
    procedure ClearAndFreeItems;
    function SetInfo: TModelInfo;
    function GetOriginOffset: TVector3;
    procedure SelectModel;
    procedure UpdateModel(const AModel: TCastleModel);
    procedure DeSelectModel;
    procedure AddDebugBox;
    procedure ShowDebugBox(const AValue: Boolean);
    procedure SetDebugBoxColour(const AValue: TCastleColor);
    function AlignTo(const OtherModel: TCastleModel; const Align: TModelAlign; const Pad: TVector3): TVector3;
    procedure LoadModel(AOwner: TObject; filename: String; const DoNormalize: Boolean = True);
    property Align: TModelAlign read fAlign write fAlign;
    property DebugBox: TDebugTransformHexahedron read fDebugBox write fDebugBox;
    property HasDebugBox: Boolean read GetHasDebugBox;
    property InstanceCount: Integer read fInstanceCount write fInstanceCount;
    property ModelInfo: TModelInfo read fInfo write fInfo;
    property Offset: TVector3 read fOffset write fOffset;
  end;


  { TAnimationInfo }
  TAnimationInfo = class
  private
    fName: String;
    fDuration: Single;
    fStep: Integer;
    fPosition: TList<Integer>;
    function GetStepCount: Integer;
  public
    constructor Create(const AName: String; const ADuration: Single);
    destructor Destroy; override;
    property Name: String read fName write fName;
    property Duration: Single read fDuration;
    property Step: Integer read fStep write fStep;
    property Count: Integer read GetStepCount;
    property Position: TList<Integer> read fPosition write fPosition;
  end;

  { TModelInfo }
  TModelInfo = class
  private
    fIsValid: Boolean;
    fModel: TCastleModel;
    fName: String;
    fHash: String;
    fTranslation: TVector3;
    fScale: TVector3;
    fRotation: TVector4;
    fSize: TVector3;
    fCenter: TVector3;
    fAnimationCount: Integer;
    fAnimations: TList<String>;
    fAnimation: TAnimationInfo;
  public
    constructor Create;
    destructor Destroy; override;
    constructor SetInfo(const AModel: TCastleModel);
    property IsValid: Boolean read fIsValid write fIsValid;
    property Model: TCastleModel read fModel write fModel;
    property Name: String read fName write fName;
    property Hash: String read fHash write fHash;
    property Translation: TVector3 read fTranslation write fTranslation;
    property Scale: TVector3 read fScale write fScale;
    property Rotation: TVector4 read fRotation write fRotation;
    property Size: TVector3 read fSize write fSize;
    property Center: TVector3 read fCenter write fCenter;
    property AnimationCount: Integer read fAnimationCount write fAnimationCount;
    property Animations: TList<String> read fAnimations write fAnimations;
    property Animation: TAnimationInfo read fAnimation write fAnimation;
  end;

  TPDXModelEvent = procedure (Sender: TObject; const AModel: TCastleModel) of object;

  TModelPack = class(TComponent)
    fChildren: TObjectList<TCastleModel>;
  private
    fPackScale: Single;
    fDebugColor: TCastleColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddModel(const AFileName: String): TCastleModel; overload;
    function AddModel(const AFileName: String; const AlignValue: TModelAlign): TCastleModel; overload;
    procedure SetDebugBoxColour(const AValue: TCastleColor);
    property Kids: TObjectList<TCastleModel> read fChildren write fChildren;
    property PackScale: Single read fPackScale write fPackScale;
  end;

function ModelAlign(const AlignX: TModelXAlign = modelXDefined; AlignY: TModelYAlign = modelYDefined; AlignZ: TModelZAlign = modelZDefined): TModelAlign;
function ModelAlignDefined: TModelAlign;
function ModelAlignOrigin: TModelAlign;
function ModelAlignCenter: TModelAlign;
function ModelAlignYBottom: TModelAlign;

implementation

uses
  Math,
  CastleApp,
  System.Hash,
  CastleLog,
  CastleBoxes,
  FrameToImage,
  System.IOUtils,
  CastleUriUtils;

function ModelAlign(const AlignX: TModelXAlign = modelXDefined; AlignY: TModelYAlign = modelYDefined; AlignZ: TModelZAlign = modelZDefined): TModelAlign;
begin
  Result.X:= AlignX;
  Result.Y:= AlignY;
  Result.Z:= AlignZ;
end;

function ModelAlignDefined: TModelAlign;
begin
  Result.X := modelXDefined;
  Result.Y := modelYDefined;
  Result.Z := modelZDefined;
end;

function ModelAlignOrigin: TModelAlign;
begin
  Result.X := modelXOrigin;
  Result.Y := modelYOrigin;
  Result.Z := modelZOrigin;
end;

function ModelAlignCenter: TModelAlign;
begin
  Result.X := modelXCenter;
  Result.Y := modelYCenter;
  Result.Z := modelZCenter;
end;

function ModelAlignYBottom: TModelAlign;
begin
  Result.X := modelXCenter;
  Result.Y := modelYBottom;
  Result.Z := modelZCenter;
end;

{ TCastleModel }

procedure TCastleModel.AddDebugBox;
begin
    fDebugBox := TDebugTransformHexahedron.Create(Self);
    fDebugBox.Parent := Self;
//    fDebugBox.Radius := 1.0;
    fDebugBox.Exists := False;
end;

function TCastleModel.AlignTo(const OtherModel: TCastleModel;
  const Align: TModelAlign; const Pad: TVector3): TVector3;
var
  newCenter, otherCenter, otherLoc, newSize, otherSize: TVector3;
begin
  if BoundingBox.IsEmptyOrZero then
    begin
      newSize := Vector3(0, 0, 0);
      newCenter := Vector3(0, 0, 0);
    end
  else
    begin
      newSize := Vector3(BoundingBox.Size.X / 2,
                     BoundingBox.Size.Y / 2,
                     BoundingBox.Size.Z / 2);
      newCenter := Vector3(BoundingBox.Max.X - newSize.X,
                     BoundingBox.Max.Y - newSize.Y,
                     BoundingBox.Max.Z - newSize.Z);
    end;

  if not Assigned(OtherModel) then
    begin
      otherSize := Vector3(0, 0, 0);
      otherCenter := Vector3(0, 0, 0);
      otherLoc := Vector3(0, 0, 0);
    end
  else
    begin
      if OtherModel.BoundingBox.IsEmptyOrZero then
        begin
          otherSize := Vector3(0, 0, 0);
          otherCenter := Vector3(0, 0, 0);
          otherLoc := Vector3(0, 0, 0);
        end
      else
        begin
          otherSize := Vector3(OtherModel.BoundingBox.Size.X / 2,
                         OtherModel.BoundingBox.Size.Y / 2,
                         OtherModel.BoundingBox.Size.Z / 2);
          otherCenter := Vector3(OtherModel.BoundingBox.Max.X - otherSize.X,
                         OtherModel.BoundingBox.Max.Y - otherSize.Y,
                         OtherModel.BoundingBox.Max.Z - otherSize.Z);
          otherLoc := OtherModel.Translation;
        end;
    end;

  Result := otherCenter +
            Vector3(otherSize.X,
                    -otherSize.Y,
                    otherSize.Z) +
            Vector3(-newSize.X,
                    newSize.Y,
                    newSize.Z) +
            Pad;
  // Left X, Bottom Y, Front Z

{
  Result := otherCenter +
            Vector3(otherSize.X,
                    -otherSize.Y,
                    -otherSize.Z) +
            Vector3(-newSize.X,
                    newSize.Y,
                    -newSize.Z) +
            Pad;
  // Left X, Bottom Y, Back Z

  Result := otherCenter +
            Vector3(-otherSize.X,
                    -otherSize.Y,
                    -otherSize.Z) +
            Vector3(newSize.X,
                    newSize.Y,
                    -newSize.Z) +
            Pad;
  // Right X, Bottom Y, Back Z

  Result := otherCenter +
            Vector3(0,
                    -otherSize.Y,
                    -otherSize.Z) +
            Vector3(0,
                    newSize.Y,
                    -newSize.Z) +
            Pad;
  // Center X, Bottom Y, Back Z

  Result := otherCenter +
            Vector3(otherSize.X,
                    -otherSize.Y,
                    otherSize.Z) +
            Vector3(newSize.X,
                    newSize.Y,
                    -newSize.Z) +
            Pad;
  // Right X, Bottom Y, Front Z

  Result := otherCenter +
            Vector3(otherSize.X,
                    -otherSize.Y,
                    -otherSize.Z) +
            Vector3(newSize.X,
                    newSize.Y,
                    newSize.Z) +
            Pad;
  // Right X, Bottom Y, Back Z

  Result := otherCenter +
            Vector3(otherSize.X,
                    otherSize.Y,
                    0) +
            Vector3(newSize.X,
                    -newSize.Y,
                    0) +
            Pad;
  // Right X, Bottom Y, Center Z

  Result := otherCenter +
            Vector3(otherSize.X,
                    -otherSize.Y,
                    0) +
            Vector3(newSize.X,
                    newSize.Y,
                    0) +
            Pad;
  // Right X, Bottom Y, Center Z

  Result := otherCenter +
            Vector3(otherSize.X,
                    0,
                    0) +
            Vector3(newSize.X,
                    0,
                    0) +
            Pad;

  // Right X, Middle Y,Z
  Result := otherCenter +
            Vector3(-otherSize.X,
                    0,
                    0) +
            Vector3(-newSize.X,
                    0,
                    0) +
            Pad;
  // Left X, Middle Y,Z
}

end;

procedure TCastleModel.ClearAndFreeItems;
begin
  while Count <> 0 do
    begin
      FreeAndNil(Items[0]);
    end;
end;

constructor TCastleModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fScale := 0.5;
  fAlign.X := modelXCenter;
  fAlign.Y := modelYCenter;
  fAlign.Z := modelZCenter;
  fChildOf := AOwner;
  fInstanceCount := 0;
  fInfo := TModelInfo.Create;
{$ifdef normal}
  Normalize;
{$endif}
  fDebugBox := Nil;
end;

procedure TCastleModel.DeSelectModel;
var
  OwningCastleApp: TObject;
begin
  OwningCastleApp := GetOwnerApp;
  if OwningCastleApp is TCastleApp then
    TCastleApp(OwningCastleApp).SelectedModel := Nil;
end;

destructor TCastleModel.Destroy;
begin
{$ifdef showfree}
  WriteLnLog('Freeing TCastleModel');
{$endif}
  if Assigned(fInfo) then
    begin
{$ifdef showfree}
      WriteLnLog('Freeing TCastleModel''s TModelInfo');
{$endif}
      FreeAndNil(fInfo);
    end;
  inherited;
end;

function TCastleModel.SetInfo: TModelInfo;
begin
  fInfo.SetInfo(Self);
  Result := fInfo;
end;

function TCastleModel.GetOwnerApp: TObject;
var
  OwningCastleApp: TCastleApp;
begin
  OwningCastleApp := Nil;

  if fChildOf <> Nil then
    begin
      if fChildOf is TCastleApp then
        begin
          OwningCastleApp := fChildOf as TCastleApp;
        end
      else if fChildOf is TModelPack then
        begin
          if TModelPack(fChildOf).Owner is TCastleApp then
            begin
              OwningCastleApp := TModelPack(fChildOf).Owner as TCastleApp;
            end
        end
      else
        raise Exception.Create('Can''t find owning CastleApp');
    end;

  if not(OwningCastleApp is TCastleApp) then
    raise Exception.Create('Can''t find owning CastleApp');

   Result := OwningCastleApp;
end;

function TCastleModel.GetNormalScale: Single;
begin
  if fChildOf is TModelPack then
    Result := TModelPack(fChildOf).PackScale
  else
    Result := fScale;
end;

procedure TCastleModel.LoadModel(AOwner: TObject; filename: String; const DoNormalize: Boolean = True);
begin
  try
    if UriFileExists(filename) then
      begin
        Load(filename);
        fChildOf := AOwner;
{$ifdef normal}
        if DoNormalize then
          Normalize;
{$endif}
      end;
  except
    on E : Exception do
      begin
        Raise Exception.Create('Error in LoadScene : ' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

function TCastleModel.Normalize: Boolean;
var
//  BBMax: Single;
  CX, CY, CZ: Single;
  AScale: Single;
begin
  Result := False;
  if not(RootNode = nil) then
    begin
    if not LocalBoundingBox.IsEmptyOrZero then
      begin
        if LocalBoundingBox.MaxSize > 0 then
          begin

    fAlign := ModelAlignCenter; // sbdbg

            CX := 1;
            CY := 1;
            CZ := 1;

            case fAlign.X of
              modelXLeft: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X);
              modelXCenter: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2);
              modelXRight: CX := Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + LocalBoundingBox.SizeX;
              modelXDefined: CX := LocalBoundingBox.Center.X;
              modelXOrigin: CX := 0;
            end;

            case fAlign.Y of
              modelYBottom: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y);
              modelYCenter: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2);
              modelYTop: CY := Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + LocalBoundingBox.SizeY;
              modelYDefined: CY := LocalBoundingBox.Center.Y;
              modelYOrigin: CY := 0;
            end;

            case fAlign.Z of
              modelZBack: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z);
              modelZCenter: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2);
              modelZFront: CZ := Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + LocalBoundingBox.SizeZ;
              modelZDefined: CZ := LocalBoundingBox.Center.Z;
              modelZOrigin: CZ := 0;
            end;
           {

            Center := Vector3(Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2),
                              Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2),
                              Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2));
            Translation := -Center;
            BBMax := LocalBoundingBox.MaxSize;
            Scale := Vector3(NormalScale / BBMax,
                             NormalScale / BBMax,
                             NormalScale / BBMax);
            Result := True;
            BBMax := Max(LocalBoundingBox.SizeX, LocalBoundingBox.SizeZ);
            }
//            Translation := Vector3(0,0,0);

              AScale :=  NormalScale;
              Scale := Vector3(AScale, AScale, AScale);
//              Center := Vector3(CX, CY, CZ); // sbdbg
              fOffset := Vector3(CX, CY, CZ); // sbdbg

//              WriteLnLog('Set Center to ' + Center.ToString);
//              Translation := -BoundingBox.Center;
              Result := True;
        end;
      end;
    end;
end;

function TCastleModel.GetOriginOffset: TVector3;
var
  CX, CY, CZ: Single;
  bb: TBox3D;
begin
  Result := Vector3(0,0,0);
  if not(RootNode = nil) then
    begin
      bb := BoundingBox;
      if not bb.IsEmptyOrZero then
        begin
          if bb.MaxSize > 0 then
            begin
              CX := Min(bb.Data[0].X, bb.Data[1].X) + (bb.SizeX / 2){ - bb.Center.X};
              CY := Min(bb.Data[0].Y, bb.Data[1].Y) + (bb.SizeY / 2){ - bb.Center.Y};
              CZ := Min(bb.Data[0].Z, bb.Data[1].Z) + (bb.SizeZ / 2){ - bb.Center.Z};
              Result := Vector3(CX, CY, CZ);
            end;
        end;
    end;
end;

procedure TCastleModel.SelectModel;
var
  OwningCastleApp: TObject;
begin
  OwningCastleApp := GetOwnerApp;
  if OwningCastleApp is TCastleApp then
    TCastleApp(OwningCastleApp).SelectedModel := Self;
end;

procedure TCastleModel.SetDebugBoxColour(const AValue: TCastleColor);
begin
  if Assigned(fDebugBox) then
    begin
      if not TCastleColor.PerfectlyEquals(fDebugBox.BoxColor, AValue) then
        fDebugBox.BoxColor := AValue;
    end;
end;

procedure TCastleModel.ShowDebugBox(const AValue: Boolean);
begin
  if Assigned(fDebugBox) then
    begin
      if fDebugBox.Exists <> AValue then
        fDebugBox.Exists := AValue;
    end;
end;

procedure TCastleModel.UpdateModel(const AModel: TCastleModel);
begin
  fChildOf := AModel.fChildOf;
  fScale := Amodel.fScale;
//  Inc(AModel.InstanceCount);
  fAlign :=  Amodel.fAlign;
//  fDebugBox: TDebugTransformHexahedron;
//  fInfo := Amodel.fInfo;
end;

function TCastleModel.GetHasDebugBox: Boolean;
begin
  Result := False;
  if Assigned(fDebugBox) then
    begin
      if fDebugBox.Exists then
        Result := True;
    end;
end;

{ TModelPack }

function TModelPack.AddModel(const AFileName: String): TCastleModel;
begin
  Result := AddModel(AFileName, ModelAlignYBottom);
end;

function TModelPack.AddModel(const AFileName: String; const AlignValue: TModelAlign): TCastleModel;
var
  model: TCastleModel;
begin
  Result := Nil;
  if UriFileExists(AFileName) then
    begin
      model := TCastleModel.Create(Owner);
      model.Align := AlignValue;
      try
        model.Load(AFileName);
        if Assigned(model) then
          begin
            fChildren.Add(model);
{$ifdef normal}
            model.Normalize;
{$endif}
            model.fChildOf := Self;
            model.AddDebugBox;
            model.SetDebugBoxColour(fDebugColor);
          end;
      except
        on E : Exception do
          begin
            FreeAndNil(model);
//            Raise Exception.Create('Error in TModelPack.AddModel : ' + E.ClassName + ' - ' + E.Message);
           end;
      end;
    end;

    Result := model;
end;

constructor TModelPack.Create(AOwner: TComponent);
begin
  inherited;
  fChildren := TObjectList<TCastleModel>.Create(False);
  fDebugColor := Green;
  fPackScale := 1;
end;

destructor TModelPack.Destroy;
begin
{$ifdef showfree}
  WriteLnLog('Freeing TModelPack');
{$endif}
  FreeAndNil(fChildren);
  inherited;
end;

procedure TModelPack.SetDebugBoxColour(const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(fDebugColor, AValue) then
    fDebugColor := AValue;
end;

{ TModelInfo }

constructor TModelInfo.Create;
begin
  fAnimation := Nil;
  fAnimations := Nil;
end;

constructor TModelInfo.SetInfo(const AModel: TCastleModel);
var
  I: Integer;
begin
  fAnimation := Nil;
  fAnimations := Nil;
  fIsValid := True;
  fModel := AModel;
  fName := TPath.GetFileName(AModel.Url);
  if FileExists(AModel.Url) then
    fHash := THashMD5.GetHashStringFromFile(AModel.Url)
  else
    fHash := '';

  fTranslation := AModel.Translation;
  fScale := AModel.Scale;
  fRotation := AModel.Rotation;
  fAnimationCount := AModel.AnimationsList.Count;
  if fAnimationCount > 0 then
    begin
      fAnimations := TList<String>.Create;
      for I := 0 to fAnimationCount - 1 do
        begin
          fAnimations.Add(AModel.AnimationsList[I]);
        end;
    end;

  if AModel.LocalBoundingBox.IsEmptyOrZero then
    begin
      fSize := Vector3(0,0,0);
      fCenter := Vector3(0,0,0);
    end
  else
    begin
      fSize := AModel.BoundingBox.Size;
      fCenter := AModel.BoundingBox.Center;
    end;
end;

destructor TModelInfo.Destroy;
begin
{$ifdef showfree}
  WriteLnLog('Freeing TModelInfo');
{$endif}
{
  if fModel is TCastleModel then
    begin
      if Assigned(fModel) then
        FreeAndNil(fModel);
    end;
}
  if Assigned(fAnimations) then
    begin
{$ifdef showfree}
      WriteLnLog('Freeing TModelInfo');
{$endif}
      FreeAndNil(fAnimations);
    end;
  inherited;
end;

{ TAnimationInfo }

constructor TAnimationInfo.Create(const AName: String; const ADuration: Single);
begin
  fName := AName;
  fDuration := ADuration;
  fStep := 0;
  fPosition := TList<Integer>.Create;
end;

destructor TAnimationInfo.Destroy;
begin
{$ifdef showfree}
  WriteLnLog('Freeing TAnimationInfo');
{$endif}
  FreeAndNil(fPosition);
  inherited;
end;

function TAnimationInfo.GetStepCount: Integer;
begin
  Result := fPosition.Count;
end;

end.

