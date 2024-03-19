unit CastleModel;

{$define poly}

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  CastleDebugTransform, CastleColors,
  CastleScene, CastleQuaternions, CastleVectors,
  SpritelyTypes,
  SpritelyDebug;

type
  TModelPack = class;

  TGimbal = class(TComponent)
  private
//    fOffsetScale: TVector3;
//    fOffsetTranslation: TVector3;
    fOffsetRotation: TVector4;
    fScale: Single;
    fTranslation: TVector3;
    fRotation: TVector4;
    procedure SetScale(const AValue: Single);
    procedure SetTranslation(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector4);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply;
    property Scale: Single read fScale write SetScale;
    property Translation: TVector3 read fTranslation write SetTranslation;
    property Rotation: TVector4 read fRotation write SetRotation;
  end;

  TCastleModel = class(TCastleScene)
  private
    fChildOf: TModelPack;
    fScale: Single;
    fGimbal: TGimbal;
    fAlign: TModelAlign;
    fDebugBox: TDebugTransformHexahedron;
    function GetHasDebugBox: Boolean;
  public
    Frame: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNormalScale: Single;
    property NormalScale: Single read GetNormalScale write fScale;
    function Normalize: Boolean;
    procedure SelectModel;
    procedure DeSelectModel;
    procedure AddDebugBox;
    procedure ShowDebugBox(const AValue: Boolean);
    procedure SetDebugBoxColour(const AValue: TCastleColor);
    function GetInfo: TModelInfo;
    function AlignTo(const OtherModel: TCastleModel; const Align: TModelAlign; const Pad: TVector3): TVector3;
    procedure LoadModel(filename: String);
    property Gimbal: TGimbal read fGimbal write fGimbal;
    property Align: TModelAlign read fAlign write fAlign;
    property DebugBox: TDebugTransformHexahedron read fDebugBox write fDebugBox;
    property HasDebugBox: Boolean read GetHasDebugBox;
  end;

  TPDXModelEvent = procedure (Sender: TObject; const AModel: TCastleModel) of object;

  TModelPack = class(TComponent)
    fChildren: TObjectList<TCastleModel>;
  private
    fDebugColor: TCastleColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddModel(const AFileName: String): TCastleModel; overload;
    function AddModel(const AFileName: String; const AlignValue: TModelAlign): TCastleModel; overload;
    procedure SetDebugBoxColour(const AValue: TCastleColor);
    property Kids: TObjectList<TCastleModel> read fChildren write fChildren;
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
  CastleLog,
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
//      WriteLnLog('Self Model = ' + Url);
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
//          WriteLnLog('Other Model = ' + otherModel.Url);
          otherSize := Vector3(OtherModel.BoundingBox.Size.X / 2,
                         OtherModel.BoundingBox.Size.Y / 2,
                         OtherModel.BoundingBox.Size.Z / 2);
          otherCenter := Vector3(OtherModel.BoundingBox.Max.X - otherSize.X,
                         OtherModel.BoundingBox.Max.Y - otherSize.Y,
                         OtherModel.BoundingBox.Max.Z - otherSize.Z);
          otherLoc := OtherModel.Translation;
        end;
    end;

//  WriteLnLog('Other : Center = ' + otherCenter.ToString + ', Size = ' + otherSize.ToString + ', Loc = ' + otherLoc.ToString);
//  WriteLnLog('Self  : Center = ' + newCenter.ToString + ', Size = ' + newSize.ToString);

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

constructor TCastleModel.Create(AOwner: TComponent);
begin
  inherited;
  fScale := 0.5;
  fAlign.X := modelXCenter;
  fAlign.Y := modelYCenter;
  fAlign.Z := modelZCenter;
//  Normalize;
  fGimbal := TGimbal.Create(Self);
  fDebugBox := Nil;
end;

procedure TCastleModel.DeSelectModel;
var
  OwningCastleApp: TCastleApp;
begin
  if fChildOf <> Nil then
    begin
      if fChildOf.Owner is TCastleApp then
        begin
          OwningCastleApp := fChildOf.Owner as TCastleApp;
          OwningCastleApp.SelectedModel := Nil;
        end;
    end;
end;

destructor TCastleModel.Destroy;
begin
  fGimbal.Free;
  inherited;
end;

function TCastleModel.GetInfo: TModelInfo;
begin
  Result.Name := Self.Url;
  Result.Translation := Self.Translation;
  Result.Scale := Self.Scale;
  Result.Rotation := Self.Rotation;
  if Self.LocalBoundingBox.IsEmptyOrZero then
    begin
      Result.Size := Vector3(0,0,0);
      Result.Center := Vector3(0,0,0);
    end
  else
    begin
      Result.Size := Self.BoundingBox.Size;
      Result.Center := Self.BoundingBox.Center;
    end;
end;

function TCastleModel.GetNormalScale: Single;
var
  OwningCastleApp: TCastleApp;
begin
  OwningCastleApp := Nil;

  if fChildOf <> Nil then
    begin
      if fChildOf.Owner is TCastleApp then
        begin
          OwningCastleApp := fChildOf.Owner as TCastleApp;
        end
      else
        raise Exception.Create('Can''t find owning CastleApp');
    end
  else if Owner is TCastleApp then
    begin
      OwningCastleApp := Owner as TCastleApp;
    end;

  if not(OwningCastleApp is TCastleApp) then
    raise Exception.Create('Can''t find owning CastleApp');

   Result := OwningCastleApp.Stage.fScale;
end;

procedure TCastleModel.LoadModel(filename: String);
begin
  try
    if UriFileExists(filename) then
      begin
        Load(filename);
        fChildOf := Nil;
        Normalize;
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
              Center := Vector3(CX, CY, CZ);
              WriteLnLog('Set Center to ' + Center.ToString);
              Translation := -BoundingBox.Center;
              Result := True;
        end;
      end;
    end;
end;

procedure TCastleModel.SelectModel;
var
  OwningCastleApp: TCastleApp;
begin
  if fChildOf <> Nil then
    begin
      if fChildOf.Owner is TCastleApp then
        begin
 //         SendMessage('Selecting');
          OwningCastleApp := fChildOf.Owner as TCastleApp;
          OwningCastleApp.SelectedModel := Self;
        end;
    end;
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
            model.Normalize;
            model.fChildOf := Self;
            model.AddDebugBox;
            model.SetDebugBoxColour(fDebugColor);
            Result := model;
          end;
      except
        on E : Exception do
          begin
            Raise Exception.Create('Error in AddModel : ' + E.ClassName + ' - ' + E.Message);
           end;
      end;
    end;

end;

constructor TModelPack.Create(AOwner: TComponent);
begin
  inherited;
  fChildren := TObjectList<TCastleModel>.Create(True);
  fDebugColor := Green;
end;

destructor TModelPack.Destroy;
begin
  fChildren.Free;
  inherited;
end;

procedure TModelPack.SetDebugBoxColour(const AValue: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(fDebugColor, AValue) then
    fDebugColor := AValue;
end;

{ TGimbal }

procedure TGimbal.Apply;
begin
  if Owner is TCastleModel then
    begin
      TCastleModel(Owner).Rotation := fOffsetRotation + fRotation;
//      TCastleModel(Owner).Scale := fOffsetScale * fScale;
//      TCastleModel(Owner).Translation := fOffsetTranslation + fTranslation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Rotation when owner is not TCastleModel');
end;

constructor TGimbal.Create(AOwner: TComponent);
begin
  inherited;
  fScale := 1.0;
end;

destructor TGimbal.Destroy;
begin
  inherited;
end;

procedure TGimbal.SetRotation(const AValue: TVector4);
begin
  if Owner is TCastleModel then
    begin
      fRotation := AValue;
      TCastleModel(Owner).Rotation := fOffsetRotation + fRotation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Rotation when owner is not TCastleModel');
end;

procedure TGimbal.SetScale(const AValue: Single);
begin
  if Owner is TCastleModel then
    begin
      fScale := AValue;
//      TCastleModel(Owner).Scale := fOffsetScale * fScale;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Scale when owner is not TCastleModel');
end;

procedure TGimbal.SetTranslation(const AValue: TVector3);
begin
  if Owner is TCastleModel then
    begin
      fTranslation := AValue;
 //     TCastleModel(Owner).Translation := fOffsetTranslation + fTranslation;
    end
  else
    Raise Exception.Create('Attempted to set Gimbal Translation when owner is not TCastleModel');
end;

end.

