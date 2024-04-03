unit SpritelySettings;

{$define badtex}

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.JSON, JSON.Types, System.Generics.Collections,
  CastleApplicationProperties,
  CastleLog
  ;

type
  TProjection = class
  private
    fName: String;
    fAzimuth: Single;
    fInclination: Single;
	fStretch: Single;
  public
    constructor Create(const AName: String; const AInclination: Single; AAzimuth: Single; AStretch: Single);
    destructor Destroy; override;
    property Name: String read fName write fName;
    property Azimuth: Single read fAzimuth write fAzimuth;
    property Inclination: Single read fInclination write fInclination;
  	property Stretch: Single read fStretch write fStretch;
  end;

  TThumbnailOption = (NeverThumbnail, ThumbnailOnDemand, AlwaysThumbnail);
  TPreloadOption = (NeverPreload, PreloadOnDemand, AlwaysPreload);

  TProjectionArray = TArray<TProjection>;

  TAnimationConfig = record
    FramesPerSecond: Integer;
    ConstantFramesPerSprite: Boolean;
    FramesPerSprite: Integer;
  end;

  TViewportConfig = record
    DefaultView: Integer;
    ZoomAndPan: Boolean;
    UseModelCenter: Boolean;
    Autofit: Boolean;
  end;

  TPickerConfig = record
    ThumbConfig: TThumbnailOption;
    PreloadConfig: TPreloadOption;
    UpDownLoadsModel: Boolean;
  end;

  TConfig = record
    Animation: TAnimationConfig;
    Viewport: TViewportConfig;
    Picker: TPickerConfig;
  end;

  TPDXSettings = class
    AppHome: String;
    AppData: String;
    ThumbData: String;
    ThumbType: String;
    AppVersion: String;
    LoadFromDir: String;
    LastModel: String;
    SaveToDir: String;
    SearchDir: String;
    Projections: TProjectionArray;
    UpdateRequired: Boolean;
    Config: TConfig;
    procedure Load;
    procedure Save;
    {$ifdef badtex}
    procedure OnWarningRaiseException(const Category, S: string);
    {$endif}
  private
    procedure CreateProjections;
    procedure SetDefaults;
    procedure SetDefaultConfig;
    procedure ExtractConfig(AConfig: TJSONObject);
    procedure ExtractAnimationConfig(AValue: TJSONObject);
    procedure ExtractViewportConfig(AValue: TJSONObject);
    procedure ExtractPickerConfig(AValue: TJSONObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function CopyString(const AValue: String): String;

var
  SystemSettings: TPDXSettings = Nil;

const
  APPNAME: String = 'Spritely';
  APPVER: String = '0.0.1';
  APIBASE: String = 'https://spritely.co.uk/';

implementation

uses
  Math,
  CastleGLUtils,
  JSON.Serializers;

function CopyString(const AValue: String): String;
begin
  Result := Copy(AValue, 1, Length(AValue));
end;


constructor TPDXSettings.Create;
begin
  inherited;
  LogFileName := 'Spritely.log';
  InitializeLog;
  {$ifdef badtex}
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}OnWarningRaiseException);
  {$endif}
  {$IF DEFINED(MSWINDOWS)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + APPNAME;
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + APPNAME;
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  AppHome := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.config') + APPNAME;
  {$ENDIF}
  // System agnostic path for data files

  AppHome := IncludeTrailingPathDelimiter(AppHome);
  AppData := AppHome;
  ThumbData := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(AppHome)+'Thumbs');
  ThumbType := '.png';
  AppVersion := APPVER;
  {$IF DEFINED(MSWINDOWS)}
  SearchDir := GetCurrentDir() + '\data';
  LoadFromDir := GetCurrentDir() + '\data';
  SaveToDir := GetCurrentDir() + '\data';
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  SearchDir := AppHome + '/data';
  LoadFromDir := AppHome + '/data';
  SaveToDir := AppHome + '/data';
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  SearchDir := AppHome + '/data';
  LoadFromDir := AppHome + '/data';
  SaveToDir := AppHome + '/data';
  LogGLInformationVerbose := true;
  {$ENDIF}
  UpdateRequired := True;
  LastModel := '';

  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
    end;

  if not DirectoryExists(ThumbData) then
    begin
      ForceDirectories(ThumbData);
    end;

  if FileExists(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json') then
    begin
      Load;
      // If AppHome has been removed (e.g. USB) then it won't exist any more
      // so reset it back to default in that situation
    end
  else
    begin
      SetDefaults;
      SetDefaultConfig;
      Save;
    end;

end;

procedure TPDXSettings.SetDefaultConfig;
begin
  Config := Default(TConfig);
  Config.Animation.FramesPerSecond := 30;
  Config.Animation.ConstantFramesPerSprite := False;
  Config.Animation.FramesPerSprite := 8;
  Config.Viewport.DefaultView := 3;
  Config.Viewport.ZoomAndPan := False;
  Config.Viewport.UseModelCenter := False;
  Config.Viewport.Autofit := True;
  Config.Picker.ThumbConfig := TThumbnailOption.ThumbnailOnDemand;
  Config.Picker.PreloadConfig := TPreloadOption.PreloadOnDemand;
  Config.Picker.UpDownLoadsModel := False;
end;

procedure TPDXSettings.SetDefaults;
begin
  CreateProjections;
end;

procedure TPDXSettings.ExtractAnimationConfig(AValue: TJSONObject);
begin
  AValue.TryGetValue('FramesPerSecond', Config.Animation.FramesPerSecond);
  AValue.TryGetValue('ConstantSpeed', Config.Animation.ConstantFramesPerSprite);
  AValue.TryGetValue('FramesPerSprite', Config.Animation.FramesPerSprite);
end;

procedure TPDXSettings.ExtractViewportConfig(AValue: TJSONObject);
begin
  AValue.TryGetValue('DefaultView', Config.Viewport.DefaultView);
  if not AValue.TryGetValue('ZoomAndPan', Config.Viewport.ZoomAndPan) then
    Config.Viewport.ZoomAndPan := False;
  if not AValue.TryGetValue('UseModelCenter', Config.Viewport.UseModelCenter) then
    Config.Viewport.UseModelCenter := False;
  if not AValue.TryGetValue('Autofit', Config.Viewport.Autofit) then
    Config.Viewport.Autofit := True;
end;

procedure TPDXSettings.ExtractPickerConfig(AValue: TJSONObject);
begin
  if not AValue.TryGetValue('ThumbConfig', Config.Picker.ThumbConfig) then
    Config.Picker.ThumbConfig := TThumbnailOption.ThumbnailOnDemand;
  if not AValue.TryGetValue('PreloadConfig', Config.Picker.PreloadConfig) then
    Config.Picker.PreloadConfig := TPreloadOption.PreloadOnDemand;
  if not AValue.TryGetValue('UpDownLoadsModel', Config.Picker.UpDownLoadsModel) then
    Config.Picker.UpDownLoadsModel := False;
end;

procedure TPDXSettings.ExtractConfig(AConfig: TJSONObject);
var
  jobj: TJSONObject;
begin
  if AConfig.TryGetValue('Animation', jobj) then
    begin
      ExtractAnimationConfig(jobj);
    end
  else
    begin
      Config.Animation.FramesPerSecond := 30;
      Config.Animation.ConstantFramesPerSprite := False;
      Config.Animation.FramesPerSprite := 8;
    end;

  if AConfig.TryGetValue('Viewport', jobj) then
    begin
      ExtractViewportConfig(jobj);
    end
  else
    begin
      Config.Viewport.DefaultView := 3;
      Config.Viewport.ZoomAndPan := False;
      Config.Viewport.UseModelCenter := False;
      Config.Viewport.Autofit := True;
    end;

  if AConfig.TryGetValue('Picker', jobj) then
    begin
      ExtractPickerConfig(jobj);
    end
  else
    begin
      Config.Picker.ThumbConfig := TThumbnailOption.ThumbnailOnDemand;
      Config.Picker.PreloadConfig := TPreloadOption.PreloadOnDemand;
      Config.Picker.UpDownLoadsModel := False;
    end;
end;

procedure TPDXSettings.Load;
var
  jobj, jobj2: TJSONObject;
  jarr: TJSONArray;
  JsonText: String;
  LastVersion: String;
  I: Integer;
  AName: String;
  AInclination: Single;
  AAzimuth: Single;
  AStretch: Single;
  AConfig: TJSONObject;
begin
  try
    JsonText := TFile.ReadAllText(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json');
  except
     on E : Exception do
       Raise Exception.Create('Load Settings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
  end;

  jobj := TJSonObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    try
      jobj.TryGetValue('AppHome', AppHome);
      jobj.TryGetValue('AppData', AppData);
      jobj.TryGetValue('ThumbData', ThumbData);
      jobj.TryGetValue('ThumbType', ThumbType);
      jobj.TryGetValue('AppVersion', AppVersion);
      jobj.TryGetValue('LoadFromDir', LoadFromDir);
      jobj.TryGetValue('SaveToDir', SaveToDir);
      jobj.TryGetValue('SearchDir', SearchDir);
      jobj.TryGetValue('LastModel', LastModel);
      if jobj.TryGetValue('Config', AConfig) then
        begin
          ExtractConfig(AConfig);
        end
      else
        SetDefaultConfig;
      if(jobj.TryGetValue('Projections', jarr)) then
        begin
          if jarr.Count > 0 then
            begin
              SetLength(Projections, jarr.Count);
              for I := 0 to jarr.Count - 1 do
                begin
                  try
                    jobj2 := jarr[I] as TJSONObject;
                    jobj2.TryGetValue('fName', AName);
                    jobj2.TryGetValue('fInclination', AInclination);
                    jobj2.TryGetValue('fAzimuth', AAzimuth);
                    jobj2.TryGetValue('fStretch', AStretch);
                    Projections[I] := TProjection.Create(AName, AInclination, AAzimuth, AStretch);
                  except
                   on E : Exception do
                     Raise Exception.Create('Load Projection Settings - Exception : Class = ' +
                      E.ClassName + ', Message = ' + E.Message);
                  end;
                end;
            end;
          end;

      if ThumbData = String.Empty then
        ThumbData := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(AppHome)+'Thumbs');
      if ThumbType = String.Empty then
        ThumbType := '.png';

      if not Assigned(Projections) then
        begin
          CreateProjections;
        end
      else
        begin
          if Length(Projections) = 0 then
            begin
              CreateProjections;
            end;
        end;

      if AppVersion = LastVersion then
        UpdateRequired := False
      else
        UpdateRequired := True;

    except
     on E : Exception do
       WriteLnLog('CGE - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
    end;
  finally
    FreeAndNil(jobj);
  end;
end;

procedure TPDXSettings.Save;
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  lSerializer := TJsonSerializer.Create;
  try
    try
      JsonText := lSerializer.Serialize<TPDXSettings>(Self);
      try
        TFile.WriteAllText(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json', JsonText);
      except
         on E : Exception do
           Raise Exception.Create('Save Settings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
      end;
    except
     on E : Exception do
     begin
       Raise Exception.Create('Save Settings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
  end;
end;

procedure TPDXSettings.CreateProjections;
const
  ProjCount = 12;
begin
  SetLength(Projections, ProjCount);
                                    // Name, Azimuth, Inclination
  Projections[0] := TProjection.Create('Front', (Pi/2), 0, 1);
  Projections[1] := TProjection.Create('Top Down', 0, 0, 1);
  Projections[2] := TProjection.Create('Pixel Isometric', ((Pi/2) - (pi / 6)), 0.785398185253143, 1);
  Projections[3] := TProjection.Create('True Isometric', ((Pi/2) - 0.615088935), 0.785398185253143, 1);
  Projections[4] := TProjection.Create('45 Deg', ((Pi/2) - (pi / 4)), 0.785398185253143, 1);
  Projections[5] := TProjection.Create('Root 2', ((Pi/2) - 0.955316618), 0.785398185253143, 1);
  Projections[6] := TProjection.Create('3/4 View', ((Pi/2) - 1.10714872), 0.785398185253143, 1);
  Projections[7] := TProjection.Create('Military', ((Pi/2) - 1.10714872), 0.785398185253143, 0.81625);
  Projections[8] := TProjection.Create('Back', Pi/2, Pi, 1);
  Projections[9] := TProjection.Create('Right', Pi/2, -Pi/2, 1);
  Projections[10] := TProjection.Create('Left', Pi/2, Pi/2, 1);
  Projections[11] := TProjection.Create('Bottom Up', Pi, 0, 1);
end;


destructor TPDXSettings.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Projections) - 1 do
    FreeAndNil(Projections[I]);
  SetLength(Projections, 0);
  inherited;
end;

{$ifdef badtex}
procedure TPDXSettings.OnWarningRaiseException(const Category, S: string);
begin
  raise Exception.CreateFmt('Cat: %s Msg: %s',
    [Category, S]);
end;
{$endif}

{ TProjection }

constructor TProjection.Create(const AName: String; const AInclination: Single; AAzimuth: Single; AStretch: Single);
begin
  fName := AName;
  fInclination := AInclination;
  fAzimuth := AAzimuth;
  fStretch := AStretch; 
end;

destructor TProjection.Destroy;
begin
  inherited;
end;

initialization
//  {$Message 'Settings init'}
  SystemSettings := TPDXSettings.Create;

finalization
  SystemSettings.UpdateRequired := False;
  SystemSettings.Save;
  SystemSettings.Free;

end.
