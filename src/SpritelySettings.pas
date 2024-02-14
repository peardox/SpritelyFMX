unit SpritelySettings;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.JSON, JSON.Types
  ;

type
  TPDXSettings = class
    AppHome: String;
    AppData: String;
    AppVersion: String;
    LoadFromDir: String;
    SaveToDir: String;
    UpdateRequired: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

var
  SystemSettings: TPDXSettings = Nil;

const
  APPNAME: String = 'Spritely';
  APPVER: String = '0.0.1';
  APIBASE: String = 'https://spritely.co.uk/';

implementation

uses
  JSON.Serializers;

constructor TPDXSettings.Create;
begin
  inherited;
  {$IF DEFINED(MSWINDOWS)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + APPNAME;
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + APPNAME;
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + APPNAME;
  {$ENDIF}
  // System agnostic path for data files

  AppHome := IncludeTrailingPathDelimiter(AppHome);
  AppData := AppHome;
  AppVersion := APPVER;
  {$IF DEFINED(MSWINDOWS)}
  LoadFromDir := GetCurrentDir() + '\data';
  SaveToDir := GetCurrentDir() + '\data';
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  LoadFromDir := AppHome + '/data';
  SaveToDir := AppHome + '/data';
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  LoadFromDir := AppHome + '/data';
  SaveToDir := AppHome + '/data';
  {$ENDIF}
  UpdateRequired := True;

  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
    end;
  if FileExists(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json') then
    begin
      Load;
      // If AppHome has been removed (e.g. USB) then it won't exist any more
      // so reset it back to default in that situation
    end
  else
    begin
      Save;
    end;

end;

destructor TPDXSettings.Destroy;
begin
  inherited;
end;

procedure TPDXSettings.Load;
var
  jobj: TJSONObject;
  JsonText: String;
  LastVersion: String;
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
      jobj.TryGetValue('AppVersion', AppVersion);
      jobj.TryGetValue('LoadFromDir', LoadFromDir);
      jobj.TryGetValue('SaveToDir', SaveToDir);

      if AppVersion = LastVersion then
        UpdateRequired := False
      else
        UpdateRequired := True;

    except
     on E : Exception do
       Raise Exception.Create('Load Settings - Exception : Class = ' +
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

initialization
  {$Message 'Settings init'}
  SystemSettings := TPDXSettings.Create;

finalization
  SystemSettings.UpdateRequired := False;
  SystemSettings.Save;
  SystemSettings.Free;

end.