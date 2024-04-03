unit SpritelyCheckTextures;

// {$define logit}
interface

uses System.Generics.Collections, SpritelyTypes, CastleModel;

type
  TFileDirectory = class
  private
    fParentDir: String;
    fSubDir: String;
    fFileName: String;
    fGroupID: Integer;
  public
    constructor Create(const AParentDir: String; ASubDir: String; AFileName: String; AGroupID: Integer);
    destructor Destroy; override;
    function GetFullFileName: String;
    property ParentDir: String read fParentDir write fParentDir;
    property SubDir: String read fSubDir write fSubDir;
    property FileName: String read fFileName write fFileName;
    property GroupID: Integer read fGroupID write fGroupID;
  end;
  TFileDirectoryList = TObjectList<TFileDirectory>;

procedure CheckGLTFTextures(const ADir: String);
function ScanGLTFModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
function ScanOBJModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
function ScanDAEModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
function ScanSTLModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
procedure SaveModelList(const AFilename: String; const AList: TFileDirectoryList);

implementation

uses System.SysUtils,
  System.Variants,
  System.Classes,
  CastleLog,
  System.JSON,
  JSON.Types,
  JSON.Serializers,
  System.IOUtils;

procedure ScanFile(const ADir: String; const AFile: String);
var
  jobj: TJSONObject;
  jPair: TJSonPair;
  jarr: TJSONArray;
  Name: String;
  Value: TJsonValue;
  JsonText: String;
  I: Integer;
  uri: String;
begin
  JsonText := TFile.ReadAllText(ADir + TPath.DirectorySeparatorChar + AFile);
  jobj := TJSonObject.ParseJSONValue(JsonText) as TJSONObject;
  for jPair in jobj do
    begin
      Name := jPair.JsonString.Value;
      Value := jPair.JsonValue;
      if Value is TJsonArray then
        begin
          if Name = 'images' then
            begin
              jarr := Value as TJSONArray;
              for I := 0 to jarr.Count - 1 do
                begin
                  if jarr[i] is TJsonObject then
                    begin
                      if jarr[i].TryGetValue('uri', uri) then
                        begin
                          if not FileExists(ADir + TPath.DirectorySeparatorChar + uri) then
                            WriteLnLog('Missing uri ' + uri + ' in '  + ADir + TPath.DirectorySeparatorChar + AFile);
                        end;
                    end;
                end;
            end;
        end;
    end;
  jobj.Free;
  {
  JsonText := StringReplace(JsonText, '..\\..\\..\\..\\..\\_bases\\2D\\', '', [rfReplaceAll, rfIgnoreCase]);
  JsonText := StringReplace(JsonText, '..\\..\\..\\..\\_bases\\2D\\', '', [rfReplaceAll, rfIgnoreCase]);
  JsonText := StringReplace(JsonText, '..\\..\\..\\_bases\\2D\\', '', [rfReplaceAll, rfIgnoreCase]);
  TFile.WriteAllText(AFile, JsonText);
  }
end;

procedure ScanModels(const ADir: String; const ASubDir: String; const AFileExt: TArray<String>; var AList: TObjectList<TFileDirectory>; var GroupID: Integer);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  AFileSpec: String;
  ASrch: String;
  I: Integer;
  DirItem: TFileDirectory;
  first: Boolean;
begin
  {$IF DEFINED(MSWINDOWS)}
  AFileSpec := '*.*';
  {$ELSE}
  AFileSpec := '*';
  {$ENDIF}
  First := True;

  FileAttrs := faAnyFile or faDirectory;
  if ASubDir = String.Empty then
    ASrch := ADir + TPath.DirectorySeparatorChar + AFileSpec
  else
    ASrch := ADir + TPath.DirectorySeparatorChar + ASubDir + TPath.DirectorySeparatorChar + AFileSpec;

  if FindFirst(ASrch, FileAttrs, sr) = 0 then
    begin
    repeat
      begin
        if ((sr.Attr and faDirectory) <> faDirectory) then // Not a Directory
          begin
            for I := Low(AFileExt) to High(AFileExt) do
              begin
                if CompareText(TPath.GetExtension(sr.Name), AFileExt[I]) = 0 then
                  begin
                    if First then
                      begin
                        First := False;
                        Inc(GroupID);
                        {$ifdef logit}
                        WriteLnLog('');
                        {$endif}
                      end;
                    if ASubDir = String.Empty then
                      DirItem := TFileDirectory.Create(ADir, '', sr.Name, GroupID)
                    else
                      DirItem := TFileDirectory.Create(ADir, ASubDir, sr.Name, GroupID);
                    {$ifdef logit}
                    WriteLnLog(Format('%02d - %s - %s', [DirItem.GroupID, DirItem.SubDir, DirItem.FileName]));
                    {$endif}
                    AList.Add(DirItem);
                    break;
                  end;
              end;
          end
        else
          begin
            if (sr.Name <> '.') and (sr.Name <> '..')  then
              begin
                if ASubDir = String.Empty then
                  ScanModels(ADir, sr.Name, AFileExt, AList, GroupID)
                else
                  ScanModels(ADir, ASubDir + TPath.DirectorySeparatorChar + sr.Name, AFileExt, AList, GroupID);
              end;
          end
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
    end;
end;

procedure SaveModelList(const AFilename: String; const AList: TFileDirectoryList);
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  lSerializer := TJsonSerializer.Create;
  try
    try
      JsonText := lSerializer.Serialize<TFileDirectoryList>(AList);
      try
        TFile.WriteAllText(AFilename, JsonText);
      except
         on E : Exception do
           Raise Exception.Create('Save Model List - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
      end;
    except
     on E : Exception do
     begin
       Raise Exception.Create('Save Model List - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
  end;
end;

function ScanGLTFModels(const ADir: String; const OwnObjects: Boolean): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
  Group: Integer;
begin
  Group := 0;
  TheList := TObjectList<TFileDirectory>.Create(OwnObjects);
  ScanModels(ADir, '', ['.gltf', '.glb'], TheList, Group);
  Result := TheList;
end;

function ScanDAEModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
  Group: Integer;
begin
  Group := 0;
  TheList := TObjectList<TFileDirectory>.Create(OwnObjects);
  ScanModels(ADir, '', ['.dae'], TheList, Group);
  Result := TheList;
end;

function ScanSTLModels(const ADir: String; const OwnObjects: Boolean = True): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
  Group: Integer;
begin
  Group := 0;
  TheList := TObjectList<TFileDirectory>.Create(OwnObjects);
  ScanModels(ADir, '', ['.stl'], TheList, Group);
  Result := TheList;
end;

function ScanOBJModels(const ADir: String; const OwnObjects: Boolean): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
  Group: Integer;
begin
  Group := 0;
  TheList := TObjectList<TFileDirectory>.Create(OwnObjects);
  ScanModels(ADir, '', ['.obj'], TheList, Group);
  Result := TheList;
end;

procedure CheckGLTFTextures(const ADir: String);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  AFileSpec: String;
  ASrch: String;
const
  GLTF: String = '.gltf';
begin
  {$IF DEFINED(MSWINDOWS)}
  AFileSpec := '*.*';
  {$ELSE}
  AFileSpec := '*';
  {$ENDIF}
  FileAttrs := faAnyFile or faDirectory;
  ASrch := ADir + TPath.DirectorySeparatorChar + AFileSpec;
  if FindFirst(ASrch, FileAttrs, sr) = 0 then
    begin
    repeat
      begin
        if ((sr.Attr and faDirectory) <> faDirectory) then // Not a Directory
          begin
            if CompareText(TPath.GetExtension(sr.Name), GLTF) = 0 then
              ScanFile(ADir, sr.Name);
          end
        else
          begin
            if (sr.Name <> '.') and (sr.Name <> '..')  then
              CheckGLTFTextures(ADir + TPath.DirectorySeparatorChar + sr.Name);
          end
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
    end;
end;

{ TFileDirectory }

constructor TFileDirectory.Create(const AParentDir: String; ASubDir,
  AFileName: String; AGroupID: Integer);
begin
  inherited Create;
  fParentDir := AParentDir;
  fSubDir := ASubDir;
  fFileName := AFileName;
  fGroupID := AGroupID;
end;

destructor TFileDirectory.Destroy;
begin
  inherited;
end;

function TFileDirectory.GetFullFileName: String;
begin
  if fSubDir = String.Empty then
    Result := fParentDir + TPath.DirectorySeparatorChar + fFileName
  else
    Result := fParentDir + TPath.DirectorySeparatorChar + fSubDir + TPath.DirectorySeparatorChar + fFileName;
end;

end.
