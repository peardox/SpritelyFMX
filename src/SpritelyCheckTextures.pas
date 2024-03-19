unit SpritelyCheckTextures;

interface

uses System.Generics.Collections{, FMX.Types};


type
  TFileDirectory = class {(TFMXObject)}
  private
    fParentDir: String;
    fSubDir: String;
    fFileName: String;
  public
    constructor Create(const AParentDir: String; ASubDir: String; AFileName: String);
    function GetFullFileName: String;
    property ParentDir: String read fParentDir write fParentDir;
    property SubDir: String read fSubDir write fSubDir;
    property FileName: String read fFileName write fFileName;
  end;

procedure CheckGLTFTextures(const ADir: String);
function ScanGLTFModels(const ADir: String): TObjectList<TFileDirectory>;
function ScanOBJModels(const ADir: String): TObjectList<TFileDirectory>;

implementation

uses System.SysUtils,
  System.Variants,
  System.Classes,
  CastleLog,
  System.JSON,
  JSON.Types,
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

procedure ScanModels(const ADir: String; const ASubDir: String; const AFileExt: TArray<String>; var AList: TObjectList<TFileDirectory>);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  AFileSpec: String;
  ASrch: String;
  I: Integer;
  DirItem: TFileDirectory;
begin
  {$IF DEFINED(MSWINDOWS)}
  AFileSpec := '*.*';
  {$ELSE}
  AFileSpec := '*';
  {$ENDIF}
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
                if TPath.GetExtension(sr.Name) = AFileExt[I] then
                  begin
                    if ASubDir = String.Empty then
                      DirItem := TFileDirectory.Create(ADir, '', sr.Name)
                    else
                      DirItem := TFileDirectory.Create(ADir, ASubDir, sr.Name);
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
                  ScanModels(ADir, sr.Name, AFileExt, AList)
                else
                  ScanModels(ADir, ASubDir + TPath.DirectorySeparatorChar + sr.Name, AFileExt, AList);
              end;
          end
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
    end;
end;

function ScanGLTFModels(const ADir: String): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
begin
  TheList := TObjectList<TFileDirectory>.Create;
  ScanModels(ADir, '', ['.gltf', '.glb'], TheList);
  Result := TheList;
end;

function ScanOBJModels(const ADir: String): TObjectList<TFileDirectory>;
var
  TheList: TObjectList<TFileDirectory>;
begin
  TheList := TObjectList<TFileDirectory>.Create;
  ScanModels(ADir, '', ['.obj'], TheList);
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
            if TPath.GetExtension(sr.Name) = GLTF then
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
  AFileName: String);
begin
  inherited Create;
  fParentDir := AParentDir;
  fSubDir := ASubDir;
  fFileName := AFileName;
end;

function TFileDirectory.GetFullFileName: String;
begin
  if fSubDir = String.Empty then
    Result := fParentDir + TPath.DirectorySeparatorChar + fFileName
  else
    Result := fParentDir + TPath.DirectorySeparatorChar + fSubDir + TPath.DirectorySeparatorChar + fFileName;
end;

end.
