unit SpritelyCheckTextures;

interface

procedure CheckGLTFTextures(const ADir: String);

implementation

uses System.SysUtils,
  System.Variants,
  System.Generics.Collections,
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

end.
