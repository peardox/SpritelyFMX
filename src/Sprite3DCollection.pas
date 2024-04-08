unit Sprite3DCollection;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.JSON, JSON.Types, System.Generics.Collections
  ;

type
  TSprite3DFile = class
    Name: String;
    FileName: String;
    FileHash: String;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSprite3DFolder = class
    Name: String;
    SubDir: String;
    Files: TObjectDictionary<String, TSprite3DFile>;
    Folders: TObjectDictionary<String, TSprite3DFolder>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSprite3DCollection = class
    Name: String;
    RootDir: String;
    Files: TObjectDictionary<String, TSprite3DFile>;
    Folders: TObjectDictionary<String, TSprite3DFolder>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

implementation

{ TSprite3DCollection }

constructor TSprite3DCollection.Create;
begin

end;

destructor TSprite3DCollection.Destroy;
begin
  inherited;
end;

procedure TSprite3DCollection.Load;
begin

end;

procedure TSprite3DCollection.Save;
begin

end;

{ TSprite3DFolder }

constructor TSprite3DFolder.Create;
begin

end;

destructor TSprite3DFolder.Destroy;
begin
  inherited;
end;

{ TSprite3DFile }

constructor TSprite3DFile.Create;
begin

end;

destructor TSprite3DFile.Destroy;
begin
  inherited;
end;

end.
