unit VK.Entity.Album;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo;

type
  TVkAlbumThumb = class
  private
    FHeight: Integer;
    FPhoto_135: string;
    FPhoto_270: string;
    FPhoto_300: string;
    FPhoto_34: string;
    FPhoto_600: string;
    FPhoto_68: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAlbumThumb;
  end;

  TVkPhotoAlbum = class
  private
    FCreated: Extended;
    FDescription: string;
    FId: string;
    FOwner_id: Integer;
    FSize: Integer;
    FThumb: TVkAlbumThumb;
    FTitle: string;
    FUpdated: Extended;
  public
    property Created: Extended read FCreated write FCreated;
    property Description: string read FDescription write FDescription;
    property Id: string read FId write FId;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Size: Integer read FSize write FSize;
    property Thumb: TVkAlbumThumb read FThumb write FThumb;
    property Title: string read FTitle write FTitle;
    property Updated: Extended read FUpdated write FUpdated;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoAlbum;
  end;

implementation

{TVkPhotoAlbum}

constructor TVkPhotoAlbum.Create;
begin
  inherited;
  FThumb := TVkAlbumThumb.Create();
end;

destructor TVkPhotoAlbum.Destroy;
begin
  FThumb.Free;
  inherited;
end;

function TVkPhotoAlbum.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotoAlbum.FromJsonString(AJsonString: string): TVkPhotoAlbum;
begin
  result := TJson.JsonToObject<TVkPhotoAlbum>(AJsonString)
end;

{TVkAlbumThumb}

function TVkAlbumThumb.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAlbumThumb.FromJsonString(AJsonString: string): TVkAlbumThumb;
begin
  result := TJson.JsonToObject<TVkAlbumThumb>(AJsonString)
end;

end.

