unit VK.Entity.Album;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo;

type
  TVkPhotoAlbum = class
  private
    FCreated: Extended;
    FDescription: string;
    FId: string;
    FOwner_id: Extended;
    FSize: Extended;
    FThumb: TVkPhoto;
    FTitle: string;
    FUpdated: Extended;
  public
    property Created: Extended read FCreated write FCreated;
    property Description: string read FDescription write FDescription;
    property Id: string read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property Size: Extended read FSize write FSize;
    property Thumb: TVkPhoto read FThumb write FThumb;
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
  FThumb := TVkPhoto.Create();
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

end.

