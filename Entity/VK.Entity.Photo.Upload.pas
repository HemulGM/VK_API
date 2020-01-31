unit VK.Entity.Photo.Upload;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkPhotoUploadResponse = class
  private
    FHash: string;
    FPhoto: string;
    FServer: Integer;
  public
    property Hash: string read FHash write FHash;
    property Photo: string read FPhoto write FPhoto;
    property Server: Integer read FServer write FServer;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoUploadResponse;
  end;

  TVkPhotoGetUploadResponse = class
  private
    FAlbum_id: Integer;
    FUpload_url: string;
    FUser_id: Integer;
    FGroup_id: Integer;
  public
    property AlbumId: Integer read FAlbum_id write FAlbum_id;
    property UploadUrl: string read FUpload_url write FUpload_url;
    property UserId: Integer read FUser_id write FUser_id;
    property GroupId: Integer read FGroup_id write FGroup_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoGetUploadResponse;
  end;

implementation

{TVkPhotoUploadResponse}

function TVkPhotoUploadResponse.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotoUploadResponse.FromJsonString(AJsonString: string): TVkPhotoUploadResponse;
begin
  result := TJson.JsonToObject<TVkPhotoUploadResponse>(AJsonString)
end;

{TVkPhotoGetUploadResponse}

function TVkPhotoGetUploadResponse.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotoGetUploadResponse.FromJsonString(AJsonString: string): TVkPhotoGetUploadResponse;
begin
  result := TJson.JsonToObject<TVkPhotoGetUploadResponse>(AJsonString)
end;

end.

