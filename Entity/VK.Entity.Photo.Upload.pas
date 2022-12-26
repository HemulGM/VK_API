unit VK.Entity.Photo.Upload;

interface

uses
  VK.Entity.Common, VK.Types;

type
  TVkPhotoUploadResponse = class(TVkEntity)
  private
    FHash: string;
    FPhoto: string;
    FServer: Integer;
  public
    property Hash: string read FHash write FHash;
    property Photo: string read FPhoto write FPhoto;
    property Server: Integer read FServer write FServer;
  end;

  TVkPhotoGetUploadResponse = class(TVkEntity)
  private
    FAlbum_id: Integer;
    FUpload_url: string;
    FUser_id: TVkPeerId;
    FGroup_id: TVkPeerId;
  public
    property AlbumId: Integer read FAlbum_id write FAlbum_id;
    property UploadUrl: string read FUpload_url write FUpload_url;
    property UserId: TVkPeerId read FUser_id write FUser_id;
    property GroupId: TVkPeerId read FGroup_id write FGroup_id;
  end;

implementation

end.

