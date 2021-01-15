unit VK.Entity.Video.Save;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkVideoSaved = class(TVkEntity)
  private
    FAccess_key: string;
    FDescription: string;
    FOwner_id: Integer;
    FTitle: string;
    FUpload_url: string;
    FVideo_id: Integer;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Description: string read FDescription write FDescription;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property UploadUrl: string read FUpload_url write FUpload_url;
    property VideoId: Integer read FVideo_id write FVideo_id;
  end;

implementation

end.

