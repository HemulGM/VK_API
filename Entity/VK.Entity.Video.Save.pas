unit VK.Entity.Video.Save;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Types;

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

  TVkVideoUploadResponse = class(TVkEntity, IAttachment)
  private
    FOwner_id: Integer;
    FVideo_hash: string;
    FSize: Int64;
    FVideo_id: Integer;
    FAccess_key: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property VideoHash: string read FVideo_hash write FVideo_hash;
    property Size: Int64 read FSize write FSize;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property VideoId: Integer read FVideo_id write FVideo_id;
    function ToAttachment: TAttachment;
  end;

implementation

{ TVkVideoUploadResponse }

function TVkVideoUploadResponse.ToAttachment: TAttachment;
begin
  Result.Id := FVideo_id;
  Result.&Type := 'video';
  Result.OwnerId := FOwner_id;
  Result.Kind := TAttachmentKind.Media;
  Result.AccessKey := FAccess_key;
end;

end.

