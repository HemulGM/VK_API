unit VK.Entity.Album;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Photo, VK.Entity.Common,
  VK.Entity.Attachment, VK.Entity.Privacy, VK.Entity.Common.List;

type
  TVkPhotoAlbum = class(TVkObject, IAttachment)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    FDescription: string;
    FOwner_id: Integer;
    FSize: Integer;
    FThumb: TVkThumb;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdated: TDateTime;
    FThumb_id: integer;
    FThumb_is_last: Integer;
    FPrivacy_view: TVkPrivacy;
    FPrivacy_comment: TVkPrivacy;
    FSizes: TVkSizes;
    FThumb_src: string;
    FUpload_by_admins_only: Boolean;
    FComments_disabled: Boolean;
    FCan_upload: Boolean;
    FAccess_key: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property CanUpload: Boolean read FCan_upload write FCan_upload;
    property CommentsDisabled: Boolean read FComments_disabled write FComments_disabled;
    property Created: TDateTime read FCreated write FCreated;
    property Description: string read FDescription write FDescription;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PrivacyComment: TVkPrivacy read FPrivacy_comment write FPrivacy_comment;
    property PrivacyView: TVkPrivacy read FPrivacy_view write FPrivacy_view;
    property Size: Integer read FSize write FSize;
    property Sizes: TVkSizes read FSizes write FSizes;
    property Thumb: TVkThumb read FThumb write FThumb;
    property ThumbId: integer read FThumb_id write FThumb_id;
    property ThumbIsLast: Integer read FThumb_is_last write FThumb_is_last;
    property ThumbSrc: string read FThumb_src write FThumb_src;
    property Title: string read FTitle write FTitle;
    property Updated: TDateTime read FUpdated write FUpdated;
    property UploadByAdminsOnly: Boolean read FUpload_by_admins_only write FUpload_by_admins_only;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: string;
  end;

  TVkPhotoAlbums = TVkEntityList<TVkPhotoAlbum>;

implementation

uses
  System.DateUtils, VK.Types, VK.CommonUtils;

{TVkPhotoAlbum}

constructor TVkPhotoAlbum.Create;
begin
  inherited;
  FThumb := TVkThumb.Create();
  FPrivacy_view := TVkPrivacy.Create();
  FPrivacy_comment := TVkPrivacy.Create();
end;

destructor TVkPhotoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  FThumb.Free;
  FPrivacy_view.Free;
  FPrivacy_comment.Free;
  inherited;
end;

function TVkPhotoAlbum.ToAttachment: string;
begin
  Result := Attachment.Album(Id, OwnerId, AccessKey);
end;

end.

