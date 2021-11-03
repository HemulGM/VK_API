unit VK.Entity.Album;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Photo, VK.Entity.Common, VK.Entity.Privacy, VK.Entity.Common.List,
  VK.Types, VK.Wrap.Interceptors;

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
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FThumb_is_last: Boolean;
    FPrivacy_view: TVkPrivacy;
    FPrivacy_comment: TVkPrivacy;
    FSizes: TVkSizes;
    FThumb_src: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUpload_by_admins_only: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FComments_disabled: Boolean;
    FCan_upload: Boolean;
    FAccess_key: string;
  public
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    property Id;
    property AccessKey: string read FAccess_key write FAccess_key;
    property CanUpload: Boolean read FCan_upload write FCan_upload;
    property CommentsDisabled: Boolean read FComments_disabled write FComments_disabled;
    /// <summary>
    /// Дата создания альбома
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// Описание альбома
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Идентификатор владельца альбома
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PrivacyComment: TVkPrivacy read FPrivacy_comment write FPrivacy_comment;
    property PrivacyView: TVkPrivacy read FPrivacy_view write FPrivacy_view;
    /// <summary>
    /// Количество фотографий в альбоме
    /// </summary>
    property Size: Integer read FSize write FSize;
    property Sizes: TVkSizes read FSizes write FSizes;
    /// <summary>
    /// Обложка альбома, объект photo
    /// </summary>
    property Thumb: TVkThumb read FThumb write FThumb;
    property ThumbId: integer read FThumb_id write FThumb_id;
    property ThumbIsLast: Boolean read FThumb_is_last write FThumb_is_last;
    property ThumbSrc: string read FThumb_src write FThumb_src;
    /// <summary>
    /// Название альбома.
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// Дата последнего обновления альбома
    /// </summary>
    property Updated: TDateTime read FUpdated write FUpdated;
    property UploadByAdminsOnly: Boolean read FUpload_by_admins_only write FUpload_by_admins_only;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkPhotoAlbums = TVkEntityList<TVkPhotoAlbum>;

implementation

uses
  VK.CommonUtils;

{TVkPhotoAlbum}

destructor TVkPhotoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  if Assigned(FThumb) then
    FThumb.Free;
  if Assigned(FPrivacy_view) then
    FPrivacy_view.Free;
  if Assigned(FPrivacy_comment) then
    FPrivacy_comment.Free;
  inherited;
end;

function TVkPhotoAlbum.ToAttachment: TAttachment;
begin
  Result := TAttachment.Album(OwnerId, Id, AccessKey);
end;

end.

