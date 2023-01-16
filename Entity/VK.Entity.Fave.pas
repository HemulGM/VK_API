unit VK.Entity.Fave;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Link,
  VK.Entity.Media, VK.Entity.Video, VK.Entity.Market, VK.Entity.Photo,
  VK.Entity.Common, VK.Entity.Common.List, VK.Types, VK.Wrap.Interceptors;

type
  TVkFaveTag = TVkBasicObject;

  TVkFaveTags = TVkEntityList<TVkFaveTag>;

  TVkFave = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FAdded_date: TDateTime;
    FSeen: Boolean;
    FTags: TArray<TVkFaveTag>;
    [JsonReflectAttribute(ctString, rtString, TFaveTypeInterceptor)]
    FType: TVkFaveType;
    FLink: TVkLink;
    FPost: TVkPost;
    FVideo: TVkVideo;
    FProduct: TVkProduct;
    FPhoto: TVkPhoto;
  public
    property AddedDate: TDateTime read FAdded_date write FAdded_date;
    property Link: TVkLink read FLink write FLink;
    property Post: TVkPost read FPost write FPost;
    property Video: TVkVideo read FVideo write FVideo;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Product: TVkProduct read FProduct write FProduct;
    property Seen: Boolean read FSeen write FSeen;
    property Tags: TArray<TVkFaveTag> read FTags write FTags;
    property &Type: TVkFaveType read FType write FType;
    destructor Destroy; override;
  end;

  TVkFaves = TVkEntityList<TVkFave>;

implementation

uses
  VK.CommonUtils;

destructor TVkFave.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaveTag>(FTags);
  if Assigned(FLink) then
    FLink.Free;
  if Assigned(FPost) then
    FPost.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  if Assigned(FProduct) then
    FProduct.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  inherited;
end;

end.

