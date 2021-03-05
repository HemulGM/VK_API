unit VK.Entity.Group;

interface

uses
  System.SysUtils, Generics.Collections, Rest.Json, VK.Entity.Common, VK.Types, VK.Entity.Photo, REST.JsonReflect,
  REST.Json.Interceptors, VK.Entity.Market, VK.Entity.Group.Counters, VK.Wrap.Interceptors, VK.Entity.Database.Cities,
  VK.Entity.Database.Countries, VK.Entity.Common.List, VK.Entity.Geo;

type
  TVkGroupSubject = TVkBasicObject;

  TVkCoverImages = TVkEntityList<TVkImage>;

  TVkCover = class
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEnabled: Boolean;
    FImages: TArray<TVkImage>;
  public
    /// <summary>
    /// ���������� � ���, �������� �� �������
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// ����� ����������� �������
    /// </summary>
    property Images: TArray<TVkImage> read FImages write FImages;
    destructor Destroy; override;
  end;

  TVkGroupLink = class(TVkBasicObject)
  private
    FPhoto_50: string;
    FUrl: string;
    FDesc: string;
    FPhoto_100: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEdit_title: Boolean;
    FImage_processing: Integer;
  public
    /// <summary>
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// URL
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Name;
    /// <summary>
    /// �������� ������
    /// </summary>
    property Desc: string read FDesc write FDesc;
    /// <summary>
    /// URL �����������-������ ������� 50px
    /// </summary>
    property Photo_50: string read FPhoto_50 write FPhoto_50;
    /// <summary>
    /// URL �����������-������ ������� 100px
    /// </summary>
    property Photo_100: string read FPhoto_100 write FPhoto_100;
    property EditTitle: Boolean read FEdit_title write FEdit_title;
    property ImageProcessing: Integer read FImage_processing write FImage_processing;
  end;

  TVkGroupMemberState = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FMember: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_invite: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRequest: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FInvitation: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_recall: Boolean;
    FUser_id: Integer;
  public
    /// <summary>
    /// �������� �� ������������ ���������� ����������
    /// </summary>
    property Member: Boolean read FMember write FMember;
    /// <summary>
    /// ���� �� ���������� ������ �� ������������ �� ���������� � ������ (����� ������ ����� �������� ������� Groups.Leave)
    /// </summary>
    property Request: Boolean read FRequest write FRequest;
    /// <summary>
    /// ��������� �� ������������ � ������ ��� �������
    /// </summary>
    property Invitation: Boolean read FInvitation write FInvitation;
    /// <summary>
    /// ����� �� ����� ������� ���������� ������������ � ������
    /// </summary>
    property CanInvite: Boolean read FCan_invite write FCan_invite;
    /// <summary>
    /// ����� �� ����� �������� �����������. ����������, ���� Invitation: True
    /// </summary>
    property CanRecall: Boolean read FCan_recall write FCan_recall;
    /// <summary>
    /// ������������� ������������.
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
  end;

  TVkGroupMemberStates = TVkEntityList<TVkGroupMemberState>;

  TVkGroupMarket = class(TVkObject)
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEnabled: Boolean;
    FMain_album_id: Integer;
    FPrice_min: Integer;
    FCurrency_text: string;
    FPrice_max: Integer;
    FContact_id: Integer;
    FCurrency: TVkProductCurrency;
    FType: string;
  public
    /// <summary>
    /// ���������� � ���, ������� �� ���� ������� � ����������
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// ����������� ���� �������
    /// </summary>
    property PriceMin: Integer read FPrice_min write FPrice_min;
    /// <summary>
    /// ������������ ���� �������
    /// </summary>
    property PriceMax: Integer read FPrice_max write FPrice_max;
    /// <summary>
    /// ������������� ������� �������� �������
    /// </summary>
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    /// <summary>
    /// ������������� ����������� ���� ��� ����� � ���������. ������������ ������������� ��������, ���� ��� ����� � ��������� ������������ ��������� ����������
    /// </summary>
    property ContactId: Integer read FContact_id write FContact_id;
    /// <summary>
    /// ���������� � ������
    /// </summary>
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Name: string read FName write FName;
    /// <summary>
    /// ���������� � ���� ��������
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// ��������� �����������
    /// </summary>
    property CurrencyText: string read FCurrency_text write FCurrency_text;
    destructor Destroy; override;
  end;

  TVkGroupAddress = class(TVkObject)
  private
    FAdditional_address: string;
    FAddress: string;
    FCity_id: Integer;
    FCountry_id: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FMetro_station_id: Integer;
    FTime_offset: Integer;
    FTitle: string;
    FWork_info_status: string;
  public
    property Additional_address: string read FAdditional_address write FAdditional_address;
    property Address: string read FAddress write FAddress;
    property CityId: Integer read FCity_id write FCity_id;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property MetroStationId: Integer read FMetro_station_id write FMetro_station_id;
    property TimeOffset: Integer read FTime_offset write FTime_offset;
    property Title: string read FTitle write FTitle;
    property WorkInfoStatus: string read FWork_info_status write FWork_info_status;
  end;

  TVkGroupAddresses = TVkEntityList<TVkGroupAddress>;

  TVkBanInfo = class
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FComment: string;
  public
    /// <summary>
    /// ���� ��������� ����������
    /// </summary>
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    /// <summary>
    /// ����������� � ����������
    /// </summary>
    property Comment: string read FComment write FComment;
  end;

  TVkContact = class
  private
    FEmail: string;
    FPhone: string;
    FDesc: string;
    FUser_id: Integer;
  public
    /// <summary>
    /// ������������� ������������
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
    /// <summary>
    /// ���������
    /// </summary>
    property Desc: string read FDesc write FDesc;
    /// <summary>
    /// ����� ��������
    /// </summary>
    property Phone: string read FPhone write FPhone;
    /// <summary>
    /// ����� e-mail
    /// </summary>
    property Email: string read FEmail write FEmail;
  end;

  TVkAddresses = class
  private
    FIs_enabled: Boolean;
    FMain_address_id: Integer;
  public
    /// <summary>
    /// ������� �� ���� ������� � ����������
    /// </summary>
    property IsEnabled: Boolean read FIs_enabled write FIs_enabled;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    property MainAddressId: Integer read FMain_address_id write FMain_address_id;
  end;

  TVkGroup = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TGroupAdminLevelInterceptor)]
    FAdmin_level: TVkGroupAdminLevel;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_admin: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_advertiser: Boolean;
    [JsonReflectAttribute(ctString, rtString, TGroupAccessInterceptor)]
    FIs_closed: TVkGroupAccess;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_member: Boolean;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    [JsonReflectAttribute(ctString, rtString, TGroupTypeInterceptor)]
    FType: TVkGroupType;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FTrending: Boolean;
    FMembers_count: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FVerified: Boolean;
    FCountry: TVkCountry;
    FMember_status: Integer;
    FCity: TVkCity;
    FActivity: string;
    [JsonReflectAttribute(ctString, rtString, TDeactivatedInterceptor)]
    FDeactivated: TVkDeactivated;
    FInvited_by: Integer;
    FAddresses: TVkAddresses;
    [JsonReflectAttribute(ctString, rtString, TAgeLimitsInterceptor)]
    FAge_limits: TVkAgeLimits;
    FBan_info: TVkBanInfo;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_create_topic: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_message: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_post: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_see_all_posts: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_upload_doc: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_upload_video: Boolean;
    FContacts: TArray<TVkContact>;
    FCover: TVkCover;
    FCrop_photo: TVkCropPhoto;
    FDescription: string;
    FFixed_post: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FHas_photo: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_favorite: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_hidden_from_feed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_messages_blocked: Boolean;
    FLinks: TArray<TVkGroupLink>;
    FMain_album_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TGroupMainSectionInterceptor)]
    FMain_section: TVkGroupMainSection;
    FMarket: TVkGroupMarket;
    FPlace: TVkPlace;
    FPublic_date_label: string;
    FSite: string;
    FStatus: string;
    FWall: Integer;
    FWiki_page: string;
    FCounters: TVkGroupCounters;
    FTrack_code: string;
    [JsonReflectAttribute(ctString, rtString, TIntDateTimeInterceptor)]
    FStart_date: TDateTime;
    function GetFixedPostId: string;
  public
    /// <summary>
    /// ������ �������� �������. � ����� ������������ ��������� ��������, ������� �� ������ ��� ���, � � ������� ���� ������
    /// </summary>
    property Activity: string read FActivity write FActivity;
    /// <summary>
    /// ������� ���������� �������� ������������ (���� IsAdmin = True)
    /// </summary>
    property AdminLevel: TVkGroupAdminLevel read FAdmin_level write FAdmin_level;
    /// <summary>
    /// ���������� �� ������� ����������
    /// </summary>
    property Addresses: TVkAddresses read FAddresses write FAddresses;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    property AgeLimits: TVkAgeLimits read FAge_limits write FAge_limits;
    /// <summary>
    /// ���������� � ��������� � ������ ������ ���������� (���� ������������ ������ ��� ������� ���������� �� ����� ����������)
    /// </summary>
    property BanInfo: TVkBanInfo read FBan_info write FBan_info;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ������� ����� ���������� � ������
    /// </summary>
    property CanCreateTopic: Boolean read FCan_create_topic write FCan_create_topic;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ �������� ��������� ����������
    /// </summary>
    property CanMessage: Boolean read FCan_message write FCan_message;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ������ �� ����� ����������
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_message;
    /// <summary>
    /// ���������� � ���, ��������� �� ������ ����� ������ �� ����� ������
    /// </summary>
    property CanSeeAllPosts: Boolean read FCan_see_all_posts write FCan_see_all_posts;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ��������� � ������
    /// </summary>
    property CanUploadDoc: Boolean read FCan_upload_doc write FCan_upload_doc;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ����������� � ������
    /// </summary>
    property CanUploadVideo: Boolean read FCan_upload_video write FCan_upload_video;
    /// <summary>
    /// �����, ��������� � ���������� � ����������
    /// </summary>
    property City: TVkCity read FCity write FCity;
    /// <summary>
    /// ���������� �� ����� ��������� ��������� ��������
    /// </summary>
    property Contacts: TArray<TVkContact> read FContacts write FContacts;
    /// <summary>
    /// ������, ���������� �������� ����������, ����� �������� ����� ����� �� ��������� �����: photos, albums, audios, videos, topics, docs.
    /// </summary>
    property Counters: TVkGroupCounters read FCounters write FCounters;
    /// <summary>
    /// ������, ��������� � ���������� � ����������.
    /// </summary>
    property Country: TVkCountry read FCountry write FCountry;
    /// <summary>
    /// ������� ����������.
    /// </summary>
    property Cover: TVkCover read FCover write FCover;
    /// <summary>
    /// ���������� ������ � ������, �� ������� �������� ���������� � ����������� ���������� ����������.
    /// </summary>
    property CropPhoto: TVkCropPhoto read FCrop_photo write FCrop_photo;
    /// <summary>
    /// ������������ � ������, ���� ���������� ������� ��� �������������
    /// </summary>
    property Deactivated: TVkDeactivated read FDeactivated write FDeactivated;
    /// <summary>
    /// ����� �������� ����������.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// ������������� ������������ ������. �������� �������������� ������ � ������ ����� ������� wall.getById, ������� � ���� posts {group_id}_{post_id}.
    /// </summary>
    property FixedPost: Integer read FFixed_post write FFixed_post;
    /// <summary>
    /// ������������� ������������ ������. �������� �������������� ������ � ������ ����� ������� wall.getById, ������� � ���� posts.
    /// </summary>
    property FixedPostId: string read GetFixedPostId;
    /// <summary>
    /// ���������� � ���, ����������� �� � ���������� ������� ����������.
    /// </summary>
    property HasPhoto: Boolean read FHas_photo write FHas_photo;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    property Id;
    /// <summary>
    /// ������������� ������������, ������� �������� ����������� � ����������.
    /// ���� ������������ ������ ��� ������ groups.getInvites.
    /// </summary>
    property InvitedBy: Integer read FInvited_by write FInvited_by;
    /// <summary>
    /// ���������� � ���, �������� �� ������� ������������ �������������.
    /// </summary>
    property IsAdmin: Boolean read FIs_admin write FIs_admin;
    /// <summary>
    /// ���������� � ���, �������� �� ������� ������������ ��������������.
    /// </summary>
    property IsAdvertiser: Boolean read FIs_advertiser write FIs_advertiser;
    /// <summary>
    /// �������� �� ���������� ��������
    /// </summary>
    property IsClosed: TVkGroupAccess read FIs_closed write FIs_closed;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property IsMessagesBlocked: Boolean read FIs_messages_blocked write FIs_messages_blocked;
    property IsMember: Boolean read FIs_member write FIs_member;
    /// <summary>
    /// ���������� �� ����� ������ ����������
    /// </summary>
    property Links: TArray<TVkGroupLink> read FLinks write FLinks;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    /// <summary>
    /// ���������� � ������� ������
    /// </summary>
    property MainSection: TVkGroupMainSection read FMain_section write FMain_section;
    /// <summary>
    /// ���������� � ��������
    /// </summary>
    property Market: TVkGroupMarket read FMarket write FMarket;
    /// <summary>
    /// ���������� ���������� � ����������
    /// </summary>
    property MembersCount: Integer read FMembers_count write FMembers_count;
    { TODO -oHemulGM -c : ������� ��� 16.02.2021 13:57:27 }
    /// <summary>
    /// ������ ��������� �������� ������������
    /// </summary>
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// �����, ��������� � ���������� � ����������
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    property PublicDateLabel: string read FPublic_date_label write FPublic_date_label;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Site: string read FSite write FSite;
    property StartDate: TDateTime read FStart_date write FStart_date;
    property Status: string read FStatus write FStatus;
    property&Type: TVkGroupType read FType write FType;
    property Trending: Boolean read FTrending write FTrending;
    property Verified: Boolean read FVerified write FVerified;
    property Wall: Integer read FWall write FWall;
    property WikiPage: string read FWiki_page write FWiki_page;
    destructor Destroy; override;
  end;

  TVkGroups = TVkEntityList<TVkGroup>;

  TVkGroupTag = class(TVkObject)
  private
    FColor: string;
    FName: string;
  public
    property Color: string read FColor write FColor;
    property Id;
    property Name: string read FName write FName;
  end;

  TVkGroupTags = TVkEntityList<TVkGroupTag>;

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;

implementation

uses
  VK.CommonUtils;

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(List) to High(List) do
    if List[i].Id = Abs(Id) then
      Exit(i);
end;

{TVkGroup}

destructor TVkGroup.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkContact>(FContacts);
  TArrayHelp.FreeArrayOfObject<TVkGroupLink>(FLinks);
  if Assigned(FCity) then
    FCity.Free;
  if Assigned(FCountry) then
    FCountry.Free;
  if Assigned(FAddresses) then
    FAddresses.Free;
  if Assigned(FBan_info) then
    FBan_info.Free;
  if Assigned(FCover) then
    FCover.Free;
  if Assigned(FCounters) then
    FCounters.Free;
  if Assigned(FCrop_photo) then
    FCrop_photo.Free;
  if Assigned(FMarket) then
    FMarket.Free;
  if Assigned(FPlace) then
    FPlace.Free;
  inherited;
end;

function TVkGroup.GetFixedPostId: string;
begin
  //{group_id}_{post_id}
  Result := FId.ToString + '_' + FFixed_post.ToString;
end;

{ TVkGroupMarket }

destructor TVkGroupMarket.Destroy;
begin
  if Assigned(FCurrency) then
    FCurrency.Free;
  inherited;
end;

{ TVkCover }

destructor TVkCover.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImages);
  inherited;
end;

end.

