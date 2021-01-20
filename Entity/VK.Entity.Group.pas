unit VK.Entity.Group;

interface

uses
  System.SysUtils, Generics.Collections, Rest.Json, VK.Entity.Common,
  VK.Entity.Photo, VK.Entity.Market, VK.Entity.Group.Counters,
  VK.Entity.Database.Cities, VK.Entity.Database.Countries, VK.Entity.Common.List;

type
  TVkGroupStatusType = (gsNone, gsOnline, gsAnswerMark);

  TVkGroupSubject = TVkBasicObject;

  TVkGroupStatus = class(TVkEntity)
  private
    FMinutes: Integer;
    FStatus: string;
    function GetStatus: TVkGroupStatusType;
  public
    /// <summary>
    /// ������ ������� ������ � ������� (��� status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// C����� ����������
    /// </summary>
    /// <returns>none � ���������� �� ������; online � ���������� ������ (�������� ���������); answer_mark � ���������� �������� ������.</returns>
    property StatusStr: string read FStatus write FStatus;
    /// <summary>
    /// C����� ����������
    /// </summary>
    property Status: TVkGroupStatusType read GetStatus;
  end;

  TVkCoverImages = TVkEntityList<TVkImage>;

  TVkCover = class
  private
    FEnabled: Boolean;
    FImages: TArray<TVkImage>;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property Images: TArray<TVkImage> read FImages write FImages;
  end;

  TVkGroupLink = class(TVkBasicObject)
  private
    FPhoto_50: string;
    FUrl: string;
    FDesc: string;
    FPhoto_100: string;
    FEdit_title: Integer;
    FImage_processing: Integer;
  public
    property Url: string read FUrl write FUrl;
    property Desc: string read FDesc write FDesc;
    property Photo_50: string read FPhoto_50 write FPhoto_50;
    property Photo_100: string read FPhoto_100 write FPhoto_100;
    property EditTitle: Integer read FEdit_title write FEdit_title;
    property ImageProcessing: Integer read FImage_processing write FImage_processing;
  end;

  TVkGroupMemberState = class(TVkEntity)
  private
    FMember: Boolean;
    FUser_id: Integer;
    FCan_invite: Boolean;
    FRequest: Boolean;
    FInvitation: Boolean;
    FCan_recall: Boolean;
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
    FEnabled: Integer;
    FMain_album_id: Integer;
    FPrice_min: Integer;
    FCurrency_text: string;
    FPrice_max: Integer;
    FContact_id: Integer;
    FCurrency: TVkProductCurrency;
  public
    property Enabled: Integer read FEnabled write FEnabled;
    property PriceMin: Integer read FPrice_min write FPrice_min;
    property PriceMax: Integer read FPrice_max write FPrice_max;
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    property ContactId: Integer read FContact_id write FContact_id;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Name: string read FName write FName;
    property Currency_text: string read FCurrency_text write FCurrency_text;
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

  TVkGroupState = (gsOpen = 0, gsClose = 1, gsPrivate = 2);

  TVkGroup = class(TVkObject)
  private
    FAdmin_level: Integer;
    FIs_admin: Boolean;
    FIs_advertiser: Boolean;
    FIs_closed: Integer;
    FIs_member: Boolean;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FTrending: Boolean;
    FMembers_count: Integer;
    FVerified: Boolean;
    FCountry: TVkCountry;
    FMember_status: Integer;
    FCity: TVkCity;
    FActivity: string;
    FDeactivated: string;
    FInvited_by: Integer;
    FAddresses: TVkAddresses;
    FAge_limits: Integer;
    FBan_info: TVkBanInfo;
    Fcan_create_topic: Boolean;
    Fcan_message: Boolean;
    Fcan_post: Boolean;
    Fcan_see_all_posts: Boolean;
    Fcan_upload_doc: Boolean;
    Fcan_upload_video: Boolean;
    FContacts: TArray<TVkContact>;
    FCover: TVkCover;
    FCrop_photo: TVkCropPhoto;
    FDescription: string;
    FFixed_post: Integer;
    FHas_photo: Boolean;
    FIs_favorite: Boolean;
    FIs_hidden_from_feed: Boolean;
    FIs_messages_blocked: Boolean;
    FLinks: TArray<TVkGroupLink>;
    FMain_album_id: Integer;
    FMain_section: Integer;
    FMarket: TVkGroupMarket;
    FPlace: TVkPlace;
    FPublic_date_label: string;
    FSite: string;
    FStatus: string;
    FWall: Integer;
    FWiki_page: string;
    FCounters: TVkGroupCounters;
    FTrack_code: string;
    function GetIsBanned: Boolean;
    function GetIsDeleted: Boolean;
    function GetIsDeactivated: Boolean;
    function GetFixedPostId: string;
  public
    /// <summary>
    /// ������ �������� �������. � ����� ������������ ��������� ��������, ������� �� ������ ��� ���, � � ������� ���� ������.
    /// </summary>
    property Activity: string read FActivity write FActivity;
    /// <summary>
    /// ������� ���������� �������� ������������ (���� is_admin = 1):
    /// 1 � ���������;
    /// 2 � ��������;
    /// 3 � �������������.
    /// </summary>
    property AdminLevel: Integer read FAdmin_level write FAdmin_level;
    /// <summary>
    /// ���������� �� ������� ����������.
    /// </summary>
    property Addresses: TVkAddresses read FAddresses write FAddresses;
    /// <summary>
    /// ���������� �����������.
    /// 1 � ���; 2 � 16+; 3 � 18+.
    /// </summary>
    property AgeLimits: Integer read FAge_limits write FAge_limits;
    /// <summary>
    /// ���������� � ��������� � ������ ������ ���������� (���� ������������ ������ ��� ������� ���������� �� ����� ����������)
    /// </summary>
    property BanInfo: TVkBanInfo read FBan_info write FBan_info;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ������� ����� ���������� � ������.
    /// </summary>
    property CanCreateTopic: Boolean read Fcan_create_topic write Fcan_create_topic;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ �������� ��������� ����������.
    /// </summary>
    property CanMessage: Boolean read Fcan_message write Fcan_message;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ������ �� ����� ����������.
    /// </summary>
    property CanPost: Boolean read Fcan_post write Fcan_post;
    /// <summary>
    /// ���������� � ���, ��������� �� ������ ����� ������ �� ����� ������.
    /// </summary>
    property CanSeeAllPosts: Boolean read Fcan_see_all_posts write Fcan_see_all_posts;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ��������� � ������.
    /// </summary>
    property CanUploadDoc: Boolean read Fcan_upload_doc write Fcan_upload_doc;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ����������� � ������.
    /// </summary>
    property CanUploadVideo: Boolean read Fcan_upload_video write Fcan_upload_video;
    /// <summary>
    /// �����, ��������� � ���������� � ����������.
    /// </summary>
    property City: TVkCity read FCity write FCity;
    /// <summary>
    /// ���������� �� ����� ��������� ��������� ��������.
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
    /// ������������ � ������, ���� ���������� ������� ��� �������������. ��������� ��������:
    /// deleted � ���������� �������;
    /// banned � ���������� �������������;
    /// </summary>
    property Deactivated: string read FDeactivated write FDeactivated;
    /// <summary>
    /// ���������� �������
    /// </summary>
    property IsDeleted: Boolean read GetIsDeleted;
    /// <summary>
    /// ���������� �������������
    /// </summary>
    property IsBanned: Boolean read GetIsBanned;
    /// <summary>
    /// ���������� �� �������
    /// </summary>
    property IsDeactivated: Boolean read GetIsDeactivated;
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
    /// �������� �� ���������� ��������. ��������� ��������:
    /// 0 � ��������;
    /// 1 � ��������;
    /// 2 � �������.
    /// </summary>
    property IsClosed: Integer read FIs_closed write FIs_closed;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property IsMessagesBlocked: Boolean read FIs_messages_blocked write FIs_messages_blocked;
    property IsMember: Boolean read FIs_member write FIs_member;
    property Links: TArray<TVkGroupLink> read FLinks write FLinks;
    property MainAlbumId: Integer read FMain_album_id write FMain_album_id;
    property MainSection: Integer read FMain_section write FMain_section;
    property Market: TVkGroupMarket read FMarket write FMarket;
    property MembersCount: Integer read FMembers_count write FMembers_count;
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property TrackCode: string read FTrack_code write FTrack_code;
    property Place: TVkPlace read FPlace write FPlace;
    property PublicDateLabel: string read FPublic_date_label write FPublic_date_label;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Site: string read FSite write FSite;
    property Status: string read FStatus write FStatus;
    property&Type: string read FType write FType;
    property Trending: Boolean read FTrending write FTrending;
    property Verified: Boolean read FVerified write FVerified;
    property Wall: Integer read FWall write FWall;
    property WikiPage: string read FWiki_page write FWiki_page;
    constructor Create; override;
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

constructor TVkGroup.Create;
begin
  inherited;
end;

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
  inherited;
end;

function TVkGroup.GetFixedPostId: string;
begin
  //{group_id}_{post_id}
  Result := FId.ToString + '_' + FFixed_post.ToString;
end;

function TVkGroup.GetIsBanned: Boolean;
begin
  Result := FDeactivated = 'banned';
end;

function TVkGroup.GetIsDeactivated: Boolean;
begin
  Result := not FDeactivated.IsEmpty;
end;

function TVkGroup.GetIsDeleted: Boolean;
begin
  Result := FDeactivated = 'deleted';
end;

{ TVkGroupStatus }

function TVkGroupStatus.GetStatus: TVkGroupStatusType;
begin
  if FStatus = 'none' then
    Exit(gsNone);
  if FStatus = 'online' then
    Exit(gsOnline);
  if FStatus = 'answer_mark' then
    Exit(gsAnswermark);
  Result := gsNone;
end;

end.

