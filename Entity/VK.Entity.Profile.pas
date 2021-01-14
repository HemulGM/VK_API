unit VK.Entity.Profile;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common,
  VK.Entity.Photo, VK.Entity.Database.Cities, VK.Entity.Database.Countries;

type
  TVkProfile = class;

  TVkFriendsMutual = class(TVkEntity)
  private
    FCount: Integer;
    FUsers: TArray<TVkProfile>;
  public
    property Count: Integer read FCount write FCount;
    property Users: TArray<TVkProfile> read FUsers write FUsers;
    destructor Destroy; override;
  end;

  TVkFriendsOnline = class(TVkEntity)
  private
    FOnline: TArray<Integer>;
    FOnline_mobile: TArray<Integer>;
  public
    property Online: TArray<Integer> read FOnline write FOnline;
    property OnlineMobile: TArray<Integer> read FOnline_mobile write FOnline_mobile;
  end;

  TVkFriendInfo = class(TVkEntity)
  private
    FFriend_status: Integer;
    FSign: string;
    FUser_id: Integer;
    FIs_request_unread: Boolean;
  public
    property FriendStatus: Integer read FFriend_status write FFriend_status;
    property IsRequestUnread: Boolean read FIs_request_unread write FIs_request_unread;
    property Sign: string read FSign write FSign;
    property UserId: Integer read FUser_id write FUser_id;
  end;

  TVkFriendInfos = class(TVkEntity)
  private
    FItems: TArray<TVkFriendInfo>;
  public
    property Items: TArray<TVkFriendInfo> read FItems write FItems;
    destructor Destroy; override;
  end;

  TVkFriendDeleteInfo = class(TVkEntity)
  private
    FSuccess: Integer;
    Fout_request_deleted: Integer;
    Fin_request_deleted: Integer;
    Fsuggestion_deleted: Integer;
    Ffriend_deleted: Integer;
    function GetSuccess: Boolean;
    procedure SetSuccess(const Value: Boolean);
  public
    property Success: Boolean read GetSuccess write SetSuccess;
    property FriendDeleted: Integer read Ffriend_deleted write Ffriend_deleted;
    property OutRequestDeleted: Integer read Fout_request_deleted write Fout_request_deleted;
    property InRequestDeleted: Integer read Fin_request_deleted write Fin_request_deleted;
    property SuggestionDeleted: Integer read Fsuggestion_deleted write Fsuggestion_deleted;
  end;

  TVkRelative = class(TVkObject)
  private
    FType: string;
  public
    property TypeRelative: string read FType write FType;
  end;

  TVkSchoolInfo = class(TVkObject)
  private
    FCity: Integer;
    FClass: string;
    FCountry: Integer;
    FName: string;
    FSpeciality: string;
    FYear_from: Integer;
    FYear_graduated: Integer;
    FYear_to: Integer;
  public
    property City: Integer read FCity write FCity;
    property ClassNum: string read FClass write FClass;
    property Country: Integer read FCountry write FCountry;
    property Name: string read FName write FName;
    property Speciality: string read FSpeciality write FSpeciality;
    property YearFrom: Integer read FYear_from write FYear_from;
    property YearGraduated: Integer read FYear_graduated write FYear_graduated;
    property YearTo: Integer read FYear_to write FYear_to;
  end;

  TVkUniversities = class(TVkObject)
  private
    FChair: Integer;
    FChair_name: string;
    FCity: Integer;
    FCountry: Integer;
    FEducation_form: string;
    FEducation_status: string;
    FFaculty: Integer;
    FFaculty_name: string;
    FGraduation: Integer;
    FName: string;
  public
    property Chair: Integer read FChair write FChair;
    property ChairName: string read FChair_name write FChair_name;
    property City: Integer read FCity write FCity;
    property Country: Integer read FCountry write FCountry;
    property EducationForm: string read FEducation_form write FEducation_form;
    property EducationStatus: string read FEducation_status write FEducation_status;
    property Faculty: Integer read FFaculty write FFaculty;
    property FacultyName: string read FFaculty_name write FFaculty_name;
    property Graduation: Integer read FGraduation write FGraduation;
    property Name: string read FName write FName;
  end;

  TVkPersonal = class(TVkEntity)
  private
    FAlcohol: Integer;
    FInspired_by: string;
    FLangs: TArray<string>;
    FLife_main: Integer;
    FPeople_main: Integer;
    FPolitical: Integer;
    FReligion: string;
    FReligion_id: Integer;
    FSmoking: Integer;
  public
    property Alcohol: Integer read FAlcohol write FAlcohol;
    property InspiredBy: string read FInspired_by write FInspired_by;
    property Langs: TArray<string> read FLangs write FLangs;
    property LifeMain: Integer read FLife_main write FLife_main;
    property PeopleMain: Integer read FPeople_main write FPeople_main;
    property Political: Integer read FPolitical write FPolitical;
    property Religion: string read FReligion write FReligion;
    property ReligionId: Integer read FReligion_id write FReligion_id;
    property Smoking: Integer read FSmoking write FSmoking;
  end;

  TVkMilitary = class(TVkEntity)
  private
    FCountry_id: Integer;
    FFrom: Integer;
    FUnit: string;
    FUnit_id: Integer;
    FUntil: Int64;
  public
    property CountryId: Integer read FCountry_id write FCountry_id;
    property From: Integer read FFrom write FFrom;
    property&Unit: string read FUnit write FUnit;
    property UnitId: Integer read FUnit_id write FUnit_id;
    property UntilDate: Int64 read FUntil write FUntil;
  end;

  TVkCareer = class(TVkEntity)
  private
    FCity_id: Integer;
    FCompany: string;
    FCountry_id: Integer;
    FFrom: Integer;
    FPosition: string;
    FUntil: Int64;
  public
    property CityId: Integer read FCity_id write FCity_id;
    property Company: string read FCompany write FCompany;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property From: Integer read FFrom write FFrom;
    property Position: string read FPosition write FPosition;
    property UntilDate: Int64 read FUntil write FUntil;
  end;

  TVkOccupation = class(TVkObject)
  private
    FName: string;
    FType: string;
  public
    property Name: string read FName write FName;
    property TypeOcc: string read FType write FType;
  end;

  TVkCropPhoto = class(TVkEntity)
  private
    FCrop: TVkCrop;
    FPhoto: TVkPhoto;
    FRect: TVkRect;
  public
    /// <summary>
    /// Вырезанная фотография сообщества.
    /// </summary>
    property Crop: TVkCrop read FCrop write FCrop;
    /// <summary>
    /// Объект photo фотографии пользователя, из которой вырезается главное фото сообщества.
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Миниатюрная квадратная фотография, вырезанная из фотографии crop. Содержит набор полей, аналогичный объекту crop.
    /// </summary>
    property Rect: TVkRect read FRect write FRect;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkLastSeen = class(TVkEntity)
  private
    FPlatform: Integer;
    FTime: Int64;
  public
    property&Platform: Integer read FPlatform write FPlatform;
    property Time: Int64 read FTime write FTime;
  end;

  TVkUserOnlineInfo = class(TVkEntity)
  private
    FIs_mobile: Boolean;
    FIs_online: Boolean;
    FLast_seen: Int64;
    FVisible: Boolean;
  public
    property IsMobile: Boolean read FIs_mobile write FIs_mobile;
    property IsOnline: Boolean read FIs_online write FIs_online;
    property LastSeen: Int64 read FLast_seen write FLast_seen;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TVkProfile = class(TVkObject)
  private
    FAbout: string;
    FActivities: string;
    FBdate: string;
    FBlacklisted: Boolean;
    FBlacklisted_by_me: Boolean;
    FBooks: string;
    FCan_access_closed: Boolean;
    FCan_be_invited_group: Boolean;
    FCan_post: Boolean;
    FCan_see_all_posts: Boolean;
    FCan_see_audio: Boolean;
    FCan_send_friend_request: Boolean;
    FCan_write_private_message: Boolean;
    FCareer: TArray<TVkCareer>;
    FCommon_count: Integer;
    FCountry: TVkCountry;
    FCrop_photo: TVkCropPhoto;
    FDomain: string;
    FEducation_form: string;
    FEducation_status: string;
    FFacebook: string;
    FFacebook_name: string;
    FFaculty: Integer;
    FFaculty_name: string;
    FFirst_name: string;
    FFollowers_count: Integer;
    FFriend_status: Integer;
    FGames: string;
    FGraduation: Integer;
    FHas_mobile: Boolean;
    FHas_photo: Boolean;
    FHome_phone: string;
    FHome_town: string;
    FInstagram: string;
    FInterests: string;
    FIs_closed: Boolean;
    FIs_favorite: Boolean;
    FIs_friend: Boolean;
    FIs_hidden_from_feed: Boolean;
    FLast_name: string;
    FLast_seen: TVkLastSeen;
    FMilitary: TArray<TVkMilitary>;
    FMobile_phone: string;
    FMovies: string;
    FMusic: string;
    FNickname: string;
    FOccupation: TVkOccupation;
    FOnline: Boolean;
    FPersonal: TVkPersonal;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_200_orig: string;
    FPhoto_400_orig: string;
    FPhoto_50: string;
    FPhoto_id: string;
    FPhoto_max: string;
    FPhoto_max_orig: string;
    FQuotes: string;
    FRelation: Integer;
    FRelation_partner: TVkRelationPartner;
    FRelatives: TArray<TVkRelative>;
    FSchools: TArray<TVkSchoolInfo>;
    FScreen_name: string;
    FSex: Integer;
    FSite: string;
    FSkype: string;
    FStatus: string;
    FTimezone: Integer;
    FTv: string;
    FTwitter: string;
    FUniversities: TArray<TVkUniversities>;
    FUniversity: Integer;
    FUniversity_name: string;
    FVerified: Boolean;
    FTrending: Integer;
    FActivity: string;
    FCity: TVkCity;
    FMutual: TVkFriendsMutual;
    FFound_with: string;
    FType: string;
    FInvited_by: Boolean;
    FOnline_info: TVkUserOnlineInfo;
    FPhoto_medium_rec: string;
    FPhoto: string;
    FPhoto_big: string;
    FPhoto_medium: string;
    function GetRefer: string;
    function FGetFullName: string;
  public
    property&Type: string read FType write FType;
    property About: string read FAbout write FAbout;
    property Activities: string read FActivities write FActivities;
    property Activity: string read FActivity write FActivity;
    property BirthDate: string read FBdate write FBdate;
    property Blacklisted: Boolean read FBlacklisted write FBlacklisted;
    property BlacklistedByMe: Boolean read FBlacklisted_by_me write FBlacklisted_by_me;
    property Books: string read FBooks write FBooks;
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property CanBeInvitedGroup: Boolean read FCan_be_invited_group write FCan_be_invited_group;
    property CanPost: Boolean read FCan_post write FCan_post;
    property CanSeeAllPosts: Boolean read FCan_see_all_posts write FCan_see_all_posts;
    property CanSeeAudio: Boolean read FCan_see_audio write FCan_see_audio;
    property CanSendFriendRequest: Boolean read FCan_send_friend_request write FCan_send_friend_request;
    property CanWritePrivateMessage: Boolean read FCan_write_private_message write FCan_write_private_message;
    property Career: TArray<TVkCareer> read FCareer write FCareer;
    property City: TVkCity read FCity write FCity;
    property CommonCount: Integer read FCommon_count write FCommon_count;
    property Country: TVkCountry read FCountry write FCountry;
    property CropPhoto: TVkCropPhoto read FCrop_photo write FCrop_photo;
    property Domain: string read FDomain write FDomain;
    property EducationForm: string read FEducation_form write FEducation_form;
    property EducationStatus: string read FEducation_status write FEducation_status;
    property Facebook: string read FFacebook write FFacebook;
    property FacebookName: string read FFacebook_name write FFacebook_name;
    property Faculty: Integer read FFaculty write FFaculty;
    property FacultyName: string read FFaculty_name write FFaculty_name;
    property FirstName: string read FFirst_name write FFirst_name;
    property FollowersCount: Integer read FFollowers_count write FFollowers_count;
    property FoundWith: string read FFound_with write FFound_with;
    property FriendStatus: Integer read FFriend_status write FFriend_status;
    property Games: string read FGames write FGames;
    property Graduation: Integer read FGraduation write FGraduation;
    property HasMobile: Boolean read FHas_mobile write FHas_mobile;
    property HasPhoto: Boolean read FHas_photo write FHas_photo;
    property HomePhone: string read FHome_phone write FHome_phone;
    property HomeTown: string read FHome_town write FHome_town;
    property Instagram: string read FInstagram write FInstagram;
    property Interests: string read FInterests write FInterests;
    property InvitedBy: Boolean read FInvited_by write FInvited_by;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsFriend: Boolean read FIs_friend write FIs_friend;
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property LastName: string read FLast_name write FLast_name;
    property LastSeen: TVkLastSeen read FLast_seen write FLast_seen;
    property Military: TArray<TVkMilitary> read FMilitary write FMilitary;
    property MobilePhone: string read FMobile_phone write FMobile_phone;
    property Movies: string read FMovies write FMovies;
    property Music: string read FMusic write FMusic;
    property Mutual: TVkFriendsMutual read FMutual write FMutual;
    property NickName: string read FNickname write FNickname;
    property Occupation: TVkOccupation read FOccupation write FOccupation;
    property Online: Boolean read FOnline write FOnline;
    property OnlineInfo: TVkUserOnlineInfo read FOnline_info write FOnline_info;
    property Personal: TVkPersonal read FPersonal write FPersonal;
    property Photo: string read FPhoto write FPhoto;
    property PhotoBig: string read FPhoto_big write FPhoto_big;
    property PhotoMedium: string read FPhoto_medium write FPhoto_medium;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo200_Orig: string read FPhoto_200_orig write FPhoto_200_orig;
    property Photo400_Orig: string read FPhoto_400_orig write FPhoto_400_orig;
    property PhotoId: string read FPhoto_id write FPhoto_id;
    property PhotoMax: string read FPhoto_max write FPhoto_max;
    property PhotoMax_Orig: string read FPhoto_max_orig write FPhoto_max_orig;
    property PhotoMediumRec: string read FPhoto_medium_rec write FPhoto_medium_rec;
    property Quotes: string read FQuotes write FQuotes;
    property Relation: Integer read FRelation write FRelation;
    property RelationPartner: TVkRelationPartner read FRelation_partner write FRelation_partner;
    property Relatives: TArray<TVkRelative> read FRelatives write FRelatives;
    property Schools: TArray<TVkSchoolInfo> read FSchools write FSchools;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Sex: Integer read FSex write FSex;
    property Site: string read FSite write FSite;
    property Skype: string read FSkype write FSkype;
    property Status: string read FStatus write FStatus;
    property TimeZone: Integer read FTimezone write FTimezone;
    property Trending: Integer read FTrending write FTrending;
    property TV: string read FTv write FTv;
    property Twitter: string read FTwitter write FTwitter;
    property Universities: TArray<TVkUniversities> read FUniversities write FUniversities;
    property University: Integer read FUniversity write FUniversity;
    property UniversityName: string read FUniversity_name write FUniversity_name;
    property Verified: Boolean read FVerified write FVerified;
    //
    property Refer: string read GetRefer;
    property GetFullName: string read FGetFullName;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkProfiles = class(TVkEntity)
  private
    FItems: TArray<TVkProfile>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkProfile> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkProfiles);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkFriendsList = class(TVkObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TVkFriendsLists = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkFriendsList>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFriendsList> read FItems write FItems;
    destructor Destroy; override;
  end;

function FindUser(Id: Integer; List: TArray<TVkProfile>): Integer;

implementation

uses
  VK.CommonUtils;

function FindUser(Id: Integer; List: TArray<TVkProfile>): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(List) to High(List) do
    if List[i].Id = Id then
      Exit(i);
end;

{TVkCropPhoto}

constructor TVkCropPhoto.Create;
begin
  inherited;
  FPhoto := TVkPhoto.Create();
  FCrop := TVkCrop.Create();
  FRect := TVkRect.Create();
end;

destructor TVkCropPhoto.Destroy;
begin
  FPhoto.Free;
  FCrop.Free;
  FRect.Free;
  inherited;
end;

{TVkUser}

constructor TVkProfile.Create;
begin
  inherited;
  FCountry := TVkCountry.Create();
  FCity := TVkCity.Create();
  FLast_seen := TVkLastSeen.Create();
  FCrop_photo := TVkCropPhoto.Create();
  FOccupation := TVkOccupation.Create();
  FRelation_partner := TVkRelationPartner.Create();
  FPersonal := TVkPersonal.Create();
  FMutual := TVkFriendsMutual.Create();
  FOnline_info := TVkUserOnlineInfo.Create;
end;

destructor TVkProfile.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCareer>(FCareer);
  TArrayHelp.FreeArrayOfObject<TVkMilitary>(FMilitary);
  TArrayHelp.FreeArrayOfObject<TVkUniversities>(FUniversities);
  TArrayHelp.FreeArrayOfObject<TVkSchoolInfo>(FSchools);
  TArrayHelp.FreeArrayOfObject<TVkRelative>(FRelatives);
  FOnline_info.Free;
  FCountry.Free;
  FCity.Free;
  FLast_seen.Free;
  FCrop_photo.Free;
  FOccupation.Free;
  FRelation_partner.Free;
  FPersonal.Free;
  FMutual.Free;
  inherited;
end;

function TVkProfile.FGetFullName: string;
begin
  Result := FFirst_name + ' ' + FLast_name;
end;

function TVkProfile.GetRefer: string;
begin
  Result := '[' + Domain + '|' + FirstName + ']';
end;

{TVkProfiles}

procedure TVkProfiles.Append(Users: TVkProfiles);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkProfile));
end;

constructor TVkProfiles.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkProfiles.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<TVkProfile>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

procedure TVkProfiles.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkFriendInfos }

destructor TVkFriendInfos.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFriendInfo>(FItems);
  inherited;
end;

{ TVkFriendDeleteInfo }

function TVkFriendDeleteInfo.GetSuccess: Boolean;
begin
  Result := FSuccess = 1;
end;

procedure TVkFriendDeleteInfo.SetSuccess(const Value: Boolean);
begin
  FSuccess := BoolToInt(Value);
end;

{ TVkFriendsLists }

destructor TVkFriendsLists.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFriendsList>(FItems);
  inherited;
end;

{ TVkFriendsMutual }

destructor TVkFriendsMutual.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FUsers);
  inherited;
end;

end.

