unit VK.Entity.User;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common, VK.Entity.Photo;

type
  TVkUser = class;

  TVkFriendsMutual = class
  private
    FCount: Integer;
    FUsers: TArray<TVkUser>;
  public
    property Count: Integer read FCount write FCount;
    property Users: TArray<TVkUser> read FUsers write FUsers;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendsMutual;
  end;

  TVkFriendsOnline = class
  private
    FOnline: TArray<Integer>;
    FOnline_mobile: TArray<Integer>;
  public
    property Online: TArray<Integer> read FOnline write FOnline;
    property OnlineMobile: TArray<Integer> read FOnline_mobile write FOnline_mobile;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendsOnline;
  end;

  TVkFriendInfo = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendInfo;
  end;

  TVkFriendInfos = class
  private
    FItems: TArray<TVkFriendInfo>;
  public
    property Items: TArray<TVkFriendInfo> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendInfos;
  end;

  TVkFriendDeleteInfo = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendDeleteInfo;
  end;

  TVkRelative = class
  private
    FId: Integer;
    FType: string;
  public
    property Id: Integer read FId write FId;
    property TypeRelative: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelative;
  end;

  TVkSchool = class
  private
    FCity: Integer;
    FClass: string;
    FCountry: Integer;
    FId: string;
    FName: string;
    FSpeciality: string;
    FYear_from: Integer;
    FYear_graduated: Integer;
    FYear_to: Integer;
  public
    property City: Integer read FCity write FCity;
    property ClassNum: string read FClass write FClass;
    property Country: Integer read FCountry write FCountry;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Speciality: string read FSpeciality write FSpeciality;
    property YearFrom: Integer read FYear_from write FYear_from;
    property YearGraduated: Integer read FYear_graduated write FYear_graduated;
    property YearTo: Integer read FYear_to write FYear_to;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchool;
  end;

  TVkUniversities = class
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
    FId: Integer;
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
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUniversities;
  end;

  TVkPersonal = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPersonal;
  end;

  TVkMilitary = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMilitary;
  end;

  TVkCareer = class
  private
    FCity_id: Extended;
    FCompany: string;
    FCountry_id: Extended;
    FFrom: Extended;
    FPosition: string;
    FUntil: Extended;
  public
    property CityId: Extended read FCity_id write FCity_id;
    property Company: string read FCompany write FCompany;
    property CountryId: Extended read FCountry_id write FCountry_id;
    property From: Extended read FFrom write FFrom;
    property Position: string read FPosition write FPosition;
    property UntilDate: Extended read FUntil write FUntil;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCareer;
  end;

  TVkOccupation = class
  private
    FId: Extended;
    FName: string;
    FType: string;
  public
    property Id: Extended read FId write FId;
    property Name: string read FName write FName;
    property TypeOcc: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOccupation;
  end;

  TVkCropPhoto = class
  private
    FCrop: TVkCrop;
    FPhoto: TVkPhoto;
    FRect: TVkRect;
  public
    property Crop: TVkCrop read FCrop write FCrop;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Rect: TVkRect read FRect write FRect;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCropPhoto;
  end;

  TVkLastSeen = class
  private
    FPlatform: Extended;
    FTime: Extended;
  public
    property platform: Extended read FPlatform write FPlatform;
    property Time: Extended read FTime write FTime;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLastSeen;
  end;

  TVkUserOnlineInfo = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUserOnlineInfo;
  end;

  TVkUser = class
  private
    FAbout: string;
    FActivities: string;
    FBdate: string;
    FBlacklisted: Integer;
    FBlacklisted_by_me: Integer;
    FBooks: string;
    FCan_access_closed: Boolean;
    FCan_be_invited_group: Boolean;
    FCan_post: Integer;
    FCan_see_all_posts: Integer;
    FCan_see_audio: integer;
    FCan_send_friend_request: Integer;
    FCan_write_private_message: Integer;
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
    FHas_mobile: Integer;
    FHas_photo: Integer;
    FHome_phone: string;
    FHome_town: string;
    FId: Integer;
    FInstagram: string;
    FInterests: string;
    FIs_closed: Boolean;
    FIs_favorite: Integer;
    FIs_friend: Integer;
    FIs_hidden_from_feed: Integer;
    FLast_name: string;
    FLast_seen: TVkLastSeen;
    FMilitary: TArray<TVkMilitary>;
    FMobile_phone: string;
    FMovies: string;
    FMusic: string;
    FNickname: string;
    FOccupation: TVkOccupation;
    FOnline: Integer;
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
    FSchools: TArray<TVkSchool>;
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
    FVerified: Integer;
    FTrending: Integer;
    FActivity: string;
    FCity: TVkCity;
    FMutual: TVkFriendsMutual;
    FFound_with: string;
    FType: string;
    FInvited_by: Integer;
    FOnline_info: TVkUserOnlineInfo;
    FPhoto_medium_rec: string;
    FPhoto: string;
    function GetRefer: string;
    function FGetFullName: string;
  public
    property&Type: string read FType write FType;
    property About: string read FAbout write FAbout;
    property Activities: string read FActivities write FActivities;
    property Activity: string read FActivity write FActivity;
    property BirthDate: string read FBdate write FBdate;
    property Blacklisted: Integer read FBlacklisted write FBlacklisted;
    property BlacklistedByMe: Integer read FBlacklisted_by_me write FBlacklisted_by_me;
    property Books: string read FBooks write FBooks;
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property CanBeInvitedGroup: Boolean read FCan_be_invited_group write FCan_be_invited_group;
    property CanPost: Integer read FCan_post write FCan_post;
    property CanSeeAllPosts: Integer read FCan_see_all_posts write FCan_see_all_posts;
    property CanSeeAudio: integer read FCan_see_audio write FCan_see_audio;
    property CanSendFriendRequest: Integer read FCan_send_friend_request write FCan_send_friend_request;
    property CanWritePrivateMessage: Integer read FCan_write_private_message write FCan_write_private_message;
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
    property HasMobile: Integer read FHas_mobile write FHas_mobile;
    property HasPhoto: Integer read FHas_photo write FHas_photo;
    property HomePhone: string read FHome_phone write FHome_phone;
    property HomeTown: string read FHome_town write FHome_town;
    property Id: Integer read FId write FId;
    property Instagram: string read FInstagram write FInstagram;
    property Interests: string read FInterests write FInterests;
    property InvitedBy: Integer read FInvited_by write FInvited_by;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property IsFavorite: Integer read FIs_favorite write FIs_favorite;
    property IsFriend: Integer read FIs_friend write FIs_friend;
    property IsHiddenFromFeed: Integer read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property LastName: string read FLast_name write FLast_name;
    property LastSeen: TVkLastSeen read FLast_seen write FLast_seen;
    property Military: TArray<TVkMilitary> read FMilitary write FMilitary;
    property MobilePhone: string read FMobile_phone write FMobile_phone;
    property Movies: string read FMovies write FMovies;
    property Music: string read FMusic write FMusic;
    property Mutual: TVkFriendsMutual read FMutual write FMutual;
    property NickName: string read FNickname write FNickname;
    property Occupation: TVkOccupation read FOccupation write FOccupation;
    property Online: Integer read FOnline write FOnline;
    property OnlineInfo: TVkUserOnlineInfo read FOnline_info write FOnline_info;
    property Personal: TVkPersonal read FPersonal write FPersonal;
    property Photo: string read FPhoto write FPhoto;
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
    property Schools: TArray<TVkSchool> read FSchools write FSchools;
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
    property Verified: Integer read FVerified write FVerified;
    //
    property Refer: string read GetRefer;
    property GetFullName: string read FGetFullName;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUser;
  end;

  TVkUsers = class
  private
    FItems: TArray<TVkUser>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkUser> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkUsers);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUsers;
  end;

  TVkFriendsList = class
  private
    FId: Integer;
    FName: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendsList;
  end;

  TVkFriendsLists = class
  private
    FCount: Integer;
    FItems: TArray<TVkFriendsList>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFriendsList> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFriendsLists;
  end;

function FindUser(Id: Integer; List: TArray<TVkUser>): Integer;

implementation

uses
  VK.CommonUtils;

function FindUser(Id: Integer; List: TArray<TVkUser>): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(List) to High(List) do
    if List[i].Id = Id then
      Exit(i);
end;

{TVkRelative}

function TVkRelative.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRelative.FromJsonString(AJsonString: string): TVkRelative;
begin
  result := TJson.JsonToObject<TVkRelative>(AJsonString)
end;

{TVkSchool}

function TVkSchool.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSchool.FromJsonString(AJsonString: string): TVkSchool;
begin
  result := TJson.JsonToObject<TVkSchool>(AJsonString)
end;

{TVkUniversities}

function TVkUniversities.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUniversities.FromJsonString(AJsonString: string): TVkUniversities;
begin
  result := TJson.JsonToObject<TVkUniversities>(AJsonString)
end;

{TVkPersonal}

function TVkPersonal.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPersonal.FromJsonString(AJsonString: string): TVkPersonal;
begin
  result := TJson.JsonToObject<TVkPersonal>(AJsonString)
end;

{TVkMilitary}

function TVkMilitary.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMilitary.FromJsonString(AJsonString: string): TVkMilitary;
begin
  result := TJson.JsonToObject<TVkMilitary>(AJsonString)
end;

{TVkCareer}

function TVkCareer.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCareer.FromJsonString(AJsonString: string): TVkCareer;
begin
  result := TJson.JsonToObject<TVkCareer>(AJsonString)
end;

{TVkOccupation}

function TVkOccupation.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkOccupation.FromJsonString(AJsonString: string): TVkOccupation;
begin
  result := TJson.JsonToObject<TVkOccupation>(AJsonString)
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

function TVkCropPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCropPhoto.FromJsonString(AJsonString: string): TVkCropPhoto;
begin
  result := TJson.JsonToObject<TVkCropPhoto>(AJsonString)
end;

{TVkLastSeen}

function TVkLastSeen.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLastSeen.FromJsonString(AJsonString: string): TVkLastSeen;
begin
  result := TJson.JsonToObject<TVkLastSeen>(AJsonString)
end;

{TVkUser}

constructor TVkUser.Create;
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

destructor TVkUser.Destroy;
var
  LcareerItem: TVkCareer;
  LmilitaryItem: TVkMilitary;
  LuniversitiesItem: TVkUniversities;
  LschoolsItem: TVkSchool;
  LrelativesItem: TVkRelative;
begin

  for LcareerItem in FCareer do
    LcareerItem.Free;
  for LmilitaryItem in FMilitary do
    LmilitaryItem.Free;
  for LuniversitiesItem in FUniversities do
    LuniversitiesItem.Free;
  for LschoolsItem in FSchools do
    LschoolsItem.Free;
  for LrelativesItem in FRelatives do
    LrelativesItem.Free;

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

function TVkUser.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUser.FromJsonString(AJsonString: string): TVkUser;
begin
  result := TJson.JsonToObject<TVkUser>(AJsonString)
end;

function TVkUser.FGetFullName: string;
begin
  Result := FFirst_name + ' ' + FLast_name;
end;

function TVkUser.GetRefer: string;
begin
  Result := '[' + Domain + '|' + FirstName + ']';
end;

{TVkUsers}

procedure TVkUsers.Append(Users: TVkUsers);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkUser));
end;

constructor TVkUsers.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkUsers.Destroy;
var
  LItemsItem: TVkUser;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

function TVkUsers.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUsers.FromJsonString(AJsonString: string): TVkUsers;
begin
  result := TJson.JsonToObject<TVkUsers>(AJsonString);
end;

procedure TVkUsers.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkFriendInfo }

class function TVkFriendInfo.FromJsonString(AJsonString: string): TVkFriendInfo;
begin
  result := TJson.JsonToObject<TVkFriendInfo>(AJsonString);
end;

function TVkFriendInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendInfos }

destructor TVkFriendInfos.Destroy;
var
  LItemsItem: TVkFriendInfo;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;
  inherited;
end;

class function TVkFriendInfos.FromJsonString(AJsonString: string): TVkFriendInfos;
begin
  result := TJson.JsonToObject<TVkFriendInfos>(AJsonString);
end;

function TVkFriendInfos.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendDeleteInfo }

class function TVkFriendDeleteInfo.FromJsonString(AJsonString: string): TVkFriendDeleteInfo;
begin
  result := TJson.JsonToObject<TVkFriendDeleteInfo>(AJsonString);
end;

function TVkFriendDeleteInfo.GetSuccess: Boolean;
begin
  Result := FSuccess = 1;
end;

procedure TVkFriendDeleteInfo.SetSuccess(const Value: Boolean);
begin
  FSuccess := BoolToInt(Value);
end;

function TVkFriendDeleteInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendsList }

class function TVkFriendsList.FromJsonString(AJsonString: string): TVkFriendsList;
begin
  result := TJson.JsonToObject<TVkFriendsList>(AJsonString);
end;

function TVkFriendsList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendsLists }

destructor TVkFriendsLists.Destroy;
var
  Item: TVkFriendsList;
begin
  for Item in FItems do
    Item.Free;
  inherited;
end;

class function TVkFriendsLists.FromJsonString(AJsonString: string): TVkFriendsLists;
begin
  result := TJson.JsonToObject<TVkFriendsLists>(AJsonString);
end;

function TVkFriendsLists.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendsOnline }

class function TVkFriendsOnline.FromJsonString(AJsonString: string): TVkFriendsOnline;
begin
  result := TJson.JsonToObject<TVkFriendsOnline>(AJsonString);
end;

function TVkFriendsOnline.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFriendsMutual }

destructor TVkFriendsMutual.Destroy;
var
  Item: TVkUser;
begin
  for Item in FUsers do
    Item.Free;
  inherited;
end;

class function TVkFriendsMutual.FromJsonString(AJsonString: string): TVkFriendsMutual;
begin
  result := TJson.JsonToObject<TVkFriendsMutual>(AJsonString);
end;

function TVkFriendsMutual.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkUserOnlineInfo }

class function TVkUserOnlineInfo.FromJsonString(AJsonString: string): TVkUserOnlineInfo;
begin
  result := TJson.JsonToObject<TVkUserOnlineInfo>(AJsonString);
end;

function TVkUserOnlineInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

