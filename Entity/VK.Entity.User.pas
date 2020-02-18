unit VK.Entity.User;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common, VK.Entity.Photo;

const
  UserFieldsAll = 'photo_id, verified, sex, bdate, city, country, home_town, has_photo, ' +
    'photo_50, photo_100, photo_200_orig, photo_200, photo_400_orig, photo_max, ' +
    'photo_max_orig, online, domain, has_mobile, contacts, site, education, ' +
    'universities, schools, status, last_seen, followers_count, common_count, ' +
    'occupation, nickname, relatives, relation, personal, connections, exports, ' +
    'activities, interests, music, movies, tv, books, games, about, quotes, ' +
    'can_post, can_see_all_posts, can_see_audio, can_write_private_message, ' +
    'can_send_friend_request, is_favorite, is_hidden_from_feed, timezone, ' +
    'screen_name, maiden_name, crop_photo, is_friend, friend_status, career, ' +
    'military, blacklisted, blacklisted_by_me, can_be_invited_group';

type
  TVkRelative = class
  private
    FId: Extended;
    FType: string;
  public
    property Id: Extended read FId write FId;
    property TypeRelative: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelative;
  end;

  TVkSchool = class
  private
    FCity: Extended;
    FClass: string;
    FCountry: Extended;
    FId: string;
    FName: string;
    FSpeciality: string;
    FYear_from: Extended;
    FYear_graduated: Extended;
    FYear_to: Extended;
  public
    property City: Extended read FCity write FCity;
    property ClassNum: string read FClass write FClass;
    property Country: Extended read FCountry write FCountry;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Speciality: string read FSpeciality write FSpeciality;
    property YearFrom: Extended read FYear_from write FYear_from;
    property YearGraduated: Extended read FYear_graduated write FYear_graduated;
    property YearTo: Extended read FYear_to write FYear_to;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchool;
  end;

  TVkUniversities = class
  private
    FChair: Extended;
    FChair_name: string;
    FCity: Extended;
    FCountry: Extended;
    FEducation_form: string;
    FEducation_status: string;
    FFaculty: Extended;
    FFaculty_name: string;
    FGraduation: Extended;
    FId: Extended;
    FName: string;
  public
    property Chair: Extended read FChair write FChair;
    property ChairName: string read FChair_name write FChair_name;
    property City: Extended read FCity write FCity;
    property Country: Extended read FCountry write FCountry;
    property EducationForm: string read FEducation_form write FEducation_form;
    property EducationStatus: string read FEducation_status write FEducation_status;
    property Faculty: Extended read FFaculty write FFaculty;
    property FacultyName: string read FFaculty_name write FFaculty_name;
    property Graduation: Extended read FGraduation write FGraduation;
    property Id: Extended read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUniversities;
  end;

  TVkPersonal = class
  private
    FAlcohol: Extended;
    FInspired_by: string;
    FLangs: TArray<string>;
    FLife_main: Extended;
    FPeople_main: Extended;
    FPolitical: Extended;
    FReligion: string;
    FReligion_id: Extended;
    FSmoking: Extended;
  public
    property Alcohol: Extended read FAlcohol write FAlcohol;
    property InspiredBy: string read FInspired_by write FInspired_by;
    property Langs: TArray<string> read FLangs write FLangs;
    property LifeMain: Extended read FLife_main write FLife_main;
    property PeopleMain: Extended read FPeople_main write FPeople_main;
    property Political: Extended read FPolitical write FPolitical;
    property Religion: string read FReligion write FReligion;
    property ReligionId: Extended read FReligion_id write FReligion_id;
    property Smoking: Extended read FSmoking write FSmoking;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPersonal;
  end;

  TVkMilitary = class
  private
    FCountry_id: Extended;
    FFrom: Extended;
    FUnit: string;
    FUnit_id: Extended;
    FUntil: Extended;
  public
    property CountryId: Extended read FCountry_id write FCountry_id;
    property From: Extended read FFrom write FFrom;
    property&Unit: string read FUnit write FUnit;
    property UnitId: Extended read FUnit_id write FUnit_id;
    property UntilDate: Extended read FUntil write FUntil;
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

  TVkUser = class
  private
    FAbout: string;
    FActivities: string;
    FBdate: string;
    FBlacklisted: Extended;
    FBlacklisted_by_me: Extended;
    FBooks: string;
    FCan_access_closed: Boolean;
    FCan_be_invited_group: Boolean;
    FCan_post: Extended;
    FCan_see_all_posts: Extended;
    FCan_see_audio: integer;
    FCan_send_friend_request: Extended;
    FCan_write_private_message: Extended;
    FCareer: TArray<TVkCareer>;
    FCommon_count: Extended;
    FCountry: TVkCountry;
    FCrop_photo: TVkCropPhoto;
    FDomain: string;
    FEducation_form: string;
    FEducation_status: string;
    FFacebook: string;
    FFacebook_name: string;
    FFaculty: Extended;
    FFaculty_name: string;
    FFirst_name: string;
    FFollowers_count: Extended;
    FFriend_status: Extended;
    FGames: string;
    FGraduation: Extended;
    FHas_mobile: Extended;
    FHas_photo: Extended;
    FHome_phone: string;
    FHome_town: string;
    FId: Integer;
    FInstagram: string;
    FInterests: string;
    FIs_closed: Boolean;
    FIs_favorite: Extended;
    FIs_friend: Extended;
    FIs_hidden_from_feed: Extended;
    FLast_name: string;
    FLast_seen: TVkLastSeen;
    FMilitary: TArray<TVkMilitary>;
    FMobile_phone: string;
    FMovies: string;
    FMusic: string;
    FNickname: string;
    FOccupation: TVkOccupation;
    FOnline: Extended;
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
    FRelation: Extended;
    FRelation_partner: TVkRelationPartner;
    FRelatives: TArray<TVkRelative>;
    FSchools: TArray<TVkSchool>;
    FScreen_name: string;
    FSex: Extended;
    FSite: string;
    FSkype: string;
    FStatus: string;
    FTimezone: Extended;
    FTv: string;
    FTwitter: string;
    FUniversities: TArray<TVkUniversities>;
    FUniversity: Extended;
    FUniversity_name: string;
    FVerified: Extended;
    function GetRefer: string;
    function FGetFullName: string;
  public
    property About: string read FAbout write FAbout;
    property Activities: string read FActivities write FActivities;
    property BirthDate: string read FBdate write FBdate;
    property Blacklisted: Extended read FBlacklisted write FBlacklisted;
    property BlacklistedByMe: Extended read FBlacklisted_by_me write FBlacklisted_by_me;
    property Books: string read FBooks write FBooks;
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property CanBeInvitedGroup: Boolean read FCan_be_invited_group write FCan_be_invited_group;
    property CanPost: Extended read FCan_post write FCan_post;
    property CanSeeAllPosts: Extended read FCan_see_all_posts write FCan_see_all_posts;
    property CanSeeAudio: integer read FCan_see_audio write FCan_see_audio;
    property CanSendFriendRequest: Extended read FCan_send_friend_request write FCan_send_friend_request;
    property CanWritePrivateMessage: Extended read FCan_write_private_message write FCan_write_private_message;
    property Career: TArray<TVkCareer> read FCareer write FCareer;
    property CommonCount: Extended read FCommon_count write FCommon_count;
    property Country: TVkCountry read FCountry write FCountry;
    property CropPhoto: TVkCropPhoto read FCrop_photo write FCrop_photo;
    property Domain: string read FDomain write FDomain;
    property EducationForm: string read FEducation_form write FEducation_form;
    property EducationStatus: string read FEducation_status write FEducation_status;
    property Facebook: string read FFacebook write FFacebook;
    property FacebookName: string read FFacebook_name write FFacebook_name;
    property Faculty: Extended read FFaculty write FFaculty;
    property FacultyName: string read FFaculty_name write FFaculty_name;
    property FirstName: string read FFirst_name write FFirst_name;
    property FollowersCount: Extended read FFollowers_count write FFollowers_count;
    property FriendStatus: Extended read FFriend_status write FFriend_status;
    property Games: string read FGames write FGames;
    property Graduation: Extended read FGraduation write FGraduation;
    property HasMobile: Extended read FHas_mobile write FHas_mobile;
    property HasPhoto: Extended read FHas_photo write FHas_photo;
    property HomePhone: string read FHome_phone write FHome_phone;
    property HomeTown: string read FHome_town write FHome_town;
    property Id: Integer read FId write FId;
    property Instagram: string read FInstagram write FInstagram;
    property Interests: string read FInterests write FInterests;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property IsFavorite: Extended read FIs_favorite write FIs_favorite;
    property IsFriend: Extended read FIs_friend write FIs_friend;
    property IsHiddenFromFeed: Extended read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property LastName: string read FLast_name write FLast_name;
    property LastSeen: TVkLastSeen read FLast_seen write FLast_seen;
    property Military: TArray<TVkMilitary> read FMilitary write FMilitary;
    property MobilePhone: string read FMobile_phone write FMobile_phone;
    property Movies: string read FMovies write FMovies;
    property Music: string read FMusic write FMusic;
    property NickName: string read FNickname write FNickname;
    property Occupation: TVkOccupation read FOccupation write FOccupation;
    property Online: Extended read FOnline write FOnline;
    property Personal: TVkPersonal read FPersonal write FPersonal;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo200_Orig: string read FPhoto_200_orig write FPhoto_200_orig;
    property Photo400_Orig: string read FPhoto_400_orig write FPhoto_400_orig;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property PhotoId: string read FPhoto_id write FPhoto_id;
    property PhotoMax: string read FPhoto_max write FPhoto_max;
    property PhotoMax_Orig: string read FPhoto_max_orig write FPhoto_max_orig;
    property Quotes: string read FQuotes write FQuotes;
    property Relation: Extended read FRelation write FRelation;
    property RelationPartner: TVkRelationPartner read FRelation_partner write FRelation_partner;
    property Relatives: TArray<TVkRelative> read FRelatives write FRelatives;
    property Schools: TArray<TVkSchool> read FSchools write FSchools;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Sex: Extended read FSex write FSex;
    property Site: string read FSite write FSite;
    property Skype: string read FSkype write FSkype;
    property Status: string read FStatus write FStatus;
    property TimeZone: Extended read FTimezone write FTimezone;
    property TV: string read FTv write FTv;
    property Twitter: string read FTwitter write FTwitter;
    property Universities: TArray<TVkUniversities> read FUniversities write FUniversities;
    property University: Extended read FUniversity write FUniversity;
    property UniversityName: string read FUniversity_name write FUniversity_name;
    property Verified: Extended read FVerified write FVerified;
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

implementation

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
  FLast_seen := TVkLastSeen.Create();
  FCrop_photo := TVkCropPhoto.Create();
  FOccupation := TVkOccupation.Create();
  FRelation_partner := TVkRelationPartner.Create();
  FPersonal := TVkPersonal.Create();
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

  FCountry.Free;
  FLast_seen.Free;
  FCrop_photo.Free;
  FOccupation.Free;
  FRelation_partner.Free;
  FPersonal.Free;
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

end.

