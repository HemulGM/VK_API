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
    [JSONName('type')]
    FType: string;
  public
    property id: Extended read FId write FId;
    property type_Relative: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelative;
  end;

  TVkSchool = class
  private
    FCity: Extended;
    [JSONName('class')]
    FClass: string;
    FCountry: Extended;
    FId: string;
    FName: string;
    FSpeciality: string;
    FYear_from: Extended;
    FYear_graduated: Extended;
    FYear_to: Extended;
  public
    property city: Extended read FCity write FCity;
    property class_num: string read FClass write FClass;
    property country: Extended read FCountry write FCountry;
    property id: string read FId write FId;
    property name: string read FName write FName;
    property speciality: string read FSpeciality write FSpeciality;
    property year_from: Extended read FYear_from write FYear_from;
    property year_graduated: Extended read FYear_graduated write FYear_graduated;
    property year_to: Extended read FYear_to write FYear_to;
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
    property chair: Extended read FChair write FChair;
    property chair_name: string read FChair_name write FChair_name;
    property city: Extended read FCity write FCity;
    property country: Extended read FCountry write FCountry;
    property education_form: string read FEducation_form write FEducation_form;
    property education_status: string read FEducation_status write FEducation_status;
    property faculty: Extended read FFaculty write FFaculty;
    property faculty_name: string read FFaculty_name write FFaculty_name;
    property graduation: Extended read FGraduation write FGraduation;
    property id: Extended read FId write FId;
    property name: string read FName write FName;
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
    property alcohol: Extended read FAlcohol write FAlcohol;
    property inspired_by: string read FInspired_by write FInspired_by;
    property langs: TArray<string> read FLangs write FLangs;
    property life_main: Extended read FLife_main write FLife_main;
    property people_main: Extended read FPeople_main write FPeople_main;
    property political: Extended read FPolitical write FPolitical;
    property religion: string read FReligion write FReligion;
    property religion_id: Extended read FReligion_id write FReligion_id;
    property smoking: Extended read FSmoking write FSmoking;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPersonal;
  end;

  TVkMilitary = class
  private
    FCountry_id: Extended;
    FFrom: Extended;
    [JSONName('unit')]
    FUnit: string;
    FUnit_id: Extended;
    [JSONName('until')]
    FUntil: Extended;
  public
    property country_id: Extended read FCountry_id write FCountry_id;
    property from: Extended read FFrom write FFrom;
    property unit_name: string read FUnit write FUnit;
    property unit_id: Extended read FUnit_id write FUnit_id;
    property until_date: Extended read FUntil write FUntil;
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
    [JSONName('until')]
    FUntil: Extended;
  public
    property city_id: Extended read FCity_id write FCity_id;
    property company: string read FCompany write FCompany;
    property country_id: Extended read FCountry_id write FCountry_id;
    property from: Extended read FFrom write FFrom;
    property position: string read FPosition write FPosition;
    property until_date: Extended read FUntil write FUntil;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCareer;
  end;

  TVkOccupation = class
  private
    FId: Extended;
    FName: string;
    [JSONName('type')]
    FType: string;
  public
    property id: Extended read FId write FId;
    property name: string read FName write FName;
    property type_occ: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOccupation;
  end;

  TVkCropPhoto = class
  private
    FCrop: TVkCrop;
    FPhoto: TVkPhoto;
    FRect: TVkRect;
  public
    property crop: TVkCrop read FCrop write FCrop;
    property photo: TVkPhoto read FPhoto write FPhoto;
    property rect: TVkRect read FRect write FRect;
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
    property time: Extended read FTime write FTime;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLastSeen;
  end;

  TVkCountry = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCountry;
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
    FCan_see_audio: Extended;
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
    FId: Extended;
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
  public
    property about: string read FAbout write FAbout;
    property activities: string read FActivities write FActivities;
    property bdate: string read FBdate write FBdate;
    property blacklisted: Extended read FBlacklisted write FBlacklisted;
    property blacklisted_by_me: Extended read FBlacklisted_by_me write FBlacklisted_by_me;
    property books: string read FBooks write FBooks;
    property can_access_closed: Boolean read FCan_access_closed write FCan_access_closed;
    property can_be_invited_group: Boolean read FCan_be_invited_group write FCan_be_invited_group;
    property can_post: Extended read FCan_post write FCan_post;
    property can_see_all_posts: Extended read FCan_see_all_posts write FCan_see_all_posts;
    property can_see_audio: Extended read FCan_see_audio write FCan_see_audio;
    property can_send_friend_request: Extended read FCan_send_friend_request write FCan_send_friend_request;
    property can_write_private_message: Extended read FCan_write_private_message write FCan_write_private_message;
    property career: TArray<TVkCareer> read FCareer write FCareer;
    property common_count: Extended read FCommon_count write FCommon_count;
    property country: TVkCountry read FCountry write FCountry;
    property crop_photo: TVkCropPhoto read FCrop_photo write FCrop_photo;
    property domain: string read FDomain write FDomain;
    property education_form: string read FEducation_form write FEducation_form;
    property education_status: string read FEducation_status write FEducation_status;
    property facebook: string read FFacebook write FFacebook;
    property facebook_name: string read FFacebook_name write FFacebook_name;
    property faculty: Extended read FFaculty write FFaculty;
    property faculty_name: string read FFaculty_name write FFaculty_name;
    property first_name: string read FFirst_name write FFirst_name;
    property followers_count: Extended read FFollowers_count write FFollowers_count;
    property friend_status: Extended read FFriend_status write FFriend_status;
    property games: string read FGames write FGames;
    property graduation: Extended read FGraduation write FGraduation;
    property has_mobile: Extended read FHas_mobile write FHas_mobile;
    property has_photo: Extended read FHas_photo write FHas_photo;
    property home_phone: string read FHome_phone write FHome_phone;
    property home_town: string read FHome_town write FHome_town;
    property id: Extended read FId write FId;
    property instagram: string read FInstagram write FInstagram;
    property interests: string read FInterests write FInterests;
    property is_closed: Boolean read FIs_closed write FIs_closed;
    property is_favorite: Extended read FIs_favorite write FIs_favorite;
    property is_friend: Extended read FIs_friend write FIs_friend;
    property is_hidden_from_feed: Extended read FIs_hidden_from_feed write FIs_hidden_from_feed;
    property last_name: string read FLast_name write FLast_name;
    property last_seen: TVkLastSeen read FLast_seen write FLast_seen;
    property military: TArray<TVkMilitary> read FMilitary write FMilitary;
    property mobile_phone: string read FMobile_phone write FMobile_phone;
    property movies: string read FMovies write FMovies;
    property music: string read FMusic write FMusic;
    property nickname: string read FNickname write FNickname;
    property occupation: TVkOccupation read FOccupation write FOccupation;
    property online: Extended read FOnline write FOnline;
    property personal: TVkPersonal read FPersonal write FPersonal;
    property photo_100: string read FPhoto_100 write FPhoto_100;
    property photo_200: string read FPhoto_200 write FPhoto_200;
    property photo_200_orig: string read FPhoto_200_orig write FPhoto_200_orig;
    property photo_400_orig: string read FPhoto_400_orig write FPhoto_400_orig;
    property photo_50: string read FPhoto_50 write FPhoto_50;
    property photo_id: string read FPhoto_id write FPhoto_id;
    property photo_max: string read FPhoto_max write FPhoto_max;
    property photo_max_orig: string read FPhoto_max_orig write FPhoto_max_orig;
    property quotes: string read FQuotes write FQuotes;
    property relation: Extended read FRelation write FRelation;
    property relation_partner: TVkRelationPartner read FRelation_partner write FRelation_partner;
    property relatives: TArray<TVkRelative> read FRelatives write FRelatives;
    property schools: TArray<TVkSchool> read FSchools write FSchools;
    property screen_name: string read FScreen_name write FScreen_name;
    property sex: Extended read FSex write FSex;
    property site: string read FSite write FSite;
    property skype: string read FSkype write FSkype;
    property status: string read FStatus write FStatus;
    property timezone: Extended read FTimezone write FTimezone;
    property tv: string read FTv write FTv;
    property twitter: string read FTwitter write FTwitter;
    property universities: TArray<TVkUniversities> read FUniversities write FUniversities;
    property university: Extended read FUniversity write FUniversity;
    property university_name: string read FUniversity_name write FUniversity_name;
    property verified: Extended read FVerified write FVerified;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUser;
  end;

  TVkUsers = class
  private
    FResponse: TArray<TVkUser>;
  public
    property response: TArray<TVkUser> read FResponse write FResponse;
    property Items: TArray<TVkUser> read FResponse write FResponse;
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

{TVkCountry}

function TVkCountry.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountry.FromJsonString(AJsonString: string): TVkCountry;
begin
  result := TJson.JsonToObject<TVkCountry>(AJsonString)
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

{TVkUsers}

destructor TVkUsers.Destroy;
var
  LItemsItem: TVkUser;
begin
  for LItemsItem in FResponse do
    LItemsItem.Free;

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

end.

