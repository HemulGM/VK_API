unit VK.Users.Types;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types;

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
  TRelativesClass = class
  private
    FId: Extended;
    [JSONName('type')]
    FType: string;
  public
    property id: Extended read FId write FId;
    property type_Relative: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRelativesClass;
  end;

  TSchoolsClass = class
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
    class function FromJsonString(AJsonString: string): TSchoolsClass;
  end;

  TUniversitiesClass = class
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
    class function FromJsonString(AJsonString: string): TUniversitiesClass;
  end;

  TPersonalClass = class
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
    class function FromJsonString(AJsonString: string): TPersonalClass;
  end;

  TRelation_partnerClass = class
  private
    FFirst_name: string;
    FId: Extended;
    FLast_name: string;
  public
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property last_name: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRelation_partnerClass;
  end;

  TMilitaryClass = class
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
    class function FromJsonString(AJsonString: string): TMilitaryClass;
  end;

  TCareerClass = class
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
    class function FromJsonString(AJsonString: string): TCareerClass;
  end;

  TOccupationClass = class
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
    class function FromJsonString(AJsonString: string): TOccupationClass;
  end;

  TRectClass = class
  private
    FX: Extended;
    FX2: Extended;
    FY: Extended;
    FY2: Extended;
  public
    property x: Extended read FX write FX;
    property x2: Extended read FX2 write FX2;
    property y: Extended read FY write FY;
    property y2: Extended read FY2 write FY2;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRectClass;
  end;

  TCropClass = class
  private
    FX: Extended;
    FX2: Extended;
    FY: Extended;
    FY2: Extended;
  public
    property x: Extended read FX write FX;
    property x2: Extended read FX2 write FX2;
    property y: Extended read FY write FY;
    property y2: Extended read FY2 write FY2;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCropClass;
  end;

  TSizesClass = class
  private
    FHeight: Extended;
    [JSONName('type')]
    FType: string;
    FUrl: string;
    FWidth: Extended;
  public
    property height: Extended read FHeight write FHeight;
    property type_sz: string read FType write FType;
    property url: string read FUrl write FUrl;
    property width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TSizesClass;
  end;

  TPhotoClass = class
  private
    FAlbum_id: Extended;
    FDate: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FPost_id: Extended;
    FSizes: TArray<TSizesClass>;
    FText: string;
  public
    property album_id: Extended read FAlbum_id write FAlbum_id;
    property date: Extended read FDate write FDate;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_id: Extended read FPost_id write FPost_id;
    property sizes: TArray<TSizesClass> read FSizes write FSizes;
    property text: string read FText write FText;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TPhotoClass;
  end;

  TCrop_photoClass = class
  private
    FCrop: TCropClass;
    FPhoto: TPhotoClass;
    FRect: TRectClass;
  public
    property crop: TCropClass read FCrop write FCrop;
    property photo: TPhotoClass read FPhoto write FPhoto;
    property rect: TRectClass read FRect write FRect;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCrop_photoClass;
  end;

  TLast_seenClass = class
  private
    FPlatform: Extended;
    FTime: Extended;
  public
    property platform: Extended read FPlatform write FPlatform;
    property time: Extended read FTime write FTime;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TLast_seenClass;
  end;

  TCountryClass = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCountryClass;
  end;

  TUserClass = class
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
    FCareer: TArray<TCareerClass>;
    FCommon_count: Extended;
    FCountry: TCountryClass;
    FCrop_photo: TCrop_photoClass;
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
    FLast_seen: TLast_seenClass;
    FMilitary: TArray<TMilitaryClass>;
    FMobile_phone: string;
    FMovies: string;
    FMusic: string;
    FNickname: string;
    FOccupation: TOccupationClass;
    FOnline: Extended;
    FPersonal: TPersonalClass;
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
    FRelation_partner: TRelation_partnerClass;
    FRelatives: TArray<TRelativesClass>;
    FSchools: TArray<TSchoolsClass>;
    FScreen_name: string;
    FSex: Extended;
    FSite: string;
    FSkype: string;
    FStatus: string;
    FTimezone: Extended;
    FTv: string;
    FTwitter: string;
    FUniversities: TArray<TUniversitiesClass>;
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
    property career: TArray<TCareerClass> read FCareer write FCareer;
    property common_count: Extended read FCommon_count write FCommon_count;
    property country: TCountryClass read FCountry write FCountry;
    property crop_photo: TCrop_photoClass read FCrop_photo write FCrop_photo;
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
    property last_seen: TLast_seenClass read FLast_seen write FLast_seen;
    property military: TArray<TMilitaryClass> read FMilitary write FMilitary;
    property mobile_phone: string read FMobile_phone write FMobile_phone;
    property movies: string read FMovies write FMovies;
    property music: string read FMusic write FMusic;
    property nickname: string read FNickname write FNickname;
    property occupation: TOccupationClass read FOccupation write FOccupation;
    property online: Extended read FOnline write FOnline;
    property personal: TPersonalClass read FPersonal write FPersonal;
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
    property relation_partner: TRelation_partnerClass read FRelation_partner write FRelation_partner;
    property relatives: TArray<TRelativesClass> read FRelatives write FRelatives;
    property schools: TArray<TSchoolsClass> read FSchools write FSchools;
    property screen_name: string read FScreen_name write FScreen_name;
    property sex: Extended read FSex write FSex;
    property site: string read FSite write FSite;
    property skype: string read FSkype write FSkype;
    property status: string read FStatus write FStatus;
    property timezone: Extended read FTimezone write FTimezone;
    property tv: string read FTv write FTv;
    property twitter: string read FTwitter write FTwitter;
    property universities: TArray<TUniversitiesClass> read FUniversities write FUniversities;
    property university: Extended read FUniversity write FUniversity;
    property university_name: string read FUniversity_name write FUniversity_name;
    property verified: Extended read FVerified write FVerified;

    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TUserClass;
  end;

  TUsersClass = class
  private
    FResponse: TArray<TUserClass>;
  public
    property response: TArray<TUserClass> read FResponse write FResponse;
    property Items: TArray<TUserClass> read FResponse write FResponse;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TUsersClass;
  end;

implementation

{TRelativesClass}

function TRelativesClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TRelativesClass.FromJsonString(AJsonString: string): TRelativesClass;
begin
  result := TJson.JsonToObject<TRelativesClass>(AJsonString)
end;

{TSchoolsClass}

function TSchoolsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TSchoolsClass.FromJsonString(AJsonString: string): TSchoolsClass;
begin
  result := TJson.JsonToObject<TSchoolsClass>(AJsonString)
end;

{TUniversitiesClass}

function TUniversitiesClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TUniversitiesClass.FromJsonString(AJsonString: string): TUniversitiesClass;
begin
  result := TJson.JsonToObject<TUniversitiesClass>(AJsonString)
end;

{TPersonalClass}

function TPersonalClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TPersonalClass.FromJsonString(AJsonString: string): TPersonalClass;
begin
  result := TJson.JsonToObject<TPersonalClass>(AJsonString)
end;

{TRelation_partnerClass}

function TRelation_partnerClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TRelation_partnerClass.FromJsonString(AJsonString: string): TRelation_partnerClass;
begin
  result := TJson.JsonToObject<TRelation_partnerClass>(AJsonString)
end;

{TMilitaryClass}

function TMilitaryClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TMilitaryClass.FromJsonString(AJsonString: string): TMilitaryClass;
begin
  result := TJson.JsonToObject<TMilitaryClass>(AJsonString)
end;

{TCareerClass}

function TCareerClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCareerClass.FromJsonString(AJsonString: string): TCareerClass;
begin
  result := TJson.JsonToObject<TCareerClass>(AJsonString)
end;

{TOccupationClass}

function TOccupationClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TOccupationClass.FromJsonString(AJsonString: string): TOccupationClass;
begin
  result := TJson.JsonToObject<TOccupationClass>(AJsonString)
end;

{TRectClass}

function TRectClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TRectClass.FromJsonString(AJsonString: string): TRectClass;
begin
  result := TJson.JsonToObject<TRectClass>(AJsonString)
end;

{TCropClass}

function TCropClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCropClass.FromJsonString(AJsonString: string): TCropClass;
begin
  result := TJson.JsonToObject<TCropClass>(AJsonString)
end;

{TSizesClass}

function TSizesClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TSizesClass.FromJsonString(AJsonString: string): TSizesClass;
begin
  result := TJson.JsonToObject<TSizesClass>(AJsonString)
end;

{TPhotoClass}

destructor TPhotoClass.Destroy;
var
  LsizesItem: TSizesClass;
begin

  for LsizesItem in FSizes do
    LsizesItem.Free;

  inherited;
end;

function TPhotoClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TPhotoClass.FromJsonString(AJsonString: string): TPhotoClass;
begin
  result := TJson.JsonToObject<TPhotoClass>(AJsonString)
end;

{TCrop_photoClass}

constructor TCrop_photoClass.Create;
begin
  inherited;
  FPhoto := TPhotoClass.Create();
  FCrop := TCropClass.Create();
  FRect := TRectClass.Create();
end;

destructor TCrop_photoClass.Destroy;
begin
  FPhoto.Free;
  FCrop.Free;
  FRect.Free;
  inherited;
end;

function TCrop_photoClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCrop_photoClass.FromJsonString(AJsonString: string): TCrop_photoClass;
begin
  result := TJson.JsonToObject<TCrop_photoClass>(AJsonString)
end;

{TLast_seenClass}

function TLast_seenClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TLast_seenClass.FromJsonString(AJsonString: string): TLast_seenClass;
begin
  result := TJson.JsonToObject<TLast_seenClass>(AJsonString)
end;

{TCountryClass}

function TCountryClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCountryClass.FromJsonString(AJsonString: string): TCountryClass;
begin
  result := TJson.JsonToObject<TCountryClass>(AJsonString)
end;

{TItemClass}

constructor TUserClass.Create;
begin
  inherited;
  FCountry := TCountryClass.Create();
  FLast_seen := TLast_seenClass.Create();
  FCrop_photo := TCrop_photoClass.Create();
  FOccupation := TOccupationClass.Create();
  FRelation_partner := TRelation_partnerClass.Create();
  FPersonal := TPersonalClass.Create();
end;

destructor TUserClass.Destroy;
var
  LcareerItem: TCareerClass;
  LmilitaryItem: TMilitaryClass;
  LuniversitiesItem: TUniversitiesClass;
  LschoolsItem: TSchoolsClass;
  LrelativesItem: TRelativesClass;
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

function TUserClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TUserClass.FromJsonString(AJsonString: string): TUserClass;
begin
  result := TJson.JsonToObject<TUserClass>(AJsonString)
end;

{TUsers}

destructor TUsersClass.Destroy;
var
  LItemsItem: TUserClass;
begin
  for LItemsItem in FResponse do
    LItemsItem.Free;

  inherited;
end;

function TUsersClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TUsersClass.FromJsonString(AJsonString: string): TUsersClass;
begin
  result := TJson.JsonToObject<TUsersClass>(AJsonString);
end;

end.

