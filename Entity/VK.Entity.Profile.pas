unit VK.Entity.Profile;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  REST.Json.Types, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Database.Cities,
  VK.Entity.Database.Countries, VK.Types, VK.Entity.Counters,
  VK.Wrap.Interceptors, VK.Entity.Audio, VK.Entity.Common.List;

type
  TVkProfile = class;

  TVkLangFull = class(TVkEntity)
  private
    FId: Integer;
    FNative_name: string;
  public
    property Id: Integer read FId write FId;
    property NativeName: string read FNative_name write FNative_name;
  end;

  TVkExport = class(TVkEntity)
  private
    FInstagram: Boolean;
    FLivejournal: Boolean;
    FFacebook: Boolean;
    FTwitter: Boolean;
  public
    property Twitter: Boolean read FTwitter write FTwitter;
    property Facebook: Boolean read FFacebook write FTwitter;
    property Livejournal: Boolean read FLivejournal write FTwitter;
    property Instagram: Boolean read FInstagram write FTwitter;
  end;

  TVkFriendsMutual = class(TVkCounterEntity)
  private
    FUsers: TArray<TVkProfile>;
  public
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

  TVkFriendInfos = TVkEntityList<TVkFriendInfo>;

  TVkFriendDeleteInfo = class(TVkEntity)
  private
    FSuccess: Boolean;
    FOut_request_deleted: Boolean;
    FIn_request_deleted: Boolean;
    FSuggestion_deleted: Boolean;
    FFriend_deleted: Boolean;
  public
    property Success: Boolean read FSuccess write FSuccess;
    property FriendDeleted: Boolean read FFriend_deleted write FFriend_deleted;
    property OutRequestDeleted: Boolean read FOut_request_deleted write FOut_request_deleted;
    property InRequestDeleted: Boolean read FIn_request_deleted write FIn_request_deleted;
    property SuggestionDeleted: Boolean read FSuggestion_deleted write FSuggestion_deleted;
  end;

  TVkRelative = class(TVkBasicObject)
  private
    FType: string;
  public
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    property Id;
    /// <summary>
    /// Имя родственника (если родственник не является пользователем ВКонтакте, то предыдущее значение id возвращено не будет)
    /// </summary>
    property Name;
    /// <summary>
    /// Тип родственной связи.
    ///  Возможные значения:
    ///  child — сын/дочь;
    ///  sibling — брат/сестра;
    ///  parent — отец/мать;
    ///  grandparent — дедушка/бабушка;
    ///  grandchild — внук/внучка
    /// </summary>
    property TypeRelative: string read FType write FType;
  end;

  TVkSchoolInfo = class(TVkBasicObject)
  private
    FCity: Integer;
    FClass: string;
    FCountry: Integer;
    FSpeciality: string;
    FYear_from: Integer;
    FYear_graduated: Integer;
    FYear_to: Integer;
    FType: Integer;
    FType_str: string;
  public
    /// <summary>
    /// Идентификатор школы
    /// </summary>
    property Id;
    /// <summary>
    /// Наименование школы
    /// </summary>
    property Name;
    /// <summary>
    /// Идентификатор города, в котором расположена школа
    /// </summary>
    property City: Integer read FCity write FCity;
    /// <summary>
    /// Буква класса
    /// </summary>
    property &Class: string read FClass write FClass;
    /// <summary>
    /// Идентификатор страны, в которой расположена школа
    /// </summary>
    property Country: Integer read FCountry write FCountry;
    /// <summary>
    /// Специализация
    /// </summary>
    property Speciality: string read FSpeciality write FSpeciality;
    /// <summary>
    /// Идентификатор типа
    /// </summary>
    property &Type: Integer read FType write FType;
    /// <summary>
    /// Название типа. Возможные значения для пар type-typeStr
    ///  0 — "школа";
    ///  1 — "гимназия";
    ///  2 —"лицей";
    ///  3 — "школа-интернат";
    ///  4 — "школа вечерняя";
    ///  5 — "школа музыкальная";
    ///  6 — "школа спортивная";
    ///  7 — "школа художественная";
    ///  8 — "колледж";
    ///  9 — "профессиональный лицей";
    ///  10 — "техникум";
    ///  11 — "ПТУ";
    ///  12 — "училище";
    ///  13 — "школа искусств".
    /// </summary>
    property TypeStr: string read FType_str write FType_str;
    /// <summary>
    /// Год начала обучения
    /// </summary>
    property YearFrom: Integer read FYear_from write FYear_from;
    /// <summary>
    /// Год окончания обучения
    /// </summary>
    property YearTo: Integer read FYear_to write FYear_to;
    /// <summary>
    /// Год выпуска
    /// </summary>
    property YearGraduated: Integer read FYear_graduated write FYear_graduated;
  end;

  TVkUniversities = class(TVkBasicObject)
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
    FEducation_form_id: Integer;
    FEducation_status_id: Integer;
  public
    /// <summary>
    /// Идентификатор университета
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор страны, в которой расположен университет
    /// </summary>
    property Country: Integer read FCountry write FCountry;
    /// <summary>
    /// Идентификатор города, в котором расположен университет
    /// </summary>
    property City: Integer read FCity write FCity;
    /// <summary>
    /// Наименование университета
    /// </summary>
    property Name;
    /// <summary>
    /// Идентификатор факультета
    /// </summary>
    property Faculty: Integer read FFaculty write FFaculty;
    /// <summary>
    /// Наименование факультета
    /// </summary>
    property FacultyName: string read FFaculty_name write FFaculty_name;
    /// <summary>
    /// Идентификатор кафедры
    /// </summary>
    property Chair: Integer read FChair write FChair;
    /// <summary>
    /// Наименование кафедры
    /// </summary>
    property ChairName: string read FChair_name write FChair_name;
    /// <summary>
    /// Год окончания обучения
    /// </summary>
    property Graduation: Integer read FGraduation write FGraduation;
    /// <summary>
    /// Форма обучения (Id)
    /// </summary>
    property EducationFormId: Integer read FEducation_form_id write FEducation_form_id;
    /// <summary>
    /// Форма обучения
    /// </summary>
    property EducationForm: string read FEducation_form write FEducation_form;
    /// <summary>
    /// Статус (например, «Выпускник (специалист)»)
    /// </summary>
    property EducationStatus: string read FEducation_status write FEducation_status;
    /// <summary>
    /// Статус (например, «Выпускник (специалист)») (Id)
    /// </summary>
    property EducationStatusId: Integer read FEducation_status_id write FEducation_status_id;
  end;

  TVkPersonal = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TPersonalAttitudeInterceptor)]
    FAlcohol: TVkPersonalAttitude;
    FInspired_by: string;
    FLangs: TArray<string>;
    FLife_main: Integer;
    FPeople_main: Integer;
    [JsonReflectAttribute(ctString, rtString, TPoliticalInterceptor)]
    FPolitical: TVkPolitical;
    FReligion: string;
    FReligion_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TPersonalAttitudeInterceptor)]
    FSmoking: TVkPersonalAttitude;
    FLangs_full: TArray<TVkLangFull>;
  public
    /// <summary>
    /// Отношение к алкоголю
    /// </summary>
    property Alcohol: TVkPersonalAttitude read FAlcohol write FAlcohol;
    /// <summary>
    /// Источники вдохновения
    /// </summary>
    property InspiredBy: string read FInspired_by write FInspired_by;
    /// <summary>
    /// Языки
    /// </summary>
    property Langs: TArray<string> read FLangs write FLangs;
    property LangsFull: TArray<TVkLangFull> read FLangs_full write FLangs_full;
    /// <summary>
    /// Главное в жизни
    ///  1 — семья и дети;
    ///  2 — карьера и деньги;
    ///  3 — развлечения и отдых;
    ///  4 — наука и исследования;
    ///  5 — совершенствование мира;
    ///  6 — саморазвитие;
    ///  7 — красота и искусство;
    ///  8 — слава и влияние;
    /// </summary>
    property LifeMain: Integer read FLife_main write FLife_main;
    /// <summary>
    /// Главное в людях
    ///  1 — ум и креативность;
    ///  2 — доброта и честность;
    ///  3 — красота и здоровье;
    ///  4 — власть и богатство;
    ///  5 — смелость и упорство;
    ///  6 — юмор и жизнелюбие.
    /// </summary>
    property PeopleMain: Integer read FPeople_main write FPeople_main;
    /// <summary>
    /// Политические предпочтения
    /// </summary>
    property Political: TVkPolitical read FPolitical write FPolitical;
    /// <summary>
    /// Мировоззрение
    /// </summary>
    property Religion: string read FReligion write FReligion;
    /// <summary>
    /// Мировоззрение
    /// </summary>
    property ReligionId: Integer read FReligion_id write FReligion_id;
    /// <summary>
    /// Отношение к курению
    /// </summary>
    property Smoking: TVkPersonalAttitude read FSmoking write FSmoking;
    destructor Destroy; override;
  end;

  TVkMilitary = class(TVkEntity)
  private
    FCountry_id: Integer;
    FFrom: Integer;
    FUnit: string;
    FUnit_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUntil: TDateTime;
  public
    property CountryId: Integer read FCountry_id write FCountry_id;
    property From: Integer read FFrom write FFrom;
    property &Unit: string read FUnit write FUnit;
    property UnitId: Integer read FUnit_id write FUnit_id;
    property &Until: TDateTime read FUntil write FUntil;
  end;

  TVkCareer = class(TVkEntity)
  private
    FCity_id: Integer;
    FCompany: string;
    FCountry_id: Integer;
    FFrom: Integer;
    FPosition: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUntil: TDateTime;
    FGroup_id: Integer;
    FCity_name: string;
  public
    property CityId: Integer read FCity_id write FCity_id;
    property GroupId: Integer read FGroup_id write FGroup_id;
    property Company: string read FCompany write FCompany;
    property CityName: string read FCity_name write FCity_name;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property From: Integer read FFrom write FFrom;
    property Position: string read FPosition write FPosition;
    property &Until: TDateTime read FUntil write FUntil;
  end;

  TVkOccupation = class(TVkBasicObject)
  private
    FType: string;
    FCountry_id: Integer;
    FCity_id: Integer;
  public
    /// <summary>
    /// Тип
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Идентификатор школы, вуза, сообщества компании (в которой пользователь работает);
    /// </summary>
    property Id;
    /// <summary>
    /// Название школы, вуза или места работы
    /// </summary>
    property Name;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property CityId: Integer read FCity_id write FCity_id;
  end;

  TVkLastSeen = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TPlatformInterceptor)]
    FPlatform: TVkPlatform;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTime: TDateTime;
  public
    property &Platform: TVkPlatform read FPlatform write FPlatform;
    property Time: TDateTime read FTime write FTime;
  end;

  TVkUserOnlineInfo = class(TVkEntity)
  private
    FIs_mobile: Boolean;
    FIs_online: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FLast_seen: TDateTime;
    FVisible: Boolean;
    FApp_id: Int64;
  public
    property IsMobile: Boolean read FIs_mobile write FIs_mobile;
    property IsOnline: Boolean read FIs_online write FIs_online;
    property LastSeen: TDateTime read FLast_seen write FLast_seen;
    property Visible: Boolean read FVisible write FVisible;
    property AppId: Int64 read FApp_id write FApp_id;
  end;

  TVkProfile = class(TVkObject)
  private
    FAbout: string;
    FActivities: string;
    FBdate: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FBlacklisted: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FBlacklisted_by_me: Boolean;
    FBooks: string;
    FCan_access_closed: Boolean;
    FCan_be_invited_group: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_post: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_see_all_posts: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_see_audio: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_send_friend_request: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
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
    [JsonReflectAttribute(ctString, rtString, TFriendStatusInterceptor)]
    FFriend_status: TVkFriendStatus;
    FGames: string;
    FGraduation: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FHas_mobile: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FHas_photo: Boolean;
    FHome_phone: string;
    FHome_town: string;
    FInstagram: string;
    FInterests: string;
    FIs_closed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_favorite: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_friend: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_hidden_from_feed: Boolean;
    FLast_name: string;
    FLast_seen: TVkLastSeen;
    FMilitary: TArray<TVkMilitary>;
    FMobile_phone: string;
    FMovies: string;
    FMusic: string;
    FNickname: string;
    FOccupation: TVkOccupation;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
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
    [JsonReflectAttribute(ctString, rtString, TRelationInterceptor)]
    FRelation: TVkRelation;
    FRelation_partner: TVkProfile;
    FRelatives: TArray<TVkRelative>;
    FSchools: TArray<TVkSchoolInfo>;
    FScreen_name: string;
    [JsonReflectAttribute(ctString, rtString, TSexInterceptor)]
    FSex: TVkSex;
    FSite: string;
    FSkype: string;
    FStatus: string;
    FTimezone: Extended;
    FTv: string;
    FTwitter: string;
    FUniversities: TArray<TVkUniversities>;
    FUniversity: Integer;
    FUniversity_name: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FVerified: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FTrending: Boolean;
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
    FCan_invite_to_chats: Boolean;
    FTrack_code: string;
    FLists: TArray<Integer>;
    [JsonReflectAttribute(ctString, rtString, TDeactivatedInterceptor)]
    FDeactivated: TVkDeactivated;
    FLivejournal: string;
    FCounters: TVkCounters;
    FExports: TArray<TVkExport>;
    FFirst_name_nom: string;
    FFirst_name_dat: string;
    FFirst_name_abl: string;
    FFirst_name_acc: string;
    FFirst_name_ins: string;
    FFirst_name_gen: string;
    FLast_name_nom: string;
    FLast_name_dat: string;
    FLast_name_abl: string;
    FLast_name_acc: string;
    FLast_name_ins: string;
    FLast_name_gen: string;
    FMaiden_name: string;
    FStatus_audio: TVkAudio;
    FWall_default: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_no_index: Boolean;
    function GetRefer: string;
    function FGetFullName: string;
    function GetFullNameAcc: string;
  public
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    property Id;
    /// <summary>
    /// Содержимое поля «О себе» из профиля.
    /// </summary>
    property About: string read FAbout write FAbout;
    /// <summary>
    /// Содержимое поля «Деятельность» из профиля.
    /// </summary>
    property Activities: string read FActivities write FActivities;
    /// <summary>
    ///
    /// </summary>
    property Activity: string read FActivity write FActivity;
    /// <summary>
    /// Дата рождения. Возвращается в формате D.M.YYYY или D.M (если год рождения скрыт). Если дата рождения скрыта целиком, поле отсутствует в ответе.
    /// </summary>
    property BirthDate: string read FBdate write FBdate;
    /// <summary>
    /// Информация о том, находится ли текущий пользователь в черном списке
    /// </summary>
    property Blacklisted: Boolean read FBlacklisted write FBlacklisted;
    /// <summary>
    /// Информация о том, находится ли пользователь в черном списке у текущего пользователя
    /// </summary>
    property BlacklistedByMe: Boolean read FBlacklisted_by_me write FBlacklisted_by_me;
    /// <summary>
    /// Содержимое поля «Любимые книги» из профиля пользователя.
    /// </summary>
    property Books: string read FBooks write FBooks;
    /// <summary>
    /// Может ли текущий пользователь видеть профиль при is_closed = 1 (например, он есть в друзьях).
    /// </summary>
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    /// <summary>
    /// Может ли текущий пользователь пригласить в группу
    /// </summary>
    property CanBeInvitedGroup: Boolean read FCan_be_invited_group write FCan_be_invited_group;
    /// <summary>
    /// Может ли текущий пользователь пригласить в чат
    /// </summary>
    property CanInviteToChats: Boolean read FCan_invite_to_chats write FCan_invite_to_chats;
    /// <summary>
    /// Информация о том, может ли текущий пользователь оставлять записи на стене
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_post;
    /// <summary>
    /// Информация о том, может ли текущий пользователь видеть чужие записи на стене
    /// </summary>
    property CanSeeAllPosts: Boolean read FCan_see_all_posts write FCan_see_all_posts;
    /// <summary>
    /// Информация о том, может ли текущий пользователь видеть аудиозаписи
    /// </summary>
    property CanSeeAudio: Boolean read FCan_see_audio write FCan_see_audio;
    /// <summary>
    /// Информация о том, будет ли отправлено уведомление пользователю о заявке в друзья от текущего пользователя
    /// </summary>
    property CanSendFriendRequest: Boolean read FCan_send_friend_request write FCan_send_friend_request;
    /// <summary>
    /// Информация о том, может ли текущий пользователь отправить личное сообщение
    /// </summary>
    property CanWritePrivateMessage: Boolean read FCan_write_private_message write FCan_write_private_message;
    /// <summary>
    /// Информация о карьере пользователя
    /// </summary>
    property Career: TArray<TVkCareer> read FCareer write FCareer;
    /// <summary>
    /// Информация о городе, указанном на странице пользователя в разделе «Контакты»
    /// </summary>
    property City: TVkCity read FCity write FCity;
    /// <summary>
    /// Количество общих друзей с текущим пользователем
    /// </summary>
    property CommonCount: Integer read FCommon_count write FCommon_count;
    /// <summary>
    /// Информация о стране, указанной на странице пользователя в разделе «Контакты»
    /// </summary>
    property Country: TVkCountry read FCountry write FCountry;
    /// <summary>
    /// Количество различных объектов у пользователя. Поле возвращается только в методе users.get при запросе информации об одном пользователе, с передачей пользовательского access_token
    /// </summary>
    property Counters: TVkCounters read FCounters write FCounters;
    /// <summary>
    /// Возвращает данные о точках, по которым вырезаны профильная и миниатюрная фотографии пользователя, при наличии
    /// </summary>
    property CropPhoto: TVkCropPhoto read FCrop_photo write FCrop_photo;
    /// <summary>
    /// Поле возвращается, если страница пользователя удалена или заблокирована, содержит значение deleted или banned. В этом случае опциональные поля не возвращаются.
    /// </summary>
    property Deactivated: TVkDeactivated read FDeactivated write FDeactivated;
    /// <summary>
    /// Короткий адрес страницы. Возвращается строка, содержащая короткий адрес страницы (например, andrew). Если он не назначен, возвращается "id"+user_id, например, id35828305
    /// </summary>
    property Domain: string read FDomain write FDomain;
    /// <summary>
    /// EducationForm
    /// </summary>
    property EducationForm: string read FEducation_form write FEducation_form;
    /// <summary>
    /// EducationStatus
    /// </summary>
    property EducationStatus: string read FEducation_status write FEducation_status;
    /// <summary>
    /// Внешние сервисы, в которые настроен экспорт из ВК (twitter, facebook, livejournal, instagram)
    /// </summary>
    property &Exports: TArray<TVkExport> read FExports write FExports;
    /// <summary>
    /// Facebook
    /// </summary>
    property Facebook: string read FFacebook write FFacebook;
    /// <summary>
    /// Имя на Facebook
    /// </summary>
    property FacebookName: string read FFacebook_name write FFacebook_name;
    /// <summary>
    /// Идентификатор факультета
    /// </summary>
    property Faculty: Integer read FFaculty write FFaculty;
    /// <summary>
    /// Название факультета
    /// </summary>
    property FacultyName: string read FFaculty_name write FFaculty_name;
    /// <summary>
    /// Имя
    /// </summary>
    property FirstName: string read FFirst_name write FFirst_name;
    /// <summary>
    /// Имя (именительный)
    /// </summary>
    property FirstNameNom: string read FFirst_name_nom write FFirst_name_nom;
    /// <summary>
    /// Имя (родительный)
    /// </summary>
    property FirstNameGen: string read FFirst_name_gen write FFirst_name_gen;
    /// <summary>
    /// Имя (дательный)
    /// </summary>
    property FirstNameDat: string read FFirst_name_dat write FFirst_name_dat;
    /// <summary>
    /// Имя (винительный)
    /// </summary>
    property FirstNameAcc: string read FFirst_name_acc write FFirst_name_acc;
    /// <summary>
    /// Имя (творительный)
    /// </summary>
    property FirstNameIns: string read FFirst_name_ins write FFirst_name_ins;
    /// <summary>
    /// Имя (предложный)
    /// </summary>
    property FirstNameAbl: string read FFirst_name_abl write FFirst_name_abl;
    /// <summary>
    /// Количество подписчиков пользователя
    /// </summary>
    property FollowersCount: Integer read FFollowers_count write FFollowers_count;
    /// <summary>
    ///
    /// </summary>
    property FoundWith: string read FFound_with write FFound_with;
    /// <summary>
    /// Статус дружбы с пользователем
    /// 0 — не является другом
    /// 1 — отправлена заявка/подписка пользователю
    /// 2 — имеется входящая заявка/подписка от пользователя
    /// 3 — является другом
    /// </summary>
    property FriendStatus: TVkFriendStatus read FFriend_status write FFriend_status;
    /// <summary>
    /// Содержимое поля «Любимые игры» из профиля
    /// </summary>
    property Games: string read FGames write FGames;
    /// <summary>
    /// Год окончания
    /// </summary>
    property Graduation: Integer read FGraduation write FGraduation;
    /// <summary>
    /// Информация о том, известен ли номер мобильного телефона пользователя
    /// </summary>
    property HasMobile: Boolean read FHas_mobile write FHas_mobile;
    /// <summary>
    /// True, если пользователь установил фотографию для профиля
    /// </summary>
    property HasPhoto: Boolean read FHas_photo write FHas_photo;
    /// <summary>
    /// Дополнительный номер телефона пользователя
    /// </summary>
    property HomePhone: string read FHome_phone write FHome_phone;
    /// <summary>
    /// Название родного города
    /// </summary>
    property HomeTown: string read FHome_town write FHome_town;
    /// <summary>
    /// Instagram
    /// </summary>
    property Instagram: string read FInstagram write FInstagram;
    /// <summary>
    /// Содержимое поля «Интересы» из профиля
    /// </summary>
    property Interests: string read FInterests write FInterests;
    /// <summary>
    /// Был приглашен
    /// </summary>
    property InvitedBy: Boolean read FInvited_by write FInvited_by;
    /// <summary>
    /// Скрыт ли профиль пользователя настройками приватности.
    /// </summary>
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    /// <summary>
    /// Информация о том, есть ли пользователь в закладках у текущего пользователя
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    /// <summary>
    /// Информация о том, является ли пользователь другом текущего пользователя
    /// </summary>
    property IsFriend: Boolean read FIs_friend write FIs_friend;
    /// <summary>
    /// Информация о том, скрыт ли пользователь из ленты новостей текущего пользователя
    /// </summary>
    property IsHiddenFromFeed: Boolean read FIs_hidden_from_feed write FIs_hidden_from_feed;
    /// <summary>
    /// Индексируется ли профиль поисковыми сайтами
    /// True - профиль скрыт от поисковых сайтов
    /// False - профиль доступен поисковым сайтам.
    /// (В настройках приватности: https://vk.com/settings?act=privacy,
    /// в пункте «Кому в интернете видна моя страница», выбрано значение «Всем»
    /// </summary>
    property IsNoIndex: Boolean read FIs_no_index write FIs_no_index;
    /// <summary>
    /// Фамилия
    /// </summary>
    property LastName: string read FLast_name write FLast_name;
    /// <summary>
    /// Фамилия (именительный)
    /// </summary>
    property LastNameNom: string read FLast_name_nom write FLast_name_nom;
    /// <summary>
    /// Фамилия (родительный)
    /// </summary>
    property LastNameGen: string read FLast_name_gen write FLast_name_gen;
    /// <summary>
    /// Фамилия (дательный)
    /// </summary>
    property LastNameDat: string read FLast_name_dat write FLast_name_dat;
    /// <summary>
    /// Фамилия (винительный)
    /// </summary>
    property LastNameAcc: string read FLast_name_acc write FLast_name_acc;
    /// <summary>
    /// Фамилия (творительный)
    /// </summary>
    property LastNameIns: string read FLast_name_ins write FLast_name_ins;
    /// <summary>
    /// Фамилия (предложный)
    /// </summary>
    property LastNameAbl: string read FLast_name_abl write FLast_name_abl;
    /// <summary>
    /// Время последнего посещения
    /// </summary>
    property LastSeen: TVkLastSeen read FLast_seen write FLast_seen;
    /// <summary>
    /// Разделенные запятой идентификаторы списков друзей, в которых состоит пользователь
    /// </summary>
    property Lists: TArray<Integer> read FLists write FLists;
    /// <summary>
    /// LiveJournal
    /// </summary>
    property LiveJournal: string read FLivejournal write FLivejournal;
    /// <summary>
    /// Девичья фамилия
    /// </summary>
    property MaidenName: string read FMaiden_name write FMaiden_name;
    /// <summary>
    /// Информация о военной службе пользователя
    /// </summary>
    property Military: TArray<TVkMilitary> read FMilitary write FMilitary;
    /// <summary>
    /// Номер мобильного телефона пользователя (только для Standalone-приложений)
    /// </summary>
    property MobilePhone: string read FMobile_phone write FMobile_phone;
    /// <summary>
    /// Содержимое поля «Любимые фильмы» из профиля пользователя
    /// </summary>
    property Movies: string read FMovies write FMovies;
    /// <summary>
    /// Содержимое поля «Любимая музыка» из профиля пользователя
    /// </summary>
    property Music: string read FMusic write FMusic;
    /// <summary>
    /// Общее
    /// </summary>
    property Mutual: TVkFriendsMutual read FMutual write FMutual;
    /// <summary>
    /// Никнейм (отчество) пользователя
    /// </summary>
    property NickName: string read FNickname write FNickname;
    /// <summary>
    /// Информация о текущем роде занятия пользователя
    /// </summary>
    property Occupation: TVkOccupation read FOccupation write FOccupation;
    /// <summary>
    /// Информация о том, находится ли пользователь сейчас на сайте. Если пользователь использует мобильное приложение либо мобильную версию, возвращается дополнительное поле online_mobile, содержащее 1. При этом, если используется именно приложение, дополнительно возвращается поле online_app, содержащее его идентификатор
    /// </summary>
    property Online: Boolean read FOnline write FOnline;
    /// <summary>
    /// Информация о статусе онлайн
    /// </summary>
    property OnlineInfo: TVkUserOnlineInfo read FOnline_info write FOnline_info;
    /// <summary>
    /// Информация о полях из раздела «Жизненная позиция»
    /// </summary>
    property Personal: TVkPersonal read FPersonal write FPersonal;
    /// <summary>
    /// Фото 50
    /// </summary>
    property Photo50: string read FPhoto_50 write FPhoto_50;
    /// <summary>
    /// Фото 100
    /// </summary>
    property Photo100: string read FPhoto_100 write FPhoto_100;
    /// <summary>
    /// Фото 200
    /// </summary>
    property Photo200: string read FPhoto_200 write FPhoto_200;
    /// <summary>
    /// Фото 200 Orig
    /// </summary>
    property Photo200_Orig: string read FPhoto_200_orig write FPhoto_200_orig;
    /// <summary>
    /// Фото 400 Orig
    /// </summary>
    property Photo400_Orig: string read FPhoto_400_orig write FPhoto_400_orig;
    /// <summary>
    /// Фото
    /// </summary>
    property Photo: string read FPhoto write FPhoto;
    /// <summary>
    /// Фото Big
    /// </summary>
    property PhotoBig: string read FPhoto_big write FPhoto_big;
    /// <summary>
    /// 415730216_457299006
    /// </summary>
    property PhotoId: string read FPhoto_id write FPhoto_id;
    /// <summary>
    /// Фото Max
    /// </summary>
    property PhotoMax: string read FPhoto_max write FPhoto_max;
    /// <summary>
    /// Фото Max Orig
    /// </summary>
    property PhotoMax_Orig: string read FPhoto_max_orig write FPhoto_max_orig;
    /// <summary>
    /// Фото Medium
    /// </summary>
    property PhotoMedium: string read FPhoto_medium write FPhoto_medium;
    /// <summary>
    /// Фото Medium Rec
    /// </summary>
    property PhotoMediumRec: string read FPhoto_medium_rec write FPhoto_medium_rec;
    /// <summary>
    /// Любимые цитаты
    /// </summary>
    property Quotes: string read FQuotes write FQuotes;
    /// <summary>
    /// Семейное положение
    /// </summary>
    property Relation: TVkRelation read FRelation write FRelation;
    /// <summary>
    /// Информация о партнёре
    /// </summary>
    property RelationPartner: TVkProfile read FRelation_partner write FRelation_partner;
    /// <summary>
    /// Список родственников
    /// </summary>
    property Relatives: TArray<TVkRelative> read FRelatives write FRelatives;
    /// <summary>
    /// Список школ, в которых учился пользователь
    /// </summary>
    property Schools: TArray<TVkSchoolInfo> read FSchools write FSchools;
    /// <summary>
    /// Короткое имя страницы
    /// </summary>
    property ScreenName: string read FScreen_name write FScreen_name;
    /// <summary>
    /// Пол
    /// </summary>
    property Sex: TVkSex read FSex write FSex;
    /// <summary>
    /// Адрес сайта, указанный в профиле
    /// </summary>
    property Site: string read FSite write FSite;
    /// <summary>
    /// Skype
    /// </summary>
    property Skype: string read FSkype write FSkype;
    /// <summary>
    /// Статус пользователя. Возвращается строка, содержащая текст статуса, расположенного в профиле под именем. Если включена опция «Транслировать в статус играющую музыку», возвращается дополнительное поле status_audio, содержащее информацию о композиции
    /// </summary>
    property Status: string read FStatus write FStatus;
    /// <summary>
    /// Аудиозапись в статусе
    /// </summary>
    property StatusAudio: TVkAudio read FStatus_audio write FStatus_audio;
    /// <summary>
    /// Временная зона. Только при запросе информации о текущем пользователе
    /// </summary>
    property TimeZone: Extended read FTimezone write FTimezone;
    /// <summary>
    /// TrackCode
    /// </summary>
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// Информация о том, есть ли на странице пользователя «огонёк»
    /// </summary>
    property Trending: Boolean read FTrending write FTrending;
    /// <summary>
    /// Любимые телешоу
    /// </summary>
    property TV: string read FTv write FTv;
    /// <summary>
    /// Twitter
    /// </summary>
    property Twitter: string read FTwitter write FTwitter;
    /// <summary>
    /// Список вузов, в которых учился пользователь
    /// </summary>
    property Universities: TArray<TVkUniversities> read FUniversities write FUniversities;
    /// <summary>
    /// Идентификатор университета
    /// </summary>
    property University: Integer read FUniversity write FUniversity;
    /// <summary>
    /// Название университета
    /// </summary>
    property UniversityName: string read FUniversity_name write FUniversity_name;
    /// <summary>
    /// Возвращается True, если страница пользователя верифицирована, False — если нет
    /// </summary>
    property Verified: Boolean read FVerified write FVerified;
    /// <summary>
    /// Type
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Режим стены по умолчанию. Возможные значения: owner, all.
    /// </summary>
    property WallDefault: string read FWall_default write FWall_default;
    /// <summary>
    /// [Domain|FirstName]
    /// </summary>
    property Refer: string read GetRefer;
    /// <summary>
    /// FirstName LastName
    /// </summary>
    property FullName: string read FGetFullName;
    property FullNameAcc: string read GetFullNameAcc;
    destructor Destroy; override;
  end;

  TVkProfiles = TVkObjectList<TVkProfile>;

  TVkFriendsList = class(TVkObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TVkFriendsLists = TVkObjectList<TVkFriendsList>;

implementation

uses
  VK.CommonUtils;

{TVkProfile}

destructor TVkProfile.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCareer>(FCareer);
  TArrayHelp.FreeArrayOfObject<TVkMilitary>(FMilitary);
  TArrayHelp.FreeArrayOfObject<TVkExport>(FExports);
  TArrayHelp.FreeArrayOfObject<TVkUniversities>(FUniversities);
  TArrayHelp.FreeArrayOfObject<TVkSchoolInfo>(FSchools);
  TArrayHelp.FreeArrayOfObject<TVkRelative>(FRelatives);
  if Assigned(FOnline_info) then
    FOnline_info.Free;
  if Assigned(FCountry) then
    FCountry.Free;
  if Assigned(FCity) then
    FCity.Free;
  if Assigned(FLast_seen) then
    FLast_seen.Free;
  if Assigned(FCrop_photo) then
    FCrop_photo.Free;
  if Assigned(FOccupation) then
    FOccupation.Free;
  if Assigned(FRelation_partner) then
    FRelation_partner.Free;
  if Assigned(FPersonal) then
    FPersonal.Free;
  if Assigned(FMutual) then
    FMutual.Free;
  if Assigned(FCounters) then
    FCounters.Free;
  if Assigned(FStatus_audio) then
    FStatus_audio.Free;
  inherited;
end;

function TVkProfile.FGetFullName: string;
begin
  Result := FFirst_name + ' ' + FLast_name;
end;

function TVkProfile.GetFullNameAcc: string;
begin
  Result := FFirst_name_acc + ' ' + FLast_name_acc;
end;

function TVkProfile.GetRefer: string;
begin
  Result := '[' + Domain + '|' + FirstName + ']';
end;

{ TVkFriendsMutual }

destructor TVkFriendsMutual.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FUsers);
  inherited;
end;

{ TVkPersonal }

destructor TVkPersonal.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkLangFull>(FLangs_full);
  inherited;
end;

end.

