unit VKAuth.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types,
  System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, VK.API, VK.Components, VK.Types, Vcl.ExtCtrls, VK.Handler,
  Vcl.StdCtrls, System.Generics.Defaults, Vcl.ComCtrls, VK.UserEvents,
  VK.GroupEvents, VK.Entity.Media, System.Net.URLClient, System.Net.HttpClient,
  VK.Entity.Message, VK.Entity.ClientInfo, VK.Entity.Video, VK.Entity.Photo,
  VK.Entity.Audio, System.JSON, VK.Entity.GroupSettings, Vcl.CategoryButtons;

type
  TFormMain = class(TForm)
    VK1: TVK;
    Panel2: TPanel;
    Memo1: TMemo;
    MemoLog: TMemo;
    VkUserEvents1: TVkUserEvents;
    VkGroupEventsController1: TVkGroupEventsController;
    VkGroupEvents1: TVkGroupEvents;
    PageControl2: TPageControl;
    TabSheetWelcome: TTabSheet;
    TabSheetAuth: TTabSheet;
    TabSheetMethods: TTabSheet;
    TabSheetTests: TTabSheet;
    CategoryButtons1: TCategoryButtons;
    MemoCode: TMemo;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet9: TTabSheet;
    ButtonUploadAudioMessage: TButton;
    ButtonLongPollStart: TButton;
    ButtonLongPollStop: TButton;
    ButtonGLongPollStart: TButton;
    ButtonGLongPollStop: TButton;
    ButtonCallMethod: TButton;
    ButtonSearchGetHints: TButton;
    ButtonDBGetRegions: TButton;
    ButtonDBGetSchoolClasses: TButton;
    ButtonStorageGet: TButton;
    ButtonSecureGetBalance: TButton;
    ButtonStoriesGet: TButton;
    TabSheet1: TTabSheet;
    ButtonAccountBan: TButton;
    ButtonAccountUnban: TButton;
    ButtonAccountActiveOffers: TButton;
    ButtonAccountAppPrem: TButton;
    ButtonAccountCounters: TButton;
    ButtonAccountPushSettings: TButton;
    ButtonAccountSaveProfileInfo: TButton;
    ButtonAccountOnline: TButton;
    ButtonAccountOffline: TButton;
    Button40: TButton;
    TabSheet2: TTabSheet;
    ButtonAuthCheckPhone: TButton;
    ButtonAuthSingup: TButton;
    TabSheet3: TTabSheet;
    ButtonAudioGet: TButton;
    ButtonAudioGetAlbums: TButton;
    ButtonAudioGetRecoms: TButton;
    ButtonAudioGetPop: TButton;
    ButtonAudioGetChart: TButton;
    ButtonGetCatalog: TButton;
    ButtonCreatePlaylist: TButton;
    ButtonEditPlaylist: TButton;
    ButtonAudioAddToPlaylist: TButton;
    ButtonAudioGetCount: TButton;
    TabSheet4: TTabSheet;
    ButtonBoardCreateComment: TButton;
    TabSheet5: TTabSheet;
    ButtonStatusGet: TButton;
    ButtonStatusSet: TButton;
    TabSheet6: TTabSheet;
    ButtonWallPost: TButton;
    ButtonWallGet: TButton;
    TabSheet7: TTabSheet;
    Button12: TButton;
    TabSheet8: TTabSheet;
    ButtonGroupsGetMembers: TButton;
    ButtonGroupsGetById: TButton;
    ButtonGroupsGet: TButton;
    ButtonVideoDelete: TButton;
    TabSheet10: TTabSheet;
    ButtonMesGetConv: TButton;
    ButtonMesGetHistory: TButton;
    ButtonMesSendToPeer: TButton;
    Button49: TButton;
    ButtonSendPhoto: TButton;
    TabSheet11: TTabSheet;
    ButtonFriendsGet: TButton;
    ButtonGetFriendWithAudio: TButton;
    TabSheetPolls: TTabSheet;
    ButtonPollsGetBG: TButton;
    TabSheetPodcasts: TTabSheet;
    ButtonPodcastsSearch: TButton;
    TabSheetNewsfeed: TTabSheet;
    ButtonNewsfeedGet: TButton;
    TabSheetAds: TTabSheet;
    ButtonAdsGetAccounts: TButton;
    Memo2: TMemo;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    Panel3: TPanel;
    ButtonLogin: TButton;
    Label2: TLabel;
    LabelLogin: TLabel;
    Panel4: TPanel;
    Button47: TButton;
    Label3: TLabel;
    HeaderControl1: THeaderControl;
    ButtonSendAudioMessage: TButton;
    ButtonUploadAudio: TButton;
    ButtonBoardGetBoard: TButton;
    TabSheetPhotos: TTabSheet;
    ButtonPhotosGetAlbum: TButton;
    ButtonMessageGetChat: TButton;
    ButtonMessageGetConverstion: TButton;
    ButtonGetMessageById: TButton;
    TabSheetMarket: TTabSheet;
    ButtonMarketFilterCategories: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonAccountBanClick(Sender: TObject);
    procedure ButtonAccountUnbanClick(Sender: TObject);
    procedure ButtonAccountActiveOffersClick(Sender: TObject);
    procedure ButtonAccountAppPremClick(Sender: TObject);
    procedure ButtonAccountCountersClick(Sender: TObject);
    procedure ButtonAccountPushSettingsClick(Sender: TObject);
    procedure ButtonAccountSaveProfileInfoClick(Sender: TObject);
    procedure ButtonCallMethodClick(Sender: TObject);
    procedure ButtonAccountOnlineClick(Sender: TObject);
    procedure ButtonAccountOfflineClick(Sender: TObject);
    procedure VK1Login(Sender: TObject);
    procedure VK1Log(Sender: TObject; const Value: string);
    procedure ButtonAuthCheckPhoneClick(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure ButtonLongPollStartClick(Sender: TObject);
    procedure ButtonLongPollStopClick(Sender: TObject);
    procedure ButtonGLongPollStartClick(Sender: TObject);
    procedure ButtonGLongPollStopClick(Sender: TObject);
    procedure VkUserEvents1UserOnline(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp: TDateTime);
    procedure VkUserEvents1RecoverMessages(Sender: TObject; PeerId, LocalId: Integer);
    procedure VkUserEvents1ChangeDialogFlags(Sender: TObject; DialogChangeData: TDialogChangeData);
    procedure VkUserEvents1ChangeMessageFlags(Sender: TObject; MessageChangeData: TMessageChangeData);
    procedure VkUserEvents1ChatChanged(Sender: TObject; const ChatId: Integer; IsSelf: Boolean);
    procedure VkUserEvents1ChatChangeInfo(Sender: TObject; const PeerId: Integer; TypeId: TVkChatChangeInfoType; Info: Integer);
    procedure VkUserEvents1DeleteMessages(Sender: TObject; PeerId, LocalId: Integer);
    procedure VkUserEvents1EditMessage(Sender: TObject; MessageData: TMessageData);
    procedure VkUserEvents1UserOffline(Sender: TObject; UserId: Integer; InactiveUser: Boolean; TimeStamp: TDateTime);
    procedure VkUserEvents1NewMessage(Sender: TObject; MessageData: TMessageData);
    procedure VkUserEvents1ReadMessages(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer);
    procedure VkUserEvents1UsersRecording(Sender: TObject; Data: TChatRecordingData);
    procedure VkUserEvents1UsersTyping(Sender: TObject; Data: TChatTypingData);
    procedure VkUserEvents1UserTyping(Sender: TObject; UserId, ChatId: Integer);
    procedure VkUserEvents1UserCall(Sender: TObject; UserId, CallId: Integer);
    procedure VkUserEvents1CountChange(Sender: TObject; Count: Integer);
    procedure VkUserEvents1NotifyChange(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer);
    procedure ButtonStatusGetClick(Sender: TObject);
    procedure ButtonStatusSetClick(Sender: TObject);
    procedure ButtonWallPostClick(Sender: TObject);
    procedure ButtonUploadAudioMessageClick(Sender: TObject);
    procedure VkGroupEventsController1MessageNew(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
    procedure VkGroupEventsController1WallReplyRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1WallReplyNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1WallReplyEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1WallReplyDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
    procedure VkGroupEventsController1WallPostNew(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
    procedure VkGroupEventsController1WallRepost(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
    procedure VkGroupEventsController1VideoNew(Sender: TObject; GroupId: Integer; Video: TVkVideo; const EventId: string);
    procedure VkGroupEventsController1VideoCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1PhotoCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1MarketCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1VideoCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1PhotoCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1MarketCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1VideoCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1PhotoCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1MarketCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1BoardPostRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1BoardPostEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1BoardPostNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
    procedure VkGroupEventsController1VideoCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
    procedure VkGroupEventsController1PhotoCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
    procedure VkGroupEventsController1MarketCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
    procedure VkGroupEventsController1BoardPostDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
    procedure VkGroupEventsController1PhotoNew(Sender: TObject; GroupId: Integer; Photo: TVkPhoto; const EventId: string);
    procedure VkGroupEventsController1AudioNew(Sender: TObject; GroupId: Integer; Audio: TVkAudio; const EventId: string);
    procedure VkGroupEventsController1GroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
    procedure VkGroupEventsController1GroupLeave(Sender: TObject; GroupId, UserId: Integer; IsSelf: Boolean; const EventId: string);
    procedure VkGroupEventsController1UserBlock(Sender: TObject; GroupId: Integer; Info: TVkGroupUserBlock; const EventId: string);
    procedure VkGroupEventsController1UserUnBlock(Sender: TObject; GroupId: Integer; Info: TVkGroupUserUnBlock; const EventId: string);
    procedure VkGroupEventsController1MessageReply(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
    procedure VkGroupEventsController1MessageEdit(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
    procedure VkGroupEventsController1MessageDeny(Sender: TObject; GroupId, UserId: Integer; Key: string; const EventId: string);
    procedure VkGroupEventsController1MessageAllow(Sender: TObject; GroupId, UserId: Integer; Key: string; const EventId: string);
    procedure VkGroupEventsController1GroupPollVoteNew(Sender: TObject; GroupId: Integer; Info: TVkGroupPollVoteNew; const EventId: string);
    procedure VkGroupEventsController1GroupOfficersEdit(Sender: TObject; GroupId: Integer; Info: TVkGroupOfficersEdit; const EventId: string);
    procedure VkGroupEventsController1GroupChangeSettings(Sender: TObject; GroupId: Integer; Changes: TVkGroupSettingsChange; const EventId: string);
    procedure VkGroupEventsController1GroupChangePhoto(Sender: TObject; GroupId: Integer; Changes: TVkGroupChangePhoto; const EventId: string);
    procedure VkGroupEventsController1GroupAppPayload(Sender: TObject; GroupId: Integer; Info: TVkAppPayload; const EventId: string);
    procedure VkGroupEventsController1GroupPayTransaction(Sender: TObject; GroupId: Integer; Info: TVkPayTransaction; const EventId: string);
    procedure VkGroupEventsController1MessageTypingState(Sender: TObject; GroupId, UserId: Integer; State: string; const EventId: string);
    procedure ButtonAudioGetClick(Sender: TObject);
    procedure ButtonBoardCreateCommentClick(Sender: TObject);
    procedure VK1Error(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure ButtonAudioGetAlbumsClick(Sender: TObject);
    procedure ButtonAudioGetRecomsClick(Sender: TObject);
    procedure ButtonGroupsGetMembersClick(Sender: TObject);
    procedure ButtonUploadAudioClick(Sender: TObject);
    procedure ButtonMesGetConvClick(Sender: TObject);
    procedure ButtonFriendsGetClick(Sender: TObject);
    procedure ButtonAudioGetPopClick(Sender: TObject);
    procedure ButtonAudioGetChartClick(Sender: TObject);
    procedure VK1Auth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
    procedure ButtonGetCatalogClick(Sender: TObject);
    procedure ButtonCreatePlaylistClick(Sender: TObject);
    procedure ButtonEditPlaylistClick(Sender: TObject);
    procedure ButtonAudioAddToPlaylistClick(Sender: TObject);
    procedure ButtonPollsGetBGClick(Sender: TObject);
    procedure ButtonPodcastsSearchClick(Sender: TObject);
    procedure ButtonSearchGetHintsClick(Sender: TObject);
    procedure ButtonDBGetRegionsClick(Sender: TObject);
    procedure ButtonDBGetSchoolClassesClick(Sender: TObject);
    procedure ButtonStorageGetClick(Sender: TObject);
    procedure ButtonSecureGetBalanceClick(Sender: TObject);
    procedure ButtonStoriesGetClick(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure Button40Click(Sender: TObject);
    procedure VkUserEvents1UnhandledEvents(Sender: TObject; const JSON: TJSONValue);
    procedure VkGroupEventsController1GroupUnhandledEvents(Sender: TObject; GroupId: Integer; const JSON: TJSONValue);
    procedure ButtonWallGetClick(Sender: TObject);
    procedure ButtonAuthSingupClick(Sender: TObject);
    procedure ButtonGroupsGetByIdClick(Sender: TObject);
    procedure ButtonGroupsGetClick(Sender: TObject);
    procedure ButtonAudioGetCountClick(Sender: TObject);
    procedure ButtonGetFriendWithAudioClick(Sender: TObject);
    procedure ButtonMesGetHistoryClick(Sender: TObject);
    procedure Button47Click(Sender: TObject);
    procedure ButtonNewsfeedGetClick(Sender: TObject);
    procedure ButtonMesSendToPeerClick(Sender: TObject);
    procedure Button49Click(Sender: TObject);
    procedure ButtonAdsGetAccountsClick(Sender: TObject);
    procedure ButtonSendPhotoClick(Sender: TObject);
    procedure ButtonVideoDeleteClick(Sender: TObject);
    procedure CategoryButtons1ButtonClicked(Sender: TObject; const Button: TButtonItem);
    procedure TabSheetAuthResize(Sender: TObject);
    procedure ButtonSendAudioMessageClick(Sender: TObject);
    procedure ButtonBoardGetBoardClick(Sender: TObject);
    procedure ButtonPhotosGetAlbumClick(Sender: TObject);
    procedure ButtonMessageGetChatClick(Sender: TObject);
    procedure ButtonMessageGetConverstionClick(Sender: TObject);
    procedure ButtonGetMessageByIdClick(Sender: TObject);
    procedure ButtonMarketFilterCategoriesClick(Sender: TObject);
  private
    FToken: string;
    FChangePasswordHash: string;
    FTokenExpiry: Int64;
    procedure FillMethods;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.IOUtils, System.Rtti, VK.Entity.Common, Vk.Controller,
  VK.Entity.AccountInfo, VK.Entity.ProfileInfo, VK.Entity.ActiveOffers,
  VK.Entity.Counters, VK.Entity.PushSettings, VK.Entity.Profile,
  VK.Entity.Keyboard, VK.Status, VK.Wall, VK.Docs, VK.Entity.Doc.Save, VK.Utils,
  VK.Account, VK.Entity.AccountInfoRequest, VK.Vcl.OAuth2, VK.Entity.Playlist,
  VK.Audio, VK.Messages, VK.Entity.Audio.Upload, VK.Entity.Conversation,
  VK.Entity.Status, VK.Entity.Catalog, VK.Entity.Catalog.Section, VK.CommonUtils,
  VK.Groups, VK.Entity.Audio.Catalog, VK.Entity.Poll, VK.Entity.Podcast,
  VK.Entity.Search, VK.Entity.Database.Regions, VK.Entity.Database.Schools,
  VK.Entity.Storage, VK.Entity.Stories, VK.Entity.Podcast.Episode, VK.Auth,
  VK.Photos, VK.Entity.Group, VK.Entity.Auth, VK.Clients, VK.Entity.Photo.Upload,
  REST.Json, VK.Entity.Newsfeed, VK.Newsfeed, System.Threading, VK.Entity.Ads,
  VK.Entity.Message.Chat, VK.Entity.Board, VK.Board,
  VK.Entity.Common.ExtendedList, VK.Entity.Market, Vk.Market;

{$R *.dfm}

procedure TFormMain.ButtonAccountOfflineClick(Sender: TObject);
begin
  if VK1.Account.SetOffline then
    Memo1.Lines.Add('offline')
  else
    Memo1.Lines.Add('Error offline');
end;

procedure TFormMain.ButtonAuthCheckPhoneClick(Sender: TObject);
begin
  //depricated
  {if VK1.Auth.CheckPhone('+79512202849', True) then
    Memo1.Lines.Add('CheckPhone is ok')
  else
    Memo1.Lines.Add('CheckPhone not is ok');   }
end;

procedure TFormMain.Button12Click(Sender: TObject);
var
  Users: TVkProfiles;
begin
  if VK1.Users.Get(Users, [286400863, 415730216, VK1.UserId], TVkExtendedFields.AllForGroup) then
  try
    for var User in Users.Items do
    begin
      Memo1.Lines.Add('About: ' + User.About);
      Memo1.Lines.Add('BirthDate: ' + User.BirthDate);
      Memo1.Lines.Add('Books: ' + User.Books);
      Memo1.Lines.Add('Domain: ' + User.Domain);
      Memo1.Lines.Add('FirstName: ' + User.FirstName);
      Memo1.Lines.Add('Movies: ' + User.Movies);
      Memo1.Lines.Add('------------');
    end;
    if Length(Users.Items) > 2 then
      Memo1.Lines.Add(Users.Items[2].ToJsonString);
    Memo1.Lines.Add('------------');
  finally
    Users.Free;
  end;
end;

procedure TFormMain.ButtonLongPollStartClick(Sender: TObject);
begin
  VkUserEvents1.Start;
end;

procedure TFormMain.ButtonLongPollStopClick(Sender: TObject);
begin
  VkUserEvents1.Stop;
end;

procedure TFormMain.ButtonGLongPollStartClick(Sender: TObject);
begin
  VkGroupEventsController1.Start;
end;

procedure TFormMain.ButtonGLongPollStopClick(Sender: TObject);
begin
  VkGroupEventsController1.Stop;
end;

procedure TFormMain.ButtonStatusGetClick(Sender: TObject);
var
  Status: TVkStatus;
begin
  if Vk1.Status.Get(Status) then
  try
    Memo1.Lines.Add(Status.Text);
    if Assigned(Status.Audio) then
      Memo1.Lines.Add(Status.Audio.Artist + ' ' + Status.Audio.Title + ', ' + Status.Audio.Url);
  finally
    Status.Free;
  end
  else
    Memo1.Lines.Add('Error');
end;

procedure TFormMain.ButtonStatusSetClick(Sender: TObject);
begin
  if VK1.Status.&Set('Test22') then
    Memo1.Lines.Add('Status set')
  else
    Memo1.Lines.Add('Status not set');
end;

procedure TFormMain.ButtonWallPostClick(Sender: TObject);
var
  Params: TVkParamsWallPost;
  Status: Boolean;
  Photos: TVkPhotos;
  Attach: TAttachment;
begin
  Status := VK1.Photos.UploadForGroupWall(Photos, 145962568,
    ['D:\Мультимедиа\Картинки\Аниме\anime-wallpaper-1366x768 (100).jpg',
    'D:\Мультимедиа\Картинки\Аниме\anime-wallpaper-1366x768 (149).jpg']);
  //VK1.Wall.Post('', -145962568, TAttachment.Video(58553419, 456239240));

  Attach := 'http://habrahabr.ru';
  if Status then
  try
    //.Attachments('doc533494309_58553419'); //TAttachment.Doc(533494309, 58553419, '657138cd5d7842ae0a')
    VK1.Wall.Post(Params
      .Message('Test Text')
      .OwnerId(-145962568)
      .FromGroup(True)
      .Signed(True)
      .Attachments([Attach] + Photos.ToAttachments));
  finally
    Photos.Free;
  end;
end;

procedure TFormMain.ButtonAccountBanClick(Sender: TObject);
var
  Status: Boolean;
begin
  if VK1.Account.Ban(Status, -1) and Status then
    Memo1.Lines.Add('Banned')
  else
    Memo1.Lines.Add('Error banned');
end;

procedure TFormMain.ButtonUploadAudioMessageClick(Sender: TObject);
var
 // Url, Response: string;
  Doc: TVkDocSaved;
begin
  if Vk1.Docs.SaveAudioMessage(Doc, '1.ogg', 'Тестовая аудиозапись', '') then
  try
    Memo1.Lines.Add(Doc.&Type);
    Memo1.Lines.Add(Doc.AudioMessage.LinkOgg);
    Memo1.Lines.Add(Doc.AudioMessage.ToAttachment);
  finally
    Doc.Free;
  end
  else
    Memo1.Lines.Add('Error ');
  //or
  //
  {if VK1.Docs.GetMessagesUploadServer(Url, dutAudioMessage) then
  begin
    if VK1.Upload(Url, ['1.ogg'], Response) then
    begin
      if VK1.Docs.Save(Doc, Response, 'Тестовая аудиозапись', '') then
      try
        Memo1.Lines.Add(Doc.&Type);
        Memo1.Lines.Add(Doc.AudioMessage.LinkOgg);
        Memo1.Lines.Add(Doc.AudioMessage.ToAttachment);
      finally
        Doc.Free;
      end;
    end
    else
    begin
      Memo1.Lines.Add('Error ' + Response);
    end;
  end;   }
end;

procedure TFormMain.ButtonAudioGetClick(Sender: TObject);
var
  List: TVkAudios;
  Audio: TVkAudio;
begin     {
  if VK1.Audio.GetById(Audio, 280015709, 456247586) then
  try

  finally
    Audio.Free;
  end;  }

  if VK1.Audio.Get(List, TVkParamsAudioGet.Create.OwnerId(280015709).AudioId(456247586)) then
  try
    for Audio in List.Items do
    begin
      Memo1.Lines.Add(
        Audio.Artist + ' - ' +
        Audio.Title + BoolToString(Audio.ContentRestricted > 0, ' - аудиозапись не доступна', ''));
    end;
  finally
    List.Free;
  end
  else
    Memo1.Lines.Add('Error get audios');
end;

procedure TFormMain.ButtonBoardCreateCommentClick(Sender: TObject);
begin
  if VK1.Board.CreateComment(145962568, 39960452, 'Мой комментарий') then
    Memo1.Lines.Add('Комментарий добавлен')
  else
    Memo1.Lines.Add('Комментарий НЕ добавлен');
end;

procedure TFormMain.ButtonBoardGetBoardClick(Sender: TObject);
var
  Items: TVkBoardTopics;
  Params: TVkParamsBoardGet;
begin
  if VK1.Board.GetTopics(Items, Params.GroupId(145962568).Extended) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Title + ' - ' + Item.Comments.ToString);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonAudioGetAlbumsClick(Sender: TObject);
var
  List: TVkPlaylists;
begin
  if VK1.Audio.GetPlaylists(List, 415730216) then
  try
    for var Item in List.Items do
    begin
      Memo1.Lines.Add(
        Item.Title + '-' +
        Item.Description +
        ' Playlist Type: ' + Item.AlbumType);
    end;
  finally
    List.Free;
  end
  else
    Memo1.Lines.Add('Error GetPlaylists');
end;

procedure TFormMain.ButtonAudioGetRecomsClick(Sender: TObject);
var
  List: TVkAudios;
begin
  if VK1.Audio.GetRecommendations(List) then
  try
    for var Item in List.Items do
    begin
      Memo1.Lines.Add(
        Item.Artist + '-' +
        Item.Title + ' ' +
        BoolToString(Item.ContentRestricted > 0,
        ' - аудиозапись не доступна', ''));
    end;
  finally
    List.Free;
  end
  else
    Memo1.Lines.Add('Error GetRecommendations');
end;

procedure TFormMain.ButtonGroupsGetMembersClick(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Users: TVkProfiles;
      TS, TSEnd: Cardinal;
      Param: TVkParamsGroupsGetMembers;
    begin
      TS := GetTickCount;
      if VK1.Groups.GetMembers(Users, Param.GroupId('fcarsenaltula')) then
      try
        TSEnd := GetTickCount;
        TThread.Synchronize(nil,
          procedure
          begin
            Memo1.Lines.Add('done ' + Users.Count.ToString);
            Memo1.Lines.Add('time ' + ((TSEnd - TS) / 1000).ToString);
            for var User in Users.Items do
              Memo1.Lines.Add(User.FirstName + ' ' + User.LastName);
          end);
      finally
        Users.Free;
      end;
    end).Start;
end;

procedure TFormMain.ButtonUploadAudioClick(Sender: TObject);
var
  Audio: TVkAudio;
begin
  if VK1.Audio.Upload(Audio, 'D:\Мультимедиа\Музыка\ТГК\Триагрутрика - Вечерний Челябинск (2012)\06. Блэк дэй.mp3') then
  try
    Memo1.Lines.Add(Audio.Title);
  finally
    Audio.Free;
  end;
end;

procedure TFormMain.ButtonMarketFilterCategoriesClick(Sender: TObject);
begin
  var Items: TVkMarketCategories;
  var Params: TVkParamsMarketFilter;
  if VK1.Market.FilterCategories(Items, Params) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Name);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonMesGetConvClick(Sender: TObject);
var
  List: TVkConversationItems;
  Params: TVkParamsConversationsGet;
begin
  Params.Offset(0);
  Params.Extended;
  if VK1.Messages.GetConversations(List, Params) then
  try
    Memo1.Lines.Add('Count: ' + List.Count.ToString);
    for var Item in List.Items do
    begin
      if Assigned(Item.Conversation) then
      begin
        if Assigned(Item.Conversation.Peer) then
          Memo1.Lines.Add('Тип беседы: ' + Item.Conversation.Peer.&Type.ToString);
        if Item.Conversation.IsChat then
        begin
          if Assigned(Item.Conversation.ChatSettings) then
            Memo1.Lines.Add('Название беседы: ' + Item.Conversation.ChatSettings.Title);
        end
        else
        begin
          if Assigned(Item.Conversation.Peer) then
            Memo1.Lines.Add('Собеседник: ' + Item.Conversation.Peer.Id.ToString);
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TFormMain.ButtonFriendsGetClick(Sender: TObject);
var
  Users: TVkProfiles;
begin
  if VK1.Friends.Get(Users, TVkExtendedFields.All) then
  try
    for var User in Users.Items do
      Memo1.Lines.Add(User.FullName);
  finally
    Users.Free;
  end;
end;

procedure TFormMain.ButtonAudioGetPopClick(Sender: TObject);
var
  List: TVkAudios;
begin
  if VK1.Audio.GetPopular(List) then
  try
    for var Item in List.Items do
      Memo1.Lines.Add(Item.Artist + '-' + Item.Title);
  finally
    List.Free;
  end;
end;

procedure TFormMain.ButtonAccountUnbanClick(Sender: TObject);
begin
  if VK1.Account.UnBan(-1) then
    Memo1.Lines.Add('Unbanned')
  else
    Memo1.Lines.Add('Error unbanned');
end;

procedure TFormMain.ButtonAudioGetChartClick(Sender: TObject);
var
  Chart: TVkSectionData;
begin
  if VK1.Catalog.GetChart(Chart) then
  try
    for var Audio in Chart.Audios do
    begin
      if Assigned(Audio.AudioChartInfo) then
        Memo1.Lines.Add(
          Audio.AudioChartInfo.Position.ToString + ' - ' +
          Audio.Artist + ' - ' +
          Audio.Title);
    end;
  finally
    Chart.Free;
  end;
end;

procedure TFormMain.ButtonAccountActiveOffersClick(Sender: TObject);
var
  Offers: TVkActiveOffers;
begin
  if VK1.Account.GetActiveOffers(Offers, 0) then
  try
    Memo1.Lines.Add('ActiveOffers ' + Offers.Count.ToString);
    for var Offer in Offers.Items do
    begin
      Memo1.Lines.Add('--');
      Memo1.Lines.Add(Offer.Description);
    end;
  finally
    Offers.Free;
  end;
end;

procedure TFormMain.Button40Click(Sender: TObject);
var
  PI: TVkProfileInfo;
begin
  if VK1.Account.GetProfileInfo(PI) then
  try
    Memo1.Lines.Add(PI.ToJsonString);
  finally
    PI.Free;
  end
  else
    Memo1.Lines.Add('error');
end;

procedure TFormMain.ButtonAuthSingupClick(Sender: TObject);
var
  St: TVkAuthSignup;
  Params: TVkParamsSignup;
begin
  Params := Params.
    FirstName('Антон').
    LastName('Антонов').
    ClientId(VK1.AppID).
    ClientSecret(VK1.AppKey).
    Phone('+79530021369').
    Password('123qweASD').
    Sex(TVkSex.Male).
    Birthday(StrToDate('22.03.1991'));
  if VK1.Auth.Signup(St, Params) then
  try
    Memo1.Lines.Add(St.ToJsonString);
  finally
    St.Free;
  end;
end;

procedure TFormMain.ButtonGroupsGetByIdClick(Sender: TObject);
var
  Items: TVkGroups;
begin
  if Vk1.Groups.GetById(Items, '18cams', TVkExtendedFields.All) then
  try
    if not Items.IsEmpty then
      Memo1.Lines.Add(Items.Groups[0].Name);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonGroupsGetClick(Sender: TObject);
var
  Items: TVkGroups;
  Params: TVkParamsGroupsGet;
begin
  if Vk1.Groups.Get(Items, Params.Fields(TVkExtendedFields.All)) then
  try
    if not Items.IsEmpty then
      Memo1.Lines.Add(Items.Groups[0].Name);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonAudioGetCountClick(Sender: TObject);
var
  Count: Integer;
begin
  if VK1.Audio.GetCount(Count, -172534590) then
    Memo1.Lines.Add(Count.ToString)
  else
    Memo1.Lines.Add('error');
end;

procedure TFormMain.ButtonGetFriendWithAudioClick(Sender: TObject);
var
  Response: TResponse;
  Items: TVkProfiles;
begin
  Response := VK1.Execute(Format(TFile.ReadAllText('GetFriendsWithAudio.js'), [20, 0]));
  if Response.GetObject(Items) then
  try
    Memo1.Lines.Add(Items.Count.ToString);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonGetMessageByIdClick(Sender: TObject);
begin
  var Items: TVkMessages;
  var Params: TVkParamsMessageGet;
  Params.MessageId(840128);
  if VK1.Messages.GetById(Items, Params) then
  try
    if Length(Items.Items) > 0 then
      Memo1.Lines.Add(Items.Items[0].Text);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonMesGetHistoryClick(Sender: TObject);
var
  Items: TVkMessageHistory;
  Params: TVkParamsMessageHistory;
begin
  Params := Params.
    PeerId(-145962568).
    Extended(True);
  if VK1.Messages.GetHistory(Items, Params) then
  try
    for var Message in Items.Items do
      for var Attachment in Message.Attachments do
      begin
        Memo1.Lines.Add(Attachment.&Type.ToString);
        Memo1.Lines.Add(Attachment.ToJsonString);
      end;
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonMessageGetChatClick(Sender: TObject);
begin
  var Items: TVkChats;
  var Params: TVkParamsMessageGetChat;
  Params := Params.ChatId(175);
  if VK1.Messages.GetChat(Items, Params) then
  try
    if Length(Items.Items) > 0 then
      Memo1.Lines.Add(Items.Items[0].Photo50);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonMessageGetConverstionClick(Sender: TObject);
begin
  var Items: TVkConversations;
  var Params: TVkParamsConversationsGetById;
  Params.PeerId(2000000175);
  if VK1.Messages.GetConversationsById(Items, Params) then
  try
    if Length(Items.Items) > 0 then
      Memo1.Lines.Add(Items.Items[0].Style);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.Button47Click(Sender: TObject);
begin
  TFile.Delete('token.tmp');
  VK1.Token := '';
  DeleteCache('vk.com');
end;

procedure TFormMain.ButtonNewsfeedGetClick(Sender: TObject);
var
  Items: TVkNews;
begin
  if VK1.Newsfeed.Get(Items) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Text + ' ' + Item.&Type.ToString);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.Button49Click(Sender: TObject);
{var
  Keys: TVkKeyboard;
  k: string; }
begin
  //Keys := TVkKeyboard.Create;
  //Keys.AddButtonLine;
  //Keys.AddButtonToLine(0, TVkKeyboardButton.CreateText('Нажми', '{\"button\": \"2\"}'));
  //k := '{"buttons":[[{"action":{"label":"test","payload":"{\"button\": \"2\"}","type":"text"},"color":"positive"}]],"one_time":false}';
  //try
  //  //VK1.Messages.New.UserId(58553419).GroupId(145962568).Message('hi').Keyboard(Keys).Send.Free;
  //  VK1.Messages.SetActivity(TVkMessageActivity.Typing, 0, '58553419', 145962568);
  //  Sleep(5 * 1000);
  //  VK1.Messages.New.UserId(58553419).GroupId(145962568).Message('hi').Keyboard(k).Send.Free;
  //finally
  //  Keys.Free;
  //end;  }
end;

procedure TFormMain.ButtonAccountAppPremClick(Sender: TObject);
var
  Perm: Integer;
begin
  if VK1.Account.GetAppPermissions(Perm, 58553419) then
    Memo1.Lines.Add(Perm.ToString);
end;

procedure TFormMain.ButtonAccountCountersClick(Sender: TObject);
var
  Counters: TVkCounters;
begin
  if VK1.Account.GetCounters(Counters) then
  try
    Memo1.Lines.Add('messages ' + Counters.Messages.ToString);
    Memo1.Lines.Add('notifications ' + Counters.Notifications.ToString);
  finally
    Counters.Free;
  end;
end;

procedure TFormMain.ButtonAccountPushSettingsClick(Sender: TObject);
var
  PushSettings: TVkPushSettings;
begin
  if VK1.Account.GetPushSettings(PushSettings, '1') then
  try
    Memo1.Lines.Add('disabled ' + PushSettings.Disabled.ToString);
    if Assigned(PushSettings.Conversations) then
      Memo1.Lines.Add('conversations ' + PushSettings.Conversations.Count.ToString);
  finally
    PushSettings.Free;
  end;
end;

procedure TFormMain.ButtonAccountSaveProfileInfoClick(Sender: TObject);
var
  Response: TVkAccountInfoRequest;
  Info: TVkParamsProfileInfo;
begin
  if VK1.Account.SaveProfileInfo(Info.Status('test123'), Response) then
  try
    Memo1.Lines.Add(Response.Status);
  finally
    Response.Free;
  end;
end;

procedure TFormMain.ButtonCallMethodClick(Sender: TObject);
begin
  //VK1.CallMethod('audio.getById', [['audios', '444273385_456239099,444273385_456239107']],
  VK1.CallMethod('audio.search', [['q', 'Noize']],
    procedure(Respone: TResponse)
    begin
      Memo1.Lines.Add(Respone.ResponseText);
    end);
end;

procedure TFormMain.ButtonAccountOnlineClick(Sender: TObject);
begin
  if VK1.Account.SetOnline then
    Memo1.Lines.Add('online')
  else
    Memo1.Lines.Add('Error online');
end;

procedure TFormMain.ButtonCreatePlaylistClick(Sender: TObject);
var
  Playlist: TVkAudioPlaylist;
begin
  if VK1.Audio.CreatePlaylist(Playlist, VK1.UserId, 'Новый плейлист 2021') then
  try
    Memo1.Lines.Add(Playlist.ToJsonString);
  finally
    Playlist.Free;
  end;
end;

procedure TFormMain.ButtonEditPlaylistClick(Sender: TObject);
var
  Params: TVkParamsAudioEditPlaylist;
begin
  //Params := Params.OwnerId(VK1.UserId);
  if VK1.Audio.EditPlaylist(Params.
    PlaylistId(11).
    AudioIds(['58553419_456239116']))
    then
  begin
    Memo1.Lines.Add('ok');
  end
  else
    Memo1.Lines.Add('error');
end;

procedure TFormMain.ButtonAudioAddToPlaylistClick(Sender: TObject);
var
  Items: TVkAudioInfoItems;
begin
  if VK1.Audio.AddToPlaylist(Items, VK1.UserId, 11, ['58553419_456239101']) then
  try
    Memo1.Lines.Add(Items.ToJsonString);
  finally
    Items.Free;
  end
  else
    Memo1.Lines.Add('error');
end;

procedure TFormMain.ButtonPollsGetBGClick(Sender: TObject);
var
  Items: TVkPollBackgrounds;
begin
  if VK1.Polls.GetBackgrounds(Items) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Name);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonPodcastsSearchClick(Sender: TObject);
var
  Items: TVkPodcastSearch;
begin
  if VK1.Podcasts.Search(Items, 'гонки') then
  try
    for var Podcast in Items.Podcasts do
      Memo1.Lines.Add(Podcast.OwnerTitle);
    for var Episode in Items.Episodes do
      Memo1.Lines.Add(Episode.Title);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonSearchGetHintsClick(Sender: TObject);
var
  Items: TVkSearchItems;
begin
  if VK1.Search.GetHints(Items, 'vk') then
  try
    for var Item in Items.Items do
    begin
      Memo1.Lines.Add(Item.Description);
      Memo1.Lines.Add(Item.Section.ToString);
      Memo1.Lines.Add(Item.&Type);
    end;
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonDBGetRegionsClick(Sender: TObject);
var
  Items: TVkRegions;
begin
  if VK1.Database.GetRegions(Items, 1, 'Алтай') then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Title);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonDBGetSchoolClassesClick(Sender: TObject);
var
  Items: TVkSchoolClasses;
begin
  if VK1.Database.GetSchoolClasses(Items) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.Text);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonStorageGetClick(Sender: TObject);
var
  Value: string;
begin
  if VK1.Storage.Get(Value, 'name') then
    Memo1.Lines.Add(Value);
end;

procedure TFormMain.ButtonSecureGetBalanceClick(Sender: TObject);
var
  Value: Integer;
begin
  if VK1.Secure.GetAppBalance(Value) then
    Memo1.Lines.Add(Value.ToString);
end;

procedure TFormMain.ButtonStoriesGetClick(Sender: TObject);
var
  Items: TVkStoriesBlock;
begin
  if VK1.Stories.Get(Items) then
  try
    for var Block in Items.Items do
    begin
      Memo1.Lines.Add(Block.&Type);
      if Block.IsStories then
        for var Storie in Block.Stories do
          Memo1.Lines.Add(Storie.Id.ToString);
      if Block.IsCommunityGroupedStories then
        for var Group in Block.Grouped do
          Memo1.Lines.Add(Group.&Type);
    end;
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonAdsGetAccountsClick(Sender: TObject);
var
  Items: TVkAdsAccounts;
begin
  if VK1.Ads.GetAccounts(Items) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.ToJsonString);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.ButtonGetCatalogClick(Sender: TObject);
var
  Catalog: TVkAudioCatalog;
begin
  if VK1.Audio.GetCatalog(Catalog) then
  try
    for var CatalogItem in Catalog.Items do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add(CatalogItem.&Type);
      if Length(CatalogItem.Audios) > 0 then
      begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('AUDIOS');
        for var Audio in CatalogItem.Audios do
          Memo1.Lines.Add(Audio.Artist + ' - ' + Audio.Title);
      end;
      if Length(CatalogItem.Playlists) > 0 then
      begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('PLAYLISTS');
        for var Playlist in CatalogItem.Playlists do
          Memo1.Lines.Add(Playlist.Title + ' - ' + Playlist.Description);
      end;
      if Length(CatalogItem.Items) > 0 then
      begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('ITEMS');
        for var Link in CatalogItem.Items do
          Memo1.Lines.Add(Link.Title + ' - ' + Link.Subtitle);
      end;
    end;
  finally
    Catalog.Free;
  end;
end;

procedure TFormMain.ButtonLoginClick(Sender: TObject);
begin
  LabelLogin.Caption := 'Logging...';
  VK1.Login;
end;

procedure TFormMain.ButtonMesSendToPeerClick(Sender: TObject);
var
  Item: Integer;
begin
  if VK1.Messages.SendToPeer(Item, 2000000001, 'Hello') then
    Memo1.Lines.Add('ok');
end;

procedure TFormMain.ButtonSendAudioMessageClick(Sender: TObject);
begin
  VK1.Messages.SendAudioMessage(2000000001, '1.ogg');
end;

procedure TFormMain.ButtonPhotosGetAlbumClick(Sender: TObject);
begin
  VK1.Walk(
    function(Offset: Integer; var Cancel: Boolean): Integer
    var
      Items: TVkPhotos;
      Params: TVkParamsPhotosGet;
    begin
      Result := 0;
      Params := Params.
        OwnerId(-34109479).
        AlbumId(171648968).
        Offset(Offset).
        Count(100);
      if VK1.Photos.Get(Items, Params) then
      try
        Result := Length(Items.Items);
        for var Item in Items.Items do
        begin
          VK1.DownloadFile(Item.Sizes.GetSizeMaxSum.Url, 'D:\Temp\Photos\' + Item.Id.ToString + '.jpg');
          Memo1.Lines.Add(Item.Sizes.GetSizeMax.Url);
        end;
      finally
        Items.Free;
      end;
    end, 100);
end;

procedure TFormMain.ButtonSendPhotoClick(Sender: TObject);
begin
  VK1.Messages.New.UserId(58553419).AddPhotos(['D:\Downloads\6q8q9f.gif']).Send;
end;

procedure TFormMain.ButtonVideoDeleteClick(Sender: TObject);
begin
  Vk1.Video.Delete(456239072, -184755622);
end;

procedure TFormMain.ButtonWallGetClick(Sender: TObject);
var
  Items: TVkPosts;
  Params: TVkParamsWallGet;
begin
  if VK1.Wall.Get(Items, Params.OwnerId(Vk1.UserId)) then
  try
    for var Item in Items.Items do
      Memo1.Lines.Add(Item.ToJsonString);
  finally
    Items.Free;
  end;
end;

procedure TFormMain.CategoryButtons1ButtonClicked(Sender: TObject; const Button: TButtonItem);
begin
  MemoCode.Text := TRttiMethod(Button.Data).ToString;
end;

procedure TFormMain.FillMethods;
var
  Context: TRTTIContext;
begin
  CategoryButtons1.Categories.Clear;

  var GetParams :=
    function(const M: TRttiMethod): string
    begin
      Result := '';
      for var Param in M.GetParameters do
        Result := Result + Param.Name + ', ';
      Result := Result.Trim([',', ' ']);
    end;

  for var Prop in Context.GetType(TVK).GetProperties do
    if Prop.PropertyType.BaseType = Context.GetType(TVkController) then
      with CategoryButtons1.Categories.Add do
      begin
        Caption := Prop.Name;
        for var Method in Prop.PropertyType.GetDeclaredMethods do
          with Items.Add do
          begin
            Caption := Method.Name + ' (' + GetParams(Method) + ')';
            Data := Method;
          end;
        Collapsed := True;
      end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FillMethods;
  //Это мои данные AppID, AppKey, ServiceKey, эту строчку нужно убрать
  //{$INCLUDE app_cred.inc}  //Моё приложение
  //VK1.SetProxy('177.22.24.246', 3128);
  VK1.Application := TVkApplicationData.VKAdmin;
  if TFile.Exists('token.tmp') then
    VK1.Token := TFile.ReadAllText('token.tmp');
  //VK1.Token := '7317a7a252ba1180322eb675d541c32a23117564e028f91e69d2024be99afd29b318173eb2f207779feae';
  //VK1.Token := 'vk1.a.6bospdnUgd3UWpZLANXxOjWbwcuJ6fN1CJdYaut6jgNX0_tBC4SME9O30r8ghEULds3pYnxgR1NjFeTz0Iw_UwCUK2fIC0M3tswjQcPq73yiR-xzPrtHmiyUrQXNZryF4X3LG-X8yzhrR86x_9TmV4sM7rlHnb4okrFAm--RgpUCwWLNDxq0SnOtzbDOjIpU';
end;

procedure TFormMain.TabSheetAuthResize(Sender: TObject);
begin
  Label2.AutoSize := False;
  Label2.AutoSize := True;
  Label3.AutoSize := False;
  Label3.AutoSize := True;
end;

{
procedure TFormMain.VK1Auth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var
  ChangePasswordHash: string);
begin
  //Token := 'd45g6534f6gfsdfygvjcv6y90856j34vvvx98t3jfsd29i43j34fsdvxcvf59tjd35';
end;}

procedure TFormMain.VK1Auth(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
begin
  if FToken.IsEmpty then
  begin
    TFormOAuth2.Execute(Url,
      procedure(Form: TFormOAuth2)
      begin
        FToken := Form.Token;
        FTokenExpiry := Form.TokenExpiry;
        FChangePasswordHash := Form.ChangePasswordHash;
        if not FToken.IsEmpty then
          VK1.Login
        else
        begin
          LabelLogin.Caption := 'login error';
          ShowMessage('Ошибка загрузки страницы авторизации');
        end;
      end);
  end
  else
  begin
    Token := FToken;
    TokenExpiry := FTokenExpiry;
  end;
end;

procedure TFormMain.VK1Error(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  ShowMessage('Ошибка: ' + Code.ToString + ' - ' + Text);
  MemoLog.Lines.Add('Ошибка: ' + Code.ToString + ' - ' + Text);
  if not VK1.IsLogin then
    LabelLogin.Caption := 'login error';
end;

procedure TFormMain.VK1Log(Sender: TObject; const Value: string);
begin
  MemoLog.Lines.Add('Log: ' + Value);
end;

procedure TFormMain.VK1Login(Sender: TObject);
begin
  TFile.WriteAllText('token.tmp', VK1.Token);
  LabelLogin.Caption := 'login success';
  Memo1.Lines.Add(VK1.Token);
end;

procedure TFormMain.VkGroupEventsController1AudioNew(Sender: TObject; GroupId: Integer; Audio: TVkAudio; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новоая аудиозапись в группе ' + GroupId.ToString + ' "' + Audio.Url + '" от ' +
    Audio.OwnerId.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к обсуждению удалён в группе ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" от ' +
    Info.OwnerId.ToString + ', обсуждение ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к обсуждению изменили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', обсуждение ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый комментарий к обсуждению в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', обсуждение ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к обсуждению восстановили в группе ' + GroupId.ToString + ' "' +
    Comment.Text + '" обсуждение ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupAppPayload(Sender: TObject; GroupId: Integer; Info: TVkAppPayload; const EventId: string);
begin
  Memo1.Lines.Add('Событие в VK Mini Apps в группе ' + GroupId.ToString + ' инициатор ' + Info.UserId.ToString
    + ', Payload: ' + Info.Payload);
end;

procedure TFormMain.VkGroupEventsController1GroupChangePhoto(Sender: TObject; GroupId: Integer; Changes: TVkGroupChangePhoto; const EventId: string);
begin
  Memo1.Lines.Add(
    'Изменение фото в группе ' + GroupId.ToString + ' инициатор ' + Changes.UserId.ToString + ' => ' + Changes.Photo.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupChangeSettings(Sender: TObject; GroupId: Integer; Changes: TVkGroupSettingsChange; const EventId: string);
begin
  Memo1.Lines.Add(
    'Изменение параметров в группе ' + GroupId.ToString + ' инициатор ' + Changes.UserId.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
begin
  Memo1.Lines.Add(
    'К группе присоединился ' + UserId.ToString + ' "' + JoinType.ToString + '"');
end;

procedure TFormMain.VkGroupEventsController1GroupLeave(Sender: TObject; GroupId, UserId: Integer; IsSelf: Boolean; const EventId: string);
begin
  Memo1.Lines.Add(
    'Группу покинул ' + UserId.ToString + ' "' + BoolToString(IsSelf, 'Сам покинул', 'Исключен') + '"');
end;

procedure TFormMain.VkGroupEventsController1GroupOfficersEdit(Sender: TObject; GroupId: Integer; Info: TVkGroupOfficersEdit; const EventId: string);
begin
  Memo1.Lines.Add(
    'Изменение в руководстве в группе ' + GroupId.ToString + ' ' + Info.UserId.ToString + ' => ' +
    Info.LevelNew.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupPayTransaction(Sender: TObject; GroupId: Integer; Info: TVkPayTransaction; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый платёж через VK Pay в группе ' + GroupId.ToString + ' от ' + Info.FromId.ToString + ', сумма ' + Info.Amount);
end;

procedure TFormMain.VkGroupEventsController1GroupPollVoteNew(Sender: TObject; GroupId: Integer; Info: TVkGroupPollVoteNew; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый голос в группе ' + GroupId.ToString + ', в опросе ' + Info.PollId.ToString + ', от ' +
    Info.UserId.ToString + ', ответ ' + Info.OptionId.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupUnhandledEvents(Sender: TObject; GroupId: Integer; const JSON: TJSONValue);
begin
  Memo1.Lines.Add(
    'Не обработанное событие ' + GroupId.ToString + ' ' + JSON.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к товару удалён в группе ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" от ' +
    Info.OwnerId.ToString + ', товар ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к товару изменили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', товар ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый комментарий к товару в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', товар ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к товару восстановили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" товар ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageAllow(Sender: TObject; GroupId, UserId: Integer; Key: string; const EventId: string);
begin
  Memo1.Lines.Add(
    'Пользователь подписался на сообщения в группе ' + GroupId.ToString + ': ' + UserId.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageDeny(Sender: TObject; GroupId, UserId: Integer; Key: string; const EventId: string);
begin
  Memo1.Lines.Add(
    'Пользователь отказался от подписки на сообщения в группе ' + GroupId.ToString + ': ' + UserId.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageEdit(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
begin
  Memo1.Lines.Add(
    'Редактирование сообщения в группе ' + GroupId.ToString + ': ' + Message.Text);
end;

procedure TFormMain.VkGroupEventsController1MessageNew(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
begin
  Memo1.Lines.Add('Новое сообщение в группе ' + GroupId.ToString + ': ' + Message.Text);
  Memo1.Lines.Add(Message.PeerId.ToString);
  if Message.Text = 'погода' then
    VK1.Messages.
      New.
      PeerId(Message.PeerId).
      Message('Тут типа я вам показываю прогноз погоды, ага. На улице 10 градусов').
      Send;
end;

procedure TFormMain.VkGroupEventsController1MessageReply(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
begin
  Memo1.Lines.Add(
    'Исходящее сообщение в группе ' + GroupId.ToString + ': ' + Message.Text);
end;

procedure TFormMain.VkGroupEventsController1MessageTypingState(Sender: TObject; GroupId, UserId: Integer; State: string; const EventId: string);
begin
  Memo1.Lines.Add(
    'Набирают сообщение в группе ' + GroupId.ToString + ': ' + UserId.ToString + ' ' + State);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к фото удалён в группе ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" от ' +
    Info.OwnerId.ToString + ', фото ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к фото изменили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', фото ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый комментарий к фото в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', фото ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к фото восстановили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" фото ' +
    Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoNew(Sender: TObject; GroupId: Integer; Photo: TVkPhoto; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новое фото в группе ' + GroupId.ToString + ' "' + Photo.Text + '" от ' + Photo.OwnerId.ToString);
end;

procedure TFormMain.VkGroupEventsController1UserBlock(Sender: TObject; GroupId: Integer; Info: TVkGroupUserBlock; const EventId: string);
begin
  Memo1.Lines.Add(
    'Заблокирован пользователь ' + Info.UserId.ToString + ' Причина: ' + Info.Reason.ToString);
end;

procedure TFormMain.VkGroupEventsController1UserUnBlock(Sender: TObject; GroupId: Integer; Info: TVkGroupUserUnBlock; const EventId: string);
begin
  Memo1.Lines.Add(
    'Разблокирован пользователь ' + Info.UserId.ToString +
    ' Причина: ' + BoolToString(Info.ByEndDate, 'Окончание времени блокировки', 'Вручную'));
end;

procedure TFormMain.VkGroupEventsController1VideoCommentDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к видео удалён в группе ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" от ' +
    Info.OwnerId.ToString + ', видео ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к видео изменили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', видео ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый комментарий к видео в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' +
    Comment.FromId.ToString + ', видео ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к видео восстановили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" видео ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoNew(Sender: TObject; GroupId: Integer; Video: TVkVideo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новое видео в группе ' + GroupId.ToString + ' "' + Video.Title + '" от ' + Video.OwnerId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallPostNew(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый пост в группе ' + GroupId.ToString + ': ' + Post.Text);
end;

procedure TFormMain.VkGroupEventsController1WallReplyDelete(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий удалён в группе ' + GroupId.ToString + ' "' + Info.Id.ToString + '" от ' + Info.DeleterId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyEdit(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий изменили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' + Comment.FromId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyNew(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Новый комментарий в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' + Comment.FromId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyRestore(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info: TVkObjectInfo; const EventId: string);
begin
  Memo1.Lines.Add(
    'Комментарий к посту восстановили в группе ' + GroupId.ToString + ' "' + Comment.Text + '" от ' + Comment.FromId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallRepost(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
begin
  Memo1.Lines.Add(
    'Репост записи в группе ' + GroupId.ToString + ' "' + Post.Text + '" от ' + Post.FromId.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeDialogFlags(Sender: TObject; DialogChangeData: TDialogChangeData);
begin
  Memo1.Lines.Add(
    'Изменение флагов диалога ' + DialogChangeData.PeerId.ToString + ': ' + DialogChangeData.ChangeType.ToString + ' ' +
    DialogChangeData.Flags.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeMessageFlags(Sender: TObject; MessageChangeData: TMessageChangeData);
begin
  Memo1.Lines.Add('Изменение флагов сообщения ' +
    MessageChangeData.MessageId.ToString + ': ' +
    MessageChangeData.ChangeType.ToString + ' ' +
    MessageChangeData.Flags.ToString +
    ' Чат: ' + MessageChangeData.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChanged(Sender: TObject; const ChatId: Integer; IsSelf: Boolean);
begin
  Memo1.Lines.Add(
    'Изменения в беседе ' + ChatId.ToString + ': ' + IsSelf.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChangeInfo(Sender: TObject; const PeerId: Integer; TypeId: TVkChatChangeInfoType; Info: Integer);
begin
  Memo1.Lines.Add(
    'Изменения в беседе ' + PeerId.ToString + ': ' + VkChatChangeInfoType[TypeId] + ' -> ' + Info.ToString);
end;

procedure TFormMain.VkUserEvents1CountChange(Sender: TObject; Count: Integer);
begin
  Memo1.Lines.Add(
    'Кол-во уведомлений ' + Count.ToString);
end;

procedure TFormMain.VkUserEvents1DeleteMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add(
    'Сообщения в чате ' + PeerId.ToString + ' удалены до ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1EditMessage(Sender: TObject; MessageData: TMessageData);
begin
  Memo1.Lines.Add(
    'Редактирование сообщения в чате ' + MessageData.PeerId.ToString + ': ' + MessageData.Text);
end;

procedure TFormMain.VkUserEvents1NewMessage(Sender: TObject; MessageData: TMessageData);
var
  Msg: TVkMessage;
begin
  Memo1.Lines.Add(
    'Новое сообщение в чате ' + MessageData.PeerId.ToString + ' ' + MessageData.MessageId.ToString +
    ' ' + MessageData.Flags.ToString + ': ' + MessageData.Text);

  if TVkMessageFlag.Outbox in MessageData.Flags then
  begin
    //Сообщение от нас
  end
  else
  begin
    //Сообщение от кого-то
    if Pos('хуй', AnsiLowerCase(MessageData.Text)) <> 0 then
    begin
      VK1.Messages.New.
        PeerId(MessageData.PeerId).
        Message('Ай яй, так материться').
        Attachment(TAttachment.Album(58553419, 234519653)).
        Send;
    end;

    if VK1.Messages.GetById(Msg, MessageData.MessageId) then
    try
      if Length(Msg.Attachments) > 0 then
      begin
        if Msg.Attachments[0].&Type = TVkAttachmentType.AudioMessage then
          VK1.Messages.New.
            PeerId(MessageData.PeerId).
            Message('Опять сука свои голосовые сообщения отправляете. Уббб блять').
            Send;
      end;
    finally
      Msg.Free;
    end;
  end;
end;

procedure TFormMain.VkUserEvents1NotifyChange(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer);
begin
  Memo1.Lines.Add(
    'Изменились настройки оповещения ' + PeerId.ToString + ' Звук -> ' + Sound.ToString + ' на срок ' + DisableUntil.ToString);
end;

procedure TFormMain.VkUserEvents1ReadMessages(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer);
begin
  if Incoming then
    Memo1.Lines.Add('Все входящие сообщения в чате - ' + PeerId.ToString + ' до ' + LocalId.ToString + ' прочтены')
  else
    Memo1.Lines.Add('Все исходящие сообщения в чате - ' + PeerId.ToString + ' до ' + LocalId.ToString + ' прочтены');
end;

procedure TFormMain.VkUserEvents1RecoverMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add(
    'Сообщения в чате ' + PeerId.ToString + ' восстановлены до ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1UnhandledEvents(Sender: TObject; const JSON: TJSONValue);
begin
  Memo1.Lines.Add(
    'Не обработанное событие ' + JSON.ToString);
end;

procedure TFormMain.VkUserEvents1UserCall(Sender: TObject; UserId, CallId: Integer);
begin
  Memo1.Lines.Add(
    'Пользователь ' + UserId.ToString + ' совершил звонок с идентификатором ' + CallId.ToString);
end;

procedure TFormMain.VkUserEvents1UserOffline(Sender: TObject; UserId: Integer; InactiveUser: Boolean; TimeStamp: TDateTime);
begin
  Memo1.Lines.Add(
    'Оффлайн - ' + UserId.ToString + ' ' + VkUserActive[InactiveUser]);
end;

procedure TFormMain.VkUserEvents1UserOnline(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp: TDateTime);
begin
  Memo1.Lines.Add(
    'Онлайн - ' + UserId.ToString + ' ' + VkPlatforms[VkPlatform]);
end;

procedure TFormMain.VkUserEvents1UsersRecording(Sender: TObject; Data: TChatRecordingData);
begin
  Memo1.Lines.Add(
    'Несколько пользователей записывают аудиосообщение ' + Data.UserIds.ToString + ' в чате ' + Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UsersTyping(Sender: TObject; Data: TChatTypingData);
begin
  Memo1.Lines.Add(
    'Несколько пользователей набирают текст ' + Data.UserIds.ToString + ' в чате ' + Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UserTyping(Sender: TObject; UserId, ChatId: Integer);
begin
  Memo1.Lines.Add(
    'Пользователь набирает текст ' + UserId.ToString + ' в чате ' + ChatId.ToString);
end;

end.

