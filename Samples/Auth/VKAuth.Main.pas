unit VKAuth.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VK.API, VK.Components, VK.Types, Vcl.ExtCtrls,
  VK.Handler, Vcl.StdCtrls, System.Generics.Defaults, Vcl.ComCtrls, VK.UserEvents, VK.GroupEvents,
  VK.Entity.Media, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  VK.Entity.Message, VK.Entity.ClientInfo;

type
  TFormMain = class(TForm)
    VK1: TVK;
    Panel1: TPanel;
    LabelLogin: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Panel2: TPanel;
    Memo1: TMemo;
    MemoLog: TMemo;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    VkUserEvents1: TVkUserEvents;
    VkGroupEventsController1: TVkGroupEventsController;
    VkGroupEvents1: TVkGroupEvents;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure VK1Error(Sender: TObject; Code: Integer; Text: string);
    procedure VK1Login(Sender: TObject);
    procedure VK1Log(Sender: TObject; const Value: string);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure VK1Auth(Sender: TObject; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
    procedure VK1ErrorLogin(Sender: TObject; Code: Integer; Text: string);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure VkUserEvents1UserOnline(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp: TDateTime);
    procedure VkUserEvents1RecoverMessages(Sender: TObject; PeerId, LocalId: Integer);
    procedure VkUserEvents1ChangeDialogFlags(Sender: TObject; DialogChangeData: TDialogChangeData);
    procedure VkUserEvents1ChangeMessageFlags(Sender: TObject; MessageChangeData: TMessageChangeData);
    procedure VkUserEvents1ChatChanged(Sender: TObject; const ChatId: Integer; IsSelf: Boolean);
    procedure VkUserEvents1ChatChangeInfo(Sender: TObject; const PeerId: Integer; TypeId:
      TChatChangeInfoType; Info: Integer);
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
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure VkGroupEventsController1MessageNew(Sender: TObject; GroupId: Integer; Message:
      TVkMessage; ClientInfo: TVkClientInfo; EventId: string);
    procedure VkGroupEventsController1WallReplyRestore(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1WallReplyNew(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1WallReplyEdit(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1WallReplyDelete(Sender: TObject; GroupId: Integer; Info:
      TVkCommentInfo; EventId: string);
    procedure VkGroupEventsController1WallPostNew(Sender: TObject; GroupId: Integer; Post: TVkPost; EventId: string);
  private
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  VK.Entity.AccountInfo, VK.Entity.ProfileInfo, VK.Entity.ActiveOffers, VK.Entity.Counters,
  VK.Entity.PushSettings, VK.Structs, VK.Entity.User, VK.Entity.Keyboard, VK.Status, VK.Wall,
  VK.Docs, VK.Entity.Doc.Save;

{$R *.dfm}

procedure TFormMain.Button10Click(Sender: TObject);
begin
  if VK1.Account.SetOffline then
    Memo1.Lines.Add('offline')
  else
    Memo1.Lines.Add('Error offline');
end;

procedure TFormMain.Button11Click(Sender: TObject);
begin
  if VK1.Auth.CheckPhone('+79512202849', True) then
    Memo1.Lines.Add('CheckPhone')
  else
    Memo1.Lines.Add('Error CheckPhone');
end;

procedure TFormMain.Button12Click(Sender: TObject);
var
  Users: TVkUsers;
  i: Integer;
begin
  if VK1.Users.Get(Users, '286400863,415730216', UserFieldsAll, '') then
  begin
    for i := Low(Users.Items) to High(Users.Items) do
    begin
      Memo1.Lines.Add('about: ' + Users.Items[i].about);
      Memo1.Lines.Add('bdate: ' + Users.Items[i].bdate);
      Memo1.Lines.Add('books: ' + Users.Items[i].books);
      Memo1.Lines.Add('domain: ' + Users.Items[i].domain);
      Memo1.Lines.Add('first_name: ' + Users.Items[i].first_name);
      Memo1.Lines.Add('movies: ' + Users.Items[i].movies);
      Memo1.Lines.Add('------------');
    end;
    Users.Free;
  end;
end;

procedure TFormMain.Button13Click(Sender: TObject);
begin
//  Vk1.UserLongPollServerStart;
  VkUserEvents1.Start;
end;

procedure TFormMain.Button14Click(Sender: TObject);
begin
  VkUserEvents1.Stop;
end;

procedure TFormMain.Button15Click(Sender: TObject);
begin
  VkGroupEventsController1.Start;
end;

procedure TFormMain.Button16Click(Sender: TObject);
begin
  VkGroupEventsController1.Stop;
end;

procedure TFormMain.Button17Click(Sender: TObject);
var
  Status: TVkStatus;
begin
  if Vk1.Status.Get(Status) then
  begin
    Memo1.Lines.Add(Status.Text);
    if Assigned(Status.Audio) then
    begin
      Memo1.Lines.Add(Status.Audio.artist + ' ' + Status.Audio.title + ', ' + Status.Audio.url);
      Status.Audio.Free;
    end;
  end
  else
    Memo1.Lines.Add('Error');
end;

procedure TFormMain.Button18Click(Sender: TObject);
begin
  if VK1.Status.&Set('Test22') then
    Memo1.Lines.Add('Status set')
  else
    Memo1.Lines.Add('Status not set');
end;

procedure TFormMain.Button19Click(Sender: TObject);
var
  Params: TVkWallParams;
begin
//  VK1.Wall.Post('', -145962568, ['video58553419_456239240']);
  Params.Message('Test Text');
  Params.OwnerId(-145962568);
  Params.FromGroup(True);
  Params.Signed(True);
  Params.Attachments(['doc58553419_533494309_657138cd5d7842ae0a']);
  VK1.Wall.Post(Params);
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  if VK1.Account.Ban(-1) then
    Memo1.Lines.Add('Banned')
  else
    Memo1.Lines.Add('Error banned');
end;

procedure TFormMain.Button20Click(Sender: TObject);
var
  Url, Response: string;
  Doc: TVkDocSaved;
begin
  if VK1.Docs.GetMessagesUploadServer(Url, dutAudioMessage) then
  begin
    if VK1.Uploader.Upload(Url, '1.ogg', Response) then
    begin
      if VK1.Docs.Save(Doc, Response, 'Тестовая аудиозапись', '') then
      begin
        Memo1.Lines.Add(Doc.&type);
        Memo1.Lines.Add(Doc.audio_message.link_ogg);
        Memo1.Lines.Add(Doc.audio_message.ToAttachment);
        Doc.Free;
      end;
    end
    else
    begin
      Memo1.Lines.Add('Error ' + Response);
    end;
  end;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  if VK1.Account.UnBan(-1) then
    Memo1.Lines.Add('Unbanned')
  else
    Memo1.Lines.Add('Error unbanned');
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  Offers: TVkActiveOffers;
  i: Integer;
begin
  if VK1.Account.GetActiveOffers(Offers, 0) then
  begin
    Memo1.Lines.Add('ActiveOffers ' + Offers.count.ToString);
    for i := 0 to Length(Offers.items) - 1 do
    begin
      Memo1.Lines.Add('--');
      Memo1.Lines.Add(Offers.items[i].description);
    end;
    Offers.Free;
  end;
end;

procedure TFormMain.Button4Click(Sender: TObject);
var
  Perm: Int64;
begin
  if VK1.Account.GetAppPermissions(Perm, 58553419) then
    Memo1.Lines.Add(Perm.ToString);
end;

procedure TFormMain.Button5Click(Sender: TObject);
var
  Counters: TCountersClass;
begin
  if VK1.Account.GetCounters(Counters) then
  begin
    Memo1.Lines.Add('messages ' + Counters.messages.ToString);
    Memo1.Lines.Add('notifications ' + Counters.notifications.ToString);
    Counters.Free;
  end;
end;

procedure TFormMain.Button6Click(Sender: TObject);
var
  PushSettings: TVkPushSettings;
begin
  if VK1.Account.GetPushSettings(PushSettings, '1') then
  begin
    Memo1.Lines.Add('disabled ' + PushSettings.disabled.ToString);
    Memo1.Lines.Add('conversations ' + PushSettings.conversations.count.ToString);
    PushSettings.Free;
  end;
end;

procedure TFormMain.Button7Click(Sender: TObject);
var
  Response: TResponse;
  Info: TProfileInfoData;
begin
  Info.Clear;
  Info.status := 'test1';
  if VK1.Account.SaveProfileInfo(Info, Response) then
    Memo1.Lines.Add(Response.Response);
end;

procedure TFormMain.Button8Click(Sender: TObject);
begin
  VK1.CallMethod('account.getProfileInfo', [],
    procedure(Respone: TResponse)
    begin
      ShowMessage(Respone.Response);
    end);
end;

procedure TFormMain.Button9Click(Sender: TObject);
begin
  if VK1.Account.SetOnline() then
    Memo1.Lines.Add('online')
  else
    Memo1.Lines.Add('Error online');
end;

procedure TFormMain.VK1Auth(Sender: TObject; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
begin
  //Если определён этот метод, то авторизация происходить не будет, т.к. токен уже есть
  //Для использования обычной OAuth2 авторизации достаточно убрать этот метод
  //Это мой токен, эту строчку нужно убрать
  {$INCLUDE vk_admin.inc}  //vk admin
  //{$INCLUDE delphi_live.inc}  //delphi live
  TokenExpiry := 0;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  //Это мои данные AppID, AppKey, ServiceKey, эту строчку нужно убрать
  {$INCLUDE app_cred.inc}  //Моё приложение
  VK1.Login;
end;

procedure TFormMain.VK1Error(Sender: TObject; Code: Integer; Text: string);
begin
  if (not VK1.IsLogin) and (not VK1.UseServiceKeyOnly) then
  begin
    LabelLogin.Caption := 'login error';
    ShowMessage('Ошибка авторизации');
  end
  else
    ShowMessage('Ошибка: ' + Code.ToString + ' - ' + Text);
  Memo1.Lines.Add('Ошибка: ' + Code.ToString + ' - ' + Text);
end;

procedure TFormMain.VK1ErrorLogin(Sender: TObject; Code: Integer; Text: string);
begin
  MemoLog.Lines.Add('Ошибка авторизации: ' + Code.ToString + ' - ' + Text);
end;

procedure TFormMain.VK1Log(Sender: TObject; const Value: string);
begin
  MemoLog.Lines.Add('Log: ' + Value);
end;

procedure TFormMain.VK1Login(Sender: TObject);
begin
  LabelLogin.Caption := 'login success';
end;

procedure TFormMain.VkGroupEventsController1MessageNew(Sender: TObject; GroupId: Integer; Message:
  TVkMessage; ClientInfo: TVkClientInfo; EventId: string);
begin
  Memo1.Lines.Add(Message.text);
end;

procedure TFormMain.VkGroupEventsController1WallPostNew(Sender: TObject; GroupId: Integer; Post:
  TVkPost; EventId: string);
begin
  Memo1.Lines.Add('Новый пост в группе ' + GroupId.ToString + ': ' + Post.text);
end;

procedure TFormMain.VkGroupEventsController1WallReplyDelete(Sender: TObject; GroupId: Integer; Info:
  TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('Комментарий удалён в группе ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" от ' +
    Info.OwnerId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyEdit(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('Комментарий изменили в группе ' + GroupId.ToString + ' "' + Comment.text + '" от ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyNew(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('Новый комментарий в группе ' + GroupId.ToString + ' "' + Comment.text + '" от ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('Комментарий восстановили в группе ' + GroupId.ToString + ' "' + Comment.text + '" от ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeDialogFlags(Sender: TObject; DialogChangeData: TDialogChangeData);
begin
  Memo1.Lines.Add('Изменение флагов диалога ' +
    DialogChangeData.PeerId.ToString + ': ' +
    DialogChangeData.ChangeType.ToString + ' ' +
    DialogChangeData.Flags.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeMessageFlags(Sender: TObject; MessageChangeData: TMessageChangeData);
begin
  Memo1.Lines.Add('Изменение флагов сообщения ' +
    MessageChangeData.MessageId.ToString + ': ' +
    MessageChangeData.ChangeType.ToString + ' ' +
    MessageChangeData.Flags.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChanged(Sender: TObject; const ChatId: Integer; IsSelf: Boolean);
begin
  Memo1.Lines.Add('Изменения в беседе ' + ChatId.ToString + ': ' + IsSelf.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChangeInfo(Sender: TObject; const PeerId: Integer; TypeId:
  TChatChangeInfoType; Info: Integer);
begin
  Memo1.Lines.Add('Изменения в беседе ' + PeerId.ToString + ': ' + TypeId.ToString + ' -> ' + Info.ToString);
end;

procedure TFormMain.VkUserEvents1CountChange(Sender: TObject; Count: Integer);
begin
  Memo1.Lines.Add('Кол-во уведомлений ' + Count.ToString);
end;

procedure TFormMain.VkUserEvents1DeleteMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add('Сообщения в чате ' + PeerId.ToString + ' удалены до ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1EditMessage(Sender: TObject; MessageData: TMessageData);
begin
  Memo1.Lines.Add('Редактирование сообщения в чате ' + MessageData.PeerId.ToString + ': ' + MessageData.Text);
end;

procedure TFormMain.VkUserEvents1NewMessage(Sender: TObject; MessageData: TMessageData);
var
  Msg: TVkMessage;
begin
  Memo1.Lines.Add('Новое сообщение в чате ' + MessageData.PeerId.ToString + ' ' + MessageData.MessageId.ToString
    + ' ' + MessageData.Flags.ToString
    + ': ' + MessageData.Text);
  if VK1.Messages.GetById(MessageData.MessageId, Msg) then
  begin
    Memo1.Lines.Add(Msg.text);
    Msg.Free;
  end;
  if mfOutbox in MessageData.Flags then
  begin
    //Сообщение от нас
  end
  else
  begin
    //Сообщение от кого-то
    if Pos('хуй', AnsiLowerCase(MessageData.Text)) <> 0 then
    begin
      VK1.Messages.Send
        .PeerId(MessageData.PeerId)
        .Message('Ай яй, так материться')
        .Attachemt(['album58553419_234519653'])
        .Send.Free;
    end;
  end;
end;

procedure TFormMain.VkUserEvents1NotifyChange(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer);
begin
  Memo1.Lines.Add('Изменились настройки оповещения ' + PeerId.ToString + ' Звук -> ' + Sound.ToString
    + ' на срок ' + DisableUntil.ToString);
end;

procedure TFormMain.VkUserEvents1ReadMessages(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer);
begin
  case Incoming of
    True:
      Memo1.Lines.Add('Все входящие сообщения в чате - ' + PeerId.ToString + ' до ' + LocalId.ToString + ' прочтены');
    False:
      Memo1.Lines.Add('Все исходящие сообщения в чате - ' + PeerId.ToString + ' до ' + LocalId.ToString + ' прочтены');
  end;
end;

procedure TFormMain.VkUserEvents1RecoverMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add('Сообщения в чате ' + PeerId.ToString + ' восстановлены до ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1UserCall(Sender: TObject; UserId, CallId: Integer);
begin
  Memo1.Lines.Add('Пользователь ' + UserId.ToString + ' совершил звонок с идентификатором ' + CallId.ToString);
end;

procedure TFormMain.VkUserEvents1UserOffline(Sender: TObject; UserId: Integer; InactiveUser: Boolean;
  TimeStamp: TDateTime);
begin
  Memo1.Lines.Add('Оффлайн - ' + UserId.ToString + ' ' + VkUserActive[InactiveUser]);
end;

procedure TFormMain.VkUserEvents1UserOnline(Sender: TObject; UserId: Integer; VkPlatform:
  TVkPlatform; TimeStamp: TDateTime);
begin
  Memo1.Lines.Add('Онлайн - ' + UserId.ToString + ' ' + VkPlatforms[VkPlatform]);
end;

procedure TFormMain.VkUserEvents1UsersRecording(Sender: TObject; Data: TChatRecordingData);
begin
  Memo1.Lines.Add('Несколько пользователей записывают аудиосообщение ' + Data.UserIds.ToString +
    ' в чате ' + Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UsersTyping(Sender: TObject; Data: TChatTypingData);
begin
  Memo1.Lines.Add('Несколько пользователей набирают текст ' + Data.UserIds.ToString + ' в чате ' +
    Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UserTyping(Sender: TObject; UserId, ChatId: Integer);
begin
  Memo1.Lines.Add('Пользователь набирает текст ' + UserId.ToString + ' в чате ' + ChatId.ToString);
end;

end.

