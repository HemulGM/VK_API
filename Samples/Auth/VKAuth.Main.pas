unit VKAuth.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VK.API, VK.Components, VK.Types, Vcl.ExtCtrls,
  VK.Handler, Vcl.StdCtrls, System.Generics.Defaults, Vcl.ComCtrls, VK.UserEvents, VK.GroupEvents,
  VK.Entity.Media, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  VK.Entity.Message, VK.Entity.ClientInfo, VK.Entity.Video, VK.Entity.Photo, VK.Entity.Audio,
  VK.Entity.GroupSettings;

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
    Button21: TButton;
    Button22: TButton;
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
    procedure VK1Login(Sender: TObject);
    procedure VK1Log(Sender: TObject; const Value: string);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
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
    procedure VkGroupEventsController1WallRepost(Sender: TObject; GroupId: Integer; Post: TVkPost; EventId: string);
    procedure VkGroupEventsController1VideoNew(Sender: TObject; GroupId: Integer; Video: TVkVideo; EventId: string);
    procedure VkGroupEventsController1VideoCommentRestore(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1PhotoCommentRestore(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1MarketCommentRestore(Sender: TObject; GroupId: Integer;
      Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1VideoCommentNew(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1PhotoCommentNew(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1MarketCommentNew(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1VideoCommentEdit(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1PhotoCommentEdit(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1MarketCommentEdit(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1BoardPostRestore(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1BoardPostEdit(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1BoardPostNew(Sender: TObject; GroupId: Integer; Comment:
      TVkComment; Info: TVkObjectInfo; EventId: string);
    procedure VkGroupEventsController1VideoCommentDelete(Sender: TObject; GroupId: Integer; Info:
      TVkCommentInfo; EventId: string);
    procedure VkGroupEventsController1PhotoCommentDelete(Sender: TObject; GroupId: Integer; Info:
      TVkCommentInfo; EventId: string);
    procedure VkGroupEventsController1MarketCommentDelete(Sender: TObject; GroupId: Integer; Info:
      TVkCommentInfo; EventId: string);
    procedure VkGroupEventsController1BoardPostDelete(Sender: TObject; GroupId: Integer; Info:
      TVkCommentInfo; EventId: string);
    procedure VkGroupEventsController1PhotoNew(Sender: TObject; GroupId: Integer; Photo: TVkPhoto; EventId: string);
    procedure VkGroupEventsController1AudioNew(Sender: TObject; GroupId: Integer; Audio: TVkAudio; EventId: string);
    procedure VkGroupEventsController1GroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType:
      TGroupJoinType; EventId: string);
    procedure VkGroupEventsController1GroupLeave(Sender: TObject; GroupId, UserId: Integer; IsSelf:
      Boolean; EventId: string);
    procedure VkGroupEventsController1UserBlock(Sender: TObject; GroupId: Integer; Info:
      TVkGroupUserBlock; EventId: string);
    procedure VkGroupEventsController1UserUnBlock(Sender: TObject; GroupId: Integer; Info:
      TVkGroupUserUnBlock; EventId: string);
    procedure VkGroupEventsController1MessageReply(Sender: TObject; GroupId: Integer; Message:
      TVkMessage; EventId: string);
    procedure VkGroupEventsController1MessageEdit(Sender: TObject; GroupId: Integer; Message:
      TVkMessage; EventId: string);
    procedure VkGroupEventsController1MessageDeny(Sender: TObject; GroupId, UserId: Integer; Key, EventId: string);
    procedure VkGroupEventsController1MessageAllow(Sender: TObject; GroupId, UserId: Integer; Key, EventId: string);
    procedure VkGroupEventsController1GroupPollVoteNew(Sender: TObject; GroupId: Integer; Info:
      TVkGroupPollVoteNew; EventId: string);
    procedure VkGroupEventsController1GroupOfficersEdit(Sender: TObject; GroupId: Integer; Info:
      TVkGroupOfficersEdit; EventId: string);
    procedure VkGroupEventsController1GroupChangeSettings(Sender: TObject; GroupId: Integer; Changes:
      TVkGroupSettingsChange; EventId: string);
    procedure VkGroupEventsController1GroupChangePhoto(Sender: TObject; GroupId: Integer; Changes:
      TVkGroupChangePhoto; EventId: string);
    procedure VkGroupEventsController1GroupAppPayload(Sender: TObject; GroupId: Integer; Info:
      TVkAppPayload; EventId: string);
    procedure VkGroupEventsController1GroupPayTransaction(Sender: TObject; GroupId: Integer; Info:
      TVkPayTransaction; EventId: string);
    procedure VkGroupEventsController1MessageTypingState(Sender: TObject; GroupId, UserId: Integer;
      State, EventId: string);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure VK1Error(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure VK1ErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure VK1Auth(Sender: TObject; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
  private
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  VK.Entity.AccountInfo, VK.Entity.ProfileInfo, VK.Entity.ActiveOffers, VK.Entity.Counters,
  VK.Entity.PushSettings, VK.Entity.User, VK.Entity.Keyboard, VK.Status, VK.Wall, VK.Docs,
  VK.Entity.Doc.Save, VK.Utils, VK.Account, VK.Entity.AccountInfoRequest, VK.OAuth2;

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
      if VK1.Docs.Save(Doc, Response, '�������� �����������', '') then
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

procedure TFormMain.Button21Click(Sender: TObject);
var
  List: TVkAudios;
  i: Integer;
begin
  if VK1.Audio.Get(List, 415730216) then
  begin
    for i := Low(List) to High(List) do
    begin
      Memo1.Lines.Add(List[i].artist + '-' + List[i].title + ' URL: ' + List[i].Url);
      List[i].Free;
    end;
  end
  else
    Memo1.Lines.Add('Error unbanned');
end;

procedure TFormMain.Button22Click(Sender: TObject);
begin
  if VK1.Board.CreateComment(145962568, 39960452, '��� �����������') then
    Memo1.Lines.Add('����������� ��������')
  else
    Memo1.Lines.Add('����������� �� ��������');
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
  Response: TVkAccountInfoRequest;
  Info: TVkProfileInfoParams;
begin
  Info.Status('test123');
  if VK1.Account.SaveProfileInfo(Info, Response) then
  begin
    Memo1.Lines.Add(Response.status);
    Response.Free;
  end;
end;

procedure TFormMain.Button8Click(Sender: TObject);
begin
  //audio.get
  //audio.search,q
  {
  "id": 456239099,
                "owner_id": 444273385,
  }
  //VK1.CallMethod('audio.getById', [['audios', '444273385_456239099,444273385_456239107']],
  VK1.CallMethod('audio.get', [['owner_id', '415730216']],
    procedure(Respone: TResponse)
    begin
      Memo1.Lines.Add(Respone.Response);
    end);
end;

procedure TFormMain.Button9Click(Sender: TObject);
begin
  if VK1.Account.SetOnline() then
    Memo1.Lines.Add('online')
  else
    Memo1.Lines.Add('Error online');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  //��� ��� ������ AppID, AppKey, ServiceKey, ��� ������� ����� ������
  {$INCLUDE app_cred.inc}  //�� ����������
  VK1.Login;
end;

procedure TFormMain.VK1Auth(Sender: TObject; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string);
begin
  //���� �������� ���� �����, �� ����������� ����������� �� �����, �.�. ����� ��� ����
  //��� ������������� ������� OAuth2 ����������� ���������� ������ ���� �����
  //��� ��� �����, ��� ������� ����� ������
  {$INCLUDE vk_admin.inc}  //vk admin
  //{$INCLUDE delphi_live.inc}  //delphi live
end;

procedure TFormMain.VK1Error(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  ShowMessage('������: ' + Code.ToString + ' - ' + Text);
  Memo1.Lines.Add('������: ' + Code.ToString + ' - ' + Text);
end;

procedure TFormMain.VK1ErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  MemoLog.Lines.Add('������ �����������: ' + Code.ToString + ' - ' + Text);
  LabelLogin.Caption := 'login error';
end;

procedure TFormMain.VK1Log(Sender: TObject; const Value: string);
begin
  MemoLog.Lines.Add('Log: ' + Value);
end;

procedure TFormMain.VK1Login(Sender: TObject);
begin
  LabelLogin.Caption := 'login success';
end;

procedure TFormMain.VkGroupEventsController1AudioNew(Sender: TObject; GroupId: Integer; Audio:
  TVkAudio; EventId: string);
begin
  Memo1.Lines.Add('������ ����������� � ������ ' + GroupId.ToString + ' "' + Audio.url + '" �� ' +
    Audio.owner_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostDelete(Sender: TObject; GroupId: Integer; Info:
  TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���������� ����� � ������ ' + GroupId.ToString + ' "' + Info.ObjectId.ToString
    + '" �� ' +
    Info.OwnerId.ToString + ', ���������� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostEdit(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���������� �������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ���������� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostNew(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ����������� � ���������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ���������� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1BoardPostRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���������� ������������ � ������ ' + GroupId.ToString + ' "' +
    Comment.text + '" ���������� ' +
    Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupAppPayload(Sender: TObject; GroupId: Integer; Info:
  TVkAppPayload; EventId: string);
begin
  Memo1.Lines.Add('������� � VK Mini Apps � ������ ' + GroupId.ToString + ' ��������� ' + Info.UserId.ToString
    + ', Payload: ' + Info.Payload);
end;

procedure TFormMain.VkGroupEventsController1GroupChangePhoto(Sender: TObject; GroupId: Integer;
  Changes: TVkGroupChangePhoto; EventId: string);
begin
  Memo1.Lines.Add('��������� ���� � ������ ' + GroupId.ToString + ' ��������� ' + Changes.UserId.ToString
    + ' => ' + Changes.Photo.id.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupChangeSettings(Sender: TObject; GroupId: Integer;
  Changes: TVkGroupSettingsChange; EventId: string);
begin
  Memo1.Lines.Add('��������� ���������� � ������ ' + GroupId.ToString + ' ��������� ' + Changes.user_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupJoin(Sender: TObject; GroupId, UserId: Integer;
  JoinType: TGroupJoinType; EventId: string);
begin
  Memo1.Lines.Add('� ������ ������������� ' + UserId.ToString + ' "' + JoinType.ToString + '"');
end;

procedure TFormMain.VkGroupEventsController1GroupLeave(Sender: TObject; GroupId, UserId: Integer;
  IsSelf: Boolean; EventId: string);
begin
  Memo1.Lines.Add('������ ������� ' + UserId.ToString + ' "' + BoolToString(IsSelf, '��� �������', '��������') + '"');
end;

procedure TFormMain.VkGroupEventsController1GroupOfficersEdit(Sender: TObject; GroupId: Integer;
  Info: TVkGroupOfficersEdit; EventId: string);
begin
  Memo1.Lines.Add('��������� � ����������� � ������ ' + GroupId.ToString + ' ' + Info.UserId.ToString + ' => ' +
    Info.LevelNew.ToString);
end;

procedure TFormMain.VkGroupEventsController1GroupPayTransaction(Sender: TObject; GroupId: Integer;
  Info: TVkPayTransaction; EventId: string);
begin
  Memo1.Lines.Add('����� ����� ����� VK Pay � ������ ' + GroupId.ToString + ' �� ' + Info.FromId.ToString +
    ', ����� ' + Info.Amount);
end;

procedure TFormMain.VkGroupEventsController1GroupPollVoteNew(Sender: TObject; GroupId: Integer; Info:
  TVkGroupPollVoteNew; EventId: string);
begin
  Memo1.Lines.Add('����� ����� � ������ ' + GroupId.ToString + ', � ������ ' + Info.PollId.ToString + ', �� ' +
    Info.UserId.ToString + ', ����� ' + Info.OptionId.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentDelete(Sender: TObject; GroupId: Integer;
  Info: TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ������ ����� � ������ ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" �� ' +
    Info.OwnerId.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentEdit(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ������ �������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentNew(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ����������� � ������ � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MarketCommentRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ������ ������������ � ������ ' + GroupId.ToString + ' "' + Comment.text + '" ����� ' +
    Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageAllow(Sender: TObject; GroupId, UserId: Integer;
  Key, EventId: string);
begin
  Memo1.Lines.Add('������������ ���������� �� ��������� � ������ ' + GroupId.ToString +
    ': ' + UserId.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageDeny(Sender: TObject; GroupId, UserId: Integer;
  Key, EventId: string);
begin
  Memo1.Lines.Add('������������ ��������� �� �������� �� ��������� � ������ ' + GroupId.ToString +
    ': ' + UserId.ToString);
end;

procedure TFormMain.VkGroupEventsController1MessageEdit(Sender: TObject; GroupId: Integer; Message:
  TVkMessage; EventId: string);
begin
  Memo1.Lines.Add('�������������� ��������� � ������ ' + GroupId.ToString + ': ' + Message.text);
end;

procedure TFormMain.VkGroupEventsController1MessageNew(Sender: TObject; GroupId: Integer; Message:
  TVkMessage; ClientInfo: TVkClientInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ��������� � ������ ' + GroupId.ToString + ': ' + Message.text);
end;

procedure TFormMain.VkGroupEventsController1MessageReply(Sender: TObject; GroupId: Integer; Message:
  TVkMessage; EventId: string);
begin
  Memo1.Lines.Add('��������� ��������� � ������ ' + GroupId.ToString + ': ' + Message.text);
end;

procedure TFormMain.VkGroupEventsController1MessageTypingState(Sender: TObject; GroupId, UserId:
  Integer; State, EventId: string);
begin
  Memo1.Lines.Add('�������� ��������� � ������ ' + GroupId.ToString + ': ' + UserId.ToString + ' ' + State);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentDelete(Sender: TObject; GroupId: Integer;
  Info: TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���� ����� � ������ ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" �� ' +
    Info.OwnerId.ToString + ', ���� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentEdit(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���� �������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ���� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentNew(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ����������� � ���� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ���� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoCommentRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ���� ������������ � ������ ' + GroupId.ToString + ' "' + Comment.text + '" ���� ' +
    Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1PhotoNew(Sender: TObject; GroupId: Integer; Photo:
  TVkPhoto; EventId: string);
begin
  Memo1.Lines.Add('����� ���� � ������ ' + GroupId.ToString + ' "' + Photo.text + '" �� ' +
    Photo.owner_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1UserBlock(Sender: TObject; GroupId: Integer; Info:
  TVkGroupUserBlock; EventId: string);
begin
  Memo1.Lines.Add('������������ ������������ ' + Info.UserId.ToString + ' �������: ' + Info.Reason.ToString);
end;

procedure TFormMain.VkGroupEventsController1UserUnBlock(Sender: TObject; GroupId: Integer; Info:
  TVkGroupUserUnBlock; EventId: string);
begin
  Memo1.Lines.Add('������������� ������������ ' + Info.UserId.ToString + ' �������: ' + BoolToString
    (Info.ByEndDate, '��������� ������� ����������', '�������'));
end;

procedure TFormMain.VkGroupEventsController1VideoCommentDelete(Sender: TObject; GroupId: Integer;
  Info: TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ����� ����� � ������ ' + GroupId.ToString + ' "' + Info.ObjectId.ToString + '" �� ' +
    Info.OwnerId.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentEdit(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ����� �������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentNew(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ����������� � ����� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString + ', ����� ' + Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoCommentRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ����� ������������ � ������ ' + GroupId.ToString + ' "' + Comment.text + '" ����� ' +
    Info.Id.ToString);
end;

procedure TFormMain.VkGroupEventsController1VideoNew(Sender: TObject; GroupId: Integer; Video:
  TVkVideo; EventId: string);
begin
  Memo1.Lines.Add('����� ����� � ������ ' + GroupId.ToString + ' "' + Video.title + '" �� ' +
    Video.owner_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallPostNew(Sender: TObject; GroupId: Integer; Post:
  TVkPost; EventId: string);
begin
  Memo1.Lines.Add('����� ���� � ������ ' + GroupId.ToString + ': ' + Post.text);
end;

procedure TFormMain.VkGroupEventsController1WallReplyDelete(Sender: TObject; GroupId: Integer; Info:
  TVkCommentInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� ����� � ������ ' + GroupId.ToString + ' "' + Info.Id.ToString + '" �� ' +
    Info.DeleterId.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyEdit(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� �������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyNew(Sender: TObject; GroupId: Integer; Comment:
  TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����� ����������� � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallReplyRestore(Sender: TObject; GroupId: Integer;
  Comment: TVkComment; Info: TVkObjectInfo; EventId: string);
begin
  Memo1.Lines.Add('����������� � ����� ������������ � ������ ' + GroupId.ToString + ' "' + Comment.text + '" �� ' +
    Comment.from_id.ToString);
end;

procedure TFormMain.VkGroupEventsController1WallRepost(Sender: TObject; GroupId: Integer; Post:
  TVkPost; EventId: string);
begin
  Memo1.Lines.Add('������ ������ � ������ ' + GroupId.ToString + ' "' + Post.text + '" �� ' +
    Post.from_id.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeDialogFlags(Sender: TObject; DialogChangeData: TDialogChangeData);
begin
  Memo1.Lines.Add('��������� ������ ������� ' +
    DialogChangeData.PeerId.ToString + ': ' +
    DialogChangeData.ChangeType.ToString + ' ' +
    DialogChangeData.Flags.ToString);
end;

procedure TFormMain.VkUserEvents1ChangeMessageFlags(Sender: TObject; MessageChangeData: TMessageChangeData);
begin
  Memo1.Lines.Add('��������� ������ ��������� ' +
    MessageChangeData.MessageId.ToString + ': ' +
    MessageChangeData.ChangeType.ToString + ' ' +
    MessageChangeData.Flags.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChanged(Sender: TObject; const ChatId: Integer; IsSelf: Boolean);
begin
  Memo1.Lines.Add('��������� � ������ ' + ChatId.ToString + ': ' + IsSelf.ToString);
end;

procedure TFormMain.VkUserEvents1ChatChangeInfo(Sender: TObject; const PeerId: Integer; TypeId:
  TChatChangeInfoType; Info: Integer);
begin
  Memo1.Lines.Add('��������� � ������ ' + PeerId.ToString + ': ' + TypeId.ToString + ' -> ' + Info.ToString);
end;

procedure TFormMain.VkUserEvents1CountChange(Sender: TObject; Count: Integer);
begin
  Memo1.Lines.Add('���-�� ����������� ' + Count.ToString);
end;

procedure TFormMain.VkUserEvents1DeleteMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add('��������� � ���� ' + PeerId.ToString + ' ������� �� ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1EditMessage(Sender: TObject; MessageData: TMessageData);
begin
  Memo1.Lines.Add('�������������� ��������� � ���� ' + MessageData.PeerId.ToString + ': ' + MessageData.Text);
end;

procedure TFormMain.VkUserEvents1NewMessage(Sender: TObject; MessageData: TMessageData);
var
  Msg: TVkMessage;
begin
  Memo1.Lines.Add('����� ��������� � ���� ' + MessageData.PeerId.ToString + ' ' + MessageData.MessageId.ToString
    + ' ' + MessageData.Flags.ToString
    + ': ' + MessageData.Text);
  if VK1.Messages.GetById(MessageData.MessageId, Msg) then
  begin
    Memo1.Lines.Add(Msg.text);
    Msg.Free;
  end;
  if mfOutbox in MessageData.Flags then
  begin
    //��������� �� ���
  end
  else
  begin
    //��������� �� ����-��
    if Pos('���', AnsiLowerCase(MessageData.Text)) <> 0 then
    begin
      VK1.Messages.Send
        .PeerId(MessageData.PeerId)
        .Message('�� ��, ��� ����������')
        .Attachemt(['album58553419_234519653'])
        .Send.Free;
    end;
  end;
end;

procedure TFormMain.VkUserEvents1NotifyChange(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer);
begin
  Memo1.Lines.Add('���������� ��������� ���������� ' + PeerId.ToString + ' ���� -> ' + Sound.ToString
    + ' �� ���� ' + DisableUntil.ToString);
end;

procedure TFormMain.VkUserEvents1ReadMessages(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer);
begin
  case Incoming of
    True:
      Memo1.Lines.Add('��� �������� ��������� � ���� - ' + PeerId.ToString + ' �� ' + LocalId.ToString + ' ��������');
    False:
      Memo1.Lines.Add('��� ��������� ��������� � ���� - ' + PeerId.ToString + ' �� ' + LocalId.ToString + ' ��������');
  end;
end;

procedure TFormMain.VkUserEvents1RecoverMessages(Sender: TObject; PeerId, LocalId: Integer);
begin
  Memo1.Lines.Add('��������� � ���� ' + PeerId.ToString + ' ������������� �� ' + LocalId.ToString);
end;

procedure TFormMain.VkUserEvents1UserCall(Sender: TObject; UserId, CallId: Integer);
begin
  Memo1.Lines.Add('������������ ' + UserId.ToString + ' �������� ������ � ��������������� ' + CallId.ToString);
end;

procedure TFormMain.VkUserEvents1UserOffline(Sender: TObject; UserId: Integer; InactiveUser: Boolean;
  TimeStamp: TDateTime);
begin
  Memo1.Lines.Add('������� - ' + UserId.ToString + ' ' + VkUserActive[InactiveUser]);
end;

procedure TFormMain.VkUserEvents1UserOnline(Sender: TObject; UserId: Integer; VkPlatform:
  TVkPlatform; TimeStamp: TDateTime);
begin
  Memo1.Lines.Add('������ - ' + UserId.ToString + ' ' + VkPlatforms[VkPlatform]);
end;

procedure TFormMain.VkUserEvents1UsersRecording(Sender: TObject; Data: TChatRecordingData);
begin
  Memo1.Lines.Add('��������� ������������� ���������� �������������� ' + Data.UserIds.ToString +
    ' � ���� ' + Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UsersTyping(Sender: TObject; Data: TChatTypingData);
begin
  Memo1.Lines.Add('��������� ������������� �������� ����� ' + Data.UserIds.ToString + ' � ���� ' +
    Data.PeerId.ToString);
end;

procedure TFormMain.VkUserEvents1UserTyping(Sender: TObject; UserId, ChatId: Integer);
begin
  Memo1.Lines.Add('������������ �������� ����� ' + UserId.ToString + ' � ���� ' + ChatId.ToString);
end;

end.

