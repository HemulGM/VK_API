program VKBotTemplate;

{$APPTYPE CONSOLE}

uses
  VK.Bot,
  VK.Types,
  VK.Bot.Utils,
  VK.Messages,
  VK.GroupEvents,
  VK.Entity.Message,
  VK.Entity.ClientInfo,
  VK.Entity.Media;

var
  VKBot: TVkBotChat;

type
  TMethods = class
    class procedure OnWallPostNew(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
    class procedure OnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
  end;

{ TMethods }

class procedure TMethods.OnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
begin

end;

class procedure TMethods.OnWallPostNew(Sender: TObject; GroupId: Integer; Post: TVkPost; const EventId: string);
var
  MessageId: Integer;
  Params: TVkParamsMessageSend;
begin
  Params.ChatId(2000002);
  Params.Message('Новый пост');
  VKBot.API.Messages.Send(MessageId, Params);
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  VKBot := TVkBotChat.GetInstance(192458090, '<token>');
  with VKBot do
  begin
    LongPoll.OnWallPostNew := TMethods.OnWallPostNew;

    AddListener<TOnGroupMessageNew>(TMethods.OnNewMessage);
    AddListener<TOnWallPostAction>(TMethods.OnWallPostNew);
    OnMessage :=
      procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo)
      begin
        if PeerIdIsUser(Message.PeerId) then
        begin
          if Assigned(Message.Action) then
            case Message.Action.&Type of
              TVkMessageActionType.ChatInviteUser:
                Bot.API.Messages.SendToPeer(Message.PeerId, 'Welcome');
            end
          else
          begin
            Bot.API.Messages.SendToPeer(Message.PeerId, 'Your message: ' + Message.Text);
          end;
        end;
      end;

    if Init and Run then
    begin
      Console.Run(
        procedure(const Command: string; var Quit: Boolean)
        begin
          Quit := Command = 'exit';
        end);
    end
    else
      Readln;

    Free;
  end;
end.

