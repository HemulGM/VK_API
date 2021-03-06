program VKBotTemplate;

{$APPTYPE CONSOLE}

uses
  VK.Bot,
  VK.Types,
  VK.Bot.Utils,
  VK.Entity.Message,
  VK.Entity.ClientInfo;

begin
  ReportMemoryLeaksOnShutdown := True;
  with TVkBotChat.GetInstance do
  begin
    Token := 'cfd849da38b35fd5182fed5fb3254877c98edc414907cc0e861c8ddbb925650ff2c81c19c96a145eb1194';
    GroupId := 145962568;

    OnMessage :=
      procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo)
      begin
        if PeerIdIsUser(Message.PeerId) then
        begin
          case Message.Action.&Type of
            TVkMessageActionType.ChatInviteUser:
              Bot.API.Messages.SendToPeer(Message.PeerId, 'Welcome');
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
    end;

    Free;
  end;
end.

