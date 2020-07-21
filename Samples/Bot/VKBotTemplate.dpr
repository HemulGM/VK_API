program VKBotTemplate;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.StrUtils,
  VK.Bot,
  VK.Types,
  VK.Bot.Utils,
  VK.Entity.Message,
  VK.Entity.ClientInfo;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    with TVkBotChat.GetInstance do
    begin
      Token := 'cfd849da38b35fd5182fed5fb3254877c98edc414907cc0e861c8ddbb925650ff2c81c19c96a145eb1194'; //Bot Token
      GroupId := 145962568;  //Bot Group Id

      OnMessage :=
        procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo)
        begin
          Console.AddLine(['Message', Message.PeerId.ToString, Message.ConversationMessageId.ToString,
            IfThen(Message.Text.IsEmpty, '<attach>', Message.Text)], BLUE or GREEN);
          //DoSomething with NewMessage async
          if Message.Action.&Type = 'chat_invite_user' then
            Bot.API.Messages.SendToPeer(Message.PeerId, 'Welcome')
          else
            Bot.API.Messages.SendToPeer(Message.PeerId, '=)');
        end;

      OnMessageEdit :=
        procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage)
        begin
          Console.AddLine(['Message', Message.PeerId.ToString, Message.ConversationMessageId.ToString,
            IfThen(Message.Text.IsEmpty, '<attach>', Message.Text)], BLUE or GREEN);
          //DoSomething with EditMessage async
          Bot.API.Messages.SendToPeer(Message.PeerId, '=|');
        end;

      OnError :=
        procedure(Bot: TVkBot; E: Exception; Code: Integer; Text: string)
        begin
          Console.AddLine(['Error:', Text, Code.ToString], RED);
        end;

      if Init and Run then
      begin
        Console.Run(
          procedure(Command: string; var Quit: Boolean)
          begin
            Quit := Command = 'exit';
          end);
      end;

      Free;
    end;
  except
    on E: Exception do
    begin
      Console.AddLineWait([E.ClassName, E.Message], RED);
    end;
  end;
end.

