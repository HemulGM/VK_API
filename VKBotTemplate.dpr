program VKBotTemplate;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  System.StrUtils,
  Vcl.Forms,
  VK.API,
  VK.Components,
  VK.Entity.Message,
  VK.Entity.ClientInfo,
  VK.Bot,
  VK.Types;

procedure AddLine(Text: string; AColor: ShortInt = 0); overload;
begin
  if AColor = 0 then
    AColor := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), AColor);
  Writeln(Text);
end;

procedure AddText(Text: string; AColor: ShortInt = 0); overload;
begin
  if AColor = 0 then
    AColor := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), AColor);
  Write(Text);
end;

procedure AddLine(AItems: array of string; AColor: ShortInt); overload;
var
  Str, AText: string;
begin
  for Str in AItems do
    AText := AText + ', ' + Str;
  Delete(AText, 1, 2);
  AddLine(AText, AColor);
end;

begin
  try
    //Найстрока бота
    TVkBotChat.GetInstance<TVkBotChat>.OnInit :=
      procedure(Bot: TVkBot)
      begin
        AddText('Initializate...');

        Bot.VK.Token := ''; //Укажите токен
        Bot.GroupId := 0;  //Укажите ид группы бота
      end;

    //Событие при закрытии бота
    TVkBotChat.GetInstance<TVkBotChat>.OnDestroy :=
      procedure(Bot: TVkBot)
      begin
        //DoSomething
      end;

    //Событие при возникновении ошибки
    TVkBotChat.GetInstance<TVkBotChat>.OnError :=
      procedure(Bot: TVkBot; E: Exception; Code: Integer; Text: string)
      begin
        AddLine(Text, FOREGROUND_RED);
      end;

    //Событие нового сообщения
    TVkBotChat.GetInstance<TVkBotChat>.OnMessage :=
      procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo)
      var
        NewMessage: TVkMessage;
      begin
        NewMessage := TVkMessage.FromJsonString(Message.ToJsonString);
        AddLine(['Сообщение', NewMessage.PeerId.ToString, NewMessage.FromId.ToString, IfThen(NewMessage.Text.IsEmpty,
          '<вложение>', NewMessage.Text)], FOREGROUND_BLUE or FOREGROUND_GREEN);

        TThread.CreateAnonymousThread(
          procedure
          begin
            try
              try
                //DoSomething with NewMessage
              finally
                NewMessage.Free;
              end;
            except
            end;
          end).Start;
      end;

    //Событие редактирования сообщения
    TVkBotChat.GetInstance<TVkBotChat>.OnMessageEdit :=
      procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage)
      var
        NewMessage: TVkMessage;
      begin
        NewMessage := TVkMessage.FromJsonString(Message.ToJsonString);
        AddLine(['Редактирование', NewMessage.PeerId.ToString, NewMessage.FromId.ToString, IfThen(NewMessage.Text.IsEmpty,
          '<вложение>', NewMessage.Text)], FOREGROUND_BLUE);

        TThread.CreateAnonymousThread(
          procedure
          begin
            try
              try
                //DoSomething with NewMessage
              finally
                NewMessage.Free;
              end;
            except
            end;
          end).Start;
      end;

    //Запуск бота
    if TVkBot.GetInstance<TVkBotChat>.Run then
      AddLine('Done!', FOREGROUND_GREEN);

    //Рабочий цикл
    while not Application.Terminated do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Application.DoApplicationIdle;
    end;

    //Освобождение бота
    TVkBot.GetInstance<TVkBotChat>.Free;
  except
    on E: Exception do
    begin
      AddLine([E.ClassName, E.Message], FOREGROUND_RED);
      Readln;
    end;
  end;
end.

