unit VK.ExternalAuth;

interface

uses
  System.Classes;

type
  TOnExternalAuthWork = reference to procedure(var Cancel: Boolean);

  TOnExternalAuthFinish = reference to procedure(const Token: string);

var
  ServerPorts: TArray<Word> = [9991, 9992, 9993];

/// <summary>
/// ”становите удобные вам порты или оставьте стандартные.
/// ƒл€ работы этого метода, в настройках ¬  приложени€ требуетс€ установить "Ѕазовый домен" = "127.0.0.1" и
/// добавить несколько (в зависимости от кол-ва портов) "ƒоверенный redirect URI" в виде "http://127.0.0.1:9991/vkoauth".
/// “.е. дл€ стандартного поведение нужно добвать 3 доверенных редиректа дл€ каждого порта.
/// Ќесколько портов используетс€ дл€ нескольких попыток запустить сервер на указанном порте (может быть зан€т кем-то другим).
/// ¬ идеале будет использован только первый порт.
/// </summary>
procedure GetTokenExternal(AuthUrl: string; OnWork: TOnExternalAuthWork; OnFinish: TOnExternalAuthFinish);

implementation

uses
  System.SysUtils, System.Threading, HTTP.Server, VK.CommonUtils,
  Winapi.ShellAPI, Winapi.Windows;

procedure GetTokenExternal(AuthUrl: string; OnWork: TOnExternalAuthWork; OnFinish: TOnExternalAuthFinish);
begin
  TTask.Run(
    procedure
    begin
      var Server := THTTPServer.Create;
      var Token, ChangePasswordHash, TokenExpiry: string;
      try
        var Stop := False;
        var Success := False;
        Server.Route('/vkoauth',
          procedure(Request: TRequest; Response: TResponse)
          begin
            Response.ContentType := 'text/html';
            Response.ResponseNo := 200;
            Response.ResponseText :=
              '<!DOCTYPE html>' + #13#10 +
              '<html>' + #13#10 +
              '<head>' + #13#10 +
              '	<meta charset="utf-8">' + #13#10 +
              '	<meta name="viewport" content="width=device-width, initial-scale=1">' + #13#10 +
              '	<title></title>' + #13#10 +
              '</head>' + #13#10 +
              '<body>' + #13#10 +
              '	<script>' + #13#10 +
              '   window.open(document.location.origin + "/vktoken?" + document.location.hash.replace("#", ""), "_self")' +
              '	</script>' + #13#10 +
              '</body>' + #13#10 +
              '</html>';
          end);
        Server.Route('/vktoken',
          procedure(Request: TRequest; Response: TResponse)
          begin
            if Request.QueryParams.Contains('access_token') then
            begin
              for var i := 0 to Request.Params.Count - 1 do
              begin
                if Request.Params.KeyNames[i] = 'access_token' then
                  Token := Request.Params.ValueFromIndex[i]
                else if Request.Params.KeyNames[i] = 'expires_in' then
                  TokenExpiry := Request.Params.ValueFromIndex[i];
              end;
              Response.ResponseNo := 200;
              Response.ResponseText := 'ћожно закрыть страницу';
              Success := True;
            end;
          end);

        var Port := 0;
        var RunSuccess := False;
        for Port in ServerPorts do
        try
          Server.RunSync(Port);
          RunSuccess := True;
          Break;
        except
          Continue;
        end;
        if not RunSuccess then
          raise Exception.Create('Ќе удалось запустить сервер дл€ получени€ токена');
        AuthUrl := AuthUrl + '&redirect_uri=http://127.0.0.1:' + Port.ToString + '/vkoauth';
        ShellExecute(0, 'open', PChar(AuthUrl), nil, nil, SW_NORMAL);
        repeat
          Sleep(1);
          OnWork(Stop);
        until Stop or Success;
      finally
        Server.Free;
        TThread.Queue(nil,
          procedure
          begin
            OnFinish(Token);
          end);
      end;
    end);
end;

end.

