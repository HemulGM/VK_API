unit VK.LongPollServer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  REST.Client, System.JSON, System.Net.HttpClient, VK.Types, VK.Handler,
  System.Generics.Collections;

type
  TVkLongPollData = record
    Key: string;
    Wait: string;
    TS: string;
    Server: string;
    Version: string;
    Mode: string;
    Action: string;
    function Request: string;
  end;

  TVkLongPollServer = class
  private
    FThread: TThread;
    FLongPollNeedStop: Boolean;
    FLongPollStopped: Boolean;
    FLongPollData: TVkLongPollData;
    FParams: TParams;
    FMethod: string;
    FOnError: TOnVKError;
    FInterval: Integer;
    FGroupID: string;
    FOnUpdate: TOnLongPollServerUpdate;
    FHandler: TVkHandler;
    FLogging: Boolean;
    FDoSync: Boolean;
    function QueryLongPollServer: Boolean;
    procedure DoError(E: Exception);
    procedure OnLongPollRecieve(Updates: TJSONArray);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetInterval(const Value: Integer);
    procedure SetGroupID(const Value: string);
    procedure SetOnUpdate(const Value: TOnLongPollServerUpdate);
    procedure SetMethod(const Value: string);
    procedure SetParams(const Value: TParams);
    function GetIsWork: Boolean;
    procedure SetHandler(const Value: TVkHandler);
    procedure SetLogging(const Value: Boolean);
    procedure SetDoSync(const Value: Boolean);
  public
    function Start: Boolean;
    procedure Stop;
    constructor Create; overload;
    constructor Create(AClient: TRESTClient; AMethod: string; AParams: TParams); overload;
    destructor Destroy; override;
    property OnError: TOnVKError read FOnError write SetOnError;
    property Interval: Integer read FInterval write SetInterval;
    property GroupID: string read FGroupID write SetGroupID;
    property OnUpdate: TOnLongPollServerUpdate read FOnUpdate write SetOnUpdate;
    property Handler: TVkHandler read FHandler write SetHandler;
    property Method: string read FMethod write SetMethod;
    property Params: TParams read FParams write SetParams;
    property IsWork: Boolean read GetIsWork;
    property Logging: Boolean read FLogging write SetLogging;
    property Thread: TThread read FThread;
    property DoSync: Boolean read FDoSync write SetDoSync;
  end;

const
  DefaultLongPollServerInterval = 1000;

implementation

uses
  {$IFDEF NEEDFMX}
  FMX.Forms, FMX.Types,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  System.SyncObjs;

{ TLongPollServer }

constructor TVkLongPollServer.Create(AClient: TRESTClient; AMethod: string; AParams: TParams);
begin
  inherited Create;
  FDoSync := True;
  Method := AMethod;
  Params := AParams;
end;

constructor TVkLongPollServer.Create;
begin
  inherited;
  FDoSync := True;
  FLogging := False;
  FInterval := DefaultLongPollServerInterval;
end;

destructor TVkLongPollServer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TVkLongPollServer.DoError(E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, E, -10000, E.Message);
end;

function TVkLongPollServer.GetIsWork: Boolean;
begin
  Result := not FLongPollNeedStop;
end;

function TVkLongPollServer.QueryLongPollServer: Boolean;
var
  JSText: string;
  ResponseJSON: TJSONValue;
begin
  Result := False;
  //Выполняем запрос
  try
    with FHandler.Execute(FMethod, FParams) do
    begin
      ResponseJSON := GetJSONResponse;
      JSText := Response;
      Result := Success;
    end;
  except
    on E: Exception do
    begin
      DoError(TVkLongPollServerException.Create(E.Message + #13#10 + JSText));
      Exit;
    end;
  end;
  //Если запрос выполнен успешно
  if Result and Assigned(ResponseJSON) then
  begin
    try
      FLongPollData.Action := 'a_check';
      FLongPollData.Mode := '10';
      FLongPollData.Key := ResponseJSON.GetValue('key', '');
      FLongPollData.Server := ResponseJSON.GetValue('server', '');
      FLongPollData.TS := ResponseJSON.GetValue('ts', '');
      FLongPollData.Wait := '25';
      FLongPollData.Version := '3';
      Result := not FLongPollData.Server.IsEmpty;
    finally
      ResponseJSON.Free;
    end;
  end
  else
    Result := False;

  if not Result then
  begin
    DoError(TVkLongPollServerException.Create('QueryLongPollServer error '#13#10 + JSText));
  end;
end;

procedure TVkLongPollServer.OnLongPollRecieve(Updates: TJSONArray);
var
  i: Integer;
begin
  for i := 0 to Updates.Count - 1 do
  begin
    try
      if FLogging then
        FHandler.Log(Self, Updates.Items[i].ToString);

      FOnUpdate(Self, FGroupID, Updates.Items[i]);
    except
      on E: Exception do
        DoError(E);
    end;
  end;
end;

procedure TVkLongPollServer.SetDoSync(const Value: Boolean);
begin
  FDoSync := Value;
end;

procedure TVkLongPollServer.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
end;

procedure TVkLongPollServer.SetGroupID(const Value: string);
begin
  FGroupID := Value;
end;

procedure TVkLongPollServer.SetHandler(const Value: TVkHandler);
begin
  FHandler := Value;
end;

procedure TVkLongPollServer.SetInterval(const Value: Integer);
begin
  FInterval := Value;
end;

procedure TVkLongPollServer.SetMethod(const Value: string);
begin
  FMethod := Value;
end;

procedure TVkLongPollServer.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TVkLongPollServer.SetOnUpdate(const Value: TOnLongPollServerUpdate);
begin
  FOnUpdate := Value;
end;

procedure TVkLongPollServer.SetParams(const Value: TParams);
var
  Param: TParam;
begin
  FParams := Value;
  //Сохраним id группы, если его передали
  for Param in FParams do
    if Param[0] = 'group_id' then
      FGroupID := Param[1];
end;

function TVkLongPollServer.Start: Boolean;
begin
  Result := False;
  FLongPollNeedStop := False;
  //Проверяем, есть ли обработчик событий, иначе нафига нам лонгпул)
  if not Assigned(FOnUpdate) then
    raise Exception.Create('Необходимо обязательно указать обработчик входящих обновлений');
  //Подключаемся к серверу, получаем данные для запросов
  if not QueryLongPollServer then
    Exit;
  //Запускаем поток для выполнения запросов
  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      HTTP: THTTPClient;
      ReqCode: Integer;
      Stream: TStringStream;
      JSON: TJSONValue;
      Updates: TJSONArray;
    begin
      FLongPollStopped := False;
      HTTP := THTTPClient.Create;
      Stream := TStringStream.Create('', TEncoding.UTF8);
      try
        while (not TThread.Current.CheckTerminated) and (not FLongPollNeedStop) do
        begin
          //Очистим результат запроса с прошлого раза
          Stream.Clear;
          //Выполняем запрос
          ReqCode := 0;
          try
            ReqCode := HTTP.Get(FLongPollData.Request, Stream).StatusCode;
          except
            on E: Exception do
            begin
              TThread.ForceQueue(nil,
                procedure
                begin
                  DoError(TVkLongPollServerHTTPException.Create(E.Message));
                end);
            end;
          end;
          //Если пора останавливаться - выходим из цикла
          if FLongPollNeedStop then
            Break;
          //Если запрос выполнен успешно
          if ReqCode = 200 then
          begin
            //Парсим данные
            try
              Stream.Position := 0;
              JSON := TJSONObject.ParseJSONValue(Stream.DataString);
            except
              JSON := nil;
            end;
            //Если данные были получены
            if Assigned(JSON) then
            begin
              //Обновляем данные лонгпул сервера
              FLongPollData.TS := JSON.GetValue('ts', '');
              //Если пора останавливаться - выходим из цикла
              if FLongPollNeedStop then
                Break;
              //Пробуем получить список обновлений
              if JSON.TryGetValue<TJSONArray>('updates', Updates) then
              begin
                //Отдаем обработку обновлений в основной поток
                if FDoSync then
                begin
                  TThread.Synchronize(nil,
                    procedure
                    begin
                      if not FLongPollNeedStop then
                      begin
                        OnLongPollRecieve(Updates);
                      end;
                    end);
                end
                else
                begin
                  if not FLongPollNeedStop then
                  begin
                    OnLongPollRecieve(Updates);
                  end;
                end;
              end
              else //Ошибка при парсинге
              begin
                //Если ошибка, то пробуем переподключиться к лонгпул серверу
                if FDoSync then
                begin
                  TThread.Synchronize(nil,
                    procedure
                    begin
                      if not FLongPollNeedStop then
                      begin
                        if not QueryLongPollServer then
                        begin
                          DoError(TVkLongPollServerParseException.Create('QueryLongPollServer error, result: '
                            + Stream.DataString));
                        end;
                      end;
                    end);
                end
                else
                begin
                  if not FLongPollNeedStop then
                  begin
                    if not QueryLongPollServer then
                    begin
                      DoError(TVkLongPollServerParseException.Create('QueryLongPollServer error, result: '
                        + Stream.DataString));
                    end;
                  end;
                end;
              end;
              JSON.Free;
            end;
          end
          else
          begin
            //Если ошибка, то пробуем переподключиться к лонгпул серверу
            TThread.Synchronize(nil,
              procedure
              begin
                if not FLongPollNeedStop then
                begin
                  if not QueryLongPollServer then
                  begin
                    DoError(TVkLongPollServerParseException.Create('QueryLongPollServer error, result: '
                      + Stream.DataString));
                  end;
                end;
              end);
          end;
          //Интервал между запросами
          Sleep(FInterval);
        end;
      except
        on E: Exception do
          DoError(E);
      end;
      HTTP.Free;
      Stream.Free;
      FLongPollStopped := True;
    end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
  Result := True;
end;

procedure TVkLongPollServer.Stop;
begin
  FLongPollNeedStop := True;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    while not FLongPollStopped do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
    FThread.Free;
    FThread := nil;
  end
  else
    FLongPollNeedStop := False;
end;

{ TLongPollData }

function TVkLongPollData.Request: string;
begin
  Result := Server +
    '?act=' + Action +
    '&key=' + Key +
    '&mode=' + Mode +
    '&ts=' + TS +
    '&wait=' + Wait +
    '&version=' + Version;
  if Pos('http', Result) = 0 then
    Result := 'http://' + Result;
end;

end.

