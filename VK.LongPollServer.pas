unit VK.LongPollServer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, Vcl.Dialogs, System.Classes, System.Variants,
  REST.Client, System.JSON, System.Net.HttpClient, VK.Types, System.Generics.Collections;

type
  TLongPollServer = class;

  TGroupLongPollServers = class(TList<TLongPollServer>)
    function Find(GroupID: string): Integer;
    procedure Clear;
  end;

  TLongPollData = record
    key: string;
    wait: string;
    ts: string;
    server: string;
    version: string;
    function Request: string;
  end;

  TLongPollServer = class
  private
    FThread: TThread;
    FLongPollNeedStop: Boolean;
    FLongPollStopped: Boolean;
    FLongPollData: TLongPollData;
    FParams: TParams;
    FMethod: string;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    FOnError: TOnVKError;
    FInterval: Integer;
    FGroupID: string;
    FOnUpdate: TOnLongPollServerUpdate;
    function QueryLongPollServer: Boolean;
    procedure DoError(E: Exception);
    procedure OnLongPollRecieve(Updates: TJSONArray);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetInterval(const Value: Integer);
    procedure SetGroupID(const Value: string);
    procedure SetOnUpdate(const Value: TOnLongPollServerUpdate);
    procedure SetClient(const Value: TCustomRESTClient);
    function GetClient: TCustomRESTClient;
    procedure SetMethod(const Value: string);
    procedure SetParams(const Value: TParams);
    function GetIsWork: Boolean;
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
    property Client: TCustomRESTClient read GetClient write SetClient;
    property Method: string read FMethod write SetMethod;
    property Params: TParams read FParams write SetParams;
    property IsWork: Boolean read GetIsWork;
  end;

const
  DefaultLongPollServerInterval = 1000;

implementation

uses
  Vcl.Forms, Winapi.Windows, System.SyncObjs;

{ TLongPollServer }

constructor TLongPollServer.Create(AClient: TRESTClient; AMethod: string; AParams: TParams);
begin
  inherited Create;
  Method := AMethod;
  Params := AParams;
  Client := AClient;
end;

constructor TLongPollServer.Create;
begin
  inherited;
  FInterval := DefaultLongPollServerInterval;
  //Response
  RESTResponse := TRESTResponse.Create(nil);
  RESTResponse.ContentType := 'text/html';
  //Request
  RESTRequest := TRESTRequest.Create(nil);
  RESTRequest.Response := RESTResponse;
end;

destructor TLongPollServer.Destroy;
begin
  Stop;
  RESTResponse.Free;
  RESTRequest.Free;
  inherited;
end;

procedure TLongPollServer.DoError(E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, E, -10000, E.Message);
end;

function TLongPollServer.GetClient: TCustomRESTClient;
begin
  Result := RESTRequest.Client;
end;

function TLongPollServer.GetIsWork: Boolean;
begin
  Result := not FLongPollNeedStop;
end;

function TLongPollServer.QueryLongPollServer: Boolean;
var
  i: Integer;
  JSON: TJSONValue;
begin
  RESTRequest.Params.Clear;
  for i := Low(FParams) to High(FParams) do
    RESTRequest.AddParameter(FParams[i][0], FParams[i][1]);
  RESTRequest.Resource := FMethod;
  try
    RESTRequest.Execute;
    JSON := RESTResponse.JSONValue;
  except
    on E: Exception do
    begin
      DoError(E);
      Exit(False);
    end;
  end;

  if RESTResponse.JSONValue.TryGetValue<TJSONValue>('response', JSON) then
  begin
    FLongPollData.key := JSON.GetValue('key', '');
    FLongPollData.server := JSON.GetValue('server', '');
    FLongPollData.ts := JSON.GetValue('ts', '');
    FLongPollData.wait := '25';
    FLongPollData.version := '3';
    Result := not FLongPollData.server.IsEmpty;
  end
  else
    Result := False;
  if not Result then
  begin
    DoError(TVkLongPollServerException.Create('QueryLongPollServer error '#13#10 + JSON.ToString));
    Exit(False);
  end;
end;

procedure TLongPollServer.OnLongPollRecieve(Updates: TJSONArray);
var
  i: Integer;
begin
  for i := 0 to Updates.Count - 1 do
  try
    FOnUpdate(Self, FGroupID, Updates.Items[i]);
  except
    on E: Exception do
      DoError(E);
  end;
end;

procedure TLongPollServer.SetClient(const Value: TCustomRESTClient);
begin
  RESTRequest.Client := Value;
end;

procedure TLongPollServer.SetGroupID(const Value: string);
begin
  FGroupID := Value;
end;

procedure TLongPollServer.SetInterval(const Value: Integer);
begin
  FInterval := Value;
end;

procedure TLongPollServer.SetMethod(const Value: string);
begin
  FMethod := Value;
end;

procedure TLongPollServer.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TLongPollServer.SetOnUpdate(const Value: TOnLongPollServerUpdate);
begin
  FOnUpdate := Value;
end;

procedure TLongPollServer.SetParams(const Value: TParams);
var
  Param: TParam;
begin
  FParams := Value;
  //Сохраним id группы, если его передали
  for Param in FParams do
    if Param[0] = 'group_id' then
      FGroupID := Param[1];
end;

function TLongPollServer.Start: Boolean;
begin
  Result := False;
  FLongPollNeedStop := False;
  //
  if not Assigned(FOnUpdate) then
    raise Exception.Create('Необходимо обязательно указать обработчик входящих обновлений');
  if not QueryLongPollServer then
    Exit;
  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      HTTP: THTTPClient;
      Stream: TStringStream;
      JSON: TJSONValue;
      Updates: TJSONArray;
    begin
      FLongPollStopped := False;
      HTTP := THTTPClient.Create;
      Stream := TStringStream.Create;
      try
        while (not TThread.Current.CheckTerminated) and (not FLongPollNeedStop) do
        begin
          Stream.Clear;
          HTTP.Get(FLongPollData.Request, Stream);
          if FLongPollNeedStop then
            Break;
          JSON := TJSONObject.ParseJSONValue(UTF8ToString(Stream.DataString));
          if Assigned(JSON) then
          begin
            FLongPollData.ts := JSON.GetValue('ts', '');
            if FLongPollNeedStop then
              Break;
            if JSON.TryGetValue<TJSONArray>('updates', Updates) then
            begin
              TThread.Synchronize(TThread.Current,
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
              TThread.Synchronize(TThread.Current,
                procedure
                begin
                  if not FLongPollNeedStop then
                  begin
                    if not QueryLongPollServer then
                    begin
                      DoError(TVkLongPollServerException.Create('QueryLongPollServer error, result: '
                        + Stream.DataString));
                      FLongPollNeedStop := True;
                    end;
                  end;
                end);
            end;
            JSON.Free;
          end;
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

procedure TLongPollServer.Stop;
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
  end;
end;

{ TLongPollData }

function TLongPollData.Request: string;
begin
  Result := server + '?act=a_check&key=' + key + '&ts=' + ts + '&wait=' + wait + '&version=' + version;
  if Pos('http', Result) = 0 then
    Result := 'http://' + Result;
end;

{ TGroupLongPollServers }

procedure TGroupLongPollServers.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

function TGroupLongPollServers.Find(GroupID: string): Integer;
var
  i: Integer;
  Param: TParam;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    for Param in Items[i].FParams do
      if Param[0] = 'group_id' then
        if Param[1] = GroupID then
          Exit(i);
  end;
end;

end.

