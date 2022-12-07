unit ChatFMX.PreviewManager;

interface

uses
  System.SysUtils, System.Classes, System.Messaging, System.Threading,
  System.SyncObjs, System.Generics.Collections, System.Net.HttpClient;

type
  TPreviewData = record
    Url: string;
    FileName: string;
    class function Create(Url, FileName: string): TPreviewData; static;
  end;

  TOnLog = procedure(Sender: TObject; Value: string) of object;

  TMessagePreview = class(TMessage<TPreviewData>);

  TPreview = class
  private
    class var
      FInstance: TPreview;
  private
    FWorker: ITask;
    FLocalPath: string;
    FQueue: TThreadList<string>;
    FLoaded: TDictionary<string, string>;
    FOnLog: TOnLog;
    function LockLoaded: TDictionary<string, string>;
    procedure UnlockLoaded;
    procedure Worker;
    function GetFileName(const Url: string; out FileName: string; out Stream: TFileStream): Boolean;
    procedure AppendFile(const Url, FileName: string);
    procedure Event(Url, FileName: string);
    function CheckFile(const Url: string; out FileName: string): Boolean;
    function ParseUrlFileName(const Url: string): string;
    procedure SetOnLog(const Value: TOnLog);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Subscribe(Url: string; CallBack: TMessageListenerMethod);
    procedure Unsubscribe(const AListenerMethod: TMessageListenerMethod);
    procedure Log(Value: string);
    property OnLog: TOnLog read FOnLog write SetOnLog;
    class function Instance: TPreview;
    class destructor Destroy;
  end;

implementation

uses
  FMX.Forms, System.Net.URLClient, System.IOUtils;

{ TPreview }

constructor TPreview.Create;
begin
  inherited;
  FLocalPath := TPath.Combine(TPath.GetHomePath, 'FMXChatCache');
  try
    TDirectory.CreateDirectory(FLocalPath);
  except
    //
  end;
  FLoaded := TDictionary<string, string>.Create;
  FQueue := TThreadList<string>.Create;
  FWorker := TTask.Run(Worker);
end;

class destructor TPreview.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

destructor TPreview.Destroy;
begin
  LockLoaded.Free;
  FQueue.Free;
  inherited;
end;

function TPreview.CheckFile(const Url: string; out FileName: string): Boolean;
begin
  Result := False;
  FileName := ParseUrlFileName(Url);
  if not FileName.IsEmpty then
  begin
    FileName := TPath.Combine(FLocalPath, FileName);
    Result := TFile.Exists(FileName);
  end;
end;

procedure TPreview.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TPreview.Subscribe(Url: string; CallBack: TMessageListenerMethod);
begin
  TMessageManager.DefaultManager.SubscribeToMessage(TMessagePreview, CallBack);
  TTask.Run(
    procedure
    begin
      var FileName: string;
      var List := LockLoaded;
      try
        List.TryGetValue(Url, FileName);
      finally
        UnlockLoaded;
      end;
      if (not FileName.IsEmpty) then
        Event(Url, FileName)
      else if CheckFile(Url, FileName) then
        Event(Url, FileName)
      else
        FQueue.Add(Url);
    end);
end;

function TPreview.LockLoaded: TDictionary<string, string>;
begin
  TMonitor.Enter(FLoaded);
  Result := FLoaded;
end;

procedure TPreview.Log(Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

procedure TPreview.UnlockLoaded;
begin
  TMonitor.Exit(FLoaded);
end;

procedure TPreview.Unsubscribe(const AListenerMethod: TMessageListenerMethod);
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessagePreview, AListenerMethod);
end;

function TPreview.ParseUrlFileName(const Url: string): string;
begin
  try
    var UrlEnc := TURI.Create(Url);
    var Items := UrlEnc.Path.Split(['/']);
    if Length(Items) > 0 then
      Result := Items[High(Items)]
    else
      Result := '';
  except
    Result := '';
  end;
end;

function TPreview.GetFileName(const Url: string; out FileName: string; out Stream: TFileStream): Boolean;
begin
  Result := False;
  try
    FileName := ParseUrlFileName(Url);
    if not FileName.IsEmpty then
    begin
      FileName := TPath.Combine(FLocalPath, FileName);
      if TFile.Exists(FileName) then
        TFile.Delete(FileName);
      Stream := TFileStream.Create(FileName, fmCreate);
      Result := True;
    end;
  except
    Result := False;
  end;
end;

class function TPreview.Instance: TPreview;
begin
  if not Assigned(FInstance) then
    FInstance := TPreview.Create;
  Result := FInstance;
end;

procedure TPreview.Event(Url, FileName: string);
var
  AUrl, AFileName: string;
begin
  AUrl := Url;
  AFileName := FileName;
  TThread.Queue(nil,
    procedure
    var
      Data: TPreviewData;
    begin
      Data := TPreviewData.Create(AUrl, AFileName);
      TMessageManager.DefaultManager.SendMessage(Self, TMessagePreview.Create(Data), True);
    end);
end;

procedure TPreview.AppendFile(const Url, FileName: string);
begin
  LockLoaded.Add(Url, FileName);
  UnlockLoaded;
  Event(Url, FileName);
end;

procedure TPreview.Worker;
var
  Urls: TArray<string>;
  HTTP: THTTPClient;
begin
  HTTP := THTTPClient.Create;
  try
    while not Application.Terminated do
    begin
      with FQueue.LockList do
      try
        Urls := ToArray;
        Clear;
      finally
        FQueue.UnlockList;
      end;
      for var Url in Urls do
      begin
        var Succ := False;
        var FileName: string;
        try
          var Stream: TFileStream := nil;
          if GetFileName(Url, FileName, Stream) then
          try
            Succ := HTTP.Get(Url, Stream).StatusCode = 200;
          finally
            if Assigned(Stream) then
              Stream.Free;
          end;
        except
          //skip
        end;
        if Succ then
          AppendFile(Url, FileName)
        else
        begin
          AppendFile(Url, '');
          TFile.Delete(FileName);
        end;
      end;
      Sleep(10);
    end;
  finally
    HTTP.Free;
  end;
end;

{ TPreviewData }

class function TPreviewData.Create(Url, FileName: string): TPreviewData;
begin
  Result.Url := Url;
  Result.FileName := FileName;
end;

end.

