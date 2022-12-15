unit ChatFMX.PreviewManager;

interface

uses
  System.SysUtils, System.IOUtils, System.Classes, System.Messaging,
  System.Threading, System.SyncObjs, System.Generics.Collections,
  System.Net.HttpClient;

type
  TPreviewData = record
    Url: string;
    FileName: string;
    class function Create(Url, FileName: string): TPreviewData; static;
  end;

  TFileHelper = record helper for TFile
    class function GetSize(const Path: string): Int64; static;
  end;

  TOnLog = procedure(Sender: TObject; Value: string) of object;

  TMessagePreview = class(TMessage<TPreviewData>);

  TPreview = class
  private
    class var
      FInstance: TPreview;
    class var
      FManager: TMessageManager;
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
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  FMX.Forms, {$IFDEF MSWINDOWS} Winapi.Windows, {$ELSE} Posix.SysStat, {$ENDIF}
  System.Net.URLClient, System.Hash, System.NetEncoding;

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
  if Assigned(FManager) then
    FManager.Free;
  if Assigned(FInstance) then
    FInstance.Free;
end;

destructor TPreview.Destroy;
begin
  FWorker.Cancel;
  TTask.WaitForAll(FWorker);
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

    Result := TFile.Exists(FileName) and (TFile.GetSize(FileName) > 0);
  end;
end;

class constructor TPreview.Create;
begin
  FManager := TMessageManager.Create;
end;

procedure TPreview.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TPreview.Subscribe(Url: string; CallBack: TMessageListenerMethod);
begin
  if Url.IsEmpty then
    Exit;
  FManager.SubscribeToMessage(TMessagePreview, CallBack);
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
  FManager.Unsubscribe(TMessagePreview, AListenerMethod);
end;

function TPreview.ParseUrlFileName(const Url: string): string;
begin
  //Result := '';

  Result := THashSHA2.GetHashString(TNetEncoding.Base64.Encode(Url));
  {for var C in TNetEncoding.Base64.Encode(Url) do
    if 'abcdefghijklmnopqrstuvwxyz'.Contains(string(C).ToLower) then
      Result := Result + C;
  Result := Result.Substring(0, 100);}
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
      FManager.SendMessage(Self, TMessagePreview.Create(Data), True);
    end);
end;

procedure TPreview.AppendFile(const Url, FileName: string);
begin
  var List := LockLoaded;
  try
    List.Add(Url, FileName);
  except
    // fine
  end;
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
    while (not Application.Terminated) and (TTask.CurrentTask.Status <> TTaskStatus.Canceled) do
    try
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
    except
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

{ TFileHelper }

class function TFileHelper.GetSize(const Path: string): Int64;
{$IFDEF MSWINDOWS}
var
  LPath: string;
  LInfo: TWin32FileAttributeData;
begin
  if (Length(Path) < MAX_PATH) or TPath.IsExtendedPrefixed(Path) then
    LPath := Path
  else
    LPath := '\\?\' + Path;
  if GetFileAttributesEx(PChar(LPath), GetFileExInfoStandard, @LInfo) then
  begin
    Result := LInfo.nFileSizeHigh;
    Result := Result shl 32 + LInfo.nFileSizeLow;
  end
  else begin
    Result := -1;
  end;
end;
{$ELSE}
var
  LFileName: UTF8String;
  LStatBuf: _stat;
begin
  LFileName := UTF8Encode(Path);
  if stat(PAnsiChar(LFileName), LStatBuf) = 0 then
    Result := LStatBuf.st_size
  else
    Result := -1;
end;
{$ENDIF}

end.

