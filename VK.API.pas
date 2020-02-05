unit VK.API;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, REST.Client,
  Vcl.Controls, REST.Authenticator.OAuth, IPPeerClient, VK.Types, VK.OAuth2, VK.Account, VK.Handler,
  VK.Auth, VK.Users, System.Net.HttpClient, VK.LongPollServer, System.JSON, VK.Messages,
  System.Generics.Collections, VK.Status, VK.Wall, VK.Uploader, VK.Docs, VK.Audio, VK.Likes,
  VK.Board, Vcl.Forms, REST.Types, VK.FakeAndroidProto, VK.Friends, VK.Groups, VK.Photos;

type
  TCustomVK = class(TComponent)
  private
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnLogin: TOnLogin;
    FPermissionsList: TPermissions;
    FAuthForm: TFormOAuth2;
    FAppID: string;
    FAppKey: string;
    FEndPoint: string;
    FHandler: TVkHandler;
    FBaseURL: string;
    FAPIVersion: string;
    FAccount: TAccountController;
    FAuth: TAuthController;
    FServiceKey: string;
    FUseServiceKeyOnly: Boolean;
    FIsLogin: Boolean;
    FOnError: TOnVKError;
    FOnLog: TOnLog;
    FOnErrorLogin: TOnVKError;
    FChangePasswordHash: string;
    FUsers: TUsersController;
    FOnCaptcha: TOnCaptcha;
    FOnConfirm: TOnConfirm;
    FOnAuth: TOnAuth;
    FMessages: TMessagesController;
    FStatus: TStatusController;
    FWall: TWallController;
    FUploader: TUploader;
    FDoc: TDocController;
    FLikes: TLikesController;
    FAudio: TAudioController;
    FBoard: TBoardController;
    FFriends: TFriendsController;
    FGroups: TGroupsController;
    FPhotos: TPhotosController;
    function GetPermissions: string;
    procedure FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure FAuthError(const AText: string; AStatusCode: Integer; var Cancel: WordBool);
    procedure FAuthClose(Sender: TObject; var Action: TCloseAction);
    procedure FLog(Sender: TObject; const Value: string);
    procedure FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetOnLogin(const Value: TOnLogin);
    procedure SetPermissionsList(const Value: TPermissions);
    procedure DoLogin;
    procedure SetAppID(const Value: string);
    procedure SetAppKey(const Value: string);
    procedure SetEndPoint(const Value: string);
    procedure SetPermissions(const Value: string);
    procedure SetHandler(const Value: TVkHandler);
    procedure SetBaseURL(const Value: string);
    procedure SetAPIVersion(const Value: string);
    procedure SetServiceKey(const Value: string);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetOnErrorLogin(const Value: TOnVKError);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnAuth(const Value: TOnAuth);
    function GetToken: string;
    procedure FVKErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Await;
    procedure DoLog(Sender: TObject; Text: string);
    procedure DoError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure DoErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure Login(AParentWindow: TWinControl = nil); overload;
    procedure Login(Login, Password: string; AParentWindow: TWinControl = nil); overload;
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Param: string; Value: string; Callback:
      TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Callback: TCallMethodCallback = nil); overload;
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code">код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    function Execute(Code: string): TResponse;
    procedure SetProxy(IP: string; Port: Integer; UserName: string = ''; Password: string = '');
    property PermissionsList: TPermissions read FPermissionsList write SetPermissionsList;
    property Uploader: TUploader read FUploader;
    //Группы методов
    property Account: TAccountController read FAccount;
    property Auth: TAuthController read FAuth;
    property Users: TUsersController read FUsers;
    property Messages: TMessagesController read FMessages;
    property Status: TStatusController read FStatus;
    property Wall: TWallController read FWall;
    property Docs: TDocController read FDoc;
    property Likes: TLikesController read FLikes;
    property Audio: TAudioController read FAudio;
    property Board: TBoardController read FBoard;
    property Friends: TFriendsController read FFriends;
    property Groups: TGroupsController read FGroups;
    property Photos: TPhotosController read FPhotos;
    //
    property AppID: string read FAppID write SetAppID;
    property AppKey: string read FAppKey write SetAppKey;
    property EndPoint: string read FEndPoint write SetEndPoint;
    property Permissions: string read GetPermissions write SetPermissions;
    property Handler: TVkHandler read FHandler write SetHandler;
    property APIVersion: string read FAPIVersion write SetAPIVersion;
    property BaseURL: string read FBaseURL write SetBaseURL;
    property ServiceKey: string read FServiceKey write SetServiceKey;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property IsLogin: Boolean read FIsLogin;
    property ChangePasswordHash: string read FChangePasswordHash;
    //
    property OnLogin: TOnLogin read FOnLogin write SetOnLogin;
    property OnError: TOnVKError read FOnError write SetOnError;
    property OnErrorLogin: TOnVKError read FOnErrorLogin write SetOnErrorLogin;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property OnAuth: TOnAuth read FOnAuth write SetOnAuth;
    property Token: string read GetToken;
  end;

implementation

uses
  System.DateUtils, VK.Captcha;

{ TVK }

procedure TCustomVK.CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, Params);
  if Assigned(Callback) then
  begin
    Callback(Response);
  end;
end;

procedure TCustomVK.CallMethod(MethodName: string; Param: string; Value: string; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, [Param, Value]);
  if Assigned(Callback) then
  begin
    Callback(Response);
  end;
end;

procedure TCustomVK.CallMethod(MethodName: string; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName);
  if Assigned(Callback) then
  begin
    Callback(Response);
  end;
end;

constructor TCustomVK.Create(AOwner: TComponent);
begin
  inherited;
  FIsLogin := False;
  FUseServiceKeyOnly := False;
  FOAuth2Authenticator := TOAuth2Authenticator.Create(Self);
  FHandler := TVkHandler.Create(Self);
  FHandler.OnError := FVKError;
  FHandler.OnLog := FLog;
  FHandler.Client.Authenticator := FOAuth2Authenticator;
  FHandler.OnCaptcha := FAskCaptcha;
  FHandler.OnConfirm := FOnConfirm;
  //Defaults
  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  APIVersion := '5.103';
  PermissionsList := ['groups', 'friends', 'wall', 'photos', 'video', 'docs', 'notes', 'market'];
  //
  FAccount := TAccountController.Create(FHandler);
  FAuth := TAuthController.Create(FHandler);
  FUsers := TUsersController.Create(FHandler);
  FMessages := TMessagesController.Create(FHandler);
  FStatus := TStatusController.Create(FHandler);
  FWall := TWallController.Create(FHandler);
  FDoc := TDocController.Create(FHandler);
  FLikes := TLikesController.Create(FHandler);
  FAudio := TAudioController.Create(FHandler);
  FBoard := TBoardController.Create(FHandler);
  FFriends := TFriendsController.Create(FHandler);
  FGroups := TGroupsController.Create(FHandler);
  FPhotos := TPhotosController.Create(FHandler);
  //
  FUploader := TUploader.Create;
end;

procedure TCustomVK.Await;
begin
  while FHandler.Executing do
    Application.ProcessMessages;
end;

destructor TCustomVK.Destroy;
begin
  Await;
  if Assigned(FAuthForm) then
    FAuthForm.Close;

  FBoard.Free;
  FFriends.Free;
  FGroups.Free;
  FPhotos.Free;
  FLikes.Free;
  FAudio.Free;
  FDoc.Free;
  FUploader.Free;
  FWall.Free;
  FStatus.Free;
  FUsers.Free;
  FAccount.Free;
  FAuth.Free;
  FMessages.Free;
  FHandler.Free;
  inherited;
end;

procedure TCustomVK.DoError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  FVKError(Sender, E, Code, Text);
end;

procedure TCustomVK.DoErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  FVKErrorLogin(Sender, E, Code, Text);
end;

procedure TCustomVK.DoLog(Sender: TObject; Text: string);
begin
  FLog(Self, Text);
end;

procedure TCustomVK.DoLogin;
begin
  FIsLogin := True;
  if Assigned(FOnLogin) then
    FOnLogin(Self);
end;

procedure TCustomVK.FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
begin
  if Assigned(FOnCaptcha) then
  begin
    FOnCaptcha(Self, CaptchaImg, Answer);
  end
  else
    TFormCaptcha.Execute(CaptchaImg, Answer);
end;

function TCustomVK.Execute(Code: string): TResponse;
var
  Resp: TResponse;
begin
  CallMethod('execute', [['code', Code]],
    procedure(Response: TResponse)
    begin
      Resp := Response;
    end);
  Result := Resp;
end;

procedure TCustomVK.FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
var
  i: integer;
  Str: string;
  Params: TStringList;
begin
  i := Pos('#access_token=', AURL);
  if (i = 0) then
    i := Pos('&access_token=', AURL);
  if (i <> 0) and (FOAuth2Authenticator.AccessToken.IsEmpty) then
  begin
    Str := AURL;
    Delete(Str, 1, i);
    Params := TStringList.Create;
    try
      Params.Delimiter := '&';
      Params.DelimitedText := Str;
      FChangePasswordHash := Params.Values['change_password_hash'];
      FOAuth2Authenticator.AccessToken := Params.Values['access_token'];
      FLog(Self, FOAuth2Authenticator.AccessToken);
      if Params.IndexOf('expires_in') >= 0 then
        FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, StrToInt(Params.Values['expires_in']))
      else
        FOAuth2Authenticator.AccessTokenExpiry := 0;
      DoLogin;
    finally
      Params.Free;
    end;
    DoCloseWebView := True;
  end;
end;

procedure TCustomVK.FLog(Sender: TObject; const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

procedure TCustomVK.FAuthClose(Sender: TObject; var Action: TCloseAction);
var
  E: Exception;
  IsError: Boolean;
begin
  IsError := FAuthForm.IsError;
  Action := caFree;
  FAuthForm := nil;
  //Это необходимо, чтобы не передавать ошибку авторизации, если мы сразу завершаем программу
  if Application.Terminated then
    Exit;
  if not IsError then
  begin
    if FOAuth2Authenticator.AccessToken.IsEmpty then
    begin
      E := TVkException.Create('Токен не был получен');
      if Assigned(FOnErrorLogin) then
      begin
        FOnErrorLogin(Self, E, ERROR_VK_NOTOKEN, E.Message);
        if Assigned(E) then
          E.Free;
      end
      else
      begin
        DoLog(Self, E.Message);
        E.Free;
      end;
    end;
  end;
end;

procedure TCustomVK.FAuthError(const AText: string; AStatusCode: Integer; var Cancel: WordBool);
var
  E: Exception;
begin
  E := TVkException.Create('Токен не был получен');
  if Assigned(FOnErrorLogin) then
  begin
    FOnErrorLogin(Self, E, ERROR_VK_NOTOKEN, AText);
    if Assigned(E) then
      E.Free;
  end
  else
  begin
    DoLog(Self, E.Message);
    E.Free;
  end;
  if Assigned(FAuthForm) then
  begin
    FAuthForm.Close;
  end;
end;

procedure TCustomVK.FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
  begin
    FOnError(Self, E, Code, Text);
    if Assigned(E) then
      E.Free;
  end
  else
    raise E;
end;

procedure TCustomVK.FVKErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if Assigned(FOnErrorLogin) then
  begin
    FOnErrorLogin(Self, E, Code, Text);
    if Assigned(E) then
      E.Free;
  end
  else
    raise E;
end;

procedure TCustomVK.Login(Login, Password: string; AParentWindow: TWinControl = nil);
var
  Token, PasswordHash: string;
  TokenExpiry: Int64;
  HTTP: THTTPClient;
  RedirectUrl, Error: string;
  Stream: TStringStream;
  JSON: TJSONValue;
//  Proto: TStream;
begin
  PasswordHash := '';
  Token := '';
  TokenExpiry := 0;
  if Assigned(FOnAuth) then
    FOnAuth(Self, Token, TokenExpiry, PasswordHash);

  if not Token.IsEmpty then
  begin
    FChangePasswordHash := PasswordHash;
    FOAuth2Authenticator.AccessToken := Token;
    if TokenExpiry > 0 then
      FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, TokenExpiry)
    else
      FOAuth2Authenticator.AccessTokenExpiry := 0;
    DoLogin;
  end
  else
  begin
    HTTP := THTTPClient.Create;
    Stream := TStringStream.Create;
    try
      {Proto := TMemoryStream.Create;
      with TAndroidCheckinRequest.Create do
      begin
        Checkin := TAndroidChekin.Create;
        with Checkin do
        begin
          CellOperator := '310260';
          Roaming := 'mobile:LTE:';
          SimOperator := '310260';
          &Type := 'DEVICE_ANDROID_OS';
        end;
        Digest := '1-929a0dca0eee55513280171a8585da7dcd3700f8';
        Locale := 'en_US';
        LoggingId := -8212629671123625360;
        Meid := '358240051111110';
        OtaCerts.Add('71Q6Rn2DDZl1zPDVaaeEHItd+Yg=');
        TimeZone := 'America/New_York';
        Version := 3;
        SaveToStream(Proto);
        Checkin.Free;
        Free;
      end;
      HTTP.ContentType := 'application/x-protobuffer';
      HTTP.UserAgent := 'Android-GCM/1.5 (generic_x86 KK)'; }
      HTTP.Get(
        'https://oauth.vk.com/token?grant_type=password' +
        '&client_id=' + FAppID +
        '&client_secret=' + FAppKey +
        '&username=' + Login +
        '&password=' + Password, Stream);
     // Proto.Free;
      try
        JSON := TJSONObject.ParseJSONValue(Stream.DataString);
        if Assigned(JSON) then
        begin
          RedirectUrl := JSON.GetValue<string>('redirect_uri', '');
          JSON.Free;
        end
        else
          RedirectUrl := '';
        if RedirectUrl = '' then
          Error := Stream.DataString;
      except
        RedirectUrl := '';
      end;
    finally
      HTTP.Free;
      Stream.Free;
    end;
    if not RedirectUrl.IsEmpty then
    begin
      if not Assigned(FAuthForm) then
      begin
        FAuthForm := TFormOAuth2.Create(nil);
        FAuthForm.OnAfterRedirect := FAfterRedirect;
        FAuthForm.OnError := FAuthError;
        FAuthForm.OnClose := FAuthClose;
      end;
      FAuthForm.ShowWithURL(AParentWindow, RedirectUrl)
    end
    else
    begin
      DoError(Self, TVkException.Create('Ошибка запроса авторизации'), -1, Error);
    end;
  end;
end;

procedure TCustomVK.Login(AParentWindow: TWinControl);
var
  Token, PasswordHash: string;
  TokenExpiry: Int64;
begin
  FOAuth2Authenticator.AccessToken := EmptyStr;
  FOAuth2Authenticator.ClientID := FAppID;
  //FOAuth2Authenticator.ClientSecret := FAppKey;
  FOAuth2Authenticator.ResponseType := TOAuth2ResponseType.rtTOKEN;
  FOAuth2Authenticator.AuthorizationEndpoint := FEndPoint;
  FOAuth2Authenticator.Scope := PermissionsList.ToString;
  PasswordHash := '';
  Token := '';
  TokenExpiry := 0;
  if Assigned(FOnAuth) then
    FOnAuth(Self, Token, TokenExpiry, PasswordHash);

  if not Token.IsEmpty then
  begin
    FChangePasswordHash := PasswordHash;
    FOAuth2Authenticator.AccessToken := Token;
    if TokenExpiry > 0 then
      FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, TokenExpiry)
    else
      FOAuth2Authenticator.AccessTokenExpiry := 0;
    DoLogin;
  end
  else
  begin
    if not Assigned(FAuthForm) then
    begin
      FAuthForm := TFormOAuth2.Create(nil);
      FAuthForm.OnAfterRedirect := FAfterRedirect;
      FAuthForm.OnError := FAuthError;
      FAuthForm.OnClose := FAuthClose;
      if not FHandler.Client.ProxyServer.IsEmpty then
        FAuthForm.SetProxy(FHandler.Client.ProxyServer, FHandler.Client.ProxyPort, FHandler.Client.ProxyUsername,
          FHandler.Client.ProxyPassword);
    end;
    FAuthForm.ShowWithURL(AParentWindow, FOAuth2Authenticator.AuthorizationRequestURI);
  end;
end;

procedure TCustomVK.SetAPIVersion(const Value: string);
begin
  FAPIVersion := Value;
  FHandler.Client.AddParameter('v', FAPIVersion);
end;

procedure TCustomVK.SetServiceKey(const Value: string);
begin
  FServiceKey := Value;
  if csDesigning in ComponentState then
    Exit;

  if FUseServiceKeyOnly then
  begin
    FHandler.Client.AddParameter('access_token', FServiceKey);
  end;
end;

procedure TCustomVK.SetAppID(const Value: string);
begin
  FAppID := Value;
end;

procedure TCustomVK.SetAppKey(const Value: string);
begin
  FAppKey := Value;
end;

procedure TCustomVK.SetEndPoint(const Value: string);
begin
  FEndPoint := Value;
end;

procedure TCustomVK.SetHandler(const Value: TVkHandler);
begin
  FHandler := Value;
end;

procedure TCustomVK.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  FHandler.Client.BaseURL := FBaseURL;
end;

procedure TCustomVK.SetOnAuth(const Value: TOnAuth);
begin
  FOnAuth := Value;
end;

procedure TCustomVK.SetOnCaptcha(const Value: TOnCaptcha);
begin
  FOnCaptcha := Value;
end;

procedure TCustomVK.SetOnConfirm(const Value: TOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TCustomVK.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TCustomVK.SetOnErrorLogin(const Value: TOnVKError);
begin
  FOnErrorLogin := Value;
end;

procedure TCustomVK.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TCustomVK.SetOnLogin(const Value: TOnLogin);
begin
  FOnLogin := Value;
end;

procedure TCustomVK.SetPermissions(const Value: string);
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Params.Delimiter := ',';
    Params.DelimitedText := Value;
    FPermissionsList.Assign(Params);
  finally
    Params.Free;
  end;
end;

function TCustomVK.GetPermissions: string;
begin
  Result := FPermissionsList.ToString;
end;

function TCustomVK.GetToken: string;
begin
  Result := FOAuth2Authenticator.AccessToken;
end;

procedure TCustomVK.SetPermissionsList(const Value: TPermissions);
begin
  FPermissionsList := Value;
end;

procedure TCustomVK.SetProxy(IP: string; Port: Integer; UserName, Password: string);
begin
  FHandler.Client.ProxyServer := IP;
  FHandler.Client.ProxyPort := Port;
  FHandler.Client.ProxyUsername := UserName;
  FHandler.Client.ProxyPassword := Password;
end;

procedure TCustomVK.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
  if csDesigning in ComponentState then
    Exit;

  FHandler.UseServiceKeyOnly := Value;
  if FUseServiceKeyOnly then
  begin
    FHandler.Client.Authenticator := nil;
    FHandler.Client.AddParameter('access_token', FServiceKey);
  end
  else
  begin
    FHandler.Client.Authenticator := FOAuth2Authenticator;
    FHandler.Client.Params.Delete('access_token');
  end;
end;

end.

