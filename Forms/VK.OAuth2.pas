unit VK.OAuth2;

interface

uses
  Windows, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.OleCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, SHDocVw;

type
  TOAuth2WebFormRedirectEvent = procedure(const AURL: string; var DoCloseWebView: boolean) of object;

  TOAuth2WebFormTitleChangedEvent = procedure(const ATitle: string; var DoCloseWebView: boolean) of object;

  TOAuth2WebFormErrorEvent = procedure(const AText: string; AStatusCode: Integer; var Cancel: WordBool) of object;

  TFormOAuth2 = class(TForm)
    Browser: TWebBrowser;
    EditAddr: TEdit;
    Shape1: TShape;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserTitleChange(ASender: TObject; const Text: WideString);
    procedure FormCreate(Sender: TObject);
    procedure BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags,
      TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure EditAddrChange(Sender: TObject);
    procedure BrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame,
      StatusCode: OleVariant; var Cancel: WordBool);
    procedure BrowserFileDownload(ASender: TObject; ActiveDocument: WordBool; var Cancel: WordBool);
  private
    FOnBeforeRedirect: TOAuth2WebFormRedirectEvent;
    FOnAfterRedirect: TOAuth2WebFormRedirectEvent;
    FOnBrowserTitleChanged: TOAuth2WebFormTitleChangedEvent;
    FOnError: TOAuth2WebFormErrorEvent;
    FLastTitle: string;
    FProxyUserName: string;
    FProxyPassword: string;
    FLastURL: string;
    FBrakeAll: Boolean;
    FNeedShow: Boolean;
    FFreeze: Boolean;
    FIsError: Boolean;
    procedure DoError(const AText: string; AStatusCode: Integer; var Cancel: WordBool);
    procedure SetIsError(const Value: Boolean);
  public
    procedure ShowWithURL(const AURL: string); overload;
    procedure ShowWithURL(AParent: TWinControl; const AURL: string); overload;
    procedure SetProxy(Server: string; Port: Integer; UserName: string = ''; Password: string = '');
    property LastTitle: string read FLastTitle;
    property LastURL: string read FLastURL;
    property OnAfterRedirect: TOAuth2WebFormRedirectEvent read FOnAfterRedirect write FOnAfterRedirect;
    property OnBeforeRedirect: TOAuth2WebFormRedirectEvent read FOnBeforeRedirect write FOnBeforeRedirect;
    property OnTitleChanged: TOAuth2WebFormTitleChangedEvent read FOnBrowserTitleChanged write FOnBrowserTitleChanged;
    property OnError: TOAuth2WebFormErrorEvent read FOnError write FOnError;
    property IsError: Boolean read FIsError write SetIsError;
  end;

var
  FormOAuth2: TFormOAuth2;

procedure DeleteCache(URLContains: string);

procedure FixIE;

implementation

uses
  WinInet, Registry, UrlMon, System.Net.HttpClient, HGM.Common.Base64;

{$R *.dfm}

procedure TFormOAuth2.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
end;

procedure TFormOAuth2.SetProxy(Server: string; Port: Integer; UserName: string; Password: string);
var
  PIInfo: PInternetProxyInfo;
begin
  New(PIInfo);
  PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY;
  PIInfo^.lpszProxy := PAnsiChar(AnsiString(Server + ':' + Port.ToString));
  PIInfo^.lpszProxyBypass := '';
  UrlMkSetSessionOption(INTERNET_OPTION_PROXY, PIInfo, SizeOf(Internet_Proxy_Info), 0);
  Dispose(PIInfo);
  FProxyUserName := UserName;
  FProxyPassword := Password;
end;

procedure FixIE;
const
  IEVersion = 11001;
var
  Reg: TRegistry;
begin
  Reg := TRegIniFile.Create(KEY_WRITE);
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', True) then
  begin
    try
      Reg.WriteInteger(ExtractFileName(Application.ExeName), IEVersion);
    except
    end;
  end;
  Reg.CloseKey;
  Reg.Free;
end;

procedure DeleteCache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0 then
    lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
  begin
    repeat
      if (URLContains = '') or (Pos(URLContains, lpEntryInfo^.lpszSourceUrlName) <> 0) then
        DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then
        lpEntryInfo^.dwStructSize := dwEntrySize;
    until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  end;
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;

procedure TFormOAuth2.EditAddrChange(Sender: TObject);
begin
  if Copy(EditAddr.Text, 1, 6) = ' https' then
    EditAddr.Color := $00D8FFCC
  else
    EditAddr.Color := $0096AFFF;
end;

procedure TFormOAuth2.BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL,
  Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  LDoCloseForm: boolean;
begin
  if Assigned(FOnBeforeRedirect) then
  begin
    LDoCloseForm := False;

    FOnBeforeRedirect(URL, LDoCloseForm);

    if LDoCloseForm then
    begin
      Cancel := True;
      if Showing then
        if not FFreeze then
          Close;
    end;
  end;
end;

procedure TFormOAuth2.BrowserFileDownload(ASender: TObject; ActiveDocument: WordBool; var Cancel: WordBool);
begin
  Cancel := FBrakeAll;
end;

procedure TFormOAuth2.BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
var
  LDoCloseForm: Boolean;
begin
  FLastURL := VarToStrDef(URL, '');
  EditAddr.Text := ' ' + FLastURL;
  if Showing then
    Repaint;
  if Assigned(FOnAfterRedirect) then
  begin
    LDoCloseForm := False;

    FOnAfterRedirect(FLastURL, LDoCloseForm);

    if LDoCloseForm then
    begin
      FNeedShow := False;
      if Showing then
        if not FFreeze then
          Close;
    end;
  end;
end;

procedure TFormOAuth2.DoError(const AText: string; AStatusCode: Integer; var Cancel: WordBool);
begin
  FIsError := True;
  if Assigned(FOnError) then
    FOnError(AText, AStatusCode, Cancel)
  else
    raise Exception.Create('Ошибка авторизации: ' + AText + #13#10 + 'StatusCode: ' + AStatusCode.ToString);
end;

procedure TFormOAuth2.BrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL,
  Frame, StatusCode: OleVariant; var Cancel: WordBool);
begin
  FBrakeAll := True;
  Cancel := True;
  DoError(URL, StatusCode, Cancel);
end;

procedure TFormOAuth2.BrowserTitleChange(ASender: TObject; const Text: WideString);
var
  LCloseForm: boolean;
begin
  if (Text <> FLastTitle) then
  begin
    FLastTitle := Text;

    if Assigned(FOnBrowserTitleChanged) then
    begin
      LCloseForm := False;
      FOnBrowserTitleChanged(FLastTitle, LCloseForm);

      if LCloseForm then
      begin
        FNeedShow := False;
        if Showing then
          if not FFreeze then
            Close;
      end;
    end;
  end;
end;

procedure TFormOAuth2.FormCreate(Sender: TObject);
begin
  FOnAfterRedirect := nil;
  FOnBeforeRedirect := nil;
  FOnBrowserTitleChanged := nil;
  FLastTitle := '';
  FIsError := False;
  FBrakeAll := False;
  FLastURL := '';
  FixIE;
end;

procedure TFormOAuth2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    if not FFreeze then
      Close;
  end;
end;

procedure TFormOAuth2.ShowWithURL(AParent: TWinControl; const AURL: string);
var
  TS: Cardinal;
  S: string;
begin
  if Assigned(AParent) then
  begin
    SetParent(AParent);
    Align := alClient;
    BorderStyle := bsNone;
  end;

  FNeedShow := True;
  FLastURL := AURL;
  if not FProxyUserName.IsEmpty then
  begin
    Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    Browser.Navigate2(AURL,
      EmptyParam{Flags},
      EmptyParam{TargetFrameName},
      EmptyParam{PostData},
      'Proxy-Authorization: BASIC ' + S + #13#10 +
      'X-StopHandling: 1' + #13#10);
  end
  else
    Browser.Navigate(AURL);
  TS := GetTickCount;
  FFreeze := True;
  while TS + 3000 > GetTickCount do
    Application.ProcessMessages;
  FFreeze := False;

  if FNeedShow then
    Show
  else
    Close;
end;

procedure TFormOAuth2.ShowWithURL(const AURL: string);
var
  TS: Cardinal;
  S: string;
begin
  SetParent(nil);
  Align := alNone;
  BorderStyle := bsSizeable;

  FNeedShow := True;
  FLastURL := AURL;

  if not FProxyUserName.IsEmpty then
  begin
    Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    Browser.Navigate2(AURL,
      EmptyParam{Flags},
      EmptyParam{TargetFrameName},
      EmptyParam{PostData},
      'Proxy-Authorization: BASIC ' + S + #13#10 +
      'X-StopHandling: 1' + #13#10);
  end
  else
    Browser.Navigate(AURL);
  TS := GetTickCount;
  FFreeze := True;
  while TS + 3000 > GetTickCount do
    Application.ProcessMessages;
  FFreeze := False;
  if FNeedShow then
    ShowModal
  else
    Close;
end;

end.

