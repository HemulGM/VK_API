﻿unit VK.FMX.OAuth2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.WebBrowser, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Layouts;

type
  TFormFMXOAuth2 = class;

  TAuthResult = reference to procedure(Form: TFormFMXOAuth2);

  TFormFMXOAuth2 = class(TForm)
    Browser: TWebBrowser;
    LayoutLoad: TLayout;
    AniIndicatorLoad: TAniIndicator;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserDidFinishLoad(ASender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BrowserDidFailLoadWithError(ASender: TObject);
  private
    FLastTitle: string;
    FProxyUserName: string;
    //FProxyPassword: string;
    FLastURL: string;
    FBrakeAll: Boolean;
    FNeedShow: Boolean;
    FIsError: Boolean;
    FToken: string;
    FTokenExpiry: Int64;
    FChangePasswordHash: string;
    FProc: TAuthResult;
    FExecuteCompleted: Boolean;
    FErrorCode: Integer;
    FClosed: Boolean;
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: Boolean);
    procedure SetChangePasswordHash(const Value: string);
    procedure SetToken(const Value: string);
    procedure SetTokenExpiry(const Value: Int64);
    procedure SetErrorCode(const Value: Integer);
    procedure SetIsError(const Value: Boolean);
  public
    procedure ShowWithURL(const AURL: string; Modal: Boolean); overload;
    procedure InternatlFixIE;
    procedure InternatlDeleteCache(URLContains: string);
    property Token: string read FToken write SetToken;
    property TokenExpiry: Int64 read FTokenExpiry write SetTokenExpiry;
    property IsError: Boolean read FIsError write SetIsError;
    property ErrorCode: Integer read FErrorCode write SetErrorCode;
    property ChangePasswordHash: string read FChangePasswordHash write SetChangePasswordHash;
    property LastUrl: string read FLastURL;
    class procedure Execute(Url: string; Proc: TAuthResult; AStyleBook: TStyleBook = nil);
  end;

var
  FormFMXOAuth2: TFormFMXOAuth2;


{$IFDEF MSWINDOWS}
procedure FixIE;

procedure DeleteCache(URLContains: string);
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.UrlMon, Winapi.WinInet, System.Win.Registry,
  {$ENDIF}
  System.Net.HttpClient, VK.CommonUtils;

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure FixIE;
const
  IEVersion = 11001;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegIniFile.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', True) then
      begin
        try
          Reg.WriteInteger(ExtractFileName(ParamStr(0)), IEVersion);
          //Reg.DeleteKey(ExtractFileName(ParamStr(0)));
        except
        end;
      end;
      Reg.CloseKey;
    finally
      Reg.Free;
    end;
  except
  end;
end;

procedure DeleteCache(URLContains: string);
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
{$ENDIF}

class procedure TFormFMXOAuth2.Execute(Url: string; Proc: TAuthResult; AStyleBook: TStyleBook);
var
  Form: TFormFMXOAuth2;
begin
  Form := TFormFMXOAuth2.Create(Application);
  Form.FProc := Proc;
  Form.FExecuteCompleted := False;
  Form.StyleBook := AStyleBook;
  Form.ShowWithURL(Url, {$IFDEF MSWINDOWS} True {$ELSE} False {$ENDIF});
end;

procedure TFormFMXOAuth2.BrowserDidFailLoadWithError(ASender: TObject);
begin
  FIsError := True;
  FErrorCode := -1;
  FLastURL := Browser.URL;
  Browser.Stop;
  Close;
end;

procedure TFormFMXOAuth2.BrowserDidFinishLoad(ASender: TObject);
var
  LDoCloseForm: Boolean;
  URL: string;
begin
  URL := Browser.URL;
  FLastURL := VarToStrDef(URL, '');
  LDoCloseForm := False;

  FAfterRedirect(FLastURL, LDoCloseForm);

  if LDoCloseForm then
  begin
    FNeedShow := False;
    if not FExecuteCompleted then
    begin
      FExecuteCompleted := True;
      Close;
    end;
  end
  else
  begin
    LayoutLoad.Visible := False;
    AniIndicatorLoad.Enabled := False;
    Browser.Visible := True;
    Show;
  end;
end;

procedure TFormFMXOAuth2.InternatlDeleteCache(URLContains: string);
begin
  {$IFDEF MSWINDOWS}
  DeleteCache(URLContains);
  {$ENDIF}
end;

procedure TFormFMXOAuth2.FAfterRedirect(const AURL: string; var DoCloseWebView: Boolean);
var
  FTokenExpiryStr: string;
begin
  if FToken.IsEmpty then
  begin
    if GetTokenFromUrl(AURL, FToken, FChangePasswordHash, FTokenExpiryStr) then
    begin
      FTokenExpiry := StrToIntDef(FTokenExpiryStr, 0);
      DoCloseWebView := True;
    end;
  end;
  DoCloseWebView := not FToken.IsEmpty;
end;

procedure TFormFMXOAuth2.InternatlFixIE;
begin
  {$IFDEF MSWINDOWS}
  FixIE;
  {$ENDIF}
end;

procedure TFormFMXOAuth2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not FClosed then
  begin
    FClosed := True;
    FProc(Self);
  end;
end;

procedure TFormFMXOAuth2.FormCreate(Sender: TObject);
  {$IFDEF MSWINDOWS}
const
  UserAgent =
      'Mozilla/5.0 (Windows Phone 10.0;  Android 4.2.1; Nokia; Lumia 520) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Mobile Safari/537.36 Edge/12.0';
  {$ENDIF}
begin
  FLastTitle := '';
  FIsError := False;
  FBrakeAll := False;
  FLastURL := '';
  {$IFDEF MSWINDOWS}
  UrlMkSetSessionOption(URLMON_OPTION_USERAGENT, PAnsiChar(AnsiString(UserAgent)), Length(UserAgent), 0);
  {$ENDIF}
end;

procedure TFormFMXOAuth2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TFormFMXOAuth2.ShowWithURL(const AURL: string; Modal: Boolean);
begin
  FClosed := False;
  FLastURL := AURL;
  FToken := '';
  FTokenExpiry := 0;
  Browser.Visible := False;
  LayoutLoad.Visible := True;
  AniIndicatorLoad.Enabled := True;

  if not FProxyUserName.IsEmpty then
  begin
    //Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    //Browser.Navigate(
    //Browser.Navigate2(AURL, EmptyParam{Flags}, EmptyParam{TargetFrameName}, EmptyParam{PostData}, 'Proxy-Authorization: BASIC ' + S + #13#10 + 'X-StopHandling: 1' + #13#10);
    Browser.Navigate(AURL)
  end
  else
    Browser.Navigate(AURL);

  if Modal then
  begin
    ShowModal;
    BringToFront;
  end
  else
    BringToFront;
end;

procedure TFormFMXOAuth2.SetChangePasswordHash(const Value: string);
begin
  FChangePasswordHash := Value;
end;

procedure TFormFMXOAuth2.SetErrorCode(const Value: Integer);
begin
  FErrorCode := Value;
end;

procedure TFormFMXOAuth2.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
end;

procedure TFormFMXOAuth2.SetToken(const Value: string);
begin
  FToken := Value;
end;

procedure TFormFMXOAuth2.SetTokenExpiry(const Value: Int64);
begin
  FTokenExpiry := Value;
end;

end.

