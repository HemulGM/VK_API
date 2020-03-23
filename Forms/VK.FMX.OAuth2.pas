unit VK.FMX.OAuth2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Controls.Presentation, FMX.Edit;

type
  TFormFMXOAuth2 = class;

  TAuthResult = reference to procedure(From: TFormFMXOAuth2);

  TFormFMXOAuth2 = class(TForm)
    Browser: TWebBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserDidFinishLoad(ASender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLastTitle: string;
    FProxyUserName: string;
    FProxyPassword: string;
    FLastURL: string;
    FBrakeAll: Boolean;
    FNeedShow: Boolean;
    FIsError: Boolean;
    FToken: string;
    FTokenExpiry: Int64;
    FChangePasswordHash: string;
    FProc: TAuthResult;
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: Boolean);
    procedure SetChangePasswordHash(const Value: string);
    procedure SetToken(const Value: string);
    procedure SetTokenExpiry(const Value: Int64);
  public
    procedure ShowWithURL(const AURL: string; Modal: Boolean); overload;
    property Token: string read FToken write SetToken;
    property TokenExpiry: Int64 read FTokenExpiry write SetTokenExpiry;
    property ChangePasswordHash: string read FChangePasswordHash write SetChangePasswordHash;
    class procedure Execute(Url: string; Proc: TAuthResult);
  end;

var
  FormFMXOAuth2: TFormFMXOAuth2;

implementation

uses
  System.Net.HttpClient;

{$R *.fmx}

class procedure TFormFMXOAuth2.Execute(Url: string; Proc: TAuthResult);
var
  Form: TFormFMXOAuth2;
begin
  Form := TFormFMXOAuth2.Create(Application);
  Form.FProc := Proc;
  Form.ShowWithURL(Url, False);
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
    Close;
  end;
end;

procedure TFormFMXOAuth2.FAfterRedirect(const AURL: string; var DoCloseWebView: Boolean);
var
  i: integer;
  Str: string;
  Params: TStringList;
begin
  i := Pos('#access_token=', AURL);
  if (i = 0) then
    i := Pos('&access_token=', AURL);
  if (i <> 0) and (FToken.IsEmpty) then
  begin
    Str := AURL;
    Delete(Str, 1, i);
    Params := TStringList.Create;
    try
      Params.Delimiter := '&';
      Params.DelimitedText := Str;
      FChangePasswordHash := Params.Values['change_password_hash'];
      FToken := Params.Values['access_token'];
      if Params.IndexOf('expires_in') >= 0 then
        FTokenExpiry := StrToInt(Params.Values['expires_in'])
      else
        FTokenExpiry := 0;
      DoCloseWebView := True;
    finally
      Params.Free;
    end;
  end;
end;

procedure TFormFMXOAuth2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FProc(Self);
  //Action := TCloseAction.caFree;
end;

procedure TFormFMXOAuth2.FormCreate(Sender: TObject);
begin
  FLastTitle := '';
  FIsError := False;
  FBrakeAll := False;
  FLastURL := '';
end;

procedure TFormFMXOAuth2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    Close;
  end;
end;

procedure TFormFMXOAuth2.ShowWithURL(const AURL: string; Modal: Boolean);
begin
  FLastURL := AURL;
  FToken := '';
  FTokenExpiry := 0;

  if not FProxyUserName.IsEmpty then
  begin
    //Base64EncodeStr(FProxyUserName + ':' + FProxyPassword, S);
    //Browser.Navigate(
    //Browser.Navigate2(AURL, EmptyParam{Flags}, EmptyParam{TargetFrameName}, EmptyParam{PostData}, 'Proxy-Authorization: BASIC ' + S + #13#10 + 'X-StopHandling: 1' + #13#10);
  end
  else
    Browser.Navigate(AURL);

  if Modal then
  begin
    ShowModal;
    BringToFront;
  end
  else
  begin
    Show;
    BringToFront;
  end;
end;

procedure TFormFMXOAuth2.SetChangePasswordHash(const Value: string);
begin
  FChangePasswordHash := Value;
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

