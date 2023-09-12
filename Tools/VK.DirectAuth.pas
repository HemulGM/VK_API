unit VK.DirectAuth;

interface

uses
  System.Classes, System.SysUtils, VK.Types;

function GetTokenDirect(AppId, AppKey, Login, Password: string; OnCaptcha: TOnCaptcha; On2FA: TOn2FA): string;

implementation

uses
  System.Net.HttpClient, VK.Entity.Login, VK.CommonUtils;

function GetTokenDirect(AppId, AppKey, Login, Password: string; OnCaptcha: TOnCaptcha; On2FA: TOn2FA): string;
var
  HTTP: THTTPClient;
  Response: TStringStream;
  EndResponse: IHTTPResponse;
  FormData: TStringList;
  Info: TVkLoginInfo;
  Remember: Boolean;
  Hash, Code, Url, CaptchaSid: string;
begin
  Assert(Assigned(OnCaptcha));
  Assert(Assigned(On2FA));
  Result := '';
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    try
      Url := Format('https://oauth.vk.com/token?grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s', [AppId, AppKey, Login, Password]);
      case HTTP.Get(Url, Response).StatusCode of
        401:
          try
            Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(Response.DataString);
            try
              if not Info.Error.IsEmpty then
              begin
                if Info.Error = 'need_validation' then
                  if TVkValidationType.FromString(Info.ValidationType) <> TVkValidationType.Unknown then
                    if HTTP.Get(Info.RedirectUri, Response).StatusCode = 200 then
                      if GetActionLinkHash(Response.DataString, Hash) then
                        if On2FA(TVkValidationType.FromString(Info.ValidationType), Code, Remember) then
                        begin
                          FormData := TStringList.Create;
                          try
                            FormData.AddPair('code', Code);
                            FormData.AddPair('remember', BoolToString(Remember));

                            HTTP.HandleRedirects := False;
                            EndResponse := HTTP.Post('https://vk.com/login?act=authcheck_code&hash=' + Hash, FormData);
                            Response.SaveToFile('D:\temp.txt');
                            while EndResponse.StatusCode = 200 do
                            begin
                              Response.LoadFromStream(EndResponse.ContentStream);
                              if CheckForCaptcha(Response.DataString, CaptchaSid) then
                              begin
                                Code := '';
                                OnCaptcha(nil, 'https://vk.com/captcha.php?sid=' + CaptchaSid, Code);
                                if not Code.IsEmpty then
                                begin
                                  EndResponse := HTTP.Post('https://vk.com/login?act=authcheck_code&hash=' + Hash + '&captcha_sid=' + CaptchaSid + '&captcha_key=' + Code, FormData);
                                end
                                else
                                  Break;
                              end
                              else
                                Break;
                            end;

                            if EndResponse.StatusCode = 302 then
                              if GetTokenFromUrl(EndResponse.HeaderValue['Location'], Hash, Url, Code) then
                              begin
                                Result := Hash;
                              end;
                          except
                            raise TVkParserException.Create(Response.DataString);
                          end;
                          FormData.Free;
                        end;
                while Info.Error = 'need_captcha' do
                begin
                  Code := '';
                  OnCaptcha(nil, Info.CaptchaImg, Code);
                  if not Code.IsEmpty then
                  begin
                    EndResponse := HTTP.Get(Url + '&captcha_sid=' + Info.CaptchaSid + '&captcha_key=' + Code, Response);
                    Info.Free;
                    Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(Response.DataString);
                    Result := Info.AccessToken;
                  end
                  else
                    Break;
                end;
                if Info.Error = 'invalid_client' then
                begin
                  raise TVkParserException.Create(Info.ErrorDescription);
                end;
              end;
            finally
              Info.Free;
            end;
          except
            raise TVkParserException.Create(Response.DataString);
          end;
        200:
          begin
            Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(Response.DataString);
            Result := Info.AccessToken;
            Info.Free;
          end;
      end;
    except
      raise TVkParserException.Create('Login request error');
    end;
  finally
    Response.Free;
    HTTP.Free;
  end;
end;

end.

