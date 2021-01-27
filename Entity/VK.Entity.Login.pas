unit VK.Entity.Login;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common;

type
  TVkLoginInfo = class(TVkEntity)
  private
    FError: string;
    FError_description: string;
    FPhone_mask: string;
    FRedirect_uri: string;
    FValidation_sid: string;
    FValidation_type: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FExpires_in: TDateTime;
    FUser_id: Integer;
    FAccess_token: string;
    FCaptcha_img: string;
    FCaptcha_sid: string;
  public
    property Error: string read FError write FError;
    property ErrorDescription: string read FError_description write FError_description;
    property PhoneMask: string read FPhone_mask write FPhone_mask;
    property RedirectUri: string read FRedirect_uri write FRedirect_uri;
    property ValidationSid: string read FValidation_sid write FValidation_sid;
    property ValidationType: string read FValidation_type write FValidation_type;
    property AccessToken: string read FAccess_token write FAccess_token;
    property ExpiresIn: TDateTime read FExpires_in write FExpires_in;
    property UserId: Integer read FUser_id write FUser_id;
    property CaptchaImg: string read FCaptcha_img write FCaptcha_img;
    property CaptchaSid: string read FCaptcha_sid write FCaptcha_sid;
  end;

implementation

end.

