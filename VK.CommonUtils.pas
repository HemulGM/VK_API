unit VK.CommonUtils;

interface

{$INCLUDE include.inc}

uses
  System.Classes, System.Net.HttpClient;

type
  TArrayHelp = class
    class procedure FreeArrayOfObject<T: class>(var Target: TArray<T>); overload;
    class procedure FreeArrayOfArrayOfObject<T: class>(var Target: TArray<TArray<T>>); overload;
  end;

function BoolToString(Value: Boolean): string; overload;

function BoolToInt(Value: Boolean): Integer; overload;

function BoolToString(Value: Boolean; TrueValue, FalseValue: string): string; overload;

function GetTokenFromUrl(const Url: string; var Token, ChangePasswordHash, TokenExpiry: string): Boolean;

function GetActionLinkHash(const Html: string; var Hash: string): Boolean;

function CheckForCaptcha(const Html: string; var CaptchaUrl: string): Boolean;

function IndexInt(const Value: Integer; const Items: array of Integer): Integer;

implementation

uses
  System.DateUtils, System.Math, System.StrUtils, System.SysUtils,
  System.IOUtils;

function IndexInt(const Value: Integer; const Items: array of Integer): Integer;
var
  i: integer;
begin
  for i := Low(Items) to High(Items) do
    if Value = Items[i] then
      Exit(i);
  Result := -1;
end;

function GetTokenFromUrl(const Url: string; var Token, ChangePasswordHash, TokenExpiry: string): Boolean;
var
  i: integer;
  Str: string;
  Params: TStringList;
begin
  i := Pos('#access_token=', Url);
  if (i = 0) then
    i := Pos('&access_token=', Url);
  if i <> 0 then
  begin
    Str := Url;
    Delete(Str, 1, i);
    Params := TStringList.Create;
    try
      Params.Delimiter := '&';
      Params.DelimitedText := Str;
      ChangePasswordHash := Params.Values['change_password_hash'];
      Token := Params.Values['access_token'];
      TokenExpiry := Params.Values['expires_in'];
    finally
      Params.Free;
    end;
  end;
  Result := not Token.IsEmpty;
end;

function GetActionLinkHash(const Html: string; var Hash: string): Boolean;
const
  Pattern = 'action="/login?act=authcheck_code&hash=';
var
  i: Integer;
begin
  Hash := '';
  i := Pos(Pattern, Html);
  if i > 0 then
  begin
    Hash := Copy(Html, i + Pattern.Length, 150);
    i := Pos('"', Hash);
    if i > 0 then
      Hash := Copy(Hash, 1, i - 1);
  end;
  Result := not Hash.IsEmpty;
end;

function CheckForCaptcha(const Html: string; var CaptchaUrl: string): Boolean;
const
  Pattern = 'img src="/captcha.php?sid=';
var
  i: Integer;
begin
  CaptchaUrl := '';
  i := Pos(Pattern, Html);
  if i > 0 then
  begin
    CaptchaUrl := Copy(Html, i + Pattern.Length, 150);
    i := Pos('"', CaptchaUrl);
    if i > 0 then
      CaptchaUrl := Copy(CaptchaUrl, 1, i - 1);
  end;
  Result := not CaptchaUrl.IsEmpty;
end;

function BoolToInt(Value: Boolean): Integer; overload;
begin
  Result := IfThen(Value, 1, 0);
end;

function BoolToString(Value: Boolean): string;
begin
  Result := IfThen(Value, '1', '0');
end;

function BoolToString(Value: Boolean; TrueValue, FalseValue: string): string;
begin
  Result := IfThen(Value, TrueValue, FalseValue);
end;

class procedure TArrayHelp.FreeArrayOfObject<T>(var Target: TArray<T>);
  {$IFNDEF AUTOREFCOUNT}
var
  Item: T;
  {$ENDIF}
begin
  {$IFNDEF AUTOREFCOUNT}
  for Item in Target do
    if Assigned(Item) then
      Item.Free;
  {$ENDIF}
  SetLength(Target, 0);
end;

class procedure TArrayHelp.FreeArrayOfArrayOfObject<T>(var Target: TArray<TArray<T>>);
  {$IFNDEF AUTOREFCOUNT}
var
  Item: T;
  Items: TArray<T>;
  {$ENDIF}
begin
  {$IFNDEF AUTOREFCOUNT}
  for Items in Target do
    for Item in Items do
      if Assigned(Item) then
        Item.Free;
  {$ENDIF}
  SetLength(Target, 0);
end;

end.


