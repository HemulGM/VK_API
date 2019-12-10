unit VK.Account;

interface

uses
  System.SysUtils, REST.Client, VK.Entity, VK.Types, VK.Account.Info, VK.Account.ProfileInfo;

type
  TAccount = class(TVKEntity)
    function GetInfo(Fields: TFields = []): TAccountInfoClass;
    function GetProfileInfo: TProfileInfoClass;
    function Ban(OwnerID: Integer): Boolean;
    function UnBan(OwnerID: Integer): Boolean;
    function ChangePassword(NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword: string): TResponse;
  end;

implementation

{ TAccount }

function TAccount.ChangePassword(NewPassword, RestoreSid, ChangePasswordHash, OldPassword: string): TResponse;
begin
  Result := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]);
end;

function TAccount.GetInfo(Fields: TFields): TAccountInfoClass;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getInfo', ['fields', FieldsToString(Fields)]);
  if Response.Success then
    Result := TAccountInfoClass.FromJsonString(Response.JSON)
  else
    Result := TAccountInfoClass.Create;
end;

function TAccount.GetProfileInfo: TProfileInfoClass;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getProfileInfo');
  if Response.Success then
    Result := TProfileInfoClass.FromJsonString(Response.JSON)
  else
    Result := TProfileInfoClass.Create;
end;

function TAccount.Ban(OwnerID: Integer): Boolean;
begin
  Result := Handler.Execute('account.ban', ['owner_id', OwnerID.ToString]).Success;
end;

function TAccount.UnBan(OwnerID: Integer): Boolean;
begin
  Result := Handler.Execute('account.unban', ['owner_id', OwnerID.ToString]).Success;
end;

end.

