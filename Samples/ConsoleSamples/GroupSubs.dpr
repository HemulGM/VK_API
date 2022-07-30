program GroupSubs;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  VK.API,
  VK.Groups,
  VK.Entity.Profile;

const
  API_KEY = {$INCLUDE D:\Projects\HH_Delphi_Stat\vk_bot_key.api_key};

begin
  try
    var VK := VKAPI.Create(API_KEY);
    try
      var Members: TVkProfiles;
      var Params: TVkParamsGroupsGetMembers;
      if VK.Groups.GetMembers(Members, Params.GroupId(192458090)) then
      try
        for var Item in Members.Items do
          Writeln(Item.FullName);
      finally
        Members.Free;
      end;
    finally
      VK.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.

