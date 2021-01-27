unit VK.Entity.Account.Banned;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkBannedList = class(TVkEntityListSimple<Integer>)
  private
    FProfiles: TArray<TVkProfile>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkBannedList}

destructor TVkBannedList.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

end.

