unit VK.Entity.Account.Banned;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Common;

type
  TVkBannedList = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<Integer>;
    FProfiles: TArray<TVkProfile>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
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

