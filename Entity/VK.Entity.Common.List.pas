unit VK.Entity.Common.List;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Profile,
  VK.Entity.Group;

type
  TVkEntityExtendedList<T: TVkEntity> = class(TVkEntityList<T>)
  private
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkEntityExtendedList<T> }

destructor TVkEntityExtendedList<T>.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
end;

end.

