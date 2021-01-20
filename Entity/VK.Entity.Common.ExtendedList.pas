unit VK.Entity.Common.ExtendedList;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common,
  VK.Entity.Common.List, VK.Entity.Profile, VK.Entity.Group;

type
  TVkEntityExtendedList<T: TVkEntity> = class(TVkEntityList<T>)
  protected
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
  end;

  TVkEntityExtendedSimpleList<T> = class(TVkEntityListSimple<T>)
  protected
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
  end;

  TVkIdList = TVkEntityListSimple<Integer>;

  TVkBasicIndexItems = TVkEntityListSimple<Integer>;

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

{ TVkEntityExtendedSimpleList<T> }

destructor TVkEntityExtendedSimpleList<T>.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
end;

end.

