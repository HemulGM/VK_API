unit VK.Entity.Search;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group;

type
  TVkSearchItem = class
  private
    FDescription: string;
    FProfile: TVkProfile;
    FSection: string;
    FType: string;
    FGroup: TVkGroup;
    FGlobal: Boolean;
  public
    property Description: string read FDescription write FDescription;
    property Profile: TVkProfile read FProfile write FProfile;
    property Group: TVkGroup read FGroup write FGroup;
    /// <summary>
    /// ¬озможные значени€ дл€ сообществ:
    /// groups Ч группы;
    /// events Ч меропри€ти€;
    /// publics Ч публичные страницы.
    /// ¬озможные значени€ дл€ профилей:
    /// correspondents Ч собеседники по переписке;
    /// people Ч попул€рные пользователи;
    /// friends Ч друзь€;
    /// mutual_friends Ч пользователи, имеющие общих друзей с текущим.
    /// </summary>
    property Section: string read FSection write FSection;
    /// <summary>
    /// ѕоле возвращаетс€, если объект был найден в глобальном поиске, всегда содержит 1
    /// </summary>
    property Global: Boolean read FGlobal write FGlobal;
    property&Type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSearchItem;
  end;

  TVkSearchItems = class
  private
    FCount: Integer;
    FItems: TArray<TVkSearchItem>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkSearchItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSearchItems;
  end;

implementation

uses
  VK.CommonUtils;

{TVkSearchItem}

constructor TVkSearchItem.Create;
begin
  inherited;
end;

destructor TVkSearchItem.Destroy;
begin
  if Assigned(FProfile) then
    FProfile.Free;
  if Assigned(FGroup) then
    FGroup.Free;
  inherited;
end;

function TVkSearchItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSearchItem.FromJsonString(AJsonString: string): TVkSearchItem;
begin
  result := TJson.JsonToObject<TVkSearchItem>(AJsonString)
end;

{TVkSearchItems}

destructor TVkSearchItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSearchItem>(FItems);
  inherited;
end;

function TVkSearchItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSearchItems.FromJsonString(AJsonString: string): TVkSearchItems;
begin
  result := TJson.JsonToObject<TVkSearchItems>(AJsonString)
end;

end.

