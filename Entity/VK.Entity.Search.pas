unit VK.Entity.Search;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.User, VK.Entity.Group;

type
  TVkSearchItem = class
  private
    FDescription: string;
    FProfile: TVkUser;
    FSection: string;
    FType: string;
    FGroup: TVkGroup;
    FGlobal: Boolean;
  public
    property Description: string read FDescription write FDescription;
    property Profile: TVkUser read FProfile write FProfile;
    property Group: TVkGroup read FGroup write FGroup;
    /// <summary>
    /// ��������� �������� ��� ���������:
    /// groups � ������;
    /// events � �����������;
    /// publics � ��������� ��������.
    /// ��������� �������� ��� ��������:
    /// correspondents � ����������� �� ���������;
    /// people � ���������� ������������;
    /// friends � ������;
    /// mutual_friends � ������������, ������� ����� ������ � �������.
    /// </summary>
    property Section: string read FSection write FSection;
    /// <summary>
    /// ���� ������������, ���� ������ ��� ������ � ���������� ������, ������ �������� 1
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
  for var LitemsItem in FItems do
    LitemsItem.Free;
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

