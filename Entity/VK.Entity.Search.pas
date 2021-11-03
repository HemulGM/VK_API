unit VK.Entity.Search;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Common, VK.Entity.Common.List;

type
  TVkSearchItem = class(TVkEntity)
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
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TVkSearchItems = TVkEntityList<TVkSearchItem>;

implementation

uses
  VK.CommonUtils;

{TVkSearchItem}

destructor TVkSearchItem.Destroy;
begin
  if Assigned(FProfile) then
    FProfile.Free;
  if Assigned(FGroup) then
    FGroup.Free;
  inherited;
end;

end.

