unit VK.Storage;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Storage;

type
  /// <summary>
  /// ������ ��� ������ � ����������� � ����������.
  /// </summary>
  TStorageController = class(TVkController)
  public
    /// <summary>
    /// ���������� �������� ����������, �������� ������� �������� � ��������� key.
    /// </summary>
    function Get(var Items: TVkStorageItems; Keys: TArrayOfString; UserId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// ���������� �������� ����������, �������� ������� �������� � ��������� key.
    /// </summary>
    function Get(var Value: string; Key: string; UserId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// ���������� �������� ���� ����������.
    /// </summary>
    function GetKeys(var Items: TVkStorageKeys; Offset: Integer = 0; Count: Integer = 100; UserId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// ��������� �������� ����������, �������� ������� �������� � ��������� key.
    /// ���������������� ���������� ��������� � ������������, � ������ �� ��� ������ ���������� ����� �������� � ��� ������. ����� ���� ������� �� ����� 1000 ���������� ��� ������� ������������. �� ����� 1000 ������� � ��� �� ������� ������������.
    /// </summary>
    function &Set(const Key, Value: string; UserId: TVkPeerId = 0): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStorageController }

function TStorageController.Get(var Items: TVkStorageItems; Keys: TArrayOfString; UserId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('keys', Keys);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  Result := Handler.Execute('storage.get', Params).GetObjects(Items);
end;

function TStorageController.&Set(const Key, Value: string; UserId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('key', Key);
  Params.Add('value', Value);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  Result := Handler.Execute('storage.set', Params).ResponseIsTrue
end;

function TStorageController.Get(var Value: string; Key: string; UserId: TVkPeerId): Boolean;
var
  Params: TParams;
  Items: TVkStorageItems;
begin
  Params.Add('key', Key);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  with Handler.Execute('storage.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Value := '';
        Items := TVkStorageItems.FromJsonString<TVkStorageItems>(ResponseAsItems);
        try
          if Length(Items.Items) > 0 then
            Value := Items.Items[0].Value;
        finally
          Items.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TStorageController.GetKeys(var Items: TVkStorageKeys; Offset, Count: Integer; UserId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  Result := Handler.Execute('storage.getKeys', Params).GetObjects(Items);
end;

end.

