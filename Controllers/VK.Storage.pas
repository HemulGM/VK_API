unit VK.Storage;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Storage;

type
  /// <summary>
  /// ������ ��� ������ � ����������� � ����������.
  /// </summary>
  TStorageController = class(TVkController)
  public
    /// <summary>
    /// ���������� �������� ����������, �������� ������� �������� � ��������� key.
    /// </summary>
    function Get(var Items: TVkStorageItems; Keys: TArrayOfString; UserId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� �������� ����������, �������� ������� �������� � ��������� key.
    /// </summary>
    function Get(var Value: string; Key: string; UserId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� �������� ���� ����������.
    /// </summary>
    function GetKeys(var Items: TVkStorageKeys; Offset: Integer = 0; Count: Integer = 100; UserId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� �������� ����������, �������� ������� �������� � ��������� key.
    /// ���������������� ���������� ��������� � ������������, � ������ �� ��� ������ ���������� ����� �������� � ��� ������. ����� ���� ������� �� ����� 1000 ���������� ��� ������� ������������. �� ����� 1000 ������� � ��� �� ������� ������������.
    /// </summary>
    function &Set(const Key, Value: string; UserId: Integer = 0): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStorageController }

function TStorageController.Get(var Items: TVkStorageItems; Keys: TArrayOfString; UserId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('keys', Keys);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  with Handler.Execute('storage.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkStorageItems.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TStorageController.&Set(const Key, Value: string; UserId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('key', Key);
  Params.Add('value', Value);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  with Handler.Execute('storage.set', Params) do
    Result := Success and ResponseIsTrue
end;

function TStorageController.Get(var Value: string; Key: string; UserId: Integer): Boolean;
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
        Items := TVkStorageItems.FromJsonString(AppendItemsTag(Response));
        if Length(Items.Items) > 0 then
          Value := Items.Items[0].Value;
        Items.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TStorageController.GetKeys(var Items: TVkStorageKeys; Offset, Count, UserId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  with Handler.Execute('storage.getKeys', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkStorageKeys.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

end.

