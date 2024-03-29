﻿unit VK.Entity.Common.ExtendedList;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common,
  VK.Entity.Common.List, VK.Entity.Profile, VK.Entity.Group, VK.Types,
  VK.Entity.Photo;

type
  IExtended = interface
    ['{CD673BC9-1E4C-4963-A300-1900905401E5}']
    function GetProfileById(const Id: TVkPeerId; out Profile: TVkProfile): Boolean;
    function GetGroupById(const Id: TVkPeerId; out Group: TVkGroup): Boolean;
    function ProfileCount: Integer;
    function Profile(const Index: Integer): TVkProfile;
  end;

  /// <summary>
  /// Базовый класс список со списком профилей и групп (с освобождением элементов списка)
  /// </summary>
  TVkEntityExtendedList<T: TVkEntity> = class(TVkEntityList<T>, IExtended)
  protected
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    function GetProfileById(const Id: TVkPeerId; out Profile: TVkProfile): Boolean;
    function GetGroupById(const Id: TVkPeerId; out Group: TVkGroup): Boolean;
    function ProfileCount: Integer;
    function Profile(const Index: Integer): TVkProfile;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Базовый класс список со списком профилей и групп (без освобождения элементов списка)
  /// </summary>
  TVkEntityExtendedSimpleList<T> = class(TVkEntityListSimple<T>)
  protected
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    function GetProfileById(const Id: TVkPeerId; out Profile: TVkProfile): Boolean;
    function GetGroupById(const Id: TVkPeerId; out Group: TVkGroup): Boolean;
    destructor Destroy; override;
  end;

  TVkPhotos = class(TVkEntityExtendedList<TVkPhoto>)
  public
    function ToAttachments: TAttachmentArray;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPhotos}

function TVkPhotos.ToAttachments: TAttachmentArray;
var
  i: Integer;
begin
  SetLength(Result, Length(FItems));
  for i := Low(FItems) to High(FItems) do
    Result[i] := FItems[i].ToAttachment;
end;

{ TVkEntityExtendedList<T> }

destructor TVkEntityExtendedList<T>.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
end;

function TVkEntityExtendedList<T>.GetGroupById(const Id: TVkPeerId; out Group: TVkGroup): Boolean;
begin
  for var Item in FGroups do
    if Item.Id = Abs(Id) then
    begin
      Group := Item;
      Exit(True);
    end;
  Result := False;
end;

function TVkEntityExtendedList<T>.GetProfileById(const Id: TVkPeerId; out Profile: TVkProfile): Boolean;
begin
  for var Item in FProfiles do
    if Item.Id = Id then
    begin
      Profile := Item;
      Exit(True);
    end;
  Result := False;
end;

function TVkEntityExtendedList<T>.Profile(const Index: Integer): TVkProfile;
begin
  Result := FProfiles[Index];
end;

function TVkEntityExtendedList<T>.ProfileCount: Integer;
begin
  Result := Length(FProfiles);
end;

{ TVkEntityExtendedSimpleList<T> }

destructor TVkEntityExtendedSimpleList<T>.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  inherited;
end;

function TVkEntityExtendedSimpleList<T>.GetGroupById(const Id: TVkPeerId; out Group: TVkGroup): Boolean;
begin
  for var Item in FGroups do
    if Item.Id = Abs(Id) then
    begin
      Group := Item;
      Exit(True);
    end;
  Result := False;
end;

function TVkEntityExtendedSimpleList<T>.GetProfileById(const Id: TVkPeerId; out Profile: TVkProfile): Boolean;
begin
  for var Item in FProfiles do
    if Item.Id = Id then
    begin
      Profile := Item;
      Exit(True);
    end;
  Result := False;
end;

end.

