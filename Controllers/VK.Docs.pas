unit VK.Docs;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Doc.Save,
  VK.Entity.Video.Save, VK.Entity.Doc, VK.Entity.Doc.Types;

type
  TVkParamsDocsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ���������.
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsDocsGet;
    /// <summary>
    /// ������ �� ���� ���������.
    /// </summary>
    function &Type(const Value: TVkDocTypeFilter): TVkParamsDocsGet;
    /// <summary>
    /// ���������� ����
    /// </summary>
    function ReturnTags(const Value: Boolean): TVkParamsDocsGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������.
    /// </summary>
    function Offset(const Value: Integer): TVkParamsDocsGet;
    /// <summary>
    /// ���������� ����������, ���������� � ������� ����� �������.
    /// </summary>
    function Count(const Value: Integer): TVkParamsDocsGet;
  end;

  TVkParamsDocsSearch = record
    List: TParams;
    /// <summary>
    /// True � ������ ����� ����������� ���������� ������������.
    /// </summary>
    function SearchOwn(const Value: Boolean): TVkParamsDocsSearch;
    /// <summary>
    /// ������ ���������� �������. ��������, ������� �������.
    /// </summary>
    function Query(const Value: string): TVkParamsDocsSearch;
    /// <summary>
    /// ���������� ����������, ���������� � ������� ����� �������.
    /// </summary>
    function Count(const Value: Integer): TVkParamsDocsSearch;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������.
    /// </summary>
    function Offset(const Value: Integer): TVkParamsDocsSearch;
    /// <summary>
    /// ���������� ����.
    /// </summary>
    function ReturnTags(const Value: Boolean): TVkParamsDocsSearch;
  end;

  TDocController = class(TVkController)
  public    /// <summary>
    /// �������� ����� ������� ��� �������� ��������� � ������ ���������.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// ��������� ����� ������� ��� �������� ��������� � ������ ���������.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����� ��� �������� �������� �� ������.
    /// </summary>
    function Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean = False): Boolean;
    /// <summary>
    /// ��������� ��������������
    /// </summary>
    function SaveAudioMessage(var Doc: TVkDocSaved; FileName: string; Title, Tags: string; PeerId: Integer = 0; ReturnTags: Boolean = False): Boolean;
    /// <summary>
    /// ���������� ����������� ���������� � ���������� ������������ ��� ����������.
    /// </summary>
    function Get(var Items: TVkDocuments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ���������� � ���������� ������������ ��� ����������.
    /// </summary>
    function Get(var Items: TVkDocuments; Params: TVkParamsDocsGet): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ���������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkDocuments; Docs: TArrayOfString; ReturnTags: Boolean): Boolean; overload;
    /// <summary>
    /// �������� �������� � ��������� �������� ������������.
    /// </summary>
    function Add(var Id: Integer; OwnerId, DocId: Integer; AccessKey: string): Boolean; overload;
    /// <summary>
    /// ������� �������� ������������ ��� ������.
    /// </summary>
    function Delete(OwnerId, DocId: Integer): Boolean; overload;
    /// <summary>
    /// ����������� �������� ������������ ��� ������.
    /// </summary>
    function Edit(OwnerId, DocId: Integer; Title: string; Tags: TArrayOfString = []): Boolean; overload;
    /// <summary>
    /// ���������� ��������� ��� ������������ ���� ����������.
    /// </summary>
    function GetTypes(var Items: TVkDocTypes; OwnerId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ����������.
    /// </summary>
    function GetUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� � ����� ������������, ��� ����������� �������� ��������� �� ����� ��� ������ ����������.
    /// </summary>
    function GetWallUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ������ �� ����������.
    /// </summary>
    function Search(var Items: TVkDocuments; Params: TVkParamsDocsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TDocController }

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer): Boolean;
var
  Params: TParams;
begin
  case&Type of
    TVkDocUploadType.Doc:
      Params.Add('type', 'doc');
    TVkDocUploadType.AudioMessage:
      Params.Add('type', 'audio_message');
  end;
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  Result := Handler.Execute('docs.getMessagesUploadServer', Params).GetValue('upload_url', UploadUrl)
end;

function TDocController.Get(var Items: TVkDocuments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('docs.get', Params).GetObject(Items);
end;

function TDocController.Add(var Id: Integer; OwnerId, DocId: Integer; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('doc_id', DocId);
  if not AccessKey.IsEmpty then
    Params.Add('access_key', AccessKey);
  Result := Handler.Execute('docs.Add', Params).ResponseAsInt(Id);
end;

function TDocController.Delete(OwnerId, DocId: Integer): Boolean;
begin
  Result := Handler.Execute('docs.delete', [['doc_id', DocId.ToString], ['owner_id', OwnerId.ToString]]).ResponseIsTrue;
end;

function TDocController.Edit(OwnerId, DocId: Integer; Title: string; Tags: TArrayOfString): Boolean;
var
  Params: TParams;
begin
  Params.Add('doc_id', DocId);
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  Params.Add('tags', Tags);
  Result := Handler.Execute('docs.edit', Params).ResponseIsTrue;
end;

function TDocController.Get(var Items: TVkDocuments; Params: TVkParamsDocsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TDocController.GetById(var Items: TVkDocuments; Docs: TArrayOfString; ReturnTags: Boolean): Boolean;
begin
  Result := Handler.Execute('docs.getById', [
    ['docs', Docs.ToString],
    ['return_tags', BoolToString(ReturnTags)]]).
    GetObject(Items);
end;

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean;
var
  Params: TParams;
begin
  case&Type of
    TVkDocUploadType.Doc:
      Params.Add('type', 'doc');
    TVkDocUploadType.AudioMessage:
      Params.Add('type', 'audio_message');
  end;
  Result := Handler.Execute('docs.getMessagesUploadServer', Params).GetValue('upload_url', UploadUrl);
end;

function TDocController.GetTypes(var Items: TVkDocTypes; OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('docs.getTypes', ['owner_id', OwnerId.ToString]).GetObject(Items);
end;

function TDocController.GetUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('docs.getUploadServer', ['group_id', GroupId.ToString]).GetValue('upload_url', UploadUrl);
end;

function TDocController.GetWallUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('docs.getWallUploadServer', ['group_id', GroupId.ToString]).GetValue('upload_url', UploadUrl);
end;

function TDocController.Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('file', FileData);
  Params.Add('title', Title);
  Params.Add('tags', Tags);
  if ReturnTags then
    Params.Add('return_tags', BoolToString(ReturnTags));
  Result := Handler.Execute('docs.save', Params).GetObject(Doc);
end;

function TDocController.SaveAudioMessage(var Doc: TVkDocSaved; FileName, Title, Tags: string; PeerId: Integer; ReturnTags: Boolean): Boolean;
var
  Url, Response: string;
begin
  Result := False;
  if GetMessagesUploadServer(Url, TVkDocUploadType.AudioMessage, PeerId) then
  begin
    if TCustomVK(VK).Upload(Url, FileName, Response) then
      Result := Save(Doc, Response, Title, Tags);
  end;
end;

function TDocController.Search(var Items: TVkDocuments; Params: TVkParamsDocsSearch): Boolean;
begin
  Result := Handler.Execute('docs.search', Params.List).GetObject(Items);
end;

{ TVkParamsDocsGet }

function TVkParamsDocsGet.Count(const Value: Integer): TVkParamsDocsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDocsGet.Offset(const Value: Integer): TVkParamsDocsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsDocsGet.OwnerId(const Value: Integer): TVkParamsDocsGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsDocsGet.ReturnTags(const Value: Boolean): TVkParamsDocsGet;
begin
  List.Add('return_tags', Value);
  Result := Self;
end;

function TVkParamsDocsGet.&Type(const Value: TVkDocTypeFilter): TVkParamsDocsGet;
begin
  List.Add('type', Ord(Value));
  Result := Self;
end;

{ TVkParamsDocsSearch }

function TVkParamsDocsSearch.Count(const Value: Integer): TVkParamsDocsSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.Offset(const Value: Integer): TVkParamsDocsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.Query(const Value: string): TVkParamsDocsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.ReturnTags(const Value: Boolean): TVkParamsDocsSearch;
begin
  List.Add('return_tags', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.SearchOwn(const Value: Boolean): TVkParamsDocsSearch;
begin
  List.Add('search_own', Value);
  Result := Self;
end;

end.

