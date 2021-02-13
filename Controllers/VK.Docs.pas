unit VK.Docs;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Doc.Save, VK.Entity.Video.Save, VK.Entity.Doc, VK.Entity.Doc.Types;

type
  TVkDocUploadType = (dutDoc, dutAudioMessage);

  TVkDocTypeFilter = (vdtAll, vdtText, vdtArchives, vdtGIF, vdtPictures, vdtAudios, vdtVideos, vdtBooks, vdtOther);

  TVkParamsDocsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ���������.
    /// </summary>
    function OwnerId(Value: Integer): TVkParamsDocsGet;
    /// <summary>
    /// ������ �� ���� ���������.
    /// </summary>
    function &Type(Value: TVkDocTypeFilter): TVkParamsDocsGet;
    /// <summary>
    /// ���������� ����
    /// </summary>
    function ReturnTags(Value: Boolean): TVkParamsDocsGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������.
    /// </summary>
    function Offset(Value: Integer): TVkParamsDocsGet;
    /// <summary>
    /// ���������� ����������, ���������� � ������� ����� �������.
    /// </summary>
    function Count(Value: Integer): TVkParamsDocsGet;
  end;

  TVkParamsDocsSearch = record
    List: TParams;
    /// <summary>
    /// True � ������ ����� ����������� ���������� ������������.
    /// </summary>
    function SearchOwn(Value: Boolean): TVkParamsDocsSearch;
    /// <summary>
    /// ������ ���������� �������. ��������, ������� �������.
    /// </summary>
    function Query(Value: string): TVkParamsDocsSearch;
    /// <summary>
    /// ���������� ����������, ���������� � ������� ����� �������.
    /// </summary>
    function Count(Value: Integer): TVkParamsDocsSearch;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������.
    /// </summary>
    function Offset(Value: Integer): TVkParamsDocsSearch;
    /// <summary>
    /// ���������� ����.
    /// </summary>
    function ReturnTags(Value: Boolean): TVkParamsDocsSearch;
  end;

  TDocController = class(TVkController)
  public
    /// <summary>
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
    dutDoc:
      Params.Add('type', 'doc');
    dutAudioMessage:
      Params.Add('type', 'audio_message');
  end;
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  Result := Handler.Execute('docs.getMessagesUploadServer', Params).GetValue('upload_url', UploadUrl)
end;

function TDocController.Get(var Items: TVkDocuments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('docs.get', Params).GetObject<TVkDocuments>(Items);
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
  with Handler.Execute('docs.delete', [['doc_id', DocId.ToString], ['owner_id', OwnerId.ToString]]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TDocController.Edit(OwnerId, DocId: Integer; Title: string; Tags: TArrayOfString): Boolean;
var
  Params: TParams;
begin
  Params.Add('doc_id', DocId);
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  Params.Add('tags', Tags);
  with Handler.Execute('docs.edit', Params) do
  begin
    Result := Success and ResponseIsTrue;
  end;
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
    GetObject<TVkDocuments>(Items);
end;

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean;
var
  Params: TParams;
begin
  case&Type of
    dutDoc:
      Params.Add('type', 'doc');
    dutAudioMessage:
      Params.Add('type', 'audio_message');
  end;
  Result := Handler.Execute('docs.getMessagesUploadServer', Params).GetValue('upload_url', UploadUrl);
end;

function TDocController.GetTypes(var Items: TVkDocTypes; OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('docs.getTypes', ['owner_id', OwnerId.ToString]).GetObject<TVkDocTypes>(Items);
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
  Result := Handler.Execute('docs.save', Params).GetObject<TVkDocSaved>(Doc);
end;

function TDocController.SaveAudioMessage(var Doc: TVkDocSaved; FileName, Title, Tags: string; PeerId: Integer; ReturnTags: Boolean): Boolean;
var
  Url, Response: string;
begin
  Result := False;
  if GetMessagesUploadServer(Url, dutAudioMessage, PeerId) then
  begin
    try
      if TCustomVK(VK).Upload(Url, FileName, Response) then
        Result := Save(Doc, Response, Title, Tags)
      else
        TCustomVK(VK).DoError(Self, TVkException.Create(Response), -1, Response);
    except
      Result := False;
    end;
  end;
end;

function TDocController.Search(var Items: TVkDocuments; Params: TVkParamsDocsSearch): Boolean;
begin
  Result := Handler.Execute('docs.search', Params.List).GetObject<TVkDocuments>(Items);
end;

{ TVkParamsDocsGet }

function TVkParamsDocsGet.Count(Value: Integer): TVkParamsDocsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDocsGet.Offset(Value: Integer): TVkParamsDocsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsDocsGet.OwnerId(Value: Integer): TVkParamsDocsGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsDocsGet.ReturnTags(Value: Boolean): TVkParamsDocsGet;
begin
  List.Add('return_tags', Value);
  Result := Self;
end;

function TVkParamsDocsGet.&Type(Value: TVkDocTypeFilter): TVkParamsDocsGet;
begin
  List.Add('type', Ord(Value));
  Result := Self;
end;

{ TVkParamsDocsSearch }

function TVkParamsDocsSearch.Count(Value: Integer): TVkParamsDocsSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.Offset(Value: Integer): TVkParamsDocsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.Query(Value: string): TVkParamsDocsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.ReturnTags(Value: Boolean): TVkParamsDocsSearch;
begin
  List.Add('return_tags', Value);
  Result := Self;
end;

function TVkParamsDocsSearch.SearchOwn(Value: Boolean): TVkParamsDocsSearch;
begin
  List.Add('search_own', Value);
  Result := Self;
end;

end.

