unit VK.Catalog;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Catalog, VK.Entity.Catalog.Section, VK.CommonUtils;

type
  TCatalogController = class(TVkController)
  public
    /// <summary>
    /// �������� ����� ������� ������������ ��� ����������.
    /// </summary>
    /// <param name="Status">������������ ������ (����� � �����, ���� ����)</param>
    /// <param name="Id">������������� ������������ ��� ����������, ���������� � ������� �������� ����� ��������</param>
    /// <param name="IsGroup">���� ����� �������� ������ ����������</param>
    function GetAudio(var Catalog: TVkCatalog; NeedBlocks: Boolean = False): Boolean;
    function GetSection(var Section: TVkSectionData; SectionId: string; NeedBlocks: Boolean = False): Boolean;
  end;

implementation

uses
  VK.API;

{ TCatalogController }

function TCatalogController.GetAudio(var Catalog: TVkCatalog; NeedBlocks: Boolean): Boolean;
var
  Params: TParams;
begin
  if NeedBlocks then
    Params.Add('need_blocks', True);
  Params.Add('https', True);
  with Handler.Execute('catalog.getAudio', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Catalog := TVkCatalog.FromJsonString(Response);
    end;
  end;
end;

function TCatalogController.GetSection(var Section: TVkSectionData; SectionId: string; NeedBlocks: Boolean): Boolean;
var
  Params: TParams;
begin
  if NeedBlocks then
    Params.Add('need_blocks', True);
  Params.Add('https', True);
  Params.Add('section_id', SectionId);
  with Handler.Execute('catalog.getSection', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Section := TVkSectionData.FromJsonString(Response);
    end;
  end;
end;

end.

