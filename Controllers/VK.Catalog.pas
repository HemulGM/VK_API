unit VK.Catalog;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Catalog,
  VK.Entity.Catalog.Section, VK.CommonUtils;

type
  TCatalogController = class(TVkController)
  public
    /// <summary>
    /// Получает текст статуса пользователя или сообщества.
    /// </summary>
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
      try
        Catalog := TVkCatalog.FromJsonString(Response);
      except
        Result := False;
      end;
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
      try
        Section := TVkSectionData.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

end.

