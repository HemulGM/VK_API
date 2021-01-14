unit VK.Catalog;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Catalog, VK.Entity.Catalog.Section, VK.CommonUtils;

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
  Result := Handler.Execute('catalog.getAudio', Params).GetObject<TVkCatalog>(Catalog);
end;

function TCatalogController.GetSection(var Section: TVkSectionData; SectionId: string; NeedBlocks: Boolean): Boolean;
var
  Params: TParams;
begin
  if NeedBlocks then
    Params.Add('need_blocks', True);
  Params.Add('https', True);
  Params.Add('section_id', SectionId);
  Result := Handler.Execute('catalog.getSection', Params).GetObject<TVkSectionData>(Section);
end;

end.

