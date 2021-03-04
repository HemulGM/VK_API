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
    //function GetAudioSearch(): Boolean;
    function GetChart(var Items: TVkSectionData): Boolean;
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
  Result := Handler.Execute('catalog.getAudio', Params).GetObject(Catalog);
end;

function TCatalogController.GetChart(var Items: TVkSectionData): Boolean;
var
  Catalog: TVkCatalog;
  Section: TVkSectionData;
  Block: Integer;
begin
  Result := False;
  if GetAudio(Catalog, True) then
  begin
    try
      if GetSection(Section, Catalog.Catalog.Sections[1].Id, False) then
      begin
        try
          Block := Section.Section.FindBlock('music_audios', 'music_chart_triple_stacked_slider');
          Result := (Block <> -1) and GetSection(Items, Section.Section.Blocks[Block].Id, False);
        finally
          Section.Free;
        end;
      end;
    finally
      Catalog.Free;
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
  Result := Handler.Execute('catalog.getSection', Params).GetObject(Section);
end;

end.

