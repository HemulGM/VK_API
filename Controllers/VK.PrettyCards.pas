unit VK.PrettyCards;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types;

type
  { TODO -oМалинин Геннадий -c : Доделать методы PrettyCard 26.02.2021 14:10:18 }
  /// <summary>
  /// Карусель — набор карточек, которые прикрепляются к записи. К каждой карточке можно добавить название и короткое описание, изображение, кнопку. Также можно установить две цены — старую и новую — например, чтобы показать скидку.
  /// На текущий момент карусель поддерживается только в скрытых рекламных записях (см. wall.postAdsStealth).
  /// </summary>
  TPrettyCardsController = class(TVkController)
  public
    /// <summary>
    /// Создаёт карточку карусели.
    /// На текущий момент карусель поддерживается только в скрытых рекламных записях (см. метод wall.postAdsStealth)
    /// </summary>
   { function &Create(var Items: TVkPodcasts): Boolean;   }
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPrettyCardsController }
  {
function TPrettyCardsController.GetPopular(var Items: TVkPodcasts): Boolean;
begin
  with Handler.Execute('podcasts.getPopular') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPodcasts.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;  }

end.

