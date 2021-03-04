unit VK.Entity.Message.Templates;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Types, VK.Entity.Keyboard, VK.Wrap.Interceptors;

type
  TVkMessageTemplate = class(TVkEntity)
  private
    FType: string;
  public
    property&Type: string read FType write FType;  //carousel
  end;

  TVkMessageTemplateCarouselAction = class
  private
    FType: string;
    FLink: string;
  public
    property Link: string read FLink write FLink;
    property&Type: string read FType write FType; //open_link, open_photo
  end;

  TVkMessageTemplateCarouselElement = class
  private
    FPhoto_id: string;
    FTitle: string;
    FDescription: string;
    FAction: TVkMessageTemplateCarouselAction;
    FButtons: TVkKeyboardButtons;
  public
    /// <summary>
    /// Заголовок, максимум 80 символов
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// Подзаголовок, максимум 80 символов
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Id изображения, которое надо прикрепить.
    /// Пропорции изображения: 13/8;
    /// Минимальный размер: 221х136;
    /// Загрузка изображений для карусели происходит также, как и загрузка изображений ботами в сообщениях.
    /// </summary>
    property PhotoId: string read FPhoto_id write FPhoto_id;
    /// <summary>
    /// Массив с кнопками — можно передать любые кнопки, которые описаны в разделе «Клавиатуры для ботов» → «Структура данных». Один элемент карусели может содержать не больше 3-х кнопок
    /// </summary>
    property Buttons: TVkKeyboardButtons read FButtons write FButtons;
    /// <summary>
    /// Объект, описывающий действие, которое необходимо выполнить при нажатии на элемент карусели
    ///  Поддерживается два действия:
    /// open_link - открыть ссылку из поля "link".
    /// open_photo - открыть фото текущего элемента карусели.
    /// </summary>
    property Action: TVkMessageTemplateCarouselAction read FAction write FAction;
    destructor Destroy; override;
  end;

  TVkMessageTemplateCarousel = class(TVkMessageTemplate)
  private
    FElements: TArray<TVkMessageTemplateCarouselElement>;
  public
    /// <summary>
    /// Содержит список элементов карусели
    /// </summary>
    property Elements: TArray<TVkMessageTemplateCarouselElement> read FElements write FElements;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkMessageTemplateCarouselElement }

destructor TVkMessageTemplateCarouselElement.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkKeyboardButton>(FButtons);
  inherited;
end;

{ TVkMessageTemplateCarousel }

constructor TVkMessageTemplateCarousel.Create;
begin
  inherited;
  FType := 'carousel';
end;

destructor TVkMessageTemplateCarousel.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkMessageTemplateCarouselElement>(FElements);
  inherited;
end;

end.

