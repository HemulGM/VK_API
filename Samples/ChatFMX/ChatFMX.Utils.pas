unit ChatFMX.Utils;

interface

uses
  VK.Types;

function AttachmentToText(const Value: TVkAttachmentType): string;

function MessageActionTypeToText(const Value: TVkMessageActionType): string;

implementation

function MessageActionTypeToText(const Value: TVkMessageActionType): string;
begin
  case Value of
    TVkMessageActionType.ChatPhotoUpdate:
      Result := 'обновлена фотография беседы';
    TVkMessageActionType.ChatPhotoRemove:
      Result := 'удалена фотография беседы';
    TVkMessageActionType.ChatCreate:
      Result := 'создана беседа';
    TVkMessageActionType.ChatTitleUpdate:
      Result := 'обновлено название беседы';
    TVkMessageActionType.ChatInviteUser:
      Result := 'приглашен пользователь';
    TVkMessageActionType.ChatKickUser:
      Result := 'исключен пользователь';
    TVkMessageActionType.ChatPinMessage:
      Result := 'закреплено сообщение';
    TVkMessageActionType.ChatUnpinMessage:
      Result := 'откреплено сообщение';
    TVkMessageActionType.ChatInviteUserByLink:
      Result := 'пользователь присоединился к беседе по ссылке';
  else
    Result := '';
  end;
end;

function AttachmentToText(const Value: TVkAttachmentType): string;
begin
  case Value of
    TVkAttachmentType.Photo:
      Result := 'Фотография';
    TVkAttachmentType.Video:
      Result := 'Видео';
    TVkAttachmentType.Audio:
      Result := 'Аудиозапись';
    TVkAttachmentType.Doc:
      Result := 'Файл';
    TVkAttachmentType.Link:
      Result := 'Ссылка';
    TVkAttachmentType.Market:
      Result := 'Товар';
    TVkAttachmentType.MarketAlbum:
      Result := 'Товары';
    TVkAttachmentType.Wall:
      Result := 'Запись на стене';
    TVkAttachmentType.WallReply:
      Result := 'Запись на стене';
    TVkAttachmentType.Sticker:
      Result := 'Стикер';
    TVkAttachmentType.Gift:
      Result := 'Подарок';
    TVkAttachmentType.Call:
      Result := 'Звонок';
    TVkAttachmentType.AudioMessage:
      Result := 'Аудиосообщение';
    TVkAttachmentType.PostedPhoto:
      Result := 'Фотография';
    TVkAttachmentType.Graffiti:
      Result := 'Граффити';
    TVkAttachmentType.Note:
      Result := 'Заметка';
    TVkAttachmentType.App:
      Result := 'Приложение';
    TVkAttachmentType.Poll:
      Result := 'Опрос';
    TVkAttachmentType.Page:
      Result := 'Страница';
    TVkAttachmentType.Album:
      Result := 'Альбом';
    TVkAttachmentType.PhotosList:
      Result := 'Фотографии';
    TVkAttachmentType.PrettyCards:
      Result := 'Карточки';
    TVkAttachmentType.Event:
      Result := 'Событие';
    TVkAttachmentType.MoneyTransfer:
      Result := 'Денежный перевод';
  else
    Result := '';
  end;
end;

end.

