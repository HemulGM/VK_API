unit ChatFMX.Utils;

interface

uses
  VK.Types, System.SysUtils, VK.Entity.Message;

function AttachmentToText(const Value: TVkAttachmentType): string;

function WordOfCount(const Count: Integer; const Words: TArrayOfString): string;

function WordOfSex(const Sex: TVkSex; const Words: TArrayOfString): string;

function HumanDateTime(Value: TDateTime; ShowTime: Boolean = False; ShortDate: Boolean = False): string;

function MessageActionToText(const Value: TVkMessageAction; FromId: TVkPeerId; const FromText, MemberText: string): string;

function ParseMention(const Value: string): string;

implementation

uses
  System.DateUtils, System.StrUtils, System.RegularExpressions;

function ParseMention(const Value: string): string;
const
  Pattern1 = '\[(\w+)\|(.+?)\]';
  Pattern2 = '(@\w+)\ \((.+?)\)';
begin
  Result := Value;

  //[id5555555|UserName]
  var RegExp := TRegEx.Create(Pattern1);
  var Matches := RegExp.Matches(Value);
  for var Match in Matches do
    if Match.Groups.Count > 2 then
      Result := Result.Replace(Match.Value, Match.Groups[2].Value, [rfReplaceAll]);

  //@username (User Name)
  RegExp := TRegEx.Create(Pattern2);
  Matches := RegExp.Matches(Value);
  for var Match in Matches do
    if Match.Groups.Count > 2 then
      Result := Result.Replace(Match.Value, Match.Groups[2].Value, [rfReplaceAll]);
end;

function WordOfCount(const Count: Integer; const Words: TArrayOfString): string;
begin
  if Length(Words) < 3 then
    Exit('');
  var Num := Count.ToString;
  case Num[High(Num)] of
    '1':
      Exit(Words[0]);
    '2', '3', '4':
      Exit(Words[1]);
    '5', '6', '7', '8', '9', '0':
      Exit(Words[2]);
  end;
end;

function WordOfSex(const Sex: TVkSex; const Words: TArrayOfString): string;
begin
  if Length(Words) < 2 then
    Exit('');
  case Sex of
    TVkSex.None, TVkSex.Male:
      Result := Words[0];
    TVkSex.Female:
      Result := Words[1];
  end;
end;

function MonthTextOf(Value: TDateTime; Short: Boolean): string;
begin
  case MonthOf(Value) of
    1:
      Result := IfThen(Short, 'янв', 'января');
    2:
      Result := IfThen(Short, 'фев', 'февраля');
    3:
      Result := IfThen(Short, 'мар', 'марта');
    4:
      Result := IfThen(Short, 'апр', 'апреля');
    5:
      Result := IfThen(Short, 'мая', 'мая');
    6:
      Result := IfThen(Short, 'июн', 'июня');
    7:
      Result := IfThen(Short, 'июл', 'июля');
    8:
      Result := IfThen(Short, 'авг', 'августа');
    9:
      Result := IfThen(Short, 'сен', 'сентября');
    10:
      Result := IfThen(Short, 'окт', 'октября');
    11:
      Result := IfThen(Short, 'ноя', 'ноября');
    12:
      Result := IfThen(Short, 'дек', 'декабря');
  else
    Result := '?????';
  end;
end;

function HumanDateTime(Value: TDateTime; ShowTime: Boolean; ShortDate: Boolean): string;
begin
  if IsSameDay(Value, Today) then
    Result := 'сегодня'
  else if IsSameDay(Value, Yesterday) then
    Result := 'вчера'
  else if YearOf(Value) = YearOf(Now) then
    Result := FormatDateTime('d ' + MonthTextOf(Value, ShortDate), Value)
  else
    Result := FormatDateTime('d ' + MonthTextOf(Value, ShortDate) + ' YYYY', Value);
  if ShowTime then
  begin
    Result := Result + ' в ' + FormatDateTime('H:nn', Value);
  end;
end;

function MessageActionToText(const Value: TVkMessageAction; FromId: TVkPeerId; const FromText, MemberText: string): string;
begin
  Result := '';
  case Value.&Type of
    TVkMessageActionType.ChatPhotoUpdate:
      Result := FromText + ' обновил(а) фотографию чата';
    TVkMessageActionType.ChatPhotoRemove:
      Result := FromText + ' удалил(а) фотографию чата';
    TVkMessageActionType.ChatCreate:
      Result := FromText + ' создал(а) чат «' + Value.Text + '»';
    TVkMessageActionType.ChatTitleUpdate:
      Result := FromText + ' изменил(а) название чата';
    TVkMessageActionType.ChatInviteUser:
      if Value.MemberId = FromId then
        Result := FromText + ' вошел(ла) в чат'
      else
        Result := FromText + ' пригласил(а) ' + MemberText;
    TVkMessageActionType.ChatKickUser:
      if Value.MemberId = FromId then
        Result := FromText + ' вышел(ла) из чата'
      else
        Result := FromText + ' исключил(а) ' + MemberText;
    TVkMessageActionType.ChatPinMessage:
      Result := FromText + ' закрепил(а) сообщение';
    TVkMessageActionType.ChatUnpinMessage:
      Result := FromText + ' открепил(а) сообщение';
    TVkMessageActionType.ChatInviteUserByLink:
      Result := FromText + ' присоединился к чату по ссылке';
    TVkMessageActionType.ConversationStyleUpdate:
      Result := FromText + ' изменил(а) оформление чата на «' + Value.Style + '». Оформление чата доступно в мобильном приложении';
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
      Result := 'Голосовое сообщение';
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

