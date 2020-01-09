unit VK.Types;

interface

uses
  System.Classes, System.Generics.Collections;

const
  ERROR_VK_UNKNOWN = -1;
  ERROR_VK_NOTOKEN = -2;

type
  TOnLogin = procedure(Sender: TObject) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; Code: Integer; Text: string) of object;

  TFields = array of string;

  TParam = array of string;

  TParams = array of TParam;

  TPremission = string;

  TPermissions = class(TList<TPremission>)
    function ToString: string; override;
    procedure Assign(Source: TStrings);
  end;

  TResponseError = record
    Code: Integer;
    Text: string;
  end;

  TResponse = record
    Success: Boolean;
    Value: string;
    JSON: string;
    Error: TResponseError;
  end;

  TCallMethodCallback = reference to procedure(Respone: TResponse);

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

procedure AddParam(var Dest: TParams; Param: TParam);

implementation

procedure AddParam(var Dest: TParams; Param: TParam);
begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[High(Dest)] := Param;
end;

function FieldsToString(Fields: TFields): string;
var
  i: Integer;
begin
  for i := Low(Fields) to High(Fields) do
  begin
    if i <> Low(Fields) then
      Result := Result + ',';
    Result := Result + Fields[i];
  end;
end;

function VKErrorString(ErrorCode: Integer): string;
var
  ErrStr: string;
begin
  case ErrorCode of
    1:
      ErrStr := 'Произошла неизвестная ошибка. Попробуйте повторить запрос позже.';
    2:
      ErrStr :=
        'Приложение выключено. Необходимо включить приложение в настройках https://vk.com/editapp?id={Ваш API_ID} или использовать тестовый режим (test_mode=1)';
    3:
      ErrStr :=
        'Передан неизвестный метод. Проверьте, правильно ли указано название вызываемого метода: https://vk.com/dev/methods.';
    4:
      ErrStr := 'Неверная подпись.';
    5:
      ErrStr := 'Авторизация пользователя не удалась. Убедитесь, что Вы используете верную схему авторизации.';
    6:
      ErrStr :=
        'Слишком много запросов в секунду. Задайте больший интервал между вызовами или используйте метод execute. Подробнее об ограничениях на частоту вызовов см. на странице https://vk.com/dev/api_requests.';
    7:
      ErrStr :=
        'Нет прав для выполнения этого действия. Проверьте, получены ли нужные права доступа при авторизации. Это можно сделать с помощью метода account.getAppPermissions.';
    8:
      ErrStr :=
        'Неверный запрос. Проверьте синтаксис запроса и список используемых параметров (его можно найти на странице с описанием метода).';
    9:
      ErrStr :=
        'Слишком много однотипных действий. Нужно сократить число однотипных обращений. Для более эффективной работы Вы можете использовать execute или JSONP.';
    10:
      ErrStr := 'Произошла внутренняя ошибка сервера. Попробуйте повторить запрос позже.';
    11:
      ErrStr :=
        'В тестовом режиме приложение должно быть выключено или пользователь должен быть залогинен. Выключите приложение в настройках https://vk.com/editapp?id={Ваш API_ID}';
    14:
      ErrStr := 'Требуется ввод кода с картинки (Captcha).';
    15:
      ErrStr :=
        'Доступ запрещён. Убедитесь, что Вы используете верные идентификаторы, и доступ к контенту для текущего пользователя есть в полной версии сайта.';
    16:
      ErrStr :=
        'Требуется выполнение запросов по протоколу HTTPS, т.к. пользователь включил настройку, требующую работу через безопасное соединение.'#13#10 +
        ' Чтобы избежать появления такой ошибки, в Standalone-приложении Вы можете предварительно проверять состояние этой настройки у пользователя методом account.getInfo.';
    17:
      ErrStr :=
        'Требуется валидация пользователя. Действие требует подтверждения — необходимо перенаправить пользователя на служебную страницу для валидации.';
    18:
      ErrStr := 'Страница удалена или заблокирована. Страница пользователя была удалена или заблокирована';
    20:
      ErrStr :=
        'Данное действие запрещено для не Standalone приложений. Если ошибка возникает несмотря на то, что Ваше приложение имеет тип Standalone, убедитесь, что при авторизации Вы используете redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr := 'Данное действие разрешено только для Standalone и Open API приложений.';
    23:
      ErrStr :=
        'Метод был выключен. Все актуальные методы ВК API, которые доступны в настоящий момент, перечислены здесь: https://vk.com/dev/methods.';
    24:
      ErrStr := 'Требуется подтверждение со стороны пользователя.';
    27:
      ErrStr := 'Ключ доступа сообщества недействителен.';
    28:
      ErrStr := 'Ключ доступа приложения недействителен.';
    29:
      ErrStr :=
        'Достигнут количественный лимит на вызов метода Подробнее об ограничениях на количество вызовов см. на странице https://vk.com/dev/data_limits';
    30:
      ErrStr :=
        'Профиль является приватным Информация, запрашиваемая о профиле, недоступна с используемым ключом доступа';
    33:
      ErrStr := 'Not implemented yet';
    100:
      ErrStr :=
        'Один из необходимых параметров был не передан или неверен. Проверьте список требуемых параметров и их формат на странице с описанием метода.';
    101:
      ErrStr :=
        'Неверный API ID приложения. Найдите приложение в списке администрируемых на странице https://vk.com/apps?act=settings и укажите в запросе верный API_ID (идентификатор приложения).';
    113:
      ErrStr :=
        'Неверный идентификатор пользователя. Убедитесь, что Вы используете верный идентификатор. Получить ID по короткому имени можно методом utils.resolveScreenName.';
    148:
      ErrStr := 'Пользователь не установил приложение в левое меню';
    150:
      ErrStr := 'Неверный timestamp. Получить актуальное значение Вы можете методом utils.getServerTime.';
    200:
      ErrStr :=
        'Доступ к альбому запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    201:
      ErrStr :=
        'Доступ к аудио запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    203:
      ErrStr :=
        'Доступ к группе запрещён. Убедитесь, что текущий пользователь является участником или руководителем сообщества (для закрытых и частных групп и встреч).';
    300:
      ErrStr :=
        'Альбом переполнен. Перед продолжением работы нужно удалить лишние объекты из альбома или использовать другой альбом.';
    500:
      ErrStr :=
        'Действие запрещено. Вы должны включить переводы голосов в настройках приложения. Проверьте настройки приложения: https://vk.com/editapp?id={Ваш API_ID}&section=payments';
    600:
      ErrStr := 'Нет прав на выполнение данных операций с рекламным кабинетом.';
    603:
      ErrStr := 'Произошла ошибка при работе с рекламным кабинетом.';
    1260:
      ErrStr := 'Invalid screen name';
    3300:
      ErrStr := 'Recaptcha needed';
    3301:
      ErrStr := 'Phone validation needed';
    3302:
      ErrStr := 'Password validation needed';
    3303:
      ErrStr := 'Otp app validation needed';
    3304:
      ErrStr := 'Email confirmation needed';
    3305:
      ErrStr := 'Assert votes';
  else
    ErrStr := 'Неизвестная ошибка';
  end;

  Result := ErrStr;
end;

{ TPermissions }

procedure TPermissions.Assign(Source: TStrings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;

function TPermissions.ToString: string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Result + Items[i];
    if i < (Count - 1) then
      Result := Result + ',';
  end;
end;

end.

