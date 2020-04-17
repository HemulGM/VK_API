unit VK.FMX.Captcha;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Edit, FMX.Objects, FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TFormFMXCaptcha = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Layout1: TLayout;
    ImageCaptcha: TImage;
    EditEnter: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function Execute(const CaptchaURL: string; var Answer: string): Boolean; static;
  end;

var
  FormFMXCaptcha: TFormFMXCaptcha;

implementation

uses
  System.Net.HttpClient;

{$R *.fmx}

function DownloadURL(URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  HTTP := THTTPClient.Create;
  try
    try
      HTTP.HandleRedirects := True;
      HTTP.Get(URL, Result);
      Result.Position := 0;
    except
      //Ну, ошибка... Поток всё равно создан и ошибки не должно возникнуть,
      //если проверить размер потока перед его использованием
    end;
  finally
    HTTP.Free;
  end;
end;

class function TFormFMXCaptcha.Execute(const CaptchaURL: string; var Answer: string): Boolean;
var
  Mem: TMemoryStream;
begin
  Mem := DownloadURL(CaptchaURL);
  with TFormFMXCaptcha.Create(nil) do
  try
    if Mem.Size > 0 then
    begin
      ImageCaptcha.Bitmap.LoadFromStream(Mem);
    end
    else
      ImageCaptcha.Bitmap.Assign(nil);
    EditEnter.Text := '';
    Result := ShowModal = mrOk;
    Answer := EditEnter.Text;
  finally
    Mem.Free;
    Free;
  end;
end;

procedure TFormFMXCaptcha.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

