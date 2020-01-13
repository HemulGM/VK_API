unit VK.Captcha;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.StdCtrls, VK.Utils;

type
  TFormCaptcha = class(TForm)
    ImageCaptcha: TImage;
    Panel1: TPanel;
    EditEnter: TEdit;
    ButtonFlatOK: TButton;
    procedure ButtonFlatOKClick(Sender: TObject);
  public
    class function Execute(const CaptchaURL: string; var Answer: string): Boolean;
  end;

var
  FormCaptcha: TFormCaptcha;

implementation

{$R *.dfm}

procedure TFormCaptcha.ButtonFlatOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TFormCaptcha.Execute(const CaptchaURL: string; var Answer: string): Boolean;
var
  Mem: TMemoryStream;
  Jpg: TJPEGImage;
begin
  Mem := DownloadURL(CaptchaURL);
  Jpg := TJPEGImage.Create;
  with TFormCaptcha.Create(nil) do
  try
    if Mem.Size > 0 then
    begin
      Jpg.LoadFromStream(Mem);
      ImageCaptcha.Picture.Assign(Jpg);
    end
    else
      ImageCaptcha.Picture.Assign(nil);
    EditEnter.Text := '';
    Result := ShowModal = mrOk;
  finally
    Jpg.Free;
    Mem.Free;
    Free;
  end;
end;

end.

