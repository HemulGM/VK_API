unit VK.Captcha;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.StdCtrls, HGM.Button, HGM.Common.Utils;

type
  TFormCaptcha = class(TForm)
    ImageCaptcha: TImage;
    Panel1: TPanel;
    ButtonFlatOK: TButtonFlat;
    EditEnter: TEdit;
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
  Result := False;
  Mem := DownloadURL(CaptchaURL);
  Jpg := TJPEGImage.Create;
  with TFormCaptcha.Create(nil) do
  try
    if Mem.Size > 0 then
    begin
      Jpg.LoadFromStream(Mem);
      ImageCaptcha.Picture.Assign(Jpg);
      EditEnter.Text := '';
      Result := ShowModal = mrOk;
    end;
  finally
    Jpg.Free;
    Mem.Free;
    Free;
  end;
end;

end.

