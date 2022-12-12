unit ChatFMX.Frame.MessageDate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFrameMessageDate = class(TFrame)
    LabelText: TLabel;
  private
    { Private declarations }
  public
    procedure Fill(Date: TDateTime);
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ChatFMX.Utils;

{$R *.fmx}

{ TFrameMessageDate }

constructor TFrameMessageDate.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

procedure TFrameMessageDate.Fill(Date: TDateTime);
begin
  LabelText.Text := HumanDateTime(Date);
end;

end.

