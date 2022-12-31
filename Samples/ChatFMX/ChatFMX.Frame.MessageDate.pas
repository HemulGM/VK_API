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
    FDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    { Private declarations }
  public
    procedure Fill(Date: TDateTime);
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read FDate write SetDate;
  end;

implementation

uses
  ChatFMX.Utils, System.DateUtils;

{$R *.fmx}

{ TFrameMessageDate }

constructor TFrameMessageDate.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

procedure TFrameMessageDate.Fill(Date: TDateTime);
begin
  LabelText.Text := HumanDateTime(IncSecond(Date));
end;

procedure TFrameMessageDate.SetDate(const Value: TDateTime);
begin
  FDate := Value;
end;

end.

