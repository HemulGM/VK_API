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
    FMessageId: Extended;
    procedure SetDate(const Value: TDateTime);
  public
    procedure Fill(ADate: TDateTime; AMessageId: Int64);
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read FDate write SetDate;
    property MessageId: Extended read FMessageId;
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

procedure TFrameMessageDate.Fill(ADate: TDateTime; AMessageId: Int64);
begin
  FMessageId := AMessageId + 0.1;
  TagFloat := FMessageId;
  LabelText.Text := HumanDateTime(IncSecond(ADate));
end;

procedure TFrameMessageDate.SetDate(const Value: TDateTime);
begin
  FDate := Value;
end;

end.

