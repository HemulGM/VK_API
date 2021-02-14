unit VK.Entity.Attachment;

interface

uses
  VK.Types;

type
  IAttachment = interface(IInterface)
    function ToAttachment: TAttachment;
  end;

implementation

end.

