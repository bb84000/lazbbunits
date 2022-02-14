unit lazbbinput;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

  {TinputDlg}
  TInputDlg = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    EText: TEdit;
    LMessage: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  InputDlg: TInputDlg;

implementation

{$R *.lfm}

{ TInputDlg }

procedure TInputDlg.FormShow(Sender: TObject);
begin
  LMessage.width:= ClientWidth-30;
  EText.width:= ClientWidth-30;
  //Align buttons
  BtnOK.left:= (Clientwidth - BtnOK.Width - BtnCancel.Width - 30) div 2;
  BtnCancel.left:= BtnOK.left+BtnOK.Width+30;
end;

end.

