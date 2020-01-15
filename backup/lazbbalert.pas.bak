unit lazbbalert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAlertBox }

  TAlertBox = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    CBNoShowAlert: TCheckBox;
    Image1: TImage;
    MAlert: TMemo;
    Panel1: TPanel;
    procedure BtnCancelClick(Sender: TObject);
  private

  public

  end;

var
  AlertBox: TAlertBox;

implementation

{$R *.lfm}

{ TAlertBox }

procedure TAlertBox.BtnCancelClick(Sender: TObject);
begin
  CBNoShowAlert.Checked:= False;
  ModalResult:= mrCancel;
end;

end.

