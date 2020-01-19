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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    DlgType : TMsgDlgType;
  end;

var
  AlertBox: TAlertBox;
  Picture: TPicture;
implementation

{$R *.lfm}
{$R lazbbalert.res}

{ TAlertBox }

procedure TAlertBox.BtnCancelClick(Sender: TObject);
begin
  CBNoShowAlert.Checked:= False;
  ModalResult:= mrCancel;
end;


procedure TAlertBox.FormCreate(Sender: TObject);
begin
  DlgType:= mtCustom;
end;

// To bypass

procedure TAlertBox.FormShow(Sender: TObject);
begin
  Case DlgType of
    mtWarning:  Image1.Picture.LoadFromResourceName(HInstance, 'AL_WARNING');
    mtInformation: Image1.Picture.LoadFromResourceName(HInstance, 'AL_INFORMATION');
    mtError: Image1.Picture.LoadFromResourceName(HInstance, 'AL_ERROR');
    mtConfirmation: Image1.Picture.LoadFromResourceName(HInstance, 'AL_CONFIRMATION');
  end;
end;

end.

