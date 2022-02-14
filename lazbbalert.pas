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
    LAlert: TLabel;
    PnlMessage: TPanel;
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
  // Adjust height of label, buttons position and form haight according text in the memo
  // Set width, then minwidth=width, then anchor left, right and top, finally set autosize.
  PnLMessage.Height:= LAlert.height+10;
  if PnlMessage.Height < Image1.Height+10 then
  begin
    PnlMessage.Height:= Image1.Height+10;
    LAlert.Top:= (PnlMessage.ClientHeight-LAlert.Height) div 2;
  end;
  Image1.Top:= (PnlMessage.ClientHeight-Image1.Height) div 2 ;
  BtnOk.Top:= PnlMessage.Top+PnlMessage.Height+10;
  CBNoShowAlert.Top:= BtnOK.Top+2;
  BtnCancel.top:= BtnOK.Top;
  Height:= BtnOK.top+BtnOK.Height+10;
  Case DlgType of
    mtWarning: begin
      Image1.Picture.LoadFromResourceName(HInstance, 'AL_WARNING');
      Beep;
    end;
    mtInformation: Image1.Picture.LoadFromResourceName(HInstance, 'AL_INFORMATION');
    mtError: begin
      Image1.Picture.LoadFromResourceName(HInstance, 'AL_ERROR');
      Beep;
    end;
    mtConfirmation: Image1.Picture.LoadFromResourceName(HInstance, 'AL_CONFIRMATION');
  end;
  if CBNoShowAlert.visible then
  begin
    //Align buttons son the right
    BtnCancel.left:= PnlMessage.left+PnlMessage.width-BtnCancel.width;
    BtnOK.Left:= BtnCancel.left-BtnCancel.Width-10;
  end else
  begin
    // Center buttons in case of width change
    BtnOK.Left:= (ClientWidth-BtnOK.width*2-10) div 2;
    BtnCancel.Left:= BtnOK.Left+BtnOK.Width+10;
  end;
end;

end.

