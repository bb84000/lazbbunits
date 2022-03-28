{******************************************************************************
 lazbbabout - About box for author applications                                
 bb - sdtp - november 2019                                                     
 Can change with to adapt to program    
 Deprecated unit. Use lazbbaboutdlg instead
*******************************************************************************}
unit lazbbabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, lclintf;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BtnOK: TBitBtn;
    Image1: TImage;
    LProductName: TLabel;
    LWebSite: TLabel;
    LVersion: TLabel;
    LUpdate: TLabel;
    LCopyright: TLabel;
    PnlDesc: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure LURLClick(Sender: TObject);
  private

  public
    ErrorMessage: String;
    UrlUpdate: String;
    UrlWebsite: String;
    LastUpdate: TDateTime;

  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }

// Click on Web site URL : launch default browser on author home page
// Click on update URL : launch default browser new version web page

procedure TAboutBox.LURLClick(Sender: TObject);
var
  sendername: string;
  url: string;
begin
  SenderName:= UpperCase(Tcomponent(Sender).Name);
  if SenderName= 'LUPDATE' then
  begin
    url:= UrlUpdate;
    LastUpdate:= now();
  end;
  if SenderName= 'LWEBSITE' then url:= UrlWebsite;
  If length(url) > 0 then
  OpenURL(url);
end;


procedure TAboutBox.FormActivate(Sender: TObject);
begin
  LWebSite.Caption:= 'Web site: '+UrlWebsite;
end;

// Resize controls when box is resized
procedure TAboutBox.FormChangeBounds(Sender: TObject);
begin
  PnlDesc.left:= 7;
  PnlDesc.Width:= ClientWidth-14;
  LProductName.left:= 0;
  LproductName.Width:= PnlDesc.Width;
  LVersion.left:= 0;
  LVersion.width:= PnlDesc.Width;
  LUpdate.Left:= 0;
  LUpdate.Width:= PnlDesc.Width;
  LCopyright.left:= 0;
  LCopyright.Width:= PnlDesc.Width;
  LWebsite.Left:= 0;
  LWebSite.Width:= PnlDesc.Width;
  BtnOk.Left:= (ClientWidth-BtnOK.Width) div 2;
end;

end.


