{****************************************************************************** }
{ lazbbabout - About box for author applications                                }
{ bb - sdtp - november 2019                                                     }
{Can change with to adapt to program                                            }
{*******************************************************************************}
unit lazbbaboutdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    Buttons, lclintf, fphttpclient, fpopenssl, openssl, opensslsockets, lazbbutils;

type
  TAboutDlg = class(TForm)
    BtnOK: TBitBtn;
    Image1: TImage;
    PnlMain: TPanel;
    LProductName: TLabel;
    LSourceCode: TLabel;
    LWebSite: TLabel;
    LVersion: TLabel;
    LUpdate: TLabel;
    LCopyright: TLabel;
    LProgPage: TLabel;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure LabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LURLClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private



  public


    // Variables
    ErrorNum: Integer;
    ErrorMessage: String;         // Errormessage passed to main app
    UrlWebsite: String;           // Author Web site
    UrlProgSite: String;          // URL of program page with download link
    UrlSourceCode: String;        // URL of source code if exists
    LastUpdate: TDateTime;
    ChkVerUrl: String;            // New version check theme (Github)
    Version: String;              // Current version
    LastVersion: String;          // Dernière version
    sLastUpdateSearch: String;    // Date de la dernière recherche
    sUpdateAvailable: string;     // Nouvelle version disponible
    sNoUpdateAvailable: string;   // Pas de nouvelle version disponible
    ProgName: String;             // Nom du programme
    Checked, NewVersion: Boolean; // New version check
    function ChkNewVersion (url: string=''): string;

  end;
var
  AboutBox: TAboutDlg;

implementation


{ TForm2 }

constructor TAboutDlg.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner);
  SetBounds(50, 50, 350, 208);
  BorderStyle:= bsDialog;
  Caption:= 'About...';
  Position:= poMainFormCenter;
  PnlMain:= TPanel.Create(Self);
  PnlMain.Parent:= self;
  PnlMain.SetBounds(8, 8, 336, 152);
  PnlMain.BevelInner:= bvRaised;
  PnlMain.BevelOuter:= bvLowered;
  LProductName:= TLabel.Create(PnlMain);
  LProductName.Parent:= PnlMain;
  LProductName.Autosize:= false;
  LProductName.SetBounds(0, 10, 336, 15);
  LProductName.Caption:= 'Product Name';
  LProductName.Alignment:= taCenter;
  LVersion:= TLabel.Create(PnlMain);
  LVersion.Parent:= PnlMain;
  LVersion.Autosize:= false;
  LVersion.SetBounds(0, 30, 336, 15);
  LVersion.Caption:= 'Version';
  LVersion.Alignment:= taCenter;
  LVersion.ShowHint:= true;
  LVersion.OnMouseEnter:= @LabelMouseEnter;
  LVersion.OnMouseLeave:= @LabelMouseLeave;
  LSourceCode:= TLabel.Create(PnlMain);
  LSourceCode.Parent:= PnlMain;
  LSourceCode.Autosize:= false;
  LSourceCode.SetBounds(0, 130, 336, 15);
  LSourceCode.Caption:= 'Source code';
  LSourceCode.Alignment:= taCenter;
  LSourceCode.Font.Color:= clBlue;
  LSourceCode.ShowHint:= true;
  LSourceCode.OnClick:= @LURLClick;
  LSourceCode.OnMouseDown:= @LabelMouseDown;
  LSourceCode.OnMouseEnter:= @LabelMouseEnter;
  LSourceCode.OnMouseLeave:= @LabelMouseLeave;
  LSourceCode.OnMouseUp:= @LabelMouseUp;
  LWebSite:= TLabel.Create(PnlMain);
  LWebSite.Parent:= PnlMain;
  LWebSite.Autosize:= false;
  LWebSite.SetBounds(0, 90, 336, 15);
  LWebSite.Caption:= 'Web site';
  LWebSite.Alignment:= taCenter;
  LWebsite.Font.Color:= clBlue;
  LWebSite.ShowHint:= true;
  LWebSite.OnClick:= @LURLClick;
  LWebSite.OnMouseDown:= @LabelMouseDown;
  LWebSite.OnMouseEnter:= @LabelMouseEnter;
  LWebSite.OnMouseLeave:= @LabelMouseLeave;
  LWebSite.OnMouseUp:= @LabelMouseUp;
  LUpdate:= TLabel.Create(PnlMain);
  LUpdate.Parent:= PnlMain;
  LUpdate.Autosize:= false;
  LUpdate.SetBounds(0, 70, 336, 15);
  LUpdate.Caption:= 'Update';
  LUpdate.Alignment:= taCenter;
  LUpdate.Font.Color:= clBlue;
  LUpdate.ShowHint:= true;
  Lupdate.OnClick:= @LURLClick;
  Lupdate.OnMouseDown:= @LabelMouseDown;
  Lupdate.OnMouseEnter:= @LabelMouseEnter;
  Lupdate.OnMouseLeave:= @LabelMouseLeave;
  Lupdate.OnMouseUp:= @LabelMouseUp;
  LCopyright:= TLabel.Create(PnlMain);
  LCopyright.Parent:= PnlMain;
  LCopyright.Autosize:= false;
  LCopyright.SetBounds(0, 50, 336, 15);
  LCopyright.Caption:= 'Copyright';
  LCopyright.Alignment:= taCenter;
  LProgPage:= TLabel.Create(PnlMain);
  LProgPage.Parent:= PnlMain;
  LProgPage.Autosize:= false;
  LProgPage.SetBounds(0, 110, 336, 15);
  LProgPage.Caption:= 'Program Site';
  LProgPage.Alignment:= taCenter;
  LProgPage.Font.Color:= clBlue;
  LProgPage.ShowHint:= true;
  LProgPage.OnClick:= @LURLClick;
  LProgPage.OnMouseDown:= @LabelMouseDown;
  LProgPage.OnMouseEnter:= @LabelMouseEnter;
  LProgPage.OnMouseLeave:= @LabelMouseLeave;
  LProgPage.OnMouseUp:= @LabelMouseUp;
  BtnOK:= TBitBtn.create(self);
  BtnOK.Parent:= self;
  BtnOK.SetBounds(144, 168, 75, 30 );
  BtnOK.Kind:= bkOK;
  BtnOK.ModalResult:= mrOK;
  Image1:= TImage.Create(PnlMain);
  Image1.Parent:= PnlMain;
  Image1.SetBounds (10, 10, 34, 34);

end;

procedure TAboutDlg.FormActivate(Sender: TObject);
begin
  //LWebSite.Caption:= 'Web site';
  LWebSite.Hint:= UrlWebsite;
  LProgPage.Hint:= UrlProgSite;
  if length(UrlSourceCode)=0 then
  begin
    LSourceCode.Caption:= '';
    LVersion.Top:= LProductName.Top+25;
    LCopyright.Top:= LVersion.Top+25;
    LUpdate.Top:= LCopyright.Top+25;
    LWebsite.Top:= LUpdate.Top+25;
    LProgPage.Top:= LWebsite.Top+25;
    LSourceCode.visible:= false;
  end else
  begin
    LSourceCode.Caption:= 'Source code';
    LSourceCode.Hint:= UrlSourceCode;
    LVersion.Top:= LProductName.Top+20;
    LCopyright.Top:= LVersion.Top+20;
    LUpdate.Top:= LCopyright.Top+20;
    LWebsite.Top:= LUpdate.Top+20;
    LProgPage.Top:= LWebsite.Top+20;
    LSourceCode.Top:= LProgPage.Top+20;
    LSourceCode.visible:= true;
  end;
end;

// Resize controls when box is resized
procedure TAboutDlg.FormChangeBounds(Sender: TObject);
begin
  PnlMain.left:= 7;
  PnlMain.Width:= ClientWidth-14;
  LProductName.left:= 0;
  LproductName.Width:= PnlMain.Width;
  LVersion.left:= 0;
  LVersion.width:= PnlMain.Width;
  LUpdate.Left:= 0;
  LUpdate.Width:= PnlMain.Width;
  LCopyright.left:= 0;
  LCopyright.Width:= PnlMain.Width;
  LProgPage.Left:= 0;
  LProgPage.Width:= PnlMain.Width;
  LWebsite.Left:= 0;
  LWebSite.Width:= PnlMain.Width;
  LSourceCode.Width:= PnlMain.Width;
  BtnOk.Left:= (ClientWidth-BtnOK.Width) div 2;
end;

// Get new version on Github
// Get latest release page
// extract version value fromn header title

function TAboutDlg.ChkNewVersion(url: string=''): string;
var
  MyHTTPCli: TFPHTTPClient;
  spage: string;
  stagurl: string;
  titlebeg, titleend: Integer;
  A: TStringArray;
  sl: TStringList;
  s: string;
  i, p: integer;
begin
  result:= '';
  sl:= TStringList.create();
   if length(url)=0 then url:= ChkVerUrl;
  { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 20000;
    MyHTTPCli.AllowRedirect:= true;
    MyHTTPCli.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
    // Parse last release page to get last version
    // <title>Release Version 0.9.1.7 - 08/04/2021 etc
    sl.text:= MyHTTPCli.Get(url);
    Application.ProcessMessages; //let loading complete
    if sl.Count > 0 then
    begin
      for i:= 0 to sl.Count-1 do
      begin
        p:= pos('<title>', sl.Strings[i]) ;
        if p > 0 then
        begin
          s:=Copy(sl.Strings[i], p+8, 40);
          A:= s.Split(' ');
          break;
        end;
      end;
      // Avoid exception if we dont find the proper line
      if length(A)>=2 then result:= A[2]
      else result:= '';
    end;
  except
    on e:Exception do
       ErrorMessage:= e.message
  end;
  if Assigned(MyHTTPCli) then MyHTTPCli.Free;
  if Assigned(sl) then sl.free;
end;

procedure TAboutDlg.LURLClick(Sender: TObject);
var
  sendername: string;
  url: string;
begin
  Sender:= Tcomponent(Sender);
  //SenderName:= UpperCase(Tcomponent(Sender).Name);
  if Sender= LUpdate then
  begin
    Beep;
    // New version found, click get download page
    if NewVersion then
    begin
      // Link to program binaries is in the help file
      OpenURL(UrlProgSite);
      exit;
    end;
    // No new version at the moment, check it
    //url:= UrlUpdate;
    LastUpdate:= now();
    LUpdate.Hint:= sLastUpdateSearch + ': ' + DateToStr(trunc(LastUpdate));
    checked:= true;
    LastVersion:= ChkNewVersion(ChkVerUrl);
    if VersionToInt(LastVersion)>VersionToInt(Version) then
    begin
      LUpdate.Caption:= Format(sUpdateAvailable, [LastVersion]);
      NewVersion:= true;
    end else
    begin
      LUpdate.Caption:= sNoUpdateAvailable;
    end;
  end;
  if Sender= LWebSite then
  begin
    url:= UrlWebsite;
    If length(url) > 0 then OpenURL(url);
  end;
  if Sender= LSourceCode then
  begin
    url:= UrlSourceCode;
    If length(url) > 0 then OpenURL(url);
  end;
  if Sender= LProgPage then
  begin
    url:= UrlProgSite;
    If length(url) > 0 then OpenURL(url);
  end;
end;

// URL Label reactions  : cursor changes to hand when mouse enter and brignt when selected

procedure TAboutDlg.LabelMouseEnter(Sender: TObject);
begin
  if TLabel(Sender)=LVersion then  TLabel(Sender).Cursor:= crHelp
  else TLabel(Sender).Cursor:=  crHandPoint;
end;

procedure TAboutDlg.LabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Cursor:= crDefault;
end;

procedure TAboutDlg.LabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).font.style:= [fsBold];
end;

procedure TAboutDlg.LabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.style:= [];
end;

Initialization
AboutBox:= TAboutDlg.createnew(nil);


end.


