{****************************************************************************** }
{ lazbbabout - About box for author applications                                }
{ Check new versions functions                                                  }
{ bb - sdtp - april 2021                                                     }
{ Can change width to adapt to program                                           }
{*******************************************************************************}
unit lazbbaboutupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, lclintf, fphttpclient, fpopenssl, openssl, opensslsockets, lazbbutils;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BtnOK: TBitBtn;
    Image1: TImage;
    LProductName: TLabel;
    LSourceCode: TLabel;
    LWebSite: TLabel;
    LVersion: TLabel;
    LUpdate: TLabel;
    LCopyright: TLabel;
    PnlDesc: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LURLClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private

  public
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
  AboutBox: TAboutBox;

implementation

{$R *.lfm}


// Get new version on Github
// Get latest release page
// extract version value fromn header title

function TAboutBox.ChkNewVersion(url: string=''): string;
var
  MyHTTPCli: TFPHTTPClient;
  spage: string;
  stagurl: string;
  titlebeg, titleend: Integer;
  A: TStringArray;
  sl: TStringList;
  i: integer;
begin
  result:= '';
  sl:= TStringList.create();
   if length(url)=0 then url:= ChkVerUrl;
  { SSL initialization has to be done by hand here }
  InitSSLInterface;
  MyHTTPCli:= TFPHTTPClient.Create(nil);
  try
    MyHTTPCli.IOTimeout:= 10000;
    MyHTTPCli.AllowRedirect:= true;
    MyHTTPCli.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
    // Parse history.txt to get last version
    sl.text:= MyHTTPCli.Get(url);
    if sl.Count > 0 then
    begin
      for i:= 0 to sl.Count-1 do
      begin
        if Pos('Version', sl.Strings[i])=1 then A:= sl.Strings[i].Split(' ');
      end;
      result:= A[1];
    end;
  except
    on e:Exception do
    ErrorMessage:= e.message
  end;
  if Assigned(MyHTTPCli) then MyHTTPCli.Free;
  if Assigned(sl) then sl.free;
end;

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
  if SenderName= 'LWEBSITE' then
  begin
    url:= UrlWebsite;
    If length(url) > 0 then OpenURL(url);
  end;
  if SenderName= 'LSOURCECODE' then
  begin
    url:= UrlSourceCode;
    If length(url) > 0 then OpenURL(url);
  end;
end;




procedure TAboutBox.FormActivate(Sender: TObject);
begin

  //LWebSite.Caption:= 'Web site';
  LWebSite.Hint:= UrlWebsite;
  if length(UrlSourceCode)=0 then
  begin
    LSourceCode.Caption:= '';
    LVersion.Top:= LProductName.Top+25;
    LCopyright.Top:= LVersion.Top+25;
    LUpdate.Top:= LCopyright.Top+25;
    LWebsite.Top:= LUpdate.Top+25;
    LSourceCode.visible:= false;
  end else
  begin
    //LSourceCode.Caption:= 'Source code';
    LSourceCode.Hint:= UrlSourceCode;
    LVersion.Top:= LProductName.Top+20;
    LCopyright.Top:= LVersion.Top+20;
    LUpdate.Top:= LCopyright.Top+20;
    LWebsite.Top:= LUpdate.Top+20;
    LSourceCode.Top:= LWebsite.Top+20;
    LSourceCode.visible:= true;
  end;
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
  LSourceCode.Width:= PnlDesc.Width;
  BtnOk.Left:= (ClientWidth-BtnOK.Width) div 2;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  inherited;
  UrlSourceCode:='';
end;

// URL Label reactions  : cursor changes to hand when mouse enter and brignt when selected

procedure TAboutBox.LabelMouseEnter(Sender: TObject);
begin
  if TLabel(Sender).Name='LVersion' then  TLabel(Sender).Cursor:= crHelp
  else TLabel(Sender).Cursor:=  crHandPoint;
end;

procedure TAboutBox.LabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Cursor:= crDefault;
end;

procedure TAboutBox.LabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).font.style:= [fsBold];
end;

procedure TAboutBox.LabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.style:= [];
end;

end.

