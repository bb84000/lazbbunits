{******************************************************************************
 lazbbupdatedlg - Auto update dialog for author applications on GitHub
 Download zip install archive and launch installer
 bb - sdtp - march 2025
 - Remove UpdateDlg creation in application level, creation is done
   in the unit initialization section.
*******************************************************************************}
unit lazbbupdatedlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, zipper,  lazbbinifiles,
  {$IFDEF WINDOWS}
  ShellAPI,
  {$ENDIF}
  ExProgressbar, fphttpclient, opensslsockets;

type
  { TUpdateDlg }

  TUpdateDlg = class(TForm )
    BtnDownload: TButton;
    BtnUpdate: TButton;
    BtnAbort: TButton;
    Panel1: TPanel;
    ProgressbarEx1: TProgressbarEx;
    SUpdate: TStaticText;
    procedure BtnDownloadClick(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    tmpdir: String;
    tmpzip: String;
    sUpdtAvailable: String;
    sNoUpdtAvailable: String;
    sUpdtInstall: String;
    DlError: String;
    ZipError: String;
    DefaultCaption: String;
  public
    ProgName: String;             // Nom du programme
    UrlInstall: String;           // URL du zip s'installation
    ExeInstall: String;           // Exécutablke installation
    sNewVer: String;              // New version string
    NewVersion: Boolean;
    procedure Translate(LngFile: TBbIniFile);
    procedure DataReceived(Sender: TObject; Const ContentLength, CurrentPos : Int64);
  end;

var
  UpdateDlg: TUpdateDlg;

implementation

{$R *.lfm}

{ TUpdateDlg }

procedure TUpdateDlg.FormShow(Sender: TObject);
begin
  tmpdir:= GetTempDir;
  tmpzip:= tmpdir+ProgName+'.zip';
  if FileExists(tmpzip) then DeleteFile (tmpzip) ;
  if FileExists(tmpdir+ExeInstall) then DeleteFile (tmpdir+ExeInstall);
  if Newversion then
  begin
    SUpdate.Caption:= Format(sUpdtAvailable, [sNewVer]);  ;
    BtnUpdate.Enabled:= false;
    BtnDownload.Enabled:= true;
  end else
  begin
    SUpdate.Caption:= Format(sNoUpdtAvailable, [DefaultCaption]);
    BtnUpdate.Enabled:= false;
    BtnDownload.Enabled:= False;
  end;
end;

procedure TUpdateDlg.BtnDownloadClick(Sender: TObject);
var
  HttpCli: TFPHTTPClient;
  data: TFileStream;
begin
  try
    data:= TFileStream.Create(tmpzip, fmCreate);
    HttpCli:= TFPHTTPClient.Create(nil);
    HttpCli.AddHeader('User-Agent','Mozilla 5.0 (bb84000 '+ProgName+')');
    HttpCli.AllowRedirect:= True;
    HttpCli.OnDataReceived:= @DataReceived;
    HttpCli.Get (UrlInstall, data);
    if  data.size = 0 then
    begin
      SUpdate.Caption:= DlError;
      BtnUpdate.Enabled:= False;
    end;
  except
    SUpdate.Caption:= DlError;
    BtnUpdate.Enabled:= False;
  end;
  if assigned(data) then data.free;
  if assigned(HttpCli) then HttpCli.free;
  if FileExists(tmpzip) then
  begin
    SUpdate.Caption:= sUpdtInstall;
    BtnUpdate.Enabled:= true;
    BtnDownload.Enabled:= False;
  end;
end;

procedure TUpdateDlg.BtnUpdateClick(Sender: TObject);
var
   UZip: TUnZipper;
   sei: TShellExecuteInfoA;
begin
  if not FileExists(tmpzip) then
  begin
    BtnUpdate.Enabled:= False;
    close;
  end;
  UZip:= TUnZipper.Create;
  try
    UZip.FileName:= tmpzip;
    UZip.OutputPath:= tmpdir;
    uZip.UnZipFile(tmpzip, ExeInstall);
  finally
    UZip.free;
  end;
  DeleteFile(tmpzip);
  if not FileExists(tmpdir+ExeInstall) then
  begin
    SUpdate.Caption:= ZipError;
    exit;
  end;
  {$IFDEF WINDOWS}
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(tmpdir+ExeInstall);
  sei.lpParameters := '';
  sei.nShow := 1; //SW_SHOWNORMAL;
  ShellExecuteExA(@sei);
  {$ENDIF}
end;

procedure TUpDateDlg.DataReceived(Sender: TObject; const ContentLength,  CurrentPos: Int64);
begin
  if ContentLength > 0 then
    ProgressBarEx1.Position := ProgressBarEx1.Min + CurrentPos * (ProgressBarEx1.Max - ProgressBarEx1.Min) div ContentLength;
  ProgressBarEx1.update;
end;


// Self localization procedure. LangFile parameter is language related ini file

procedure TUpdateDlg.Translate(LngFile: TBbInifile);

begin
  if assigned (Lngfile) then
  with LngFile do
  begin
    DefaultCaption:= ReadString('Common', 'DefaultCaption', '...');
    Caption:= Format(ReadString('UpdateDlg','Caption','Mise à jour de %s'), [DefaultCaption]);
    BtnDownload.Caption:= ReadString('UpdateDlg', 'BtnDownload.Caption', BtnDownload.Caption);
    BtnUpdate.Caption:= ReadString('UpdateDlg', 'BtnUpdate.Caption', BtnUpdate.Caption);
    BtnAbort.Caption:= ReadString('UpdateDlg', 'BtnAbort.Caption', BtnAbort.Caption);
    sUpdtAvailable:= ReadString('UpdateDlg', 'sUpdtAvailable', 'Nouvelle version %s disponible.');
    sNoUpdtAvailable:= ReadString('UpdateDlg', 'sNoUpdtAvailable', '%s est à jour');
    sUpdtInstall:= ReadString('UpdateDlg', 'sUpdtInstall',
                'Cliquer sur le bouton "Mettre à jour" pour installer la mise à jour');
    DlError:= ReadString('UpdateDlg', 'DlError', 'Erreur de téléchargement');
    ZipError:=  ReadString('UpdateDlg', 'ZipError', 'Erreur archive');
  end;
end;

Initialization

UpdateDlg:= TUpdateDlg.create(nil);

finalization

if Assigned(UpdateDlg) then UpdateDlg.Destroy;


end.

