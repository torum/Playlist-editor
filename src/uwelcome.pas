unit UWelcome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList,Buttons;

type

  { TfrmWelcome }

  TfrmWelcome = class(TForm)
    BevelNew: TBevel;
    BevelOpen: TBevel;
    BitBtnOpeniTunes: TBitBtn;
    BitBtnOpenM3u: TBitBtn;
    BitBtnOpenXspf: TBitBtn;
    BitBtnSelectFiles: TBitBtn;
    ImageNew: TImage;
    ImageOpen: TImage;
    StaticTextDropFilesHere: TStaticText;
    StaticTextNewImageCaption: TStaticText;
    StaticTextOpenImageCaption: TStaticText;
    procedure BitBtnOpeniTunesClick(Sender: TObject);
    procedure BitBtnOpenM3uClick(Sender: TObject);
    procedure BitBtnOpenXspfClick(Sender: TObject);
    procedure BitBtnSelectFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private

  public
    closeAction:TAction;
    droppedFiles:TStringList;

  end;

var
  frmWelcome: TfrmWelcome;

implementation

uses UMain;

{$R *.lfm}

{ TfrmWelcome }

procedure TfrmWelcome.FormCreate(Sender: TObject);
begin
  self.AllowDropFiles:=true;
  droppedFiles := TStringList.Create;
end;

procedure TfrmWelcome.BitBtnSelectFilesClick(Sender: TObject);
begin
  //TODO use OpenFileDialog here.
  closeAction := frmMain.actNewFile;
  self.ModalResult:=mrOK;
end;

procedure TfrmWelcome.BitBtnOpeniTunesClick(Sender: TObject);
begin
  closeAction := frmMain.actOpenTxt;
  self.ModalResult:=mrOK;
end;

procedure TfrmWelcome.BitBtnOpenM3uClick(Sender: TObject);
begin
  closeAction := frmMain.actOpenM3u;
  self.ModalResult:=mrOK;
end;

procedure TfrmWelcome.BitBtnOpenXspfClick(Sender: TObject);
begin
  closeAction := frmMain.actOpenXSPF;
  self.ModalResult:=mrOK;
end;

procedure TfrmWelcome.FormDestroy(Sender: TObject);
begin
  droppedFiles.Free;
end;

procedure TfrmWelcome.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  FileName : String;
begin
  droppedFiles.Clear;
  for FileName in FileNames do
  begin
    droppedFiles.Add(FileName);
    //todo
    //if filename count 1 and ext m3u,m3u8,xspf,or txt, then open playlist,
    //otherwise new files.

  end;

  frmMain.CreateNew(droppedFiles);

  closeAction:=nil;
  self.ModalResult:=mrCancel;

end;

end.

