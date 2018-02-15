unit uMain;

{$mode objfpc}{$H+}

{
 *** Now that all basic functionalities are implemented, the code needs to be
 completely refactored and rewritten with a Playlist Class
 for maintainability and extensibility.
}

{
Source:
 https://github.com/torumyax/Playlist-editor

Required Component:
 VirtualTreeView-Lazarus 5.5.3-r1
 utf8tools(included)

Tested on
 Windows 10: Lazarus 1.8.0 r56594 FPC 3.0.4 x86_64-win64-win32/win64
 Ubuntu 17.10: Lazarus 1.8.0 rc4+dfsg-1 FPC 3.0.2 x86_64-linux-gtk2
 macOS 10.03.2 High Sierra on iMac(21.5 Inch, Late 2012 - Intel Core i5, 8GB memory).
               Lazarus 1.8.0 rexported FPC 3.0.4 i386-darwin-carbon
 Ubuntu 16.04 LTS

TODO
 add(1.file,2.text,3.file drop to add),
 isDirty check
 save as "custom" file ext.
 help->about page.
 move up & down popup menu?
 non utf8 xspf reading.
 iTunes XML format?
 commandline conversion?

Known issues and bugs:
 XML reader won't accept non UTF-8 auch as Shift_JIS encoded file.
 Treeview "multiselect" and "right click select" won't work togeter. "right click select" -> disabled.

 On Windows, When dragging files over from shell, hint text shows "Copy"
  https://stackoverflow.com/questions/12993003/changing-drag-cursor-in-virtualtreeview

 On Ubuntu, VirtualTreeView shows Kanji characters half dissapeared.
  https://forum.lazarus.freepascal.org/index.php/topic,40042.0.html
 On Ubuntu, Treeview drag and drop is buggy.
  https://forum.lazarus.freepascal.org/index.php/topic,40061.0.html
 On Ubuntu, file drop from shell which requires ActiveX won't work.

 On macOS, Treeview mouse wheel scrolling is not working. always go back to where it was.
  https://forum.lazarus.freepascal.org/index.php/topic,40061.0.html
 On macOS, header sort glyphs transparent isn't woking.


}




interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ExtCtrls, StdCtrls, Clipbrd, ActnList, strutils,
  LazUTF8, LConvEncoding, charencstreams, laz2_XMLRead, laz2_XMLWrite, laz2_DOM,
  UOptions
  {$ifdef windows}, ActiveX{$else}, FakeActiveX{$endif};


type
  TPlaylistType = (iTunesTSV, m3u, xspf, new, error);

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actConvertToM3U: TAction;
    actConvertToXSPF: TAction;
    actCopySelectedPath: TAction;
    actAddFilesToPlaylist: TAction;
    actTryOpenContainingFolder: TAction;
    actRemoveFilesFromPlaylist: TAction;
    actSaveM3uAnsi: TAction;
    actSaveM3u8: TAction;
    actSaveM3uBOM: TAction;
    actURIDecodeToWinPathDelimiAll: TAction;
    actURIDecodeAll: TAction;
    actURIEncodeAll: TAction;
    actReplaceAll: TAction;
    actSaveXSPF: TAction;
    actSaveM3u: TAction;
    actSaveiTunesTxt: TAction;
    actOpenXSPF: TAction;
    actOpenM3u: TAction;
    actNewFile: TAction;
    actOpenTxt: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItemVer: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemSelectFiles: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItemSaveTxt: TMenuItem;
    MenuItemVTVCopySelected: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemOpenTxt: TMenuItem;
    MenuItemOpenM3u: TMenuItem;
    MenuItemOpenXspf: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveM3u: TMenuItem;
    MenuItemSaveXspf: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelContents: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TimerHideStatusbar: TTimer;
    VirtualStringTree1: TVirtualStringTree;
    procedure actAddFilesToPlaylistExecute(Sender: TObject);
    procedure actConvertToM3UExecute(Sender: TObject);
    procedure actConvertToXSPFExecute(Sender: TObject);
    procedure actCopySelectedPathExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actOpenM3uExecute(Sender: TObject);
    procedure actOpenTxtExecute(Sender: TObject);
    procedure actOpenXSPFExecute(Sender: TObject);
    procedure actRemoveFilesFromPlaylistExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actSaveM3u8Execute(Sender: TObject);
    procedure actSaveM3uAnsiExecute(Sender: TObject);
    procedure actSaveM3uBOMExecute(Sender: TObject);
    procedure actSaveM3uExecute(Sender: TObject);
    procedure actSaveiTunesTxtExecute(Sender: TObject);
    procedure actSaveXSPFExecute(Sender: TObject);
    procedure actTryOpenContainingFolderExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemHelpClick(Sender: TObject);
    procedure MenuItemSelectFilesClick(Sender: TObject);
    procedure MenuItemReplaceClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemVerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TimerHideStatusbarTimer(Sender: TObject);
    procedure VirtualStringTree1BeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1ColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VirtualStringTree1DragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VirtualStringTree1DragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: LongWord;
      Mode: TDropMode);
    procedure VirtualStringTree1DragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
      Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
    procedure VirtualStringTree1FocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VirtualStringTree1HeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
  private
    slMain:TStringlist;
    xDocMain: TXMLDocument;
    currentFormat:TPlaylistType;
    isBusy:boolean;
    isRearrangedOrder:boolean;
    isLocationEdited:boolean;
    isDirty:boolean;
    isCloseRequested:boolean;
    FstrAppVer:string;
    function URLEncode(s: string): string;
    function URLDecode(s: string): string;
    function ConvertURItoLocalPath(const strPath:string):string;
    function ConvertLocalPathToURI(const strURI:string):string;
    function ConvertMacPathToUnixPath(const strURI:string):string;
    procedure SaveM3uAs(blnAnsi:boolean; blnUTF8wBOM:boolean; strFileExt:string);
  public
    procedure CreateNew(sl:TStrings);
    procedure ReplaceAll(sFind:string;sReplace:string);

  end;

var
  frmMain: TfrmMain;
  frmOptions : TfrmOptions;

implementation

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Column0: integer;
    Column1: String;
    Column2: integer;
  end;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FstrAppVer:='ver 0.0.1';
  PanelContents.BringToFront;
  slMain:= TStringList.Create;

  actSaveiTunesTxt.Enabled:=false;
  actSaveM3u.Enabled:=false;
  actSaveM3uAnsi.Enabled:=false;
  actSaveM3uBOM.Enabled:=false;
  actSaveM3u8.Enabled:=false;
  actSaveXSPF.Enabled:=false;

  actConvertToM3U.Enabled:=false;
  actConvertToXSPF.Enabled:=false;

  //actReplaceAll.Enabled:=false;
  actCopySelectedPath.Enabled:=false;
  actTryOpenContainingFolder.Enabled:=false;
  actRemoveFilesFromPlaylist.Enabled:=false;
  actCopySelectedPath.Enabled:=false;
  actAddFilesToPlaylist.Enabled:=false;

  ProgressBar1.Height:=6;
  ProgressBar1.BorderWidth:=0;
  ProgressBar1.Visible:=false;

  StatusBar1.Visible:=false;
  //hide StatusBar1
  TimerHideStatusbar.Enabled:=false;
  TimerHideStatusbar.Interval:=5000;

  VirtualStringTree1.DefaultNodeHeight:=36;

  VirtualStringTree1.Header.SortColumn:=0;
  VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);

  //self.AllowDropFiles:=true;

  {$ifdef windows}
  VirtualStringTree1.DragType:=dtOLE; //needs this for drag&drop from explorer.
  //VirtualStringTree1.DragType:=dtVCL; //test
  {$else}
  VirtualStringTree1.DragType:=dtVCL;
  {$endif}
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

end;

procedure TfrmMain.MenuItemHelpClick(Sender: TObject);
begin

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  // show nice little welcome form?
  {
  if not Assigned(frmOptions) then begin
    frmOptions := TfrmOptions.Create(self);
    frmOptions.Caption:='Welcome';

    frmOptions.PageControl1.ActivePageIndex:=0;

    if (frmOptions.ShowModal = mrOK) then
    begin
      if Assigned(frmOptions.closeAction) then
      begin
         frmOptions.closeAction.Execute;
      end;
    end;
  end;
  }
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isCloseRequested:=true;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  slMain.Free;
  if Assigned(xDocMain) then xDocMain.Free;
end;

procedure TfrmMain.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.MenuItemVerClick(Sender: TObject);
begin
  showmessage(ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+ ' - ' + FstrAppVer);
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
//var
//  Node: PVirtualNode;
begin
  //copy
  //Node := VirtualStringTree1.FocusedNode;
  //if Node = nil then begin
  //  actCopySelectedPath.Enabled:=false;
  //  actTryOpenContainingFolder.Enabled:=false;
  //end else begin
  // actCopySelectedPath.Enabled:=true;
  // actTryOpenContainingFolder.Enabled:=true;
  //end;

  if VirtualStringTree1.SelectedCount > 0 then
  begin
    actRemoveFilesFromPlaylist.Enabled:=true;
    actCopySelectedPath.Enabled:=true;
  end else
  begin
    actRemoveFilesFromPlaylist.Enabled:=false;
    actCopySelectedPath.Enabled:=false;
  end;

  if (currentFormat = error) then begin
    actAddFilesToPlaylist.Enabled:=false;
  end else
  begin
    actAddFilesToPlaylist.Enabled:=true;
  end;

end;

// New playlist file
procedure TfrmMain.actNewFileExecute(Sender: TObject);
begin
  MenuItemSelectFilesClick(nil);
end;

procedure TfrmMain.MenuItemSelectFilesClick(Sender: TObject);
begin
  if isBusy then exit;

  //TODO check isDirty

  isRearrangedOrder:=false;
  isLocationEdited:=false;
  isDirty:=false;

  VirtualStringTree1.Header.SortColumn:=0;
  VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);

  //open
  OpenDialog1.DefaultExt:='*.*';
  OpenDialog1.Filter:='Files|*.*';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist, ofAllowMultiSelect ];
  if OpenDialog1.Execute then
  begin
    if (OpenDialog1.Files.Count > 0)then
    begin

      CreateNew(OpenDialog1.Files);

    end;
  end;
end;

procedure TfrmMain.CreateNew(sl:TStrings);
var
  i:integer;
  Data: PNodeData;
  tNode: PVirtualNode;
begin
  //accepts OpenDialog.files or Dropfiles

  isBusy:=true;
  screen.Cursor:=crHourGlass;

  self.Caption:=' [New] ';

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;

  VirtualStringTree1.Header.Columns[0].Text:='#';
  {$ifdef windows}
  VirtualStringTree1.Header.Columns[1].Text:='New Playlist (Windows file system path)';
  {$else}
  VirtualStringTree1.Header.Columns[1].Text:='New Playlist (Linux file system path)';
  {$endif}
  currentFormat:=new;

  Statusbar1.Panels[0].Text:='New';
  Statusbar1.Panels[1].Text:='';

  ProgressBar1.Position := 0;
  ProgressBar1.Max := sl.Count;
  ProgressBar1.Style:=pbstNormal;
  ProgressBar1.Visible:=true;

  for i := 0 to -1 + sl.Count do
  begin
    slMain.Add(sl[i]);

    tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
    Data := VirtualStringTree1.GetNodeData(tNode);
    Data^.Column0 := i+1;
    Data^.Column1 := sl[i];
    Data^.Column2 := i+1;

    ProgressBar1.Position := i;
    Statusbar1.Panels[1].Text:='Processing... ['+intToStr(i+1)+'/'+intToStr(sl.Count) + ']' ;
    Application.ProcessMessages;
    if (Application.Terminated or isCloseRequested) then exit;

  end;
  Statusbar1.Panels[1].Text:='';
  isBusy:=false;

  //can't do it. so,
  actSaveiTunesTxt.Enabled:=false;

  actSaveM3u.Enabled:=true;
  actSaveXSPF.Enabled:=false;
  actConvertToM3U.Enabled:=true;
  actConvertToXSPF.Enabled:=true;


  screen.Cursor:=crDefault;
  StatusBar1.Visible:=false;
  ProgressBar1.Visible:=false;
  ProgressBar1.Position:=0;
end;

// Open playlist

// Open iTunes txt
procedure TfrmMain.actOpenTxtExecute(Sender: TObject);
var
  slRow:TStringlist;
  line:integer;
  el:TCharEncStream;
  Data: PNodeData;
  tNode: PVirtualNode;
  filename:string;
begin
  if isBusy then exit;

  //TODO check isDirty

  isRearrangedOrder:=false;
  isLocationEdited:=false;
  isDirty:=false;

  VirtualStringTree1.Header.SortColumn:=0;
  VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);

  OpenDialog1.DefaultExt:='*.txt';
  OpenDialog1.Filter:='Text|*.txt';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if (not FileExists(OpenDialog1.Filename)) then exit;
       filename:=OpenDialog1.Filename;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;
  TimerHideStatusbar.Enabled:=false;//just in case.

  self.Caption:=' [iTunes Text(TSV)] ' + OpenDialog1.Filename;

  //init variables
  slMain.Clear;
  VirtualStringTree1.Clear;
  //VirtualStringTree1.DefaultNodeHeight:=36;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  // TODO!! Windows path or Mac path?
  VirtualStringTree1.Header.Columns[1].Text:='iTunes Playlist (file path)';

  currentFormat:=iTunesTSV;

  Statusbar1.Panels[0].Text:='iTunes';
  Statusbar1.Panels[1].Text:='';
  Statusbar1.Visible:=true;

  el:=TCharEncStream.Create;
  slRow:=TStringlist.Create;

  try
    try
      el.LoadFromFile(filename);
      //convert UTF-16 to UTF-8 and "load" to the main list.
      slMain.Text:=el.UTF8Text;

      slRow.StrictDelimiter := true;
      slRow.Delimiter := #$9; //TAB
      //slRow.QuoteChar := '"';
      slRow.LineBreak:=#13#10;

      ProgressBar1.Position := 0;
      ProgressBar1.Max := slMain.Count;
      ProgressBar1.Style:=pbstNormal;
      ProgressBar1.Visible:=true;

      if (slMain.Count > 0) then begin

        //for each tab delimited line
        for line := 1 to -1 + slMain.Count do
        begin
          slRow.DelimitedText := slMain[line];

          if (slRow.Count = 31) then begin
            //add to treeview
            tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
            Data := VirtualStringTree1.GetNodeData(tNode);
            Data^.Column0 := line;
            Data^.Column1 := slRow[30];
            Data^.Column2 := line;
          end else begin
              showmessage('Wrong column number. invalid text!');
              Statusbar1.Panels[1].Text:='Error: Wrong column number. Invalid text!';
              Statusbar1.Visible:= true;
              isBusy:=false;
              screen.Cursor:=crDefault;
              //TODO raise exception
              exit;
          end;

          ProgressBar1.Position := line;
          Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line)+'/'+intToStr(slMain.Count-1) + ']' ;
          Application.ProcessMessages;
          if (Application.Terminated or isCloseRequested) then exit;
        end;

        Statusbar1.Panels[1].Text:='';
        Statusbar1.Visible:=false;

        //iTunes to iTunes is ok.
        actSaveiTunesTxt.Enabled:=true;

        actSaveM3u.Enabled:=true;
        actSaveM3uBOM.Enabled:=true;
        actSaveM3uAnsi.Enabled:=true;
        actSaveM3u8.Enabled:=true;
        actSaveXSPF.Enabled:=false;

        actConvertToM3U.Enabled:=true;
        actConvertToXSPF.Enabled:=true;

      end else begin
        showmessage('Empty & invalid text!');
        //todo more error msg?
        Statusbar1.Panels[1].Text:='Empty & invalid text!';
        Statusbar1.Visible:= true;
        TimerHideStatusbar.Enabled:=true;

        isBusy:=false;
        screen.Cursor:=crDefault;

        slRow.Free;
        el.Free;
        exit;
      end;
    except
      Statusbar1.Panels[1].Text:='Error opening txt: '+filename;
      Statusbar1.Visible:= true;
      TimerHideStatusbar.Enabled:=true;
      slMain.Clear;
      actSaveiTunesTxt.Enabled:=false;
      actSaveM3u.Enabled:=false;
      actSaveM3uBOM.Enabled:=false;
      actSaveM3uAnsi.Enabled:=false;
      actSaveM3u8.Enabled:=false;
      actSaveXSPF.Enabled:=false;
      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=false;
      currentFormat:=error;
    end;
  finally
    isBusy:=false;
    slRow.Free;
    el.Free;
    screen.Cursor:=crDefault;

    ProgressBar1.Visible:=false;
    ProgressBar1.Position:=0;

  end;
end;

// Open M3U
procedure TfrmMain.actOpenM3uExecute(Sender: TObject);
var
  line,i:integer;
  el:TCharEncStream;
  Data: PNodeData;
  tNode: PVirtualNode;
  filename:string;
begin
  if isBusy then exit;

  //TODO check isDirty

  isRearrangedOrder:=false;
  isLocationEdited:=false;
  isDirty:=false;

  VirtualStringTree1.Header.SortColumn:=0;
  VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);

  //open m3u
  OpenDialog1.DefaultExt:='*.m3u';
  OpenDialog1.Filter:='M3U|*.m3u;*.m3u8';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if not (FileExists(OpenDialog1.Filename)) then exit;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;
  TimerHideStatusbar.Enabled:=false;//just in case.

  self.Caption:=' [M3U] ' + OpenDialog1.Filename;

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='M3U Playlist ';

  currentFormat:=m3u;
  filename := OpenDialog1.Filename;

  Statusbar1.Panels[0].Text:='M3U';
  Statusbar1.Panels[1].Text:='';
  Statusbar1.Visible:=true;

  el:=TCharEncStream.Create;

  try
    try
      el.LoadFromFile(filename);

      slMain.Text:=el.UTF8Text;

      ProgressBar1.Position := 0;
      ProgressBar1.Max := slMain.Count;
      ProgressBar1.Style:=pbstNormal;
      ProgressBar1.Visible:=true;

      line:=0;
      for i := 0 to -1 + slMain.Count do
      begin

        if (not AnsiStartsStr('#', slMain.Strings[i])) and
           (not (slMain.Strings[i] = #13#10)) and
           (not (slMain.Strings[i] = '')) then
        begin

          line:=line+1;
          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := line;
          Data^.Column1 := slMain[i];
          Data^.Column2 := line;

          ProgressBar1.Position := i;
          Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line)+'/'+intToStr(slMain.Count) + ']' ;
          Application.ProcessMessages;
          if (Application.Terminated or isCloseRequested) then exit;

        end;
      end;
      Statusbar1.Panels[1].Text:='';
      Statusbar1.Visible:=false;

      //can't do it. so,
      actSaveiTunesTxt.Enabled:=false;

      actSaveM3u.Enabled:=true;
      actSaveM3uBOM.Enabled:=true;
      actSaveM3uAnsi.Enabled:=true;
      actSaveM3u8.Enabled:=true;
      actSaveXSPF.Enabled:=false;

      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=true;

    except
      //todo
      Statusbar1.Panels[1].Text:='Error opening m3u: '+filename;
      Statusbar1.Visible:= true;
      TimerHideStatusbar.Enabled:=true;
      slMain.Clear;
      actSaveiTunesTxt.Enabled:=false;
      actSaveM3u.Enabled:=false;
      actSaveM3uBOM.Enabled:=false;
      actSaveM3uAnsi.Enabled:=false;
      actSaveM3u8.Enabled:=false;
      actSaveXSPF.Enabled:=false;
      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=false;
      currentFormat:=error;
    end;
  finally
    el.Free;
    isBusy:=false;
    screen.Cursor:=crDefault;
    //stop bar
    ProgressBar1.Visible:=false;
    ProgressBar1.Style:=pbstNormal;
    ProgressBar1.Position:=0;
  end;
end;

// Open XSPF
procedure TfrmMain.actOpenXSPFExecute(Sender: TObject);
var
  xTrackListNode,xTrackNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
  Data: PNodeData;
  tNode: PVirtualNode;
  line:integer;
  filename:string;
begin
  if isBusy then exit;

  //TODO check isDirty

  isRearrangedOrder:=false;
  isLocationEdited:=false;
  isDirty:=false;

  VirtualStringTree1.Header.SortColumn:=0;
  VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);

  //open xspf
  OpenDialog1.DefaultExt:='*.xspf';
  OpenDialog1.Filter:='XSPF|*.xspf';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if not (FileExists(OpenDialog1.Filename)) then exit;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;
  TimerHideStatusbar.Enabled:=false;//just in case.

  self.Caption:=' [XSPF] ' + OpenDialog1.Filename;

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='XSPF Playlist (URI)';

  currentFormat:=xspf;
  filename := OpenDialog1.Filename;
  Statusbar1.Panels[0].Text:='XSPF';
  Statusbar1.Panels[1].Text:='';
  Statusbar1.Visible:=true;

  try
    if Assigned(xDocMain) then xDocMain.Free;
    ReadXMLFile(xDocMain, filename);

    ProgressBar1.Position := 0;
    //ProgressBar1.Max := //no way to know.


    if Assigned(xDocMain) then
    begin

     if Assigned(xDocMain.DocumentElement) then
     begin
       if (xDocMain.DocumentElement.NodeName = 'playlist') then
       begin
         //start ProgressBar
         ProgressBar1.Style:=pbstMarquee;
         progressBar1.Visible:=true;

         xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
         if Assigned(xTracklistNodelist) then
         begin
           if (xTracklistNodelist.Count>0) then
           begin
             xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
             if Assigned(xTrackListNode) then
             begin

               xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
               if Assigned(xTrackNodelist) then
               begin
                 if (xTrackNodelist.Count>0) then
                 begin
                   xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                   line:=0;

                   while Assigned(xTrackNode) do
                   begin

                     xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                     if Assigned(xLocationNodelist) then
                     begin
                       if (xLocationNodelist.Count>0) then
                       begin

                         line:=line+1;

                         slMain.Add(xLocationNodelist.Item[0].TextContent);

                         tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
                         Data := VirtualStringTree1.GetNodeData(tNode);
                         Data^.Column0 := line;
                         Data^.Column1 := xLocationNodelist.Item[0].TextContent;
                         Data^.Column2 := line;

                         Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line) + ']' ;
                         Application.ProcessMessages;
                         if (Application.Terminated or isCloseRequested) then exit;
                       end;
                     end;
                     xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                   end;

                   Statusbar1.Panels[1].Text:='';
                 end;
               end;
             end;
           end;
         end;

       end;
     end;
    end;

    isBusy:=false;
    screen.Cursor:=crDefault;

    //can't do it. so,
    actSaveiTunesTxt.Enabled:=false;

    actSaveM3u.Enabled:=false;
    actSaveM3uBOM.Enabled:=false;
    actSaveM3uAnsi.Enabled:=false;
    actSaveM3u8.Enabled:=false;
    actSaveXSPF.Enabled:=true;

    actConvertToM3U.Enabled:=true;
    actConvertToXSPF.Enabled:=false;

    Statusbar1.Panels[1].Text:='';
    Statusbar1.Visible:= false;

    //stop ProgressBar
    ProgressBar1.Visible:=false;
    ProgressBar1.Style:=pbstNormal;
    ProgressBar1.Position:=0;
    TimerHideStatusbar.Enabled:=true;
    except
      On E :Exception do begin
        isBusy:=false;
        screen.Cursor:=crDefault;
        //stop ProgressBar
        ProgressBar1.Visible:=false;
        ProgressBar1.Style:=pbstNormal;
        ProgressBar1.Position:=0;

        Statusbar1.Visible:= true;
        TimerHideStatusbar.Enabled:=true;

        actSaveiTunesTxt.Enabled:=false;
        actSaveM3u.Enabled:=false;
        actSaveM3uBOM.Enabled:=false;
        actSaveM3uAnsi.Enabled:=false;
        actSaveM3u8.Enabled:=false;
        actSaveXSPF.Enabled:=false;
        actConvertToM3U.Enabled:=false;
        actConvertToXSPF.Enabled:=false;

        currentFormat:=error;
        //Statusbar1.Panels[1].Text:='Error opening xspf: '+filename;
        Statusbar1.Panels[1].Text:= 'Error opening xspf: '+ E.Message;
      end;
    end;

end;


// Save playlist

// Save iTunes txt
procedure TfrmMain.actSaveiTunesTxtExecute(Sender: TObject);
var
  us:TCharEncStream;
  slFile:TStringList;
  tNode: PVirtualNode;
  Data: PNodeData;
begin
  if isBusy then exit;
  if slMain.Count < 1 then exit;

  //save as txt
  case currentFormat of
    iTunesTSV:begin

      us:=TCharEncStream.Create;
      slFile:=TStringlist.Create;
      try

        if SaveDialog1.Execute then
        begin

          //TODO only if isDragged Rearranged deleted?
          tNode := VirtualStringTree1.GetFirst;
          while Assigned(tNode) do
          begin
            Data:= VirtualStringTree1.GetNodeData(tNode);

            slFile.Add(slMain[Data^.Column2]); //don't -1 iTunes col header

            tNode := VirtualStringTree1.GetNextSibling(tNode);
          end;

           //save as BOMed UTF-16
          us.HasBOM:=true;
          us.HaveType:=true;
          us.ForceType:=true;
          us.UniStreamType:=ufUtf16le;//ufUtf16be;
          us.UTF8Text:=slFile.Text;

          SaveDialog1.DefaultExt:='*.txt';
          SaveDialog1.Filter:='TEXT|*.txt';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;

          try
            us.SaveToFile(SaveDialog1.Filename);
            Statusbar1.Panels[1].Text:='Saved as txt: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
            self.Caption:=' [iTunes Text(TSV)] ' + SaveDialog1.Filename;

            isDirty:=false;
          except
            Statusbar1.Panels[1].Text:='Error saving txt: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
          end;
        end;
      finally
        us.Free;
        slFile.Free;
      end;
    end else begin
      showmessage('"m3u/xspf to iTunes text" not supported! Use m3u.');//shouldn't be happening.
      Statusbar1.Panels[1].Text:= '"m3u/xspf to iTunes text" not supported! Use m3u.';
      TimerHideStatusbar.Enabled:=true;
    end;
  end;
end;

// Save M3U
procedure TfrmMain.actSaveM3uExecute(Sender: TObject);
begin
  //UTF8 without BOM .m3u
  SaveM3uAs(false,false,'.m3u');
end;

procedure TfrmMain.actSaveM3uBOMExecute(Sender: TObject);
begin
  //UTF8 with BOM .m3u
  SaveM3uAs(false,true,'.m3u');
end;

procedure TfrmMain.actSaveM3u8Execute(Sender: TObject);
begin
  //UTF8 with BOM .m3u8
  SaveM3uAs(false,true,'.m3u8');
end;

procedure TfrmMain.actSaveM3uAnsiExecute(Sender: TObject);
begin
  //Ansi
  SaveM3uAs(true,false,'.m3u');
end;

procedure TfrmMain.SaveM3uAs(blnAnsi:boolean; blnUTF8wBOM:boolean; strFileExt:string);
var
  slFile,slRow:TStringList;
  fs:TFileStream;
  Buffer: RawByteString;//should work with string?
  tNode: PVirtualNode;
  Data: PNodeData;
begin
  if isBusy then exit;
  if slMain.Count < 1 then exit;

  //save as m3u
  case currentFormat of

    iTunesTSV:begin


      slFile:=TStringlist.Create;
      slRow:=TStringlist.Create;
      slRow.StrictDelimiter := true;
      slRow.Delimiter := #$9; //TAB
      slRow.LineBreak:=#13#10;

      try

        SaveDialog1.DefaultExt:='*'+strFileExt;
        SaveDialog1.Filter:='M3U|*'+strFileExt;
        SaveDialog1.FileName:='';
        SaveDialog1.Files.Clear;
        if SaveDialog1.Execute then
        begin
          try

            tNode := VirtualStringTree1.GetFirst;
            while Assigned(tNode) do
            begin
              Data:= VirtualStringTree1.GetNodeData(tNode);

              slRow.DelimitedText := slMain[Data^.Column2];    //don't -1. iTunes col header.

              slFile.Add(ConvertMacPathToUnixPath(slRow[30]));

              tNode := VirtualStringTree1.GetNextSibling(tNode);
            end;


            if blnAnsi then
            begin
              //TODO UTF8ToWinCP $IFDEF Windows?? or UTF8ToSys
              Buffer := UTF8ToWinCP(slFile.Text);
              fs:=TFileStream.Create(SaveDialog1.Filename,fmCreate);
              fs.Write(PChar(Buffer)^, Length(Buffer));
              fs.Free;
            end else if blnUTF8wBOM then
            begin
              slFile.Text:=UTF8ToUTF8BOM(slFile.Text);
              slFile.SaveToFile(SaveDialog1.Filename);
            end else
            begin
              //UTF-8 without BOM
              slFile.SaveToFile(SaveDialog1.Filename);
            end;

            isDirty:=false;

            Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
            self.Caption:=' [M3U] ' + SaveDialog1.Filename;
          except
            //todo
            Statusbar1.Panels[1].Text:='Error saving m3u: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
          end;
        end;

      finally
        slFile.Free;
        slRow.Free;
      end;

      {
      slFile:=TStringlist.Create;
      slRow:=TStringlist.Create;
      slRow.StrictDelimiter := true;
      slRow.Delimiter := #$9; //TAB
      //slRow.QuoteChar := '"';
      slRow.LineBreak:=#13#10;

      for i := 1 to -1 + slMain.Count do
      begin
        slRow.DelimitedText := slMain[i];
        slFile.Add(slRow[30]);
      end;

      try

        SaveDialog1.DefaultExt:='*'+strFileExt;
        SaveDialog1.Filter:='M3U|*'+strFileExt;
        SaveDialog1.FileName:='';
        SaveDialog1.Files.Clear;
        if SaveDialog1.Execute then
        begin
          try

            if blnAnsi then
            begin
              //TODO UTF8ToWinCP $IFDEF Windows?? or UTF8ToSys
              Buffer := UTF8ToWinCP(slFile.Text);
              fs:=TFileStream.Create(SaveDialog1.Filename,fmCreate);
              fs.Write(PChar(Buffer)^, Length(Buffer));
              fs.Free;
            end else if blnUTF8wBOM then
            begin
              slFile.Text:=UTF8ToUTF8BOM(slFile.Text);
              slFile.SaveToFile(SaveDialog1.Filename);
            end else
            begin
              //UTF-8 without BOM
              slFile.SaveToFile(SaveDialog1.Filename);
            end;

            Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
            self.Caption:=' [M3U] ' + SaveDialog1.Filename;
          except
            //todo
            Statusbar1.Panels[1].Text:='Error saving m3u: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
          end;
        end;

      finally
        slFile.Free;
        slRow.Free;
      end;
      }
    end;

    m3u:begin

      SaveDialog1.DefaultExt:='*'+strFileExt;
      SaveDialog1.Filter:='M3U|*'+strFileExt;
      SaveDialog1.FileName:='';
      SaveDialog1.Files.Clear;
      if SaveDialog1.Execute then
      begin

        try

          slFile:=TStringlist.Create;
          if isRearrangedOrder then //try preserve m3U extention
          begin
            tNode := VirtualStringTree1.GetFirst;
            while Assigned(tNode) do
            begin
              Data:= VirtualStringTree1.GetNodeData(tNode);

              slFile.Add(slMain[Data^.Column2-1]);

              tNode := VirtualStringTree1.GetNextSibling(tNode);
            end;
          end else
          begin
            slFile.Assign(slMain);
          end;

          if blnAnsi then
          begin
            //TODO UTF8ToWinCP $IFDEF Windows?? or UTF8ToSys
            //Buffer := UTF8ToWinCP(slMain.Text);
            Buffer := UTF8ToWinCP(slFile.Text);
            fs:=TFileStream.Create(SaveDialog1.Filename,fmCreate);
            fs.Write(PChar(Buffer)^, Length(Buffer));
            fs.Free;
          end else if blnUTF8wBOM then
          begin
            //slMain.Text:=UTF8ToUTF8BOM(slMain.Text);
            //slMain.SaveToFile(SaveDialog1.Filename);
            slFile.Text:=UTF8ToUTF8BOM(slFile.Text);
            slFile.SaveToFile(SaveDialog1.Filename);
          end else
          begin
            //UTF-8 without BOM
            //slMain.SaveToFile(SaveDialog1.Filename);
            slFile.SaveToFile(SaveDialog1.Filename);
          end;

          isDirty:=false;

          slFile.Free;

          Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
          self.Caption:=' [M3U] ' + SaveDialog1.Filename;
        except
          //todo
          Statusbar1.Panels[1].Text:='Error saving m3u: '+SaveDialog1.Filename;
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
        end;
      end;

    end;

    xspf:begin
      //need URI conversion. you have to convert first to save as m3u.
      //disabled in menu.
    end;

    new:begin

      SaveDialog1.DefaultExt:='*'+strFileExt;
      SaveDialog1.Filter:='M3U|*'+strFileExt;
      SaveDialog1.FileName:='';
      SaveDialog1.Files.Clear;
      if SaveDialog1.Execute then
      begin

        try
          slFile:=TStringlist.Create;
          tNode := VirtualStringTree1.GetFirst;
          while Assigned(tNode) do
          begin
            Data:= VirtualStringTree1.GetNodeData(tNode);

            slFile.Add(slMain[Data^.Column2-1]);

            tNode := VirtualStringTree1.GetNextSibling(tNode);
          end;

          if blnAnsi then
          begin
            //TODO UTF8ToWinCP $IFDEF Windows?? or UTF8ToSys
            Buffer := UTF8ToWinCP(slFile.Text);
            fs:=TFileStream.Create(SaveDialog1.Filename,fmCreate);
            fs.Write(PChar(Buffer)^, Length(Buffer));
            fs.Free;
          end else if blnUTF8wBOM then
          begin
            slFile.Text:=UTF8ToUTF8BOM(slFile.Text);
            slFile.SaveToFile(SaveDialog1.Filename);
          end else
          begin
            //UTF-8 without BOM
            slFile.SaveToFile(SaveDialog1.Filename);
          end;

          isDirty:=false;

          slFile.Free;

          Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
          self.Caption:=' [M3U] ' + SaveDialog1.Filename;
        except
          //todo
          Statusbar1.Panels[1].Text:='Error saving m3u: '+SaveDialog1.Filename;
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
        end;
      end;

    end;
  end;
end;

// Save XSPF
procedure TfrmMain.actSaveXSPFExecute(Sender: TObject);
var
  xDoc:TXMLDocument;
  xRootNode,xTrackListNode,xTrackNode,xNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
  xDomText:TDOMText;
  Data: PNodeData;
  tNode: PVirtualNode;
  line:integer;
begin
  if isBusy then exit;

  //save as xspf
  case currentFormat of
    iTunesTSV:begin
      //disabled in menu. you have to convert it first.
    end;

    m3u:begin
      //disabled in menu. you have to convert it first.
    end;

    xspf:begin

      if not Assigned(xDocMain) then exit; //todo

      //use existing xDocMain
      SaveDialog1.DefaultExt:='xspf';
      SaveDialog1.Filter:='XSPF|*.xspf';
      SaveDialog1.FileName:='';
      SaveDialog1.Files.Clear;
      if SaveDialog1.Execute then
      begin
        try
          if isRearrangedOrder then
          begin

            screen.Cursor:=crHourGlass;
            ProgressBar1.Position := 0;

            {
            slMain.Clear;
            //self.Caption:=' [XSPF] processing...';

            if Assigned(xDocMain) then
            begin
              if Assigned(xDocMain.DocumentElement) then
              begin
               if (xDocMain.DocumentElement.NodeName = 'playlist') then
               begin

                 ProgressBar1.Style:=pbstMarquee;//start bar
                 progressBar1.Visible:=true;

                 xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
                 if Assigned(xTracklistNodelist) then
                 begin
                   if (xTracklistNodelist.Count>0) then
                   begin
                     xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
                     if Assigned(xTrackListNode) then
                     begin

                       xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
                       if Assigned(xTrackNodelist) then
                       begin
                         if (xTrackNodelist.Count>0) then
                         begin
                           xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                           line:=0;

                           while Assigned(xTrackNode) do
                           begin

                             xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                             if Assigned(xLocationNodelist) then
                             begin
                               if (xLocationNodelist.Count>0) then
                               begin

                                 line:=line+1;

                                 slMain.Add(xLocationNodelist.Item[0].TextContent);

                                 Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line) + ']' ;
                                 Application.ProcessMessages;

                               end;
                             end;
                             xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                           end;

                           Statusbar1.Panels[1].Text:='';
                         end;
                       end;
                     end;
                   end;
                 end;

               end;
              end;
              //TODO why do I get error.
              //xDocMain.Free;
            end;
            }

            xdoc := TXMLDocument.create;
            xRootNode := xdoc.CreateElement('playlist');
            xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
            xdoc.Appendchild(xRootNode);

            xTrackListNode := xdoc.CreateElement('trackList');
            xRootNode.AppendChild(xTrackListNode);

            tNode := VirtualStringTree1.GetFirst;
            while Assigned(tNode) do
            begin
                xTrackNode := xdoc.CreateElement('track');
                xNode := xdoc.CreateElement('location');

                //get treedata
                Data:= VirtualStringTree1.GetNodeData(tNode);
                //set data
                xDomText:=xDoc.CreateTextNode(slMain[Data^.Column2-1]);
                //append xml
                xNode.AppendChild(xDomText);
                xTrackNode.AppendChild(xNode);

                xTrackListNode.AppendChild(xTrackNode);

                //go to next tree
                tNode := VirtualStringTree1.GetNextSibling(tNode);
            end;

            writeXMLFile(xdoc,SaveDialog1.filename);

            isDirty:=false;
          end else
          begin
            writeXMLFile(xDocMain,SaveDialog1.filename);
            isDirty:=false;
          end;

          isBusy:=false;
          screen.Cursor:=crDefault;

          Statusbar1.Panels[1].Text:='saved as xspf.';
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
          self.Caption:=' [XSPF] ' + SaveDialog1.Filename;

          //stop progress bar
          ProgressBar1.Visible:=false;
          ProgressBar1.Style:=pbstNormal;
          ProgressBar1.Position:=0;

        except
          //todo
          Statusbar1.Panels[1].Text:='error saving xspf.';
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;

          isBusy:=false;
          screen.Cursor:=crDefault;
          //stop progress bar
          ProgressBar1.Visible:=false;
          ProgressBar1.Style:=pbstNormal;
          ProgressBar1.Position:=0;
        end;
      end;

    end;

    new:begin
      //disabled in menu. you have to convert it first.
    end;

  end;
end;


// Convert to

// %encording for URI
function TfrmMain.URLEncode(s: string): string;
var
  i: integer;
  source: PAnsiChar;
begin
  // https://www.tweaking4all.com/forums/topic/delphilazarusfpc-how-to-encode-and-decode-urls-2-simple-functions/

  result := '';
  source := pansichar(s);
  for i := 1 to length(source) do
    if not (source[i - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.', ':', '/']) then
      result := result + '%' + inttohex(ord(source[i - 1]), 2)
    else
      result := result + source[i - 1];
end;

function TfrmMain.URLDecode(s: string): string;
var
  i,lengthsource: integer;
  source: PAnsiChar;
begin
  result := '';
  source := pansichar(s);
  lengthsource := length(source);
  i:=1;
  while (i<=lengthsource) do
    begin
      if source[i-1] <> '%' then
        result := result + source[i-1]
      else if (source[i-1] = '%') and (i+1<=lengthsource) then
        try
          begin
            result := result + Chr(Hex2Dec('$'+source[i]+source[i+1]));
            i:=i+2;
          end;
        except
        end
      else
        result := result + source[i-1];
      inc(i);
    end;
end;

function TfrmMain.ConvertURItoLocalPath(const strPath:string):string;
var
  s:string;
begin

  if AnsiStartsStr('file:///',strPath) then
  begin
    //must be local path

    s:=stringReplace(strPath, 'file:///', '',[rfReplaceAll]);
    if AnsiContainsStr(s, ':/') then begin
      //must be Windows drive letters.
      s := StringReplace(s,'/','\',[rfReplaceAll]);
    end else
    begin
      //must be Linux?
      s:=stringReplace(strPath, 'file:///', '/',[rfReplaceAll]);
    end;

  end else if AnsiStartsStr('file://',strPath) then
  begin
    //must be Win UNC
    s:=stringReplace(strPath, 'file://', '\\',[rfReplaceAll]);
    s := StringReplace(s,'/','\',[rfReplaceAll]);
  end;

  result := URLDecode(s);
end;

function TfrmMain.ConvertLocalPathToURI(const strURI:string):string;
var
  s:string;
begin
  if length(strURI) < 2 then begin
    result:=strURI;
    exit;
  end;

  s:=strURI;

  if AnsiStartsStr('\\',s) then
  begin
    //must be windows network path (UNC).
    //replace \->/
    s := StringReplace(s,'\','/',[rfReplaceAll]);
    //URLEncode
    s:=URLEncode(s);
    //add "file:" in the beggining.
    s:='file:'+s;

  end else if AnsiStartsStr('\',s) then
  begin
    //must be windows relative path
    //replace \->/
    s := StringReplace(s,'\','/',[rfReplaceAll]);
    //URLEncode
    s:=URLEncode(s);

  end else if AnsiStartsStr('Macintosh HD:',s) then
  begin
    //Mac iTunes txt
    s := StringReplace(s,'Macintosh HD:','',[rfReplaceAll]);
    s := StringReplace(s,':','/',[rfReplaceAll]);
    s:=URLEncode(s);
    s:='file:///'+s;

  end else if AnsiContainsStr(s, '\') and (s[2] = ':') then
  begin
    //startWith "*:"
    //must be windows local and full path
    //replace \->/
    s := StringReplace(s,'\','/',[rfReplaceAll]);
    //URLEncode
    s:=URLEncode(s);
    //AddURIScheme('file:///');
    s:='file:///'+s;

  end else if AnsiContainsStr(s, '\') then //and (startWith '\' or startWith **?)
  begin
    //must be windows local & relative path
    //replace \->/
    s := StringReplace(s,'\','/',[rfReplaceAll]);
    //URLEncode
    s:=URLEncode(s);

  end else if AnsiContainsStr(s, '://')
           or AnsiContainsStr(s, ':/')
           //must be full URI
           //don't do anything.
           or AnsiContainsStr(s, '//')
           or AnsiContainsStr(s, '#')
           //must be relative URI or network URI
           then
  begin
    //don't do anything.

  end else if AnsiStartsStr('/',s) then
  begin
    //must be *nix abs path or relative URI
      //No way of knowing which is which?

      //don't add scheme if URI's relative path.
    //But, let's assume it is local file path.

    //URIScheme('file:///');
    s:='file://'+s;
    //URLEncode
    s:=URLEncode(s);
  end else
  begin
    //*nix relative path or relative URI
    //No way of knowing which is which.
    //don't add scheme.

    //URLEncode
    s:=URLEncode(s);
  end;
  {

  if contains '://'  or ':/'
    must be full URI

  if startWith '//' or startWith '#'
    must be relative URI or network URI

  if startWith ** or './' or '../'
      relative path or relative URI
      No way of knowing which is which?
      don't add scheme.
      URIEncode?

  if startWith '/'
      *nix abs path or relative URI
      No way of knowing which is which?
      don't add scheme.
      URIEncode?

  if startWith '**'
    *nix relative path or relative URI
    No way of knowing which is which?
    don't add scheme.
    URIEncode?

  }
  result:=s;
end;

function TfrmMain.ConvertMacPathToUnixPath(const strURI:string):string;
var
  s:string;
begin
  s:=strURI;
  if AnsiStartsStr('Macintosh HD:',s) then
  begin
    //Mac iTunes txt
    s := StringReplace(s,'Macintosh HD:','',[rfReplaceAll]);
    s := StringReplace(s,':','/',[rfReplaceAll]);
    s:='/'+s;
  end;
  result:=s;
end;

// Convert to M3U
procedure TfrmMain.actConvertToM3UExecute(Sender: TObject);
var
  slTmp,slRow:TStringList;
  s:string;
  i,line:integer;
  tNode: PVirtualNode;
  Data: PNodeData;
  xTrackListNode,xTrackNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
begin

  isDirty:=true;

  case currentFormat of

    iTunesTSV:begin
      slTmp:=TStringlist.Create;
      slRow:=TStringlist.Create;
      slRow.StrictDelimiter := true;
      slRow.Delimiter := #$9; //TAB
      slRow.LineBreak:=#13#10;

      isBusy:=true;
      screen.Cursor:=crHourGlass;

      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
          //get treedata
          Data:= VirtualStringTree1.GetNodeData(tNode);

          slRow.DelimitedText := slMain[Data^.Column2];//don't -1 iTunes header

          //set data
          slTmp.Add(ConvertMacPathToUnixPath(slRow[30]));

          //go to next tree
          tNode := VirtualStringTree1.GetNextSibling(tNode);
      end;

      {
      for i := 1 to -1 + slMain.Count do
      begin
        slRow.DelimitedText := slMain[i];
        slTmp.Add(slRow[30]);
      end;
      }

      //TODO isRearranged:=false;?

      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;
      slRow.Free;

      VirtualStringTree1.clear;
      VirtualStringTree1.Header.SortColumn:=0;
      VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;

      currentFormat:= m3u;

      self.Caption:=' [M3U] ';
      Statusbar1.Panels[0].Text:='M3U';
      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='M3U Playlist';

      isBusy:=false;
      screen.Cursor:=crDefault;

      //can't do it anymore. so,
      actSaveiTunesTxt.Enabled:=false;

      actSaveM3u.Enabled:=true;
      actSaveM3uBOM.Enabled:=true;
      actSaveM3uAnsi.Enabled:=true;
      actSaveM3u8.Enabled:=true;
      actSaveXSPF.Enabled:=false;

      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=true;

    end;

    m3u:begin
      //nothing to do. disable menu.
    end;

    xspf:begin

      if not Assigned(xDocMain) then
      begin

       exit;
      end;

      isBusy:=true;
      isLocationEdited:=true;

      screen.Cursor:=crHourGlass;
      ProgressBar1.Position := 0;

      slMain.Clear;
      slTmp:=TStringlist.Create;

      self.Caption:=' [XSPF] converting...';

      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='M3U Playlist';


      //if isRearranged,
      if Assigned(xDocMain) then
      begin
        if Assigned(xDocMain.DocumentElement) then
        begin
         if (xDocMain.DocumentElement.NodeName = 'playlist') then
         begin

           ProgressBar1.Style:=pbstMarquee;//start bar
           progressBar1.Visible:=true;

           xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
           if Assigned(xTracklistNodelist) then
           begin
             if (xTracklistNodelist.Count>0) then
             begin
               xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
               if Assigned(xTrackListNode) then
               begin

                 xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
                 if Assigned(xTrackNodelist) then
                 begin
                   if (xTrackNodelist.Count>0) then
                   begin
                     xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                     line:=0;

                     while Assigned(xTrackNode) do
                     begin

                       xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                       if Assigned(xLocationNodelist) then
                       begin
                         if (xLocationNodelist.Count>0) then
                         begin

                           line:=line+1;

                           //ConvertURItoLocalPath
                           s:=ConvertURItoLocalPath(xLocationNodelist.Item[0].TextContent);

                           slMain.Add(s);

                           {
                           tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
                           Data := VirtualStringTree1.GetNodeData(tNode);
                           Data^.Column0 := line;
                           Data^.Column1 := s;
                           Data^.Column2 := line;
                           }
                           Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line) + ']' ;
                           Application.ProcessMessages;

                         end;
                       end;
                       xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                     end;

                     Statusbar1.Panels[1].Text:='';
                   end;
                 end;
               end;
             end;
           end;

         end;
        end;
        //TODO why do I get an error...
        //xDocMain.Free;
      end;

      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
          //get treedata
          Data:= VirtualStringTree1.GetNodeData(tNode);

          slTmp.Add(slMain[Data^.Column2-1]);

          //go to next tree
          tNode := VirtualStringTree1.GetNextSibling(tNode);
      end;
      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;

      VirtualStringTree1.clear;
      VirtualStringTree1.Header.SortColumn:=0;
      VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;


      statusbar1.Panels[1].Text:='';
      isBusy:=false;
      screen.Cursor:=crDefault;

      StatusBar1.Visible:=false;
      ProgressBar1.Visible:=false;
      ProgressBar1.Position:=0;

      currentFormat:= m3u;

      self.Caption:=' [M3U] ';
      Statusbar1.Panels[0].Text:='M3U';

      actSaveiTunesTxt.Enabled:=false;
      actSaveM3u.Enabled:=true;
      actSaveM3uBOM.Enabled:=true;
      actSaveM3uAnsi.Enabled:=true;
      actSaveM3u8.Enabled:=true;
      actSaveXSPF.Enabled:=false;

      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=true;

    end;

    new:begin


      currentFormat:= m3u;

      self.Caption:=' [M3U] ';
      Statusbar1.Panels[0].Text:='M3U';
      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='M3U Playlist';

      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
          //get treedata
          Data:= VirtualStringTree1.GetNodeData(tNode);

          //set data
          slTmp.Add(slMain[Data^.Column2-1]);

          //go to next tree
          tNode := VirtualStringTree1.GetNextSibling(tNode);
      end;

      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;

      VirtualStringTree1.clear;
      VirtualStringTree1.Header.SortColumn:=0;
      VirtualStringTree1.Header.SortDirection:=VirtualTrees.TSortDirection(sdAscending);
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;


      actSaveiTunesTxt.Enabled:=false;
      actSaveM3u.Enabled:=true;
      actSaveM3uBOM.Enabled:=true;
      actSaveM3uAnsi.Enabled:=true;
      actSaveM3u8.Enabled:=true;
      actSaveXSPF.Enabled:=false;

      actConvertToM3U.Enabled:=false;
      actConvertToXSPF.Enabled:=true;
    end;

  end;
end;

// Convert to XSPF
procedure TfrmMain.actConvertToXSPFExecute(Sender: TObject);
var
  xDoc:TXMLDocument;
  xRootNode,xTrackListNode,xTrackNode,xNode:TDomElement;
  xDomText:TDOMText;
  slRow,slTmp:TStringList;
  i:integer;
  s:string;
  tNode: PVirtualNode;
  Data: PNodeData;
  iDoc:TXMLDocument;
  iRootNode,iTrackListNode,iTrackNode,iNode:TDomElement;
  iDomText:TDOMText;
begin

  isLocationEdited:=true;
  isDirty:=true;

  case currentFormat of

    iTunesTSV:begin

      screen.Cursor:=crHourGlass;
      ProgressBar1.Position := 0;

      //slMain.Clear;   not yet.
      slTmp:=TStringlist.Create;
      //not yet
      //VirtualStringTree1.clear;

      self.Caption:=' [XSPF] converting...';

      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='XSPF Playlist (URI)';

      //create xDoc
      xdoc := TXMLDocument.create;
      xRootNode := xdoc.CreateElement('playlist');
      xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
      xdoc.Appendchild(xRootNode);

      xTrackListNode := xdoc.CreateElement('trackList');
      xRootNode.AppendChild(xTrackListNode);

      slRow:=TStringlist.Create;
      slRow.StrictDelimiter := true;
      slRow.Delimiter := #$9; //TAB
      slRow.LineBreak:=#13#10;

      //rearranged.
      //for i := 1 to -1 + slMain.Count do
      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
        //slRow.DelimitedText := slMain[i];
        //get treedata
        Data:= VirtualStringTree1.GetNodeData(tNode);
        slRow.DelimitedText:=slMain[Data^.Column2];  //don't -1 iTunes header

        xTrackNode := xdoc.CreateElement('track');
        xNode := xdoc.CreateElement('location');

        //convert to uri
        s:=ConvertLocalPathToURI(slRow[30]);
        xDomText:=xDoc.CreateTextNode(s);
        slTmp.Add(s);

        {//later
        //add tree node
        tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
        Data := VirtualStringTree1.GetNodeData(tNode);
        Data^.Column0 := i;
        Data^.Column1 := s;
        Data^.Column2 := i;
         }

        xNode.AppendChild(xDomText);
        xTrackNode.AppendChild(xNode);

        xTrackListNode.AppendChild(xTrackNode);

        //go to next
        tNode := VirtualStringTree1.GetNextSibling(tNode);
      end;

      if Assigned(xDocMain) then xDocMain.Free;
      xDocMain := xdoc;

      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;

      //todo
      VirtualStringTree1.clear;
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;

      currentFormat:= xspf;

      self.Caption:=' [XSPF] ';
      Statusbar1.Panels[0].Text:='XSPF';

      isBusy:=false;
      screen.Cursor:=crDefault;

      //can't do it anymore. so,
      actSaveiTunesTxt.Enabled:=false;

      actSaveM3u.Enabled:=false;
      actSaveM3uBOM.Enabled:=false;
      actSaveM3uAnsi.Enabled:=false;
      actSaveM3u8.Enabled:=false;
      actSaveXSPF.Enabled:=true;

      actConvertToM3U.Enabled:=true;
      actConvertToXSPF.Enabled:=false;
    end;

    m3u:begin

      screen.Cursor:=crHourGlass;
      ProgressBar1.Position := 0;

      //slMain.Clear;
      slTmp:=TStringlist.Create;

      //VirtualStringTree1.clear;
      self.Caption:=' [XSPF] converting...';

      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='XSPF Playlist (URI)';


      //create xDoc
      xdoc := TXMLDocument.create;
      xRootNode := xdoc.CreateElement('playlist');
      xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
      xdoc.Appendchild(xRootNode);

      xTrackListNode := xdoc.CreateElement('trackList');
      xRootNode.AppendChild(xTrackListNode);

      //for i := 1 to -1 + slMain.Count do
      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
        Data:= VirtualStringTree1.GetNodeData(tNode);

        if (not AnsiStartsStr('#', slMain[Data^.Column2-1])) and
           (not (slMain[Data^.Column2-1] = #13#10)) and
           (not (slMain[Data^.Column2-1] = '')) then
        begin

          xTrackNode := xdoc.CreateElement('track');
          xNode := xdoc.CreateElement('location');

          //convert to uri
          s:=ConvertLocalPathToURI(slMain[Data^.Column2-1]);
          xDomText:=xDoc.CreateTextNode(s);
          slTmp.Add(s);

          {
          //add tree node
          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := i;
          Data^.Column1 := s;
          Data^.Column2 := i;
          }

          xNode.AppendChild(xDomText);
          xTrackNode.AppendChild(xNode);

          xTrackListNode.AppendChild(xTrackNode);


        end;
        //go to next tree
        tNode := VirtualStringTree1.GetNextSibling(tNode);
      end;

      if Assigned(xDocMain) then xDocMain.Free;
      xDocMain := xdoc;

      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;

      VirtualStringTree1.clear;
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;

      currentFormat:= xspf;

      self.Caption:=' [XSPF] ';
      Statusbar1.Panels[0].Text:='XSPF';

      isBusy:=false;
      screen.Cursor:=crDefault;

      //can't do it anymore. so,
      actSaveiTunesTxt.Enabled:=false;

      actSaveM3u.Enabled:=false;
      actSaveM3uBOM.Enabled:=false;
      actSaveM3uAnsi.Enabled:=false;
      actSaveM3u8.Enabled:=false;
      actSaveXSPF.Enabled:=true;

      actConvertToM3U.Enabled:=true;
      actConvertToXSPF.Enabled:=false;

    end;

    xspf:begin
      //nothing to do here. Disabled at menu.
    end;

    new:begin

      screen.Cursor:=crHourGlass;
      ProgressBar1.Position := 0;

      //slMain.Clear;
      slTmp:=TStringlist.Create;

      //VirtualStringTree1.clear;
      self.Caption:=' [XSPF] converting...';

      VirtualStringTree1.Header.Columns[0].Text:='#';
      VirtualStringTree1.Header.Columns[1].Text:='XSPF Playlist (URI)';


      //create xDoc
      xdoc := TXMLDocument.create;
      xRootNode := xdoc.CreateElement('playlist');
      xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
      xdoc.Appendchild(xRootNode);

      xTrackListNode := xdoc.CreateElement('trackList');
      xRootNode.AppendChild(xTrackListNode);


      //for i := 1 to -1 + slMain.Count do
      tNode := VirtualStringTree1.GetFirst;
      while Assigned(tNode) do
      begin
       Data:= VirtualStringTree1.GetNodeData(tNode);

       if (not AnsiStartsStr('#', slMain[Data^.Column2-1])) and
          (not (slMain[Data^.Column2-1] = #13#10)) and
          (not (slMain[Data^.Column2-1] = '')) then
       begin

          xTrackNode := xdoc.CreateElement('track');
          xNode := xdoc.CreateElement('location');

          //convert to uri
          s:=ConvertLocalPathToURI(slMain[Data^.Column2-1]);
          xDomText:=xDoc.CreateTextNode(s);
          slTmp.Add(s);

          {
          //add tree node
          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := i;
          Data^.Column1 := s;
          Data^.Column2 := i;
          }

          xNode.AppendChild(xDomText);
          xTrackNode.AppendChild(xNode);

          xTrackListNode.AppendChild(xTrackNode);

          //go to next tree
          tNode := VirtualStringTree1.GetNextSibling(tNode);
        end;
      end;

      if Assigned(xDocMain) then xDocMain.Free;
      xDocMain := xdoc;

      slMain.Clear;
      slMain.Assign(slTmp);
      slTmp.Free;

      VirtualStringTree1.clear;
      //add to the treeview.loop
      for i := 0 to -1 + slMain.Count do
      begin
       tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
       Data := VirtualStringTree1.GetNodeData(tNode);
       Data^.Column0 := i+1;
       Data^.Column1 := slMain[i];
       Data^.Column2 := i+1;
      end;

      currentFormat:= xspf;

      self.Caption:=' [XSPF] ';
      Statusbar1.Panels[0].Text:='XSPF';

      isBusy:=false;
      screen.Cursor:=crDefault;

      //can't do it anymore. so,
      actSaveiTunesTxt.Enabled:=false;

      actSaveM3u.Enabled:=false;
      actSaveM3uBOM.Enabled:=false;
      actSaveM3uAnsi.Enabled:=false;
      actSaveM3u8.Enabled:=false;
      actSaveXSPF.Enabled:=true;

      actConvertToM3U.Enabled:=true;
      actConvertToXSPF.Enabled:=false;
    end;

  end;
end;


// Edits

// Replace
procedure TfrmMain.actReplaceAllExecute(Sender: TObject);
begin
  MenuItemReplaceClick(nil);
end;

procedure TfrmMain.MenuItemReplaceClick(Sender: TObject);
begin
  if isBusy then exit;
  if slMain.Count =0 then exit;
  if not Assigned(frmOptions) then begin
    frmOptions := TfrmOptions.Create(self);
  end;

  frmOptions.Caption:='Edit';

  //todo make other invisible too.
  //frmOptions.TabSheetWelcome.Visible:=false;
  frmOptions.TabSheetWelcome.TabVisible:=false;

  //frmOptions.TabSheetReplace.Visible:=true;
  frmOptions.TabSheetReplace.TabVisible:=true;
  frmOptions.PageControl1.ActivePage:=frmOptions.TabSheetReplace;


  if (frmOptions.ShowModal = mrOK) then
  begin

    ReplaceAll(frmOptions.EditFind.text,frmOptions.EditReplaceWith.text);

  end;

end;

procedure TfrmMain.ReplaceAll(sFind:string;sReplace:string);
var
  slRow:TStringlist;
  line,i:integer;
  tNode: PVirtualNode;
  Data: PNodeData;
  xTrackListNode,xTrackNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
begin
  //Replace
  if (sFind = '') then exit;
  if isBusy then exit;
  screen.Cursor:=crHourGlass;

  VirtualStringTree1.Clear;
  Statusbar1.Panels[1].Text:='';
  StatusBar1.Visible:=true;

  isLocationEdited:=true;
  isDirty:=true;

  case currentFormat of
    iTunesTSV:begin
        isBusy:=true;
        ProgressBar1.Position := 0;
        ProgressBar1.Max := slMain.Count;
        ProgressBar1.Style:=pbstNormal;
        ProgressBar1.Visible:=true;

        slRow:=TStringlist.Create;
        slRow.StrictDelimiter := true;
        slRow.Delimiter := #$9; //TAB
        //slRow.QuoteChar := '"';
        slRow.LineBreak:=#13#10;


        //TODO remove any #9 char from sReplace

        //TOOD check slRow.count

        //for each tab delimited line
        for line := 1 to -1 + slMain.Count do
        begin
          slRow.DelimitedText := slMain[line];

          //replace
          slRow[30]:=trim(StringReplace(slRow[30],sFind,sReplace,[rfReplaceAll]));

          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := line;
          Data^.Column1 := slRow[30];
          Data^.Column2 := line;

          slMain[line]:= StringReplace(trim(slRow.text),#13#10,#$9,[rfReplaceAll]);

          ProgressBar1.Position := line;
          statusbar1.Panels[1].Text:='Processing...'+intToStr(line);
          Application.ProcessMessages;
        end;
        statusbar1.Panels[1].Text:='';
        isBusy:=false;
    end;
    m3u:begin
        isBusy:=true;
        ProgressBar1.Position := 0;
        ProgressBar1.Max := slMain.Count;
        ProgressBar1.Style:=pbstNormal;
        ProgressBar1.Visible:=true;

        line:=0;
        //for each tab delimited line
        for i := 0 to -1 + slMain.Count do
        begin

          if (not AnsiStartsStr('#', slMain.Strings[i])) and
             (not (slMain.Strings[i] = #13#10)) and
             (not (slMain.Strings[i] = '')) then
          begin

            slMain[i]:=StringReplace(slMain[i],sFind,sReplace,[rfReplaceAll]);

            line:=line+1;
            tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
            Data := VirtualStringTree1.GetNodeData(tNode);
            Data^.Column0 := line;
            Data^.Column1 := slMain[i];
            Data^.Column2 := line;

            ProgressBar1.Position := line;
            statusbar1.Panels[1].Text:='Processing...'+intToStr(line);
            Application.ProcessMessages;
          end else begin
            //#comment or empty line. just leave it as is.
          end;
        end;
        statusbar1.Panels[1].Text:='';
        isBusy:=false;
    end;
    xspf:begin
      isBusy:=true;

      slMain.Clear;
      ProgressBar1.Position := 0;

      if Assigned(xDocMain) then
           begin
             if Assigned(xDocMain.DocumentElement) then
             begin
               if (xDocMain.DocumentElement.NodeName = 'playlist') then
               begin

                 //
                 ProgressBar1.Style:=pbstMarquee;//start bar
                 progressBar1.Visible:=true;

                 xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
                 if Assigned(xTracklistNodelist) then
                 begin
                   if (xTracklistNodelist.Count>0) then
                   begin
                     xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
                     if Assigned(xTrackListNode) then
                     begin

                       xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
                       if Assigned(xTrackNodelist) then
                       begin
                         if (xTrackNodelist.Count>0) then
                         begin
                           xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                           line:=0;

                           while Assigned(xTrackNode) do
                           begin

                             xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                             if Assigned(xLocationNodelist) then
                             begin
                               if (xLocationNodelist.Count>0) then
                               begin

                                 line:=line+1;
                                 //slMain.Add(xLocationNodelist.Item[0].TextContent);
                                 xLocationNodelist.Item[0].TextContent :=StringReplace(xLocationNodelist.Item[0].TextContent,sFind,sReplace,[rfReplaceAll]);
                                 slMain.Add(xLocationNodelist.Item[0].TextContent);

                                 tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
                                 Data := VirtualStringTree1.GetNodeData(tNode);
                                 Data^.Column0 := line;
                                 Data^.Column1 := xLocationNodelist.Item[0].TextContent;
                                 Data^.Column2 := line;

                                 Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line) + ']' ;
                                 Application.ProcessMessages;

                               end;
                             end;
                             xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                           end;

                           Statusbar1.Panels[1].Text:='';
                         end;
                       end;
                     end;
                   end;
                 end;

               end;
             end;
           end;

      statusbar1.Panels[1].Text:='';
      isBusy:=false;
    end;
    new:begin
        isBusy:=true;
        ProgressBar1.Position := 0;
        ProgressBar1.Max := slMain.Count;
        ProgressBar1.Style:=pbstNormal;
        ProgressBar1.Visible:=true;

        //line:=0;
        //for each tab delimited line
        for i := 0 to -1 + slMain.Count do
        begin

          slMain[i]:=StringReplace(slMain[i],sFind,sReplace,[rfReplaceAll]);

          //line:=line+1;
          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := i;
          Data^.Column1 := slMain[i];
          Data^.Column2 := i;

          ProgressBar1.Position := i;
          statusbar1.Panels[1].Text:='Processing...'+intToStr(i);
          Application.ProcessMessages;

        end;
        statusbar1.Panels[1].Text:='';
        isBusy:=false;
    end;
  end;

  VirtualStringTree1.SetFocus;

  screen.Cursor:=crDefault;
  StatusBar1.Visible:=false;
  ProgressBar1.Visible:=false;
  ProgressBar1.Position:=0;
end;


// hide status bar with timer.

procedure TfrmMain.TimerHideStatusbarTimer(Sender: TObject);
begin
  if statusbar1.Visible then begin
    Statusbar1.Visible:=false;
  end;
  TimerHideStatusbar.Enabled:=false;
end;



// Treeview popup menu

// Treeview popup Copy
procedure TfrmMain.actCopySelectedPathExecute(Sender: TObject);
var
  Node: PVirtualNode;
  NodeArray: TVTVirtualNodeEnumeration;
  Data: PNodeData;
  cl:TStringList;
begin
  // don't. because "ctrl+A"
  {
  Node := VirtualStringTree1.FocusedNode;
  if Node = nil then exit;
  if VirtualStringTree1.SelectedCount < 1 then exit;
  }
  //Clipboard.AsText:=PNodeData(VirtualStringTree1.GetNodeData(Node))^.Column1;

  cl:=TStringList.Create;

  NodeArray:=VirtualStringTree1.SelectedNodes(false);
  // TVTVirtualNodeEnumerator
  //https://stackoverflow.com/questions/44144504/how-to-iterate-objects-in-parent-child-hierarchy-without-redundant-lists
  for Node in NodeArray do
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    cl.Add(Data^.Column1);
  end;

  Clipboard.AsText:=cl.Text;
  cl.Free;

end;

procedure TfrmMain.actTryOpenContainingFolderExecute(Sender: TObject);
begin
  // do we need this?
end;


procedure TfrmMain.actAddFilesToPlaylistExecute(Sender: TObject);
begin
  //TODO
  // add and select node
  // if xspf convert to uri

  //isLocationEdited:boolean;  //if xspf convert to uri

  isDirty:=true;
  isRearrangedOrder:=true;
end;

procedure TfrmMain.actRemoveFilesFromPlaylistExecute(Sender: TObject);
begin
  if VirtualStringTree1.SelectedCount < 1 then exit;

  VirtualStringTree1.DeleteSelectedNodes;

  isDirty:=true;
  isRearrangedOrder:=true;

end;




// Treeview properties
procedure TfrmMain.VirtualStringTree1BeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
//var
//  Data1: PNodeData;
begin
  if (not Odd(Node^.index)) then
  //Data1 := Sender.GetNodeData(Node);
  //if (Odd(Data1^.Column0)) then
  begin
    ItemColor := $EBEBEC;// $FFEEEE; ////$DEDEE0;//
    EraseAction := eaColor;
  end;

end;

procedure TfrmMain.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VirtualStringTree1.Refresh;
end;

procedure TfrmMain.VirtualStringTree1ColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin

end;

procedure TfrmMain.VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PNodeData;
begin

  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    0: result := Data1^.Column0 - Data2^.Column0;    //don't use it. now we use node.index
    1: Result := CompareText(Data1^.Column1, Data2^.Column1);
   end;
end;

procedure TfrmMain.VirtualStringTree1FocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VirtualStringTree1.Refresh;
end;

procedure TfrmMain.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
  var
    Data: PNodeData;
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    if Assigned(Data) then begin
      Data^.Column0 := 0;
      Data^.Column1 := '';
      Data^.Column2 := 0;
    end;
end;

procedure TfrmMain.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TfrmMain.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PNodeData;
begin
  Data := VirtualStringTree1.GetNodeData(Node);
  case Column of
    //0: CellText := intToStr(Data^.Column0) + '.';
    0: CellText := intToStr(Node^.Index+1) + '.';
    1: CellText := Data^.Column1;
    2: CellText := intToStr(Data^.Column2) //
  end;
end;

procedure TfrmMain.VirtualStringTree1HeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column = 1 then
  begin
    // if sorted, make
    isRearrangedOrder:=true;
  end;
end;





// Treeview drag and drops.
{
https://stackoverflow.com/questions/8487335/virtualtreeview-drag-and-drop-to-arrange-nodes-in-a-list
}
procedure TfrmMain.VirtualStringTree1DragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmMain.VirtualStringTree1DragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  pSource, pTarget: PVirtualNode;
  attMode: TVTNodeAttachMode;
  List: TList;
begin

  pTarget := Sender.DropTargetNode;

  case Mode of
    dmNowhere: attMode := amNoWhere;
    dmAbove: attMode := amInsertBefore;
    dmOnNode, dmBelow: attMode := amInsertAfter;
  end;

  {
  case Sender.GetNodeLevel(pTarget) of
    0:
      case Mode of
        dmNowhere:
          attMode := amNoWhere;
        else
          attMode :=  amAddChildLast;
      end;
    1:
      case Mode of
        dmNowhere:
          attMode := amNoWhere;
        dmAbove:
          attMode := amInsertBefore;
        dmOnNode, dmBelow:
          attMode := amInsertAfter;
      end;

  end;
  }
  List:= TList.create();
  pSource :=  Sender.GetFirstSelected();
  while Assigned(pSource) do
  begin
     List.Add(pSource);
     pSource := Sender.GetNextSelected(pSource);
  end;

  for pSource in List do
  begin
   Sender.MoveTo(pSource, pTarget, attMode, False);
   //showmessage(intToStr(pTarget^.Index))
   //pData := VirtualStringTree1.GetNodeData(pSource);
   //showmessage(pData^.Column1);
  end;

  if List.Count>0 then begin
    isRearrangedOrder:=true;
    isDirty:=true;

    actSaveiTunesTxt.Enabled:=false;
  end;

  List.Free;

  //

{
var
  pSource, pTarget: PVirtualNode;
  attMode: TVTNodeAttachMode;
begin
  pSource := TVirtualStringTree(Source).FocusedNode;
  pTarget := Sender.DropTargetNode;

  case Mode of
    dmNowhere: attMode := amNoWhere;
    dmAbove: attMode := amInsertBefore;
    dmOnNode, dmBelow: attMode := amInsertAfter;
  end;

  Sender.MoveTo(pSource, pTarget, attMode, False);
}
end;

procedure TfrmMain.VirtualStringTree1DragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin

  // Allow only from Treeview source
  //Accept := (Source = Sender);


  // allow from shell,
  // but hint text shows "Copy"
  // TODO
  // https://stackoverflow.com/questions/12993003/changing-drag-cursor-in-virtualtreeview
  Accept:=true;

end;

// TODO
// Treeview file drop from explorer
{
https://stackoverflow.com/questions/3770109/how-do-you-drag-and-drop-a-file-from-explorer-shell-into-a-virtualtreeview-contr
}





end.

