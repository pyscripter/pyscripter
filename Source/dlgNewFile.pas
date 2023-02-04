unit dlgNewFile;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees,
  cFileTemplates,
  dlgPyIDEBase,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls, VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL;

type
  TNewFileDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tvCategories: TVirtualStringTree;
    Label1: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    btnCancel: TButton;
    btnCreate: TButton;
    btnManageTemplates: TButton;
    Splitter1: TSplitter;
    lvTemplates: TListview;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvCategoriesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure tvCategoriesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnManageTemplatesClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure lvTemplatesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvTemplatesDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Categories : TStringList;
    SelectedTemplate : TFileTemplate;
    procedure SetUp;
  end;

implementation

uses
  Winapi.ShellAPI, dmCommands, MPCommonObjects;

{$R *.dfm}

procedure TNewFileDialog.btnCreateClick(Sender: TObject);
begin
  if Assigned(lvTemplates.Selected) then begin
    SelectedTemplate := TFileTemplate(lvTemplates.Selected.Data);
    ModalResult := mrOK;
  end;
end;

procedure TNewFileDialog.btnManageTemplatesClick(Sender: TObject);
begin
  CommandsDataModule.actFileTemplatesExecute(Self);
  SetUp;
end;

procedure TNewFileDialog.FormCreate(Sender: TObject);
begin
  inherited;
  Categories := TStringList.Create;
  Categories.CaseSensitive := False;
  lvTemplates.LargeImages := LargeSysImages;
end;

procedure TNewFileDialog.FormDestroy(Sender: TObject);
begin
  Categories.Free;
end;

procedure TNewFileDialog.FormShow(Sender: TObject);
begin
  SetUp;
end;

procedure TNewFileDialog.lvTemplatesDblClick(Sender: TObject);
begin
  btnCreateClick(Self);
end;

procedure TNewFileDialog.lvTemplatesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnCreate.Enabled := Assigned(Item) and Selected;
end;

procedure TNewFileDialog.SetUp;
var
  i : integer;
begin
  Categories.Clear;
  tvCategories.Clear;
  lvTemplates.Items.Clear;
  for i  := 0 to FileTemplates.Count - 1 do
    if Categories.IndexOf(TFileTemplate(FileTemplates[i]).Category) < 0 then
      Categories.Add(TFileTemplate(FileTemplates[i]).Category);
  tvCategories.RootNodeCount := Categories.Count;
  if Categories.Count > 0 then
    tvCategories.Selected[tvCategories.RootNode.FirstChild] := True;
end;

procedure TNewFileDialog.tvCategoriesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
Var
  i, Index : integer;
  FileTemplate : TFileTemplate;
  FName : string;
  FileInfo: TSHFileInfo;
begin
  if Assigned(Node) and (vsSelected in Node.States) then begin
    lvTemplates.Items.Clear;
    Index := Node.Index;
    for i := 0 to FileTemplates.Count - 1 do begin
      FileTemplate := FileTemplates[i] as TFileTemplate;
      if CompareText(Categories[Index], FileTemplate.Category) = 0 then begin
        with lvTemplates.Items.Add do begin
          Caption := FileTemplate.Name;
          Data := FileTemplate;
          FName := '.' + FileTemplate.Extension;
          if SHGetFileInfo(PChar(FName),
                                   FILE_ATTRIBUTE_NORMAL,
                                   FileInfo,
                                   SizeOf( FileInfo),
                                   SHGFI_USEFILEATTRIBUTES or
                                   SHGFI_LARGEICON or
                                   SHGFI_ICON or
                                   SHGFI_SYSICONINDEX) > 0
          then
            ImageIndex := FileInfo.iIcon
          else
            ImageIndex := 0;
        end;
      end;
    end;
  end;
end;

procedure TNewFileDialog.tvCategoriesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if TextType = ttNormal then
    CellText := Categories[Node.Index]
end;

end.
