UNIT view_c_declarations_unit;

INTERFACE

uses
   Windows, Classes,
   Controls, Forms, StdCtrls, ComCtrls,
   TabNotBk, combo_type_dialog_unit;

type
   TViewCDeclarationsForm =
      class(TForm)
         TabbedNotebook: TTabbedNotebook;
         procedure FormActivate(Sender: TObject);
         procedure FormClose(Sender: TObject; var Action: TCloseAction);
      public
         PICName: string;
         Variable: p_var_sfr_list;
      end;

IMPLEMENTATION

uses
   Graphics, SysUtils;

{$R *.dfm}

procedure TViewCDeclarationsForm.FormActivate(Sender: TObject);
   var
      f: TextFile;
      s: string;
      tab_idx: integer;
      Memos: array of TMemo;
   begin
      Caption := PICName + ': C Declarations for ' + Variable^.var_name + ' SFRs';

      SetLength(Memos, Length(Variable^.sfr_names));
      TabbedNotebook.Pages.Clear;
      for tab_idx := 0 to Length(Variable^.sfr_names)-1 do
         begin
            TabbedNotebook.Pages.Add(Variable^.sfr_names[tab_idx]);
            Memos[tab_idx] := TMemo.Create(Self);
            with Memos[tab_idx] do
               begin
                  Parent := TTabSheet(TabbedNotebook.Pages.Objects[tab_idx]);
                  Align := alClient;
                  Scrollbars := ssVertical;
                  Font.Name := 'System';
                  Font.Pitch := fpFixed;
                  ReadOnly := true
               end;
         end;

      // For some reason this doesn't work with only one tab.  Add an extra tab...
      if Length(Variable^.sfr_names) = 1 then
         TabbedNotebook.Pages.Add ('');

      AssignFile (f, 'C:\Program Files (x86)\Microchip\xc8\v1.33\include\' + PICName + '.h');
      Reset (f);

      tab_idx := 0;
      while not Eof(f) do
         begin
            Readln (f, s);
            s := Trim(s);
            if (tab_idx < Length(Variable^.sfr_names))
               and
               ('// Register: ' + TabbedNotebook.Pages[tab_idx] = s)
            then
               begin
                  repeat
                     Readln (f, s);
                     if (s <> '')
                        and
                        (s[1] <> '#')
                        and
                        (Pos ('asm', s) <> 1)
                        and
                        (Pos ('// ', s) <> 1)
                     then
                        Memos[tab_idx].Lines.Add (s)
                  until s = '';

                  tab_idx := tab_idx + 1
               end
         end;
      TabbedNotebook.PageIndex := 0;
      CloseFile (f);
   end;

procedure TViewCDeclarationsForm.FormClose(Sender: TObject; var Action: TCloseAction);
   begin
      Action := caFree
   end;

END.
