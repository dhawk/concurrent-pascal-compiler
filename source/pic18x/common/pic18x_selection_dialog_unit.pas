UNIT pic18x_selection_dialog_unit;

{$IFDEF FPC}
   {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
{$IFDEF FPC}
   LCLIntf, LCLType, LMessages,
{$ELSE}
   Windows,
{$ENDIF}
   Controls,
   Forms,
   StdCtrls,
   ExtCtrls,
   dijkstra_smoothsort_unit,
   Classes;

type
   TStringClass =
      class
         s: string;
         constructor Create (_s: string);
      end;
   TPic18xSelectionDialog =
      class(TForm)
         OkButton: TButton;
         CancelButton: TButton;
         Bevel1: TBevel;
         ListBox1: TListBox;
         ResetButton: TButton;
         Label1: TLabel;
         procedure InitializeSelection(Sender: TObject);
         procedure ListBox1Click(Sender: TObject);
         procedure FormDeactivate(Sender: TObject);
      private
         include_dir: string;
         ext: string;
         possible_pics: TSmoothSortArray;
         function get_selectedPIC: string;
      public
         property SelectedPIC: string read get_selectedPIC;
         function Execute (_include_dir, _ext: string): boolean;   // ext includes .
      end;

var
   Pic18xSelectionDialog: TPic18xSelectionDialog;

function ComparePICNames (L, R: string): integer;

IMPLEMENTATION

uses
   regular_expression_unit, SysUtils, win32_utils;

{$R *.dfm}

const
   dot_dot_dot = '...';

constructor TStringClass.Create (_s: string);
   begin
      s := _s
   end;

function CompareStringClass (v1,v2: TObject): boolean;
   //   result := v1 <= v2
   begin
      result := ComparePICNames (TStringClass(v1).s, TStringClass(v2).s) < 1
   end;

procedure TPic18xSelectionDialog.InitializeSelection(Sender: TObject);
   var
      sr: TSearchRec;
      regex: t_regular_expression;
      i: integer;
      current_prefix: string;
      b: boolean;
   begin
      OkButton.Enabled := false;
      Label1.Visible := false;
      ListBox1.Visible := true;
      ListBox1.Clear;
      SetLength(possible_pics, 0);
      if FindFirst (include_dir + '*' + ext, faAnyFile, sr) = 0
      then
         begin
            repeat
               if (sr.attr and faDirectory) <> faDirectory then
                  begin
                     i := Length(possible_pics);
                     SetLength(possible_pics, i+1);
                     possible_pics[i] := TStringClass.Create(Copy (sr.Name, 1, Length(sr.Name)-Length(ext)))
                  end
            until FindNext (sr) <> 0;
            FindClose (sr)
         end;

      SmoothSort (possible_pics, CompareStringClass);

      regex := t_regular_expression.Create ('^PIC18([A-Z]*).*$');
      current_prefix := '';
      for i := 0 to Length(possible_pics)-1 do
         if (TStringClass(possible_pics[i]).s = 'MCV20USB')
            or
            (Pos ('PS', TStringClass(possible_pics[i]).s) = 1)
         then   // handle all special case PIC names
            ListBox1.AddItem(TStringClass(possible_pics[i]).s, nil)
         else   // handle all PIC18... names
            begin
               b := regex.Matches(TStringClass(possible_pics[i]).s);
               assert (b, 'found new special case PIC name: ' + TStringClass(possible_pics[i]).s);
               if regex.Match(1) <> current_prefix then
                  begin
                     ListBox1.AddItem('PIC18' + regex.Match(1) + dot_dot_dot, nil);
                     current_prefix := regex.Match(1)
                  end
            end;
      regex.Free
   end;

function PartialPICNameCompare (List: TStringList; Index1, Index2: Integer): Integer;
   begin
      result := ComparePICNames (List[Index1], List[Index2])
   end;

procedure TPic18xSelectionDialog.ListBox1Click(Sender: TObject);
   var
      selection: string;
      i, j: integer;
      current, s: string;
      display_list: TStringList;
      found: boolean;
   begin
      if ListBox1.ItemIndex > -1 then
         if Pos (dot_dot_dot, Listbox1.Items[ListBox1.ItemIndex]) > 0 then
            begin
               selection := Copy (Listbox1.Items[ListBox1.ItemIndex], 1, Length(Listbox1.Items[ListBox1.ItemIndex])-Length(dot_dot_dot));

               // remove all pics that don't match selection
               i := 0;
               while i < Length(possible_pics) do
                   if Pos(selection, TStringClass(possible_pics[i]).s) = 1 then
                      i := i + 1
                   else
                      begin
                         possible_pics[i].Free;
                         for j := i to Length(possible_pics)-2 do
                            possible_pics[j] := possible_pics[j+1];
                         SetLength(possible_pics, Length(possible_pics)-1)
                      end;

               Listbox1.Clear;
               if Length(possible_pics) > 25 then    // number of lines in listbox without scrollbar appearing
                  begin
                     display_list := TStringList.Create;
                     current := '';
                     for i := 0 to Length(possible_pics)-1 do
                        if TStringClass(possible_pics[i]).s[Length(selection)+1] <> current then
                           begin
                              current := TStringClass(possible_pics[i]).s[Length(selection)+1];
                              if (selection + current = TStringClass(possible_pics[i]).s)
                                 or
                                 (i = Length(possible_pics)-1)
                                 or
                                 (current <> TStringClass(possible_pics[i+1]).s[Length(selection)+1])
                              then
                                 display_list.Add(TStringClass(possible_pics[i]).s)
                              else
                                 begin
                                    s := selection + current + '...';
                                    found := false;
                                    for j := 0 to display_list.Count-1 do
                                       if display_list[j] = s then
                                          found := true;
                                    if not found then
                                       display_list.Add (s)
                                 end
                           end;
                     display_list.CustomSort (PartialPICNameCompare);
                     ListBox1.Items.AddStrings (display_list);
                     display_list.Free
                  end
               else
                  for i := 0 to Length(possible_pics)-1 do
                     ListBox1.AddItem (TStringClass(possible_pics[i]).s, nil)
            end
         else   // final selection
            begin
               listbox1.Visible := false;
               label1.Caption := Listbox1.Items[ListBox1.ItemIndex];
               label1.Visible := true;
               OkButton.Enabled := true
            end
   end;

function TPic18xSelectionDialog.get_selectedPIC: string;
   begin
      result:= Label1.Caption
   end;

procedure TPic18xSelectionDialog.FormDeactivate(Sender: TObject);
   var i: integer;
   begin
      inherited;
      for i := 0 to Length(possible_pics)-1 do
         possible_pics[i].Free;
      possible_pics := nil
   end;

function TPic18xSelectionDialog.Execute (_include_dir, _ext: string): boolean;
   begin
      include_dir := _include_dir;
      ext := _ext;
      result := ShowModal = mrOk
   end;


//==================
//  ComparePICNames

var
   Lregex, Rregex: t_regular_expression;

function StrToInt (s: string): integer;
   var i: integer;
   begin
      result := 0;
      for i := 1 to Length(s) do
         begin
            assert (CharInSet (s[i], ['0'..'9']));
            result := (result*10) + ord(s[i]) - ord('0')
         end
   end;

function ComparePICNames (L, R: string): integer;
   var
      sL24, sR24: string;
   begin
      if Lregex.Matches(L) and Rregex.Matches(R) then
         begin
            result := CompareText (Lregex.Match(1), Rregex.Match(1));
            if result <> 0 then
               exit;

            // want PIC18F2510 < PIC18F25K10 < PIC18F2520

            sL24 := Lregex.Match(2) + Lregex.Match(4);
            sR24 := Rregex.Match(2) + Rregex.Match(4);
            if StrToInt(sL24) < StrToInt(sR24) then
               result := -1
            else if StrToInt(sL24) > StrToInt(sR24) then
               result := 1;
            if result <> 0 then
               exit;

            result := CompareText (Lregex.Match(3), Rregex.Match(3));
            if result <> 0 then
               exit;

            result := CompareText (Lregex.Match(5), Rregex.Match(5))
         end
      else
         result := CompareText (L, R)
   end;

INITIALIZATION
   Lregex := t_regular_expression.Create ('^PIC18([A-Z]+)([0-9][0-9])([A-Z]*)([0-9]+)(.*)$');
   Rregex := t_regular_expression.Create ('^PIC18([A-Z]+)([0-9][0-9])([A-Z]*)([0-9]+)(.*)$');
   //  groups                                    (...1..)(....2.....)(...3..)(...4..)(.5)

FINALIZATION
   Lregex.Free;
   Rregex.Free

END.
