UNIT cpc_source_analysis_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes,
   cpc_multi_precision_integer_unit,
   SysUtils;

type
   TSourceLocation =
      record
      private
         source_idx: integer; // index into source array where token was found (0..Length(source)-1)
      public
         line_idx: integer; // position in line of token (1..Length(line))
         length: integer; // length of token
         function same_location (loc: TSourceLocation): boolean;
         function beyond (loc: TSourceLocation): boolean;
         function in_preamble: boolean;
         function line_no: integer;
         function line: string;
         function caret: string;
         function file_name: string;
         function NullSourceLocation: boolean;
      end;
   EFileDoesntExist =
      class (Exception)
      end;
   ECantOpenFile =
      class (Exception)
      end;

const
   NonExistantSourceLocation: TSourceLocation = (source_idx: -1; line_idx: 0; length: 0);
   FileErrorSourceLocation:   TsourceLocation = (source_idx: -2; line_idx: 0; length: 0);

type
   TIdentifierIdx = integer;

type
   TSymbolEnum =
      (sym_plus,
       sym_minus,
       sym_star,
       sym_equals,
       sym_not_equals,
       sym_assign,
       sym_dot,
       sym_dot_dot,
       sym_greater_than,
       sym_less_than,
       sym_greater_than_or_equal,
       sym_less_than_or_equal,
       sym_colon,
       sym_semicolon,
       sym_left_parenthesis,
       sym_right_parenthesis,
       sym_left_bracket,
       sym_right_bracket,
       sym_slash,
       sym_comma
      );
   TSymbolEnumSet = set of TSymbolEnum;

type
   TReservedWordEnum =
      (// reserved words used as variable descriptor
       rw_const,   // implict for parameters without other descriptor
       rw_eeprom,
       rw_for,     // variable is in use for controlling a for loop (used only temporarily while parsing for loop statement)
       rw_ioreg,
       rw_rom,
       rw_var,

       // other reserved words
       rw_abs,
       rw_and,
       rw_array,
       rw_assert,
       rw_at,
       rw_await,
       rw_begin,
       rw_case,
       rw_chr,
       rw_class,
       rw_continue,
       rw_cycle,
       rw_delay,
       rw_div,
       rw_do,
       rw_downto,
       rw_else,
       rw_empty,
       rw_end,
       rw_exitloop,
       rw_false,
       rw_function,
       rw_get,
       rw_high,
       rw_if,
       rw_in,
       rw_init,
       rw_interrupt,
       rw_packed,
       rw_loop,
       rw_low,
       rw_maxstrlen,
       rw_mod,
       rw_monitor,
       rw_not,
       rw_of,
       rw_or,
       rw_ord,
       rw_otherwise,
       rw_overlay,
       rw_pred,
       rw_priority,
       rw_procedure,
       rw_process,
       rw_property,
       rw_public,
       rw_record,
       rw_recycle,
       rw_reloop,
       rw_repeat,
       rw_round,
       rw_set,
       rw_strappend,
       rw_string,
       rw_strlen,
       rw_strpos,
       rw_succ,
       rw_then,
       rw_to,
       rw_true,
       rw_trunc,
       rw_type,
       rw_until,
       rw_while,
       rw_with
      );
   TReservedWordEnumSet = set of TReservedWordEnum;
   TVariableDescriptor = rw_const..rw_var;

type
   TTokenKind =
      (symbol_token,
       reserved_word_token,
       identifier_token,
       integer_constant_token,
       real_constant_token,
       string_constant_token,
       eof_token
      );

   TTokenType =
      record
         src_loc: TSourceLocation;
         s: string;  // valid only if token_kind is string_constant_token
         function in_preamble: boolean;
         case token_kind: TTokenKind of
            symbol_token:
               (symbol: TSymbolEnum
               );
            reserved_word_token:
               (rw: TReservedWordEnum
               );
            identifier_token:
               (identifier_idx: TIdentifierIdx
               );
            integer_constant_token:
               (i: TMultiPrecisionInteger
               );
            real_constant_token:
               (r: real
               );
      end;

type
   TLexicalAnalysis =
      class
      private
         tokens: array of TTokenType;
         function define_identifier
            (identifier: string
            ): TIdentifierIdx;
      public
         token_idx: integer;
         identifiers: array of string;  // indexed by TIdentifierIdx

         supported_data_type_id_idxs: array of TIdentifierIdx;
         constructor Create;

         procedure LoadCPUBasicDataTypeIdentifiers;
         procedure ReadInPreamble (preamble: TStringList);
         procedure ReadInSourceFile (source_file_name: string);
         procedure ReadInTestCase (line: string);
            overload;
         procedure ReadInTestCase (lines: TStrings);
            overload;
         procedure DoLexicalAnalysis;

         function NumberOfIdentifiers: integer;
         function token: TTokenType;
         function next_token: TTokenType;
         function previous_token_src_loc: TSourceLocation;
         procedure advance_token;
         function mark_token_position: integer;
         procedure backup (mark: integer);
         function token_is_symbol
            (op: TSymbolEnum
            ): boolean;
            overload;
         function token_is_symbol
            (op_set: TSymbolEnumSet
            ): boolean;
            overload;
         function token_is_reserved_word
            (rw: TReservedWordEnum
            ): boolean;
            overload;
         function token_is_reserved_word
            (rw_set: TReservedWordEnumSet
            ): boolean;
            overload;
         function token_is_identifier: boolean;
         function token_is_constant: boolean;
         function token_is_eof: boolean;

         function token_string (src_loc: TSourceLocation): string;
            overload;
         function token_string (first_src_loc, last_src_loc: TSourceLocation): string;
            overload;

         destructor Destroy;
            override;
      end;

var
   lex: TLexicalAnalysis;
   current_dir: string;

function non_printable_char (c: char): boolean;
function reserved_word_to_string (rw: TReservedWordEnum): string;
function is_reserved_word (s: string): boolean;

procedure output_source_line (st: TStrings; addr, s: string);
procedure append_source_up_to (st: TStrings; src_loc: TSourceLocation);
procedure append_remaining_source (st: TStrings);
function caret (idx: integer): string;
function extract_quoted_compiler_directive_parameter (compiler_directive, simplified_line, param_name: string; src_location: TSourceLocation): string;
procedure read_in_file (full_path_fn: string; compiler_directives_allowed: boolean; src_location: TSourceLocation);
procedure add_line_to_source (line: string; file_list_idx, line_no: integer; var in_preamble: boolean);
function symbol_id (symbol: string): integer;

IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_core_objects_unit,
   cpc_target_cpu_unit,
   wirth_balanced_binary_tree_unit;

function is_reserved_word (s: string): boolean;
   var
      rw: TReservedWordEnum;
   begin
      result := true;
      s := LowerCase(Trim(s));
      for rw := Low(TReservedWordEnum) to High(TReservedWordEnum) do
         if s = reserved_word_to_string(rw) then
            exit;
      result := false
   end;

type
   TSymbolInfo =
      record
         symbol: string;
         case token_kind: TTokenKind of
            reserved_word_token:
               (rw: TReservedWordEnum
               );
            identifier_token:
               (id: TIdentifierIdx
               )
      end;

   TSymbolTableEntry =
      class (TBalancedTreeEntry)
         info: TSymbolInfo;
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy
            (ToA: TBalancedTreeEntry
            ); // data
            override;
      end;

   TSymbolTable =
      class (TBalancedBinaryTree)
         function find_symbol_table_entry
            (symbol: string
            ): TSymbolInfo;
         procedure add_reserved_word
            (word: String;
             reserved_word: TReservedWordEnum
            );
         procedure add_identifier
            (word: String;
             id: integer
            );
      end;

var
   symbol_table: TSymbolTable;
   first_non_preamble_source_idx: integer;

function symbol_id (symbol: string): integer;
   var
      symbol_item: TSymbolInfo;
   begin
      symbol_item := symbol_table.find_symbol_table_entry(symbol);
      assert (symbol_item.token_kind = identifier_token);
      result := symbol_item.id
   end;

procedure TSymbolTable.add_reserved_word
   (word: String;
    reserved_word: TReservedWordEnum
   );
   var
      entry: TSymbolTableEntry;
   begin
      entry := TSymbolTableEntry.create;
      entry.info.symbol := string(LowerCase(word));
      entry.info.token_kind := reserved_word_token;
      entry.info.rw := reserved_word;
      symbol_table.add(entry)
   end;

procedure TSymbolTable.add_identifier
   (word: String;
    id: integer
   );
   var
      entry: TSymbolTableEntry;
   begin
      entry := TSymbolTableEntry.create;
      entry.info.symbol := string(LowerCase(word));
      entry.info.token_kind := identifier_token;
      entry.info.id := id;
      symbol_table.add(entry)
   end;

function TSymbolTableEntry.compare
   (a: TBalancedTreeEntry
   ): Shortint;   // a < self :-1  a=self :0  a > self :+1
   begin
      if TSymbolTableEntry(a).info.symbol < self.info.symbol then
         result := -1
      else if TSymbolTableEntry(a).info.symbol > self.info.symbol then
         result := +1
      else
         result := 0
   end;

procedure TSymbolTableEntry.copy
   (ToA: TBalancedTreeEntry
   );
   begin
      TSymbolTableEntry(ToA).info := info
   end;

function TSymbolTable.find_symbol_table_entry
   (symbol: string
   ): TSymbolInfo;
   function find
      (item: TBalancedTreeEntry
      ): TBalancedTreeEntry;
      begin
         if item = nil then
            result := nil
         else if TSymbolTableEntry(item).info.symbol = symbol then
            result := item
         else if TSymbolTableEntry(item).info.symbol < symbol then
            result := find(item.greater_values)
         else
            result := find(item.lesser_values)
      end;
   var
      p: TBalancedTreeEntry;
   begin
      symbol := LowerCase (symbol);
      p := find(symbol_table.root);
      if p = nil then
         result.token_kind := eof_token
      else
         result := TSymbolTableEntry(p).info;
   end;

const
   tab = chr(9);
   white_space = [' ', tab];

function non_printable_char
   (c: char
   ): boolean;
   begin
      result := not (ord(c) in [32..126]) // ' ' .. '~'
   end;

{$ifdef FPC}
type TSetOfChar = set of char;

function CharInSet (c: char; s: TSetOfChar): boolean;
   begin
      result := c in s
   end;
{$endif}

function caret (idx: integer): string;
   var
      i: integer;
   begin
      result := '';
      for i := 1 to idx - 1 do
         result := result + ' ';
      result := result + '^'
   end;


//==============
//  Source File
//==============

var
   file_list:
      array of string;
   source:
      array of
         record
            file_list_idx: integer;
            line_no: integer;
            line: string
         end;
   current_source_idx: integer;

function TSourceLocation.same_location (loc: TSourceLocation): boolean;
   begin
      result :=
         (source_idx = loc.source_idx)
         and
         (line_idx = loc.line_idx)
         and
         (length = loc.length)
   end;

function TSourceLocation.beyond (loc: TSourceLocation): boolean;
   begin
      if source_idx < loc.source_idx then
         result := false
      else if source_idx > loc.source_idx then
         result := true
      else
         result := line_idx > loc.line_idx
   end;

function TSourceLocation.in_preamble: boolean;
   begin
      result := source_idx < first_non_preamble_source_idx
   end;

function TSourceLocation.line_no: integer;
   begin
      if source_idx = -1 then
         result := 0
      else
         result := source[source_idx].line_no
   end;

function TSourceLocation.line: string;
   begin
      result := source[source_idx].line
   end;

function TSourceLocation.caret: string;
   begin
      result := cpc_source_analysis_unit.caret(line_idx)
   end;

function TSourceLocation.file_name: string;
   begin
      if source_idx = -1 then
         result := '*internal*'
      else if source[source_idx].file_list_idx = -1 then
         result := '*preamble*'
      else
         result := ExtractFileName(file_list[source[source_idx].file_list_idx])
   end;

function TSourceLocation.NullSourceLocation: boolean;
   begin
      result := (source_idx = 0)
                and
                (line_idx = 0)
                and
                (length = 0)
   end;

procedure output_source_line (st: TStrings; addr, s: string);
   begin
      st.Add ('                                    ; ' + s)
   end;

procedure append_source_up_to (st: TStrings; src_loc: TSourceLocation);
   begin
      if current_source_idx < src_loc.source_idx then
         begin
            if st[st.Count-1] <> '' then
               st.Add ('');
            repeat
               current_source_idx := current_source_idx + 1;
               if source[current_source_idx].file_list_idx <> -1 then
                  output_source_line (st, '', source[current_source_idx].line);
            until current_source_idx = src_loc.source_idx
         end
   end;

procedure append_remaining_source (st: TStrings);
   begin
      while current_source_idx < Length(source)-1 do
         begin
            current_source_idx := current_source_idx + 1;
            if source[current_source_idx].file_list_idx <> -1 then
               output_source_line (st, '', source[current_source_idx].line);
         end;
   end;

const
   include_directive = '{$include ';

function is_include_directive (line: string): boolean;
   begin
      result := Pos(include_directive, LowerCase(line)) = 1
   end;

function is_compiler_directive (line: string): boolean;
   begin
      result := Pos('{$', LowerCase(line)) = 1
   end;

function extract_quoted_compiler_directive_parameter (compiler_directive, simplified_line, param_name: string; src_location: TSourceLocation): string;
   var
      i: integer;
   begin
      // find beginning of param name
      i := Length (compiler_directive) + 1;
      while (i <= length(simplified_line)) and CharInSet(simplified_line[i], white_space) do
         i := i + 1;
      if (i > length(simplified_line))
         or
         (simplified_line[i] <> '''')
      then
         begin
            src_location.line_idx := i;
            raise compile_error.Create(format (err_openning_quote_required_for_s_name, [param_name]), src_location)
         end;
      src_location.line_idx := i;   // location of quoted param name
      i := i + 1;

      if (i > Length(simplified_line))
         or
         (simplified_line[Length(simplified_line)] <> '''') then
         begin
            src_location.line_idx := Length(simplified_line) + 1;
            raise compile_error.Create(format (err_closing_quote_required_for_s_name, [param_name]), src_location)
         end;
      // remove final '
      SetLength (simplified_line, Length(simplified_line)-1);
      result := Trim(Copy (simplified_line, i, 9999));
   end;

procedure handle_compiler_directive (source_idx: integer);
   var
      line: string;
      src_location: TSourceLocation;
   var
      full_path_fn: string;
   begin
      line := Trim (source[source_idx].line);
      src_location.source_idx := source_idx;
      src_location.line_idx := 1;
      src_location.length := 1;

      if line[Length(line)] <> '}' then
         begin
            src_location.line_idx := Length(line) + 1;
            raise compile_error.Create(err_closing_curly_bracket_required_for_compiler_directive, src_location)
         end;
      // remove final }
      SetLength (line, Length(line)-1);
      line := Trim (Line);

      if is_include_directive(line) then
         try  // see if it a relative file path to current directory
            full_path_fn := ExpandFileName (current_dir + extract_quoted_compiler_directive_parameter (include_directive, line, 'file', src_location));  // will convert relative path to absolute path
            read_in_file(full_path_fn, false, src_location)
         except
            on EFileDoesntExist do
               try  // see if it is an absolute path file name
                  full_path_fn := extract_quoted_compiler_directive_parameter (include_directive, line, 'file', src_location);
                  read_in_file(full_path_fn, false, src_location)
               except
                  on EFileDoesntExist do
                     raise compile_error.Create(format (err_include_file_doesnt_exist, [full_path_fn]), FileErrorSourceLocation)
               end;
            on ECantOpenFile do
               raise compile_error.Create(format (err_cant_open_include_file, [full_path_fn]), FileErrorSourceLocation);
         end
      else
         if not target_cpu.process_compiler_directive (line, src_location)
         then
            begin
               src_location.line_idx := 3;
               raise compile_error.Create ('unknown compiler directive', src_location)
            end
   end;

procedure add_line_to_source (line: string; file_list_idx, line_no: integer; var in_preamble: boolean);
   var
      source_idx: integer;
      src_location: TSourceLocation;
   begin
      source_idx := length(source);
      SetLength(source, source_idx + 1);
      source[source_idx].file_list_idx := file_list_idx;
      source[source_idx].line := line;
      source[source_idx].line_no := line_no;

      src_location.source_idx := source_idx;
      src_location.line_idx := 1;
      src_location.length := 1;

      // remove comments
      if pos ('//',line) > 0 then
         line := copy (line, 1, pos('//',line)-1);

      if in_preamble
      then
         begin  // only possible in main file
            if is_include_directive(line) then
               begin  // first include in main source file marks end of preamble
                  in_preamble := false;
                  first_non_preamble_source_idx := source_idx;
                  handle_compiler_directive (source_idx)
               end
            else if is_compiler_directive(line) then
               begin
                  handle_compiler_directive (source_idx);
                  first_non_preamble_source_idx := source_idx
               end
            else if Trim(line) <> '' then
               begin  // first non-blank line in main source file marks end of preamble
                  in_preamble := false;
                  first_non_preamble_source_idx := source_idx
               end
         end
      else  // not in preamble
         if is_include_directive(line) then
            begin
               src_location.source_idx := source_idx;
               handle_compiler_directive (source_idx)
            end
         else if is_compiler_directive(line) then
            raise compile_error.Create (err_compiler_directives_must_be_at_beginning_of_source, src_location)
   end;

procedure read_in_file (full_path_fn: string; compiler_directives_allowed: boolean; src_location: TSourceLocation);
   var
      f: text;
      line: string;
      line_no, file_list_idx, i: integer;
      in_preamble: boolean;
      old_current_dir: string;
   procedure mark_file_insertion (s: string);
      var i: integer;
      begin
         line := '//';
         for i := 1 to (80 - Length(s)) div 2 do
         line := line + '=';
         line := line + s;
         for i := 1 to (80 - Length(s)) div 2 do
         line := line + '=';
         line_no := line_no + 1;
         add_line_to_source ('', file_list_idx-1, line_no, in_preamble);
         line_no := line_no + 1;
         add_line_to_source (line, file_list_idx-1, line_no, in_preamble);
         line_no := line_no + 1;
         add_line_to_source ('', file_list_idx-1, line_no, in_preamble)
      end;
   begin
      old_current_dir := current_dir;
      current_dir := ExtractFilePath (full_path_fn);

      for i := 0 to Length(file_list)-1
      do if full_path_fn = file_list[i] then
         raise compile_error.Create (err_recursive_include_file, src_location);

      in_preamble := compiler_directives_allowed;

      file_list_idx := length(file_list);
      SetLength(file_list, file_list_idx + 1);
      file_list[file_list_idx] := full_path_fn;
      if not FileExists(full_path_fn) then
         raise EFileDoesntExist.Create('');
      {$I-}
      AssignFile(f, full_path_fn);
      Reset(f);
      {$I+}
      if IoResult <> 0 then
         raise ECantOpenFile.Create('');

      mark_file_insertion (' START FILE INSERTION ' + ExtractFileName(full_path_fn) + ' ');

      line_no := 1;
      while not eof(f) do
         begin
            Readln(f, line);
            line_no := line_no + 1;
            add_line_to_source (line, file_list_idx, line_no, in_preamble)
         end;

      mark_file_insertion (' END FILE INSERTION ' + ExtractFileName(full_path_fn) + ' ');

      CloseFile(f);
      current_dir := old_current_dir
   end;

function source_token
   (loc: TSourceLocation
   ): string;
   begin
      result := Copy (source[loc.source_idx].line, loc.line_idx, loc.length)
   end;

procedure read_in_source_for_test_case
   (s: string
   );
   overload;
   var
      in_preamble: boolean;
   begin
      SetLength (file_list, 1);
      file_list[0] := '*test*';
      in_preamble := true;
      add_line_to_source (s, 0, 0, in_preamble)
   end;

procedure read_in_source_for_test_case
   (lines: TStrings
   );
   overload;
   var
      i: integer;
      in_preamble: boolean;
   begin
      SetLength (file_list, 1);
      file_list[0] := '*test*';

      in_preamble := true;
      for i := 0 to lines.Count-1 do
         add_line_to_source (lines[i], 0, i+1, in_preamble)
   end;

function reserved_word_to_string
   (rw: TReservedWordEnum
   ): string;
   begin
      case rw of
         rw_abs:
            result := 'abs';
         rw_and:
            result := 'and';
         rw_array:
            result := 'array';
         rw_assert:
            result := 'assert';
         rw_at:
            result := 'at';
         rw_await:
            result := 'await';
         rw_begin:
            result := 'begin';
         rw_case:
            result := 'case';
         rw_chr:
            result := 'chr';
         rw_class:
            result := 'class';
         rw_const:
            result := 'const';
         rw_continue:
            result := 'continue';
         rw_cycle:
            result := 'cycle';
         rw_delay:
            result := 'delay';
         rw_div:
            result := 'div';
         rw_do:
            result := 'do';
         rw_downto:
            result := 'downto';
         rw_eeprom:
            result := 'eeprom';
         rw_else:
            result := 'else';
         rw_empty:
            result := 'empty';
         rw_end:
            result := 'end';
         rw_exitloop:
            result := 'exitloop';
         rw_false:
            result := 'false';
         rw_for:
            result := 'for';
         rw_function:
            result := 'function';
         rw_get:
            result := 'get';
         rw_high:
            result := 'high';
         rw_if:
            result := 'if';
         rw_in:
            result := 'in';
         rw_init:
            result := 'init';
         rw_interrupt:
            result := 'interrupt';
         rw_ioreg:
            result := 'ioreg';
         rw_low:
            result := 'low';
         rw_packed:
            result := 'packed';
         rw_loop:
            result := 'loop';
         rw_maxstrlen:
            result := 'maxstrlen';
         rw_mod:
            result := 'mod';
         rw_monitor:
            result := 'monitor';
         rw_not:
            result := 'not';
         rw_of:
            result := 'of';
         rw_or:
            result := 'or';
         rw_ord:
            result := 'ord';
         rw_otherwise:
            result := 'otherwise';
         rw_overlay:
            result := 'overlay';
         rw_pred:
            result := 'pred';
         rw_priority:
            result := 'priority';
         rw_procedure:
            result := 'procedure';
         rw_process:
            result := 'process';
         rw_property:
            result := 'property';
         rw_public:
            result := 'public';
         rw_record:
            result := 'record';
         rw_recycle:
            result := 'recycle';
         rw_reloop:
            result := 'reloop';
         rw_repeat:
            result := 'repeat';
         rw_rom:
            result := 'rom';
         rw_round:
            result := 'round';
         rw_set:
            result := 'set';
         rw_strappend:
            result := 'strappend';
         rw_string:
            result := 'string';
         rw_strlen:
            result := 'strlen';
         rw_strpos:
            result := 'strpos';
         rw_succ:
            result := 'succ';
         rw_then:
            result := 'then';
         rw_to:
            result := 'to';
         rw_true:
            result := 'true';
         rw_trunc:
            result := 'trunc';
         rw_type:
            result := 'type';
         rw_until:
            result := 'until';
         rw_var:
            result := 'var';
         rw_while:
            result := 'while';
         rw_with:
            result := 'with';
         else
            assert(false, 'missing reserved word');
      end
   end;

constructor TLexicalAnalysis.Create;
   var
      rw: TReservedWordEnum;
   begin
      assert (lex = nil);
      lex := Self;
      symbol_table := TSymbolTable.Create;
      for rw := low(TReservedWordEnum) to high(TReservedWordEnum) do
         symbol_table.add_reserved_word(reserved_word_to_string(rw), rw);

      SetLength (file_list, 0);
      SetLength (source, 0);
      current_source_idx := 0
   end;

procedure init_source (preamble: TStringList);
   var
      source_idx: integer;
      line_no: integer;
   begin
      SetLength (file_list, 0);
      SetLength (source, 0);

      if preamble.Count = 0 then
         begin  // preamble must be at least one line so that EmptySourceLocation points somewhere
            source_idx := length(source);
            SetLength(source, source_idx + 1);
            source[source_idx].file_list_idx := -1;
            source[source_idx].line := '';
            source[source_idx].line_no := 0
         end
      else
         for line_no := 0 to preamble.Count-1 do
            begin
               source_idx := length(source);
               SetLength(source, source_idx + 1);
               source[source_idx].file_list_idx := -1;
               source[source_idx].line := preamble[line_no];
               source[source_idx].line_no := line_no
            end;
      preamble.Free;

      current_source_idx := 0
   end;

procedure TLexicalAnalysis.ReadInPreamble (preamble: TStringList);
   begin
      init_source (preamble)
   end;

procedure TLexicalAnalysis.ReadInSourceFile (source_file_name: string);
   begin
      first_non_preamble_source_idx := Length(source);
      try
         read_in_file (IncludeTrailingPathDelimiter(GetCurrentDir) + source_file_name, true, NonExistantSourceLocation)
      except
         on EFileDoesntExist do
            raise compile_error.Create(format (err_source_file_doesnt_exist, [source_file_name]), FileErrorSourceLocation);
         on ECantOpenFile do
            raise compile_error.Create(format (err_cant_open_source_file, [source_file_name]), FileErrorSourceLocation)
      end
   end;

procedure TLexicalAnalysis.ReadInTestCase (line: string);
   begin
      current_dir := IncludeTrailingPathDelimiter(GetCurrentDir);
      first_non_preamble_source_idx := Length(source);
      read_in_source_for_test_case (line)
   end;

procedure TLexicalAnalysis.ReadInTestCase (lines: TStrings);
   begin
      current_dir := IncludeTrailingPathDelimiter(GetCurrentDir);
      first_non_preamble_source_idx := Length(source);
      read_in_source_for_test_case (lines)
   end;

function TTokenType.in_preamble: boolean;
   begin
      result := src_loc.source_idx < first_non_preamble_source_idx
   end;

procedure TLexicalAnalysis.LoadCPUBasicDataTypeIdentifiers;
   var
      i: integer;
   begin
      SetLength (supported_data_type_id_idxs, Length(target_cpu.supported_data_types));
      for i := 0 to Length(target_cpu.supported_data_types)-1
         do supported_data_type_id_idxs[i] :=
            define_identifier (target_cpu.supported_data_types[i].id)
   end;

function TLexicalAnalysis.NumberOfIdentifiers: integer;
   begin
      result := Length(identifiers)
   end;

function TLexicalAnalysis.define_identifier
   (identifier: string
   ): TIdentifierIdx;
   begin
      result := Length(identifiers);
      SetLength (identifiers, result + 1);
      identifiers[result] := identifier;
      symbol_table.add_identifier(identifier, result)
   end;

function TLexicalAnalysis.token: TTokenType;
   begin
      result := tokens[token_idx]
   end;

function TLexicalAnalysis.next_token: TTokenType;
   begin
      if tokens[token_idx].token_kind = eof_token then
         result := tokens[token_idx]
      else
         result := tokens[token_idx+1]
   end;

function TLexicalAnalysis.previous_token_src_loc: TSourceLocation;
   begin
      if token_idx = 0 then
         result := NonExistantSourceLocation
      else
         result := tokens[token_idx-1].src_loc;
   end;

procedure TLexicalAnalysis.advance_token;
   begin
      token_idx := token_idx + 1
   end;

function TLexicalAnalysis.mark_token_position: integer;
   begin
      result := token_idx
   end;

procedure TLexicalAnalysis.backup (mark: integer);
   begin
      token_idx := mark
   end;

function TLexicalAnalysis.token_is_symbol
   (op: TSymbolEnum
   ): boolean;
   begin
      result := (tokens[token_idx].token_kind = symbol_token)
      and
      (tokens[token_idx].symbol = op)
   end;

function TLexicalAnalysis.token_is_symbol
   (op_set: TSymbolEnumSet
   ): boolean;
   begin
      result := (tokens[token_idx].token_kind = symbol_token)
      and
      (tokens[token_idx].symbol in op_set)
   end;

function TLexicalAnalysis.token_is_reserved_word
   (rw: TReservedWordEnum
   ): boolean;
   begin
      result := (tokens[token_idx].token_kind = reserved_word_token)
      and
      (tokens[token_idx].rw = rw)
   end;

function TLexicalAnalysis.token_is_reserved_word
   (rw_set: TReservedWordEnumSet
   ): boolean;
   begin
      result := (tokens[token_idx].token_kind = reserved_word_token)
      and
      (tokens[token_idx].rw in rw_set)
   end;

function TLexicalAnalysis.token_is_identifier: boolean;
   begin
      result := (tokens[token_idx].token_kind = identifier_token)
   end;

function TLexicalAnalysis.token_is_constant: boolean;
   begin
      result := tokens[token_idx].token_kind in [integer_constant_token, real_constant_token, string_constant_token]
   end;

function TLexicalAnalysis.token_is_eof: boolean;
   begin
      result := (token_idx >= Length(tokens))
                or
                (tokens[token_idx].token_kind = eof_token)
   end;

procedure TLexicalAnalysis.DoLexicalAnalysis;
   var
      source_idx, line_idx: integer;
      line: string;
      src_loc: TSourceLocation;
   function new_token_idx
      (kind: TTokenKind
      ): integer;
      var
         i: integer;
      begin
         i := length(tokens);
         SetLength(tokens, i + 1);
         tokens[i].src_loc := src_loc;
         tokens[i].src_loc.length := line_idx - src_loc.line_idx;
         tokens[i].token_kind := kind;
         result := i
      end;
   procedure put_operator
      (op: TSymbolEnum;
       len: integer
      );
      var
         idx: integer;
      begin
         line_idx := line_idx + len;
         idx := new_token_idx(symbol_token);
         tokens[idx].symbol := op
      end;
   procedure put_reserved_word
      (rw: TReservedWordEnum
      );
      var
         idx: integer;
      begin
         idx := new_token_idx(reserved_word_token);
         tokens[idx].rw := rw
      end;
   procedure put_identifier
      (identifier_idx: TIdentifierIdx
      );
      var
         idx: integer;
      begin
         idx := new_token_idx(identifier_token);
         tokens[idx].identifier_idx := identifier_idx
      end;
   procedure put_integer_constant
      (i: TMultiPrecisionInteger
      );
      var
         idx: integer;
      begin
         idx := new_token_idx(integer_constant_token);
         tokens[idx].i := TMultiPrecisionInteger.Create (i)
      end;
   procedure put_real_constant
      (r: real
      );
      var
         idx: integer;
      begin
         idx := new_token_idx(real_constant_token);
         tokens[idx].r := r
      end;
   procedure put_string_constant
      (s: string
      );
      var
         idx: integer;
      begin
         idx := new_token_idx(string_constant_token);
         tokens[idx].s := s
      end;
   procedure put_eof;
      var
         idx: integer;
      begin
         idx := new_token_idx(eof_token);
         if idx = 0 then
            begin
               tokens[0].src_loc.source_idx := Length(source)-1;
               if Length(source) = 0
               then
                  tokens[0].src_loc.line_idx := 1
               else
                  tokens[0].src_loc.line_idx := Length(source[Length(source)-1].line) + 1
            end
         else
            begin
               tokens[idx].src_loc := tokens[idx-1].src_loc;
               tokens[idx].src_loc.line_idx := tokens[idx].src_loc.line_idx + tokens[idx].src_loc.length;
            end;
         tokens[idx].src_loc.length := 0
      end;
   const
      maxexponent = 1000;
   var
      symbol: string;
      symbol_item: TSymbolInfo;
      integer_constant: TMultiPrecisionInteger;
      string_constant: string;
   procedure parse_floating_point_number
      (mantissa: real
      );
      var
         i: integer;
         real_constant, poweroften: real;
         exponent, exppart: integer;
         expsign: boolean;
      begin
         exponent := 0;
         if line[line_idx] = '.' then
            begin // collect fraction
               line_idx := line_idx + 1;
               if (line_idx > length(line)) or not CharInSet (line[line_idx], ['0' .. '9', '_']) then
                  raise compile_error.Create(err_improper_real_const, src_loc);
               repeat
                  if line[line_idx] <> '_' then
                     begin
                        mantissa := (mantissa * 10) + ord(line[line_idx]) - ord('0');
                        exponent := exponent - 1
                     end;
                  line_idx := line_idx + 1
               until (line_idx > length(line)) or not CharInSet (line[line_idx], ['0' .. '9', '_']);
            end;
         if (line_idx <= length(line)) and CharInSet (line[line_idx], ['E', 'e']) then
            begin
               line_idx := line_idx + 1;
               if line_idx > length(line) then
                  raise compile_error.Create(err_improper_real_const, src_loc);

               exppart := 0;
               expsign := false;
               if line[line_idx] = '+' then
                  begin
                     line_idx := line_idx + 1;
                     if line_idx > length(line) then
                        raise compile_error.Create(err_improper_real_const, src_loc);
                  end
               else if line[line_idx] = '-' then
                  begin
                     expsign := true;
                     line_idx := line_idx + 1;
                     if line_idx > length(line) then
                        raise compile_error.Create(err_improper_real_const, src_loc);
                  end;
               if not CharInSet (line[line_idx], ['0' .. '9']) then
                  raise compile_error.Create(err_improper_real_const, src_loc);
               repeat
                  if exppart > maxexponent then
                     raise compile_error.Create(err_improper_real_const, src_loc);
                  exppart := exppart * 10 - ord('0') + ord(line[line_idx]);
                  line_idx := line_idx + 1;
               until (line_idx > length(line)) or not CharInSet (line[line_idx], ['0' .. '9']);
               assert(exponent <= 0);
               if expsign then
                  if maxexponent + exponent >= exppart then
                     exponent := exponent - exppart
                  else
                     raise compile_error.Create(err_improper_real_const, src_loc)
               else
                  exponent := exponent + exppart
            end;

         poweroften := 1.0;
         if exponent < 0 then
            begin
               expsign := true;
               exponent := abs(exponent)
            end
         else
            expsign := false;
         // if exponent > maxexponent then
         // begin
         // error(numbererror);
         // exponent := 0
         // end;
         for i := 1 to exponent do
            poweroften := poweroften * 10;
         { NOW EITHER MANTISSA=0.0 OR MANTISSA>=1.0 }
         if mantissa = 0 then
            real_constant := 0
         else if expsign then
            real_constant := mantissa / poweroften
         else // IF MANTISSA>=1.0 THEN WE MUST HAVE: MANTISSA*POWEROFTEM<=MAXREAL=> POWEROFTEN<=MAXREAL/MANTISSA<=MAXREAL
         // if poweroften < maxreal / mantissa then
            real_constant := mantissa * poweroften;
         // else
         // begin
         // error(numbererror);
         // real_constant := real0
         // end;
         put_real_constant(real_constant);
      end;

   begin    // DoLexicalAnalysis
      // pre-load "result" in symbol table as an identifier
      define_identifier ('result');

      integer_constant := TMultiPrecisionInteger.Create;
      try
         for source_idx := 0 to length(source) - 1 do
            begin
               src_loc.source_idx := source_idx;
               src_loc.line_idx := 1;
               src_loc.length := 1;
               if Pos ('{$', source[source_idx].line) <> 1
               then  // not a compiler directive
                  begin
                     line := source[source_idx].line;
                     line_idx := 1;
                     while (line_idx <= length(line)) and CharInSet (line[line_idx], white_space) do
                        line_idx := line_idx + 1;
                     while line_idx <= length(line) do
                        begin
                           if (line[line_idx] = '/') and (line_idx < length(line)) and (line[line_idx + 1] = '/') then // comment, skip rest of line
                              line_idx := length(line) + 1
                           else if line_idx <= length(line) then
                              begin
                                 src_loc.line_idx := line_idx;
                                 case line[line_idx] of
                                    '+':
                                       put_operator(sym_plus, 1);
                                    '-':
                                       put_operator(sym_minus, 1);
                                    '*':
                                       put_operator(sym_star, 1);
                                    '=':
                                       put_operator(sym_equals, 1);
                                    ':':
                                       if (line_idx < length(line)) and (line[line_idx + 1] = '=') then
                                          put_operator(sym_assign, 2)
                                       else
                                          put_operator(sym_colon, 1);
                                    ';':
                                       put_operator(sym_semicolon, 1);
                                    '.':
                                       if (line_idx < length(line)) and CharInSet (line[line_idx + 1], ['0'..'9']) then
                                          parse_floating_point_number (0)
                                       else if (line_idx < length(line)) and (line[line_idx + 1] = '.') then
                                          put_operator(sym_dot_dot, 2)
                                       else
                                          put_operator(sym_dot, 1);
                                    '>':
                                       if (line_idx < length(line)) and (line[line_idx + 1] = '=') then
                                          put_operator(sym_greater_than_or_equal, 2)
                                       else
                                          put_operator(sym_greater_than, 1);
                                    '<':
                                       if (line_idx < length(line)) and (line[line_idx + 1] = '=') then
                                          put_operator(sym_less_than_or_equal, 2)
                                       else if (line_idx < length(line)) and (line[line_idx + 1] = '>') then
                                          put_operator(sym_not_equals, 2)
                                       else
                                          put_operator(sym_less_than, 1);
                                    '(':
                                       put_operator(sym_left_parenthesis, 1);
                                    ')':
                                       put_operator(sym_right_parenthesis, 1);
                                    '[':
                                       put_operator(sym_left_bracket, 1);
                                    ']':
                                       put_operator(sym_right_bracket, 1);
                                    '/':
                                       put_operator(sym_slash, 1);
                                    ',':
                                       put_operator(sym_comma, 1);
                                    '''':
                                       begin
                                          line_idx := line_idx + 1;
                                          if line_idx > Length(line) then
                                             raise compile_error.Create(err_string_constant_multi_line, src_loc);
                                          string_constant := '';
                                          while (line_idx <= Length(line)) and (line[line_idx] <> '''') do
                                             begin
                                                string_constant := string_constant + line[line_idx];
                                                line_idx := line_idx + 1
                                             end;
                                          if line_idx > Length(line) then
                                             raise compile_error.Create(err_string_constant_multi_line, src_loc);
                                          line_idx := line_idx + 1;
                                          put_string_constant (string_constant)
                                       end;
                                    '#':
                                       begin
                                          line_idx := line_idx + 1;
                                          if (line_idx > Length(line))
                                             or
                                             (not CharInSet (line[line_idx], ['0'..'9']))
                                          then
                                             raise compile_error.Create (err_char_constant_expected, src_loc);
                                          integer_constant.AsInteger := 0;
                                          while (line_idx <= Length(line))
                                                and
                                                (CharInSet (line[line_idx], ['0'..'9']))
                                          do begin
                                                integer_constant.Multiply (10);
                                                integer_constant.Add (ord(line[line_idx]) - ord('0'));
                                                line_idx := line_idx + 1
                                             end;
                                          if integer_constant.gt (max_char)
                                          then
                                             raise compile_error.Create(err_char_value_outside_legal_range, src_loc);
                                          string_constant := ' ';
                                          string_constant[1] := chr(integer_constant.AsInteger);
                                          put_string_constant (string_constant)
                                       end;
                                    'A' .. 'Z', 'a' .. 'z', '_':
                                       begin
                                          symbol := '';
                                          repeat
                                             symbol := symbol + line[line_idx];
                                             line_idx := line_idx + 1
                                          until (line_idx > length(line)) or not CharInSet (line[line_idx], ['A'..'Z', 'a'..'z', '0'..'9', '_']);
                                          if symbol = '_' then
                                             raise compile_error.Create (err_invalid_identifier, src_loc);
                                          symbol_item := symbol_table.find_symbol_table_entry(symbol);
                                          case symbol_item.token_kind of
                                             reserved_word_token:
                                                put_reserved_word(symbol_item.rw);
                                             identifier_token:
                                                put_identifier(symbol_item.id);
                                             eof_token:  // means identifier not yet in symbol table, so add it
                                                begin
                                                   define_identifier (symbol);
                                                   symbol_item := symbol_table.find_symbol_table_entry(symbol);
                                                   put_identifier(symbol_item.id)
                                                end;
                                             else
                                                assert (false)
                                          end
                                       end;
                                    '$':
                                       begin   // hex constant
                                          line_idx := line_idx + 1;
                                          if (line_idx > Length(line)) then
                                             raise compile_error.Create(err_improper_hex_constant, src_loc);
                                          integer_constant.AsInteger := 0;
                                          repeat
                                             if line[line_idx] <> '_' then
                                                integer_constant.shift_left (4);
                                             case line[line_idx] of
                                                '0'..'9':
                                                   integer_constant.Add (ord(line[line_idx]) - ord('0'));
                                                'A'..'F':
                                                   integer_constant.Add (ord(line[line_idx]) - ord('A') + 10);
                                                'a'..'f':
                                                   integer_constant.Add (ord(line[line_idx]) - ord('a') + 10);
                                                '_':
                                                   ;
                                                else
                                                   raise compile_error.Create(err_improper_hex_constant, src_loc);
                                             end;
                                             line_idx := line_idx + 1
                                          until (line_idx > Length(line)) or not CharInSet (line[line_idx], ['0'..'9', 'A'..'F', 'a'..'f', '_']);
                                          put_integer_constant(integer_constant)
                                       end;
                                    '0' .. '9':
                                       begin
                                          integer_constant.AsInteger := 0;
                                          repeat
                                             if line[line_idx] <> '_' then
                                                begin
                                                   integer_constant.Multiply (10);
                                                   integer_constant.Add (ord(line[line_idx]) - ord('0'))
                                                end;
                                             line_idx := line_idx + 1
                                          until (line_idx > length(line)) or not CharInSet (line[line_idx], ['0' .. '9', '_']);
                                          if (line_idx > length(line))
                                             or
                                             ((line_idx < Length(line)) and (line[line_idx] = '.') and (line[line_idx+1] = '.'))
                                             or
                                             (not CharInSet (line[line_idx], ['.', 'E', 'e']))
                                          then
                                             put_integer_constant(integer_constant)
                                          else
                                             parse_floating_point_number (integer_constant.AsReal)
                                       end;
                                    '{':
                                       if (line_idx < length(line))
                                          and
                                          (line[line_idx+1] = '$')
                                       then
                                          raise compile_error.Create (err_compiler_directive_must_start_in_column_1, src_loc)
                                       else
                                          raise compile_error.Create (err_comment_use_double_slash_comment_style, src_loc);
                                    else
                                       raise compile_error.Create(err_invalid_char, src_loc)
                                 end;
                                 while (line_idx <= length(line)) and CharInSet (line[line_idx], white_space) do
                                    line_idx := line_idx + 1
                              end
                        end
                  end
            end;
         put_eof;
         token_idx := 0
      finally
         integer_constant.Free
      end
   end;    // DoLexicalAnalysis

destructor TLexicalAnalysis.Destroy;
   var
      i: integer;
   begin
      symbol_table.Free;
      for i := 0 to Length(tokens)-1 do
         if tokens[i].token_kind = integer_constant_token then
            tokens[i].i.Free;
      inherited;
      lex := nil
   end;

function TLexicalAnalysis.token_string (src_loc: TSourceLocation): string;
   begin
      result := token_string (src_loc, src_loc)
   end;

function TLexicalAnalysis.token_string (first_src_loc, last_src_loc: TSourceLocation): string;
   function eq (a,b: TSourceLocation): boolean;
      begin
         eq := (a.source_idx = b.source_idx)
               and
               (a.line_idx = b.line_idx)
               and
               (a.length = b.length)
      end;
   procedure add_whitespace;
      begin
      end;
   var
      i: integer;
      in_range: boolean;
   begin
      result := '';
      in_range := false;
      for i := 0 to Length(tokens) - 1 do
         begin
            if eq (tokens[i].src_loc, first_src_loc) then
               in_range := true;
            if in_range then
               begin
                  if (result <> '')
                     and
                     (result[Length(result)] in ['A'..'Z', 'a'..'z', '0'..'9', '_'])
                     and
                     (source_token(tokens[i].src_loc)[1] in ['A'..'Z', 'a'..'z', '0'..'9', '_'])
                  then
                     result := result + ' ';
                  result := result + source_token(tokens[i].src_loc);
               end;
            if eq (tokens[i].src_loc, last_src_loc) then
               in_range := false
         end
   end;


END.
