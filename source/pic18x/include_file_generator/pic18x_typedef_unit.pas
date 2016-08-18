UNIT pic18x_typedef_unit;

INTERFACE

uses
   Generics.Collections, Classes, pic18x_information_unit,
   RegularExpressions, common_unit;

type
   tTypeDef =
      class
      private
         const
            max_variable_size = 20;  // in bytes
            max_bitno = (max_variable_size*8)-1;
         type
            t_bits_used = set of 0..max_bitno;
            t_field =
               class
                  name: string;
                  bits_used: t_bits_used;
                  bitno, f_width: integer;
                  function okay (pr_width: integer): boolean;
                  constructor Create (_name: string; _bitno, _f_width: integer);
               end;
            t_overlaid_packed_record =
               class
                  initial_rank: integer;
                  pr_width: integer;    // in bits
                  fields: TObjectList<t_field>;
                  bits_used: t_bits_used;
                  keep_together: boolean;
                  constructor Create (_initial_rank, _pr_width: integer);
                  procedure add_field (f: t_field);
                  function extract_field (f: t_field): t_field;
                  procedure add_empty_bit_fields;
                  procedure sort_by_bitno;
                  destructor Destroy;
                     override;
               end;
            t_overlaid_packed_record_list = TObjectList<t_overlaid_packed_record>;
         var
            t_width: integer;    // in bits
            overlaid_packed_records: t_overlaid_packed_record_list;
            mode_order: TStringList;
            intX_regex: TRegEx;
         function field_exists (name: string): boolean;
            overload;
         function field_exists (name: string; bitno, width: integer): boolean;
            overload;
         function search_and_extract (fieldname: string): t_field;
            overload;
         function search_and_extract (ignore_field: t_field; fieldname: string): t_field;
            overload;
         procedure combine_related_single_bit_fields_into_single_mode;
         procedure combine_adjacent_HL_pairs;
         procedure combine_adjacent_same_named_fields;
         procedure move_fields_forward;
         procedure sort_packed_records_by_rank;
         constructor Create (_typename: string; _width: integer; _reversed: boolean);
      public
         typename: string;
         reversed: boolean;
         function field_name_filter (fn: string): string;
         procedure AddField (mode, name: string; bitno, width: integer);
         procedure AddSFRFields (sfr: TSFRDef; offset: integer);   // offset in bytes
         procedure AppendIncludeFileSource (out: TOutStringProc);
         destructor Destroy;
            override;
      end;

   tTypeDefList =
      class (TObjectDictionary<string, tTypeDef>)
         constructor Create;
         function GetTypeDef (type_name: string; Size: integer; reversed: boolean): tTypeDef;   // typename should not include initial 't', that will be added when necessary
         procedure AppendIncludeFileSource (out: TOutStringProc);
         procedure AppendXMLFile (out: TOutStringProc);
      end;

IMPLEMENTATION

uses
   SysUtils, Generics.Defaults;

//===============
//  tTypeDef.tField

constructor tTypeDef.t_field.Create (_name: string; _bitno, _f_width: integer);
   var i: integer;
   begin
      name := _name;
      bitno := _bitno;
      f_width := _f_width;
      for i := bitno downto bitno - f_width + 1 do
         bits_used := bits_used + [i]
   end;

function tTypeDef.t_field.okay (pr_width: integer): boolean;
   var i: integer;
   begin
      result := true;
      for i := pr_width-1 downto bitno+1 do
         if i in bits_used then
            result := false;
      for i := bitno downto bitno-f_width+1 do
         if not (i in bits_used) then
            result := false;
      for i := bitno-f_width downto 0 do
         if i in bits_used then
            result := false
   end;


//=================================
//  tTypeDef.t_overlaid_packed_record

constructor tTypeDef.t_overlaid_packed_record.Create (_initial_rank, _pr_width: integer);
   begin
      fields := TObjectList<t_field>.Create;
      initial_rank := _initial_rank;
      pr_width := _pr_width
   end;

procedure tTypeDef.t_overlaid_packed_record.add_field (f: t_field);
   begin
      assert (bits_used * f.bits_used = [], 'field bits already defined in this mode');
      bits_used := bits_used + f.bits_used;
      fields.Add (f)
   end;

function tTypeDef.t_overlaid_packed_record.extract_field (f: t_field): t_field;
   begin
      result := fields.Extract(fields[fields.IndexOf(f)]);
      bits_used := bits_used - result.bits_used
   end;

procedure tTypeDef.t_overlaid_packed_record.add_empty_bit_fields;
   var
      i, b, w: integer;
   begin
      i := pr_width - 1;
      repeat
         if i in bits_used then
            i := i - 1
         else
            begin
               b := i;
               w := 0;
               repeat
                  w := w + 1;
                  i := i - 1
               until (i < 0) or (i in bits_used);
               while w > 64 do
                  begin
                     add_field (t_field.Create ('-', b, 64));
                     b := b - 64;
                     w := w - 64
                  end;
               add_field (t_field.Create ('-', b, w))
            end
      until i < 0
   end;

procedure tTypeDef.t_overlaid_packed_record.sort_by_bitno;
   begin
      fields.Sort (TComparer<t_field>.Construct
                     (function (const L, R: t_field): integer
                         begin
                            result := R.bitno - L.bitno   // want ascending order
                         end
                     )
                  )
   end;

destructor tTypeDef.t_overlaid_packed_record.Destroy;
   begin
      fields.Free
   end;


//========
//  tTypeDef

constructor tTypeDef.Create (_typename: string; _width: integer; _reversed: boolean);
   begin
      typename := _typename;
      t_width := _width;
      reversed := _reversed;
      intX_regex := TRegEx.Create ('^INT[1-9][0-9]*$');
      overlaid_packed_records := TObjectList<t_overlaid_packed_record>.Create;
      mode_order := TStringList.Create
   end;

procedure tTypeDef.AddField (mode, name: string; bitno, width: integer);
   var
      pr: t_overlaid_packed_record;
      rank, i: integer;
   begin
      if (name = '-')
         or
         (name = '')
         or
         (field_exists (name, bitno, width))
         or
         ((typename = 'INTCON') and (bitno in [6,7]) and (width=1))   // reserved for kernel
         or
         ((typename = 'WDTCON') and (name = 'ADSHR'))                 // reserved for kernel
      then
         exit;

      rank := 0;
      for i := 0 to mode_order.Count-1 do
         if mode_order[i] = mode then
            rank := i+1;
      if rank = 0 then
         begin
            mode_order.Add (mode);
            rank := mode_order.Count
         end;
      pr := t_overlaid_packed_record.Create (rank, t_width);  // initially each field gets its own packed record
      pr.add_field (t_field.Create (name, bitno, width));
      overlaid_packed_records.Add(pr);
   end;

procedure tTypeDef.AddSFRFields (sfr: TSFRDef; offset: integer);
   var
      m: TSFRMode;
      f: TSFRField;
      bitno: integer;
   begin
      for m in sfr.modes do
         begin
            bitno := -1;
            for f in m.fields do
               begin
                  AddField (m.id, field_name_filter(f.fieldname), t_width - ((offset+1)*8) + bitno + f.width, f.width);
                  bitno := bitno + f.width
               end
         end
   end;

function tTypeDef.field_exists (name: string): boolean;
   var
      pr: t_overlaid_packed_record;
      f: t_field;
   begin
      result := true;
      for pr in overlaid_packed_records do
         for f in pr.fields do
            if f.name = name
            then exit;
      result := false
   end;

function tTypeDef.field_exists (name: string; bitno, width: integer): boolean;
   var
      pr: t_overlaid_packed_record;
      f: t_field;
   begin
      result := true;
      for pr in overlaid_packed_records do
         for f in pr.fields do
            if (f.name = name)
               and
               (f.bitno = bitno)
               and
               (f.f_width = width)
            then
               exit;
      result := false
   end;

function tTypeDef.search_and_extract (fieldname: string): t_field;
   begin
      result := search_and_extract (nil, fieldname)
   end;

function tTypeDef.search_and_extract (ignore_field: t_field; fieldname: string): t_field;
   var
      pr: t_overlaid_packed_record;
      f: t_field;
   begin  // search_and_extract
      for pr in overlaid_packed_records do
         for f in pr.fields do
            if (f <> ignore_field)
               and
               (f.name = fieldname)
            then
               begin
                  result := pr.extract_field(f);
                  exit
               end;
      result := nil
   end;   // search_and_extract

function tTypeDef.field_name_filter (fn: string): string;
   begin
      // fix field names that conflict with Concurrent Pascal reserved identifiers
      if intX_regex.IsMatch (fn) then
         result := fn + '_'
      else if fn = 'TO' then
         result := 'TO_'
      else
         result := fn
   end;

procedure tTypeDef.combine_related_single_bit_fields_into_single_mode;
   function search_and_extract_by_bitno (base: string; bitno: integer): t_field;
      var
         pr: t_overlaid_packed_record;
         f: t_field;
         fn: string;
      begin  // search_and_extract
         fn := base + IntToStr(bitno);
         for pr in overlaid_packed_records do
            for f in pr.fields do
               if (f.bitno = bitno)
                  and
                  (f.f_width = 1)
                  and
                  (f.name = fn)
               then
                  begin
                     result := pr.extract_field(f);
                     exit
                  end;
         result := nil
      end;   // search_and_extract
   var
      pr0, prCombined: t_overlaid_packed_record;
      f0, fn: t_field;
      base: string;
      bitno, highest_bitno: integer;
   begin  // combine_related_single_bit_fields_into_single_mode
      for pr0 in overlaid_packed_records do
         for f0 in pr0.fields do
            if (f0.f_width = 1)
               and
               (f0.name[Length(f0.name)] = '0')
            then
               begin
                  base := Copy (f0.name, 1, Length(f0.name)-1);
                  bitno := 1;
                  highest_bitno := 1;
                  repeat
                     fn := search_and_extract_by_bitno (base, bitno);
                     if fn <> nil then
                        begin
                           pr0.add_field (fn);
                           pr0.keep_together := true;
                           highest_bitno := bitno;
                           bitno := bitno + 1
                        end
                  until fn = nil;
                  if (highest_bitno > 1)
                     and
                     (not field_exists (base))
                  then
                     begin
                        prCombined := t_overlaid_packed_record.Create (0, t_width);
                        prCombined.add_field (t_field.Create (base, highest_bitno, highest_bitno - f0.bitno + 1));
                        overlaid_packed_records.Insert (0, prCombined)
                     end
               end
   end;   // combine_related_single_bit_fields_into_single_mode

procedure tTypeDef.combine_adjacent_HL_pairs;
   var
      prL, prCombined: t_overlaid_packed_record;
      fL, fH: t_field;
      basename: string;
   begin
      for prL in overlaid_packed_records do
         for fl in prL.fields do
            if (fl.f_width = 8)
               and
               ((fl.bitno+1) mod 8 = 0)
               and
               (Length(fl.name) > 1)
               and
               (fl.name[Length(fl.name)] = 'L')
            then
               begin
                  basename := Copy (fl.name, 1, Length(fl.name)-1);
                  fH := search_and_extract (basename + 'H');
                  if fH <> nil then
                     begin
                        assert ((fH.bitno - fH.f_width = fL.bitno), 'out of place field');
                        prL.add_field (fH);
                        prL.keep_together := true;
                        if not field_exists (basename, fH.bitno, fH.f_width + fL.f_width) then
                           begin
                              assert (not field_exists (basename));
                              prCombined := t_overlaid_packed_record.Create (0, t_width);
                              prCombined.add_field (t_field.Create (basename, fH.bitno, fH.f_width + fL.f_width));
                              overlaid_packed_records.Insert (0, prCombined)
                           end
                     end
               end
   end;

procedure tTypeDef.combine_adjacent_same_named_fields;
   var
      pr: t_overlaid_packed_record;
      fa, fb: t_field;
      done: boolean;
   begin
      for pr in overlaid_packed_records do
         for fa in pr.fields do
            repeat
               done := true;
               fb := search_and_extract (fa, fa.name);
               if fb <> nil then
                  begin
                     done := false;
                     assert (pr.bits_used * fb.bits_used = []);
                     if fa.bitno < fb.bitno then
                        fa.bitno := fb.bitno;
                     fa.f_width := fa.f_width + fb.f_width;
                     pr.bits_used := pr.bits_used + fb.bits_used;
                     fa.bits_used := fa.bits_used + fb.bits_used;
                     fb.Free
                  end
            until done
   end;

procedure tTypeDef.move_fields_forward;
   var
      pr0, pr1: t_overlaid_packed_record;
      i0, i1, j: integer;
      f: t_field;
   begin
      for i0 := 0 to overlaid_packed_records.Count-1 do
         begin
            pr0 := overlaid_packed_records[i0];
            for i1 := i0+1 to overlaid_packed_records.Count-1 do
               begin
                  pr1 := overlaid_packed_records[i1];
                  if pr1.keep_together then
                     begin
                        if pr0.bits_used * pr1.bits_used = [] then
                           while pr1.fields.Count > 0 do
                              pr0.add_field (pr1.extract_field(pr1.fields[0]))
                     end
                  else
                     begin
                        j := 0;
                        while j < pr1.fields.Count do
                           begin
                              f := pr1.fields[j];
                              if pr0.bits_used * f.bits_used = [] then
                                 pr0.add_field (pr1.extract_field(f))
                              else
                                 j := j + 1
                           end
                     end
               end
         end
   end;

procedure tTypeDef.sort_packed_records_by_rank;
   begin
      overlaid_packed_records.Sort (TComparer<t_overlaid_packed_record>.Construct
                                       (function (const L, R: t_overlaid_packed_record): integer
                                           begin
                                              result := L.initial_rank - R.initial_rank   // want descending order
                                           end
                                       )
                                    )
   end;

procedure tTypeDef.AppendIncludeFileSource (out: TOutStringProc);

   procedure delete_empty_packed_records;
      var
         i: integer;
      begin
         i := 0;
         while i < overlaid_packed_records.Count do
            if overlaid_packed_records[i].fields.Count = 0 then
               overlaid_packed_records.Delete(i)
            else
               i := i + 1
      end;

   var
      s: string;
      pr: t_overlaid_packed_record;
      f: t_field;
      indent: string;
      pr_count, field_count: integer;
   begin
      try
         sort_packed_records_by_rank;
         combine_related_single_bit_fields_into_single_mode;
         combine_adjacent_HL_pairs;
         combine_adjacent_same_named_fields;
         move_fields_forward;
         delete_empty_packed_records;
         for pr in overlaid_packed_records do
            begin
               pr.add_empty_bit_fields;
               pr.sort_by_bitno
            end;

         if overlaid_packed_records.Count = 1 then
            indent := '      '
         else
            indent := '         ';

         out ('   t' + typename + ' =');
         if overlaid_packed_records.Count > 1 then
            out ('      overlay');
         pr_count := overlaid_packed_records.Count;
         for pr in overlaid_packed_records do
            begin
               out (indent + 'packed record');

               field_count := pr.fields.Count;
               for f in pr.fields do
                  begin
                     s := indent + '   ' + f.name + ': uint' + IntToStr(f.f_width);
                     field_count := field_count - 1;
                     if field_count > 0 then
                        s := s + ';';
                     out (s)
                  end;
               pr_count := pr_count - 1;
               s := indent + 'end';
               if (pr_count > 0)
                  or
                  (overlaid_packed_records.Count = 1)
               then
                  s := s + ';';
               out (s)
            end;
         if overlaid_packed_records.Count > 1 then
            out ('      end;')
      except
         on e: EAssertionFailed do
            begin
               e.Message := 'type=' + typename + ' ' + e.Message;
               raise
            end
      end
   end;

destructor tTypeDef.Destroy;
   begin
      overlaid_packed_records.Free;
      mode_order.Free
   end;


//===============
//  tTypeDefList

constructor tTypeDefList.Create;
   begin
      inherited Create ([doOwnsValues])
   end;

function tTypeDefList.GetTypeDef (type_name: string; size: integer; reversed: boolean): tTypeDef;
   begin
      if not ContainsKey (type_name) then
         Add (type_name, tTypeDef.Create (type_name, size*8, reversed));
      result := Self[type_name]
   end;

procedure tTypeDefList.AppendIncludeFileSource (out: TOutStringProc);
   var
      typename: string;
      keyArray: TArray<string>;
      keyCollection: TObjectDictionary<string, tTypeDef>.TKeyCollection;
   begin
      keyCollection := nil;
      try
         if Count > 0 then
            begin
               // output alphabetically
               keyCollection := TObjectDictionary<string, tTypeDef>.TKeyCollection.Create (Self);
               keyArray := keyCollection.ToArray;
               TArray.Sort<string> (keyArray,
                                    TComparer<string>.Construct
                                       (function (const L, R: string): integer
                                           begin
                                              result := CompareText (L, R)
                                           end
                                       )
                                    );

               out ('type');
               for typename in keyArray do
                  begin
                     Self[typename].AppendIncludeFileSource (out);
                     out ('')
                  end
            end
      finally
         keyCollection.Free
      end
   end;

procedure tTypeDefList.AppendXMLFile (out: TOutStringProc);
   var
      typename: string;
      keyArray: TArray<string>;
      keyCollection: TObjectDictionary<string, tTypeDef>.TKeyCollection;
   begin
      keyCollection := nil;
      try
         // output alphabetically
         keyCollection := TObjectDictionary<string, tTypeDef>.TKeyCollection.Create (Self);
         keyArray := keyCollection.ToArray;
         TArray.Sort<string> (keyArray,
                              TComparer<string>.Construct
                                 (function (const L, R: string): integer
                                     begin
                                        result := CompareText (L, R)
                                     end
                                 )
                              );

         out ('   <ReversedTypes>');
         for typename in keyArray do
            if Self[typename].reversed then
               out ('      <ReversedType name="t' + typename + '"/>');
         out ('   </ReversedTypes>')
      finally
         keyCollection.Free
      end
   end;


END.
