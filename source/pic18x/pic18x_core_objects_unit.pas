UNIT pic18x_core_objects_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_definitions_unit,
   pic18x_instructions_unit,
   pic18x_macro_instructions_unit;

type
   TPIC18x_Variable =
      class (TVariable)
      protected
         procedure set_addr (addr: integer);
            override;
         function get_addr: integer;
            override;
      public
         // for process variables only
         pcb_address: integer;
         stack_address: integer;
         // used for interrupt variables only:
         dropped_interrupt_br: TGOTOMacro;
         dropped_interrupt_endif: TInstruction;
      public
         constructor CreateCopy (v: TVariable);
      end;

   TPIC18x_TypeInfo =
      class (TTypeInfo)
      protected
         function calculate_ordinal_size: integer;
         function calculate_set_size: integer;
      public
         is_in_alternate_shared_address_space: boolean;  // only used for certain ioreg types in some PICs
         function Size: integer;
            virtual;
         procedure set_reversed_byte_order;
      end;

   TByteParamProcedureOfObject = procedure (b: byte; byte_no: integer; path, value: string; _initialization_unnecessary: boolean) of object;

   TPIC18x_TypeDef_TypeInfo =
      class (TPIC18x_TypeInfo)
         function Size: integer;
            override;
         procedure enumerate_constant_bytes (path: string; constant: TDefinition; proc: TByteParamProcedureOfObject);
            virtual;
      end;

   TPIC18x_Expression_TypeInfo =
      class (TPIC18x_TypeInfo)
         is_ieee_single: boolean;
         function Size: integer;
            override;
      end;

   TPIC18x_PackedRecord_TypeInfo =
      class (TPIC18x_TypeDef_TypeInfo)
      private
         calculations_done: boolean;
         f_size: integer;
         f_unused_bits: integer;
      public
         procedure calculate_layout;
         function Size: integer;
            override;
         function get_constant_bytes_array (sc: TStructuredConstant): TDataByteArray;
         procedure enumerate_constant_bytes (path: string; constant: TDefinition; proc: TByteParamProcedureOfObject);
            override;
      end;


IMPLEMENTATION

uses
   cpc_blocks_unit,
   cpc_multi_precision_integer_unit,
   cpc_target_cpu_unit,
   cpc_types_unit,
   Math,
   pic18x_blocks_unit,
   pic18x_cpu_unit,
   pic18x_floating_point_unit,
   pic18x_kernel_unit,
   pic18x_types_unit,
   SysUtils;

var
   temp, temp2: TMultiPrecisionInteger;

constructor TPIC18x_Variable.CreateCopy (v: TVariable);
   begin
      inherited;
      pcb_address := TPIC18x_Variable(v).pcb_address
   end;

procedure TPIC18x_Variable.set_addr (addr: integer);
   begin
      assert (not is_anonymous_rom_string);
      inherited
   end;

function TPIC18x_Variable.get_addr: integer;
   begin
      if is_anonymous_rom_string then
         result := TPIC18x_CPU(target_cpu).anonymous_string_constant_rom_addr (anonymous_rom_string)
      else
         result := inherited get_addr
   end;

function TPIC18x_TypeInfo.calculate_ordinal_size: integer;
   begin
      if min_value.lt(0) then
         result := max (((min_value.BitsRequiredForSignedValue-1) div 8) + 1,
                        ((max_value.BitsRequiredForSignedValue-1) div 8) + 1
                       )
      else   // unsigned
         result := ((max_value.BitsRequiredForUnsignedValue-1) div 8) + 1
   end;

function TPIC18x_TypeInfo.calculate_set_size: integer;
   function set_size (mv: integer): integer;
      begin
         result := (mv div 8) + 1
      end;
   begin
      if (max_value.AsInteger = min_set) and (min_value.AsInteger = max_set) then
         result := 0  // empty set
      else
         result := min(set_size(max_value.AsInteger), set_size (max_set))
   end;

function TPIC18x_TypeInfo.Size: integer;
   begin
      assert (parent = nil);
      result := 0;   // suppress compiler warning
      case range_kind of
         irOrdinalRange:
            result := calculate_ordinal_size;
         irSetBounds:
            result := calculate_set_size;
         irStringLength:
            assert (false);
      else
         assert (false)
      end
   end;

procedure TPIC18x_TypeInfo.set_reversed_byte_order;
   procedure reverse_field_info (fi: TPIC18x_PackedRecordFieldInfo; packed_record_size: integer);
      begin
         if not fi.reversed_byte_order then
            begin
               fi.reversed_byte_order := true;
               fi.f_offset := packed_record_size - fi.f_offset - 1
            end
      end;
   var
      pr: TPackedRecordType;
      prfi: TPIC18x_PackedRecordFieldInfo;
      o, f: integer;
   begin
      assert (parent.definition_kind = type_definition);
      case TTypeDef(parent).type_kind of
         packed_record_type:
            begin
               pr := TPackedRecordType(parent);
               for f := 0 to Length(pr.fields)-1 do
                  begin
                     prfi := TPIC18x_PackedRecordFieldInfo(pr.fields[f].info);
                     reverse_field_info (prfi, TPIC18x_PackedRecord_TypeInfo(pr.info).Size)
                  end
            end;
         overlay_type:
            for o := 0 to Length(TOverlayType(parent).overlaid_variables)-1 do
               begin
                  assert (TOverlayType(parent).overlaid_variables[o].typedef.type_kind = packed_record_type);
                  pr := TPackedRecordType(TOverlayType(parent).overlaid_variables[o].typedef);
                  for f := 0 to Length(pr.fields)-1 do
                     begin
                        prfi := TPIC18x_PackedRecordFieldInfo(pr.fields[f].info);
                        reverse_field_info (prfi, TPIC18x_PackedRecord_TypeInfo(pr.info).Size)
                     end
               end;
      else
         assert (false)
      end
   end;

function TPIC18x_TypeDef_TypeInfo.Size: integer;
   var
      typ: TTypeDef;
      i: integer;
   begin
      result := 0;  // suppress compiler warnign
      typ := TTypeDef (parent);
      case typ.type_kind of
         basic_data_type:
            case TBasicDataType(typ).basic_data_type_kind of
               ordinal_data_type:
                  result := calculate_ordinal_size;
               floating_point_data_type:
                  result := real_variable_size;
            else
               assert (false)
            end;
         set_type:
            result := calculate_set_size;
         array_type:
            result := (TPIC18x_TypeInfo(TArrayType(typ).element_typedef.info).Size
                       *
                       (TArrayType(typ).index_typedef.info.max_value.AsInteger - TArrayType(typ).index_typedef.info.min_value.AsInteger + 1)
                      );
         record_type:
            begin
               result := 0;
               for i := 0 to Length(TRecordType(typ).fields)-1 do
                  result := result + TPIC18x_TypeInfo(TRecordType(typ).fields[i].typedef.info).Size
            end;
         overlay_type:
            begin
               result := 0;
               for i := 0 to Length(TOverlayType(typ).overlaid_variables)-1 do
                  result := max (result, TPIC18x_TypeInfo(TOverlayType(typ).overlaid_variables[i].typedef.info).Size);
            end;
         string_type:
            result := TStringType(typ).max_length + 1;
         system_type:
            begin
               result := TPIC18x_SystemType(typ).control_block_size
                         +
                         TPIC18x_ParamList(TSystemType(typ).parameters).Size
                         +
                         TPIC18x_DataItemList(TSystemType(typ).permanent_ram_vars).Size;
               if TPIC18x_SystemType(typ).contains_eeprom_vars then
                  result := result + 1;
            end;
         queue_type:
            result := queue_variable_size;
      else
         assert (false)
      end
   end;

procedure TPIC18x_TypeDef_TypeInfo.enumerate_constant_bytes (path: string; constant: TDefinition; proc: TByteParamProcedureOfObject);
   // output bytes in ascending address order
   procedure enumerate_constant (path: string; typ: TTypeDef; c: TConstant);
      var
         i: integer;
         pic_real: TPIC18x_Real;
      begin
         case typ.type_kind of
            basic_data_type:
               case TBasicDataType(typ).basic_data_type_kind of
                  ordinal_data_type:
                     for i := Size-1 downto 0 do
                        proc (c.AsByte(i), i, path, c.AsString, false);
                  floating_point_data_type:
                     begin
                        pic_real.r := c.real_value;
                        if typ = target_cpu.get_supported_data_type ('real') then
                           for i := 3 downto 0 do
                              proc (pic_real.pic_real_AsByte[i], i, path, c.AsString, false)
                        else if typ = target_cpu.get_supported_data_type (ieee_single_type_name) then
                           for i := 3 downto 0 do
                              proc (pic_real.ieee_single_AsByte[i], i, path, c.AsString, false)
                        else
                           assert (false)
                     end;
               else
                  assert (false)
               end;
            set_type:
               for i := Size-1 downto 0 do
                  proc (c.AsByte(i), i, path, '[?]', false);
            string_type:
               begin
                  proc (Length(c.s), 0, path, '''' + c.s + '''', false);
                  for i := 1 to Length(c.s) do
                     proc (ord(c.s[i]), i, path, '''' + c.s + '''', false);
                  for i := Length(c.s)+1 to Size-1 do
                     proc (0, i, path, '''' + c.s + '''', true)
               end;
         else
            assert (false)
         end
      end;
   var
      typ: TTypeDef;
      i: integer;
      sc: TStructuredConstant;
      pic_real: TPIC18x_Real;
   begin
      typ := TTypeDef (parent);
      if constant = nil then
         for i := 1 to TPIC18x_TypeDef_TypeInfo(typ.info).Size do
            proc (0, i, '', 'null', false)
      else
         case constant.definition_kind of
            constant_definition:
               if TConstant(constant).constant_kind = real_constant then
                  begin
                     pic_real.r := TConstant(constant).real_value;
                     if typ = target_cpu.get_supported_data_type ('real') then
                        for i := 3 downto 0 do
                           proc (pic_real.pic_real_AsByte[i], i, '', FloatToStr(TConstant(constant).r), false)
                     else if typ = target_cpu.get_supported_data_type (ieee_single_type_name) then
                        for i := 3 downto 0 do
                           proc (pic_real.ieee_single_AsByte[i], i, '', FloatToStr(TConstant(constant).r), false)
                     else
                        assert (false)
                  end
               else
                  enumerate_constant (path, typ, TConstant(constant));
            structured_constant_definition:
               begin
                  sc := TStructuredConstant(constant);
                  for i := 0 to sc.LengthOfSimpleConstants-1 do
                     TPIC18x_TypeDef_TypeInfo(sc[i].typedef.info).enumerate_constant_bytes (path + sc[i].path, sc[i].constant, proc);
                  if sc.overlay_typedef <> nil then
                     for i := 1 to TPIC18x_TypeDef_TypeInfo(sc.overlay_typedef.info).Size - TPIC18x_TypeDef_TypeInfo(sc.typedef.info).Size do
                        proc (0,  i, '(overlay padding)', '0', false)
               end;
         else
            assert (false)
         end;

   end;

function TPIC18x_Expression_TypeInfo.Size: integer;
   var
      expr: TExpression;
   begin
      result := 0;  // suppress compiler warning
      assert (parent.definition_kind = expression_definition);
      expr := TExpression (parent);
      case expr.expression_kind of
         char_expression,
         boolean_expression:
            result := 1;
         enum_expression,
         integer_expression:
            result := calculate_ordinal_size;
         set_expression:
            result := calculate_set_size;
         real_expression:
            result := 4;
         string_expression,
         record_expression,
         packed_record_expression,
         overlay_expression,
         array_expression,
         system_type_expression:
            result := TPIC18x_TypeInfo(expr.typedef.info).Size;
      else
         assert (false)
      end
   end;

procedure TPIC18x_PackedRecord_TypeInfo.calculate_layout;
   var
      i: integer;
      prec: TPackedRecordType;
      fld_info: TPIC18x_PackedRecordFieldInfo;
      bit, offset_from_last_byte, w, span: integer;
   begin
      if calculations_done then
         exit;

      prec := TPackedRecordType(parent);
      bit := -1;
      offset_from_last_byte := 1;
      for i := 0 to Length(prec.fields)-1 do
         begin
            if bit = -1 then
               begin
                  offset_from_last_byte := offset_from_last_byte - 1;
                  bit := 7
               end;
            fld_info := TPIC18x_PackedRecordFieldInfo (prec.fields[i].info);
            fld_info.f_offset := offset_from_last_byte;
            fld_info.f_position := bit;
            fld_info.f_width := TTypeInfo(TPackedRecordField(fld_info.parent).ordtypedef.info).PackedSizeInBits;
            span := 1;
            w := fld_info.f_width;
            repeat
               if w > bit+1 then   // field (or remainder of field) will extend into next byte
                  begin
                     span := span + 1;
                     w := w - bit - 1;
                     bit := 7;
                     offset_from_last_byte := offset_from_last_byte - 1
                  end
               else      // field (or remainder of field) will fit in current byte
                  begin
                     bit := bit - w;
                     w := 0
                  end
            until w = 0;
            fld_info.f_span := span
         end;

      for i := 0 to Length(prec.fields)-1 do
         begin  // set // normal byte order
            fld_info := TPIC18x_PackedRecordFieldInfo (prec.fields[i].info);
            fld_info.f_offset := -fld_info.f_offset
         end;

      f_size := 1 - offset_from_last_byte;
      f_unused_bits := bit+1;
      calculations_done := true
   end;

function TPIC18x_PackedRecord_TypeInfo.Size: integer;
   begin
      calculate_layout;
      result := f_size
   end;

function TPIC18x_PackedRecord_TypeInfo.get_constant_bytes_array (sc: TStructuredConstant): TDataByteArray;
   var
      i: integer;
      prec: TPackedRecordType;
      fld_info: TPIC18x_PackedRecordFieldInfo;
      temp3: integer;
      reversed_byte_order : boolean;
   begin
      prec := TPackedRecordType (parent);
      temp.AsInteger := 0;
      reversed_byte_order := false;
      for i := 0 to Length(prec.Fields)-1 do
         begin
            fld_info := TPIC18x_PackedRecordFieldInfo(prec.Fields[i].info);
            reversed_byte_order := fld_info.reversed_byte_order;
            temp.shift_left (fld_info.Width);
            temp2.Set2ToExp(fld_info.Width);
            temp2.Subtract(1);
            temp3 := sc.packed_record_fields[i].c.ordinal_value.AsInteger and temp2.AsInteger;
            temp.Add (temp3)
         end;
      temp.shift_left (f_unused_bits);
      SetLength (result, Size);
      if reversed_byte_order then
         for i := 0 to Size-1 do
            result [i] := temp.AsByte(i)
      else  // normal byte order
         for i := 0 to Size-1 do
            result [Size-1-i] := temp.AsByte(i)
   end;

procedure TPIC18x_PackedRecord_TypeInfo.enumerate_constant_bytes (path: string; constant: TDefinition; proc: TByteParamProcedureOfObject);
   var
      i: integer;
      bytes: TDataByteArray;
   begin
      assert (constant.definition_kind = structured_constant_definition);
      bytes := get_constant_bytes_array (TStructuredConstant (constant));
      for i := 0 to Size-1 do
         proc (bytes[i], i, 'xx' + IntToStr(i), 'yy', false)
   end;

INITIALIZATION
   temp := TMultiPrecisionInteger.Create;
   temp2 := TMultiPrecisionInteger.Create;

FINALIZATION
   temp.Free;
   temp2.Free

END.
