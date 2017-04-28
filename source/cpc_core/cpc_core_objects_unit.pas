UNIT cpc_core_objects_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_definitions_unit,
   cpc_multi_precision_integer_unit,
   cpc_source_analysis_unit,
   SysUtils;

const
   min_char = 0;
   max_char = 255;
   min_set = 0;
   max_set = 255;

type
   TSet256 = set of 0..max_set;
   TEnumType = class;
   TStructuredConstant = class;

   TTypeInfoRangeKind = (irNonRange, irOrdinalRange, irSetBounds, irStringLength);
   TIntegerRange = (irAlwaysNegative, irNegativeOrPositive, irAlwaysNonNegative);
   TDefaultValue = class;
   TTypeInfo =
      class (TCPUSpecificInfo)
      private
         f_min_value, f_max_value: TMultiPrecisionInteger;
         f_default_value: TDefaultValue;
         function get_min_value:  TMultiPrecisionInteger;
         function get_max_value:  TMultiPrecisionInteger;
      public
         range_kind: TTypeInfoRangeKind;
         property min_value: TMultiPrecisionInteger read get_min_value;
         property max_value: TMultiPrecisionInteger read get_max_value;
            // range_kind=irOrdinalRange: min & max values for ordinals;
            // range_kind=irSetBounds:    min & max set member value for sets
            // range_kind=irStringLength: min & max length for string;
         constructor Create (_parent: TDefinition);
            overload; virtual;
         constructor Create (_info: TTypeInfo);
            overload; virtual;
         destructor Destroy;
            override;
         function DefaultValue: TStructuredConstant;
         function IntegerRange: TIntegerRange;
         function Signed: boolean;
         function UnSigned: boolean;
         function PackedSizeInBits: integer;
         procedure Assign (ti: TTypeInfo);
      end;

   TOrdinalBaseType =
      (empty_set_ordinal_base_unknown,
       ordinal_base_is_integer,
       ordinal_base_is_char,
       ordinal_base_is_bool,
       ordinal_base_is_enum
      );

   TTypeKind =
      (incomplete_type,
       basic_data_type,
       record_type,
       set_type,
       array_type,
       system_type,
       packed_record_type,
       string_type,
       queue_type,
       overlay_type
      );
   TTypeDef =
      class(TDefinition)
      private
         f_type_kind: TTypeKind;
         f_name: string;
         last_src_loc: TSourceLocation;
         procedure set_type_kind (tk: TTypeKind);
         procedure set_name (n: string);
         function get_name: string;
      public
         definition_complete: boolean;
         defining_scope: integer;
         property type_kind: TTypeKind read f_type_kind write set_type_kind;
         property name: string read get_name write set_name;
         constructor Create
            (kind: TTypeKind
            );
         function info: TTypeInfo;
         procedure CheckAssignmentCompatability
            (def: TDefinition   // def may be TConstant or TExpression
            );
            virtual;
         function ContainsQueueVariables: boolean;
            virtual;
         function IsClassSystemType: boolean;
            virtual;
         function IsMonitorSystemType: boolean;
            virtual;
         function IsProcessSystemType: boolean;
            virtual;
         function IsInterruptType: boolean;
            virtual;
         function requires_initialization: boolean;
            virtual;
         function ContainsSystemType: boolean;
         function ReadOnly: boolean;
            virtual;
         function SmallType: boolean;  // true if boolean, char, enum, integer, real or set
            virtual;
         procedure SetMonitorPriorities
            (prio: integer
            );
            virtual;
         function ArrayElementSize (Size, Alignment: cardinal): cardinal;
         function IsOrdinal: boolean;
            virtual;
         function IsInteger: boolean;
            virtual;
         procedure MarkTypeDefinitionAsComplete;
      end;

   TBasicDataTypeKind =
      (ordinal_data_type,
       floating_point_data_type
      );
   TBasicDataType =
      class(TTypeDef)
      private
         f_basic_data_type_kind: TBasicDataTypeKind;
         procedure set_basic_data_type_kind (bdt: TBasicDataTypeKind);
      public
         property basic_data_type_kind: TBasicDataTypeKind read f_basic_data_type_kind write set_basic_data_type_kind;
         constructor Create
            (kind: TBasicDataTypeKind
            );
         function SmallType: boolean;
            override;
      end;

   TFloatingPointDataType =
      class(TBasicDataType)
         var
            size_in_bits: integer;
         constructor Create
            (_size_in_bits: cardinal
            );
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
      end;

   TOrdinalDataType =
      class(TBasicDataType)
         ordinal_kind: TOrdinalBaseType;
         enum_typedef: TEnumType; // valid only if ordinal_kind is ordinal_base_is_enum
         constructor Create
            (kind: TOrdinalBaseType
            );
            overload;
         function IsOrdinal: boolean;
            override;
      private
         mp_initialized: boolean
      end;

   TIntegerDataType =
      class(TOrdinalDataType)
         read_only: boolean;    // true if kernelN type
         constructor CreateSignedOrdinal
            (size_in_bits: cardinal
            );
         constructor CreateUnsignedOrdinal
            (size_in_bits: cardinal
            );
         constructor CreateReadOnly
            (size_in_bits: cardinal
            );
         function ValueInRange
            (value: integer
            ): boolean;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function IsInteger: boolean;
            override;
         function ReadOnly: boolean;
            override;
      end;

   TCharDataType =
      class(TOrdinalDataType)
         size_in_bits: integer;
         constructor Create
            (_size_in_bits: integer
            );
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
      end;

   TBooleanDataType =
      class(TOrdinalDataType)
         size_in_bits: integer;
         constructor Create
            (_size_in_bits: integer
            );
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
      end;

   TEnumType =
      class(TOrdinalDataType)
         enum_type_kind:
            (unknown_enum_type,
             sequential_value_enum_type,
             specified_value_enum_type
            );
         enums:
            array of
               record
                  identifier_idx: TIdentifierIdx;
                  identifier_src_loc: TSourceLocation;
                  ord_src_loc: TSourceLocation; // only used in descendant class TPackedRecordEnumFieldType
                  value: int64
               end;
         constructor CreateFromSourceTokens;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
      end;

   TSubRangeType =
      class(TOrdinalDataType)
         constructor CreateFromSourceTokens;
         constructor CreateIntegerSubrange (min, max: integer);
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function IsInteger: boolean;
            override;
      end;

   TSetType =
      class(TTypeDef)
         typedef_src_loc: TSourceLocation;
         ordinal_kind: TOrdinalBaseType;
         enum_typedef: TEnumType; // valid only if ordinal_kind is ordinal_base_is_enum
         constructor CreateFromSourceTokens;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function SmallType: boolean;
            override;
      end;

   TStringType =
      class(TTypeDef)
         max_length: integer;    // -1 for undimensioned strlen
         function known_strlen: boolean;
         constructor Create (_max_length: integer);
         constructor CreateFromSourceTokens;
         procedure CheckAssignmentCompatability
            (def: TDefinition    // def may be TConstant or TExpression
            );
            override;
      end;

   TQueueType =
      class(TTypeDef)
         constructor Create;
         function ContainsQueueVariables: boolean;
            override;
      end;

   TConstantKind =
      (integer_constant,
       real_constant,
       string_constant,
       enum_constant,
       boolean_constant,
       set_constant
      );

   TConstant =
      class(TDefinition)
      strict private
         variant_constant_record:
            record
               ordinal_value: TMultiPrecisionInteger;  // valid only for constant_kind = integer_constant, enum_constant
               s: string; // only used if kind = string_constant, would be in variant below if strings were allowed in variants
               enum_typedef: TEnumType; // valid only for enum constant or set constant where set_base_type is ordinal_base_is_enum
               case constant_kind: TConstantKind of
                  real_constant:
                     (r: real
                     );
                  set_constant:
                     (sett: TSet256;
                      set_ordinal_base_type: TOrdinalBaseType
                     );
            end;
         // note: delphi seems to have problems setting variant parts directly from property, workaround: use set/get routines
         f_dont_reference_count_enum_typedef: boolean;
         function get_b: boolean;
         procedure set_b (b: boolean);
         function get_constant_kind: TConstantKind;
         procedure set_constant_kind (kind: TConstantKind);
         function get_r: real;
         procedure set_r (r: real);
         function get_s: string;
         procedure set_s (s: string);
         function get_sett: TSet256;
         procedure set_sett (s: TSet256);
         function get_set_ordinal_base_type: TOrdinalBaseType;
         procedure set_set_ordinal_base_type (sob: TOrdinalBaseType);
      public
         property b: boolean read get_b write set_b;
         property r: real read get_r write set_r;
         property ordinal_value: TMultiPrecisionInteger read variant_constant_record.ordinal_value write variant_constant_record.ordinal_value;
         property s: string read get_s write set_s;
         property constant_kind: TConstantKind read get_constant_kind write set_constant_kind;
         property enum_typedef: TEnumType read variant_constant_record.enum_typedef write variant_constant_record.enum_typedef;
         property sett: TSet256 read get_sett write set_sett;
         property set_ordinal_base_type: TOrdinalBaseType read get_set_ordinal_base_type write set_set_ordinal_base_type;
         constructor Create;
         constructor CreateIntegerConstant
            (_i: int64
            );
            overload;
         constructor CreateIntegerConstant
            (_i: TMultiPrecisionInteger
            );
            overload;
         constructor CreateBooleanConstant
            (_b: boolean
            );
         constructor CreateRealConstant
            (_r: real
            );
         constructor CreateStringConstant
            (_s: string
            );
         constructor CreateEnumConstant
            (_definer: TEnumType;
             _enum: integer
            );
         constructor CreateSetConstant
            (_sett: TSet256;
             _ordinal_base_type: TOrdinalBaseType
            );
            overload;
         constructor CreateSetConstant
            (_sett: TSet256;
             _enum_typedef: TEnumType
            );
            overload;
         destructor Destroy;
            override;
         procedure CopyFrom
            (src: TConstant
            );
         function IsSameTypeAs
            (other_const: TConstant
            ): boolean;
         function AsString: string;
         function real_value: real; // not valid for string with length <> 1
         function AsOrdinal: integer;
         function AsByte (byteno: integer): byte;
         procedure dont_reference_count_enum_typedef;
      end;

   TSimpleConstant =
      record
         typedef: TTypeDef;
         dont_refcount_typedef: boolean;
         variant_typedef: TTypeDef;   // only used for overlay padding
         constant: TDefinition;
             // either TConstant or TStructuredConstant (packed array constant + filler0
         path, value: string
      end;
   TSimpleConstantArray = array of TSimpleConstant;
   TStructuredConstantKind = (scSimple, scArray, scRecord, scPackedRecord, scOverlay, scOther);
   TStructuredConstant =
      class(TDefinition)
      private
         linearly_arranged_elements: TSimpleConstantArray;
         function get_const (i: integer): TSimpleConstant;
         procedure add_elements_to_array
            (var arr: TSimpleConstantArray;
             annotation: string
            );
      protected
         procedure set_StructuredConstantKind;
      public
         typedef: TTypeDef;
         typedef_src_loc: TSourceLocation;
         StructuredConstantKind: TStructuredConstantKind;

         // only one of the following will be used depending on StructuredConstantKind
         simple_constant: TConstant;                    // only for if StructuredConstantKind = scSimple
         array_elements: array of TStructuredConstant;  // only for StructuredConstantKind = scArray
         array_element_typedef: TTypeDef;
         record_fields: array of TStructuredConstant;   // only for StructuredConstantKind = scRecord
         packed_record_fields:                          // only for StructuredConstantKind = scPackedRecord
            array of
               record
                  c: TConstant;
                  ctyp: TOrdinalDataType
               end;
         overlay_constant: TStructuredConstant;         // only for StructuredConstantKind = scOoverlay
         overlay_annotation: string;

         property default_anonymous [idx: integer]: TSimpleConstant read get_const; default;   // linearly arranged
         function LengthOfSimpleConstants: integer;
         constructor CreateFromSourceTokens (typ: TTypeDef; typ_src_loc: TSourceLocation);
         constructor CreateFromConstant (_typedef: TTypeDef; _constant: TConstant);
         destructor Destroy;
            override;
      end;
   TStructuredConstantArray = array of TStructuredConstant;

   TDefaultValue =
      // These objects are used only as TTypeDef->TTypeInfo->DefaultValue.  They do not reference
      // count the link back to it's typedef so as to prevent circular reference counts.
      // When the typedef is released, this object will be as well.
      class (TStructuredConstant)
         constructor CreateDefaultValue (typ: TTypeDef);
         destructor Destroy;
            override;
      end;

   EDefinitelyNotASubRange =
      class(Exception)
         src_loc: TSourceLocation;
         constructor Create
            (sl: TSourceLocation
            );
      end;

   TAddressMode =
      (absolute_address_mode,
       system_type_address_mode,
       system_type_indirect_address_mode,
       local_address_mode,
       local_indirect_address_mode
      );

   TParamMode = (ByAddress, ByValue);

   TVariable =
      class(TDefinition)
      private
         faddr: integer;
      protected
         is_anonymous_rom_string: boolean;
         anonymous_rom_string: string;  // nil if not anonymous rom string
         procedure set_addr (addr: integer);
            virtual;
         function get_addr: integer;
            virtual;
      public
         id_idx: TIdentifierIdx;
         typedef: TTypeDef;
         typedef_src_loc, decl_end_src_loc: TSourceLocation;    { src_loc has loc of identifier }
         initial_value: TStructuredConstant;
         descriptor: TVariableDescriptor;
         address_mode: TAddressMode;
         allocated: boolean; // used only for global ioregisters
         interrupt: TVariable;  // used only for interrupt process variables
         interrupt_assigned: boolean;
         context: TDefinition;   // TProgram, TSystemType or TRoutine
         init_statement_called: boolean;  // for system types
         is_ioreg_1bit_param: boolean;
         property address: integer read get_addr write set_addr;
         constructor CreateForLaterDefinition
            (_id_idx: TIdentifierIdx;
             _context: TDefinition
            );
         constructor Create
            (_id_idx: TIdentifierIdx;
             _context: TDefinition;
             _typ: TTypeDef;
             _definition_prefix: TVariableDescriptor;
             _mode: TAddressMode
            );
         constructor CreateAnonymousROMString (s: string);
         constructor CreateCopy (v: TVariable);
         destructor Destroy;
            override;
         function name: string;
         function Assignable: boolean;
         function ParamMode: TParamMode;
      end;

   TExpressionKind =
      (integer_expression,
       real_expression,
       string_expression,
       enum_expression,
       boolean_expression,
       set_expression,
       char_expression,
       record_expression,
       packed_record_expression,
       overlay_expression,
       array_expression,
       system_type_expression
      );

const
   ordinal_expression_kinds = [integer_expression, enum_expression,  boolean_expression, char_expression];

type
   TExpression =
      class(TDefinition)
      private
         f_expression_kind: TExpressionKind;
         procedure set_expression_kind (ek: TExpressionKind);
      protected
         procedure check_set_constant (constant: TConstant; valid_range: TTypeInfo; src_loc: TSourceLocation);
      public
         enum_typedef: TEnumType;  // valid only for enum_expression or set_expression where set_base_type is ordinal_base_is_enum
         set_ordinal_base_type: TOrdinalBaseType; // valid only for set_expression
         typedef: TTypeDef;  // valid for record, array & system type expressions in cpc_core
         property expression_kind: TExpressionKind read f_expression_kind write set_expression_kind;
         function info: TTypeInfo;
         constructor Create;
         function contains_constant: boolean;
            virtual;
         function contains_real_constant: boolean;
            virtual;
         function contains_integer_constant: boolean;
            virtual;
         function constant: TConstant; // invalid unless type is TConstantPrimary
            virtual;
         function ordinal_constant_value: TLargestPascalSupportedInt;  // invalid unless type is TConstantPrimary
            virtual;
         function real_constant_value: real; // invalid unless type is TConstantPrimary
            virtual;
         function boolean_constant_value: boolean;  // invalid unless type is TConstantPrimary
            virtual;
         procedure check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
            virtual;
         procedure set_min_max_for_set_constant;
         procedure copy_expression_info
            (from_expression: TExpression
            );
      end;

function SetByte (constant: TConstant; byteno: cardinal): byte;
function CreateTypeDenoterFromSourceTokens: TTypeDef;
function CreateExpressionFromSourceTokens: TExpression;
procedure check_for_write_access (v: TVariable; src_loc: TSourceLocation);


IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_constant_expression_unit,
   cpc_expressions_unit,
   cpc_target_cpu_unit,
   Math, cpc_types_unit;

function SetByte (constant: TConstant; byteno: cardinal): byte;
   function bit_in_set (bitno: cardinal): boolean;
      begin
         result := ((byteno*8) + bitno) in constant.sett
      end;
   begin
      result := 0;
      if bit_in_set (0) then result := result + 1;
      if bit_in_set (1) then result := result + 2;
      if bit_in_set (2) then result := result + 4;
      if bit_in_set (3) then result := result + 8;
      if bit_in_set (4) then result := result + 16;
      if bit_in_set (5) then result := result + 32;
      if bit_in_set (6) then result := result + 64;
      if bit_in_set (7) then result := result + 128
   end;

//============
//  TTypeInfo
//============

constructor TTypeInfo.Create (_parent: TDefinition);
   begin
      inherited Create (_parent);
      case parent.definition_kind of
         type_definition:
            case TTypeDef(parent).type_kind of
               basic_data_type:
                  range_kind := irOrdinalRange;
               set_type:
                  range_kind := irSetBounds;
               string_type:
                  range_kind := irStringLength;
            else
               range_kind := irNonRange
            end;
         expression_definition:
            range_kind := irNonRange;  // don't know what kind of expression this will be yet
      else
         assert (false)
      end
   end;

constructor TTypeInfo.Create (_info: TTypeInfo);
   begin
      inherited Create (nil);
      range_kind := _info.range_kind;
      if range_kind <> irNonRange then
         begin
            min_value.Assign (_info.min_value);
            max_value.Assign (_info.max_value)
         end
   end;

function TTypeInfo.get_min_value: TMultiPrecisionInteger;
   begin
      if f_min_value = nil then
         f_min_value := TMultiPrecisionInteger.Create (0);
      result := f_min_value
   end;

function TTypeInfo.get_max_value: TMultiPrecisionInteger;
   begin
      if f_max_value = nil then
         f_max_value := TMultiPrecisionInteger.Create (0);
      result := f_max_value
   end;

destructor TTypeInfo.Destroy;
   begin
      min_value.Free;
      max_value.Free;
      f_default_value.Release;
      inherited
   end;

function TTypeInfo.DefaultValue: TStructuredConstant;
   begin
      if f_default_value = nil then
         f_default_value := TDefaultValue.CreateDefaultValue (TTypeDef(parent));
      result := f_default_value
   end;

function TTypeInfo.IntegerRange: TIntegerRange;
   begin
      assert (range_kind = irOrdinalRange);
      if min_value.ge(0) then
         result := irAlwaysNonNegative
      else if max_value.lt(0) then
         result := irAlwaysNegative
      else
         result := irNegativeOrPositive
   end;

function TTypeInfo.Signed: boolean;
   begin
      assert (range_kind = irOrdinalRange);
      result := min_value.lt(0)
   end;

function TTypeInfo.UnSigned: boolean;
   begin
      assert (range_kind = irOrdinalRange);
      result := min_value.ge(0)
   end;

function TTypeInfo.PackedSizeInBits: integer;
   var
      i,j,size: integer;
      prt: TPackedRecordType;
      ovt: TOverlayType;
   begin
      result := 0;  // suppresses compiler warning
      case range_kind of
         irOrdinalRange:
            if Unsigned then
               result := max_value.BitsRequiredForUnsignedValue
            else
               if min_value.eq(-1) and max_value.eq(0) then
                  result := 1
               else
                  result := max(min_value.BitsRequiredForSignedValue,
                                max_value.BitsRequiredForSignedValue
                               );
         irSetBounds:
            ;
         irNonRange:
            begin
               assert ((parent <> nil) and (parent.definition_kind = type_definition));
               case TTypeDef(parent).type_kind of
                  packed_record_type:
                     begin
                        prt := TPackedRecordType(parent);
                        result := 0;
                        for i := 0 to Length(prt.fields)-1 do
                           result := result + TTypeInfo(prt.fields[i].ordtypedef.info).PackedSizeInBits
                     end;
                  overlay_type:
                     begin
                        ovt := TOverlayType(parent);
                        result := 0;
                        for i := 0 to Length(ovt.overlaid_variables)-1 do
                           begin
                              assert (ovt.overlaid_variables[i].typedef.type_kind = packed_record_type);
                              prt := TPackedRecordType(ovt.overlaid_variables[i].typedef);
                              size := 0;
                              for j := 0 to Length(prt.fields)-1 do
                                 size := size + TTypeInfo(prt.fields[j].ordtypedef.info).PackedSizeInBits;
                              if size > result then
                                 result := size
                           end;
                     end;
               else
                  assert (false)
               end
            end;
      else
         assert (false)
      end
   end;

procedure TTypeInfo.Assign (ti: TTypeInfo);
   begin
      range_kind := ti.range_kind;
      min_value.Assign (ti.min_value);
      max_value.Assign (ti.max_value)
   end;

constructor EDefinitelyNotASubRange.Create
   (sl: TSourceLocation
   );
   begin
      src_loc := sl
   end;


//==========
// TTypeDef
//==========

constructor TTypeDef.Create
   (kind: TTypeKind
   );
   begin
      inherited Create(type_definition);
      type_kind := kind;
      if CurrentDefinitionTable = nil then
         defining_scope := -1
      else
         defining_scope := CurrentDefinitionTable.current_scope
   end;

function TTypeDef.info: TTypeInfo;
   begin
      result := TTypeInfo (inherited info)
   end;

procedure TTypeDef.set_type_kind (tk: TTypeKind);
   begin
      f_type_kind := tk
   end;

procedure TTypeDef.set_name (n: string);
   begin
      f_name := n
   end;

function TTypeDef.get_name: string;
   begin
      if f_name = '' then
         begin  // anonymous type
            result := lex.token_string (src_loc, last_src_loc)
         end
      else
         result := f_name
   end;

procedure TTypeDef.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      assert(type_kind in [basic_data_type, set_type, string_type])
   end;

function TTypeDef.ContainsQueueVariables: boolean;
   begin
      result := false
   end;

function TTypeDef.IsClassSystemType: boolean;
   begin
      result := false
   end;

function TTypeDef.IsMonitorSystemType: boolean;
   begin
      result := false
   end;

function TTypeDef.IsProcessSystemType: boolean;
   begin
      result := false
   end;

function TTypeDef.IsInterruptType: boolean;
   begin
      result := false
   end;

function TTypeDef.requires_initialization: boolean;
   begin
      result := false
   end;

function TTypeDef.ContainsSystemType: boolean;
   begin
      result := IsClassSystemType or IsMonitorSystemType or IsProcessSystemType
   end;

function TTypeDef.SmallType: boolean;
   begin
      result := false
   end;

function TTypeDef.ReadOnly: boolean;
   begin
      result := false
   end;

procedure TTypeDef.SetMonitorPriorities
   (prio: integer
   );
   begin
   end;

function TTypeDef.ArrayElementSize (Size, Alignment: cardinal): cardinal;
   begin
      result := Size;
      while result mod Alignment <> 0 do
         result := result + 1
   end;

function TTypeDef.IsOrdinal: boolean;
   begin
      result := false
   end;

function TTypeDef.IsInteger: boolean;
   begin
      result := false
   end;

procedure TTypeDef.MarkTypeDefinitionAsComplete;
   begin
      definition_complete := true
   end;


//=================
//  TBasicDataType
//=================

constructor TBasicDataType.Create
   (kind: TBasicDataTypeKind
   );
   begin
      inherited Create(basic_data_type);
      MarkTypeDefinitionAsComplete;
      basic_data_type_kind := kind
   end;

function TBasicDataType.SmallType: boolean;
   begin
      result := true
   end;

procedure TBasicDataType.set_basic_data_type_kind (bdt: TBasicDataTypeKind);
   begin
      f_basic_data_type_kind := bdt;
      case f_basic_data_type_kind of
         ordinal_data_type:
            info.range_kind := irOrdinalRange;
      else
         info.range_kind := irNonRange
      end
   end;


// =========================
//  TFloatingPointDataType
// =========================

constructor TFloatingPointDataType.Create
   (_size_in_bits: cardinal
   );
   begin
      inherited Create(floating_point_data_type);
      size_in_bits := _size_in_bits
   end;

procedure TFloatingPointDataType.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            if not (TConstant(def).constant_kind in [integer_constant, real_constant]) then
               raise compile_error.Create(err_number_expected, def.src_loc);
         expression_definition:
            begin
               if not (TExpression(def).expression_kind in [integer_expression, real_expression]) then
                  raise compile_error.Create(err_numeric_expression_expected, def.src_loc)
            end;
      else
         assert(false)
      end
   end;


//===================
//  TOrdinalDataType
//===================

constructor TOrdinalDataType.Create
   (kind: TOrdinalBaseType
   );
   begin
      inherited Create(ordinal_data_type);
      ordinal_kind := kind;
      mp_initialized := true
   end;

function TOrdinalDataType.IsOrdinal: boolean;
   begin
      result := true
   end;


//===================
//  TIntegerDataType
//===================

constructor TIntegerDataType.CreateSignedOrdinal
   (size_in_bits: cardinal
   );
   begin
      inherited Create(ordinal_base_is_integer);
      info.min_value.SetMinSignedValue(size_in_bits);
      info.max_value.SetMaxSignedValue(size_in_bits)
   end;

constructor TIntegerDataType.CreateUnsignedOrdinal
   (size_in_bits: cardinal
   );
   begin
      inherited Create(ordinal_base_is_integer);
      info.min_value.AsInteger := 0;
      info.max_value.SetMaxUnsignedValue(size_in_bits)
   end;

constructor TIntegerDataType.CreateReadOnly
   (size_in_bits: cardinal
   );
   begin
      inherited Create(ordinal_base_is_integer);
      info.min_value.AsInteger := 0;
      info.max_value.SetMaxUnsignedValue(size_in_bits);
      read_only := true
   end;

procedure TIntegerDataType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      t: TMultiPrecisionInteger;
   begin
      inherited;
      t := TMultiPrecisionInteger.Create;
      try
         case def.definition_kind of
            constant_definition:
               begin
                  if TConstant(def).constant_kind <> integer_constant then
                     raise compile_error.Create(err_integer_expected, def.src_loc);

                  if info.min_value.gt(TConstant(def).ordinal_value) or info.max_value.lt(TConstant(def).ordinal_value) then
                     raise compile_error.Create(err_value_outside_legal_range, def.src_loc)
               end;
            structured_constant_definition:
               begin
                  assert (false)
               end;
            expression_definition:
               begin
                  if TExpression(def).expression_kind <> integer_expression then
                     raise compile_error.Create(err_integer_expression_expected, def.src_loc);

                  if info.max_value.lt(TExpression(def).info.min_value) then
                     raise compile_error.Create(err_expression_value_outside_legal_range,  def.src_loc);

                  if info.min_value.gt(TExpression(def).info.max_value) then
                     raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
               end;
         else
            assert(false)
         end
      finally
         t.Free
      end
   end;

function TIntegerDataType.ValueInRange
   (value: integer
   ): boolean;
   begin
      result := info.min_value.le(value) and info.max_value.ge(value)
   end;

function TIntegerDataType.IsInteger;
   begin
      result := true
   end;

function TIntegerDataType.ReadOnly: boolean;
   begin
      result := read_only
   end;

// ================
//   TCharDataType
// ================

constructor TCharDataType.Create (_size_in_bits: integer);
   begin
      inherited Create(ordinal_base_is_char);
      size_in_bits := _size_in_bits;
      info.min_value.AsInteger := min_char;
      info.max_value.AsInteger := max_char
   end;

procedure TCharDataType.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            if (TConstant(def).constant_kind <> string_constant)
               or
               (Length(TConstant(def).s) <> 1) then
               raise compile_error.Create(err_char_expected, def.src_loc);
         expression_definition:
            begin
               if (TExpression(def).expression_kind <> char_expression)
               then
                  raise compile_error.Create(err_char_expression_expected, def.src_loc);

               if info.max_value.lt (TExpression(def).info.min_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc);

               if info.min_value.gt (TExpression(def).info.max_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
            end
         else
            assert(false)
      end
   end;


// ===================
//   TBooleanDataType
// ===================

constructor TBooleanDataType.Create (_size_in_bits: integer);
   begin
      inherited Create(ordinal_base_is_bool);
      size_in_bits := _size_in_bits;
      info.min_value.AsInteger := 0;
      info.max_value.AsInteger := 1;
   end;

procedure TBooleanDataType.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            if TConstant(def).constant_kind <> boolean_constant then
               raise compile_error.Create(err_boolean_expected, def.src_loc);
         expression_definition:
            begin
               if TExpression(def).expression_kind <> boolean_expression
               then
                  raise compile_error.Create(err_boolean_expression_expected, def.src_loc);

               if info.max_value.lt (TExpression(def).info.min_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc);

               if info.min_value.gt (TExpression(def).info.max_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
            end;
         else
            assert(false)
      end
   end;


//============
//  TEnumType
//============

constructor TEnumType.CreateFromSourceTokens;
   var
      id_idx: TIdentifierIdx;
      enum_idx, i: integer;
      cexpr: TCExpression;
      enum_const: TConstant;
      prev_specified_value: int64;
   begin
      inherited Create(ordinal_base_is_enum);
      enum_type_kind := unknown_enum_type;
      enum_typedef := Self;
      info.min_value.AsInteger := maxint;
      info.max_value.AsInteger := -maxint;

      assert(lex.token_is_symbol(sym_left_parenthesis));
      lex.advance_token;

      enum_idx := -1;
      prev_specified_value := 0;  // not used, but suppresses compiler warning
      repeat // once per enum constant

         if (enum_type_kind = sequential_value_enum_type)
            and
            (enum_idx >= 255) then
            raise compile_error.Create(err_more_than_256_constants_in_enum);

         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);

         // check for (a,b,a)
         id_idx := lex.token.identifier_idx;
         for i := 0 to enum_idx do
            if enums[i].identifier_idx = id_idx then
               raise compile_error.Create (err_identifier_already_defined);

         if CurrentDefinitionTable.DefinedInCurrentScope (id_idx) then
            raise compile_error.Create (err_identifier_already_defined);

         enum_idx := enum_idx + 1;
         SetLength(enums, enum_idx + 1);
         enums[enum_idx].identifier_idx := id_idx;
         enums[enum_idx].identifier_src_loc := lex.token.src_loc;
         lex.advance_token;

         if enum_type_kind = unknown_enum_type then
            if lex.token_is_symbol([sym_comma, sym_right_parenthesis]) then
               enum_type_kind := sequential_value_enum_type
            else if lex.token_is_symbol(sym_equals) then
               enum_type_kind := specified_value_enum_type
            else
               raise compile_error.Create(err_invalid_enumeration_definition);

         case enum_type_kind of
            sequential_value_enum_type:
               enums[enum_idx].value := enum_idx;
            specified_value_enum_type:
               begin
                  if not lex.token_is_symbol(sym_equals) then
                     raise compile_error.Create(err_equals_expected);
                  lex.advance_token;

                  cexpr := TCExpression.CreateFromSourceTokens;
                  try
                     if cexpr.constant_kind <> integer_constant then
                        raise compile_error.Create(err_integer_expression_expected);
                     enums[enum_idx].value := cexpr.ordinal_value.AsInteger;
                     if (enum_idx > 0) and (prev_specified_value >= enums[enum_idx].value) then
                        raise compile_error.Create (err_enumerated_value_must_be_greater_than_previous, cexpr.src_loc);
                  finally
                     cexpr.Release
                  end;
                  prev_specified_value := enums[enum_idx].value
               end
            else
               assert(false)
         end;

         if info.min_value.gt (enums[enum_idx].value) then
            info.min_value.AsInteger := enums[enum_idx].value;

         if info.max_value.lt (enums[enum_idx].value) then
            info.max_value.AsInteger := enums[enum_idx].value;

         if not lex.token_is_symbol(sym_right_parenthesis) then
            begin
               if not lex.token_is_symbol(sym_comma) then
                  raise compile_error.Create(err_comma_expected);
               lex.advance_token
            end
      until lex.token_is_symbol(sym_right_parenthesis);
      lex.advance_token;
      // there must be no compile_error exceptions after this point

      try
         for enum_idx := 0 to Length(enums)-1 do
            begin
               enum_const := TConstant.CreateEnumConstant (Self, enums[enum_idx].value);
               CurrentDefinitionTable.DefineForCurrentScope(enums[enum_idx].identifier_idx, enum_const, enums[enum_idx].identifier_src_loc);
               enum_const.Release
            end
      except
         on e: compile_error do
            assert (false, 'check for this error in above loop: ' + e.Message)
      end
   end;

procedure TEnumType.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            begin
               if TConstant(def).constant_kind <> enum_constant then
                  raise compile_error.Create(err_enum_value_expected, def.src_loc);
               if TConstant(def).enum_typedef <> Self then
                  raise compile_error.Create(err_wrong_enum_type, def.src_loc);
            end;
         expression_definition:
            begin
               if (TExpression(def).expression_kind <> enum_expression)
               then
                  raise compile_error.Create(err_enum_expression_expected,
                  def.src_loc);
               if TExpression(def).enum_typedef <> enum_typedef then
                  raise compile_error.Create(err_wrong_enum_type, def.src_loc);
               if info.max_value.lt (TExpression(def).info.min_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc);
               if info.min_value.gt (TExpression(def).info.max_value) then
                  raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
            end;
         else
            assert(false)
      end
   end;


//================
// TSubRangeType
//================

constructor TSubRangeType.CreateFromSourceTokens;
   var
      first_of_subrange, last_of_subrange: TConstant;
      first_of_subrange_src_loc, last_of_subrange_src_loc: TSourceLocation;
      operator_location: TSourceLocation;
   begin
      // don't call inherited Create here, the constructor will be called later
      definition_kind := type_definition;
      src_loc := lex.token.src_loc;

      first_of_subrange := nil;
      last_of_subrange := nil;
      try
         first_of_subrange_src_loc := lex.token.src_loc;
         try
            first_of_subrange := TCExpression.CreateFromSourceTokens
         except
            on e: compile_error
            do raise EDefinitelyNotASubRange.Create(first_of_subrange_src_loc)
         end;

         operator_location := lex.token.src_loc;
         if not (lex.token_is_symbol(sym_dot_dot)) then
            raise compile_error.Create(err_dot_dot_expected);
         lex.advance_token;

         last_of_subrange_src_loc := lex.token.src_loc;
         last_of_subrange := TCExpression.CreateFromSourceTokens;

         if first_of_subrange.constant_kind <> last_of_subrange.constant_kind
         then
            raise compile_error.Create(err_subrange_first_and_last_must_be_of_same_type, operator_location);

         if (first_of_subrange.constant_kind = enum_constant)
            and
            (first_of_subrange.enum_typedef <> last_of_subrange.enum_typedef) then
            raise compile_error.Create(err_subrange_first_and_last_must_be_of_same_type, operator_location);

         if (first_of_subrange.constant_kind = string_constant)
            and
            (Length(first_of_subrange.s) <> 1) then
            raise compile_error.Create(err_subrange_must_be_single_char, first_of_subrange_src_loc);

         if (last_of_subrange.constant_kind = string_constant)
            and
            (Length(last_of_subrange.s) <> 1) then
            raise compile_error.Create(err_subrange_must_be_single_char, last_of_subrange_src_loc);

         case first_of_subrange.constant_kind of
            integer_constant:
               begin
                  inherited Create(ordinal_base_is_integer);
                  info.min_value.Assign (first_of_subrange.ordinal_value);
                  info.max_value.Assign (last_of_subrange.ordinal_value)
               end;
            enum_constant:
               begin
                  inherited Create(ordinal_base_is_enum);
                  info.min_value.Assign (first_of_subrange.ordinal_value);
                  info.max_value.Assign (last_of_subrange.ordinal_value);
                  assert(first_of_subrange.enum_typedef is TEnumType);
                  enum_typedef := TEnumType(first_of_subrange.enum_typedef)
               end;
            boolean_constant:
               begin
                  inherited Create(ordinal_base_is_bool);
                  info.min_value.AsInteger := ord(first_of_subrange.b);
                  info.max_value.AsInteger := ord(last_of_subrange.b)
               end;
            string_constant:
               begin
                  inherited Create(ordinal_base_is_char);
                  info.min_value.AsInteger := ord(first_of_subrange.s[1]);
                  info.max_value.AsInteger := ord(last_of_subrange.s[1])
               end;
         else
            raise compile_error.Create(err_subrange_types_must_be_ordinal, first_of_subrange_src_loc)
         end;

         if info.min_value.gt (info.max_value) then
            raise compile_error.Create(err_first_subrange_value_greater_than_last_subrange_value, operator_location)
      finally
         first_of_subrange.Release;
         last_of_subrange.Release
      end
   end;

constructor TSubRangeType.CreateIntegerSubrange (min, max: integer);
   begin
      inherited Create(ordinal_base_is_integer);
      info.min_value.AsInteger := min;
      info.max_value.AsInteger := max
   end;

function TSubRangeType.IsInteger: boolean;
   begin
      result := ordinal_kind = ordinal_base_is_integer
   end;

procedure TSubRangeType.CheckAssignmentCompatability
   (def: TDefinition
   );
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            case ordinal_kind of
               ordinal_base_is_integer:
                  begin
                     if TConstant(def).constant_kind <> integer_constant then
                        raise compile_error.Create(err_integer_expected, def.src_loc);
                     if info.min_value.gt (TConstant(def).ordinal_value) or info.max_value.lt (TConstant(def).ordinal_value) then
                        raise compile_error.Create(err_constant_value_outside_subrange, def.src_loc)
                  end;
               ordinal_base_is_char:
                  begin
                     if (TConstant(def).constant_kind <> string_constant) or (Length(TConstant(def).s) <> 1) then
                        raise compile_error.Create(err_char_expected, def.src_loc);
                     if info.min_value.gt (ord(TConstant(def).s[1])) or info.max_value.lt (ord(TConstant(def).s[1])) then
                        raise compile_error.Create(err_constant_value_outside_subrange, def.src_loc)
                  end;
               ordinal_base_is_bool:
                  begin
                     if TConstant(def).constant_kind <> boolean_constant then
                        raise compile_error.Create(err_boolean_expected, def.src_loc);
                     if info.min_value.gt (ord(TConstant(def).b)) or info.max_value.lt (ord(TConstant(def).b)) then
                        raise  compile_error.Create(err_constant_value_outside_subrange, def.src_loc)
                  end;
               ordinal_base_is_enum:
                  begin
                     if TConstant(def).constant_kind <> enum_constant then
                        raise compile_error.Create(err_enum_value_expected, def.src_loc);
                     if TConstant(def).enum_typedef <> enum_typedef then
                        raise compile_error.Create(err_wrong_enum_type, def.src_loc);
                     if info.min_value.gt (TConstant(def).ordinal_value) or info.max_value.lt (TConstant(def).ordinal_value) then
                        raise compile_error.Create(err_constant_value_outside_subrange, def.src_loc)
                  end;
            else
               assert(false)
            end;
         expression_definition:
            case ordinal_kind of
               ordinal_base_is_integer:
                  begin
                     if TExpression(def).expression_kind <> integer_expression then
                        raise compile_error.Create(err_integer_expression_expected, def.src_loc);
                     if info.max_value.lt (TExpression(def).info.min_value) or info.min_value.gt (TExpression(def).info.max_value) then
                        raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
                  end;
               ordinal_base_is_char:
                  begin
                     if (TExpression(def).expression_kind <> char_expression) then
                        raise compile_error.Create(err_char_expression_expected, def.src_loc);
                     if info.max_value.lt (TExpression(def).info.min_value) or info.min_value.gt (TExpression(def).info.max_value) then
                        raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
                  end;
               ordinal_base_is_bool:
                  begin
                     if TExpression(def).expression_kind <> boolean_expression then
                        raise compile_error.Create(err_boolean_expression_expected, def.src_loc);
                     if info.max_value.lt (TExpression(def).info.min_value) or info.min_value.gt (TExpression(def).info.max_value) then
                        raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
                  end;
               ordinal_base_is_enum:
                  begin
                     if (TExpression(def).expression_kind <>  enum_expression) then
                        raise compile_error.Create(err_enum_expression_expected, def.src_loc);
                     if TExpression(def).enum_typedef <> enum_typedef
                     then
                        raise compile_error.Create(err_wrong_enum_type, def.src_loc);
                     if info.max_value.lt (TExpression(def).info.min_value) or info.min_value.gt (TExpression(def).info.max_value) then
                        raise compile_error.Create(err_expression_value_outside_legal_range, def.src_loc)
                  end;
            else
               assert(false)
            end;
      else
         assert(false)
      end
   end;


// ===========
// TSetType
// ===========

constructor TSetType.CreateFromSourceTokens;
   var
      typedef: TTypeDef;
   begin
      inherited Create(set_type);
      assert(lex.token_is_reserved_word(rw_set));
      src_loc := lex.token.src_loc;
      lex.advance_token;

      if not lex.token_is_reserved_word(rw_of) then
         raise compile_error.Create(err_of_expected);
      lex.advance_token;

      typedef_src_loc := lex.token.src_loc;
      typedef := CreateTypeDenoterFromSourceTokens;
      try
         if (typedef.type_kind <> basic_data_type)
            or
            (TBasicDataType(typedef).basic_data_type_kind <> ordinal_data_type) then
            raise compile_error.Create(err_ordinal_type_expected, typedef_src_loc);

         ordinal_kind := TOrdinalDataType(typedef).ordinal_kind;
         enum_typedef := TOrdinalDataType(typedef).enum_typedef;
         info.Assign (typedef.info);
      finally
         typedef.Release
      end;

      if info.min_value.lt(min_set) then
         raise compile_error.Create(err_invalid_low_set_range_value, typedef_src_loc);

      if info.max_value.gt(max_set) then
         raise compile_error.Create(err_invalid_high_set_range_value, typedef_src_loc);

      MarkTypeDefinitionAsComplete
   end;

procedure TSetType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      i: integer;
      exp: TExpression;
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            begin
               if TConstant(def).constant_kind <> set_constant then
                  raise compile_error.Create(err_set_expected, def.src_loc);
               if ordinal_kind <> TConstant(def).set_ordinal_base_type then
                  raise compile_error.Create(err_set_constant_is_of_wrong_type, def.src_loc);
               if (ordinal_kind = ordinal_base_is_enum)
                  and (enum_typedef <> TConstant(def).enum_typedef) then
                  raise compile_error.Create(err_set_constant_is_of_wrong_type, def.src_loc);
               for i := 0 to max_set do
                  if i in TConstant(def).sett then
                     if info.min_value.gt(i) or info.max_value.lt(i) then
                        raise compile_error.Create(err_set_constant_has_members_outside_specified_range, def.src_loc);
            end;
         expression_definition:
            begin
               exp := TExpression(def);
               if exp.expression_kind <> set_expression then
                  raise compile_error.Create(err_set_expression_expected, exp.src_loc);
               if exp.set_ordinal_base_type <> empty_set_ordinal_base_unknown then
                  begin
                     if exp.set_ordinal_base_type <> ordinal_kind then
                        raise compile_error.Create(err_set_is_of_wrong_type, def.src_loc);
                     if (ordinal_kind = ordinal_base_is_enum) and (enum_typedef <> exp.enum_typedef) then
                        raise compile_error.Create(err_set_is_of_wrong_type, def.src_loc);
                     exp.check_set_constants_range (info, def.src_loc)
                  end
            end
         else
            assert(false)
      end
   end;

function TSetType.SmallType: boolean;
   begin
      result := true
   end;

// ==============
// TStringType

constructor TStringType.Create (_max_length: integer);
   begin
      inherited Create(string_type);
      max_length := _max_length;
      if max_length = -1 then
         name := 'string'
      else
         name := 'string[' + IntToStr(max_length) + ']';
      MarkTypeDefinitionAsComplete
   end;

constructor TStringType.CreateFromSourceTokens;
   var
      constant: TConstant;
   begin
      inherited Create(string_type);
      src_loc := lex.token.src_loc;
      assert(lex.token_is_reserved_word(rw_string));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_bracket) then
         begin  // var string
            name := 'string';
            max_length := -1
         end
      else
         begin
            lex.advance_token;

            constant := TCExpression.CreateFromSourceTokens;
            try
               if constant.constant_kind <> integer_constant then
                  raise compile_error.Create(err_integer_expression_expected, constant.src_loc);
               if constant.ordinal_value.eq (0) then
                  raise compile_error.Create(err_zero_length_string_not_allowed, constant.src_loc);
               if constant.ordinal_value.lt (0) then
                  raise compile_error.Create(err_string_length_must_be_non_negative, constant.src_loc);
               if constant.ordinal_value.gt (255) then
                  raise compile_error.Create(err_string_length_must_be_less_than_256, constant.src_loc);
               max_length := constant.ordinal_value.AsInteger;
               name := 'string[' + IntToStr(max_length) + ']'
            finally
               constant.Release
            end;

            if not lex.token_is_symbol(sym_right_bracket) then
               raise compile_error.Create(err_right_bracket_expected);
            lex.advance_token
         end;

      MarkTypeDefinitionAsComplete
   end;

function TStringType.known_strlen: boolean;
   begin
      result := max_length > -1
   end;

procedure TStringType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      compatable: boolean;
      i: integer;
   begin
      inherited;
      case def.definition_kind of
         constant_definition:
            begin
               if TConstant(def).constant_kind <> string_constant then
                  raise compile_error.Create(err_string_expected, def.src_loc);
               if Length(TConstant(def).s) > max_length then
                  raise compile_error.Create(err_string_exceeds_max_length, def.src_loc)
            end;
         expression_definition:
            case TExpression(def).expression_kind of
               char_expression:
                  if max_length = 0 then  // can't put a char into a zero-len string!
                     raise compile_error.Create(err_string_exceeds_max_length, def.src_loc);
               string_expression:
                  if (max_length > -1)
                     and
                     (TExpression(def).info.min_value.AsInteger > max_length) then
                     raise compile_error.Create(err_string_exceeds_max_length, def.src_loc);
               overlay_expression:
                  begin
                     compatable := false;
                     for i := 0 to Length(TOverlayType(TExpression(def).typedef).overlaid_variables) - 1 do
                        with TOverlayType(TExpression(def).typedef).overlaid_variables[i] do
                           if anonymous and (typedef.type_kind = string_type) then
                              compatable := true;
                     if not compatable then
                        raise compile_error.Create(err_string_expression_expected, def.src_loc);
                  end;
            else
               raise compile_error.Create(err_string_expression_expected,  def.src_loc)
            end;
         else
            assert(false)
      end
   end;


// =============
//  TQueueType

constructor TQueueType.Create;
   begin
      inherited Create(queue_type);
      MarkTypeDefinitionAsComplete
   end;

function TQueueType.ContainsQueueVariables: boolean;
   begin
      result := true
   end;


// ============
// TConstant
// ============

function TConstant.IsSameTypeAs
   (other_const: TConstant
   ): boolean;
   begin
      if constant_kind <> other_const.constant_kind then
         result := false
      else // kind = other_const.kind
         if constant_kind = enum_constant then
            result := enum_typedef = other_const.enum_typedef
         else if constant_kind = set_constant then
            result := (set_ordinal_base_type = empty_set_ordinal_base_unknown)
                      or
                      (other_const.set_ordinal_base_type = empty_set_ordinal_base_unknown)
                      or
                      ((set_ordinal_base_type <> ordinal_base_is_enum)
                       and
                       (set_ordinal_base_type = other_const.set_ordinal_base_type)
                      )
                      or
                      ((set_ordinal_base_type = ordinal_base_is_enum)
                       and
                       (other_const.set_ordinal_base_type = ordinal_base_is_enum)
                       and
                       (enum_typedef = other_const.enum_typedef)
                      )
         else
            result := true
   end;

function TConstant.get_b: boolean;
   begin
      assert (constant_kind = boolean_constant);
      assert (ordinal_value.AsInteger >= 0);
      assert (ordinal_value.AsInteger <= 1);
      result := boolean (ordinal_value.AsInteger)
   end;

procedure TConstant.set_b (b: boolean);
   begin
      assert (constant_kind = boolean_constant);
      ordinal_value.AsInteger := ord(b)
   end;

function TConstant.get_constant_kind: TConstantKind;
   begin
      result := variant_constant_record.constant_kind
   end;

procedure TConstant.set_constant_kind (kind: TConstantKind);
   begin
      variant_constant_record.constant_kind := kind
   end;

function TConstant.get_r: real;
   begin
      result := 0;  // to suppress compiler warning
      case constant_kind of
         real_constant:
            result := variant_constant_record.r;
         integer_constant:
            result := variant_constant_record.ordinal_value.AsInteger;
      else
         assert (false)
      end;
   end;

procedure TConstant.set_r (r: real);
   begin
      constant_kind := real_constant;
      variant_constant_record.r := r
   end;

function TConstant.get_s: string;
   begin
      assert (constant_kind = string_constant);
      result := variant_constant_record.s
   end;

procedure TConstant.set_s (s: string);
   begin
      constant_kind := string_constant;
      variant_constant_record.s := s;
      if Length(variant_constant_record.s) = 1 then
         ordinal_value.AsInteger := ord(variant_constant_record.s[1])
   end;

function TConstant.get_sett: TSet256;
   begin
      assert (constant_kind = set_constant);
      result := variant_constant_record.sett
   end;

procedure TConstant.set_sett (s: TSet256);
   begin
      constant_kind := set_constant;
      variant_constant_record.sett := s
   end;

function TConstant.get_set_ordinal_base_type: TOrdinalBaseType;
   begin
      result := variant_constant_record.set_ordinal_base_type
   end;

procedure TConstant.set_set_ordinal_base_type (sob: TOrdinalBaseType);
   begin
      variant_constant_record.set_ordinal_base_type := sob
   end;

constructor TConstant.Create;
   begin
      inherited Create(constant_definition);
      ordinal_value := TMultiPrecisionInteger.Create
   end;

constructor TConstant.CreateIntegerConstant
   (_i: int64
   );
   begin
      Create;
      constant_kind := integer_constant;
      ordinal_value.AsInteger := _i
   end;

constructor TConstant.CreateIntegerConstant
   (_i: TMultiPrecisionInteger
   );
   begin
      Create;
      constant_kind := integer_constant;
      ordinal_value.Assign (_i)
   end;

constructor TConstant.CreateBooleanConstant
   (_b: boolean
   );
   begin
      Create;
      constant_kind := boolean_constant;
      b := _b
   end;

constructor TConstant.CreateRealConstant
   (_r: real
   );
   begin
      Create;
      constant_kind := real_constant;
      r := _r
   end;

constructor TConstant.CreateStringConstant
   (_s: string
   );
   begin
      Create;
      constant_kind := string_constant;
      s := _s
   end;

constructor TConstant.CreateEnumConstant
   (_definer: TEnumType;
    _enum: integer
   );
   begin
      Create;
      constant_kind := enum_constant;
      enum_typedef := _definer;
      enum_typedef.AddRef;
      ordinal_value.AsInteger := _enum
   end;

constructor TConstant.CreateSetConstant
   (_sett: TSet256;
    _ordinal_base_type: TOrdinalBaseType
   );
   begin
      Create;
      assert(_ordinal_base_type <> ordinal_base_is_enum);
      constant_kind := set_constant;
      set_ordinal_base_type := _ordinal_base_type;
      sett := _sett
   end;

constructor TConstant.CreateSetConstant
   (_sett: TSet256;
    _enum_typedef: TEnumType
   );
   begin
      Create;
      constant_kind := set_constant;
      set_ordinal_base_type := ordinal_base_is_enum;
      enum_typedef := _enum_typedef;
      enum_typedef.AddRef;
      sett := _sett
   end;

destructor TConstant.Destroy;
   begin
      if not f_dont_reference_count_enum_typedef then
         enum_typedef.Release;
      ordinal_value.Free;
      inherited
   end;

procedure TConstant.CopyFrom
   (src: TConstant
   );
   begin
      constant_kind := src.constant_kind;
      case src.constant_kind of
         integer_constant:
            ordinal_value.Assign (src.ordinal_value);
         real_constant:
            r := src.r;
         string_constant:
            s := src.s;
         enum_constant:
            begin
               ordinal_value.Assign (src.ordinal_value);
               enum_typedef := src.enum_typedef;
               enum_typedef.AddRef
            end;
         boolean_constant:
            b := src.b;
         set_constant:
            begin
               sett := src.sett;
               set_ordinal_base_type := src.set_ordinal_base_type;
               if src.enum_typedef <> nil then
                  begin
                     enum_typedef := src.enum_typedef;
                     enum_typedef.AddRef
                  end
            end
         else
            assert(false)
      end;
   end;

function TConstant.AsOrdinal: integer;
   begin
      result := 0; // suppress compiler warning
      case constant_kind of
         integer_constant,
         enum_constant:
            result := integer(ordinal_value.AsInteger);
         string_constant:
            begin
               assert(Length(s) = 1);
               result := ord(s[1])
            end;
         boolean_constant:
            result := ord(b)
      else
         assert(false)
      end
   end;

function TConstant.AsByte (byteno: integer): byte;
   begin
      result := 0;  // suppress compiler warning
      case constant_kind of
         enum_constant,
         integer_constant:
            result := ordinal_value.AsByte(byteno);
         real_constant:
            assert (false);   // shouldn't be here - core compiler doesn't know about target cpu real binary format...
         string_constant:     // this function is valid for char constants, not string constants
            begin
               assert (Length(s) = 1);
               if byteno = 0 then
                  result := byte(s[1])
               else
                  result := 0
            end;
         boolean_constant:
            if byteno = 0 then
               result := ord(b)
            else
               result := 0;
         set_constant:
            result := SetByte (self, byteno);
      else
         assert (false)
      end
   end;

function TConstant.real_value: real;
   // not valid for sets or string with length <> 1
   begin
      result := 0; // suppress compiler warning
      case constant_kind of
         integer_constant,
         enum_constant:
            result := ordinal_value.AsReal;
         real_constant:
            result := r;
         string_constant:
            begin
               assert(Length(s) = 1);
               result := ord(s[1])
            end;
         boolean_constant:
            result := ord(b)
         else
            assert(false)
      end;
   end;

function TConstant.AsString: string;
   var
      i: integer;
      comma_needed: boolean;
   begin
      case constant_kind of
         integer_constant:
            result := IntToStr(ordinal_value.AsInteger);
         real_constant:
            result := format('%e', [r]);
         string_constant:
            result := '''' + s + '''';
         enum_constant:
            begin
               result := '';
               for i := 0 to Length(enum_typedef.enums)-1 do
                  if ordinal_value.AsInteger = enum_typedef.enums[i].value then
                     result := lex.identifiers[enum_typedef.enums[i].identifier_idx];
               assert (result <> '')
            end;
         boolean_constant:
            if b then
               result := 'true'
            else
               result := 'false';
         set_constant:
            begin
               result := '[';
               comma_needed := false;
               for i := 0 to max_set do
                  if i in sett then
                     begin
                        if comma_needed then
                           result := result + ',';
                        case set_ordinal_base_type of
                           ordinal_base_is_integer:
                              result := result + IntToStr(i);
                           ordinal_base_is_char:
                              if non_printable_char(chr(i)) then
                                 result := result + 'chr(' + IntToStr(i) + ')'
                              else
                                 result := result + '''' + chr(i) + '''';
                           ordinal_base_is_bool:
                              case i of
                                 0: result := result + 'false';
                                 1: result := result + 'true'
                                 else
                                    assert(false)
                              end;
                           ordinal_base_is_enum:
                              result := result + lex.identifiers[enum_typedef.enums[i].identifier_idx]
                           else
                              assert(false)
                        end;
                        comma_needed := true
                     end;
               result := result + ']'
            end
         else
            assert(false)
      end
   end;

procedure TConstant.dont_reference_count_enum_typedef;
   begin
      case constant_kind of
         enum_constant:
            {ok} ;
         set_constant:
            assert (set_ordinal_base_type = ordinal_base_is_enum);
      else
         assert (false)
      end;
      f_dont_reference_count_enum_typedef := true;
      enum_typedef.Release
   end;


// ======================
// TStructuredConstant
// ======================

constructor TStructuredConstant.CreateFromConstant (_typedef: TTypeDef; _constant: TConstant);
   begin
      inherited Create (structured_constant_definition);
      typedef := _typedef;
      set_StructuredConstantKind;
      typedef.AddRef;
      simple_constant := _constant;
      simple_constant.AddRef
   end;

constructor TStructuredConstant.CreateFromSourceTokens
   (typ: TTypeDef;
    typ_src_loc: TSourceLocation
   );

   procedure handle_simple_constant;
      var
         cexpr: Tcexpression;
      begin
         cexpr := Tcexpression.CreateFromSourceTokens;
         try
            typedef.CheckAssignmentCompatability(cexpr);
            simple_constant := cexpr;
            simple_constant.AddRef
         finally
            cexpr.Release
         end;
         // if cepxr is an integer but type is real, convert to real constant
         if (typedef.type_kind = basic_data_type)
            and
            (TBasicDataType(typedef).basic_data_type_kind = floating_point_data_type)
            and
            (TConstant(simple_constant).constant_kind = integer_constant) then
            begin
               TConstant(simple_constant).r := TConstant(simple_constant).real_value;
               TConstant(simple_constant).constant_kind := real_constant
            end
      end;

   procedure handle_array_structured_constant;
      var
         arrdef: TArrayType;
         index: TConstant;
         index_src_loc: TSourceLocation;
         i: integer;
      begin
         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         arrdef := TArrayType(typedef);

         array_element_typedef := arrdef.element_typedef;
         array_element_typedef.AddRef;

         try
            SetLength(array_elements, arrdef.index_typedef.info.max_value.AsInteger - arrdef.index_typedef.info.min_value.AsInteger + 1);

            for i := arrdef.index_typedef.info.min_value.AsInteger to arrdef.index_typedef.info.max_value.AsInteger do
               begin
                  if not lex.token_is_symbol(sym_left_bracket) then
                     raise compile_error.Create(err_left_bracket_expected);
                  lex.advance_token;

                  // TODO - check index value
                  index_src_loc := lex.token.src_loc;
                  index := Tcexpression.CreateFromSourceTokens;
                  try
                     case arrdef.index_typedef.ordinal_kind of
                        ordinal_base_is_integer:
                           begin
                              if index.constant_kind <> integer_constant then
                                 raise compile_error.Create(err_index_type_not_compatable_with_array_index_type_definition, index_src_loc);
                              if index.ordinal_value.ne(i) then
                                 raise compile_error.Create(format(err_out_of_order_index, [IntToStr(i)]), index_src_loc)
                           end;
                        ordinal_base_is_char:
                           begin
                              if (index.constant_kind <> string_constant)
                                 or
                                 (Length(index.s) <> 1) then
                                 raise compile_error.Create(err_index_type_not_compatable_with_array_index_type_definition, index_src_loc);
                              if ord(index.s[1]) <> i then
                                 if non_printable_char(chr(i)) then
                                    raise compile_error.Create(format(err_out_of_order_index, [format('chr(%d)', [i])]), index_src_loc)
                                 else
                                    raise compile_error.Create(format(err_out_of_order_index, ['''' + chr(i) + '''']), index_src_loc)
                           end;
                        ordinal_base_is_bool:
                           begin
                              if index.constant_kind <> boolean_constant then
                                 raise compile_error.Create(err_index_type_not_compatable_with_array_index_type_definition, index_src_loc);
                              if ord(index.b) <> i then
                                 case i of
                                    0:
                                       raise compile_error.Create(format(err_out_of_order_index, ['false']), index_src_loc);
                                    1:
                                       raise compile_error.Create(format(err_out_of_order_index, ['true']), index_src_loc)
                                 else
                                    assert(false)
                                 end
                           end;
                        ordinal_base_is_enum:
                           begin
                              if (index.constant_kind <> enum_constant)
                                 or
                                 (index.enum_typedef <> arrdef.index_typedef.enum_typedef) then
                                 raise compile_error.Create(err_index_type_not_compatable_with_array_index_type_definition, index_src_loc);
                              if index.ordinal_value.ne (i) then
                                 raise compile_error.Create(format(err_out_of_order_index, [lex.identifiers[arrdef.index_typedef.enum_typedef.enums[i].identifier_idx]]), index_src_loc)
                           end;
                     else
                        assert(false)
                     end
                  finally
                     index.Release
                  end;

                  if not lex.token_is_symbol(sym_right_bracket) then
                     raise compile_error.Create(err_right_bracket_expected);
                  lex.advance_token;

                  if not lex.token_is_symbol(sym_equals) then
                     raise compile_error.Create(err_equals_expected);
                  lex.advance_token;

                  array_elements[i - arrdef.index_typedef.info.min_value.AsInteger] :=
                     TStructuredConstant.CreateFromSourceTokens(arrdef.element_typedef, arrdef.element_typedef_src_loc);

                  if i < arrdef.index_typedef.info.max_value.AsInteger then
                     begin
                        if not lex.token_is_symbol(sym_comma) then
                           raise compile_error.Create(err_comma_expected);
                        lex.advance_token
                     end
               end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         except
            on e: compile_error do
               begin
                  array_element_typedef.Release;
                  array_element_typedef := nil;
                  for i := 0 to Length(array_elements)-1 do
                     array_elements[i].Release;
                  SetLength (array_elements, 0);
                  raise
               end
         end
      end;   // handle_array_structured_constant

   procedure handle_record_structured_constant;
      var
         recdef: TRecordType;
         i,j: integer;
         c: TConstant;
         all_remaining_fields_are_anonymous: boolean;
      begin
         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         try
            recdef := TRecordType(typedef);
            SetLength(record_fields, Length(recdef.fields));
            for i := 0 to Length(recdef.fields) - 1 do
               if recdef.fields[i].kind = anonymous_field then
                  begin
                     c := TConstant.CreateIntegerConstant(0);
                     record_fields[i] := TStructuredConstant.CreateFromConstant (recdef.fields[i].typedef, c);
                     c.Release
                  end
               else
                  begin
                     if lex.token.identifier_idx <> recdef.fields[i].identifier_idx
                     then
                        raise compile_error.Create(format(err_specific_field_identifer_expected, [lex.identifiers[recdef.fields[i].identifier_idx]]));
                     lex.advance_token;

                     if not lex.token_is_symbol(sym_equals) then
                        raise compile_error.Create(err_equals_expected);
                     lex.advance_token;

                     record_fields[i] := TStructuredConstant.CreateFromSourceTokens(recdef.fields[i].typedef, recdef.fields[i].typedef_src_loc);

                     all_remaining_fields_are_anonymous := true;
                     for j := i+1 to Length(recdef.fields) - 1 do
                        if recdef.fields[j].kind = normal_field then
                           all_remaining_fields_are_anonymous := false;
                     if (i < Length(recdef.fields) - 1)
                        and
                        (not all_remaining_fields_are_anonymous)
                     then
                        begin
                           if not lex.token_is_symbol(sym_comma) then
                              raise compile_error.Create(err_comma_expected);
                           lex.advance_token
                        end
                  end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         except
            on e: compile_error do
               begin
                  for i := 0 to Length(record_fields)-1 do
                     record_fields[i].Release;
                  SetLength (record_fields, 0);
                  raise
               end
         end
      end;    // handle_record_structured_constant

   procedure handle_packed_record_structured_constant;
      var
         precdef: TPackedRecordType;
         i,j: integer;
         cexpr: Tcexpression;
         all_remaining_fields_are_anonymous: boolean;
      begin
         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         try
            precdef := TPackedRecordType(typedef);
            SetLength(packed_record_fields, Length(precdef.fields));
            for i := 0 to Length(precdef.fields) - 1 do
               if precdef.fields[i].kind = anonymous_packed_field then
                  begin   // anonymous field - set it to 0
                     packed_record_fields[i].c := Tcexpression.CreateIntegerConstant (0);
                     packed_record_fields[i].ctyp := precdef.fields[i].ordtypedef;
                     packed_record_fields[i].ctyp.AddRef
                  end
               else
                  begin
                     if lex.token.identifier_idx <> precdef.fields[i].identifier_idx then
                        raise compile_error.Create(format(err_specific_field_identifer_expected, [lex.identifiers[precdef.fields[i].identifier_idx]]));
                     lex.advance_token;

                     if not lex.token_is_symbol(sym_equals) then
                        raise compile_error.Create(err_equals_expected);
                     lex.advance_token;

                     cexpr := Tcexpression.CreateFromSourceTokens;
                     try
                        precdef.fields[i].ordtypedef.CheckAssignmentCompatability(cexpr);
                        packed_record_fields[i].c := cexpr;
                        packed_record_fields[i].c.AddRef
                     finally
                        cexpr.Release
                     end;
                     packed_record_fields[i].ctyp := precdef.fields[i].ordtypedef;
                     packed_record_fields[i].ctyp.AddRef;

                     all_remaining_fields_are_anonymous := true;
                     for j := i+1 to Length(precdef.fields) - 1 do
                        if precdef.fields[j].kind = normal_packed_field then
                           all_remaining_fields_are_anonymous := false;
                     if (i < Length(precdef.fields) - 1)
                        and
                        (not all_remaining_fields_are_anonymous)
                     then
                        begin
                           if not lex.token_is_symbol(sym_comma) then
                              raise compile_error.Create(err_comma_expected);
                           lex.advance_token
                        end
                  end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         except
            on e: compile_error do
               begin
                  for i := 0 to Length(packed_record_fields)-1 do
                     begin
                        packed_record_fields[i].c.Release;
                        packed_record_fields[i].ctyp.Release
                     end;
                  SetLength (packed_record_fields, 0);
                  raise
               end
         end
      end;   // handle_packed_record_structured_constant

   procedure handle_overlay_structured_constant;
      var
         overlay_typedef: TOverlayType;
         i: integer;
         first_overlay_src_token_idx: integer;
         last_compile_err_msg: string;
         last_compile_err_src_loc: TSourceLocation;
      begin
         overlay_typedef := TOverlayType(typedef);
         overlay_annotation := '';
         typ := typedef;
         last_compile_err_src_loc := NonExistantSourceLocation;
         first_overlay_src_token_idx := lex.token_idx;
         for i := 0 to Length(overlay_typedef.overlaid_variables)-1 do
            begin
               lex.token_idx := first_overlay_src_token_idx;
               if overlay_typedef.overlaid_variables[i].anonymous then
                  try
                     overlay_constant := TStructuredConstant.CreateFromSourceTokens (overlay_typedef.overlaid_variables[i].typedef, lex.token.src_loc);
                     EXIT   // this overlaid variable matched
                  except
                     on e: compile_error do
                        begin
                           if e.source_location.beyond (last_compile_err_src_loc) then
                              begin
                                 last_compile_err_src_loc := e.source_location;
                                 last_compile_err_msg := e.Message
                              end;
                        end
                  end
               else  // not anonymous
                  begin
                     if not lex.token_is_symbol(sym_left_parenthesis) then
                        raise compile_error.Create(err_left_parenthesis_expected);
                     lex.advance_token;
                     if lex.token.identifier_idx = overlay_typedef.overlaid_variables[i].identifier_idx then
                        begin
                           overlay_annotation := overlay_annotation + '.' + lex.token_string (lex.token.src_loc);
                           lex.advance_token;
                           if not lex.token_is_symbol(sym_equals) then
                              raise compile_error.Create(err_equals_expected);
                           lex.advance_token;
                           overlay_constant := TStructuredConstant.CreateFromSourceTokens (overlay_typedef.overlaid_variables[i].typedef, lex.token.src_loc);
                           if not lex.token_is_symbol(sym_right_parenthesis) then
                              raise compile_error.Create(err_right_parenthesis_expected);
                           lex.advance_token;
                           EXIT   // this overlaid variable matched
                        end
                  end
            end;
         // should have exited by now, otherwise report furthest error message found
         raise compile_error.Create (last_compile_err_msg, last_compile_err_src_loc)
      end;

   begin    // TStructuredConstant.CreateFromSourceTokens
      inherited Create (structured_constant_definition);
      typedef := typ;
      set_StructuredConstantKind;
      typedef.AddRef;
      typedef_src_loc := typ_src_loc;
      case typedef.type_kind of
         basic_data_type, set_type, string_type:
            handle_simple_constant;
         array_type:
            handle_array_structured_constant;
         record_type:
            handle_record_structured_constant;
         packed_record_type:
            handle_packed_record_structured_constant;
         overlay_type:
            handle_overlay_structured_constant;
      else
         raise compile_error.Create(err_illegal_type_for_structured_constant, typedef_src_loc)
      end
   end;    // TStructuredConstant.CreateFromSourceTokens

destructor TStructuredConstant.Destroy;
   var
      i: integer;
   begin
      case StructuredConstantKind of
         scSimple:
            simple_constant.Release;
         scRecord:
            for i := 0 to Length(record_fields)-1 do
               record_fields[i].Release;
         scPackedRecord:
            for i := 0 to Length(packed_record_fields)-1 do
               begin
                  packed_record_fields[i].c.Release;
                  packed_record_fields[i].ctyp.Release
               end;
         scArray:
            begin
               for i := 0 to Length(array_elements)-1 do
                  array_elements[i].Release;
               array_element_typedef.Release
            end;
         scOverlay:
            overlay_constant.Release;
         scOther:
            ;
      else
         assert (false)
      end;
      if (StructuredConstantKind = scSimple)
         and
         (Length (linearly_arranged_elements) = 1)
      then
         begin
            linearly_arranged_elements[0].typedef.Release;
            linearly_arranged_elements[0].constant.Release
         end
      else
         for i := 0 to Length(linearly_arranged_elements)-1 do
            if linearly_arranged_elements[i].typedef.type_kind <> packed_record_type then
               begin
                  if not linearly_arranged_elements[i].dont_refcount_typedef then
                     linearly_arranged_elements[i].typedef.Release;
                  linearly_arranged_elements[i].variant_typedef.Release;
                  if linearly_arranged_elements[i].constant <> Self then
                     linearly_arranged_elements[i].constant.Release
               end;
      typedef.Release;
      inherited
   end;

function TStructuredConstant.get_const (i: integer): TSimpleConstant;
   begin
      if Length(linearly_arranged_elements) = 0 then
         add_elements_to_array (linearly_arranged_elements, '');
      result := linearly_arranged_elements[i]
   end;

function TStructuredConstant.LengthOfSimpleConstants: integer;
   begin
      if Length(linearly_arranged_elements) = 0 then
         add_elements_to_array (linearly_arranged_elements, '');
      result := Length(linearly_arranged_elements)
   end;

procedure TStructuredConstant.add_elements_to_array
   (var arr: TSimpleConstantArray;
    annotation: string
   );

   procedure add_simple_element_to_array (constant: TDefinition; typedef: TTypeDef; dont_refcount_typedef: boolean; path, value: string; variant_typedef: TTypeDef);
      var i: integer;
      begin  // add_simple_element_to_array
         i := Length(arr);
         SetLength(arr, i+1);

         arr[i].typedef := typedef;
         arr[i].dont_refcount_typedef := dont_refcount_typedef;
         if not dont_refcount_typedef then
            arr[i].typedef.AddRef;

         if variant_typedef <> nil then
            begin
               arr[i].variant_typedef := variant_typedef;
               variant_typedef.AddRef
            end;

         arr[i].constant := constant;
         if (constant <> nil)
            and
            (constant <> Self)
         then
            arr[i].constant.AddRef;

         arr[i].path := path;

         arr[i].value := value
      end;   // add_simple_element_to_array

   var
      i: integer;
      recdef: TRecordType;
      precdef: TPackedRecordType;
      arrdef: TArrayType;
      index_annotation: string;
      value: string;
   begin   // TStructuredConstant.add_elements_to_array
      case StructuredConstantKind of
         scSimple:
            add_simple_element_to_array (simple_constant, typedef, false, annotation, simple_constant.AsString, nil);
         scRecord:
            begin
               recdef := TRecordType(typedef);
               for i := 0 to Length(recdef.fields) - 1 do
                  record_fields[i].add_elements_to_array (arr, annotation + '.' + recdef.fields[i].name)
            end;
         scPackedRecord:
            begin
               precdef := TPackedRecordType(typedef);
               value := '(';
               for i := 0 to Length(precdef.fields)-1 do
                  begin
                     value := value + precdef.fields[i].name + '=' + packed_record_fields[i].c.AsString;
                     if i < Length(precdef.fields)-1 then
                        value := value + ','
                  end;
               i := Length(arr);
               SetLength(arr, i+1);
               arr[i].typedef := precdef;      // no AddRef here to prevent circle in reference count
               arr[i].constant := Self;        // ditto
               arr[i].path := annotation;
               arr[i].value := value + ')'
            end;
         scArray:
            begin
               arrdef := TArrayType(typedef);
               for i := arrdef.index_typedef.info.min_value.AsInteger to arrdef.index_typedef.info.max_value.AsInteger do
                  begin
                     case arrdef.index_typedef.ordinal_kind of
                        ordinal_base_is_integer:
                           index_annotation := '[' + IntToStr(i) + ']';
                        ordinal_base_is_char:
                           if non_printable_char(chr(i)) then
                              index_annotation := '[chr(' + IntToStr(i) + ')]'
                           else
                              index_annotation := '[''' + chr(i) + ''']';
                        ordinal_base_is_bool:
                           case i of
                              0: index_annotation := '[false]';
                              1: index_annotation := '[true]';
                           else
                              assert(false)
                           end;
                        ordinal_base_is_enum:
                           index_annotation := '[' + lex.identifiers[arrdef.index_typedef.enum_typedef.enums[i].identifier_idx] + ']'
                        else
                           assert(false)
                     end;
                     array_elements[i-arrdef.index_typedef.info.min_value.AsInteger].add_elements_to_array(arr, annotation + index_annotation)
                  end
            end;
         scOverlay:
            if overlay_constant <> nil then
               begin
                  overlay_constant.add_elements_to_array (arr, annotation + overlay_annotation);
                  add_simple_element_to_array (nil, typedef, true, annotation + '.{padding}', '0', overlay_constant.typedef)
               end
            else
               add_simple_element_to_array (nil, typedef, true, annotation + '.{padding}', '0', nil);
         scOther:
            add_simple_element_to_array (nil, typedef, true, annotation, 'null', nil);
      else
         assert(false)
      end
   end;  // TStructuredConstant.add_elements_to_array

procedure TStructuredConstant.set_StructuredConstantKind;
   begin
      case typedef.type_kind of
         basic_data_type, set_type, string_type:
            StructuredConstantKind := scSimple;
         record_type:
            StructuredConstantKind := scRecord;
         packed_record_type:
            StructuredConstantKind := scPackedRecord;
         array_type:
            StructuredConstantKind := scArray;
         overlay_type:
            StructuredConstantKind := scOverlay;
         system_type,
         queue_type:
            StructuredConstantKind := scOther;
      else
         assert (false)
      end
   end;


//================
//  TDefaultValue
//================

constructor TDefaultValue.CreateDefaultValue (typ: TTypeDef);

   function CreateDefaultOrdinalValue (ordtyp: TOrdinalDataType): TConstant;
      begin
         result := nil;  // suppress compiler warning
         if ordtyp.ordinal_kind = ordinal_base_is_enum then
            result := TConstant.CreateEnumConstant (TEnumType(ordtyp), TEnumType(ordtyp).enums[0].value)
         else
            if ordtyp.info.min_value.le(0) and ordtyp.info.max_value.ge(0) then   // 0 is valid value
               case ordtyp.ordinal_kind of
                  ordinal_base_is_integer:
                     result := TConstant.CreateIntegerConstant (0);
                  ordinal_base_is_char:
                     result := TConstant.CreateStringConstant (chr(0));
                  ordinal_base_is_bool:
                     result := TConstant.CreateBooleanConstant (false);
               else
                  assert (false)
               end
            else   // use min_value
               case ordtyp.ordinal_kind of
                  ordinal_base_is_integer:
                     result := TConstant.CreateIntegerConstant (typedef.info.min_value.AsInteger);
                  ordinal_base_is_char:
                     result := TConstant.CreateStringConstant (chr(typedef.info.min_value.AsInteger));
                  ordinal_base_is_bool:
                     result := TConstant.CreateBooleanConstant (boolean(typedef.info.min_value.AsInteger));
               else
                  assert (false)
               end
      end;   // CreateDefaultOrdinalValue

   var
      i: integer;
      arrtyp: TArrayType;
      rectyp: TRecordType;
      prectyp: TPackedRecordType;
   begin   // TDefaultValue.CreateDefaultValue
      inherited Create (structured_constant_definition);
      typedef := typ;
      set_StructuredConstantKind;
      case StructuredConstantKind of
         scSimple:
            case typedef.type_kind of
               basic_data_type:
                  case TBasicDataType(typ).basic_data_type_kind of
                     ordinal_data_type:
                        begin
                           simple_constant := CreateDefaultOrdinalValue (TOrdinalDataType(typ));
                           if TOrdinalDataType(typ).ordinal_kind = ordinal_base_is_enum then
                              simple_constant.dont_reference_count_enum_typedef
                        end;
                     floating_point_data_type:
                        simple_constant := TConstant.CreateRealConstant(0);
                  else
                     assert (false)
                  end;
               set_type:
                  if TSetType(typ).ordinal_kind = ordinal_base_is_enum then
                     begin
                        simple_constant := TConstant.CreateSetConstant ([], TSetType(typ).enum_typedef);
                        simple_constant.dont_reference_count_enum_typedef
                     end
                  else
                     simple_constant := TConstant.CreateSetConstant ([], TSetType(typ).ordinal_kind);
               string_type:
                  simple_constant := TConstant.CreateStringConstant ('');
            else
               assert (false)
            end;
         scArray:
            begin
               arrtyp := TArrayType(typ);
               SetLength (array_elements, arrtyp.index_typedef.info.max_value.AsInteger - arrtyp.index_typedef.info.min_value.AsInteger + 1);
               array_element_typedef := arrtyp.element_typedef;
               array_element_typedef.AddRef;
               for i := 0 to Length(array_elements)-1 do
                  begin
                     array_elements[i] := arrtyp.element_typedef.info.DefaultValue;
                     array_elements[i].AddRef
                  end
            end;
         scRecord:
            begin
               rectyp := TRecordType (typ);
               SetLength (record_fields, Length(rectyp.fields));
               for i := 0 to Length(rectyp.fields)-1 do
                  begin
                     record_fields[i] := rectyp.fields[i].typedef.info.DefaultValue;
                     record_fields[i].AddRef
                  end
            end;
         scPackedRecord:
            begin
               prectyp := TPackedRecordType (typ);
               SetLength (packed_record_fields, Length(prectyp.fields));
               for i := 0 to Length(prectyp.fields)-1 do
                  begin
                     packed_record_fields[i].c := CreateDefaultOrdinalValue (prectyp.fields[i].ordtypedef);
                     packed_record_fields[i].ctyp := prectyp.fields[i].ordtypedef;
                     packed_record_fields[i].ctyp.AddRef
                  end
            end;
         scOverlay,
         scOther:
            {nop};
      else
         assert (false)
      end;
      add_elements_to_array (linearly_arranged_elements, '');
      if StructuredConstantKind = scSimple then
         typedef.Release
   end;    // TDefaultValue.CreateDefaultValue

destructor TDefaultValue.Destroy;
   begin
      typedef := nil;   // prevents inherited Destroy from Releasing this
      if StructuredConstantKind = scSimple then
         linearly_arranged_elements[0].typedef := nil;
      inherited
   end;


//============
//  TVariable
//============

constructor TVariable.Create
   (_id_idx: TIdentifierIdx;
    _context: TDefinition;
    _typ: TTypeDef;
    _definition_prefix: TVariableDescriptor;
    _mode: TAddressMode
   );
   begin
      inherited Create(variable_definition);
      id_idx := _id_idx;
      context := _context;
      typedef := _typ;
      typedef.AddRef;
      typedef_src_loc := typedef.src_loc;
      descriptor := _definition_prefix;
      address_mode := _mode
   end;

constructor TVariable.CreateAnonymousROMString (s: string);
   var
      typ: TStringType;
   begin
      typ := TStringType.Create(Length(s));
      Create (-1, nil, typ, rw_rom, absolute_address_mode);
      typ.Release;
      target_cpu.record_anonymous_string_constant (s);
      is_anonymous_rom_string := true;
      anonymous_rom_string := s
   end;

constructor TVariable.CreateCopy (v: TVariable);
   begin
      inherited Create(variable_definition);
      is_anonymous_rom_string := v.is_anonymous_rom_string;
      anonymous_rom_string := v.anonymous_rom_string;
      id_idx := v.id_idx;
      typedef := v.typedef;
      typedef.AddRef;
      typedef_src_loc := v.typedef.src_loc;
      decl_end_src_loc := v.decl_end_src_loc;
      initial_value := v.initial_value;
      if initial_value <> nil then
         initial_value.AddRef;
      descriptor := v.descriptor;
      address_mode := v.address_mode;
      allocated := v.allocated;
      context := v.context
   end;

constructor TVariable.CreateForLaterDefinition
   (_id_idx: TIdentifierIdx;
    _context: TDefinition
   );
   begin
      inherited Create(variable_definition);
      id_idx := _id_idx;
      context := _context
   end;

destructor TVariable.Destroy;
   begin
      typedef.Release;
      interrupt.Release;
      initial_value.Release;
      inherited
   end;

function TVariable.name: string;
   begin
      result := lex.identifiers [id_idx]
   end;

function TVariable.assignable: boolean;
   begin
      result := descriptor in [rw_var, rw_eeprom, rw_ioreg];
      if not result then
         assert(descriptor in [rw_const, rw_rom])
   end;

function TVariable.ParamMode: TParamMode;
   begin
      if (descriptor in [rw_var, rw_eeprom, rw_ioreg, rw_rom])
         or
         (not typedef.SmallType) then
         result := ByAddress
      else
         result := ByValue
   end;

procedure TVariable.set_addr
   (addr: integer
   );
   begin
//      assert(reachable, 'set addr for unreachable variable');
      faddr := addr
   end;

function TVariable.get_addr: integer;
   begin
//      assert(reachable, 'get addr for unreachable variable');
      result := faddr
   end;


//==============
//  TExpression
//==============

function CreateExpressionFromSourceTokens: TExpression;
   begin
      try
         result := target_cpu.TRelationalExpression_CreateFromSourceTokens;
      except
         on e: ERelationalExpressionSimplification do
            result := e.simplified_expr
      end
   end;

constructor TExpression.Create;
   begin
      inherited Create(expression_definition);
      src_loc := lex.token.src_loc
   end;

function TExpression.info: TTypeInfo;
   begin
      result := TTypeInfo (inherited info)
   end;

procedure TExpression.set_expression_kind (ek: TExpressionKind);
   begin
      f_expression_kind := ek;
      case f_expression_kind of
         integer_expression,
         char_expression,
         boolean_expression,
         enum_expression:
            info.range_kind := irOrdinalRange;
         string_expression:
            info.range_kind := irStringLength;
         set_expression:
            info.range_kind := irSetBounds;
      else
         info.range_kind := irNonRange
      end
   end;

procedure TExpression.set_min_max_for_set_constant;
   var
      i: integer;
   begin
      assert(expression_kind = set_expression);
      assert(contains_constant);
      info.min_value.AsInteger := max_set;
      info.max_value.AsInteger := min_set;
      for i := min_set to max_set do
         if i in constant.sett then
            begin
               if info.min_value.gt(i) then
                  info.min_value.AsInteger := i;
               if info.max_value.lt(i) then
                  info.max_value.AsInteger := i
            end
   end;

function TExpression.contains_constant: boolean;
   begin
      result := false // result := constant <> nil
   end;

function TExpression.contains_integer_constant: boolean;
   begin
      result := false
      // result := (constant <> nil) and (constant.constant_kind = integer_constant)
   end;

function TExpression.contains_real_constant: boolean;
   begin
      result := false
      // result := (constant <> nil) and (constant.constant_kind = real_constant)
   end;

function TExpression.constant: TConstant;
   begin
      result := nil; // suppress compiler warning
      assert(false, 'expression is not TConstantPrimary')
   end;

function TExpression.ordinal_constant_value: TLargestPascalSupportedInt;
   begin
      result := 0; // suppress compiler warning
      assert(false, 'expression is not TConstantPrimary')
   end;

function TExpression.real_constant_value: real;
   begin
      result := 0; // suppress compiler warning
      assert(false, 'expression is not TConstantPrimary')
   end;

function TExpression.boolean_constant_value: boolean;
   begin
      result := false;  // suppress compiler warning
      assert(false, 'expression is not TConstantPrimary')
   end;

procedure TExpression.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   begin
      assert (false)    // should have been overriden in a descendant
   end;

procedure TExpression.copy_expression_info
   (from_expression: TExpression
   );
   begin
      expression_kind := from_expression.expression_kind;
      info.min_value.Assign (from_expression.info.min_value);
      info.max_value.Assign (from_expression.info.max_value);
      enum_typedef := from_expression.enum_typedef;
      set_ordinal_base_type := from_expression.set_ordinal_base_type;
      typedef := from_expression.typedef
   end;

procedure TExpression.check_set_constant (constant: TConstant; valid_range: TTypeInfo; src_loc: TSourceLocation);
   var i: integer;
   begin
      for i := min_set to max_set do
         if (constant <> nil) and (i in constant.sett) then
            if (i < valid_range.min_value.AsInteger) or (i > valid_range.max_value.AsInteger) then
               raise compile_error.Create (err_expression_contains_constant_outside_allowable_range, src_loc)
   end;

function CreateTypeDenoterFromSourceTokens: TTypeDef;
   begin
      if (lex.token_is_identifier)
         and
         (CurrentDefinitionTable[lex.token.identifier_idx].definition_kind = type_definition)
      then
         begin
            result := TTypeDef(CurrentDefinitionTable[lex.token.identifier_idx]);
            if not result.definition_complete then
               raise compile_error.Create(err_self_referential_type);
            result.AddRef;
            lex.advance_token
         end
      else // new definition
         begin
            if lex.token_is_symbol(sym_left_parenthesis) then
               result := TEnumType.CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_record) then
               result := TRecordType.CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_set) then
               result := TSetType.CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_array) then
               result := TArrayType.CreateFromSourceTokens
            else if lex.token_is_reserved_word([rw_monitor, rw_class, rw_process, rw_interrupt]) then
               result := target_cpu.TSystemType_CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_string) then
               result := TStringType.CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_packed) then
               result := TPackedRecordType.CreateFromSourceTokens
            else if lex.token_is_reserved_word(rw_overlay) then
               result := TOverlayType.CreateFromSourceTokens
            else
               try
                  result := TSubRangeType.CreateFromSourceTokens
               except
                  on e: EDefinitelyNotASubRange do
                     raise compile_error.Create(err_type_definition_expected, e.src_loc)
               end;
            result.last_src_loc := lex.previous_token_src_loc
         end;
      assert(result.IsTypeDefinition)
   end;

procedure check_for_write_access
   (v: TVariable;
    src_loc: TSourceLocation
   );
   begin
      if v.descriptor = rw_const then
         raise compile_error.Create(err_cannot_change_constant_parameter, src_loc);
      if v.descriptor = rw_rom then
         raise compile_error.Create(err_cannot_change_rom_constant, src_loc);
      if v.descriptor = rw_for then
         raise compile_error.Create(err_cannot_change_for_loop_control_variable,
         src_loc)
   end;

END.
