UNIT pic18x_types_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACe

uses
  cpc_definitions_unit;

type
   TPIC18x_RecordFieldInfo =
      class (TCPUSpecificInfo)
      private
         info_calculated: boolean;
         f_offset: integer;
      public
         function Offset: integer;
      end;

   TPIC18x_BitPosition = 0..7;

   TPIC18x_PackedRecordFieldInfo =
      class (TCPUSPecificInfo)
         f_offset: integer;
         f_position: TPIC18x_BitPosition;
         f_width: integer;
         f_span: integer;
         reversed_byte_order: boolean;   //  only for types defined for some ioreg
         function Offset: integer;  // offset of the byte containing most significant bit
         function Position: TPIC18x_BitPosition;   // most significant bit position. 7 is leftmost bit, 0 is rightmost
         function Width: integer;   // width in bits, may span multiple bytes IN LITTLE ENDIAN direction
         function Span: integer;    // number of bytes containing this field
      end;

//  Some PIC18 ioreg types are laid out big-endian (normal order) and some are laid out little-endian (reversed order).
//     A flag in the processor .ini file is used to indicate reversed packed record types.
//
//  Example layout of a normal order type:
//     tpr =
//        packed record    Offset Position Width Span          This will be packed into three bytes:
//           a: uint3;        0        7      3     1             bit#   +-7--6--5--4--3--2--1--0+
//           b: uint4;        0        4      4     1             @pr+0  |a2 a1 a0|b3 b2 b1 b0|c9|
//           c: uint10;       0        0     10     3             @pr+1  |c8 c7 c6 c5 c4 c3 c2 c1|
//           d: uint3         2        6      3     1             @pr+2  |c0|d2 d1 d0| -  -  -  -|
//           -: unit4         2        3      4     1
//        end;
//
//  Example layout of a reversed order type:
//     tpr =
//        packed record    Offset Position Width Span          This will be packed into three bytes:
//           a: uint3;        2        7      3     1             bit#   +-7--6--5--4--3--2--1--0+
//           b: uint4;        2        4      4     1             @pr+0  |c0|d2 d1 d0| -  -  -  -|
//           c: uint10;       2        0     10     3             @pr+1  |c8 c7 c6 c5 c4 c3 c2 c1|
//           d: uint3         0        6      3     1             @pr+2  |a2 a1 a0|b3 b2 b1 b0|c9|
//           -: unit4         0        3      4     1
//        end;



IMPLEMENTATION

uses
  cpc_types_unit, pic18x_core_objects_unit, pic18x_blocks_unit;

function TPIC18x_RecordFieldInfo.Offset: integer;
   var
      i: integer;
      rec: TRecordType;
   begin
      if info_calculated then
         begin
            result := f_offset;
            exit
         end;
      result := 0;  // suppress compiler warning
      rec := TRecordField(parent).containing_record;
      f_offset := 0;
      for i := 0 to Length(rec.fields)-1 do
         begin
            if Self = rec.fields[i].info then
               begin
                  if system_type_or_array_of_system_type(rec.fields[i].typedef) then
                     f_offset := f_offset + $3F;
                  result := f_offset;
                  info_calculated := true;
                  exit
               end;
            f_offset := f_offset + TPIC18x_TypeInfo(rec.fields[i].typedef.info).Size
         end;
      assert (false)  // this field wasn't in rec?
   end;

function TPIC18x_PackedRecordFieldInfo.Offset: integer;
   begin
      TPIC18x_PackedRecord_TypeInfo(TPackedRecordField(parent).containing_packed_record.info).calculate_layout;
      result := f_offset
   end;

function TPIC18x_PackedRecordFieldInfo.Position: TPIC18x_BitPosition;
   begin
      TPIC18x_PackedRecord_TypeInfo(TPackedRecordField(parent).containing_packed_record.info).calculate_layout;
      result := f_position
   end;

function TPIC18x_PackedRecordFieldInfo.Width: integer;
   begin
      TPIC18x_PackedRecord_TypeInfo(TPackedRecordField(parent).containing_packed_record.info).calculate_layout;
      result := f_width
   end;

function TPIC18x_PackedRecordFieldInfo.Span: integer;
   begin
      TPIC18x_PackedRecord_TypeInfo(TPackedRecordField(parent).containing_packed_record.info).calculate_layout;
      result := f_span
   end;

END.
