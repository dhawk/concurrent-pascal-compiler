UNIT cpc_target_cpu_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes,
   cpc_access_unit,
   cpc_blocks_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_expressions_unit,
   cpc_simple_expression_unit,
   cpc_source_analysis_unit,
   cpc_statements_unit,
   cpc_term_expression_unit;

type
   Tmod_operator_implementation = (iso_pascal_mod_operator_implementation, delphi_mod_operator_implemenation);

type
   TTargetCPUBaseClass =
      class
      protected
         procedure add_additional_supported_data_type (id: string; typedef: TTypeDef);
      public
         processor_name: string;
         supported_data_types:
            array of
               record
                  id: string;
                  typedef: TTypeDef
               end;
         constructor Create (_processor_name: string);
         destructor Destroy;
            override;

         function Preamble: TStringList;
            virtual;
         procedure process_preamble;
            virtual;
         procedure release_preamble;
            virtual;
         procedure init_compiler_directive_flags;
            virtual;

         procedure add_successful_compilation_info (results_listing: TStrings);
            virtual; abstract;

         function process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
            virtual;

         function load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
            virtual; abstract;
         function has_eeprom: boolean;
            virtual; abstract;

         function mod_operator_implementation: Tmod_operator_implementation;
            virtual; abstract;
         procedure validate_ioregister_address
            (name: string;
             addr: integer;
             type_def: TTypeDef;
             src_loc: TSourceLocation
            );
            virtual; abstract;
         procedure validate_process_priority
            (priority: integer;
             src_loc: TSourceLocation
            );
            virtual; abstract;
         function max_supported_ordinal_size: integer;
            virtual; abstract;
         function initial_process_priority: integer;
            virtual; abstract;
         function ioregister_width_in_address_units
            (width_in_bits: integer;
             src_loc: TSourceLocation
            ): integer;
            virtual; abstract;
         function get_supported_data_type (id: string): TTypeDef;
         function round_trunc_result_type: TTypeDef;
            virtual; abstract;
         procedure record_anonymous_string_constant (s: string);
            virtual; abstract;
         procedure release_anonymous_string_constants;
            virtual;
         function supports_ioreg_1bit_params: boolean;
            virtual;

         {$include TTargetCPU_constructor_function_abstract_decls.inc}

         procedure generate_machine_code (_prog: TProgram);
            virtual; abstract;
         function src_directory_relative_to_bin: string;
            virtual; abstract;
      end;

var
   target_cpu: TTargetCPUBaseClass;

IMPLEMENTATION

uses
   SysUtils;

constructor TTargetCPUBaseClass.Create (_processor_name: string);
   var
      i: integer;
   begin
      assert (target_cpu = nil);
      target_cpu := Self;
      processor_name := _processor_name;
      for i := 1 to max_supported_ordinal_size do
         begin
            add_additional_supported_data_type ('int' + IntToStr(i), TIntegerDataType.CreateSignedOrdinal(i));
            add_additional_supported_data_type ('uint' + IntToStr(i), TIntegerDataType.CreateUnSignedOrdinal(i))
         end
   end;

destructor TTargetCPUBaseClass.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(supported_data_types)-1 do
         supported_data_types[i].typedef.Release;
      target_cpu := nil
   end;

function TTargetCPUBaseClass.Preamble: TStringList;
   begin
      result := TStringList.Create
   end;

procedure TTargetCPUBaseClass.process_preamble;
   begin
   end;

procedure TTargetCPUBaseClass.release_preamble;
   begin
   end;

procedure TTargetCPUBaseClass.init_compiler_directive_flags;
   begin
   end;

function TTargetCPUBaseClass.process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
   begin
      result := false
   end;

procedure TTargetCPUBaseClass.add_additional_supported_data_type (id: string; typedef: TTypeDef);
   var i: integer;
   begin
      typedef.name := id;
      i := Length(supported_data_types);
      SetLength (supported_data_types, i+1);
      supported_data_types[i].id := id;
      supported_data_types[i].typedef := typedef
   end;

function TTargetCPUBaseClass.get_supported_data_type (id: string): TTypeDef;
   var i: integer;
   begin
      result := nil;  // to suppress compiler warning
      for i := 0 to Length(supported_data_types)-1 do
         if supported_data_types[i].id = id then
            begin
               result := supported_data_types[i].typedef;
               exit
            end;
      assert (false, 'no such typedef id')
   end;

procedure TTargetCPUBaseClass.release_anonymous_string_constants;
   begin
   end;

function TTargetCPUBaseClass.supports_ioreg_1bit_params: boolean;
   begin
      result := false
   end;

END.

