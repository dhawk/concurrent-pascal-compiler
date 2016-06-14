UNIT cpc_main_compiler_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
  Classes, cpc_definitions_unit, cpc_source_analysis_unit;

type
   TDefinitionGenerator = function: TDefinition;
   TCompilationResult = (compiled_ok, compile_error_in_preamble, compile_error_empty_source, compile_error_in_source, error_during_code_generation);
   TCompilation =
      class
      private type
         TProc = procedure of object;
      private
         s: string;
         ss: TStrings;
         procedure load_file_into_lex;
         procedure load_string_into_lex;
         procedure load_stringlist_into_lex;
         procedure run_compiler (ReadinSource: TProc; Generator: TDefinitionGenerator; ResultsListing: TStrings);
      public
         compilation_result: TCompilationResult;
         compiled_object: TDefinition;
         compiler_error_message: string;
         compiler_error_source_location: TSourceLocation;
         constructor CreateFromFile (file_name: string; Generator: TDefinitionGenerator; result_listing: TStrings);
         constructor CreateFromString (source_string: string; Generator: TDefinitionGenerator; result_listing: TStrings);
         constructor CreateFromStrings (source_strings: TStrings; Generator: TDefinitionGenerator; result_listing: TStrings);
         destructor Destroy;
            override;
      end;


IMPLEMENTATION

uses SysUtils, cpc_target_cpu_unit, cpc_common_unit;

procedure TCompilation.run_compiler (ReadinSource: TProc; Generator: TDefinitionGenerator; ResultsListing: TStrings);
   var
      i: integer;
   begin
      ResultsListing.Add (target_cpu.processor_name + ' Concurrent Pascal Compiler');
      ResultsListing.Add ('');
      try
         lex := TLexicalAnalysis.Create;
         lex.LoadCPUBasicDataTypeIdentifiers;
         lex.ReadInPreamble (target_cpu.Preamble);
         try
            ReadInSource;
            lex.DoLexicalAnalysis;

            CurrentDefinitionTable := TCurrentDefinitionTable.Create;
            CurrentDefinitionTable.DefineCPUBasicDataTypesForCurrentScope;

            compiled_object := Generator;
            compilation_result := compiled_ok;
            ResultsListing.Add ('Compiled Ok');
            ResultsListing.Add ('');
            target_cpu.add_successful_compilation_info (ResultsListing)
         except
            on e: compile_error do
               if (e.source_location.same_location (NonExistantSourceLocation))
               then
                  begin
                     compilation_result := compile_error_empty_source;
                     ResultsListing.Add ('error: empty source file')
                  end
               else if (e.source_location.same_location (FileErrorSourceLocation)) then
                  begin
                     compilation_result := compile_error_in_source;
                     ResultsListing.Add (e.Message)
                  end
               else if (e.source_location.in_preamble) then
                  if lex.token_is_eof then
                     begin
                        compilation_result := compile_error_in_source;
                        ResultsListing.Add ('error: empty source file')
                     end
                  else
                     begin
                        compilation_result := compile_error_in_preamble;
                        ResultsListing.Add ('Compile error at line ' + IntToStr (e.source_location.line_no) + ' in ' + e.source_location.file_name + ':');
                        ResultsListing.Add (e.source_location.line);
                        ResultsListing.Add (e.source_location.caret + ' ' + e.Message)
                     end
               else
                  begin
                     compilation_result := compile_error_in_source;
                     compiler_error_message := e.Message;
                     compiler_error_source_location := e.source_location;

                     ResultsListing.Add ('Compile error at line ' + IntToStr (e.source_location.line_no) + ' in ' + e.source_location.file_name + ':');
                     ResultsListing.Add (e.source_location.line);
                     ResultsListing.Add (e.source_location.caret + ' ' + e.Message)
                  end;
            on e: code_generation_error do
               begin
                  compilation_result := error_during_code_generation;
                  if e.error_messages.Count = 1
                  then
                     ResultsListing.Add ('Error during code generation:')
                  else
                     ResultsListing.Add ('Errors during code generation:');
                  for i := 0 to e.error_messages.Count-1 do
                     ResultsListing.Add ('   ' + e.error_messages[i])
               end;
         end
      finally
         lex.Free;
         if CurrentDefinitionTable <> nil then
            CurrentDefinitionTable.ExitScope;
         CurrentDefinitionTable.Free;
         TDefinition.ClearCodeBlockList;
         target_cpu.release_anonymous_string_constants
      end
   end;

procedure TCompilation.load_file_into_lex;
   begin
      lex.ReadInSourceFile (s)
   end;

procedure TCompilation.load_string_into_lex;
   begin
      lex.ReadInTestCase (s)
   end;

procedure TCompilation.load_stringlist_into_lex;
   begin
      lex.ReadInTestCase (ss)
   end;

constructor TCompilation.CreateFromFile (file_name: string; Generator: TDefinitionGenerator; result_listing: TStrings);
   begin
      s := file_name;
      run_compiler (load_file_into_lex, Generator, result_listing)
   end;

constructor TCompilation.CreateFromString (source_string: string; Generator: TDefinitionGenerator; result_listing: TStrings);
   begin
      s := source_string;
      run_compiler (load_string_into_lex, Generator, result_listing)
   end;

constructor TCompilation.CreateFromStrings (source_strings: TStrings; Generator: TDefinitionGenerator; result_listing: TStrings);
   begin
      ss := source_strings;
      run_compiler (load_stringlist_into_lex, Generator, result_listing)
   end;

destructor TCompilation.Destroy;
   begin
      compiled_object.Release;
      inherited
   end;

END.
