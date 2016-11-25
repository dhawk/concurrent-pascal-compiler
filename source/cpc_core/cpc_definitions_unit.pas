UNIT cpc_definitions_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses cpc_common_unit, cpc_source_analysis_unit;

type
   TDefinition = class;

   TCPUSpecificInfo =
      class (TReferenceCountedObject)
         parent: TDefinition;
         constructor Create (_parent: TDefinition);
      end;

   TArrayOfTDefinition =
      array of TDefinition;  // either TAccess or TExpression

   TCallType = (standalone_routine_call, systemtype_init_call, systemtype_routine_call, systemtype_property_call);
   TCallRecord =
      class
         called_name: string;
         called_access: TDefinition;  // must be TAccess
         call_type: TCallType;
         constructor Create (_called_name: string; _called_access: TDefinition; _call_type: TCallType);
      end;

   TDefinitiontKind =
      (uninitialized_definition,  // must be first (ord 0)
       access_definition,
       constant_definition,
       data_item_list_definition,
       expression_definition,
       packed_record_field_definition,
       paramlist_definition,
       program_definition,
       property_definition,
       record_field_definition,
       routine_definition,  // procedures and functions
       statement_definition,
       structured_constant_definition,
       type_definition,
       type_info,
       variable_definition,
       with_property_definition,
       with_routine_definition,
       with_variable_definition
      );
   TDefinition =
      class (TReferenceCountedObject)
      private
         f_info: TCPUSpecificInfo;
      protected
         procedure AddSelfToCodeBlockList;
      public
         src_loc: TSourceLocation;
         reachable: boolean;
         definition_kind: TDefinitiontKind;
         call_record_list: array of TCallRecord;
         class var CodeBlockList: TArrayOfTDefinition;
            // routines, system type initial statements, program initial statement and rom constants in source code order
         class procedure ClearCodeBlockList;
         constructor Create;
            overload;
         constructor Create
            (kind: TDefinitiontKind
            );
            overload;
         destructor Destroy;
            override;
         function info: TCPUSpecificInfo;
         function IsConstantDefiniton: boolean;
         function IsTypeDefinition: boolean;
         function IsDataDefinition: boolean;
         procedure MarkAsReachable;
            virtual;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            virtual;
         function Generate (param1, param2: integer): integer;
            virtual;
         procedure AddCallRecord (call: TCallRecord);
      end;

   TDefStack =
      class
         procedure define
            (definition: TDefinition;
             defining_scope: integer;
             def_src_loc: TSourceLocation
            );
         procedure redefine
            (definition: TDefinition;
             defining_scope: integer
            );
         function get_current_definition: TDefinition;
         function get_global_definition: TDefinition;
         function is_defined_in_current_scope (defining_scope: integer): boolean;
         procedure leave_scope (current_scope: integer);
         procedure ReleaseDefinitions;
      private
         stk:
            array of
               record
                  definition: TDefinition;
                  defining_scope: integer
               end
      end;

   TCurrentDefinitionTable =
      class
      private type
         TScopeStack =
            class
               procedure push
                  (i: integer
                  );
               function pop: integer;
            private
               stk:
                  array of integer;
            end;
      private var
         identifiers: array of TDefStack;  // index is TIdentifierIdx
         next_scope: integer;
         scope_stack: TScopeStack;
         function get
            (i: TIdentifierIdx
            ): TDefinition;
      public
         current_scope: integer;
         constructor Create;
         destructor Destroy;
            override;
         property default_anonymous[i: TIdentifierIdx]: TDefinition read get; default;
         procedure DefineCPUBasicDataTypesForCurrentScope;
         procedure EnterNewScope;
         procedure ExitScope;
         procedure DefineForCurrentScope
            (id_idx: TIdentifierIdx;
             def: TDefinition;
             id_loc: TSourceLocation
            );
         procedure RedefineForCurrentScope
            (id_idx: TIdentifierIdx;
             def: TDefinition
            );
         function GetDefinitionForIdentifier (id: string; should_exist: boolean): TDefinition;
         function DefinedInCurrentScope (id_idx: TIdentifierIdx): boolean;
      end;

var
   CurrentDefinitionTable: TCurrentDefinitionTable;


IMPLEMENTATION

uses SysUtils, cpc_target_cpu_unit;


//==============
//  TCallRecord
//==============

constructor TCallRecord.Create (_called_name: string; _called_access: TDefinition; _call_type: TCallType);
   begin
      called_name := _called_name;
      called_access := _called_access;
      call_type := _call_type
   end;


//===============
//  TDefinition
//===============

constructor TDefinition.Create;
   begin
      assert(false, 'called with no definition_kind param')
   end;

constructor TDefinition.Create
   (kind: TDefinitiontKind
   );
   begin
      inherited Create;
      definition_kind := kind
   end;

destructor TDefinition.Destroy;
   var
      i: integer;
   begin
      f_info.Release;
      for i := 0 to Length(call_record_list)-1 do
         call_record_list[i].Free
   end;

function TDefinition.info: TCPUSpecificInfo;
   begin
      if f_info = nil then
         f_info := target_cpu.load_cpu_specific_info (self);
      result := f_info
   end;

function TDefinition.IsConstantDefiniton: boolean;
   begin
      result := definition_kind = constant_definition
   end;

function TDefinition.IsTypeDefinition: boolean;
   begin
      result := definition_kind = type_definition
   end;

function TDefinition.IsDataDefinition: boolean;
   begin
      result := definition_kind = variable_definition
   end;

procedure TDefinition.MarkAsReachable;
   begin
      reachable := true
   end;

function TDefinition.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := false
   end;

function TDefinition.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      assert (false, 'not implemented')
   end;

class procedure TDefinition.ClearCodeBlockList;
   var i: integer;
   begin
      for i := 0 to Length(CodeBlockList) - 1 do
         CodeBlockList[i].Release;
      SetLength(CodeBlockList, 0);
   end;

procedure TDefinition.AddSelfToCodeBlockList;
   var i: integer;
   begin
      i := Length (CodeBlockList);
      SetLength (CodeBlockList, i+1);
      CodeBlockList[i] := self;
      AddRef
   end;

procedure TDefinition.AddCallRecord (call: TCallRecord);
   var i: integer;
   begin
      i := Length(call_record_list);
      SetLength (call_record_list, i+1);
      call_record_list[i] := call
   end;


// ==================
// TIdentifierList
// ==================

constructor TCurrentDefinitionTable.Create;
   var
      i: integer;
   begin
      assert (CurrentDefinitionTable = nil);
      CurrentDefinitionTable := Self;
      scope_stack := TScopeStack.Create;
      scope_stack.push(current_scope);
      SetLength (identifiers, lex.NumberOfIdentifiers);
      for i := 0 to lex.NumberOfIdentifiers-1 do
         identifiers[i] := TDefStack.Create
   end;

destructor TCurrentDefinitionTable.Destroy;
   var
      i: integer;
   begin
      scope_stack.Free;
      for i := 0 to Length(identifiers)-1 do
         identifiers[i].Free;
      inherited;
      CurrentDefinitionTable := nil
   end;

procedure TCurrentDefinitionTable.DefineCPUBasicDataTypesForCurrentScope;
   var i: integer;
   begin
      assert (Length(target_cpu.supported_data_types) = Length(lex.supported_data_type_id_idxs));
      for i := 0 to Length(target_cpu.supported_data_types)-1 do
         DefineForCurrentScope (lex.supported_data_type_id_idxs[i],
                                target_cpu.supported_data_types[i].typedef,
                                NonExistantSourceLocation
                               )
   end;

function TCurrentDefinitionTable.get
   (i: TIdentifierIdx
   ): TDefinition;
   begin
      result := identifiers[i].get_current_definition
   end;

procedure TCurrentDefinitionTable.DefineForCurrentScope
   (id_idx: TIdentifierIdx;
    def: TDefinition;
    id_loc: TSourceLocation
   );
   begin
      identifiers[id_idx].define(def, current_scope, id_loc)
   end;

function TCurrentDefinitionTable.DefinedInCurrentScope (id_idx: TIdentifierIdx): boolean;
   begin
      result := identifiers[id_idx].is_defined_in_current_scope (current_scope)
   end;

procedure TCurrentDefinitionTable.RedefineForCurrentScope
   (id_idx: TIdentifierIdx;
    def: TDefinition
   );
   begin
      identifiers[id_idx].redefine(def, current_scope)
   end;

procedure TCurrentDefinitionTable.EnterNewScope;
   begin
      scope_stack.push(current_scope);
      next_scope := next_scope + 1;
      current_scope := next_scope
   end;

procedure TCurrentDefinitionTable.ExitScope;
   var
      identifier_idx: integer;
   begin
      for identifier_idx := 0 to Length(identifiers) - 1 do
         identifiers[identifier_idx].leave_scope(current_scope);
      current_scope := scope_stack.pop
   end;

function TCurrentDefinitionTable.GetDefinitionForIdentifier (id: string; should_exist: boolean): TDefinition;
   var i: integer;
   begin
      for i := 0 to Length(lex.identifiers)-1 do
         if (LowerCase (id) = LowerCase (lex.identifiers[i]))
            and
            (true)
         then
            begin
               result := Self[i];
               exit
            end;
      result := nil;
      if should_exist then
         assert (false, 'identifier not in original source or preamble')
   end;


// =====================================
//  TCurrentDefinitionTable.TScopeStack
// =====================================

procedure TCurrentDefinitionTable.TScopeStack.push
   (i: integer
   );
   var
      idx: integer;
   begin
      idx := Length(stk);
      SetLength(stk, idx + 1);
      stk[idx] := i
   end;

function TCurrentDefinitionTable.TScopeStack.pop: integer;
   var
      idx: integer;
   begin
      idx := Length(stk) - 1;
      if idx >= 0 then
         begin
            result := stk[idx];
            SetLength(stk, idx)
         end
      else
         result := -1
   end;


// ============
// TDefStack
// ============

procedure TDefStack.define
   (definition: TDefinition;
    defining_scope: integer;
    def_src_loc: TSourceLocation
   );
   var
      idx: integer;
   begin
      idx := Length(stk);
      if (idx > 0)
         and
         (stk[idx - 1].defining_scope = defining_scope)
      then
         raise compile_error.Create(err_identifier_already_defined, def_src_loc);
      SetLength(stk, idx + 1);
      stk[idx].definition := definition;
      definition.AddRef;
      stk[idx].defining_scope := defining_scope
   end;

function TDefStack.is_defined_in_current_scope (defining_scope: integer): boolean;
   var
      idx: integer;
   begin
      idx := Length(stk);
      if (idx > 0)
         and
         (stk[idx-1].defining_scope = defining_scope)
      then
         result := true
      else
         result := false
   end;

procedure TDefStack.redefine
   (definition: TDefinition;
    defining_scope: integer
   );
   var
      idx: integer;
   begin
      idx := Length(stk) - 1;
      assert(stk[idx].defining_scope = defining_scope);
      stk[idx].definition.Release;
      stk[idx].definition := definition;
      definition.AddRef
   end;

function TDefStack.get_current_definition: TDefinition;
   var
      idx: integer;
   begin
      idx := Length(stk) - 1;
      if idx = -1 then
         raise compile_error.Create(err_undefined_identifier);
      result := stk[idx].definition
   end;

function TDefStack.get_global_definition: TDefinition;
   begin
      if Length(stk) = 0 then
         raise compile_error.Create(err_undefined_identifier);
      result := stk[0].definition
   end;

procedure TDefStack.leave_scope
   (current_scope: integer
   );
   var
      idx: integer;
   begin
      idx := Length(stk) - 1;
      if (idx >= 0)
         and
         (stk[idx].defining_scope = current_scope)
      then
         begin
            stk[idx].definition.Release;
            SetLength(stk, idx)
         end
   end;

procedure TDefStack.ReleaseDefinitions;
   var
      i: integer;
   begin
      for i := 0 to Length(stk) - 1 do
         stk[i].definition.Release;
      SetLength(stk, 0)
   end;


//===================
//  TCPUSpecificInfo
//===================

constructor TCPUSpecificInfo.Create (_parent: TDefinition);
   begin
      inherited Create;
      parent := _parent
   end;

END.
