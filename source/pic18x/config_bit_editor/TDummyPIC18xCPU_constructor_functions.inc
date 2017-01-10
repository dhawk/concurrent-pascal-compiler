function TDummyPIC18xCPU.TAbsFunctionPrimary_CreateFromSourceTokens: TAbsFunctionPrimary;
   begin
      result := TAbsFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TAccess_CreateFromSourceTokens: TAccess;
   begin
      result := TAccess.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TAccess_CreateFromVariable (v: TVariable): TAccess;
   begin
      result := TAccess.CreateFromVariable (v)
   end;

function TDummyPIC18xCPU.TAssertStatement_CreateFromSourceTokens: TAssertStatement;
   begin
      result := TAssertStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TAssignmentStatement_CreateFromSourceTokens (acc: TAccess): TAssignmentStatement;
   begin
      result := TAssignmentStatement.CreateFromSourceTokens (acc)
   end;

function TDummyPIC18xCPU.TCycleStatement_CreateFromSourceTokens: TCycleStatement;
   begin
      result := TCycleStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TReCycleStatement_CreateFromSourceTokens: TReCycleStatement;
   begin
      result := TReCycleStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TAwaitInterruptStatement_CreateFromSourceTokens: TAwaitInterruptStatement;
   begin
      result := TAwaitInterruptStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TCaseStatement_CreateFromSourceTokens: TCaseStatement;
   begin
      result := TCaseStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TChrTypeConversionPrimary_CreateFromSourceTokens: TChrTypeConversionPrimary;
   begin
      result := TChrTypeConversionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TConstantPrimary_CreateFromConstant (_constant: TConstant; _src_loc: TSourceLocation): TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromConstant (_constant, _src_loc)
   end;

function TDummyPIC18xCPU.TConstantPrimary_CreateFromIdentifierConstant: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromIdentifierConstant
   end;

function TDummyPIC18xCPU.TConstantPrimary_CreateFromReservedWordConstant: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromReservedWordConstant
   end;

function TDummyPIC18xCPU.TConstantPrimary_CreateFromSourceTokens: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TConstantPrimary_CreateFromStructuredConstantAccess (acc: TAccess): TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromStructuredConstantAccess (acc)
   end;

function TDummyPIC18xCPU.TContinueStatement_CreateFromSourceTokens: TContinueStatement;
   begin
      result := TContinueStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TDataItemList_Create: TDataItemList;
   begin
      result := TDataItemList.Create
   end;

function TDummyPIC18xCPU.TDelayStatement_CreateFromSourceTokens: TDelayStatement;
   begin
      result := TDelayStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TEmptyFunctionPrimary_CreateFromSourceTokens: TEmptyFunctionPrimary;
   begin
      result := TEmptyFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TExitLoopStatement_CreateFromSourceTokens: TExitLoopStatement;
   begin
      result := TExitLoopStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TForStatement_CreateFromSourceTokens: TForStatement;
   begin
      result := TForStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TFunctionAccessPrimary_CreateFromSourceTokens (acc: TAccess): TFunctionAccessPrimary;
   begin
      result := TFunctionAccessPrimary.CreateFromSourceTokens (acc)
   end;

function TDummyPIC18xCPU.TIfStatement_CreateFromSourceTokens: TIfStatement;
   begin
      result := TIfStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TInitStatement_CreateFromSourceTokens: TInitStatement;
   begin
      result := TInitStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TLoopStatement_CreateFromSourceTokens: TLoopStatement;
   begin
      result := TLoopStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TNotPrimary_CreateFromSourceTokens: TNotPrimary;
   begin
      result := TNotPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TParamList_CreateFromSourceTokens (context: TParamListContext; max_scope: integer): TParamList;
   begin
      result := TParamList.CreateFromSourceTokens (context, max_scope)
   end;

function TDummyPIC18xCPU.TParamList_CreatePropertyPseudoParamList (property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TParamList;
   begin
      result := TParamList.CreatePropertyPseudoParamList (property_id, property_id_src_loc, property_typdef)
   end;

function TDummyPIC18xCPU.TPredFunctionPrimary_CreateFromSourceTokens: TPredFunctionPrimary;
   begin
      result := TPredFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TProgram_CreateFromSourceTokens: TProgram;
   begin
      result := TProgram.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TProperty_CreateFromSourceTokens (cntxt: TDefinition; entry_property: boolean): TProperty;
   begin
      result := TProperty.CreateFromSourceTokens (cntxt, entry_property)
   end;

function TDummyPIC18xCPU.TRelationalExpression_CreateFromSourceTokens: TRelationalExpression;
   begin
      result := TRelationalExpression.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TReLoopStatement_CreateFromSourceTokens: TReLoopStatement;
   begin
      result := TReLoopStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TRoundFunctionPrimary_CreateFromSourceTokens: TRoundFunctionPrimary;
   begin
      result := TRoundFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TRoutine_CreateFromSourceTokens (cntxt: TDefinition; entry_routine: boolean): TRoutine;
   begin
      result := TRoutine.CreateFromSourceTokens (cntxt, entry_routine)
   end;

function TDummyPIC18xCPU.TRoutine_CreatePropertyGetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TRoutine.CreatePropertyGetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TDummyPIC18xCPU.TRoutine_CreatePropertySetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TRoutine.CreatePropertySetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TDummyPIC18xCPU.TRoutineCallStatement_CreateFromSourceTokens (acc: TAccess): TRoutineCallStatement;
   begin
      result := TRoutineCallStatement.CreateFromSourceTokens (acc)
   end;

function TDummyPIC18xCPU.TRoutineCallStatement_CreatePropertySetterCall (acc: TAccess; exp: TExpression; _src_loc: TSourceLocation): TRoutineCallStatement;
   begin
      result := TRoutineCallStatement.CreatePropertySetterCall (acc, exp, _src_loc)
   end;

function TDummyPIC18xCPU.TSetConstructorPrimary_CreateFromSourceTokens: TSetConstructorPrimary;
   begin
      result := TSetConstructorPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TSimpleExpression_CreateFromSourceTokens: TSimpleExpression;
   begin
      result := TSimpleExpression.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TStatementList_Create: TStatementList;
   begin
      result := TStatementList.Create
   end;

function TDummyPIC18xCPU.TStatementList_CreateFromSourceTokens (terminator: TBooleanFunctionOfObject; non_terminated_error_message: string; while_or_until_allowed: boolean): TStatementList;
   begin
      result := TStatementList.CreateFromSourceTokens (terminator, non_terminated_error_message, while_or_until_allowed)
   end;

function TDummyPIC18xCPU.TStrPosPrimary_Create (acc: TAccess): TStrPosPrimary;
   begin
      result := TStrPosPrimary.Create (acc)
   end;

function TDummyPIC18xCPU.TSuccFunctionPrimary_CreateFromSourceTokens: TSuccFunctionPrimary;
   begin
      result := TSuccFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TSystemType_CreateFromSourceTokens: TSystemType;
   begin
      result := TTestSystemType.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TTerm_CreateFromSourceTokens: TTerm;
   begin
      result := TTerm.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TTruncFunctionPrimary_CreateFromSourceTokens: TTruncFunctionPrimary;
   begin
      result := TTruncFunctionPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TTypeInfo_Create (_info: TTypeInfo): TTypeInfo;
   begin
      result := TTypeInfo.Create (_info)
   end;

function TDummyPIC18xCPU.TTypeInfo_Create (_parent: TDefinition): TTypeInfo;
   begin
      result := TTypeInfo.Create (_parent)
   end;

function TDummyPIC18xCPU.TUnaryMinusPrimary_CreateFromExpression (exp: TExpression): TUnaryMinusPrimary;
   begin
      result := TUnaryMinusPrimary.CreateFromExpression (exp)
   end;

function TDummyPIC18xCPU.TUnaryMinusPrimary_CreateFromSourceTokens: TUnaryMinusPrimary;
   begin
      result := TUnaryMinusPrimary.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TUntilStatement_CreateFromSourceTokens: TUntilStatement;
   begin
      result := TUntilStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TVariable_Create (_id_idx: TIdentifierIdx; _context: TDefinition;_typ: TTypeDef; _definition_prefix: TVariableDescriptor; _mode: TAddressMode): TVariable;
   begin
      result := TVariable.Create (_id_idx, _context, _typ, _definition_prefix, _mode)
   end;

function TDummyPIC18xCPU.TVariable_CreateAnonymousROMString (_s: string): TVariable;
   begin
      result := TVariable.CreateAnonymousROMString (_s)
   end;

function TDummyPIC18xCPU.TVariable_CreateCopy (v: TVariable): TVariable;
   begin
      result := TVariable.CreateCopy (v)
   end;

function TDummyPIC18xCPU.TVariable_CreateForLaterDefinition (_id_idx: TIdentifierIdx; _context: TDefinition): TVariable;
   begin
      result := TVariable.CreateForLaterDefinition (_id_idx, _context)
   end;

function TDummyPIC18xCPU.TVariableAccessPrimary_Create (acc: TAccess): TVariableAccessPrimary;
   begin
      result := TVariableAccessPrimary.Create (acc)
   end;

function TDummyPIC18xCPU.TWhileStatement_CreateFromSourceTokens: TWhileStatement;
   begin
      result := TWhileStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TWithStatement_CreateFromSourceTokens: TWithStatement;
   begin
      result := TWithStatement.CreateFromSourceTokens
   end;

function TDummyPIC18xCPU.TWithStatement_CreateFromSourceTokensStartingAtVariable: TWithStatement;
   begin
      result := TWithStatement.CreateFromSourceTokensStartingAtVariable
   end;

function TDummyPIC18xCPU.TCallRecord_Create (called_name: string; called_access: TDefinition; call_type: TCallType): TCallRecord;
   begin
      result := TCallRecord.Create (called_name, called_access, call_type)
   end;


