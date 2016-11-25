function TTestCPU.TAbsFunctionPrimary_CreateFromSourceTokens: TAbsFunctionPrimary;
   begin
      result := TAbsFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TAccess_CreateFromSourceTokens: TAccess;
   begin
      result := TAccess.CreateFromSourceTokens
   end;

function TTestCPU.TAccess_CreateFromVariable (v: TVariable): TAccess;
   begin
      result := TAccess.CreateFromVariable (v)
   end;

function TTestCPU.TAssertStatement_CreateFromSourceTokens: TAssertStatement;
   begin
      result := TAssertStatement.CreateFromSourceTokens
   end;

function TTestCPU.TAssignmentStatement_CreateFromSourceTokens (acc: TAccess): TAssignmentStatement;
   begin
      result := TAssignmentStatement.CreateFromSourceTokens (acc)
   end;

function TTestCPU.TAwaitInterruptStatement_CreateFromSourceTokens: TAwaitInterruptStatement;
   begin
      result := TAwaitInterruptStatement.CreateFromSourceTokens
   end;

function TTestCPU.TCaseStatement_CreateFromSourceTokens: TCaseStatement;
   begin
      result := TCaseStatement.CreateFromSourceTokens
   end;

function TTestCPU.TChrTypeConversionPrimary_CreateFromSourceTokens: TChrTypeConversionPrimary;
   begin
      result := TChrTypeConversionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TConstantPrimary_CreateFromConstant (_constant: TConstant; _src_loc: TSourceLocation): TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromConstant (_constant, _src_loc)
   end;

function TTestCPU.TConstantPrimary_CreateFromIdentifierConstant: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromIdentifierConstant
   end;

function TTestCPU.TConstantPrimary_CreateFromReservedWordConstant: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromReservedWordConstant
   end;

function TTestCPU.TConstantPrimary_CreateFromSourceTokens: TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TConstantPrimary_CreateFromStructuredConstantAccess (acc: TAccess): TConstantPrimary;
   begin
      result := TConstantPrimary.CreateFromStructuredConstantAccess (acc)
   end;

function TTestCPU.TContinueStatement_CreateFromSourceTokens: TContinueStatement;
   begin
      result := TContinueStatement.CreateFromSourceTokens
   end;

function TTestCPU.TCycleStatement_CreateFromSourceTokens: TCycleStatement;
   begin
      result := TCycleStatement.CreateFromSourceTokens
   end;

function TTestCPU.TDataItemList_Create: TDataItemList;
   begin
      result := TDataItemList.Create
   end;

function TTestCPU.TDelayStatement_CreateFromSourceTokens: TDelayStatement;
   begin
      result := TDelayStatement.CreateFromSourceTokens
   end;

function TTestCPU.TEmptyFunctionPrimary_CreateFromSourceTokens: TEmptyFunctionPrimary;
   begin
      result := TEmptyFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TExitLoopStatement_CreateFromSourceTokens: TExitLoopStatement;
   begin
      result := TExitLoopStatement.CreateFromSourceTokens
   end;

function TTestCPU.TForStatement_CreateFromSourceTokens: TForStatement;
   begin
      result := TForStatement.CreateFromSourceTokens
   end;

function TTestCPU.TFunctionAccessPrimary_CreateFromSourceTokens (acc: TAccess): TFunctionAccessPrimary;
   begin
      result := TFunctionAccessPrimary.CreateFromSourceTokens (acc)
   end;

function TTestCPU.TIfStatement_CreateFromSourceTokens: TIfStatement;
   begin
      result := TIfStatement.CreateFromSourceTokens
   end;

function TTestCPU.TInitStatement_CreateFromSourceTokens: TInitStatement;
   begin
      result := TInitStatement.CreateFromSourceTokens
   end;

function TTestCPU.TLoopStatement_CreateFromSourceTokens: TLoopStatement;
   begin
      result := TLoopStatement.CreateFromSourceTokens
   end;

function TTestCPU.TNotPrimary_CreateFromSourceTokens: TNotPrimary;
   begin
      result := TNotPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TParamList_CreateFromSourceTokens (context: TParamListContext; max_scope: integer): TParamList;
   begin
      result := TParamList.CreateFromSourceTokens (context, max_scope)
   end;

function TTestCPU.TParamList_CreatePropertyPseudoParamList (property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TParamList;
   begin
      result := TParamList.CreatePropertyPseudoParamList (property_id, property_id_src_loc, property_typdef)
   end;

function TTestCPU.TPredFunctionPrimary_CreateFromSourceTokens: TPredFunctionPrimary;
   begin
      result := TPredFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TProgram_CreateFromSourceTokens: TProgram;
   begin
      result := TProgram.CreateFromSourceTokens
   end;

function TTestCPU.TProperty_CreateFromSourceTokens (cntxt: TDefinition; entry_property: boolean): TProperty;
   begin
      result := TProperty.CreateFromSourceTokens (cntxt, entry_property)
   end;

function TTestCPU.TReCycleStatement_CreateFromSourceTokens: TReCycleStatement;
   begin
      result := TReCycleStatement.CreateFromSourceTokens
   end;

function TTestCPU.TRelationalExpression_CreateFromSourceTokens: TRelationalExpression;
   begin
      result := TRelationalExpression.CreateFromSourceTokens
   end;

function TTestCPU.TReLoopStatement_CreateFromSourceTokens: TReLoopStatement;
   begin
      result := TReLoopStatement.CreateFromSourceTokens
   end;

function TTestCPU.TRoundFunctionPrimary_CreateFromSourceTokens: TRoundFunctionPrimary;
   begin
      result := TRoundFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TRoutine_CreateFromSourceTokens (cntxt: TDefinition; entry_routine: boolean): TRoutine;
   begin
      result := TRoutine.CreateFromSourceTokens (cntxt, entry_routine)
   end;

function TTestCPU.TRoutine_CreatePropertyGetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TRoutine.CreatePropertyGetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TTestCPU.TRoutine_CreatePropertySetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TRoutine.CreatePropertySetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TTestCPU.TRoutineCallStatement_CreateFromSourceTokens (acc: TAccess): TRoutineCallStatement;
   begin
      result := TRoutineCallStatement.CreateFromSourceTokens (acc)
   end;

function TTestCPU.TRoutineCallStatement_CreatePropertySetterCall (acc: TAccess; exp: TExpression; _src_loc: TSourceLocation): TRoutineCallStatement;
   begin
      result := TRoutineCallStatement.CreatePropertySetterCall (acc, exp, _src_loc)
   end;

function TTestCPU.TSetConstructorPrimary_CreateFromSourceTokens: TSetConstructorPrimary;
   begin
      result := TSetConstructorPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TSimpleExpression_CreateFromSourceTokens: TSimpleExpression;
   begin
      result := TSimpleExpression.CreateFromSourceTokens
   end;

function TTestCPU.TStatementList_Create: TStatementList;
   begin
      result := TStatementList.Create
   end;

function TTestCPU.TStatementList_CreateFromSourceTokens (terminator: TBooleanFunctionOfObject; non_terminated_error_message: string; while_or_until_allowed: boolean): TStatementList;
   begin
      result := TStatementList.CreateFromSourceTokens (terminator, non_terminated_error_message, while_or_until_allowed)
   end;

function TTestCPU.TStrPosPrimary_Create (acc: TAccess): TStrPosPrimary;
   begin
      result := TStrPosPrimary.Create (acc)
   end;

function TTestCPU.TSuccFunctionPrimary_CreateFromSourceTokens: TSuccFunctionPrimary;
   begin
      result := TSuccFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TSystemType_CreateFromSourceTokens: TSystemType;
   begin
      result := TTestSystemType.CreateFromSourceTokens
   end;

function TTestCPU.TTerm_CreateFromSourceTokens: TTerm;
   begin
      result := TTerm.CreateFromSourceTokens
   end;

function TTestCPU.TTruncFunctionPrimary_CreateFromSourceTokens: TTruncFunctionPrimary;
   begin
      result := TTruncFunctionPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TTypeInfo_Create (_info: TTypeInfo): TTypeInfo;
   begin
      result := TTypeInfo.Create (_info)
   end;

function TTestCPU.TTypeInfo_Create (_parent: TDefinition): TTypeInfo;
   begin
      result := TTypeInfo.Create (_parent)
   end;

function TTestCPU.TUnaryMinusPrimary_CreateFromExpression (exp: TExpression): TUnaryMinusPrimary;
   begin
      result := TUnaryMinusPrimary.CreateFromExpression (exp)
   end;

function TTestCPU.TUnaryMinusPrimary_CreateFromSourceTokens: TUnaryMinusPrimary;
   begin
      result := TUnaryMinusPrimary.CreateFromSourceTokens
   end;

function TTestCPU.TUntilStatement_CreateFromSourceTokens: TUntilStatement;
   begin
      result := TUntilStatement.CreateFromSourceTokens
   end;

function TTestCPU.TVariable_Create (_id_idx: TIdentifierIdx; _context: TDefinition;_typ: TTypeDef; _definition_prefix: TVariableDescriptor; _mode: TAddressMode): TVariable;
   begin
      result := TVariable.Create (_id_idx, _context, _typ, _definition_prefix, _mode)
   end;

function TTestCPU.TVariable_CreateAnonymousROMString (_s: string): TVariable;
   begin
      result := TVariable.CreateAnonymousROMString (_s)
   end;

function TTestCPU.TVariable_CreateCopy (v: TVariable): TVariable;
   begin
      result := TVariable.CreateCopy (v)
   end;

function TTestCPU.TVariable_CreateForLaterDefinition (_id_idx: TIdentifierIdx; _context: TDefinition): TVariable;
   begin
      result := TVariable.CreateForLaterDefinition (_id_idx, _context)
   end;

function TTestCPU.TVariableAccessPrimary_Create (acc: TAccess): TVariableAccessPrimary;
   begin
      result := TVariableAccessPrimary.Create (acc)
   end;

function TTestCPU.TWhileStatement_CreateFromSourceTokens: TWhileStatement;
   begin
      result := TWhileStatement.CreateFromSourceTokens
   end;

function TTestCPU.TWithStatement_CreateFromSourceTokens: TWithStatement;
   begin
      result := TWithStatement.CreateFromSourceTokens
   end;

function TTestCPU.TWithStatement_CreateFromSourceTokensStartingAtVariable: TWithStatement;
   begin
      result := TWithStatement.CreateFromSourceTokensStartingAtVariable
   end;

function TTestCPU.TCallRecord_Create (called_name: string; called_access: TDefinition; call_type: TCallType): TCallRecord;
   begin
      result := TCallRecord.Create (called_name, called_access, call_type)
   end;

