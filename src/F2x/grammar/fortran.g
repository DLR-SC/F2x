/* fortran.g - This is a FORTRAN 2008 grammar for use with PLYPlus.
 *
 * It is heavily based on the grammar provided by the OpenFortranProject.
 *
 * Copyright (C) 2015, DLR e.V.
 */
 
/*********************************************************************************************************************/
/* Part Ia: Tokens - Extracted from FortranLexer.g                                                                   */
/*********************************************************************************************************************/
// Support for language extension points
T_NO_LANGUAGE_EXTENSION
    : '$no language extension' ;   // shouldn't be recognized

T_EOL
    : '\r?\n' (%newline)
    ;

CONTINUE_CHAR
    : '&\r?\n' (%newline) (%ignore)
    ;

T_CHAR_CONSTANT
    : '((\'[^\']*\')+)|(("[^"]*")+)'
    ;

T_DIGIT_STRING
    : '[0-9]+'
    ;

BINARY_CONSTANT
    : '([bB]\'[01]+\')|([bB]"[01]+")'
    ;

OCTAL_CONSTANT
    : '([oO]\'[0-7]+\')|([oO]"[0-7]+")'
    ;

HEX_CONSTANT
    : '([zZ]\'[0-9a-fA-F]+\')|([zZ]"[0-9a-fA-F]+")'
    ;

WS
    : '[ \r\t\u000C]' (%ignore)
    ;

PREPROCESS_LINE
    : '\#[^\n\r]*' (%ignore)
    ;

T_ASTERISK      : '\*'  ;
T_COLON         : ':'   ;
T_COLON_COLON   : '::'  ;
T_COMMA         : ','   ;
T_EQUALS        : '='   ;
T_EQ_EQ         : '=='  ;
T_EQ_GT         : '=>'  ;
T_GREATERTHAN   : '>'   ;
T_GREATERTHAN_EQ: '>='  ;
T_LESSTHAN      : '<'   ;
T_LESSTHAN_EQ   : '<='  ;
T_LBRACKET      : '\['  ;
T_LPAREN        : '\('  ;
T_MINUS         : '-'   ;
T_PERCENT       : '%'   ;
T_PLUS          : '\+'  ;
T_POWER         : '\*\*';
T_SLASH         : '/'   ;
T_SLASH_EQ      : '/='  ;
T_SLASH_SLASH   : '//'  ;
T_RBRACKET      : ']'   ;
T_RPAREN        : '\)'  ;
T_UNDERSCORE    : '_'   ;
T_SEMICOLON     : ';'   ;

T_DEFINED_OP : '\.[a-zA-Z]+\.' (%unless
    T_EQ            : '\.EQ\.' ;
    T_NE            : '\.NE\.' ;
    T_LT            : '\.LT\.' ;
    T_LE            : '\.LE\.' ;
    T_GT            : '\.GT\.' ;
    T_GE            : '\.GE\.' ;

    T_TRUE          : '\.TRUE\.'  ;
    T_FALSE         : '\.FALSE\.' ;

    T_NOT           : '\.NOT\.' ;
    T_AND           : '\.AND\.' ;
    T_OR            : '\.OR\.'  ;
    T_EQV           : '\.EQV\.' ;
    T_NEQV          : '\.NEQV\.';
);

T_PERIOD_EXPONENT 
    : '(\.[0-9]+[EedD][+-]?[0-9]+)|(\.[EedD][+-]?[0-9]+)|(\.[0-9]+)|([0-9]+[eEdD][+-]?[0-9]+)'
    ;

T_HOLLERITH : '$no hollerith'; // shouldn't be recognized

LINE_COMMENT
    : '![^\n\r]*' (%ignore);


/*********************************************************************************************************************/
/* Part Ib: Keywords - Extracted from FortranLexer.g, collected for disambiguation.                                  */
/*********************************************************************************************************************/
T_IDENT : '[a-zA-Z][a-zA-Z0-9_]*' (%unless
    T_INTEGER       : '(?i)INTEGER'       ;
    T_REAL          : '(?i)REAL'          ;
    T_COMPLEX       : '(?i)COMPLEX'       ;
    T_CHARACTER     : '(?i)CHARACTER'     ;
    T_LOGICAL       : '(?i)LOGICAL'       ;

    T_ABSTRACT      : '(?i)ABSTRACT'      ;
    T_ALLOCATABLE   : '(?i)ALLOCATABLE'   ;
    T_ALLOCATE      : '(?i)ALLOCATE'      ;
    T_ASSIGNMENT    : '(?i)ASSIGNMENT'    ;
    // ASSIGN statements are a deleted feature.
    T_ASSIGN        : '(?i)ASSIGN'        ;
    T_ASSOCIATE     : '(?i)ASSOCIATE'     ;
    T_ASYNCHRONOUS  : '(?i)ASYNCHRONOUS'  ;
    T_BACKSPACE     : '(?i)BACKSPACE'     ;
    T_BLOCK         : '(?i)BLOCK'         ;
    T_BLOCKDATA     : '(?i)BLOCKDATA'     ;
    T_CALL          : '(?i)CALL'          ;
    T_CASE          : '(?i)CASE'          ;
    T_CLASS         : '(?i)CLASS'         ;
    T_CLOSE         : '(?i)CLOSE'         ;
    T_CODIMENSION   : '(?i)CODIMENSION'   ;
    T_COMMON        : '(?i)COMMON'        ;
    T_CONCURRENT    : '(?i)CONCURRENT'    ;
    T_CONTAINS      : '(?i)CONTAINS'      ;
    T_CONTIGUOUS    : '(?i)CONTIGUOUS'    ;
    T_CONTINUE      : '(?i)CONTINUE'      ;
    T_CYCLE         : '(?i)CYCLE'         ;
    T_DATA          : '(?i)DATA'          ;
    T_DEFAULT       : '(?i)DEFAULT'       ;
    T_DEALLOCATE    : '(?i)DEALLOCATE'    ;
    T_DEFERRED      : '(?i)DEFERRED'      ;
    T_DO            : '(?i)DO'            ;
    T_DOUBLEPRECISION: '(?i)DOUBLEPRECISION';
    T_DOUBLECOMPLEX:  '(?i)DOUBLECOMPLEX' ;
    T_ELEMENTAL     : '(?i)ELEMENTAL'     ;
    T_ELSE          : '(?i)ELSE'          ;
    T_ELSEIF        : '(?i)ELSEIF'        ;
    T_ELSEWHERE     : '(?i)ELSEWHERE'     ;
    T_ENTRY         : '(?i)ENTRY'         ;
    T_ENUM          : '(?i)ENUM'          ;
    T_ENUMERATOR    : '(?i)ENUMERATOR'    ;
    T_EQUIVALENCE   : '(?i)EQUIVALENCE'   ;
    T_EXIT          : '(?i)EXIT'          ;
    T_EXTENDS       : '(?i)EXTENDS'       ;
    T_EXTERNAL      : '(?i)EXTERNAL'      ;
    T_FILE          : '(?i)FILE'          ;
    T_FINAL         : '(?i)FINAL'         ;
    T_FLUSH         : '(?i)FLUSH'         ;
    T_FORALL        : '(?i)FORALL'        ;
    T_FORMAT        : '(?i)FORMAT'        ; /* TODO inFormat? */
    T_FORMATTED     : '(?i)FORMATTED'     ;
    T_FUNCTION      : '(?i)FUNCTION'      ;
    T_GENERIC       : '(?i)GENERIC'       ;
    T_GO            : '(?i)GO'            ;
    T_GOTO          : '(?i)GOTO'          ;
    T_IF            : '(?i)IF'            ;
    T_IMPLICIT      : '(?i)IMPLICIT'      ;
    T_IMPORT        : '(?i)IMPORT'        ;
    T_IMPURE        : '(?i)IMPURE'        ;
    T_IN            : '(?i)IN'            ;
    T_INOUT         : '(?i)INOUT'         ;
    T_INTENT        : '(?i)INTENT'        ;
    T_INTERFACE     : '(?i)INTERFACE'     ;
    T_INTRINSIC     : '(?i)INTRINSIC'     ;
    T_INQUIRE       : '(?i)INQUIRE'       ;
    T_MODULE        : '(?i)MODULE'        ;
    T_NAMELIST      : '(?i)NAMELIST'      ;
    T_NONE          : '(?i)NONE'          ;
    T_NON_INTRINSIC : '(?i)NON_INTRINSIC' ;
    T_NON_OVERRIDABLE: '(?i)NON_OVERRIDABLE';
    T_NOPASS        : '(?i)NOPASS'        ;
    T_NULLIFY       : '(?i)NULLIFY'       ;
    T_ONLY          : '(?i)ONLY'          ;
    T_OPEN          : '(?i)OPEN'          ;
    T_OPERATOR      : '(?i)OPERATOR'      ;
    T_OPTIONAL      : '(?i)OPTIONAL'      ;
    T_OUT           : '(?i)OUT'           ;
    T_PARAMETER     : '(?i)PARAMETER'     ;
    T_PASS          : '(?i)PASS'          ;
    T_PAUSE         : '(?i)PAUSE'         ;
    T_POINTER       : '(?i)POINTER'       ;
    T_PRINT         : '(?i)PRINT'         ;
    T_PRIVATE       : '(?i)PRIVATE'       ;
    T_PROCEDURE     : '(?i)PROCEDURE'     ;
    T_PROGRAM       : '(?i)PROGRAM'       ;
    T_PROTECTED     : '(?i)PROTECTED'     ;
    T_PUBLIC        : '(?i)PUBLIC'        ;
    T_PURE          : '(?i)PURE'          ;
    T_READ          : '(?i)READ'          ;
    T_RECURSIVE     : '(?i)RECURSIVE'     ;
    T_RESULT        : '(?i)RESULT'        ;
    T_RETURN        : '(?i)RETURN'        ;
    T_REWIND        : '(?i)REWIND'        ;
    T_SAVE          : '(?i)SAVE'          ;
    T_SELECT        : '(?i)SELECT'        ;
    T_SELECTCASE    : '(?i)SELECTCASE'    ;
    T_SELECTTYPE    : '(?i)SELECTTYPE'    ;
    T_SEQUENCE      : '(?i)SEQUENCE'      ;
    T_STOP          : '(?i)STOP'          ;
    T_SUBMODULE     : '(?i)SUBMODULE'     ;
    T_SUBROUTINE    : '(?i)SUBROUTINE'    ;
    T_TARGET        : '(?i)TARGET'        ;
    T_THEN          : '(?i)THEN'          ;
    T_TO            : '(?i)TO'            ;
    T_TYPE          : '(?i)TYPE'          ;
    T_UNFORMATTED   : '(?i)UNFORMATTED'   ;
    T_USE           : '(?i)USE'           ;
    T_VALUE         : '(?i)VALUE'         ;
    T_VOLATILE      : '(?i)VOLATILE'      ;
    T_WAIT          : '(?i)WAIT'          ;
    T_WHERE         : '(?i)WHERE'         ;
    T_WHILE         : '(?i)WHILE'         ;
    T_WRITE         : '(?i)WRITE'         ;

    // these tokens (without blank characters) are from 3.3.2.2
    T_ENDASSOCIATE  : '(?i)ENDASSOCIATE'  ;
    T_ENDBLOCKDATA  : '(?i)ENDBLOCKDATA'  ;
    T_ENDDO         : '(?i)ENDDO'         ;
    T_ENDENUM       : '(?i)ENDENUM'       ;
    T_ENDFILE       : '(?i)ENDFILE'       ;
    T_ENDFORALL     : '(?i)ENDFORALL'     ;
    T_ENDFUNCTION   : '(?i)ENDFUNCTION'   ;
    T_ENDIF         : '(?i)ENDIF'         ;
    T_ENDMODULE     : '(?i)ENDMODULE'     ;
    T_ENDINTERFACE  : '(?i)ENDINTERFACE'  ;
    T_ENDPROCEDURE  : '(?i)ENDPROCEDURE'  ;
    T_ENDPROGRAM    : '(?i)ENDPROGRAM'    ;
    T_ENDSELECT     : '(?i)ENDSELECT'     ;
    T_ENDSUBMODULE  : '(?i)ENDSUBMODULE'  ;
    T_ENDSUBROUTINE : '(?i)ENDSUBROUTINE' ;
    T_ENDTYPE       : '(?i)ENDTYPE'       ;
    T_ENDWHERE      : '(?i)ENDWHERE'      ;
    T_DIMENSION     : '(?i)DIMENSION'     ;
    T_KIND          : '(?i)KIND'          ;
    T_LEN           : '(?i)LEN'           ;

    T_END           : '(?i)END';
    T_BIND          : '(?i)BIND'          ;
);


/*********************************************************************************************************************/
/* Part Ic : 'Virtual terminals' - TODO: Check if required.                                                          */
/*********************************************************************************************************************/
// extra, context-sensitive terminals that require communication between parser and scanner
// added the underscores so there is no way this could overlap w/ any valid
// idents in Fortran.  we just need this token to be defined so we can 
// create one of them while we're fixing up labeled do stmts.

T_LABEL_DO_TERMINAL : '__LABEL_DO_TERMINAL__' ;
T_LABEL_DO_TERMINAL_INSERTED : '__T_LABEL_DO_TERMINAL_INSERTED__' ;

T_DATA_EDIT_DESC : '__T_DATA_EDIT_DESC__' ;
T_CONTROL_EDIT_DESC : '__T_CONTROL_EDIT_DESC__' ;
T_CHAR_STRING_EDIT_DESC : '__T_CHAR_STRING_EDIT_DESC__' ;

T_STMT_FUNCTION : '__T_STMT_FUNCTION__' ;

T_ARITHMETIC_IF_STMT : '__T_ARITHMETIC_IF_STMT__' ;
T_WHERE_STMT : '__T_WHERE_STMT__' ;
T_WHERE_CONSTRUCT_STMT : '__T_WHERE_CONSTRUCT_STMT__' ;

T_EOF: '__T_EOF__' ;


/*********************************************************************************************************************/
/* Part IIa: FORTRAN 2008 Grammar / R2xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

/*
 * R201-F08 program
 *    is program-unit 
 *       [ program-unit ] ... 
 */
start
    : (end_of_stmt)* (program_unit)+
    ;

/*
 * R202-F08 program-unit
 *    is main-program
 *    or external-subprogram
 *    or module
 *    or submodule     // NEW_TO_2008
 *    or block-data
 */
program_unit
    : main_program
    | external_subprogram
    | module
    | submodule
    | block_data
    ;

/*
 * R203-F08 external-subprogram
 *    is function-subprogram 
 *    or subroutine-subprogram
 */
external_subprogram
    : function_subprogram
    | subroutine_subprogram
    ;

/*
 * R1101-F08 main-program
 *  is [ program-stmt ]
 *        [ specification-part ]
 *        [ execution-part ]
 *        [ internal-subprogram-part ]
 *        end-program-stmt
 */
main_program
   :   ( program_stmt )?
       specification_part
       ( execution_part )?
       ( internal_subprogram_part )?
       end_program_stmt
   ;

specification_part
    :   ( use_stmt )*
        ( import_stmt )*
        ( implicit_stmt )?
        ( declaration_construct )*
    ;

/*
 * R207-F08 declaration-construct
 *    is derived-type-def
 *    or entry-stmt
 *    or enum-def                      // NEW_NAME_2008 (was enum-alias-def)
 *    or format-stmt
 *    or interface-block
 *    or parameter-stmt
 *    or procedure-declaration-stmt
 *    or other-specification-stmt      // NEW_NAME_2008 (was specification-stmt)
 *    or type-declaration-stmt
 *    or stmt-function-stmt
 */
declaration_construct
   :   derived_type_def
   |   entry_stmt
   |   enum_def
   |   format_stmt
   |   interface_block
   |   parameter_stmt
   |   procedure_declaration_stmt
   |   other_specification_stmt
   |   type_declaration_stmt
   |   stmt_function_stmt
   ;

execution_part
    :   ( executable_construct )+
    ;

execution_part_construct
    :   executable_construct  
    |   format_stmt
    |   entry_stmt
    |   data_stmt
    ;

/*
 * R210-F08 internal-subprogram-part
 *    is contains-stmt
 *          [ internal-subprogram ] ...  // DIFFERENT_2008 (can have contains only)
 */
internal_subprogram_part
   :   contains_stmt
       ( internal_subprogram )*
   ;

internal_subprogram
    :   ( prefix )? function_subprogram
    |   subroutine_subprogram
    ;

/*
 * R212-F08 other-specification-stmt   // NEW_NAME_2008 (was specification-stmt)
 *    is access-stmt
 *    or allocatable-stmt
 *    or asynchronous-stmt
 *    or bind-stmt
 *    or codimension-stmt              // NEW_TO_2008
 *    or common-stmt
 *    or data-stmt
 *    or dimension-stmt
 *    or equivalence-stmt
 *    or external-stmt
 *    or intent-stmt
 *    or intrinsic-stmt
 *    or namelist-stmt
 *    or optional-stmt
 *    or pointer-stmt
 *    or protected-stmt
 *    or save-stmt
 *    or target-stmt
 *    or volatile-stmt
 *    or value-stmt
 */
other_specification_stmt
   :   access_stmt
   |   allocatable_stmt
   |   asynchronous_stmt
   |   bind_stmt
   |   codimension_stmt                // NEW_TO_2008
   |   common_stmt
   |   data_stmt
   |   dimension_stmt
   |   equivalence_stmt
   |   external_stmt
   |   intent_stmt
   |   intrinsic_stmt
   |   namelist_stmt
   |   optional_stmt
   |   pointer_stmt
   |   protected_stmt
   |   save_stmt
   |   target_stmt
   |   volatile_stmt
   |   value_stmt
   |   other_spec_stmt_extension
   ;

// language extension point
other_spec_stmt_extension : T_NO_LANGUAGE_EXTENSION ;

/*
 * R213-F08 executable-construct
 *    is action-stmt
 *    or associate-construct
 *    or block-construct               // NEW_TO_2008
 *    or case-construct
 *    or critical-construct            // NEW_TO_2008
 *    or do-construct
 *    or forall-construct
 *    or if-construct
 *    or select-type-construct
 *    or where-construct
 */
executable_construct
   :   action_stmt
   |   associate_construct
   |   case_construct
   |   do_construct
   |   forall_construct
   |   if_construct
   |   select_type_construct
   |   where_construct
   ;

/*
 * R214-F08 action-stmt
 *    is allocate-stmt
 *    or assignment-stmt
 *    or backspace-stmt
 *    or call-stmt
 *    or close-stmt
 *    or continue-stmt
 *    or cycle-stmt
 *    or deallocate-stmt
 *    or end-function-stmt
 *    or end-mp-subprogram-stmt        // NEW_TO_2008
 *    or end-program-stmt
 *    or end-subroutine-stmt
 *    or endfile-stmt
 *    or errorstop-stmt                // NEW_TO_2008
 *    or exit-stmt
 *    or flush-stmt
 *    or forall-stmt
 *    or goto-stmt
 *    or if-stmt
 *    or inquire-stmt
 *    or lock-stmt                     // NEW_TO_2008
 *    or nullify-stmt
 *    or open-stmt
 *    or pointer-assignment-stmt
 *    or print-stmt
 *    or read-stmt
 *    or return-stmt
 *    or rewind-stmt
 *    or stop-stmt
 *    or sync-all-stmt                 // NEW_TO_2008
 *    or sync-images-stmt              // NEW_TO_2008
 *    or sync-memory-stmt              // NEW_TO_2008
 *    or unlock-stmt                   // NEW_TO_2008
 *    or wait-stmt
 *    or where-stmt
 *    or write-stmt
 *    or arithmetic-if-stmt
 *    or computed-goto-stmt
 */
action_stmt
    :   allocate_stmt
    |   assignment_stmt
    |   backspace_stmt
    |   call_stmt
    |   close_stmt
    |   continue_stmt
    |   cycle_stmt
    |   deallocate_stmt
    |   endfile_stmt
    |   exit_stmt
    |   flush_stmt
    |   forall_stmt
    |   goto_stmt
    |   if_stmt
    |   inquire_stmt  
    |   nullify_stmt
    |   open_stmt
    |   pointer_assignment_stmt
    |   print_stmt
    |   read_stmt
    |   return_stmt
    |   rewind_stmt
    |   stop_stmt
    |   wait_stmt
    |   where_stmt
    |   write_stmt
    |   arithmetic_if_stmt
    |   computed_goto_stmt
    |   assign_stmt 
    |   assigned_goto_stmt
    |   pause_stmt
    ;

keyword
    :   name
    ;


/*********************************************************************************************************************/
/* Part IIb: FORTRAN 2008 Grammar / R3xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

name
    :   T_IDENT
    ;

literal_constant
   :   int_literal_constant
   |   real_literal_constant
   |   complex_literal_constant
   |   logical_literal_constant
   |   char_literal_constant
   |   boz_literal_constant
   |   hollerith_literal_constant  // deleted in F77
   ;

char_constant
    :   char_literal_constant
    |   T_IDENT
    ;

intrinsic_operator
    :   power_op
    |   mult_op
    |   add_op
    |   concat_op
    |   rel_op
    |   not_op
    |   and_op
    |   or_op
    |   equiv_op
    ;

defined_operator
    :   T_DEFINED_OP            
    |   extended_intrinsic_op   
    ;

extended_intrinsic_op
    :   intrinsic_operator
    ;

label
    : T_DIGIT_STRING
    ;

label_list
    :       label 
            ( T_COMMA label )*
    ;


/*********************************************************************************************************************/
/* Part IIc: FORTRAN 2008 Grammar / R4xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

type_spec
    :   intrinsic_type_spec
    |   derived_type_spec
    ;

type_param_value
    :   expr
    |   T_ASTERISK
    |   T_COLON
    ;

intrinsic_type_char : T_CHARACTER (char_selector)?;

intrinsic_type_kind
    : T_INTEGER
    | T_REAL
    | T_COMPLEX
    | T_LOGICAL
    ;

intrinsic_type_spec
    :   intrinsic_type_kind (kind_selector)?
    |   T_DOUBLEPRECISION
    |   T_DOUBLECOMPLEX
    |   intrinsic_type_char (char_selector)?
    ;

kind_selector
    : T_LPAREN (T_KIND T_EQUALS)? expr T_RPAREN
    | T_ASTERISK T_DIGIT_STRING
    ;

signed_int_literal_constant
    :   (T_PLUS | T_MINUS)?
        int_literal_constant
    ;

int_literal_constant
    :   T_DIGIT_STRING (T_UNDERSCORE kind_param)?
    ;

kind_param
    :   T_DIGIT_STRING 
    |   T_IDENT 
    ;

boz_literal_constant
    :   BINARY_CONSTANT
    |   OCTAL_CONSTANT
    |   HEX_CONSTANT
    ;

signed_real_literal_constant
    :   (T_PLUS | T_MINUS)?
        real_literal_constant
    ;

real_constant_number
    : T_DIGIT_STRING T_PERIOD_EXPONENT
    ;

real_literal_constant
//    :   T_REAL_CONSTANT (T_UNDERSCORE kind_param)? 
    :   real_constant_number (T_UNDERSCORE kind_param)? 
    ;

complex_literal_constant
    :   T_LPAREN real_part T_COMMA imag_part T_RPAREN
    ;

real_part
    :   signed_int_literal_constant  
    |   signed_real_literal_constant 
    |   T_IDENT                      
    ;

imag_part
    :   signed_int_literal_constant     
    |   signed_real_literal_constant    
    |   T_IDENT                         
    ;

char_selector
   :   T_ASTERISK char_length (T_COMMA)?
   |   T_LPAREN type_param_value
       ( T_COMMA (T_KIND T_EQUALS)? expr )?
       T_RPAREN
   |   T_LPAREN T_LEN T_EQUALS type_param_value
       ( T_COMMA T_KIND T_EQUALS expr )?
       T_RPAREN
   |   T_LPAREN T_KIND T_EQUALS expr
       ( T_COMMA (T_LEN T_EQUALS)? type_param_value )?
       T_RPAREN
   ;

char_length
   :   T_LPAREN type_param_value T_RPAREN
   |   scalar_int_literal_constant
   ;

scalar_int_literal_constant
   :   int_literal_constant
   ;

char_literal_constant
   :   T_DIGIT_STRING T_UNDERSCORE T_CHAR_CONSTANT
   |   T_IDENT T_CHAR_CONSTANT
   |   T_CHAR_CONSTANT
   ;

hollerith_literal_constant
    :   T_HOLLERITH
    ;

// R428
logical_literal_constant
    :   T_TRUE ( T_UNDERSCORE kind_param )?
    |   T_FALSE ( T_UNDERSCORE kind_param )?
    ;

derived_type_def
    :   derived_type_stmt
        type_param_or_comp_def_stmt_list  
        ( private_or_sequence )*
        ( component_def_stmt )*
        ( type_bound_procedure_part )?
        end_type_stmt
    ;

type_param_or_comp_def_stmt_list
    :   (kind_selector)? T_COMMA type_param_or_comp_def_stmt
            type_param_or_comp_def_stmt_list
    |
    ;

type_param_or_comp_def_stmt
    :   type_param_attr_spec T_COLON_COLON type_param_decl_list end_of_stmt 
    |   component_attr_spec_list T_COLON_COLON component_decl_list end_of_stmt 
    ;

derived_type_stmt
    :   (label)? T_TYPE
        ( ( T_COMMA type_attr_spec_list )? 
            T_COLON_COLON )? name
            ( T_LPAREN generic_name_list T_RPAREN )?
            end_of_stmt
    ;

type_attr_spec_list
    :   type_attr_spec ( T_COMMA type_attr_spec )*
    ;

generic_name_list
    :   T_IDENT
        ( T_COMMA T_IDENT )*
    ;

type_attr_spec
    :   access_spec
    |   T_EXTENDS T_LPAREN T_IDENT T_RPAREN
    |   T_ABSTRACT
    |   T_BIND T_LPAREN T_IDENT /* 'C' */ T_RPAREN
    ;

private_or_sequence
    :   private_components_stmt
    |   sequence_stmt
    ;

end_type_stmt
    : (label)? T_ENDTYPE ( T_IDENT )? end_of_stmt
    ;

sequence_stmt
    :   (label)? T_SEQUENCE end_of_stmt
    ;

type_param_decl
    :    T_IDENT ( T_EQUALS expr )?
    ;

type_param_decl_list
    :   type_param_decl ( T_COMMA type_param_decl )*
    ;

/*
 * R437-F08 component-attr-spec
 *    is access-spec
 *    or ALLOCATABLE
 *    or CODIMENSION lbracket coarray-spec rbracket  // NEW_TO_2008
 *    or CONTIGUOUS                                  // NEW_TO_2008
 *    or DIMENSION ( component-array-spec )
 *    or POINTER
 */
component_attr_spec
   :   access_spec
   |   T_ALLOCATABLE
   |   T_CODIMENSION T_LBRACKET coarray_spec T_RBRACKET          // NEW_TO_2008 
   |   T_CONTIGUOUS                                              // NEW_TO_2008 
   |   T_DIMENSION T_LPAREN component_array_spec T_RPAREN
   |   T_POINTER
   |   component_attr_spec_extension
  ;
  
component_attr_spec_extension : T_NO_LANGUAGE_EXTENSION ;

component_attr_spec_list
    :   component_attr_spec ( T_COMMA component_attr_spec )*
    ;

type_param_attr_spec
    :   T_IDENT /* { KIND | LEN } */ 
    ;

component_def_stmt
    :   data_component_def_stmt
    |   proc_component_def_stmt
    ;

data_component_def_stmt
    :    (label)? declaration_type_spec 
            ( ( T_COMMA component_attr_spec_list )? 
            T_COLON_COLON )? component_decl_list end_of_stmt
    ;

/*
 * R438-F08 component-decl
 *    is component-name [ ( component-array-spec ) ]
 *                      [ lbracket coarray-spec rbracket ]  // NEW_TO_2008
 *                      [ * char-length ] [ component-initialization ]
 */
component_decl
   :   name (T_LPAREN component_array_spec T_RPAREN )?
               (T_LBRACKET coarray_spec T_RBRACKET )?
               (T_ASTERISK char_length )? 
               (component_initialization )?
   ;

component_decl_list
   :   component_decl ( T_COMMA component_decl )*
   ;

component_array_spec
    :   explicit_shape_spec_list
    |   deferred_shape_spec_list
    ;

deferred_shape_spec_list
    :   T_COLON ( T_COMMA T_COLON )*
    ;

component_initialization
    :   T_EQUALS expr
    |   T_EQ_GT null_init
    ;

proc_component_def_stmt
    :   (label)? T_PROCEDURE T_LPAREN 
            ( proc_interface )? T_RPAREN T_COMMA
            proc_component_attr_spec_list T_COLON_COLON proc_decl_list 
            end_of_stmt
    ;

proc_component_attr_spec
    :    T_POINTER
    |    T_PASS ( T_LPAREN T_IDENT T_RPAREN )?
    |    T_NOPASS
    |    access_spec
    ;

proc_component_attr_spec_list
    :   proc_component_attr_spec 
            ( T_COMMA proc_component_attr_spec)*
    ;

private_components_stmt
    :   (label)? T_PRIVATE end_of_stmt
    ;

type_bound_procedure_part
   :   contains_stmt
       ( binding_private_stmt )?
       proc_binding_stmt ( proc_binding_stmt )*
   ;

binding_private_stmt
    :   (label)? T_PRIVATE end_of_stmt
    ;

proc_binding_stmt
    :   (label)? specific_binding end_of_stmt
    |   (label)? generic_binding end_of_stmt
    |   (label)? final_binding end_of_stmt
    ;

specific_binding
    :   T_PROCEDURE (T_LPAREN T_IDENT T_RPAREN)?
            ( ( T_COMMA binding_attr_list )? 
                T_COLON_COLON )?
            ( T_EQ_GT T_IDENT)?
    ;

generic_binding
    :    T_GENERIC ( T_COMMA access_spec )? T_COLON_COLON 
            generic_spec T_EQ_GT generic_name_list
    ;

binding_attr
    : T_PASS ( T_LPAREN T_IDENT T_RPAREN )?
    | T_NOPASS          
    | T_NON_OVERRIDABLE 
    | T_DEFERRED        
    | access_spec       
    ;

binding_attr_list
    :   binding_attr ( T_COMMA binding_attr )*
    ;

final_binding
    :   T_FINAL ( T_COLON_COLON )? generic_name_list 
    ;

derived_type_spec
    : name ( T_LPAREN type_param_spec_list T_RPAREN )?
    ;

type_param_spec
    : ( keyword T_EQUALS )? type_param_value
    ;

type_param_spec_list
    :   type_param_spec ( T_COMMA type_param_spec )*
    ;

structure_constructor
    : T_IDENT T_LPAREN type_param_spec_list T_RPAREN
        (T_LPAREN
        ( component_spec_list )?
        T_RPAREN)?
    ;

component_spec
    :   ( keyword T_EQUALS )? component_data_source
    ;

component_spec_list
    :   component_spec ( T_COMMA component_spec )*
    ;

component_data_source
    :   expr 
    ;

enum_def
    :   enum_def_stmt
        enumerator_def_stmt
        ( enumerator_def_stmt )*
        end_enum_stmt
    ;

enum_def_stmt
    :   (label)? T_ENUM T_COMMA T_BIND T_LPAREN 
            T_IDENT /* 'C' */ T_RPAREN end_of_stmt
    ;

enumerator_def_stmt
    :   (label)? T_ENUMERATOR ( T_COLON_COLON )? 
            enumerator_list end_of_stmt
    ;

enumerator
    :   T_IDENT ( T_EQUALS expr )?
    ;

enumerator_list
    :   enumerator ( T_COMMA enumerator )*
    ;

end_enum_stmt
    :   (label)? T_ENDENUM end_of_stmt 
    ;

array_constructor
    :   T_LPAREN T_SLASH ac_spec T_SLASH T_RPAREN
    |   T_LBRACKET ac_spec T_RBRACKET
    ;

ac_spec
    : type_spec T_COLON_COLON (ac_value_list)?
    | ac_value_list
    ;

ac_value
    :   expr
    |   ac_implied_do
    ;

ac_value_list
    :   ac_value ( T_COMMA ac_value )*
    ;

ac_implied_do
    :   T_LPAREN ac_value_list T_COMMA ac_implied_do_control T_RPAREN
    ;

ac_implied_do_control
    :    do_variable T_EQUALS expr T_COMMA expr ( T_COMMA expr )?
    ;

scalar_int_variable
    :   variable
    ;


/*********************************************************************************************************************/
/* Part IId: FORTRAN 2008 Grammar / R5xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

type_declaration_stmt
   :   (label)? declaration_type_spec
       ( (T_COMMA attr_spec )* T_COLON_COLON )?
       entity_decl_list end_of_stmt
   ;

declaration_type_spec
    :   intrinsic_type_spec
    |   T_TYPE T_LPAREN derived_type_spec T_RPAREN
    |   T_CLASS T_LPAREN derived_type_spec T_RPAREN
    |   T_CLASS T_LPAREN T_ASTERISK T_RPAREN
    ;

/*
 * R502-F08 attr-spec
 *    is access-spec
 *    or ALLOCATABLE
 *    or ASYNCHRONOUS
 *    or CODIMENSION lbracket coarray-spec rbracket  // NEW_TO_2008
 *    or CONTIGUOUS                                  // NEW_TO_2008
 *    or DIMENSION ( array-spec )
 *    or EXTERNAL
 *    or INTENT ( intent-spec )
 *    or INTRINSIC
 *    or language-binding-spec
 *    or OPTIONAL
 *    or PARAMETER
 *    or POINTER
 *    or PROTECTED
 *    or SAVE
 *    or TARGET
 *    or VALUE
 *    or VOLATILE
 */
attr_spec
   :   access_spec
   |   T_ALLOCATABLE
   |   T_ASYNCHRONOUS
   |   T_CODIMENSION T_LBRACKET coarray_spec T_RBRACKET  // NEW_TO_2008
   |   T_CONTIGUOUS                                      // NEW_TO_2008
   |   T_DIMENSION T_LPAREN array_spec T_RPAREN
   |   T_EXTERNAL
   |   T_INTENT T_LPAREN intent_spec T_RPAREN
   |   T_INTRINSIC
   |   language_binding_spec        
   |   T_OPTIONAL
   |   T_PARAMETER
   |   T_POINTER
   |   T_PROTECTED
   |   T_SAVE
   |   T_TARGET
   |   T_VALUE
   |   T_VOLATILE
   |   T_KIND
   |   T_LEN
   |   attr_spec_extension
   ;
    
// language extension point
attr_spec_extension : T_NO_LANGUAGE_EXTENSION ;

/*
 * R503-F08 entity-decl
 *    is object-name [( array-spec )]
 *         [ lracket coarray-spec rbracket ]
 *         [ * char-length ] [ initialization ]
 *    or function-name [ * char-length ]
 */
entity_decl
   :   T_IDENT ( T_LPAREN array_spec T_RPAREN )?
               ( T_LBRACKET coarray_spec T_RBRACKET )?
               ( T_ASTERISK char_length )?
               ( initialization )?
   ;

entity_decl_list
    :   entity_decl ( T_COMMA entity_decl )*
    ;

/*
 * R505-F03 object-name
 *    is name
 */
object_name
   :   T_IDENT
   ;

initialization
    :   T_EQUALS expr
    |   T_EQ_GT null_init
    ;

null_init
    :   T_IDENT /* 'NULL' */ T_LPAREN T_RPAREN
    ;

/*
 * R509-F08 coarray-spec
 *    is deferred-coshape-spec-list
 *    or explicit-coshape-spec
 */
coarray_spec
   :   array_spec_element (T_COMMA array_spec_element )*
   ;

access_spec
    :   T_PUBLIC
    |   T_PRIVATE
    ;

language_binding_spec
    :   T_BIND T_LPAREN T_IDENT /* 'C' */ 
            (T_COMMA name T_EQUALS expr)? T_RPAREN
    ;

array_spec
    :   array_spec_element
        (T_COMMA array_spec_element)*
    ;

array_spec_element
    :   expr ( T_COLON ( expr | T_ASTERISK )? )?
    |   T_ASTERISK
    |   T_COLON
    ;

explicit_shape_spec
    :   expr (T_COLON expr)?
    ;

explicit_shape_spec_list
    :   explicit_shape_spec  
            ( T_COMMA explicit_shape_spec )*
    ;

intent_spec
    :   T_IN
    |   T_OUT
    |   T_IN T_OUT
    |   T_INOUT
    ;

access_stmt
    :    (label)? access_spec ( ( T_COLON_COLON )? 
            access_id_list )? end_of_stmt
    ;

access_id
    :   generic_spec
    ;

access_id_list
    :   access_id ( T_COMMA access_id )*
    ;

allocatable_stmt
   :   (label)?
       T_ALLOCATABLE ( T_COLON_COLON )? allocatable_decl_list end_of_stmt
   ;

/*
 * R527-F08 allocatable-decl
 *    is object-name [ ( array-spec ) ] [ lbracket ( coarray-spec ) ]
 */
allocatable_decl
   :   object_name
          ( T_LPAREN array_spec T_RPAREN )?
          ( T_LBRACKET coarray_spec T_RBRACKET )?
   ;

allocatable_decl_list
   :   allocatable_decl ( T_COMMA allocatable_decl )*
   ;

asynchronous_stmt
    :   (label)? T_ASYNCHRONOUS ( T_COLON_COLON )?
        generic_name_list end_of_stmt
    ;

bind_stmt
    :   (label)? language_binding_spec
        ( T_COLON_COLON )? bind_entity_list end_of_stmt
    ;

bind_entity
    :   T_IDENT 
    |   T_SLASH T_IDENT T_SLASH 
    ;

bind_entity_list
    :   bind_entity ( T_COMMA bind_entity )*
    ;

data_stmt
    :   (label)? T_DATA data_stmt_set ( ( T_COMMA )? 
            data_stmt_set )* end_of_stmt
    ;

data_stmt_set
    :   data_stmt_object_list
        T_SLASH
        data_stmt_value_list
        T_SLASH
    ;

data_stmt_object
    :   variable
    |   data_implied_do
    ;

data_stmt_object_list
    :   data_stmt_object ( T_COMMA data_stmt_object )*
    ;

data_implied_do
    : T_LPAREN data_i_do_object_list T_COMMA T_IDENT T_EQUALS
        expr T_COMMA expr ( T_COMMA expr )? T_RPAREN
    ;

data_i_do_object
    :   data_ref
    |   data_implied_do
    ;

data_i_do_object_list
    :   data_i_do_object ( T_COMMA data_i_do_object )*
    ;

data_stmt_value
   :   designator (T_ASTERISK data_stmt_constant)?
   |   int_literal_constant (T_ASTERISK data_stmt_constant)?
   |   signed_real_literal_constant
   |   signed_int_literal_constant
   |   complex_literal_constant
   |   logical_literal_constant
   |   char_literal_constant
   |   boz_literal_constant
   |   structure_constructor       // is null_init if 'NULL()'
   |   hollerith_literal_constant  // deleted in F77
   ;

data_stmt_value_list
    :   data_stmt_value ( T_COMMA data_stmt_value )*
    ;

data_stmt_constant
    :   designator
    |   signed_int_literal_constant
    |   signed_real_literal_constant
    |   complex_literal_constant
    |   logical_literal_constant
    |   char_literal_constant
    |   boz_literal_constant
    |   structure_constructor // is null_init if 'NULL()'
    ;

/*
 * R531-F08 codimension-stmt
 *    is CODIMENSION [ :: ] codimension-decl-list
 */
codimension_stmt
   :   (label)?
       T_CODIMENSION ( T_COLON_COLON )? codimension_decl_list end_of_stmt
   ;
   
/*
 * R532-08 codimension-decl
 *    is coarray-name lbracket coarray-spec rbracket
 */
codimension_decl
   :   T_IDENT T_LBRACKET coarray_spec T_RBRACKET
   ;

codimension_decl_list
   :   codimension_decl ( T_COMMA codimension_decl )*
   ;

dimension_stmt
    :   (label)? T_DIMENSION ( T_COLON_COLON )? 
        dimension_decl ( T_COMMA dimension_decl )* end_of_stmt
    ;

dimension_decl
   :   T_IDENT T_LPAREN array_spec T_RPAREN
   ;

intent_stmt
    :   (label)? T_INTENT T_LPAREN intent_spec T_RPAREN 
            ( T_COLON_COLON )? generic_name_list end_of_stmt
    ;

optional_stmt
    :   (label)? T_OPTIONAL ( T_COLON_COLON )? 
            generic_name_list end_of_stmt
    ;

parameter_stmt
    :   (label)? T_PARAMETER T_LPAREN 
            named_constant_def_list T_RPAREN end_of_stmt
    ;

named_constant_def_list
    :   named_constant_def 
            ( T_COMMA named_constant_def )*
    ;

named_constant_def
    :   T_IDENT T_EQUALS expr
    ;

/*
 * R550-F08
 *    is POINTER [ :: ] pointer-decl-list
 */
pointer_stmt
   :   (label)? T_POINTER
       ( cray_pointer_assoc_list |
         ( ( T_COLON_COLON )? pointer_decl_list )
       ) end_of_stmt
   ;

pointer_decl_list
   :   pointer_decl ( T_COMMA pointer_decl )*
   ;

/*
 * R551-F08
 *    is object-name [ ( deferred-shape-spec-list ) ]
 *    or proc-entity-name    
 */
pointer_decl
    :    T_IDENT ( T_LPAREN deferred_shape_spec_list T_RPAREN )?
    ;

cray_pointer_assoc_list
   :   cray_pointer_assoc ( T_COMMA cray_pointer_assoc )*
   ;

cray_pointer_assoc
   :   T_LPAREN T_IDENT T_COMMA T_IDENT T_RPAREN
   ;

protected_stmt
    :   (label)? T_PROTECTED ( T_COLON_COLON )? 
            generic_name_list end_of_stmt
    ;

save_stmt
    : (label)? T_SAVE ( ( T_COLON_COLON )? 
            saved_entity_list )? end_of_stmt
    ;

saved_entity
    :   T_IDENT
    |   T_SLASH T_IDENT T_SLASH
    ;

saved_entity_list
    :   saved_entity ( T_COMMA saved_entity )*
    ;

target_stmt
   :   (label)?
       T_TARGET ( T_COLON_COLON )? target_decl_list end_of_stmt
   ;

/*
 * R557-F08 target-decl
 *    is   object-name [ ( array-spec ) ]
 *                     [ lbracket coarray-spec rbracket ]
 */
target_decl
   :   T_IDENT (T_LPAREN array_spec T_RPAREN )?
               (T_LBRACKET coarray_spec T_RBRACKET )?
   ;

target_decl_list
   :   target_decl ( T_COMMA target_decl )*
   ;

value_stmt
    :   (label)? T_VALUE ( T_COLON_COLON )? 
            generic_name_list end_of_stmt
    ;

volatile_stmt
    :   (label)? T_VOLATILE ( T_COLON_COLON )? 
            generic_name_list end_of_stmt
    ;

implicit_stmt
    :   (label)? T_IMPLICIT implicit_spec_list end_of_stmt
    |   (label)? T_IMPLICIT T_NONE end_of_stmt
    ;

implicit_spec
    :   declaration_type_spec T_LPAREN letter_spec_list T_RPAREN
    ;

implicit_spec_list
    :   implicit_spec ( T_COMMA implicit_spec )*
    ;

letter_spec 
    : T_IDENT ( T_MINUS T_IDENT )? 
    ;

letter_spec_list
    :   letter_spec ( T_COMMA letter_spec )*
    ;

namelist_stmt
    :   (label)? T_NAMELIST T_SLASH T_IDENT T_SLASH
        namelist_group_object_list
        ( ( T_COMMA )?  T_SLASH T_IDENT T_SLASH
        namelist_group_object_list )* end_of_stmt
    ;

namelist_group_object_list
    :   T_IDENT
            ( T_COMMA T_IDENT )*
    ;

equivalence_stmt
    :   (label)? T_EQUIVALENCE equivalence_set_list end_of_stmt
    ;

equivalence_set
    :   T_LPAREN equivalence_object T_COMMA equivalence_object_list T_RPAREN
    ;

equivalence_set_list
    :   equivalence_set ( T_COMMA equivalence_set )*
    ;

equivalence_object
    :   substring
    ;

equivalence_object_list
    :   equivalence_object ( T_COMMA equivalence_object )*
    ;

common_stmt
    : (label)? 
        T_COMMON ( common_block_name )?
        common_block_object_list
        ( ( T_COMMA )? common_block_name
        common_block_object_list )* end_of_stmt
    ;

common_block_name
    : T_SLASH_SLASH
    | T_SLASH (T_IDENT)? T_SLASH
    ;

common_block_object
    : T_IDENT ( T_LPAREN explicit_shape_spec_list T_RPAREN )?
    ;

common_block_object_list
    :   common_block_object ( T_COMMA common_block_object )*
    ;


/*********************************************************************************************************************/
/* Part IIe: FORTRAN 2008 Grammar / R6xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

variable
   :   designator
   ;

designator
    :   data_ref (T_LPAREN substring_range T_RPAREN)?
    |   char_literal_constant T_LPAREN substring_range T_RPAREN
    ;

designator_or_func_ref
    :   data_ref (T_LPAREN substring_range_or_arg_list T_RPAREN)?
    |   char_literal_constant T_LPAREN substring_range T_RPAREN
    ;

substring_range_or_arg_list
    :   T_COLON (expr )? // substring_range
    |   expr substr_range_or_arg_list_suffix
    |   T_IDENT T_EQUALS expr
        ( T_COMMA actual_arg_spec )*
    |   ( T_IDENT T_EQUALS  )? T_ASTERISK label
        ( T_COMMA actual_arg_spec )*
    ;

substr_range_or_arg_list_suffix
    :  T_COLON (expr)? // substring_range
    |   ( T_COMMA actual_arg_spec )*
    ;

substring
    :   data_ref (T_LPAREN substring_range T_RPAREN)?
    |   char_literal_constant T_LPAREN substring_range T_RPAREN
    ;

substring_range
    :   (expr)? T_COLON (expr)?
    ;

data_ref
    :   part_ref ( T_PERCENT part_ref )*
    ;
    
data_ref_list
	:	data_ref (T_COMMA data_ref)*
	;

/**
 * R612-F08 part-ref
 *    is part-name [ ( section-subscript-list ) ] [ image-selector]
 */
part_ref
   :   T_IDENT
   ;

allocate_stmt
    :   (label)? T_ALLOCATE T_LPAREN
        type_spec T_COLON_COLON
        allocation_list 
        ( T_COMMA alloc_opt_list)? T_RPAREN 
            end_of_stmt
    |   (label)? T_ALLOCATE T_LPAREN
        allocation_list
        ( T_COMMA alloc_opt_list )? T_RPAREN 
            end_of_stmt
    ;

alloc_opt
    :   T_IDENT T_EQUALS expr
            /* {'STAT','ERRMSG'} are variables {SOURCE'} is expr */
    ;

alloc_opt_list
    :   alloc_opt ( T_COMMA alloc_opt )*
    ;

allocation
   :   data_ref (T_LPAREN data_ref_list T_RPAREN)?
   ;

allocation_list
   :   allocation ( T_COMMA allocation )*
   ;

/**
 * R632-F08 allocate-object
 *    is variable-name
 *    structure-component
 */
allocate_object
   :   data_ref
   ;

allocate_object_list
    :   allocate_object ( T_COMMA allocate_object )*
    ;

nullify_stmt
    :   (label)? T_NULLIFY T_LPAREN pointer_object_list T_RPAREN end_of_stmt
    ;

pointer_object
    :   data_ref
    ;

pointer_object_list
    :   pointer_object ( T_COMMA pointer_object )*
    ;

deallocate_stmt
    :    (label)? T_DEALLOCATE T_LPAREN allocate_object_list 
            ( T_COMMA dealloc_opt_list )? 
            T_RPAREN end_of_stmt
    ;

dealloc_opt
    :   T_IDENT /* {'STAT','ERRMSG'} */ T_EQUALS designator
    ;

dealloc_opt_list
    :   dealloc_opt ( T_COMMA dealloc_opt )*
    ;


/*********************************************************************************************************************/
/* Part IIf: FORTRAN 2008 Grammar / R7xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

primary
    :   designator_or_func_ref
    |   literal_constant
    |   array_constructor
    |   structure_constructor
    |   T_LPAREN expr T_RPAREN
    ;

level_1_expr
    : (defined_unary_op)? primary
    ;

defined_unary_op
    :   T_DEFINED_OP
    ;

power_operand
    : level_1_expr (power_op power_operand)?
    ;   

mult_operand
    : power_operand (mult_op power_operand)*
    ;

signed_operand
   :   (add_op)? mult_operand 
   ;

add_operand
   :   signed_operand ( add_op mult_operand )*
   ;

level_2_expr
    : add_operand ( concat_op add_operand )*
    ;

power_op
    :   T_POWER
    ;

mult_op
    :   T_ASTERISK
    |   T_SLASH
    ;

add_op
    :   T_PLUS
    |   T_MINUS
    ;

level_3_expr
    : level_2_expr (rel_op level_2_expr)?
    ;

concat_op
    :   T_SLASH_SLASH
    ;

rel_op
    :   T_EQ
    |   T_NE
    |   T_LT
    |   T_LE
    |   T_GT
    |   T_GE
    |   T_EQ_EQ
    |   T_SLASH_EQ
    |   T_LESSTHAN
    |   T_LESSTHAN_EQ
    |   T_GREATERTHAN
    |   T_GREATERTHAN_EQ
    ;

and_operand
    :   (not_op)?
        level_3_expr
        (and_op (not_op)? level_3_expr)*
    ;

or_operand
    : and_operand (or_op and_operand)*
    ;

equiv_operand
    : or_operand (equiv_op or_operand)*
    ;

level_5_expr
    : equiv_operand (defined_binary_op equiv_operand)*
    ;

not_op
    :   T_NOT 
    ;

and_op
    :   T_AND
    ;

or_op
    :   T_OR
    ;

equiv_op
    :   T_EQV
    |   T_NEQV
    ;

expr
    : level_5_expr
    ;

defined_binary_op
    :   T_DEFINED_OP
    ;

assignment_stmt
    :   (label)? variable T_EQUALS expr end_of_stmt
    ;

pointer_assignment_stmt
    : (label)? data_ref T_EQ_GT 
            expr end_of_stmt
    | (label)? data_ref T_LPAREN 
            bounds_spec_list T_RPAREN T_EQ_GT expr end_of_stmt
    | (label)? data_ref T_LPAREN 
            bounds_remapping_list T_RPAREN T_EQ_GT expr end_of_stmt
    ;

bounds_spec
    :   expr T_COLON
    ;

bounds_spec_list
    :   bounds_spec ( T_COMMA bounds_spec )*
    ;

bounds_remapping
    :   expr T_COLON expr
    ;

bounds_remapping_list
    :   bounds_remapping ( T_COMMA bounds_remapping )*
    ;

where_stmt
    :
        (label)? T_WHERE_STMT T_WHERE
        T_LPAREN expr T_RPAREN assignment_stmt
    ;

where_construct
    :    where_construct_stmt ( where_body_construct )*
          ( masked_elsewhere_stmt ( where_body_construct )* )*
          ( elsewhere_stmt ( where_body_construct )* )?
         end_where_stmt
    ;

where_construct_stmt
    :   ( T_IDENT T_COLON )? T_WHERE_CONSTRUCT_STMT T_WHERE 
            T_LPAREN expr T_RPAREN end_of_stmt
    ;

where_body_construct
    :   assignment_stmt
    |   where_stmt
    |   where_construct
    ;

masked_elsewhere_stmt
    :   (label)? T_ELSE T_WHERE T_LPAREN expr T_RPAREN 
            ( T_IDENT )? end_of_stmt 
    |   (label)? T_ELSEWHERE T_LPAREN expr T_RPAREN 
            ( T_IDENT )? end_of_stmt 
    ;

elsewhere_stmt
    :   (label)? T_ELSE T_WHERE 
            (T_IDENT)? end_of_stmt
    |   (label)? T_ELSEWHERE (T_IDENT)? 
            end_of_stmt 
    ;

end_where_stmt
    : (label)? T_ENDWHERE ( T_IDENT )? end_of_stmt
    ;

forall_construct
    :   forall_construct_stmt
        ( forall_body_construct )*
        end_forall_stmt
    ;

forall_construct_stmt
    :    (label)? ( T_IDENT T_COLON )? 
            T_FORALL 
            forall_header end_of_stmt
    ;

forall_header
    : T_LPAREN forall_triplet_spec ( T_COMMA expr )? T_RPAREN
    ;

forall_triplet_spec
    : T_IDENT T_EQUALS expr T_COLON expr ( T_COLON expr )?
    ;

forall_body_construct
    :   forall_assignment_stmt
    |   where_stmt
    |   where_construct
    |   forall_construct
    |   forall_stmt
    ;

forall_assignment_stmt
    :   assignment_stmt
    |   pointer_assignment_stmt
    ;

end_forall_stmt
    : (label)? T_ENDFORALL ( T_IDENT )? end_of_stmt
    ;

forall_stmt
    :   (label)? T_FORALL
        forall_header
        forall_assignment_stmt
    ;

    
/*********************************************************************************************************************/
/* Part IIg: FORTRAN 2008 Grammar / R8xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

block
    :   ( execution_part_construct )*
    ;

if_construct
    :   if_then_stmt block ( else_if_stmt block )* ( else_stmt block )? end_if_stmt
    ;

if_then_stmt
    : (label)? ( T_IDENT T_COLON )? T_IF 
            T_LPAREN expr T_RPAREN T_THEN end_of_stmt
    ;

else_if_stmt
    : (label)? T_ELSE T_IF
        T_LPAREN expr T_RPAREN T_THEN ( T_IDENT )? end_of_stmt
    | (label)? T_ELSEIF
        T_LPAREN expr T_RPAREN T_THEN ( T_IDENT )? end_of_stmt
    ;

else_stmt
    :   (label)? T_ELSE ( T_IDENT )? end_of_stmt
    ;

end_if_stmt
    : (label)? T_ENDIF    ( T_IDENT )? end_of_stmt
    ;

if_stmt
    :   (label)? T_IF T_LPAREN expr T_RPAREN action_stmt
    ;

case_construct
    :    select_case_stmt ( case_stmt block )* end_select_stmt
    ;

select_case_stmt
    :   (label)? ( T_IDENT T_COLON )?
        (T_SELECT T_CASE | T_SELECTCASE)
            T_LPAREN expr T_RPAREN end_of_stmt
    ;

case_stmt
    :   (label)? T_CASE case_selector ( T_IDENT )? end_of_stmt
    ;

end_select_stmt
    : (label)? T_ENDSELECT    (T_IDENT)? end_of_stmt
    ;

case_selector
    :   T_LPAREN
        case_value_range_list
        T_RPAREN
    |   T_DEFAULT
    ;

case_value_range
    :   T_COLON case_value
    |   case_value case_value_range_suffix
    ;

case_value_range_suffix
    :   T_COLON ( case_value )?
    |
    ;

case_value_range_list
    :   case_value_range ( T_COMMA case_value_range )*
    ;

case_value
    :   expr
    ;

associate_construct
    :   associate_stmt
        block
        end_associate_stmt
    ;

associate_stmt
    :   (label)? ( T_IDENT T_COLON )? 
            T_ASSOCIATE T_LPAREN association_list T_RPAREN end_of_stmt
    ;

association_list
    :   association ( T_COMMA association )*
    ;

/*
 * R818-08 loop-control
 *    is   [ , ] do-variable = scalar-int-expr , scalar-int-expr [ , scalar-int-expr ]
 *    or   [ , ] WHILE ( scalar-logical-expr )
 *    or   [ , ] CONCURRENT forall-header
 */
loop_control
   :   ( T_COMMA )? do_variable T_EQUALS expr T_COMMA expr ( T_COMMA expr )?
   |   ( T_COMMA )? T_WHILE T_LPAREN expr T_RPAREN 
   |   ( T_COMMA )? T_CONCURRENT forall_header
   ;

association
    :   T_IDENT T_EQ_GT selector
    ;

selector
    :   expr
    ;

end_associate_stmt
    :   (label)? T_ENDASSOCIATE (T_IDENT)? end_of_stmt
    ;

select_type_construct
    :   select_type_stmt ( type_guard_stmt block )* end_select_type_stmt
    ;

select_type_stmt
    : (label)?
        ( T_IDENT T_COLON )? select_type
        T_LPAREN ( T_IDENT T_EQ_GT )?
        selector T_RPAREN end_of_stmt
    ;

select_type
    : T_SELECT T_TYPE
    | T_SELECTTYPE
    ;

type_guard_stmt
    :   (label)? T_TYPE T_IDENT T_LPAREN type_spec T_RPAREN ( T_IDENT )? end_of_stmt
    |   (label)? T_CLASS T_IDENT T_LPAREN type_spec T_RPAREN ( T_IDENT )? end_of_stmt
    |   (label)? T_CLASS T_DEFAULT ( T_IDENT )? end_of_stmt
    ;

end_select_type_stmt
    :   (label)? T_ENDSELECT ( T_IDENT )? end_of_stmt
    ;

do_construct
    :   block_do_construct
    ;

block_do_construct
    :   do_stmt
        block
        end_do
    ;

do_stmt
    :   (label)? ( T_IDENT T_COLON )? T_DO 
            ( T_DIGIT_STRING )? 
            ( loop_control )? end_of_stmt
    ;

do_variable
    :   T_IDENT
    ;

end_do
    :   end_do_stmt
    |   do_term_action_stmt
    ;

end_do_stmt
    : (label)? T_ENDDO    ( T_IDENT )? end_of_stmt
    ;

do_term_action_stmt
    :   label T_LABEL_DO_TERMINAL 
        (action_stmt
         | ( T_ENDDO 
                (T_IDENT )?)
                end_of_stmt
        )
    | T_LABEL_DO_TERMINAL_INSERTED
    ;

cycle_stmt
    :   (label)? T_CYCLE (T_IDENT)? end_of_stmt
    ;

exit_stmt
    :   (label)? T_EXIT (T_IDENT)? end_of_stmt
    ;

goto_stmt
   :   (label)?
       (   T_GO T_TO 
        |  T_GOTO
       )
       T_DIGIT_STRING end_of_stmt
   ;

computed_goto_stmt
    :   (label)?
        (T_GO T_TO 
         | T_GOTO ) 
            T_LPAREN label_list T_RPAREN ( T_COMMA )? expr end_of_stmt
    ;

assign_stmt 
    :   (label)? T_ASSIGN label T_TO name end_of_stmt 
    ;

assigned_goto_stmt
    :   (label)? ( T_GOTO 
                   | T_GO T_TO ) 
            name (T_COMMA stmt_label_list)? end_of_stmt
    ;

stmt_label_list
    :   T_LPAREN label ( T_COMMA label )* T_RPAREN 
    ;

pause_stmt
   :   (label)? T_PAUSE (label | char_literal_constant)? end_of_stmt 
   ;

arithmetic_if_stmt
    :   (label)? T_ARITHMETIC_IF_STMT T_IF
        T_LPAREN expr T_RPAREN label
        T_COMMA label
        T_COMMA label end_of_stmt
    ;

continue_stmt
    :   (label)? T_CONTINUE end_of_stmt
    ;

stop_stmt
    :   (label)? T_STOP (stop_code)? 
            end_of_stmt
    ;

stop_code
    : scalar_char_constant
    | T_DIGIT_STRING
    ;

scalar_char_constant
    :    char_constant
    ;

    
/*********************************************************************************************************************/
/* Part IIh: FORTRAN 2008 Grammar / R9xx - Extracted from FortranParser08.g                                          */
/*********************************************************************************************************************/

file_unit_number
    :   expr
    ;

open_stmt
    :   (label)? T_OPEN T_LPAREN connect_spec_list T_RPAREN end_of_stmt
    ;

connect_spec
    : expr
    | (T_IDENT | T_FILE)
        /* {'UNIT','ACCESS','ACTION','ASYNCHRONOUS','BLANK','DECIMAL', */
        /* 'DELIM','ENCODING'} are expr */
        /* {'ERR'} is T_DIGIT_STRING */
        /* {'FILE','FORM'} are expr */
        /* {'IOMSG','IOSTAT'} are variables */
        /* {'PAD','POSITION','RECL','ROUND','SIGN','STATUS'} are expr */
      T_EQUALS expr
    ;

connect_spec_list
    :   connect_spec ( T_COMMA connect_spec )*
    ;

close_stmt
    :   (label)? T_CLOSE T_LPAREN close_spec_list T_RPAREN end_of_stmt
    ;

close_spec
    :   expr
    |   T_IDENT /* {'UNIT','IOSTAT','IOMSG','ERR','STATUS'} */ T_EQUALS expr
    ;

close_spec_list
    :   close_spec ( T_COMMA close_spec )*
    ;

read_stmt
    :       (label)? T_READ T_LPAREN io_control_spec_list T_RPAREN ( input_item_list )? end_of_stmt
    |       (label)? T_READ format ( T_COMMA input_item_list )? end_of_stmt
    ;

write_stmt
    :   (label)? T_WRITE T_LPAREN io_control_spec_list 
            T_RPAREN ( output_item_list )? end_of_stmt
    ;

print_stmt
    :    (label)? T_PRINT format 
            ( T_COMMA output_item_list )? end_of_stmt
    ;

io_control_spec
        :   expr
        |   T_ASTERISK
        |   T_IDENT /* {'UNIT','FMT'} */ T_EQUALS T_ASTERISK
        |   T_IDENT
            /* {'UNIT','FMT'} are expr 'NML' is T_IDENT} */
            /* {'ADVANCE','ASYNCHRONOUS','BLANK','DECIMAL','DELIM'} are expr */
            /* {'END','EOR','ERR'} are labels */
            /* {'ID','IOMSG',IOSTAT','SIZE'} are variables */
            /* {'PAD','POS','REC','ROUND','SIGN'} are expr */
        T_EQUALS expr
    ;

io_control_spec_list
    :   io_control_spec ( T_COMMA io_control_spec )*
    ;

format
    :   expr
    |   T_ASTERISK
    ;

input_item
    :   variable
    |   io_implied_do
    ;

input_item_list
    :   input_item ( T_COMMA input_item )*
    ;

output_item
    :   expr
    |   io_implied_do
    ;

output_item_list
    :   output_item ( T_COMMA output_item )*
    ;

io_implied_do
    :   T_LPAREN io_implied_do_object io_implied_do_suffix T_RPAREN
    ;

io_implied_do_object
    :   output_item
    ;

io_implied_do_suffix
    :   T_COMMA io_implied_do_object io_implied_do_suffix
    |   T_COMMA io_implied_do_control
    ;

io_implied_do_control
    : do_variable T_EQUALS expr T_COMMA expr ( T_COMMA expr )?
    ;

wait_stmt
    :   (label)? T_WAIT T_LPAREN wait_spec_list T_RPAREN end_of_stmt
    ;

wait_spec
    :   expr
    |   T_IDENT /* {'UNIT','END','EOR','ERR','ID','IOMSG','IOSTAT'} */ 
            T_EQUALS expr
    ;

wait_spec_list
    :   wait_spec ( T_COMMA wait_spec )*
    ;

backspace_stmt
    :       (label)? T_BACKSPACE T_LPAREN position_spec_list T_RPAREN end_of_stmt
    |       (label)? T_BACKSPACE file_unit_number end_of_stmt
    ;

endfile_stmt
    :       (label)? T_ENDFILE T_LPAREN position_spec_list T_RPAREN end_of_stmt
    |       (label)? T_ENDFILE file_unit_number end_of_stmt
    ;

rewind_stmt
    :       (label)? T_REWIND T_LPAREN position_spec_list T_RPAREN end_of_stmt
    |       (label)? T_REWIND file_unit_number end_of_stmt
    ;

position_spec
    :   expr
    |   T_IDENT /* {'UNIT','IOSTAT','IOMSG','ERR'} */ T_EQUALS expr
    ;

position_spec_list
    :   position_spec ( T_COMMA position_spec )*
    ;

flush_stmt
    :       (label)? T_FLUSH T_LPAREN flush_spec_list T_RPAREN end_of_stmt
    |       (label)? T_FLUSH file_unit_number end_of_stmt
    ;

flush_spec
    :   expr
    |   T_IDENT /* {'UNIT','IOSTAT','IOMSG','ERR'} */ T_EQUALS expr
    ;

flush_spec_list
    :   flush_spec ( T_COMMA flush_spec )*
    ;

inquire_stmt
    :   (label)? T_INQUIRE T_LPAREN inquire_spec_list T_RPAREN end_of_stmt
    |   (label)? 
            T_INQUIRE T_LPAREN T_IDENT /* 'IOLENGTH' */ T_EQUALS 
            scalar_int_variable T_RPAREN output_item_list end_of_stmt
    ;

inquire_spec
    :   expr
    |   (T_IDENT  | T_FILE) 
        /* {'UNIT','FILE'} '=' expr portion, '=' designator portion below 
           {'ACCESS','ACTION','ASYNCHRONOUS','BLANK','DECIMAL',DELIM','DIRECT'}
           {'ENCODING','ERR','EXIST','FORM','FORMATTED','ID','IOMSG','IOSTAT'}
           {'NAME','NAMED','NEXTREC','NUMBER',OPENED','PAD','PENDING','POS'} 
           {'POSITION','READ','READWRITE','RECL','ROUND','SEQUENTIAL','SIGN'} 
           {'SIZE','STREAM','UNFORMATTED','WRITE'}  */
        T_EQUALS expr
    ;

inquire_spec_list
    :   inquire_spec ( T_COMMA inquire_spec )*
    ;


/*********************************************************************************************************************/
/* Part IIi: FORTRAN 2008 Grammar / R10xx - Extracted from FortranParser08.g                                         */
/*********************************************************************************************************************/

format_stmt
    :   (label)? T_FORMAT format_specification end_of_stmt
    ;

format_specification
    :   T_LPAREN ( format_item_list )? T_RPAREN
    ;

format_item
    :   T_DATA_EDIT_DESC 
    |   T_CONTROL_EDIT_DESC
    |   T_CHAR_STRING_EDIT_DESC
    |   (T_DIGIT_STRING)? T_LPAREN format_item_list T_RPAREN
    ;

format_item_list
    :   format_item ( (T_COMMA)? format_item )*
    ;


/*********************************************************************************************************************/
/* Part IIj: FORTRAN 2008 Grammar / R11xx - Extracted from FortranParser08.g                                         */
/*********************************************************************************************************************/

program_stmt
    :   (label)? T_PROGRAM T_IDENT end_of_stmt
    ;

end_program_stmt
    :   (label)? T_ENDPROGRAM (T_IDENT)? end_of_stmt
    |   (label)? T_END end_of_stmt
    ;
    
module
    :   module_stmt
        specification_part
        ( module_subprogram_part )?
        end_module_stmt
    ;

module_stmt
    :    (label)? T_MODULE name ( T_IDENT T_IDENT )? end_of_stmt
    ;

end_module_stmt
    :  (label)? T_ENDMODULE (T_IDENT)? end_of_stmt
    |  (label)? T_END end_of_stmt
    ;


/*
 * R1107-F08 module-subprogram-part
 *     is   contains-stmt
 *          [ module-subprogram ] ...
 */
module_subprogram_part
   :   contains_stmt
       ( module_subprogram )*
   ;

/*
 * R1108-F08 module-subprogram
 *     is   function-subprogram
 *     or   subroutine-subprogram
 *     or   separate-module-subprogram   // NEW_TO_F2008
 */
module_subprogram
   :   (prefix)? function_subprogram
   |   subroutine_subprogram
   |   separate_module_subprogram
   ;

use_stmt
    :    (label)? T_USE 
            ( (T_COMMA module_nature )? 
            T_COLON_COLON )? name ( T_COMMA 
            rename_list )? end_of_stmt
    |    (label)? T_USE 
            ( ( T_COMMA module_nature )? 
            T_COLON_COLON )? name T_COMMA T_ONLY T_COLON ( only_list )? 
            end_of_stmt
    ;

module_nature
    :   T_INTRINSIC
    |   T_NON_INTRINSIC
    ;

rename
    :   T_IDENT T_EQ_GT T_IDENT
    |   T_OPERATOR T_LPAREN T_DEFINED_OP T_RPAREN T_EQ_GT
        T_OPERATOR T_LPAREN T_DEFINED_OP T_RPAREN
    ;

rename_list
    :   rename ( T_COMMA rename )*
    ;

only
    :   generic_spec
    |   rename
    ;

only_list
    :   only ( T_COMMA only )*
    ;

/*
 * R1116-F08 submodule
 *     is submodule-stmt
 *           [ specification-part ]
 *           [ module-subprogram-part ]
 *     end-submodule-stmt
 */
submodule
   :   submodule_stmt
       specification_part  // non-optional as can be empty
       ( module_subprogram_part )?
       end_submodule_stmt
   ;

/*
 * R1117-F08 submodule-stmt
 *     is SUBMODULE ( parent-identifier ) submodule-name
 */
submodule_stmt
   :   (label)? T_SUBMODULE T_LPAREN parent_identifier T_RPAREN name end_of_stmt
   ;

/*
 * R1118-F08 parent-identifier
 *     is ancestor-module-name [ : parent-submodule-name ]
 */
parent_identifier
   :   name ( T_COLON T_IDENT )?
   ;

/*
 * R1119-F08 end-submodule-stmt
 *     is END [ SUBMODULE [ submodule-name ] ]
 */
end_submodule_stmt
   :   (label)? T_END end_of_stmt
   |   (label)? T_ENDSUBMODULE (name)? end_of_stmt
   ;

block_data
    :   block_data_stmt
        specification_part
        end_block_data_stmt
    ;

block_data_stmt
   :   (label)? T_BLOCK T_DATA (T_IDENT)? end_of_stmt
   |   (label)? T_BLOCKDATA  (T_IDENT)? end_of_stmt
   ;

end_block_data_stmt
    :   (label)? T_ENDBLOCKDATA ( T_IDENT )? end_of_stmt
    |   (label)? T_END end_of_stmt
    ;


/*********************************************************************************************************************/
/* Part IIk: FORTRAN 2008 Grammar / R12xx - Extracted from FortranParser08.g                                         */
/*********************************************************************************************************************/

interface_block
    :   interface_stmt
        ( interface_specification )*
        end_interface_stmt
    ;

interface_specification
    :   interface_body
    |   procedure_stmt
    ;

interface_stmt
    :   (label)? T_INTERFACE ( generic_spec )? end_of_stmt
    |   (label)? T_ABSTRACT T_INTERFACE end_of_stmt
    ;

end_interface_stmt
    : (label)? T_ENDINTERFACE    ( generic_spec )? end_of_stmt
    ;

interface_body
    :   (prefix)? function_stmt specification_part end_function_stmt
    |   subroutine_stmt specification_part end_subroutine_stmt
    ;

procedure_stmt
    :   (label)? ( T_MODULE )? T_PROCEDURE generic_name_list end_of_stmt
    ;

generic_spec
    :   T_IDENT
    |   T_OPERATOR T_LPAREN defined_operator T_RPAREN
    |   T_ASSIGNMENT T_LPAREN T_EQUALS T_RPAREN
    |   defined_io_generic_spec
    ;

defined_io_generic_spec
    :   T_READ T_LPAREN T_FORMATTED T_RPAREN
    |   T_READ T_LPAREN T_UNFORMATTED T_RPAREN
    |   T_WRITE T_LPAREN T_FORMATTED T_RPAREN
    |   T_WRITE T_LPAREN T_UNFORMATTED T_RPAREN
    ;

import_stmt
    :    (label)? T_IMPORT ( ( T_COLON_COLON )? generic_name_list )? end_of_stmt
    ;

external_stmt
    :   (label)? T_EXTERNAL ( T_COLON_COLON )? generic_name_list end_of_stmt
    ;

procedure_declaration_stmt
    : (label)? T_PROCEDURE T_LPAREN
        ( proc_interface )? T_RPAREN
        ( ( T_COMMA proc_attr_spec )* T_COLON_COLON )?
        proc_decl_list end_of_stmt
    ;

proc_interface
    :   T_IDENT
    |   declaration_type_spec
    ;

proc_attr_spec
    :   access_spec
    |   proc_language_binding_spec
    |   T_INTENT T_LPAREN intent_spec T_RPAREN
    |   T_OPTIONAL  
    |   T_POINTER   
    |   T_SAVE      
    |   T_PASS ( T_LPAREN T_IDENT T_RPAREN)?
    |   T_NOPASS
    |   T_DEFERRED
    |   proc_attr_spec_extension
    ;
  
proc_attr_spec_extension : T_NO_LANGUAGE_EXTENSION ;

proc_decl
    :   T_IDENT ( T_EQ_GT null_init )?
    ;

proc_decl_list
    :   proc_decl ( T_COMMA proc_decl )*
    ;

intrinsic_stmt
    :   (label)? T_INTRINSIC ( T_COLON_COLON )? generic_name_list end_of_stmt
    ;

call_stmt
    :    (label)? T_CALL procedure_designator
            ( T_LPAREN (actual_arg_spec_list)? 
            T_RPAREN )? end_of_stmt
    ;

procedure_designator
    :   data_ref
    ;

actual_arg_spec
    :   (T_IDENT T_EQUALS)? actual_arg
    ;

actual_arg_spec_list
    :   actual_arg_spec ( T_COMMA actual_arg_spec )*
    ;

actual_arg
    :   expr                
    |   T_ASTERISK label    
    ;

function_subprogram
    :   function_stmt
        specification_part
        ( execution_part )?
        ( internal_subprogram_part )?
        end_function_stmt
    ;

function_stmt
    :   (label)? T_FUNCTION name
            T_LPAREN ( generic_name_list)? T_RPAREN 
            ( suffix )? end_of_stmt
    ;

proc_language_binding_spec
    :   language_binding_spec
    ;

prefix
   :  prefix_spec ( prefix_spec )*
   ;

t_prefix
   :  t_prefix_spec ( t_prefix_spec )*
   ;

prefix_spec
   :  declaration_type_spec
   |  t_prefix_spec
   ;

t_prefix_spec
   :  T_ELEMENTAL
   |  T_IMPURE
   |  T_MODULE
   |  T_PURE
   |  T_RECURSIVE
   |  prefix_spec_extension
   ;

prefix_spec_extension : T_NO_LANGUAGE_EXTENSION ;

suffix
    :   proc_language_binding_spec ( T_RESULT T_LPAREN result_name T_RPAREN )?
    |   T_RESULT T_LPAREN result_name T_RPAREN ( proc_language_binding_spec )?
    ;

result_name
    :    name
    ;

end_function_stmt
    : (label)? T_ENDFUNCTION    ( T_IDENT )? end_of_stmt
    | (label)? T_END end_of_stmt
    ;

subroutine_subprogram
   :   subroutine_stmt
       specification_part
       ( execution_part )?
       ( internal_subprogram_part )?
       end_subroutine_stmt
   ;

subroutine_stmt
    :   (label)? (t_prefix)? T_SUBROUTINE 
            name ( T_LPAREN ( dummy_arg_list )? 
            T_RPAREN ( proc_language_binding_spec )? )? end_of_stmt
    ;

dummy_arg
    :   name
    |   T_ASTERISK
    ;

dummy_arg_list
    :   dummy_arg ( T_COMMA dummy_arg )*
    ;

end_subroutine_stmt
    : (label)? T_ENDSUBROUTINE    ( T_IDENT )? end_of_stmt
    | (label)? T_END end_of_stmt
    ;

entry_stmt
    :   (label)? T_ENTRY T_IDENT
            ( T_LPAREN ( dummy_arg_list )? T_RPAREN 
            ( suffix )? )? end_of_stmt
    ;

return_stmt
    :   (label)? T_RETURN ( expr )? end_of_stmt
    ;

/*
 * R1237-F08 separate-module-subprogram
 *     is   mp-subprogram-stmt          // NEW_TO_F2008
 *             [ specification-part ]
 *             [ execution-part ]
 *             [ internal-subprogram-part ]
 *          end-mp-subprogram
 */
separate_module_subprogram
   :   mp_subprogram_stmt
          specification_part  // non-optional as can be empty
          ( execution_part )?
          ( internal_subprogram_part )?
       end_mp_subprogram_stmt
   ;

/*
 * R1238-F08 mp-subprogram-stmt
 *     is   MODULE PROCEDURE procedure-name
 */
mp_subprogram_stmt
   :   (label)? T_MODULE T_PROCEDURE name end_of_stmt
   ;

/*
 * R1239-F08 end-mp-subprogram-stmt
 *     is END [ PROCEDURE [ procedure-name ] ]
 */
end_mp_subprogram_stmt
   :   (label)? T_END (T_PROCEDURE (name)?)? end_of_stmt
   |   (label)? T_ENDPROCEDURE (name)? end_of_stmt
   ;

contains_stmt
    :   (label)? T_CONTAINS end_of_stmt
    ;

stmt_function_stmt
    :   (label)? T_STMT_FUNCTION T_IDENT T_LPAREN 
            ( generic_name_list )? T_RPAREN 
            T_EQUALS expr end_of_stmt
    ;

end_of_stmt
    : (T_SEMICOLON (T_EOL)?)+
    | (T_EOL)+         
    | T_EOF  
    ;
