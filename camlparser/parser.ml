(*
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT

*)

open Ostap
open Combinators

ostap (
   implementation: -structure -EOF;
   interface: -signature -EOF;
   toplevel_phrase:
       -top_structure -SEMISEMI
     | -seq_expr -SEMISEMI
     | -toplevel_directive -SEMISEMI
     | -EOF;
   top_structure:
       -structure_item
     | -structure_item -top_structure;
   use_file:
       -use_file_tail
     | -seq_expr -use_file_tail;
   use_file_tail:
       -EOF
     | -SEMISEMI -EOF
     | -SEMISEMI -seq_expr -use_file_tail
     | -SEMISEMI -structure_item -use_file_tail
     | -SEMISEMI -toplevel_directive -use_file_tail
     | -structure_item -use_file_tail
     | -toplevel_directive -use_file_tail;

(* Module expressions *)

   module_expr:
       (
       -mod_longident
     | -STRUCT -structure -END
     | -FUNCTOR -LPAREN -UIDENT -COLON -module_type -RPAREN -MINUSGREATER -module_expr
     | -LPAREN -module_expr -COLON -module_type -RPAREN
     | -LPAREN -module_expr -RPAREN
     | -LPAREN -VAL -expr -COLON -package_type -RPAREN
       ) -(-LPAREN -module_expr -RPAREN)*;
   structure:
       -structure_tail
     | -seq_expr -structure_tail;
   structure_tail:
       empty
     | -SEMISEMI
     | -SEMISEMI -seq_expr -structure_tail
     | -SEMISEMI -structure_item -structure_tail
     | -structure_item -structure_tail;
   structure_item:
       -LET -rec_flag -let_bindings
     | -EXTERNAL -val_ident -COLON -core_type -EQUAL -primitive_declaration
     | -TYPE -type_declarations
     | -EXCEPTION -UIDENT -constructor_arguments
     | -EXCEPTION -UIDENT -EQUAL -constr_longident
     | -MODULE -UIDENT -module_binding
     | -MODULE -REC -module_rec_bindings
     | -MODULE -TYPE -ident -EQUAL -module_type
     | -OPEN -mod_longident
     | -CLASS -class_declarations
     | -CLASS -TYPE -class_type_declarations
     | -INCLUDE -module_expr;
   module_binding:
       -EQUAL -module_expr
     | -COLON -module_type -EQUAL -module_expr
     | -LPAREN -UIDENT -COLON -module_type -RPAREN -module_binding;
   module_rec_bindings:
       -module_rec_binding -(-AND -module_rec_binding)*;
   module_rec_binding:
       -UIDENT -COLON -module_type -EQUAL -module_expr;

(* Module types *)

   module_type:
       (
       -mty_longident
     | -SIG -signature -END
     | -FUNCTOR -LPAREN -UIDENT -COLON -module_type -RPAREN -MINUSGREATER -module_type
         (*%prec below_WITH*)
     | -MODULE -TYPE -OF -module_expr
     | -LPAREN -module_type -RPAREN
       ) -(-WITH -with_constraints)*;
   signature:
       (
       -signature_item
     | -signature_item -SEMISEMI
       )*;
   signature_item:
       -VAL -val_ident -COLON -core_type
     | -EXTERNAL -val_ident -COLON -core_type -EQUAL -primitive_declaration
     | -TYPE -type_declarations
     | -EXCEPTION -UIDENT -constructor_arguments
     | -MODULE -UIDENT -module_declaration
     | -MODULE -REC -module_rec_declarations
     | -MODULE -TYPE -ident
     | -MODULE -TYPE -ident -EQUAL -module_type
     | -OPEN -mod_longident
     | -INCLUDE -module_type
     | -CLASS -class_descriptions
     | -CLASS -TYPE -class_type_declarations;

   module_declaration:
       -COLON -module_type
     | -LPAREN -UIDENT -COLON -module_type -RPAREN -module_declaration;
   module_rec_declarations:
       -module_rec_declaration -(-AND -module_rec_declaration)*;
   module_rec_declaration:
       -UIDENT -COLON -module_type;

(* Class expressions *)

   class_declarations:
       -class_declaration -(-AND -class_declaration)*;
   class_declaration:
       -virtual_flag -class_type_parameters -LIDENT -class_fun_binding;
   class_fun_binding:
       -EQUAL -class_expr
     | -COLON -class_type -EQUAL -class_expr
     | -labeled_simple_pattern -class_fun_binding;
   class_type_parameters:
       empty
     | -LBRACKET -type_parameter_list -RBRACKET;
   class_fun_def:
       -labeled_simple_pattern -MINUSGREATER -class_expr
     | -labeled_simple_pattern -class_fun_def;
   class_expr:
       -class_simple_expr
     | -FUN -class_fun_def
     | -class_simple_expr -simple_labeled_expr_list
     | -LET -rec_flag -let_bindings -IN -class_expr;
   class_simple_expr:
       -LBRACKET -core_type_comma_list -RBRACKET -class_longident
     | -class_longident
     | -OBJECT -class_structure -END
     | -LPAREN -class_expr -COLON -class_type -RPAREN
     | -LPAREN -class_expr -RPAREN;
   class_structure:
       -class_self_pattern -class_fields;
   class_self_pattern:
       -LPAREN -pattern -RPAREN
     | -LPAREN -pattern -COLON -core_type -RPAREN
     | empty;
   class_fields:
       (
       -INHERIT -override_flag -class_expr -parent_binder
     | -VAL -virtual_value
     | -VAL -value
     | -virtual_method
     | -concrete_method
     | -CONSTRAINT -constrain
     | -INITIALIZER -seq_expr
       )*;
   parent_binder:
       -AS -LIDENT
     | empty;
   virtual_value:
       -override_flag -MUTABLE -VIRTUAL -label -COLON -core_type
     | -VIRTUAL -mutable_flag -label -COLON -core_type;
   value:
       -override_flag -mutable_flag -label -EQUAL -seq_expr
     | -override_flag -mutable_flag -label -type_constraint -EQUAL -seq_expr;
   virtual_method:
       -METHOD -override_flag -PRIVATE -VIRTUAL -label -COLON -poly_type
     | -METHOD -override_flag -VIRTUAL -private_flag -label -COLON -poly_type;
   concrete_method :
       -METHOD -override_flag -private_flag -label -strict_binding
     | -METHOD -override_flag -private_flag -label -COLON -poly_type -EQUAL -seq_expr;

(* Class types *)

   class_type:
       -class_signature
     | -QUESTION -LIDENT -COLON -simple_core_type_or_tuple -MINUSGREATER -class_type
     | -OPTLABEL -simple_core_type_or_tuple -MINUSGREATER -class_type
     | -LIDENT -COLON -simple_core_type_or_tuple -MINUSGREATER -class_type
     | -simple_core_type_or_tuple -MINUSGREATER -class_type;
   class_signature:
       -LBRACKET -core_type_comma_list -RBRACKET -clty_longident
     | -clty_longident
     | -OBJECT -class_sig_body -END;
   class_sig_body:
       -class_self_type -class_sig_fields;
   class_self_type:
       -LPAREN -core_type -RPAREN
     | empty;
   class_sig_fields:
       (
       -INHERIT -class_signature
     | -VAL -value_type
     | -virtual_method_type
     | -method_type
     | -CONSTRAINT -constrain
       )*;
   value_type:
       -VIRTUAL -mutable_flag -label -COLON -core_type
     | -MUTABLE -virtual_flag -label -COLON -core_type
     | -label -COLON -core_type;
   method_type:
       -METHOD -private_flag -label -COLON -poly_type;
   virtual_method_type:
       -METHOD -PRIVATE -VIRTUAL -label -COLON -poly_type
     | -METHOD -VIRTUAL -private_flag -label -COLON -poly_type;
   constrain:
       -core_type -EQUAL -core_type;
   class_descriptions:
       -class_description -(-AND -class_description)*;
   class_description:
       -virtual_flag -class_type_parameters -LIDENT -COLON -class_type;
   class_type_declarations:
       -class_type_declaration -(-AND -class_type_declaration)*;
   class_type_declaration:
       -virtual_flag -class_type_parameters -LIDENT -EQUAL -class_signature;

(* Core expressions *)

   seq_expr:
       -expr        (*%prec below_SEMI*)
     | -expr -SEMI
     | -expr -SEMI -seq_expr;
   labeled_simple_pattern:
       -QUESTION -LPAREN -label_let_pattern -opt_default -RPAREN
     | -QUESTION -label_var
     | -OPTLABEL -LPAREN -let_pattern -opt_default -RPAREN
     | -OPTLABEL -pattern_var
     | -TILDE -LPAREN -label_let_pattern -RPAREN
     | -TILDE -label_var
     | -LABEL -simple_pattern
     | -simple_pattern;
   pattern_var:
       -LIDENT
     | -UNDERSCORE;
   opt_default:
       empty
     | -EQUAL -seq_expr;
   label_let_pattern:
       -label_var
     | -label_var -COLON -core_type;
   label_var:
       -LIDENT;
   let_pattern:
       -pattern
     | -pattern -COLON -core_type;
   expr:
       (
       -simple_expr (* %prec below_SHARP *)
     | -simple_expr -simple_labeled_expr_list
     | -LET -rec_flag -let_bindings -IN -seq_expr
     | -LET -MODULE -UIDENT -module_binding -IN -seq_expr
     | -LET -OPEN -mod_longident -IN -seq_expr
     | -FUNCTION -opt_bar -match_cases
     | -FUN -labeled_simple_pattern -fun_def
     | -FUN -LPAREN -TYPE -LIDENT -RPAREN -fun_def
     | -MATCH -seq_expr -WITH -opt_bar -match_cases
     | -TRY -seq_expr -WITH -opt_bar -match_cases
     | -constr_longident -simple_expr (* %prec below_SHARP *)
     | -name_tag -simple_expr (* %prec below_SHARP *)
     | -IF -seq_expr -THEN -expr -ELSE -expr
     | -IF -seq_expr -THEN -expr
     | -WHILE -seq_expr -DO -seq_expr -DONE
     | -FOR -val_ident -EQUAL -seq_expr -direction_flag -seq_expr -DO -seq_expr -DONE
     | -LPAREN -COLONCOLON -RPAREN -LPAREN -expr -COMMA -expr -RPAREN
     | -subtractive -expr (* %prec prec_unary_minus *)
     | -additive -expr (* %prec prec_unary_plus *)
     | -simple_expr -DOT -label_longident -LESSMINUS -expr
     | -simple_expr -DOT -LPAREN -seq_expr -RPAREN -LESSMINUS -expr
     | -simple_expr -DOT -LBRACKET -seq_expr -RBRACKET -LESSMINUS -expr
     | -simple_expr -DOT -LBRACE -expr -RBRACE -LESSMINUS -expr
     | -label -LESSMINUS -expr
     | -ASSERT -simple_expr (* %prec below_SHARP *)
     | -LAZY -simple_expr (* %prec below_SHARP *)
     | -OBJECT -class_structure -END
       )
       -(
       -COLONCOLON -expr
     | -INFIXOP0 -expr
     | -INFIXOP1 -expr
     | -INFIXOP2 -expr
     | -INFIXOP3 -expr
     | -INFIXOP4 -expr
     | -PLUS -expr
     | -PLUSDOT -expr
     | -MINUS -expr
     | -MINUSDOT -expr
     | -STAR -expr
     | -EQUAL -expr
     | -LESS -expr
     | -GREATER -expr
     | -OR -expr
     | -BARBAR -expr
     | -AMPERSAND -expr
     | -AMPERAMPER -expr
     | -COLONEQUAL -expr
       )*
       -(-COMMA -expr)*;
   simple_expr:
       (
       -val_longident
     | -constant
     | -constr_longident (* %prec prec_constant_constructor *)
     | -name_tag (* %prec prec_constant_constructor *)
     | -LPAREN -seq_expr -RPAREN
     | -BEGIN -seq_expr -END
     | -BEGIN -END
     | -LPAREN -seq_expr -type_constraint -RPAREN
     | -mod_longident -DOT -LPAREN -seq_expr -RPAREN
     | -LBRACE -record_expr -RBRACE
     | -LBRACKETBAR -expr_semi_list -opt_semi -BARRBRACKET
     | -LBRACKETBAR -BARRBRACKET
     | -LBRACKET -expr_semi_list -opt_semi -RBRACKET
     | -PREFIXOP -simple_expr
     | -BANG -simple_expr
     | -NEW -class_longident
     | -LBRACELESS -field_expr_list -opt_semi -GREATERRBRACE
     | -LBRACELESS -GREATERRBRACE
     | -LPAREN -MODULE -module_expr -COLON -package_type -RPAREN
       )
       (
       -DOT -label_longident
     | -DOT -LPAREN -seq_expr -RPAREN
     | -DOT -LBRACKET -seq_expr -RBRACKET
     | -DOT -LBRACE -expr -RBRACE
     | -SHARP -label
       )*;
   simple_labeled_expr_list:
       -labeled_simple_expr+;
   labeled_simple_expr:
       -simple_expr (* %prec below_SHARP *)
     | -label_expr;
   label_expr:
       -LABEL -simple_expr (* %prec below_SHARP *)
     | -TILDE -label_ident
     | -QUESTION -label_ident
     | -OPTLABEL -simple_expr (* %prec below_SHARP *);
   label_ident:
       -LIDENT;
   let_bindings:
       -let_binding -(-AND -let_binding)*;
   let_binding:
       -val_ident -fun_binding
     | -val_ident -COLON -typevar_list -DOT -core_type -EQUAL -seq_expr
     | -pattern -EQUAL -seq_expr;
   fun_binding:
       -strict_binding
     | -type_constraint -EQUAL -seq_expr;
   strict_binding:
       -EQUAL -seq_expr
     | -labeled_simple_pattern -fun_binding
     | -LPAREN -TYPE -LIDENT -RPAREN -fun_binding;
   match_cases:
       -pattern -match_action -(-BAR -pattern -match_action)*;
   fun_def:
       -match_action
     | -labeled_simple_pattern -fun_def
     | -LPAREN -TYPE -LIDENT -RPAREN -fun_def;
   match_action:
       -MINUSGREATER -seq_expr
     | -WHEN -seq_expr -MINUSGREATER -seq_expr;
   expr_comma_list:
       -expr -COMMA -expr -(-COMMA -expr)*;
   record_expr:
       -simple_expr -WITH -lbl_expr_list -opt_semi
     | -lbl_expr_list -opt_semi;
   lbl_expr_list:
       (
       -label_longident -EQUAL -expr
     | -label_longident
       )
       (
       -SEMI -label_longident -EQUAL -expr
     | -SEMI -label_longident
       )*;
   field_expr_list:
       -label -EQUAL -expr -(-SEMI -label -EQUAL -expr)*;
   expr_semi_list:
       -expr -(-SEMI -expr)*;
   type_constraint:
       -COLON -core_type
     | -COLON -core_type -COLONGREATER -core_type
     | -COLONGREATER -core_type
   ;

(* Patterns *)

   pattern:
       (
       -simple_pattern
     | -constr_longident -pattern (* %prec prec_constr_appl *)
     | -name_tag -pattern (* %prec prec_constr_appl *)
     | -LPAREN -COLONCOLON -RPAREN -LPAREN -pattern -COMMA -pattern -RPAREN
     | -LAZY -simple_pattern
       )
       -(
       -AS -val_ident
     | -COLONCOLON -pattern
     | -BAR -pattern
       )*
       -(-COMMA -pattern)*
   ;
   simple_pattern:
       -val_ident (* %prec below_EQUAL *)
     | -UNDERSCORE
     | -signed_constant
     | -CHAR -DOTDOT -CHAR
     | -constr_longident
     | -name_tag
     | -SHARP -type_longident
     | -LBRACE -lbl_pattern_list -record_pattern_end -RBRACE
     | -LBRACKET -pattern_semi_list -opt_semi -RBRACKET
     | -LBRACKETBAR -pattern_semi_list -opt_semi -BARRBRACKET
     | -LBRACKETBAR -BARRBRACKET
     | -LPAREN -pattern -RPAREN
     | -LPAREN -pattern -COLON -core_type -RPAREN;

   pattern_comma_list:
       -pattern -COMMA -pattern -(-COMMA -pattern)*;
   pattern_semi_list:
       -pattern -(-SEMI -pattern)*;
   lbl_pattern_list:
       (
       -label_longident -EQUAL -pattern
     | -label_longident
       )
       (
       -SEMI -label_longident -EQUAL -pattern
     | -SEMI -label_longident
       )*;
   record_pattern_end:
       -opt_semi
     | -SEMI -UNDERSCORE -opt_semi;

(* Primitive declarations *)

   primitive_declaration:
       -STRING
     | -STRING -primitive_declaration;

(* Type declarations *)

   type_declarations:
       -type_declaration -(-AND -type_declaration)*;

   type_declaration:
       -type_parameters -LIDENT -type_kind -constraints;
   constraints:
       -constraints -CONSTRAINT -constrain
     | empty;
   type_kind:
       empty
     | -EQUAL -core_type
     | -EQUAL -PRIVATE -core_type
     | -EQUAL -constructor_declarations
     | -EQUAL -PRIVATE -constructor_declarations
     | -EQUAL -private_flag -BAR -constructor_declarations
     | -EQUAL -private_flag -LBRACE -label_declarations -opt_semi -RBRACE
     | -EQUAL -core_type -EQUAL -private_flag -opt_bar -constructor_declarations
     | -EQUAL -core_type -EQUAL -private_flag -LBRACE -label_declarations -opt_semi -RBRACE;
   type_parameters:
       empty
     | -type_parameter
     | -LPAREN -type_parameter_list -RPAREN;
   type_parameter:
       -type_variance -QUOTE -ident;
   type_variance:
       empty
     | -PLUS
     | -MINUS;
   type_parameter_list:
       -type_parameter -(-COMMA -type_parameter)*;
   constructor_declarations:
       -constructor_declaration -(-BAR -constructor_declaration)*;
   constructor_declaration:
       -constr_ident -constructor_arguments;
   constructor_arguments:
       empty
     | -OF -core_type_list;
   label_declarations:
       -label_declaration -(-SEMI -label_declaration)*;
   label_declaration:
       -mutable_flag -label -COLON -poly_type;

(* "with" constraints (additional type equations over signature components) *)

   with_constraints:
       -with_constraint -(-AND -with_constraint)*;
   with_constraint:
       -TYPE -type_parameters -label_longident -with_type_binder -core_type -constraints
       (* used label_longident instead of type_longident to disallow
          functor applications in type path *)
     | -TYPE -type_parameters -label_longident -COLONEQUAL -core_type
     | -MODULE -mod_longident -EQUAL -mod_ext_longident
     | -MODULE -mod_longident -COLONEQUAL -mod_ext_longident;
   with_type_binder:
       -EQUAL
     | -EQUAL -PRIVATE;

(* Polymorphic types *)

   typevar_list:
       -(-QUOTE -ident)+;
   poly_type:
       -core_type
     | -typevar_list -DOT -core_type;

(* Core types *)

   core_type:
       -core_type2
     | -core_type2 -AS -QUOTE -ident;
   core_type2:
       (
       -simple_core_type_or_tuple
     | -QUESTION -LIDENT -COLON -core_type2 -MINUSGREATER -core_type2
     | -OPTLABEL -core_type2 -MINUSGREATER -core_type2
     | -LIDENT -COLON -core_type2 -MINUSGREATER -core_type2
       ) -(-MINUSGREATER -core_type2)*;

   simple_core_type:
       -simple_core_type2  (* %prec below_SHARP *)
     | -LPAREN -core_type_comma_list -RPAREN (* %prec below_SHARP *);
   simple_core_type2:
       (
       -QUOTE -ident
     | -UNDERSCORE
     | -type_longident
     | -LPAREN -core_type_comma_list -RPAREN -type_longident
     | -LESS -meth_list -GREATER
     | -LESS -GREATER
     | -SHARP -class_longident -opt_present
     | -LPAREN -core_type_comma_list -RPAREN -SHARP -class_longident -opt_present
     | -LBRACKET -tag_field -RBRACKET
   (* PR#3835: this is not LR(1), would need lookahead=2
     | -LBRACKET -simple_core_type2 -RBRACKET
   *)
     | -LBRACKET -BAR -row_field_list -RBRACKET
     | -LBRACKET -row_field -BAR -row_field_list -RBRACKET
     | -LBRACKETGREATER -opt_bar -row_field_list -RBRACKET
     | -LBRACKETGREATER -RBRACKET
     | -LBRACKETLESS -opt_bar -row_field_list -RBRACKET
     | -LBRACKETLESS -opt_bar -row_field_list -GREATER -name_tag_list -RBRACKET
     | -LPAREN -MODULE -package_type -RPAREN
       )
       (
       -type_longident
     | -SHARP -class_longident -opt_present
       )*;
   package_type:
       -mty_longident
     | -mty_longident -WITH -package_type_cstrs;

   package_type_cstr:
       -TYPE -LIDENT -EQUAL -core_type;
   package_type_cstrs:
       -package_type_cstr
     | -package_type_cstr -AND -package_type_cstrs;
   row_field_list:
       -row_field -(-BAR -row_field)*;
   row_field:
       -tag_field
     | -simple_core_type2;
   tag_field:
       -name_tag -OF -opt_ampersand -amper_type_list
     | -name_tag;
   opt_ampersand:
       -AMPERSAND
     | empty;
   amper_type_list:
       -core_type -(-AMPERSAND -core_type)*;
   opt_present:
       -LBRACKETGREATER -name_tag_list -RBRACKET
     | empty;
   name_tag_list:
       -name_tag+;
   simple_core_type_or_tuple:
       -simple_core_type
     | -simple_core_type -STAR -core_type_list;
   core_type_comma_list:
       -core_type -(-COMMA -core_type)*;
   core_type_list:
       -simple_core_type -(-STAR -simple_core_type)*;
   meth_list:
       -field -SEMI -meth_list
     | -field -opt_semi
     | -DOTDOT;
   field:
       -label -COLON -poly_type;
   label:
       -LIDENT;

(* Constants *)

   constant:
       -INT
     | -CHAR
     | -STRING
     | -FLOAT
     | -INT32
     | -INT64
     | -NATIVEINT;
   signed_constant:
       -constant
     | -MINUS -INT
     | -MINUS -FLOAT
     | -MINUS -INT32
     | -MINUS -INT64
     | -MINUS -NATIVEINT
     | -PLUS -INT
     | -PLUS -FLOAT
     | -PLUS -INT32
     | -PLUS -INT64
     | -PLUS -NATIVEINT;

(* Identifiers and long identifiers *)

   ident:
       -UIDENT
     | -LIDENT;
   val_ident:
       -LIDENT
     | -LPAREN -operator -RPAREN;
   operator:
       -PREFIXOP
     | -INFIXOP0
     | -INFIXOP1
     | -INFIXOP2
     | -INFIXOP3
     | -INFIXOP4
     | -BANG
     | -PLUS
     | -PLUSDOT
     | -MINUS
     | -MINUSDOT
     | -STAR
     | -EQUAL
     | -LESS
     | -GREATER
     | -OR
     | -BARBAR
     | -AMPERSAND
     | -AMPERAMPER
     | -COLONEQUAL;
   constr_ident:
       -UIDENT
   (*  | -LBRACKET -RBRACKET *)
     | -LPAREN -RPAREN
     | -COLONCOLON
   (*  | -LPAREN -COLONCOLON -RPAREN *)
     | -FALSE
     | -TRUE;
   
   val_longident:
       -val_ident
     | -mod_longident -DOT -val_ident;
   constr_longident:
       -mod_longident       (* %prec below_DOT *)
     | -LBRACKET -RBRACKET
     | -LPAREN -RPAREN
     | -FALSE
     | -TRUE;
   label_longident:
       -LIDENT
     | -mod_longident -DOT -LIDENT;
   type_longident:
       -LIDENT
     | -mod_ext_longident -DOT -LIDENT;
   mod_longident:
       -UIDENT -(-DOT -UIDENT)*;
   mod_ext_longident:
       -UIDENT
       (
       -DOT -UIDENT
     | -LPAREN -mod_ext_longident -RPAREN
       )*;
   mty_longident:
       -ident
     | -mod_ext_longident -DOT -ident;
   clty_longident:
       -LIDENT
     | -mod_ext_longident -DOT -LIDENT;
   class_longident:
       -LIDENT
     | -mod_longident -DOT -LIDENT;

(* Toplevel directives *)

   toplevel_directive:
       -SHARP -ident
     | -SHARP -ident -STRING
     | -SHARP -ident -INT
     | -SHARP -ident -val_longident
     | -SHARP -ident -FALSE
     | -SHARP -ident -TRUE;

(* Miscellaneous *)

   name_tag:
       -BACKQUOTE -ident;
   rec_flag:
       empty
     | -REC;
   direction_flag:
       -TO
     | -DOWNTO;
   private_flag:
       empty
     | -PRIVATE;
   mutable_flag:
       empty
     | -MUTABLE;
   virtual_flag:
       empty
     | -VIRTUAL;
   override_flag:
       empty
     | -BANG;
   opt_bar:
       empty
     | -BAR;
   opt_semi:
       empty
     | -SEMI;
   subtractive:
       -MINUS
     | -MINUSDOT;
   additive:
       -PLUS
     | -PLUSDOT
)
