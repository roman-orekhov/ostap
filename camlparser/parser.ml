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

type ('a, 'b) stream =
   < getCHAR : ('a, Matcher.Token.t, 'b) parsed;
     getEOF : ('a, Matcher.Token.t, 'b) parsed;
     getFLOAT : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP0 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP1 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP2 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP3 : ('a, Matcher.Token.t, 'b) parsed;
     getINFIXOP4 : ('a, Matcher.Token.t, 'b) parsed;
     getINT : ('a, Matcher.Token.t, 'b) parsed;
     getINT32 : ('a, Matcher.Token.t, 'b) parsed;
     getINT64 : ('a, Matcher.Token.t, 'b) parsed;
     getLABEL : ('a, Matcher.Token.t, 'b) parsed;
     getLIDENT : ('a, Matcher.Token.t, 'b) parsed;
     getNATIVEINT : ('a, Matcher.Token.t, 'b) parsed;
     getOPTLABEL : ('a, Matcher.Token.t, 'b) parsed;
     getPREFIXOP : ('a, Matcher.Token.t, 'b) parsed;
     getSTRING : ('a, Matcher.Token.t, 'b) parsed;
     getUIDENT : ('a, Matcher.Token.t, 'b) parsed;
     look : string -> ('a, Matcher.Token.t, 'b) parsed;
     regexp :
      ?except:(string -> bool) ->
      string ->
      string ->
      string -> ('a, Matcher.Token.t, 'b) parsed
     .. > as 'a

ostap (
   kw[w]: -(@( w^"\\b" ? w^" " : "keyword "^w));
   implementation: -structure -EOF;
   interface: -signature -EOF;
   toplevel_phrase:
       -top_structure -";;"
     | -seq_expr -";;"
     | -toplevel_directive -";;"
     | -EOF;
   top_structure:
       -structure_item+;
   use_file:
       -use_file_tail
     | -seq_expr -use_file_tail;
   use_file_tail:
       -EOF
     | -";;" (-EOF
            | -seq_expr -use_file_tail
            | -structure_item -use_file_tail
            | -toplevel_directive -use_file_tail)
     | -structure_item -use_file_tail
     | -toplevel_directive -use_file_tail;

(* Module expressions *)

   module_expr:
       -(
       -mod_longident
     | -kw["struct"] -structure -kw["end"]
     | -kw["functor"] -"(" -UIDENT -":" -module_type -")" -"->" -module_expr
     | -"(" (-module_expr (-":" -module_type -")" | -")")
           | -kw["val"] -expr -":" -package_type -")")
       )
       -(-"(" -module_expr -")")*;
   structure:
       -structure_tail
     | -seq_expr -structure_tail;
   structure_tail:
       -";;"
         -(-seq_expr -structure_tail
        | -structure_item -structure_tail)?
     | -structure_item -structure_tail
     | -empty;
   structure_item:
       -kw["let"] -rec_flag -let_bindings
     | -kw["external"] -val_ident -":" -core_type -"=" -primitive_declaration
     | -kw["type"] -type_declarations
     | -kw["exception"] -UIDENT (-constructor_arguments | -"=" -constr_longident)
     | -kw["module"]  (-UIDENT -module_binding
                     | -kw["rec"] -module_rec_bindings
                     | -kw["type"] -ident -"=" -module_type)
     | -kw["open"] -mod_longident
     | -kw["class"]   (-class_declarations
                     | -kw["type"] -class_type_declarations)
     | -kw["include"] -module_expr;
   module_binding:
       -"=" -module_expr
     | -":" -module_type -"=" -module_expr
     | -"(" -UIDENT -":" -module_type -")" -module_binding;
   module_rec_bindings:
       -module_rec_binding -(-kw["and"] -module_rec_binding)*;
   module_rec_binding:
       -UIDENT -":" -module_type -"=" -module_expr;

(* Module types *)

   module_type:
       -(
       -mty_longident
     | -kw["sig"] -signature -kw["end"]
     | -kw["functor"] -"(" -UIDENT -":" -module_type -")" -"->" -module_type
         (*%prec below_WITH*)
     | -kw["module"] -kw["type"] -kw["of"] -module_expr
     | -"(" -module_type -")"
       )
       -(-kw["with"] -with_constraints)*;
   signature:
       -(-signature_item -";;"?)*;
   signature_item:
       -kw["val"] -val_ident -":" -core_type
     | -kw["external"] -val_ident -":" -core_type -"=" -primitive_declaration
     | -kw["type"] -type_declarations
     | -kw["exception"] -UIDENT -constructor_arguments
     | -kw["module"] (-UIDENT -module_declaration
                    | -kw["rec"] -module_rec_declarations
                    | -kw["type"] -ident -(-"=" -module_type)?)
     | -kw["open"] -mod_longident
     | -kw["include"] -module_type
     | -kw["class"] (-class_descriptions | -kw["type"] -class_type_declarations);

   module_declaration:
       -":" -module_type
     | -"(" -UIDENT -":" -module_type -")" -module_declaration;
   module_rec_declarations:
       -module_rec_declaration -(-kw["and"] -module_rec_declaration)*;
   module_rec_declaration:
       -UIDENT -":" -module_type;

(* Class expressions *)

   class_declarations:
       -class_declaration -(-kw["and"] -class_declaration)*;
   class_declaration:
       -virtual_flag -class_type_parameters -LIDENT -class_fun_binding;
   class_fun_binding:
       -"=" -class_expr
     | -":" -class_type -"=" -class_expr
     | -labeled_simple_pattern -class_fun_binding;
   class_type_parameters:
       -(-"[" -type_parameter_list -"]")?;
   class_fun_def:
       -labeled_simple_pattern+ -"->" -class_expr;
   class_expr:
       -class_simple_expr -simple_labeled_expr_list?
     | -kw["fun"] -class_fun_def
     | -kw["let"] -rec_flag -let_bindings -kw["in"] -class_expr;
   class_simple_expr:
       -"[" -core_type_comma_list -"]" -class_longident
     | -class_longident
     | -kw["object"] -class_structure -kw["end"]
     | -"(" -class_expr (-":" -class_type -")" | -")");
   class_structure:
       -class_self_pattern -class_fields;
   class_self_pattern:
       -"(" -pattern (-")" | -core_type -")")
     | -empty;
   class_fields:
       -(
       -kw["inherit"] -override_flag -class_expr -parent_binder
     | -kw["val"] (-virtual_value | -value)
     | -virtual_method
     | -concrete_method
     | -kw["constraint"] -constrain
     | -kw["initializer"] -seq_expr
       )*;
   parent_binder:
       -(-kw["as"] -LIDENT)?;
   virtual_value:
       -kw["mutable"] -kw["virtual"] -label -":" -core_type
     | -kw["virtual"] -mutable_flag -label -":" -core_type;
   value:
       -override_flag -mutable_flag -label (-"=" -seq_expr | -type_constraint -"=" -seq_expr);
   virtual_method:
       -kw["method"] (-kw["private"] -kw["virtual"] -label -":" -poly_type
                    | -kw["virtual"] -private_flag -label -":" -poly_type);
   concrete_method :
       -kw["method"] -override_flag -private_flag -label (-strict_binding | -":" -poly_type -"=" -seq_expr);

(* Class types *)

   class_type:
       -class_signature
     | -"?" -LIDENT -":" -simple_core_type_or_tuple -"->" -class_type
     | -LIDENT -":" -simple_core_type_or_tuple -"->" -class_type
     | -simple_core_type_or_tuple -"->" -class_type;
   class_signature:
       -"[" -core_type_comma_list -"]" -clty_longident
     | -clty_longident
     | -kw["object"] -class_sig_body -kw["end"];
   class_sig_body:
       -class_self_type -class_sig_fields;
   class_self_type:
       -(-"(" -core_type -")")?;
   class_sig_fields:
       -(
       -kw["inherit"] -class_signature
     | -kw["val"] -value_type
     | -virtual_method_type
     | -method_type
     | -kw["constraint"] -constrain
       )*;
   value_type:
       -kw["virtual"] -mutable_flag -label -":" -core_type
     | -kw["mutable"] -virtual_flag -label -":" -core_type
     | -label -":" -core_type;
   method_type:
       -kw["method"] -private_flag -label -":" -poly_type;
   virtual_method_type:
       -kw["method"] (-kw["private"] -kw["virtual"] -label -":" -poly_type
                    | -kw["virtual"] -private_flag -label -":" -poly_type);
   constrain:
       -core_type -"=" -core_type;
   class_descriptions:
       -class_description -(-kw["and"] -class_description)*;
   class_description:
       -virtual_flag -class_type_parameters -LIDENT -":" -class_type;
   class_type_declarations:
       -class_type_declaration -(-kw["and"] -class_type_declaration)*;
   class_type_declaration:
       -virtual_flag -class_type_parameters -LIDENT -"=" -class_signature;

(* Core expressions *)

   seq_expr:
(*
       -expr -";" -seq_expr
     | -expr -";"?;
*)
       -expr -(-";" -expr)* -";"?;
   labeled_simple_pattern:
       -"?" -"(" -label_let_pattern -opt_default -")"
     | -OPTLABEL (-"(" -let_pattern -opt_default -")" | -pattern_var)
     | -"?" -label_var
     | -"~" -"(" -label_let_pattern -")"
     | -LABEL -simple_pattern
     | -"~" -label_var
     | -simple_pattern;
   pattern_var:
       -LIDENT
(*
     | -kw["_"]
*);
   opt_default:
       -(-"=" -seq_expr)?;
   label_let_pattern:
       -label_var -(-":" -core_type)?;
   label_var:
       -LIDENT;
   let_pattern:
       -pattern -(-":" -core_type)?;
   expr:
       -(
       -kw["let"] (-rec_flag -let_bindings -kw["in"] -seq_expr
                 | -kw["module"] -UIDENT -module_binding -kw["in"] -seq_expr
                 | -kw["open"] -mod_longident -kw["in"] -seq_expr)
     | -kw["function"] -opt_bar -match_cases
     | -kw["fun"] (-labeled_simple_pattern -fun_def
                 | -"(" -kw["type"] -LIDENT -")" -fun_def)
     | -kw["match"] -seq_expr -kw["with"] -opt_bar -match_cases
     | -kw["try"] -seq_expr -kw["with"] -opt_bar -match_cases
     | -constr_longident -simple_expr (* %prec below_SHARP *)
     | -name_tag -simple_expr (* %prec below_SHARP *)
     | -kw["if"] -seq_expr -kw["then"] -expr -(-kw["else"] -expr)?
     | -kw["while"] -seq_expr -kw["do"] -seq_expr -kw["done"]
     | -kw["for"] -val_ident -"=" -seq_expr -direction_flag -seq_expr -kw["do"] -seq_expr -kw["done"]
     | -"(" -"::" -")" -"(" -expr -"," -expr -")"
     | -subtractive -expr (* %prec prec_unary_minus *)
     | -additive -expr (* %prec prec_unary_plus *)
     | -simple_expr -"." (-label_longident -"<-" -expr
                        | -"(" -seq_expr -")" -"<-" -expr
                        | -"[" -seq_expr -"]" -"<-" -expr
                        | -"{" -expr -"}" -"<-" -expr)
     | -label -"<-" -expr
     | -simple_expr -simple_labeled_expr_list?
     | -kw["assert"] -simple_expr (* %prec below_SHARP *)
     | -kw["lazy"] -simple_expr (* %prec below_SHARP *)
     | -kw["object"] -class_structure -kw["end"]
       )
       -(
       -":=" -expr
     | -kw["or"] -expr
     | -INFIXOP0 -expr
     | -"||" -expr
     | -"&&" -expr
     | -"&" -expr
     | -"=" -expr
     | -"<" -expr
     | -">" -expr
     | -INFIXOP1 -expr
     | -"::" -expr
     | -INFIXOP2 -expr
     | -"+." -expr
     | -"+" -expr
     | -"-." -expr
     | -"-" -expr
     | -INFIXOP3 -expr
     | -INFIXOP4 -expr
     | -"*" -expr
       )*
       -(-"," -expr)*;
   simple_expr:
       -(
       -val_longident
     | -constant
     | -constr_longident (* %prec prec_constant_constructor *)
     | -name_tag (* %prec prec_constant_constructor *)
     | -"(" -seq_expr (-")" | -type_constraint -")")
     | -kw["begin"] -seq_expr? -kw["end"]
     | -mod_longident -"." -"(" -seq_expr -")"
     | -"{" -record_expr -"}"
     | -"[|" (-expr_semi_list -opt_semi -"|]" | -"|]")
     | -"[" -expr_semi_list -opt_semi -"]"
     | -"!" -simple_expr
     | -PREFIXOP -simple_expr
     | -kw["new"] -class_longident
     | -"{<" (-field_expr_list -opt_semi -">}" | -">}")
     | -"(" -kw["module"] -module_expr -":" -package_type -")"
       )
       -(
       -"." (-label_longident
           | -"(" -seq_expr -")"
           | -"[" -seq_expr -"]"
           | -"{" -expr -"}")
     | -"#" -label
       )*;
   simple_labeled_expr_list:
       -labeled_simple_expr+;
   labeled_simple_expr:
       -simple_expr (* %prec below_SHARP *)
     | -label_expr;
   label_expr:
       -LABEL -simple_expr (* %prec below_SHARP *)
     | -"~" -label_ident
     | -OPTLABEL -simple_expr (* %prec below_SHARP *)
     | -"?" -label_ident;
   label_ident:
       -LIDENT;
   let_bindings:
       -let_binding -(-kw["and"] -let_binding)*;
   let_binding:
       -val_ident (-fun_binding
                 | -":" -typevar_list -"." -core_type -"=" -seq_expr)
     | -pattern -"=" -seq_expr;
   fun_binding:
       -strict_binding
     | -type_constraint -"=" -seq_expr;
   strict_binding:
       -"=" -seq_expr
     | -labeled_simple_pattern -fun_binding
     | -"(" -kw["type"] -LIDENT -")" -fun_binding;
   match_cases:
       -pattern -match_action -(-"|" -pattern -match_action)*;
   fun_def:
       -match_action
     | -labeled_simple_pattern -fun_def
     | -"(" -kw["type"] -LIDENT -")" -fun_def;
   match_action:
       -"->" -seq_expr
     | -kw["when"] -seq_expr -"->" -seq_expr;
   expr_comma_list:
       -expr -"," -expr -(-"," -expr)*;
   record_expr:
       -simple_expr -kw["with"] -lbl_expr_list -opt_semi
     | -lbl_expr_list -opt_semi;
   lbl_expr_list:
       -label_longident -(-"=" -expr)? -(-";" -label_longident -(-"=" -expr)?)*;
   field_expr_list:
       -label -"=" -expr -(-";" -label -"=" -expr)*;
   expr_semi_list:
       -expr -(-";" -expr)*;
   type_constraint:
       -":" -core_type -(-":>" -core_type)?
     | -":>" -core_type;

(* Patterns *)

   pattern:
       (
       -constr_longident -pattern (* %prec prec_constr_appl *)
     | -name_tag -pattern (* %prec prec_constr_appl *)
     | -simple_pattern
     | -"(" -"::" -")" -"(" -pattern -"," -pattern -")"
     | -kw["lazy"] -simple_pattern
       )
       -(
       -kw["as"] -val_ident
     | -"::" -pattern
     | -"|" -pattern
       )*
       -(-"," -pattern)*
   ;
   simple_pattern:
       -val_ident (* %prec below_EQUAL *)
(*
     | -kw["_"]
*)
     | -CHAR -".." -CHAR
     | -signed_constant
     | -constr_longident
     | -name_tag
     | -"#" -type_longident
     | -"{" -lbl_pattern_list -record_pattern_end -"}"
     | -"[" -pattern_semi_list -opt_semi -"]"
     | -"[|" (-pattern_semi_list -opt_semi -"|]" | -"|]")
     | -"(" -pattern (-")" | -":" -core_type -")");

   pattern_comma_list:
       -pattern -"," -pattern -(-"," -pattern)*;
   pattern_semi_list:
       -pattern -(-";" -pattern)*;
   lbl_pattern_list:
       -label_longident -(-"=" -pattern)? -(-";" -label_longident -(-"=" -pattern)?)*;
   record_pattern_end:
       -(-";" -kw["_"])? -opt_semi;

(* Primitive declarations *)

   primitive_declaration:
       -STRING+;

(* Type declarations *)

   type_declarations:
       -type_declaration -(-kw["and"] -type_declaration)*;

   type_declaration:
       -type_parameters -LIDENT -type_kind -constraints;
   constraints:
       -(-kw["constraint"] -constrain)*;
   type_kind:
       -"=" (-core_type
           | -constructor_declarations
           | -kw["private"] (-core_type | -constructor_declarations)
           | -private_flag (-"|" -constructor_declarations
                          | -"{" -label_declarations -opt_semi -"}")
           | -core_type -"=" -private_flag (-opt_bar -constructor_declarations
                                          | -"{" -label_declarations -opt_semi -"}"))
     | -empty;
   type_parameters:
       -type_parameter
     | -"(" -type_parameter_list -")"
     | -empty;
   type_parameter:
       -type_variance -"'" -ident;
   type_variance:
       -"+"
     | -"-"
     | -empty;
   type_parameter_list:
       -type_parameter -(-"," -type_parameter)*;
   constructor_declarations:
       -constructor_declaration -(-"|" -constructor_declaration)*;
   constructor_declaration:
       -constr_ident -constructor_arguments;
   constructor_arguments:
       -(-kw["of"] -core_type_list)?;
   label_declarations:
       -label_declaration -(-";" -label_declaration)*;
   label_declaration:
       -mutable_flag -label -":" -poly_type;

(* "with" constraints (additional type equations over signature components) *)

   with_constraints:
       -with_constraint -(-kw["and"] -with_constraint)*;
   with_constraint:
       -kw["type"] -type_parameters -label_longident (-with_type_binder -core_type -constraints
       (* used label_longident instead of type_longident to disallow
          functor applications in type path *)
                                                    | -":=" -core_type)
     | -kw["module"] -mod_longident (-"=" -mod_ext_longident | -":=" -mod_ext_longident);
   with_type_binder:
       -"=" -kw["private"]?;

(* Polymorphic types *)

   typevar_list:
       -(-"'" -ident)+;
   poly_type:
       -core_type
     | -typevar_list -"." -core_type;

(* Core types *)

   core_type:
       -core_type2 -(-kw["as"] -"'" -ident)?;
   core_type2:
       -(
       -"?" -LIDENT -":" -core_type2 -"->" -core_type2
     | -LIDENT -":" -core_type2 -"->" -core_type2
     | -simple_core_type_or_tuple
       )
       -(-"->" -core_type2)*;

   simple_core_type:
       -simple_core_type2  (* %prec below_SHARP *)
     | -"(" -core_type -")" (* %prec below_SHARP *);
   simple_core_type2:
       -(
       -"'" -ident
(*
     | -kw["_"]
*)
     | -type_longident
     | -"(" -core_type_comma_list -")" -type_longident
     | -"<" (-meth_list -">" | -">")
     | -"#" -class_longident -opt_present
     | -"(" -core_type_comma_list -")" -"#" -class_longident -opt_present
     | -"[" (-tag_field -"]"
   (* PR#3835: this is not LR(1), would need lookahead=2
     | -"[" -simple_core_type2 -"]"
   *)
           | -"|" -row_field_list -"]"
           | -row_field -"|" -row_field_list -"]")
     | -"[>" (-opt_bar -row_field_list -"]" | -"]")
     | -"[<" -opt_bar -row_field_list (-"]" | -">" -name_tag_list -"]")
     | -"(" -kw["module"] -package_type -")"
       )
       -(
       -type_longident
     | -"#" -class_longident -opt_present
       )*;
   package_type:
       -mty_longident -(-kw["with"] -package_type_cstrs)?;

   package_type_cstr:
       -kw["type"] -LIDENT -"=" -core_type;
   package_type_cstrs:
       -package_type_cstr -(-kw["and"] -package_type_cstr)*;
   row_field_list:
       -row_field -(-"|" -row_field)*;
   row_field:
       -tag_field
     | -simple_core_type2;
   tag_field:
       -name_tag -(-kw["of"] -opt_ampersand -amper_type_list)?;
   opt_ampersand:
       -"&"?;
   amper_type_list:
       -core_type -(-"&" -core_type)*;
   opt_present:
       -(-"[>" -name_tag_list -"]")?;
   name_tag_list:
       -name_tag+;
   simple_core_type_or_tuple:
       -simple_core_type -(-"*" -core_type_list)?;
   core_type_comma_list:
       -core_type -(-"," -core_type)*;
   core_type_list:
       -simple_core_type -(-"*" -simple_core_type)*;
   meth_list:
       -field (-";" -meth_list | -opt_semi)
     | -"..";
   field:
       -label -":" -poly_type;
   label:
       -LIDENT;

(* Constants *)

   constant:
       -CHAR
     | -STRING
     | -FLOAT
     | -INT32
     | -INT64
     | -NATIVEINT
     | -INT;
   signed_constant:
       -constant
     | -"-" (-FLOAT
           | -INT32
           | -INT64
           | -NATIVEINT
           | -INT)
     | -"+" (-FLOAT
           | -INT32
           | -INT64
           | -NATIVEINT
           | -INT);

(* Identifiers and long identifiers *)

   ident:
       -UIDENT
     | -LIDENT;
   val_ident:
       -LIDENT
     | -"(" -operator -")";
   operator:
       -":="
     | -kw["or"]
     | -PREFIXOP
     | -INFIXOP0
     | -"||"
     | -"&&"
     | -"&"
     | -"="
     | -"<"
     | -">"
     | -"!"
     | -INFIXOP1
     | -INFIXOP2
     | -"+."
     | -"+"
     | -"-."
     | -"-"
     | -INFIXOP3
     | -"*"
     | -INFIXOP4;
   constr_ident:
       -UIDENT
   (*  | -"[" -"]" *)
     | -"(" -")"
     | -"::"
   (*  | -"(" -"::" -")" *)
     | -kw["false"]
     | -kw["true"];
   
   val_longident:
       -val_ident
     | -mod_longident -"." -val_ident;
   constr_longident:
       -mod_longident       (* %prec below_DOT *)
     | -"[" -"]"
     | -"(" -")"
     | -kw["false"]
     | -kw["true"];
   label_longident:
       -LIDENT
     | -mod_longident -"." -LIDENT;
   type_longident:
       -LIDENT
     | -mod_ext_longident -"." -LIDENT;
   mod_longident:
       -UIDENT -(-"." -UIDENT)*;
   mod_ext_longident:
       -UIDENT
       (
       -"." -UIDENT
     | -"(" -mod_ext_longident -")"
       )*;
   mty_longident:
       -(-mod_ext_longident -".")? -ident;
   clty_longident:
       -LIDENT
     | -mod_ext_longident -"." -LIDENT;
   class_longident:
       -LIDENT
     | -mod_longident -"." -LIDENT;

(* Toplevel directives *)

   toplevel_directive:
       -"#" -ident -(-STRING
                  | -INT
                  | -val_longident
                  | -kw["false"]
                  | -kw["true"])?;

(* Miscellaneous *)

   name_tag:
       -"`" -ident;
   rec_flag:
       -kw["rec"]?;
   direction_flag:
       -kw["to"]
     | -kw["downto"];
   private_flag:
       -kw["private"]?;
   mutable_flag:
       -kw["mutable"]?;
   virtual_flag:
       -kw["virtual"]?;
   override_flag:
       -"!"?;
   opt_bar:
       -"|"?;
   opt_semi:
       -";"?;
   subtractive:
       -"-."
     | -"-";
   additive:
       -"+."
     | -"+"
)
