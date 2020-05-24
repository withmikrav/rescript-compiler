'use strict';

var Block = require("./block.js");

function erase_rel(rest) {
  if (typeof rest === "number") {
    return /* End_of_fmtty */0;
  }
  switch (rest.tag | 0) {
    case /* Char_ty */0 :
        return /* Char_ty */{
                tag: 0,
                _0: erase_rel(rest._0)
              };
    case /* String_ty */1 :
        return /* String_ty */{
                tag: 1,
                _0: erase_rel(rest._0)
              };
    case /* Int_ty */2 :
        return /* Int_ty */{
                tag: 2,
                _0: erase_rel(rest._0)
              };
    case /* Int32_ty */3 :
        return /* Int32_ty */{
                tag: 3,
                _0: erase_rel(rest._0)
              };
    case /* Nativeint_ty */4 :
        return /* Nativeint_ty */{
                tag: 4,
                _0: erase_rel(rest._0)
              };
    case /* Int64_ty */5 :
        return /* Int64_ty */{
                tag: 5,
                _0: erase_rel(rest._0)
              };
    case /* Float_ty */6 :
        return /* Float_ty */{
                tag: 6,
                _0: erase_rel(rest._0)
              };
    case /* Bool_ty */7 :
        return /* Bool_ty */{
                tag: 7,
                _0: erase_rel(rest._0)
              };
    case /* Format_arg_ty */8 :
        return /* Format_arg_ty */{
                tag: 8,
                _0: rest._0,
                _1: erase_rel(rest._1)
              };
    case /* Format_subst_ty */9 :
        var ty1 = rest._0;
        return /* Format_subst_ty */{
                tag: 9,
                _0: ty1,
                _1: ty1,
                _2: erase_rel(rest._2)
              };
    case /* Alpha_ty */10 :
        return /* Alpha_ty */{
                tag: 10,
                _0: erase_rel(rest._0)
              };
    case /* Theta_ty */11 :
        return /* Theta_ty */{
                tag: 11,
                _0: erase_rel(rest._0)
              };
    case /* Any_ty */12 :
        return /* Any_ty */{
                tag: 12,
                _0: erase_rel(rest._0)
              };
    case /* Reader_ty */13 :
        return /* Reader_ty */{
                tag: 13,
                _0: erase_rel(rest._0)
              };
    case /* Ignored_reader_ty */14 :
        return /* Ignored_reader_ty */{
                tag: 14,
                _0: erase_rel(rest._0)
              };
    
  }
}

function concat_fmtty(fmtty1, fmtty2) {
  if (typeof fmtty1 === "number") {
    return fmtty2;
  }
  switch (fmtty1.tag | 0) {
    case /* Char_ty */0 :
        return /* Char_ty */{
                tag: 0,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* String_ty */1 :
        return /* String_ty */{
                tag: 1,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int_ty */2 :
        return /* Int_ty */{
                tag: 2,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int32_ty */3 :
        return /* Int32_ty */{
                tag: 3,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Nativeint_ty */4 :
        return /* Nativeint_ty */{
                tag: 4,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int64_ty */5 :
        return /* Int64_ty */{
                tag: 5,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Float_ty */6 :
        return /* Float_ty */{
                tag: 6,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Bool_ty */7 :
        return /* Bool_ty */{
                tag: 7,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Format_arg_ty */8 :
        return /* Format_arg_ty */{
                tag: 8,
                _0: fmtty1._0,
                _1: concat_fmtty(fmtty1._1, fmtty2)
              };
    case /* Format_subst_ty */9 :
        return /* Format_subst_ty */{
                tag: 9,
                _0: fmtty1._0,
                _1: fmtty1._1,
                _2: concat_fmtty(fmtty1._2, fmtty2)
              };
    case /* Alpha_ty */10 :
        return /* Alpha_ty */{
                tag: 10,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Theta_ty */11 :
        return /* Theta_ty */{
                tag: 11,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Any_ty */12 :
        return /* Any_ty */{
                tag: 12,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Reader_ty */13 :
        return /* Reader_ty */{
                tag: 13,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Ignored_reader_ty */14 :
        return /* Ignored_reader_ty */{
                tag: 14,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    
  }
}

function concat_fmt(fmt1, fmt2) {
  if (typeof fmt1 === "number") {
    return fmt2;
  }
  switch (fmt1.tag | 0) {
    case /* Char */0 :
        return /* Char */{
                tag: 0,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Caml_char */1 :
        return /* Caml_char */{
                tag: 1,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* String */2 :
        return /* String */{
                tag: 2,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Caml_string */3 :
        return /* Caml_string */{
                tag: 3,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Int */4 :
        return /* Int */{
                tag: 4,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Int32 */5 :
        return /* Int32 */{
                tag: 5,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Nativeint */6 :
        return /* Nativeint */{
                tag: 6,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Int64 */7 :
        return /* Int64 */{
                tag: 7,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Float */8 :
        return /* Float */{
                tag: 8,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Bool */9 :
        return /* Bool */{
                tag: 9,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Flush */10 :
        return /* Flush */{
                tag: 10,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* String_literal */11 :
        return /* String_literal */{
                tag: 11,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Char_literal */12 :
        return /* Char_literal */{
                tag: 12,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Format_arg */13 :
        return /* Format_arg */{
                tag: 13,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Format_subst */14 :
        return /* Format_subst */{
                tag: 14,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Alpha */15 :
        return /* Alpha */{
                tag: 15,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Theta */16 :
        return /* Theta */{
                tag: 16,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Formatting_lit */17 :
        return /* Formatting_lit */{
                tag: 17,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Formatting_gen */18 :
        return /* Formatting_gen */{
                tag: 18,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Reader */19 :
        return /* Reader */{
                tag: 19,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Scan_char_set */20 :
        return /* Scan_char_set */{
                tag: 20,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Scan_get_counter */21 :
        return /* Scan_get_counter */{
                tag: 21,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Scan_next_char */22 :
        return /* Scan_next_char */{
                tag: 22,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Ignored_param */23 :
        return /* Ignored_param */{
                tag: 23,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Custom */24 :
        return /* Custom */{
                tag: 24,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    
  }
}

exports.concat_fmtty = concat_fmtty;
exports.erase_rel = erase_rel;
exports.concat_fmt = concat_fmt;
/* No side effect */
