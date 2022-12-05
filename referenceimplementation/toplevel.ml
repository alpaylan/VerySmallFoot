open Misc
open Parsetree

let wp lex =
  let (Pprogram (cl,iteml)) =
    let ast = Parser.program Lexer.token lex in
      Parsing.clear_parser();
      ast
  in (Vcgen.vcgen iteml,cl)

let verify_wp lex =
  let fmt = !Config.formatter
  and valid = ref true
  in try Error.report
      (fun () ->
         let (l,compl) = wp lex
         in let g p = valid := (Symbsimp.check_prop p compl) & !valid
         in let f (id,provisos) =
             (Format.fprintf fmt "@.Function %s@." id;
              List.iter g provisos)
         in (for i=1 to !Config.repetitions do List.iter f l done;
             Format.fprintf fmt "@.%sValid@." (if !valid then "" else "NOT ")))
    with Invalid_argument s -> Format.pp_print_string fmt s

let verify_wp_string s =
  let lex = Lexing.from_string s
  in verify_wp lex

let verify_wp_fname fname =
  let ic = if fname = "-" then stdin else open_in fname in
  let lex = Lexing.from_channel ic in
  Lexer.init lex fname;
  init_gensym ();
  verify_wp lex;
  close_in ic
