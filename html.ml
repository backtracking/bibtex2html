
let open_document ch ftitle =
  output_string ch "<html>\n\n<head>\n";
  output_string ch "<title>"; ftitle(); output_string ch "</title>\n";
  output_string ch "</head>\n<body>\n";
  flush ch
  

let close_document ch =
  output_string ch "</body>\n</html>\n";
  flush ch


let open_balise ch s =
  output_string ch ("<" ^ s ^ ">");
  flush ch

let close_balise ch s =
  output_string ch ("</" ^ s ^ ">");
  flush ch


let anchor ch s =
  open_balise ch ("A NAME=\"" ^ s ^ "\"");
  output_string ch "\n"

let open_href ch s =
  open_balise ch ("A HREF=" ^ s)

let close_href ch =
  close_balise ch "A"

let open_h ch i =
  open_balise ch (Printf.sprintf "H%d" i)

let close_h ch i =
  close_balise ch (Printf.sprintf "H%d" i)

let open_em ch =
  open_balise ch "EM"

let close_em ch =
  close_balise ch "EM"

let open_b ch =
  open_balise ch "b"

let close_b ch =
  close_balise ch "b"

let paragraph ch =
  open_balise ch "p"

