
let open_document ch ftitle =
  output_string ch "<html>\n\n";
  output_string ch "<head>\n";
  output_string ch "<title>"; ftitle(); output_string ch "</title>\n";
  output_string ch "</head>\n";
  output_string ch "<body>\n";
  flush ch
  

let close_document ch =
  output_string ch "</body>\n";
  output_string ch "</html>\n";
  flush ch


let output_anchor ch s =
  output_string ch ("<A NAME=" ^ s ^ ">\n");
  flush ch

let open_href ch s =
  output_string ch ("<A HREF=" ^ s ^ ">");
  flush ch

let close_href ch =
  output_string ch "</A>";
  flush ch

let open_h ch i =
  output_string ch ("<H" ^ (string_of_int i) ^ ">\n");
  flush ch

let close_h ch i =
  output_string ch ("</H" ^ (string_of_int i) ^ ">\n");
  flush ch

let open_em ch =
  output_string ch ("<EM>\n");
  flush ch

let close_em ch =
  output_string ch ("</EM>\n");
  flush ch
