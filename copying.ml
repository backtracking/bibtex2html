let copying () =
  prerr_endline "
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License version 2 for more details
(enclosed in the file GPL).";
  flush stderr

let banner softname =
  Printf.eprintf "This is %s version %s, compiled on %s\n"
    softname Version.version Version.date;
  Printf.eprintf "Copyright (c) 1997,1998,1999 Jean-Christophe Filliâtre and Claude Marché\n";
  Printf.eprintf "This is free software with ABSOLUTELY NO WARRANTY (use option --warranty)\n\n";
  flush stderr

