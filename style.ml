

let six = 
  ANSITerminal.(print_string [on_black; Foreground White] 
                  "6       ");
  ANSITerminal.(print_string [Reset] 
                  "\n");
  for i = 0 to 2 do
    ANSITerminal.(print_string [on_black; Foreground White] 
                    "  \xE2\x99\xA0 \xE2\x99\xA0   ");
    ANSITerminal.(print_string [Reset] 
                    "\n");
  done;
  ANSITerminal.(print_string [on_black; Foreground White] 
                  "      6");
  ANSITerminal.(print_string [Reset] 
                  "\n")