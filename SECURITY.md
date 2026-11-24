# Security and privacy

Report security problems privately to Alek Rutkowski at
alek.rutkowski@gmail.com. Include a minimal reproducible example and platform
details. Do not post credentials or private command history in an issue.

The package does not transmit data, install code on startup, or automatically
open links. Opening a generated AI prompt URL shares its encoded error context
with the selected service and may submit the prompt there immediately. Providers
without prompt URLs require an explicit copy and paste. `:explain minimal`
omits source and trace; `link_context = FALSE` makes this the default. Optional
history files store source code in plain RDS form;
they are not encrypted.

Terminal control characters are escaped before displaying inspected names,
documentation and conditions. Explicitly evaluated R code retains ordinary R
permissions, including its ability to print control sequences, access files
and call external programs. The console is not a sandbox for untrusted R code.

Bracketed paste does not execute pasted newlines when the terminal supports
that protocol. A terminal that sends a paste as ordinary keystrokes cannot
provide that distinction. Package metadata inspection avoids active bindings
and deferred promises; explicitly previewing custom objects can invoke their
formatting methods.
