library(rco)

code <- paste(
 "if (TRUE) {",
 "  live_code()",
 "} else {",
 "  dead_code()",
 "}",
 "for (i in 1:100) {",
 "  live_code()",
 "  break",
 "  dead_code()",
 "}",
 sep = "\n"
)
cat(opt_dead_code(list(code))$codes[[1]])
