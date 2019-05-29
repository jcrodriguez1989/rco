# https://github.com/halpo/parser/blob/master/inst/grammar/gram.output
# https://github.com/halpo/parser/blob/master/inst/grammar/gram.y
# https://github.com/halpo/parser/blob/master/src/gram.c

constants <- c(
  "NUM_CONST",
  "STR_CONST",
  "NULL_CONST"
)

ops <- c(
  "'+'",
  "'-'",
  "'*'",
  "'/'",
  "'^'",
  "GT",
  "GE",
  "LT",
  "LE",
  "EQ",
  "NE",
  # "SPECIAL", # todo: research what is considered as SPECIAL
  "'!'",
  "AND",
  "OR",
  "AND2",
  "OR2"
)

precedence_ops <- c(
  "'('",
  "')'",
  "'{'",
  "'}'"
)
