type token = Parser.token

let to_string : token -> string = function
  | Parser.NUM f -> "NUM " ^ Float.to_string f
  | Parser.ID s -> "ID " ^ s
  | Parser.STRING s -> "STRING " ^ s
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.TIMES -> "TIMES"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.POWER -> "POWER"
  | Parser.LT -> "LT"
  | Parser.GT -> "GT"
  | Parser.LE -> "LE"
  | Parser.GE -> "GE"
  | Parser.EQ -> "EQ"
  | Parser.NE -> "NE"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACKET -> "LBRACKET"
  | Parser.RBRACKET -> "RBRACKET"
  | Parser.COMMA -> "COMMA"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.COLON -> "COLON"
  | Parser.QUEST -> "QUEST"
  | Parser.NEWLINE -> "NEWLINE"
  | Parser.EOF -> "EOF"
  | Parser.DOT_CMD s -> "DOT_CMD " ^ s

let equal (a : token) (b : token) = match a, b with
  | Parser.NUM f1, Parser.NUM f2 -> Float.equal f1 f2
  | Parser.ID s1, Parser.ID s2 -> String.equal s1 s2
  | Parser.STRING s1, Parser.STRING s2 -> String.equal s1 s2
  | Parser.DOT_CMD s1, Parser.DOT_CMD s2 -> String.equal s1 s2
  | Parser.PLUS, Parser.PLUS
  | Parser.MINUS, Parser.MINUS
  | Parser.TIMES, Parser.TIMES
  | Parser.DIVIDE, Parser.DIVIDE
  | Parser.POWER, Parser.POWER
  | Parser.LT, Parser.LT
  | Parser.GT, Parser.GT
  | Parser.LE, Parser.LE
  | Parser.GE, Parser.GE
  | Parser.EQ, Parser.EQ
  | Parser.NE, Parser.NE
  | Parser.AND, Parser.AND
  | Parser.OR, Parser.OR
  | Parser.NOT, Parser.NOT
  | Parser.LPAREN, Parser.LPAREN
  | Parser.RPAREN, Parser.RPAREN
  | Parser.LBRACKET, Parser.LBRACKET
  | Parser.RBRACKET, Parser.RBRACKET
  | Parser.COMMA, Parser.COMMA
  | Parser.SEMICOLON, Parser.SEMICOLON
  | Parser.COLON, Parser.COLON
  | Parser.QUEST, Parser.QUEST
  | Parser.NEWLINE, Parser.NEWLINE
  | Parser.EOF, Parser.EOF -> true
  | _ -> false
