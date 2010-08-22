signature Config_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val TOK_INT: (int) *  'a * 'a -> (svalue,'a) token
val TOK_STRING: (string) *  'a * 'a -> (svalue,'a) token
val TOK_WORD: (string) *  'a * 'a -> (svalue,'a) token
val SYM_SWERVE:  'a * 'a -> (svalue,'a) token
val SYM_EQUALS:  'a * 'a -> (svalue,'a) token
val SYM_RBRACE:  'a * 'a -> (svalue,'a) token
val SYM_LBRACE:  'a * 'a -> (svalue,'a) token
val SYM_COMMA:  'a * 'a -> (svalue,'a) token
val SYM_SEMICOLON:  'a * 'a -> (svalue,'a) token
val KW_NODE:  'a * 'a -> (svalue,'a) token
val KW_SERVER:  'a * 'a -> (svalue,'a) token
end
signature Config_LRVALS=
sig
structure Tokens : Config_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
