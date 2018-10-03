module Parser (parseDoc) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import AST
import Graphics.PDF
import Types
import Data.Char



-- Analisis de tokens
def :: LanguageDef st
def = emptyDef { commentLine     = "#"
               , identStart      = letter
               , identLetter     = alphaNum
               , opLetter        = letter <|> char '_'
               , reservedNames   = ["empty", "mm", "px", "in", "nl", "courier",
                                    "helvetica", "times_roman", "b", "i", "s", "f",
                                    "justify", "flush_left", "flush_right", "center"]
               , reservedOpNames = ["def", "new_page", "outer_frame", "body",
                                    "image", "page_default", "set_ppi", "include",
                                    "text_default", "add", "clean", "float", "rect",
                                    "debug", "inner_frame", "line_space", "set_cont"]
               }


-- Extraccion de parsers por matching
TokenParser { parens = p_parens
            , identifier = p_identf
            , reservedOp = p_rOp
            , reserved = p_rsvd
            , whiteSpace = p_whiteSpace
            , stringLiteral = p_word
            , charLiteral = p_char
            , integer = p_int
            , braces = p_braces
            , brackets = p_brackets
            , commaSep = p_commaSep
            , comma = p_comma
            , symbol = p_symbol} = makeTokenParser def




p_stmt = char '\\' >> p_stmt'
p_modifier = char '\\' >> p_modifier'
p_content = char '\\' >> p_content'
p_rect = char '\\' >> p_rect'



-- Parsear texto en bruto hasta el cierre de llave.
-- La llave puede agregarse al texto con '\}'.

p_free_text :: Parser String
p_free_text = do lookAhead $ char '}'
                 return []
           <|> try escp
           <|> do x <- anyChar
                  xs <- p_free_text
                  return (x:xs)
    where escp = do p_symbol "\\}"
                    xs <- p_free_text
                    return ('}':xs)



p_exp :: Parser a -> Parser (Exp a)
p_exp parse_value = do v <- try parse_value
                       return (Value v)
                    <|> try (p_op Op)
                    <|> do char '$'
                           ident <- p_identf
                           return (Var ident)



p_op :: (Variable -> [Exp VType] -> a) -> Parser a
p_op cons = do char '\\'
               op <- p_identf
               args <- p_braces (p_commaSep p_arg)
               return $ cons op args
    where p_arg = do try (char '$')
                     v <- p_identf
                     return $ Var v
                <|> do e <- p_braces p_vtype
                       return $ Value e



p_vtype :: Parser VType
p_vtype = do r <- try p_rect
             return $ VRect r
       <|> do c <- try p_content
              return $ VCont c
       <|> do d <- try p_lpair
              return $ VLenPair d
       <|> do l <- try p_length
              return $ VLength l
       <|> do n <- try p_int
              return $ VInt n
       <|> do vd <- p_doc
              return $ VDoc vd
    where p_var_doc = do d <- p_doc
                         d' <- p_try_var
                         return $ d ++ d'
          p_try_var = do v <- try (char '$' >> p_identf)
                         d <- p_var_doc
                         return $ (StmtVar v):d
                    <|> return []



p_intPair :: Parser (Integer,Integer)
p_intPair = do x <- p_int
               p_comma
               y <- p_int
               return (x,y)



p_length :: Parser Length
p_length = do x <- p_int
              mg <- p_mgn
              return $ mg x
    where p_mgn = do p_rsvd "mm"
                     return MM
                <|> do p_rsvd "px"
                       return Pixel
                <|> do p_rsvd "in"
                       return Inch



p_lpair :: Parser LenPair
p_lpair = do x <- p_exp p_length
             p_comma
             y <- p_exp p_length
             return (x,y)



p_font :: Parser FontName
p_font = do p_rsvd "courier"
            return Courier
      <|> do p_rsvd "times_roman"
             return Times_Roman
      <|> do p_rsvd "helvetica"
             return Helvetica



p_content' :: Parser ParserCont
p_content' = do p_rOp "image"
                img <- p_braces p_free_text
                return $ Cont_image img
          <|> do p_rOp "body"
                 algn <- p_braces p_alignment
                 return $ Cont_body algn
          <|> do p_rOp "float"
                 algn <- p_braces p_alignment
                 txt <- p_braces p_free_text
                 return $ Cont_float_txt algn txt
          <|> do p_rsvd "empty"
                 return Cont_empty
    where p_alignment = do try $ p_rsvd "justify"
                           return Justified
                      <|> do try $ p_rsvd "flush_left"
                             return FlushedLeft
                      <|> do try $ p_rsvd "center"
                             return Center
                      <|> do p_rsvd "flush_right"
                             return FlushedRight



p_rect' :: Parser ParserRect
p_rect' = do p_rOp "rect"
             pos <- p_braces p_points
             mrgns <- p_braces (p_exp p_lpair)
             return $ ParserRect pos mrgns
      <|> do p_rOp "inner_frame"
             framing In_frame
      <|> do p_rOp "outer_frame"
             framing Out_frame
      <|> do p_rOp "clean"
             r <- p_braces (p_exp p_rect)
             return $ Clean r
      <|> do p_rOp "set_cont"
             r <- p_braces (p_exp p_rect)
             c <- p_braces (p_exp p_content)
             return $ Rect_set r c
    where p_points = do p1 <- p_exp p_lpair
                        p_comma
                        p2 <- p_exp p_lpair
                        return (p1, p2)
          framing st = do r <- p_braces (p_exp p_rect)
                          edgs <- p_braces (p_commaSep p_edge)
                          return $ st r (concat edgs)
          p_edge = p_symbol "u" <|> p_symbol "d"
                <|> p_symbol "l" <|> p_symbol "r"



p_modifier' :: Parser Doc
p_modifier' = do char 'b'
                 doc <- p_braces p_doc
                 return $ (Text_bold):doc ++ [Text_bold_off]
          <|> do char 'i'
                 doc <- p_braces p_doc
                 return $ (Text_italics):doc ++ [Text_ital_off]
          <|> do char 's'
                 size <- p_braces (p_exp p_int)
                 doc <- p_braces p_doc
                 return $ (Text_resize size):doc ++ [Text_size_normal]
          <|> do char 'f'
                 font <- p_braces p_font
                 doc <- p_braces p_doc
                 return $ (Text_font font):doc ++ [Text_font_normal]
          <|> do p_rsvd "nl"
                 return [Text_return]



p_stmt' :: Parser Stmt
p_stmt' = do p_rOp "def"
             v <- p_braces p_identf
             params <- p_params
             e <- p_braces p_vtype
             return $ Def v params e
       <|> do p_rOp "set_ppi"
              i <- p_braces p_int
              return $ PPI i
       <|> do p_rOp "add"
              rs <- p_braces p_manyRects
              return $ Add_rects rs
       <|> do p_rOp "page_default"
              s <- p_braces (p_exp p_lpair)
              rs <- p_braces p_manyRects
              return $ Page_dflt s rs
       <|> do try (p_rOp "new_page" >> notFollowedBy (char '{'))
              return $ Newpage_dflt
       <|> do p_rOp "new_page"
              size <- p_braces (p_exp p_lpair)
              rs <- p_braces p_manyRects
              return $ Newpage size rs
       <|> do p_rOp "text_default"
              f <- p_braces p_font
              s <- p_braces p_int
              return $ Text_dflt f s
       <|> do p_rOp "line_space"
              i <- p_braces (p_exp p_int)
              return $ Text_line_space i
       <|> do p_rOp "include"
              file <- p_braces p_free_text
              return $ Include file
       <|> do p_rOp "debug"
              return Debug
    where p_params = (try $ p_brackets (p_commaSep p_identf))
                    <|> return []
          p_manyRects = p_commaSep $ p_exp p_rect



-- No uso lexemes para no eliminar espacios del texto
p_escape :: Parser Stmt
p_escape = do char '\\'
              s <- (oneOf "\\{}$")
              return $ Text_value [s]



p_doc :: Parser Doc
p_doc = do lookAhead $ char '}'
           return []
    <|> do p <- prs
           ps <- p_doc
           return $ conc p ps
    <|> do eof
           return []
    <?> "any statement, operation or text. Remember\
         \ to escape '{', '}', '\\' and '$'"
  where prs = try p_modifier
            <|> do st <- try p_stmt
                   return [st]
            <|> do esc <- try p_escape
                   return [esc]
            <|> do op <- try (p_op StmtOp)
                   return [op]
            <|> do char '$'
                   var <- p_identf <?> "variable. Escape '$' with '\\'"
                   return [StmtVar var]
            <|> do t <- noneOf "\\{}$"
                   return [Text_value [t]]
        conc [Text_value x] ((Text_value y):ys) = (Text_value (x++y)) : ys
        conc xs ys = xs++ys





---------------------------------------------------------------------
---------------------- Función total de parseo ----------------------
---------------------------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do p_whiteSpace
                 t <- p
                 eof
                 return t


parseDoc :: String -> Either ParseError Doc
parseDoc = parse (totParser p_doc) ""

