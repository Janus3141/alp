module Parser (parser, parseDoc) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import AST
import Graphics.PDF.Text
import Types
import Data.Char


-----------------------


-- Analizador de Tokens
def :: LanguageDef st
def = emptyDef { commentLine     = "#"
               , identStart      = letter
               , identLetter     = alphaNum
               , opLetter        = letter <|> char '_'
               , reservedNames   = ["empty", "cm", "px", "in", "nl", "courier",
                                    "helvetica", "times_roman", "none", "last_page"]
               , reservedOpNames = ["def", "new_page", "new_grid", "sub", "main",
                                    "image", "page_default", "set_ppi", "vrt",
                                    "hrz", "grid_default", "text_default", "insert",
                                    "clean", "debug", "inner_frame", "outer_frame",
                                    "line_space", "body", "float", "include",
                                    "set_content", "set_grid"]
               }


-- Extraccion de parsers por matching
TokenParser { parens = p_parens
            , identifier = p_identf
            , reservedOp = p_op
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



p_expression :: Parser VType
p_expression = do g <- try p_grid
                  return $ VGrid g
            <|> do c <- try p_content
                   return $ VCont c
            <|> do d <- try p_dimension
                   return $ VDim d
            <|> do l <- try p_length
                   return $ VLength l
            <|> do n <- try p_int
                   return $ VInt n
            <|> do vd <- p_var_doc
                   return $ VDoc vd
    where p_var_doc = do d <- p_doc
                         d' <- p_try_var
                         return $ d ++ d'
          p_try_var = do v <- try (char '$' >> p_identf)
                         d <- p_var_doc
                         return $ (StmtVar v):d
                    <|> return []
                         



p_grid' :: Parser GridTerm
p_grid' = do p_op "new_grid"
             rec <- p_braces (p_exp p_grid)
             sz <- p_braces (p_exp p_dimension)
             cols <- p_braces p_int
             rows <- p_braces p_int
             return $ Grid_new rec sz cols rows
      <|> do p_op "sub"
             parnt <- p_braces (p_exp p_grid)
             col <- p_braces p_int
             row <- p_braces p_int
             return $ Grid_sub parnt col row
      <|> do p_op "main"
             page <- p_braces p_int
             return $ Grid_main page
      <|> do p_op "cp"
             rec <- p_braces (p_exp p_grid)
             return $ Grid_Copy rec
      <|> do p_rsvd "none"
             return Grid_none



p_length :: Parser Length
p_length = do x <- p_int
              mg <- p_mgn
              return $ mg x
    where p_mgn = do p_rsvd "cm"
                     return CM
                <|> do p_rsvd "px"
                       return Pixel
                <|> do p_rsvd "in"
                       return Inch



p_font :: Parser FontName
p_font = do p_rsvd "courier"
            return Courier
      <|> do p_rsvd "times_roman"
             return Times_Roman
      <|> do p_rsvd "helvetica"
             return Helvetica



p_content' :: Parser (Cont String)
p_content' = do p_op "image"
                img <- p_braces p_free_text
                return $ Cont_image img
          <|> do p_op "body"
                 algn <- p_braces p_alignment
                 return $ Cont_body algn
          <|> do p_op "float"
                 algn <- p_braces p_alignment
                 txt <- p_braces p_free_text
                 return $ Cont_float_txt algn txt
          <|> do p_rsvd "empty"
                 return Cont_empty
    where p_alignment = do try $ string "justify"
                           return Justified
                      <|> do try $ string "flush_left"
                             return FlushedLeft
                      <|> do string "flush_right"
                             return FlushedRight



p_dimension :: Parser Dimension
p_dimension = do x <- p_exp p_length
                 p_comma
                 y <- p_exp p_length
                 return (x,y)



p_exp :: Parser a -> Parser (Exp a)
p_exp parse_value = do v <- try parse_value
                       return (Value v)
                    <|> do char '$'
                           ident <- p_identf
                           return (Var ident)



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
p_stmt' = do p_op "def"
             v <- p_braces p_identf
             params <- p_params
             e <- p_braces p_expression
             return $ Def v params e
       <|> do p_op "vrt"
              line_set Vert
       <|> do p_op "hrz"
              line_set Horz
       <|> do p_op "set_ppi"
              i <- p_braces p_int
              return $ PPI i
       <|> do p_op "inner_frame"
              framing In_frame
       <|> do p_op "outer_frame"
              framing Out_frame
       <|> do p_op "set_content"
              g <- p_braces (p_exp p_grid)
              c <- p_braces (p_exp p_content)
              return $ Set_cont g c
       <|> do p_op "set_grid"
              p <- p_braces p_page
              g <- p_braces (p_exp p_grid)
              return $ Set_grid p g
       <|> do p_op "page_default"
              s <- p_braces (p_exp p_dimension)
              x <- p_braces (p_exp p_length)
              y <- p_braces (p_exp p_length)
              return $ Page_dflt s x y
       <|> do try (p_op "new_page" >> notFollowedBy (char '{'))
              return $ Newpage_dflt
       <|> do p_op "new_page"
              size <- p_braces (p_exp p_dimension)
              margns <- p_braces (p_exp p_dimension)
              return $ Newpage size margns
       <|> do p_op "grid_default"
              g <- p_braces (p_exp p_grid)
              return $ Grid_dflt g
       <|> do p_op "text_default"
              f <- p_braces p_font
              s <- p_braces p_int
              return $ Text_dflt f s
       <|> do p_op "clean"
              g <- p_braces (p_exp p_grid)
              return $ Clean g
       <|> do p_op "line_space"
              i <- p_braces (p_exp p_int)
              return $ Text_line_space i
       <|> do p_op "include"
              file <- p_braces p_free_text
              return $ Include file
       <|> do p_op "debug"
              return Debug
    where line_set st = do g <- p_braces (p_exp p_grid)
                           i <- p_braces p_int
                           p <- p_braces (p_exp p_length)
                           return $ st g i p
          framing st = do g <- p_braces (p_exp p_grid)
                          edgs <- p_braces (p_commaSep p_edge)
                          return $ st g (concat edgs)
          p_edge = p_symbol "u" <|> p_symbol "d" <|> p_symbol "l" <|> p_symbol "r"
          p_page = (try p_int >>= \x -> return (Just x))
                  <|> (p_rsvd "last_page" >> return Nothing)
          p_params = (try $ p_brackets (p_commaSep p_identf))
                    <|> return []



p_stmt = char '\\' >> p_stmt'
p_modifier = char '\\' >> p_modifier'
p_content = char '\\' >> p_content'
p_grid = char '\\' >> p_grid'



p_stmt_op :: Parser Stmt
p_stmt_op = do char '\\'
               op <- p_identf
               args <- p_braces (p_commaSep p_arg)
               return $ Op op args
    where p_arg = do try (char '$')
                     v <- p_identf
                     return $ Var v
                <|> do e <- p_braces p_expression
                       return $ Value e



-- No uso lexemes para no eliminar espacios del texto
p_escape :: Parser Stmt
p_escape = do char '\\'
              s <- (oneOf "\\{}$")
              return $ Text_value [s]



p_doc :: Parser Doc
p_doc = do lookAhead $ oneOf "}$"
           return []
    <|> do p <- prs
           ps <- p_doc
           return $ conc p ps
    <|> do eof
           return []
  where prs = try p_modifier
            <|> do st <- try p_stmt
                   return [st]
            <|> do esc <- try p_escape
                   return [esc]
            <|> do op <- try p_stmt_op
                   return [op]
            <|> do t <- noneOf "\\{}$"
                   return [Text_value [t]]
        conc [Text_value x] ((Text_value y):ys) = (Text_value (x++y)) : ys
        conc xs ys = xs++ys




------------------------------------
-- Función de parseo
------------------------------------

totParser :: Parser a -> Parser a
totParser p = do p_whiteSpace
                 t <- p
                 eof
                 return t


parseDoc :: SourceName -> String -> Either ParseError Doc
parseDoc = parse (totParser p_doc)


parser :: String -> Doc
parser s = either (error. show) id (parseDoc "" s)

