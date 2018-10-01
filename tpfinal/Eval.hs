
{-# LANGUAGE FlexibleContexts #-}


import Control.Monad.State
import Control.Monad.Except
import AST
import Types
import Graphics.PDF
import Parser

------------------------------------------------------------------------------
--------------------------------- Utilidades ---------------------------------
------------------------------------------------------------------------------

toEdges :: String -> Edges
toEdges edges = toEdges' hid edges
    where toEdges' es [] = es
          toEdges' (a,b,c,d) ('u':xs) = toEdges' (True,b,c,d) xs
          toEdges' (a,b,c,d) ('d':xs) = toEdges' (a,True,c,d) xs
          toEdges' (a,b,c,d) ('l':xs) = toEdges' (a,b,True,d) xs
          toEdges' (a,b,c,d) ('r':xs) = toEdges' (a,b,c,True) xs


fontName :: PDFFont -> FontName
fontName (PDFFont n _) = n

fontSize :: PDFFont -> Int
fontSize (PDFFont _ s) = s


toBold :: FontName -> FontName
toBold Helvetica         = Helvetica_Bold
toBold Helvetica_Oblique = Helvetica_BoldOblique
toBold Times_Roman       = Times_Bold
toBold Times_Italic      = Times_BoldItalic
toBold Courier           = Courier_Bold
toBold Courier_Oblique   = Courier_BoldOblique
toBold fn                = fn


toItalics :: FontName -> FontName
toItalics Helvetica      = Helvetica_Oblique
toItalics Helvetica_Bold = Helvetica_BoldOblique
toItalics Times_Roman    = Times_Italic
toItalics Times_Bold     = Times_BoldItalic
toItalics Courier        = Courier_Oblique
toItalics Courier_Bold   = Courier_BoldOblique
toItalics fn             = fn


type EvalCont = Cont String Text
type EvalRect = Rect EvalCont






------------------------------------------------------------------------------
---------------------------------- Errores -----------------------------------
------------------------------------------------------------------------------

data Error = UndefVar Variable
           | UnknownStmt Stmt
           | ExpectedEType String EvalVType
           | ExpectedVType String VType
           | ExpectedVar String
           | ExpectedOp String






------------------------------------------------------------------------------
------------------------------ Tipos de entorno ------------------------------
------------------------------------------------------------------------------

-- Tipos que puede tomar una variable en tiempo de evaluacion

data EvalVType = EVLength Integer
               | EVLenPair (Integer,Integer)
               | EVRect EvalRect
               | EVCont EvalCont
               | EVInt Integer
               | EVDoc Doc


toEV :: VType -> b -> EvalVType
toEV (VLength _)  = EVLength
toEV (VLenPair _) = EVLenPair
toEV (VRect _)    = EVRect
toEV (VCont _)    = EVCont
toEV (VInt _)     = EVInt
toEV (VDoc _)     = EVDoc



-- Entorno de variables. Se utiliza Left para variables que actuan como
-- operaciones (primero los nombres de los argumentos, luego la definicion
-- de la operacion), y Right para variables comunes.
type VarEnv = [(Variable, Either ([Variable],VType) EvalVType)]


-- Un documento se compone de varias secciones. Para cada seccion se utilizan
-- primero las paginas dadas manualmente y se termina de escribir el texto
-- en paginas automaticas, con la forma de la pagina por defecto.
type EvalDoc = [PDFPiece EvalCont]


-- El entorno a utilizar en el evaluador
data Env = Env { ppi :: Integer
               , font_def :: PDFFont
               , used_fonts :: [PDFFont]
               , page_def :: Page EvalCont
               , doc :: EvalDoc
               , var :: VarEnv
               , debug :: Bool
               , file :: FilePath }


initRect :: EvalRect
initRect = Rect ((0 :+ 0),(100 :+ 100)) (5,5) (hid,hid) (Cont_body Justified) Nothing

initState :: Env
initState = Env { ppi = 72
                , font_def = PDFFont Courier 12
                , used_fonts = [PDFFont Courier 12]
                , page_def = Page (100,100) [initRect]
                , doc = []
                , var = []
                , debug = False
                , file = "" }






------------------------------------------------------------------------------
--------------------- Funciones para control de entorno ----------------------
------------------------------------------------------------------------------

setDebug :: Env -> Env
setDebug env = env { debug = True }


updatePpi :: Integer -> Env -> Env
updatePpi n env = env { ppi = n }


updateDefPage :: Page EvalCont -> Env -> Env
updateDefPage page env = env { page_def = page }


updateDefFont :: PDFFont -> Env -> Env
updateDefFont f env = env { font_def = f
                          , used_fonts = init (used_fonts env) ++ [f] }


updateFonts :: PDFFont -> Env -> Env
updateFonts f env = env { used_fonts = f:(used_fonts env) }


removeFont :: Env -> Env
removeFont env = env { used_fonts = tail (used_fonts env) }


updateVar :: (Variable, Either ([Variable],VType) EvalVType) -> Env -> Env
updateVar v env = env { var = updateVar' (var env)}
    where updateVar' [] = [v]
          updateVar' (e:xs) | fst v == fst e  =  v:xs
                            | otherwise       =  e:(updateVar' xs)


recoverVar :: VarEnv -> Env -> Env
recoverVar ve env = env {var = ve}


-- Agrega los rectangulos rs a la ultima pagina creada manualmente en la ultima seccion.
-- Si todavia no se crearon paginas manuales, se crea una con el tamaño por defecto y los
-- rectangulos dados.
addRects :: [EvalRect] -> Env -> Env
addRects rs env = env { doc = docAdd (doc env) }
    where docAdd [] = [([Page defSize rs], [])]
          docAdd xs = init xs ++ [pieceAdd $ last xs]
          pieceAdd ([],txt) = ([Page defSize rs], txt)
          pieceAdd (ys,txt) = let Page size rs' = last ys
                               in (init ys ++ [Page size (rs'++rs)], txt)
          Page defSize _ = page_def env


addPg :: Page EvalCont -> Env -> Env
addPg page env = env { doc = (doc env) ++ [([page], [])] }


addText :: Text -> Env -> Env
addText txt env = env { doc = docAdd (doc env) }
    where docAdd [] = [([], txt)]
          docAdd xs = init xs ++ [pieceAdd $ last xs]
          pieceAdd (ps,[]) = (ps, txt)
          pieceAdd (ps,ys) = (ps, ys ++ txt)





------------------------------------------------------------------------------
--------------------------------- Evaluacion ---------------------------------
------------------------------------------------------------------------------


-- El conjunto de Stmt se evalua hasta que termina o hasta que se encuentra un
-- Include. En ese caso se vuelve a eval, donde se abre el archivo a incluir,
-- se evalua, y se sigue con el archivo original.

eval :: Env -> Doc -> IO (Either Error (Doc,Env))
eval e [] = return $ Right ([], e)
eval e d = do rem <- return $ evalDoc e d
              res <- either (return . Left) test rem
              either (return . Left) continue res
    where test (d',e') = case d' of
                          []               -> return $ Right (d',e')
                          ((Include f):xs) -> incldHandler e' f
                          (x:_)            -> return $ Left (UnknownStmt x)
          incldHandler env f = do cont <- readFile f
                                  eval (env {file = f}) (parser cont)
          continue (d',e') = eval (e' {file = file e}) d'




evalDoc :: Env -> Doc -> Either Error (Doc,Env)
evalDoc e d = runStateT (evalDoc' d) e


evalDoc' :: (MonadError Error m, MonadState Env m) => Doc -> m Doc
evalDoc' [] = return []
evalDoc' d@((Include _):_) = return d
evalDoc' (x:xs) = evalStmt x >> evalDoc' xs



evalStmt :: (MonadError Error m, MonadState Env m) => Stmt -> m ()
evalStmt (Def v vs e)       = case vs of
                                [] -> do exp <- liftM (toEV e) $ evalVType e
                                         modify $ updateVar (v, Right exp)
                                _  -> modify $ updateVar (v, Left (vs,e))
evalStmt (StmtVar v)        = do
        ev <- lookForVar v
        case ev of
            EVDoc d -> evalDoc' d >> return ()
            _       -> throwError (ExpectedEType "Document" ev)
evalStmt (StmtOp op args)   = do
        vvt <- lookForOp op
        case snd vvt of
            VDoc _ -> execDoc vvt args
            _      -> throwError (ExpectedVType "Document" $ snd vvt)
evalStmt (PPI n)            = modify $ updatePpi n
evalStmt (Add_rects rs)     = do rs' <- mapM evalExpRect rs
                                 modify $ addRects rs'
evalStmt (Page_dflt lp rs)  = do size <- evalExpLPair lp
                                 rs' <- mapM evalExpRect rs
                                 modify $ updateDefPage (Page size rs')
evalStmt (Newpage_dflt)     = do dflt <- gets page_def
                                 modify $ addPg dflt
evalStmt (Newpage lp rs)    = do size <- evalExpLPair lp
                                 rs' <- mapM evalExpRect rs
                                 modify $ addPg (Page size rs')
evalStmt (Text_dflt n s)    = modify $ updateDefFont (PDFFont n $ fromIntegral s)
evalStmt (Text_bold)        = transFont toBold
evalStmt (Text_bold_off)    = fontStepBack
evalStmt (Text_italics)     = transFont toItalics
evalStmt (Text_ital_off)    = fontStepBack
evalStmt (Text_font n)      = transFont (\_ -> n)
evalStmt (Text_font_normal) = fontStepBack
evalStmt (Text_resize s)    = do let name = fontName . head
                                     s' = fromIntegral s
                                 cfs <- gets used_fonts
                                 modify $ updateFonts (PDFFont (name cfs) s')
                                 modify $ addText [TextSize s']
evalStmt (Text_size_normal) = do modify removeFont
                                 cfs <- gets used_fonts
                                 modify $ addText [TextSize $ fontSize $ head cfs]
evalStmt (Text_return)      = modify $ addText [TextNewLine]
evalStmt (Text_line_space s) = do
        s' <- evalExpInt
        modify $ addText [TextLnSpace $ fromIntegral s]
evalStmt (Text_value v)     = modify $ addText (map TextT $ words v)
evalStmt (Debug)            = modify setDebug


-- Vuelve a la fuente usada antes de hacer una modificacion.
fontStepBack :: (MonadError Error m, MonadState Env m) => m ()
fontStepBack = do modify removeFont
                  cfs <- gets used_fonts
                  modify $ addText [TextFont $ fontName $ head cfs]


-- Modifica el nombre de la fuente segun f, y guarda la nueva fuente en el entorno.
transFont :: (MonadError Error m, MonadState Env m) => (FontName -> FontName) -> m ()
transFont f = do let f' = f . fontName . head
                     size = fontSize . head
                 cfs <- gets used_fonts
                 modify $ updateFonts (PDFFont (f' cfs) (size cfs))
                 modify $ addText [TextFont (f' cfs)]


-- Ejecuta una operacion Stmt. Si en la operacion aparece Include, se para la ejecucion
-- de la operacion, pero se sigue con el resto del documento.
execDoc :: (MonadError Error m, MonadState Env m) => ([Variable],VType) -> [Exp VType] -> m ()
execDoc (vars, (VDoc vt)) args = do
        oldVarState <- gets var
        evalArgs <- mapM evalExpVType args
        mapM (modify . updateVar) (rightZip vars evalArgs)
        evalDoc' vt
        modify (recoverVar oldVarState)
    where rightZip = zipWith (\x y -> (x, Right y))





evalExpVType :: (MonadError Error m, MonadState Env m) => Exp VType -> m EvalVType
evalExpVType (Var v)      = lookForVar v
evalExpVType (Op op args) = do vvt <- lookForOp op
                               liftM (toEV (snd vvt)) $ exec vvt args
evalExpVType (Value v)    = liftM (toEV v) $ evalVType v



evalVType :: (MonadError err m, MonadState Env m) => VType -> m a
evalVType (VLength len)     = evalLength len
evalVType (VLenPair (x,y))  = do x' <- evalExpLength x
                                 y' <- evalExpLength y
                                 return (x', y')
evalVType (VRect r)         = evalRect r
evalVType (VCont cont)      = evalCont cont
evalVType (VInt i)          = return i
evalVType (VDoc d)          = return d





exec :: (MonadError Error m, MonadState Env m) => ([Variable],VType) -> [Exp VType] -> m a
exec (vars,vt) args = do
        oldVarState <- gets var                            -- Guardar estado de var
        evalArgs <- mapM evalExpVType args                 -- Evaluar argumentos
        mapM (modify . updateVar) (rightZip vars evalArgs) -- Cargarlos a entorno
        value <- evalVType vt                              -- Evaluar la operacion
        modify (recoverVar oldVarState)                    -- Recuperar variables
        return value
    where rightZip = zipWith (\x y -> (x, Right y))





evalExpInt :: (MonadError Error m, MonadState Env m) => Exp Integer -> m Integer
evalExpInt (Var v) = do
        val <- lookForVar v
        case val of
          EVInt p -> return p
          _       -> throwError (ExpectedEType "Integer" val)
evalExpInt (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VInt _ -> exec vvt args
           _      -> throwError (ExpectedVType "Integer" $ snd vvt)
evalExpInt (Value x) = return x





evalExpLPair :: (MonadError Error m, MonadState Env m) => Exp LenPair -> m (Integer,Integer)
evalExpLPair (Var v) = do
        val <- lookForVar v
        case val of
          EVLenPair p -> return p
          _           -> throwError (ExpectedEType "Length pair" val)
evalExpLPair (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VLenPair _ -> exec vvt args
           _          -> throwError (ExpectedVType "Length pair" $ snd vvt)
evalExpLPair (Value (x,y)) = do x' <- evalExpLength x
                                y' <- evalExpLength y
                                return (x',y')






evalExpLength :: (MonadError Error m, MonadState Env m) => Exp Length -> m Integer
evalExpLength (Var v) = do
        val <- lookForVar v
        case val of
           EVLength l -> return l
           _          -> throwError (ExpectedEType "Length" val)
evalExpLength (Value v) = evalLength v



evalLength :: (MonadError Error m, MonadState Env m) => Length -> m Integer
evalLength (Pixel i) = return i
evalLength (CM i)    = gets ppi >>= \p -> return $ round (p * i * 2.54)
evalLength (Inch i)  = gets ppi >>= \p -> return $ p * i





evalExpRect :: (MonadError Error m, MonadState Env m) => Exp ParserRect -> m EvalRect
evalExpRect (Var v) = do
        val <- lookForVar v
        case val of
           EVRect r -> maybeDebug v r
           _        -> throwError (ExpectedEType "Rectangle" val)
evalExpRect (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VRect _ -> do r <- exec vvt args
                         maybeDebug ("\\" ++ op) r
           _       -> throwError (ExpectedVType "Rectangle" $ snd vvt)
evalExpRect (Value v) = evalRect v


maybeDebug :: (MonadError Error m, MonadState Env m) => String -> EvalRect -> m EvalRect
maybeDebug msg (Rect p m e c _) = do
        d <- gets debug
        if d then return (Rect p m e c (Just msg))
             else return (Rect p m e c Nothing)


evalRect :: (MonadError Error m, MonadState Env m) => ParserRect -> m EvalRect
evalRect (ParserRect (p1,p2) mrgns) = do
        let toPoint (x,y) = (fromIntegral x) :+ (fromIntegral y)
        p1' <- liftM toPoint $ evalExpLPair p1
        p2' <- liftM toPoint $ evalExpLPair p2
        mrgns' <- evalExpLPair mrgns
        return $ Rect (p1',p2') mrgns' (hid,hid) (Cont_body Justified) Nothing
evalRect (Clean r) = do (Rect p m e _ d) <- evalExpRect r
                        return $ Rect p m e Cont_empty d
evalRect (In_frame r e) = do (Rect p m (ext,_) c d) <- evalExpRect r
                             return $ Rect p m (ext, toEdges e) c d
evalRect (Out_frame r e) = do (Rect p m (_,int) c d) <- evalExpRect r
                              return $ Rect p m (toEdges e, int) c d
evalRect (Rect_set r c) = do (Rect p m e _ d) <- evalExpRect r
                             c' <- evalExpCont c
                             return $ Rect p m e c' d





evalExpCont :: (MonadError Error m, MonadState Env m) => Exp ParserCont -> m EvalCont
evalExpCont (Var v) = do
        val <- lookForVar v
        case val of
           EVCont c -> return c
           _        -> throwError (ExpectedEType "Content" val)
evalExpCont (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VCont _ -> exec vvt args
           _       -> throwError (ExpectedVType "Contenido" $ snd vvt)
evalExpCont (Value v) = evalCont v



evalCont :: (MonadError Error m, MonadState Env m) => ParserCont -> m EvalCont
evalCont (Cont_image x) = return $ Cont_image x
evalCont (Cont_body a) = return $ Cont_body a
evalCont (Cont_float_txt algnt t) = return $ Cont_float_txt algnt (map TextT (words t))
evalCont (Cont_empty) = return Cont_empty





lookForVar :: (MonadError Error m, MonadState Env m) => Variable -> m EvalVType
lookForVar v = do varEnv <- gets var
                  lookFor' varEnv
    where lookFor' []          = throwError (UndefVar v)
          lookFor' ((v',e):xs) | v == v'   = either error return e
                               | otherwise = lookFor' xs
          error = \_ -> throwError (ExpectedVar v)



lookForOp :: (MonadError Error m, MonadState Env m) => Variable -> m ([Variable],VType)
lookForOp v = do varEnv <- gets var
                 lookFor' varEnv
    where lookFor' []          = throwError (UndefVar v)
          lookFor' ((v',e):xs) | v == v'   = either return error e
                               | otherwise = lookFor' xs
          error = \_ -> throwError (ExpectedOp v)


