
{-# LANGUAGE FlexibleContexts #-}


module Eval (eval, EvalDoc, EvalCont) where



import Control.Monad.State
import Control.Monad.Except
import Text.Parsec.Error (ParseError)
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

data Error = UndefVar Variable FilePath
           | UnknownStmt Stmt FilePath
           | ExpectedEType String EvalVType FilePath
           | ExpectedVType String VType FilePath
           | ExpectedVar String FilePath
           | ExpectedOp String FilePath
           | ParserErr ParseError


instance Show Error where
    show (UndefVar var f)        = show f ++ ": Variable " ++ var ++ " no definida."
    show (UnknownStmt stmt f)    = show f ++ ": Sentencia " ++
                                   show stmt ++ " desconocida."
    show (ExpectedEType s evt f) = show f ++ ": Se esperaba " ++ show s ++
                                   ", pero se encontro " ++ show evt
    show (ExpectedVType s vt f)  = show f ++ ": Se esperaba " ++ show s ++
                                   ", pero se encontro " ++ show vt
    show (ExpectedVar v f)       = show f ++ ": Se esperaba una variable," ++
                                   " pero " ++ v ++ " es una operacion"
    show (ExpectedOp v f)        = show f ++ ": Se esperaba una operacion," ++
                                   " pero " ++ v ++ " es una variable"
    show (ParserErr err)          = show err



throwErrorFile :: (MonadError Error m, MonadState Env m) => (FilePath -> Error) -> m a
throwErrorFile fun = gets file >>= throwError . fun






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


instance Show EvalVType where
    show (EVLength _)  = "\"longitud\""
    show (EVLenPair _) = "\"longitudes\""
    show (EVRect _)    = "\"rectangulo\""
    show (EVCont _)    = "\"contenido\""
    show (EVInt _)     = "\"entero\""
    show (EVDoc _)     = "\"sentencias/texto\""



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
addPg page env = let PDFFont n s = head (used_fonts env)
                 in env { doc = (doc env) ++ [([page], [TextFont n, TextSize s])] }


addText :: Text -> Env -> Env
addText txt env = env { doc = docAdd (doc env) }
    where docAdd [] = [([], txt)]
          docAdd xs = init xs ++ [pieceAdd $ last xs]
          pieceAdd (ps,[]) = (ps, txt)
          pieceAdd (ps,ys) = (ps, ys ++ txt)






------------------------------------------------------------------------------
--------------------------------- Evaluacion ---------------------------------
------------------------------------------------------------------------------


-- eval devuelve solo la pagina a usar por defecto y el documento (paginas
-- manuales y texto), o un error. Ademas agrega la fuente predeterminada al
-- principio del texto, lo cual es requerido por drawPDF en Run.hs

eval :: FilePath -> Doc -> IO (Either Error (Page EvalCont, EvalDoc))
eval fl d = do ev <- eval' (initState {file = fl}) d
               return $ either Left (Right . extract . snd) ev
    where extract e = (page_def e, addFont (font_def e) (doc e))
          addFont _ [] = []
          addFont f ((ps,txt):xs) = let PDFFont n s = f
                                    in (ps,[TextFont n, TextSize s]++txt) : xs


-- El conjunto de Stmt se evalua hasta que termina o hasta que se encuentra un
-- Include. En ese caso se vuelve a eval, donde se maneja el Include, y se sigue
-- con el archivo original.

eval' :: Env -> Doc -> IO (Either Error (Doc,Env))
eval' e [] = return $ Right ([], e)
eval' e d = do rem <- return $ evalDoc e d
               res <- either (return . Left) test rem
               either (return . Left) continue res
    where test (d',e') = case d' of
                          []               -> return $ Right (d',e')
                          ((Include f):xs) -> do let fix = Right . ((,) xs)
                                                 res <- incldHandler e' f
                                                 return $ either Left fix res
                          (x:_)            -> return $ Left (UnknownStmt x (file e))
          continue (d',e') = eval' (e' {file = file e}) d'



-- Cuando se recibe un Include y se parsea el archivo incluido, si hay
-- algun error de parseo, se devuelve un ParseError sobre Error, de lo
-- contrario se lo evalua. En caso de una evaluacion correcta, eval'
-- devolvera Right ([],e), por lo tanto se omite Doc.

incldHandler :: Env -> FilePath -> IO (Either Error Env)
incldHandler env f = do cont <- readFile f
                        cont' <- return $ parseDoc cont
                        either failure evaluate cont'
    where evaluate d = do result <- eval' (env {file = f}) d
                          return $ either Left (Right . snd) result
          failure = return . Left . ParserErr



evalDoc :: Env -> Doc -> Either Error (Doc,Env)
evalDoc e d = runStateT (evalDoc' d) e


evalDoc' :: (MonadError Error m, MonadState Env m) => Doc -> m Doc
evalDoc' [] = return []
evalDoc' d@((Include _):_) = return d
evalDoc' (x:xs) = evalStmt x >> evalDoc' xs



evalStmt :: (MonadError Error m, MonadState Env m) => Stmt -> m ()
evalStmt (Def v vs e)       = case vs of
                                [] -> do exp <- evalVType e
                                         modify $ updateVar (v, Right exp)
                                _  -> modify $ updateVar (v, Left (vs,e))
evalStmt (StmtVar v)        = do
        ev <- lookForVar v
        case ev of
            EVDoc d -> evalDoc' d >> return ()
            _       -> throwErrorFile (ExpectedEType "sentencias o texto" ev)
evalStmt (StmtOp op args)   = do
        vvt <- lookForOp op
        case snd vvt of
            VDoc _ -> execDoc vvt args
            _      -> throwErrorFile (ExpectedVType "sentencias o texto" $ snd vvt)
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
                                 s' <- liftM fromIntegral $ evalExpInt s
                                 cfs <- gets used_fonts
                                 modify $ updateFonts (PDFFont (name cfs) s')
                                 modify $ addText [TextSize s']
evalStmt (Text_size_normal) = do modify removeFont
                                 cfs <- gets used_fonts
                                 modify $ addText [TextSize $ fontSize $ head cfs]
evalStmt (Text_return)      = modify $ addText [TextNewLine]
evalStmt (Text_line_space s) = do
        s' <- liftM fromIntegral $ evalExpInt s
        modify $ addText [TextLnSpace s']
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
                               exec vvt args
evalExpVType (Value v)    = evalVType v



evalVType :: (MonadError Error m, MonadState Env m) => VType -> m EvalVType
evalVType (VLength len)     = liftM EVLength $ evalLength len
evalVType (VLenPair (x,y))  = do x' <- evalExpLength x
                                 y' <- evalExpLength y
                                 return $ EVLenPair (x', y')
evalVType (VRect r)         = liftM EVRect $ evalRect r
evalVType (VCont cont)      = liftM EVCont $ evalCont cont
evalVType (VInt i)          = return $ EVInt i
evalVType (VDoc d)          = return $ EVDoc d





exec :: (MonadError Error m, MonadState Env m) => ([Variable],VType) -> [Exp VType] -> m EvalVType
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
          _       -> throwErrorFile (ExpectedEType "entero" val)
evalExpInt (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VInt _ -> do EVInt i <- exec vvt args
                        return i
           _      -> throwErrorFile (ExpectedVType "entero" $ snd vvt)
evalExpInt (Value x) = return x





evalExpLPair :: (MonadError Error m, MonadState Env m) => Exp LenPair -> m (Integer,Integer)
evalExpLPair (Var v) = do
        val <- lookForVar v
        case val of
          EVLenPair p -> return p
          _           -> throwErrorFile (ExpectedEType "longitudes" val)
evalExpLPair (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VLenPair _ -> do EVLenPair p <- exec vvt args
                            return p
           _          -> throwErrorFile (ExpectedVType "longitudes" $ snd vvt)
evalExpLPair (Value (x,y)) = do x' <- evalExpLength x
                                y' <- evalExpLength y
                                return (x',y')





evalExpLength :: (MonadError Error m, MonadState Env m) => Exp Length -> m Integer
evalExpLength (Var v) = do
        val <- lookForVar v
        case val of
           EVLength l -> return l
           _          -> throwErrorFile (ExpectedEType "longitud" val)
evalExpLength (Value v) = evalLength v



evalLength :: (MonadError Error m, MonadState Env m) => Length -> m Integer
evalLength (Pixel i) = return i
evalLength (MM i)    = do p <- gets ppi
                          return $ round ((fromIntegral p) * (fromIntegral i) * 0.0394)
evalLength (Inch i)  = do p <- gets ppi 
                          return $ p * i





evalExpRect :: (MonadError Error m, MonadState Env m) => Exp ParserRect -> m EvalRect
evalExpRect (Var v) = do
        val <- lookForVar v
        case val of
           EVRect r -> maybeDebug v r
           _        -> throwErrorFile (ExpectedEType "rectangulo" val)
evalExpRect (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VRect _ -> do EVRect r <- exec vvt args
                         maybeDebug ("\\" ++ op) r
           _       -> throwErrorFile (ExpectedVType "rectangulo" $ snd vvt)
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
           _        -> throwErrorFile (ExpectedEType "contenido" val)
evalExpCont (Op op args) = do
        vvt <- lookForOp op
        case snd vvt of
           VCont _ -> do EVCont c <- exec vvt args
                         return c
           _       -> throwErrorFile (ExpectedVType "contenido" $ snd vvt)
evalExpCont (Value v) = evalCont v



evalCont :: (MonadError Error m, MonadState Env m) => ParserCont -> m EvalCont
evalCont (Cont_image x) = return $ Cont_image x
evalCont (Cont_body a) = return $ Cont_body a
evalCont (Cont_float_txt algnt t) = do
        PDFFont nm sz <- gets font_def
        txt <- return $ [TextFont nm, TextSize sz] ++ map TextT (words t)
        return $ Cont_float_txt algnt txt
evalCont (Cont_empty) = return Cont_empty





lookForVar :: (MonadError Error m, MonadState Env m) => Variable -> m EvalVType
lookForVar v = do varEnv <- gets var
                  lookFor' varEnv
    where lookFor' []          = throwErrorFile (UndefVar v)
          lookFor' ((v',e):xs) | v == v'   = either error return e
                               | otherwise = lookFor' xs
          error = \_ -> throwErrorFile (ExpectedVar v)



lookForOp :: (MonadError Error m, MonadState Env m) => Variable -> m ([Variable],VType)
lookForOp v = do varEnv <- gets var
                 lookFor' varEnv
    where lookFor' []          = throwErrorFile (UndefVar v)
          lookFor' ((v',e):xs) | v == v'   = either return error e
                               | otherwise = lookFor' xs
          error = \_ -> throwErrorFile (ExpectedOp v)


