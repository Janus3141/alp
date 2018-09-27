
import Control.Monad.State
import Control.Monad.Except
import AST
import Types
import Graphics.PDF





-- Se representa una grilla como un conjunto de columnas de rectangulos.
data Grid = Grid [[Either (Rect String) Grid]] | GridVar Variable
            deriving Show


-- Un documento se compone de varias piezas o secciones. Para cada seccion
-- primero se utilizan las paginas de la lista dada, y se termina con
-- paginas predeterminadas hasta agotar el texto.
type DocPiece = ([Page Grid], Text)

type GridDoc = [DocPiece]



---------------------------------
--- Tipos y manejo de entorno ---
---------------------------------

data EvalVType = EVLength Integer
               | EVDim (Integer,Integer)
               | EVGrid Grid
               | EVCont (Cont String Text)
               | EVInt Integer
               | EVDoc Doc


type VarEnv = [(Variable, Either ([Variable],VType) EvalVType)]


data Env = Env { ppi :: Integer
               , font_def :: PDFFont
               , page_def :: Page Grid
               , doc :: GridDoc
               , var :: VarEnv
               , debug :: Bool
               , file :: FilePath }


initRect :: Rect
initRect = Rect ((0 :+ 0),(100 :+ 100)) (10,10) (edgs,edgs) (Cont_body Justified) Nothing
    where edgs = (False, False, False, False)

initState :: Env
initState = Env { ppi = 72
                , font_def = PDFFont Courier 12
                , page_def = Page (100,100) (Grid [[Left initRect]])
                , doc = []
                , var = []
                , debug = False
                , file = ""}





updateVar :: (Variable, [Variable], EvalVType) -> Env -> Env
updateVar v env = env { var = updateVar' (var env)}
    where updateVar [] = [v]
          updateVar (e':xs) | fst' v == fst' e'  =  v:xs
                            | otherwise          =  e':(updateVar xs)
          fst' (x,_,_) = x






eval :: Env -> Doc -> IO (Either IOError (Doc,Env))
eval e d = do rem <- return $ evalDoc e d
              res <- either return test rem
              either return continue res
    where test (d',e') = case d' of
                          []               -> return $ Right (d',e')
                          ((Include f):xs) -> incldHandler e' f
                          _                -> return $ Left algunError
          incldHandler env f = do h <- openFile f ReadMode
                                  cont <- hGetContents h
                                  doc <- parse cont
                                  rem' <- eval (env {file = f}) doc
          continue (d',e') = eval (e' {file = file e}) d'


evalDoc :: Env -> Doc -> Either IOError (Doc,Env)
evalDoc e d = runStateT (evalDoc' d) e


evalDoc :: (MonadError err m, MonadState env m) => Doc -> m Doc
evalDoc [] = return []
evalDoc (x:xs) = case x of
    Def v vs e        -> modify (updateVar (v,vs,e))
    StmtVar v         ->
    StmtOp v es       ->
    PPI n             ->
    Vert g i l        ->
    Horz g i l        ->
    Clean g           ->
    In_frame g b      ->
    Out_frame g b     ->
    Set_cont g c      ->
    Set_grid p g      ->
    Page_dflt d g     ->
    Newpage_dflt      ->
    Newpage d g       ->
    Text_dflt n s     ->
    Text_bold         ->
    Text_bold_off     ->
    Text_italics      ->
    Text_ital_off     ->
    Text_resize s     ->
    Text_size_normal  ->
    Text_font n       ->
    Text_font_normal  ->
    Text_return       ->
    Text_line_space s ->
    Text_value v      ->
    Debug             ->
    _                 ->



evalVType :: (MonadError err m, MonadState env m) => VType -> m EvalVType
evalVType (VLength len) = EVLength $ evalLength len
evalVType (VDim (x,y))  = EVDim (evalLength x, evalLength y)
evalVType (VGrid gr)    = EVGrid $ evalGrid gr
evalVType (VCont cont)  = EVCont $ evalCont cont
evalVType (VInt i)      = EVInt i
evalVType (VDoc d)      = EVDoc d


evalExp :: (MonadError err m, MonadState env m) => (a -> EvalVType) -> Exp b -> m c
evalExp cons (Var v) = do val <- lookForVar v
                          case val of
                            cons i -> return i
                            _      -> throwError algunError
evalExp cons (Op op args) = do vvt <- lookForOp op
                               



evalLength :: (MonadError err m, MonadState env m) => Length -> m Integer
evalLength (Pixel i) = i
evalLength (CM i)    = gets ppi >>= \p -> round (p * i * 2.54)
evalLength (Inch i)  = gets ppi >>= \p -> p * i


evalGrid :: (MonadError err m, MonadState env m) => GridTerm -> m Grid


evalCont :: (MonadError err m, MonadState env m) => Cont String String -> m (Cont String Text)
evalCont (Cont_float_text algnt t) = Cont_float_text algnt $ map TextT (lines t)
evalCont x = x


lookForVar :: (MonadError err m, MonadState env m) => Variable -> m EvalVType
lookForVar v = do varEnv <- gets var
                  lookFor' varEnv
    where lookFor' []          = throwError algunError
          lookFor' ((v',e):xs) | v == v'   = either (throwError algunError) return e
                               | otherwise = lookFor' xs


lookForOp :: (MonadError err m, MonadState env m) => Variable -> m ([Variable],VType)
lookForOp v = do varEnv <- gets var
                 lookFor' varEnv
    where lookFor' []          = throwError algunError
          lookFor' ((v',e):xs) | v == v'   = either return (throwError algunError) e
                               | otherwise = lookFor' xs

