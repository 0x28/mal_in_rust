module Core
( ns )
where

import Control.Exception (catch)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Readline (readline)
import Reader (read_str)
import Types
import Printer (_pr_str, _pr_list)

-- General functions

equal_Q [a, b] = return $ if a == b then MalTrue else MalFalse
equal_Q _ = error $ "illegal arguments to ="

run_1 :: (MalVal -> MalVal) -> [MalVal] -> IO MalVal
run_1 f (x:[]) = return $ f x
run_1 _ _ = error $ "function takes a single argument"

run_2 :: (MalVal -> MalVal -> MalVal) -> [MalVal] -> IO MalVal
run_2 f (x:y:[]) = return $ f x y
run_2 _ _ = error $ "function takes a two arguments"

-- Scalar functions

symbol (MalString str) = MalSymbol str
symbol _ = error $ "symbol called with non-string"

keyword (MalString str) = MalString $ "\x029e" ++ str
keyword _ = error $ "keyword called with non-string"


-- String functions

pr_str args = do
    return $ MalString $ _pr_list True " " args

str args = do
    return $ MalString $ _pr_list False "" args

prn args = do
    putStrLn $ _pr_list True " " args
    return Nil

println args = do
    putStrLn $ _pr_list False " " args
    return Nil

slurp ([MalString path]) = do
    str <- readFile path
    return $ MalString str
slurp _ = error $ "invalid arguments to slurp"

do_readline ([MalString prompt]) = do
    str <- readline prompt
    case str of
        Nothing -> error "readline failed"
        Just str -> return $ MalString str
do_readline _ = error $ "invalid arguments to readline"

-- Numeric functions

num_op op [MalNumber a, MalNumber b] = do
    return $ MalNumber $ op a b
num_op _ _ = error $ "illegal arguments to number operation"

cmp_op op [MalNumber a, MalNumber b] = do
    return $ if op a b then MalTrue else MalFalse
cmp_op _ _ = error $ "illegal arguments to comparison operation"

time_ms _ = do
   t <- getPOSIXTime
   return $ MalNumber $ round (t * 1000)


-- List functions

list args = return $ MalList args Nil

-- Vector functions

vector args = return $ MalVector args Nil

-- Hash Map functions

_pairup [x] = error "Odd number of elements to _pairup"
_pairup [] = return []
_pairup (MalString x:y:xs) = do
    rest <- _pairup xs
    return $ (x,y):rest

hash_map args = do
    pairs <- _pairup args
    return $ MalHashMap (Map.fromList pairs) Nil

assoc (MalHashMap hm _:kvs) = do
    pairs <- _pairup kvs
    return $ MalHashMap (Map.union (Map.fromList pairs) hm) Nil
assoc _ = error $ "invalid call to assoc"

dissoc (MalHashMap hm _:ks) = do
    let remover = (\hm (MalString k) -> Map.delete k hm) in
        return $ MalHashMap (foldl remover  hm ks) Nil
dissoc _ = error $ "invalid call to dissoc"

get (MalHashMap hm _:MalString k:[]) = do
    case Map.lookup k hm of
        Just mv -> return mv
        Nothing -> return Nil
get (Nil:MalString k:[]) = return Nil
get _ = error $ "invalid call to get"

contains_Q (MalHashMap hm _:MalString k:[]) = do
    if Map.member k hm then return MalTrue
    else return MalFalse
contains_Q (Nil:MalString k:[]) = return MalFalse
contains_Q _ = error $ "invalid call to contains?"

keys (MalHashMap hm _:[]) = do
    return $ MalList (map MalString (Map.keys hm)) Nil
keys _ = error $ "invalid call to keys"

vals (MalHashMap hm _:[]) = do
    return $ MalList (Map.elems hm) Nil
vals _ = error $ "invalid call to vals"


-- Sequence functions

_sequential_Q (MalList _ _) = MalTrue
_sequential_Q (MalVector _ _) = MalTrue
_sequential_Q _ = MalFalse

cons x Nil = MalList [x] Nil
cons x (MalList lst _) = MalList (x:lst) Nil
cons x (MalVector lst _) = MalList (x:lst) Nil

concat1 a (MalList lst _) = a ++ lst
concat1 a (MalVector lst _) = a ++ lst
do_concat args = return $ MalList (foldl concat1 [] args) Nil

nth ((MalList lst _):(MalNumber idx):[]) = do
    if idx < length lst then return $ lst !! idx
    else error "nth: index out of range"
nth ((MalVector lst _):(MalNumber idx):[]) = do
    if idx < length lst then return $ lst !! idx
    else error "nth: index out of range"
nth _ = error "invalid call to nth"

first (MalList lst _) = if length lst > 0 then lst !! 0 else Nil
first (MalVector lst _) = if length lst > 0 then lst !! 0 else Nil

rest (MalList lst _) = MalList (drop 1 lst) Nil
rest (MalVector lst _) = MalList (drop 1 lst) Nil

empty_Q Nil              = MalTrue
empty_Q (MalList [] _)   = MalTrue
empty_Q (MalVector [] _) = MalTrue
empty_Q _                = MalFalse

count Nil               = MalNumber 0
count (MalList lst _)   = MalNumber $ length lst
count (MalVector lst _) = MalNumber $ length lst
count _ = error $ "non-sequence passed to count"

conj ((MalList lst _):args) = return $ MalList ((reverse args) ++ lst) Nil
conj ((MalVector lst _):args) = return $ MalVector (lst ++ args) Nil
conj _ = error $ "illegal arguments to conj"

apply args = do
    f <- _get_call args
    lst <- _to_list (last args)
    f $ (init (drop 1 args)) ++ lst

do_map args = do
    f <- _get_call args
    lst <- _to_list (args !! 1)
    do new_lst <- mapM (\x -> f [x]) lst
       return $ MalList new_lst Nil

-- Metadata functions

with_meta ((MalList lst _):m:[])   = return $ MalList lst m
with_meta ((MalVector lst _):m:[]) = return $ MalVector lst m
with_meta ((MalHashMap hm _):m:[]) = return $ MalHashMap hm m
with_meta ((MalAtom atm _):m:[])   = return $ MalAtom atm m
with_meta ((Func f _):m:[])        = return $ Func f m
with_meta ((MalFunc {fn=f, ast=a, env=e, params=p, macro=mc}):m:[]) = do
    return $ MalFunc {fn=f, ast=a, env=e, params=p, macro=mc, meta=m}
with_meta _ = error $ "invalid with-meta call"

do_meta ((MalList _ m):[])      = return m
do_meta ((MalVector _ m):[])    = return m
do_meta ((MalHashMap _ m):[])   = return m
do_meta ((MalAtom _ m):[])      = return m
do_meta ((Func _ m):[])         = return m
do_meta ((MalFunc {meta=m}):[]) = return m
do_meta _ = error $ "invalid meta call"

-- Atom functions

atom (val:[]) = do
    ref <- newIORef val
    return $ MalAtom ref Nil
atom _ = error "invalid atom call"

deref (MalAtom ref _:[]) = do
    val <- readIORef ref
    return val
deref _ = error "invalid deref call"

reset_BANG (MalAtom ref _:val:[]) = do
    _ <- writeIORef ref $ val
    return val
reset_BANG _ = error "invalid deref call"

swap_BANG (MalAtom ref _:args) = do
    val <- readIORef ref
    f <- _get_call args
    new_val <- f $ [val] ++ (tail args)
    _ <- writeIORef ref $ new_val
    return new_val

ns = [
    ("=",  _func equal_Q),
    ("nil?", _func $ run_1 $ _nil_Q),
    ("true?", _func $ run_1 $ _true_Q),
    ("false?", _func $ run_1 $ _false_Q),
    ("symbol", _func $ run_1 $ symbol),
    ("symbol?", _func $ run_1 $ _symbol_Q),
    ("keyword", _func $ run_1 $ keyword),
    ("keyword?", _func $ run_1 $ _keyword_Q),

    ("pr-str", _func pr_str),
    ("str", _func str),
    ("prn", _func prn),
    ("println", _func println),
    ("readline", _func do_readline),
    ("read-string", _func (\[(MalString s)] -> read_str s)),
    ("slurp", _func slurp),

    ("<",  _func $ cmp_op (<)),
    ("<=", _func $ cmp_op (<=)),
    (">",  _func $ cmp_op (>)),
    (">=", _func $ cmp_op (>=)),
    ("+",  _func $ num_op (+)),
    ("-",  _func $ num_op (-)),
    ("*",  _func $ num_op (*)),
    ("/",  _func $ num_op (div)),
    ("time-ms", _func $ time_ms),
    
    ("list",     _func $ list),
    ("list?",    _func $ run_1 _list_Q),
    ("vector",   _func $ vector),
    ("vector?",  _func $ run_1 _vector_Q),
    ("hash-map", _func $ hash_map),
    ("map?",     _func $ run_1 _hash_map_Q),
    ("assoc",    _func $ assoc),
    ("dissoc",   _func $ dissoc),
    ("get",      _func $ get),
    ("contains?",_func $ contains_Q),
    ("keys",     _func $ keys),
    ("vals",     _func $ vals),

    ("sequential?", _func $ run_1 _sequential_Q),
    ("cons", _func $ run_2 $ cons),
    ("concat", _func $ do_concat),
    ("nth", _func nth),
    ("first", _func $ run_1 $ first),
    ("rest", _func $ run_1 $ rest),
    ("empty?", _func $ run_1 $ empty_Q),
    ("count", _func $ run_1 $ count),
    ("conj", _func $ conj),
    ("apply", _func $ apply),
    ("map", _func $ do_map),

    ("with-meta", _func $ with_meta),
    ("meta",      _func $ do_meta),
    ("atom",      _func $ atom),
    ("atom?",     _func $ run_1 _atom_Q),
    ("deref",     _func $ deref),
    ("reset!",    _func $ reset_BANG),
    ("swap!",     _func $ swap_BANG)]