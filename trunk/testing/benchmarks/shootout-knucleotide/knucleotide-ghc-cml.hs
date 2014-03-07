--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic
--
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Array.Base
import Data.Array.Unboxed
import Data.Array.IO
import qualified Data.ByteString.Char8 as S
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Concurrent
import qualified Control.Monad as CM
import Text.Printf
import Control.Concurrent.CML

send c v = sync $ transmit c v
recv c = sync $ receive c (\_ -> True)

main = do
    let skip = do
            l <- S.getLine
            if S.isPrefixOf (S.pack ">THREE") l
                then return ()
                else skip
    skip
    s <- S.getContents
    let content = S.filter ((/=) '\n') s;
    mapM_ (execute content) actions

data Actions = I Int | S String
actions = [I 1,I 2,
           S "GGT",S "GGTA",S "GGTATT",S "GGTATTTTAATT",S "GGTATTTTAATTTATAGT"]
execute content (I i) = writeFrequencies content i
execute content (S s) = writeCount content s

writeFrequencies :: S.ByteString -> Int -> IO ()
writeFrequencies input size = do
    ht <- tcalculate input size
    lst <- Main.foldM (\lst (k,v)->do
        v' <- peek v
        return $ (k,v'):lst) [] ht
    let sorted = sortBy (\(_,x) (_,y) -> y `compare` x) lst
        sum = fromIntegral ((S.length input) + 1 - size)
    mapM_ (\(k,v)-> do
        printf "%s %.3f\n"
            (toString k) ((100 * (fromIntegral v)/sum)::Double)) sorted
    putChar '\n'

writeCount :: S.ByteString -> String -> IO ()
writeCount input string = do
    let size = length string
        k = toNum (S.pack string) 0 size
    ht <- tcalculate input size
    res <- Main.lookup ht k
    case res of
        Nothing -> putStrLn $ string ++ " not found..."
        Just (s,v) -> do
            r <- peek v
            printf "%d\t%s\n" r (toString s)

tcalculate :: S.ByteString -> Int -> IO HM
tcalculate input size = do
    let
        l = [0..63]
        actions = map (\i -> (calculate input i size (length l))) l
    vars <- mapM (\action -> do
                    var <- channel
                    forkIO $ do
                        answer <- action
                        send var answer
                    return var) actions
    result <- newTable :: IO HM
    results <- mapM recv vars
    CM.foldM (\hres ht -> Main.foldM (\lst (k,v) -> do
                            res <- Main.lookup lst k
                            case res of
                                Nothing -> do
                                    r1 <- peek v
                                    r2 <- malloc
                                    poke r2 r1
                                    Main.insert lst k r2
                                Just (_,v1) -> do
                                    r1 <- peek v1
                                    r2 <- peek v
                                    poke v1 (r1+r2)
                                    return lst) hres ht)
             result results

calculate :: S.ByteString -> Int -> Int -> Int -> IO HM
calculate input beg size incr = do
    !ht <- newTable :: IO HM
    let
        calculate' ht i
         | i >= ((S.length input)+1 - size) = return ht
         | otherwise = do
            let k =  toNum input i size
            res <- Main.lookup ht k
            ht' <- case res of
                    Nothing -> do
                        !r <- malloc
                        poke r 1
                        Main.insert ht k r
                    Just (_,v) -> do
                        !r <- peek v
                        poke v (r+1)
                        return ht
            calculate' ht' (i+incr)
    calculate' ht beg

toNum :: S.ByteString -> Int -> Int -> T
toNum s beg size = toNum' 0 0 size
    where
        toNum' v1 v2 i
            | i == 0 = T v1 v2 size
            | i > 4 * sizeOf (undefined::Int) = toNum' v1 (pack v2) (i-1)
            | otherwise = toNum' (pack v1) v2 (i-1)
            where
                pack v = (v `shiftL` 2) .|.
                    (toNumA `unsafeAt` (ord (S.index s (beg+i-1))))

toString :: T -> String
toString (T v1 v2 s) = toString' v1 v2 0
    where
        toString' v1 v2 i
            | i >= s = []
            | i >= 4 * sizeOf (undefined::Int) =
                unpack v2 : toString' v1 (v2 `shiftR` 2) (i+1)
            | otherwise = unpack v1 : toString' (v1 `shiftR` 2) v2 (i+1)
            where
                unpack v = case v.&.3 of
                        0 -> 'A'
                        1 -> 'C'
                        2 -> 'T'
                        3 -> 'G'


toNumA :: UArray Int Int
toNumA = array (0,255) [(ord 'a',0),(ord 'c',1),(ord 't',2),(ord 'g',3),
            (ord 'A',0),(ord 'C',1),(ord 'T',2),(ord 'G',3)]

data T = T !Int !Int !Int
instance Eq T where
    (T a b _) == (T c d _) = a == c && b == d
class Hash h where
    hash :: h -> Int
instance Hash T where
    hash (T a b _) = a `xor` b

type HM = HashMap T (Ptr Int)
data HashMap k v = HashMap !(IOArray Int [(k,v)]) !Int !(Ptr Int)
dfltSz = 32
newTable :: IO (HashMap k v)
newTable = do
    !array <- newArray (0,dfltSz-1) []
    !pCnt <- malloc
    poke pCnt 0
    return $ HashMap array dfltSz pCnt

lookup :: (Eq k, Hash k)=>HashMap k v -> k -> IO (Maybe (k,v))
lookup (HashMap a tsz _) k = do
    let h = hash k
    !lst <- readArray a (h .&. (tsz-1))
    let
        loop [] = return Nothing
        loop ((!k',!v):xs)
            | k /= k' = loop xs
            | otherwise = return $ Just (k',v)
    loop lst

insert :: (Eq k, Hash k)=>HashMap k v -> k -> v -> IO (HashMap k v)
insert hm@(HashMap a tsz pcnt) k v = do
    let h = hash k
    !cnt <- peek pcnt
    poke pcnt (cnt+1)
    !lst <- readArray a (h .&. (tsz-1))
    writeArray a (h .&. (tsz-1)) ((k,v):lst)
    if (cnt+1) > tsz
        then rehash hm
        else return hm

rehash :: (Eq k,Hash k)=>HashMap k v -> IO (HashMap k v)
rehash hm@(HashMap _ tsz _) = do
    let newtsz = tsz * 2
    !array <- newArray (0,newtsz-1) []
    !pCnt <- malloc
    poke pCnt 0
    let newhm = HashMap array newtsz pCnt
    Main.foldM (\hm (k,v)-> Main.insert hm k v) newhm hm

foldM :: ( a -> (b,c) -> IO a) -> a -> HashMap b c -> IO a
foldM f s (HashMap a tsz _) = do
    let
        loop 0 s' = return s'
        loop i s' = do
            !lst <- readArray a (i-1)
            let
                loop' [] s' = return s'
                loop' (x:xs) s' = do
                    !s'' <- f s' x
                    loop' xs s''
            !s'' <- loop' lst s'
            loop (i-1) s''
    loop tsz s

