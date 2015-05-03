module Brainfuck where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString as B

data CPU = CPU {
    cells :: M.Map Int Word8, -- ^ Memory
    dp :: Int, -- ^ Data Pointer
    pc :: Int, -- ^ Program Counter
    ins :: [Word8] -- ^ Instructions
} deriving (Show)

type CPUState a = State CPU a
type CPUIOState a = StateT CPU IO a

initialCPU :: [Word8] -> CPU
initialCPU = CPU M.empty 0 0 

-- | Increment the data pointer
incDp :: CPUIOState ()
incDp = get >>= \cpu -> put $ cpu {dp = dp cpu + 1}

-- | Decrement the data pointer
decDp :: CPUIOState ()
decDp = get >>= \cpu -> put $ cpu {dp = dp cpu - 1}

-- | Increment the byte at the data pointer
incByte :: CPUIOState ()
incByte = get >>= \cpu -> put $ 
            cpu{cells = M.insert (dp cpu) (M.findWithDefault 0 (dp cpu) (cells cpu) + 1) (cells cpu)}

-- | Decrement the byte at the data pointer
decByte :: CPUIOState ()
decByte = get >>= \cpu -> put $ 
            cpu{cells = M.insert (dp cpu) (M.findWithDefault 0 (dp cpu) (cells cpu) - 1) (cells cpu)}

-- | Output the byte at the data pointer
outputByte :: CPUIOState ()
outputByte = do
    cpu <- get
    lift $ putChar $ toEnum $ fromIntegral (M.findWithDefault 0 (dp cpu) (cells cpu))


-- | Input a byte into memory at the current data pointer
inputByte :: CPUIOState ()
inputByte = do
    c <- lift getChar
    cpu <- get
    put $ cpu{cells = M.insert (dp cpu) (fromIntegral $ fromEnum c) (cells cpu)}


-- | If the byte at the data pointer is 0, set the pc
--   to the instruction after the next matching "]"
jumpForwardIf0 :: CPUIOState ()
jumpForwardIf0 = do
    cpu <- get
    when (M.findWithDefault 0 (dp cpu) (cells cpu) == 0)
         $ jumpForward (pc cpu + 1) 0

  -- | Given current Position and stack counter of unmatched "["
  --   move forwards until match for "]" is found
  where jumpForward :: Int -> Int -> CPUIOState ()
        jumpForward i s = do
            cpu <- get
            let byte = toEnum $ fromIntegral $ ins cpu !! i
            if byte == '['
                then jumpForward (i + 1) (s + 1)
                else if byte == ']'
                        then if s == 0
                                then put $ cpu{pc = i}
                                else jumpForward (i + 1) (s - 1)
                        else jumpForward (i + 1) s


-- | If the byte at the data pointer is not 0, set the pc
--   to the instruction of the previous matching "["         
jumpBackwardIfNot0 :: CPUIOState ()
jumpBackwardIfNot0 = do
    cpu <- get
    when (M.findWithDefault 0 (dp cpu) (cells cpu) /= 0) $
          jumpBackward (pc cpu - 1) 0
  
  -- | Given current Position and stack counter of unmatched "]"
  --   move backwards until match for "[" is found
  where jumpBackward :: Int -> Int -> CPUIOState ()
        jumpBackward i s = do
            cpu <- get
            let byte = toEnum $ fromIntegral $ (ins cpu) !! i
            if byte == ']'
                then jumpBackward (i - 1) (s + 1)
                else if byte == '['
                        then if s == 0
                                then put $ cpu{pc = i - 1}
                                else jumpBackward (i - 1) (s - 1)
                        else jumpBackward (i - 1) s

execute :: CPUIOState ()
execute = do
    cpu <- get
    let op = toEnum $ fromIntegral $ ins cpu !! pc cpu
    case op of
        '>' -> incDp
        '<' -> decDp
        '+' -> incByte
        '-' -> decByte
        '.' -> outputByte
        ',' -> inputByte
        '[' -> jumpForwardIf0
        ']' -> jumpBackwardIfNot0
        _ -> return ()

    cpu' <- get
    put $ cpu'{pc = pc cpu' + 1}
    when ((pc cpu' + 1) < length (ins cpu')) execute


hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

stripNonBF :: [Word8] -> [Word8]
stripNonBF = filter(\c -> toEnum (fromIntegral c) `elem` "><+-.[],")

-- Given a BF file, execute it
runBF :: String ->  IO ()
runBF f = do
    file <- B.readFile f
    evalStateT execute (initialCPU (stripNonBF $ B.unpack file))
    putStrLn ""



