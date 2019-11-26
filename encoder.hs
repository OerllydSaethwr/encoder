module Encoder where

-- Instruction in one of three formats: Plus Ri Lj | Minus Ri Lj Lk | Halt
data Instruction = Plus Integer Integer | Minus Integer Integer Integer | Halt deriving (Show)


encodeSingle :: (Integer, Integer) -> Integer
encodeSingle tpl = encodeDouble tpl - 1

encodeDouble :: (Integer, Integer) -> Integer
encodeDouble (x, y) = (2 ^ x) * (2 * y + 1)

encodePlus :: Integer -> Integer -> Integer
encodePlus r l = encodeDouble (2 * r, l)

encodeMinus :: Integer -> Integer -> Integer -> Integer
encodeMinus r l1 l2 = encodeDouble (2 * r + 1, encodeSingle (l1, l2))

decodeDouble :: Integer -> (Integer, Integer)
decodeDouble a = decodeDouble' a 0
  where
    decodeDouble' :: Integer -> Integer -> (Integer, Integer)
    decodeDouble' a cnt
      | odd a     = (cnt, div a 2)
      | otherwise = decodeDouble' (div a 2) (cnt + 1)

decodeSingle :: Integer -> (Integer, Integer)
decodeSingle a = decodeDouble (a + 1)
    
decodeInstruction :: Integer -> Instruction
decodeInstruction a
  | a == 0             = Halt
  | even (fst decoded) = decodePlus decoded
  | otherwise          = decodeMinus decoded
  where
    decodePlus :: (Integer, Integer) -> Instruction 
    decodePlus (x, y) = Plus (div x 2) y

    decoded = decodeDouble a

    decodeMinus :: (Integer, Integer) -> Instruction
    decodeMinus (x, y) = Minus (div (x - 1) 2) j k
      where
        (j, k) = decodeSingle y

