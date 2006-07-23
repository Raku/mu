import Test

import qualified Judy.BitSet as BS
import Judy.HashIO
import Judy.Freeze

main = no_plan $ do
    testIntSimple
    testIntGet
    testIntSwapBitSets
    testIntSetList
    testIntNullMember
    testIntSize
    testIntInsertDelete
    testIntFreeze
    testIntFromListF
    testIntToListF

    testUselessSimple
    testUselessGet
    testUselessSwapBitSets
    testUselessSetList

-- Tests for Int type. Uses default implementation for Enum types.
newIntSet :: IO (BS.BitSet Int)
newIntSet = BS.new

testIntSimple = do
    say "IntSimple"
    s <- newIntSet
    BS.set s 3 True
    BS.set s 10 True
    BS.set s 30 True
    BS.toList s .-= [3, 10, 30]

    BS.clear s
    BS.toList s .-= []

testIntGet = do
    say "IntGet"
    s <- newIntSet
    BS.get s 2 .=> False
    
    BS.set s 1 True
    BS.get s 1 .=> True
    
    BS.set s 2 True
    BS.get s 2 .=> True
    
    BS.set s 1 False
    BS.get s 1 .=> False
    
    BS.clear s
    BS.get s 1 .=> False

testIntSwapBitSets = do
    say "IntSwapBitSets"
    s1 <- newIntSet
    s2 <- newIntSet
    BS.set s1 1 True
    BS.set s2 2 True
    BS.swapBitSets s1 s2
    BS.toList s1 .-= [2]
    BS.toList s2 .-= [1]

testIntSetList = do
    say "IntSetList"
    s <- newIntSet
    BS.fromList [1..10] s
    BS.get s 10 .=> True

    BS.set s 10 False
    BS.get s 10 .=> False
    BS.get s 1  .=> True

testIntNullMember = do
    say "IntNullMember"
    s <- newIntSet
    BS.null s     .=> True
    BS.member 3 s .=> False

    BS.fromList [1..10] s
    BS.member 3 s .=> True
    BS.null s     .=> False

testIntSize = do
    say "IntSize"
    s <- newIntSet
    BS.size s .=> 0
    BS.fromList [1..10] s
    BS.size s .=> 10
    BS.clear s
    BS.size s .=> 0
    BS.set s 3 True
    BS.set s 2 True
    BS.size s .=> 2

testIntInsertDelete = do
    say "IntInsertDelete"
    s <- newIntSet
    BS.insert 1 s
    BS.toList s .-= [1]

    BS.delete 1 s
    BS.toList s .-= []

    BS.insert 1 s 
    BS.insert 2 s
    BS.insert 42 s
    BS.toList s .-= [1, 2, 42]

    BS.insert 1 s
    BS.toList s .-= [1, 2, 42]
    
    BS.delete 1 s
    BS.delete 2 s
    BS.toList s .-= [42]
    
    BS.delete 1 s
    BS.toList s .-= [42]

testIntFreeze = do
    say "IntFreeze"
    s <- newIntSet
    BS.insert 1 s
    BS.insert 2 s
    ice <- freeze s
    BS.memberF 1 ice  ==> True
    BS.memberF 2 ice  ==> True
    BS.memberF 42 ice ==> False

testIntFromListF = do
    say "IntFromListF"
    let ice = BS.fromListF [42, 59] :: Frozen (BS.BitSet Int)
    BS.memberF 42 ice ==> True
    BS.memberF 1 ice  ==> False
    BS.memberF 59 ice ==> True
    BS.memberF 2 ice  ==> False

testIntToListF = do
    say "IntToListF"
    s <- newIntSet
    BS.insert 1 s
    BS.insert 59 s
    ice <- freeze s
    BS.toListF ice   =-= [1, 59]
    BS.memberF 1 ice ==> True
    BS.memberF 2 ice ==> False


{- Tests for Useless type. -}
newUselessSet :: IO (BS.BitSet Useless)
newUselessSet = BS.new

data Useless = A | B | C | D deriving (Show, Ord, Eq, Enum)

-- Enum types can provide own implementation if needed
instance HashIO Useless where
    hashIO A = return 10
    hashIO B = return 20
    hashIO C = return 30
    hashIO D = return 40
instance UniqueHashIO Useless where
instance ReversibleHashIO Useless where
    unHashIO 10 = return A
    unHashIO 20 = return B
    unHashIO 30 = return C 
    unHashIO 40 = return D

testUselessSimple = do
    say "UselessSimple"
    s <- newUselessSet
    BS.set s A True
    BS.set s B True
    BS.toList s .-= [A, B]

    BS.clear s
    BS.toList s .-= []

    BS.set s D True
    BS.set s C True
    BS.set s D False
    BS.toList s .-= [C]

testUselessGet = do
    say "UselessGet"
    s <- newUselessSet
    BS.get s C .=> False

    BS.set s D True
    BS.get s D .=> True

    BS.set s A True
    BS.get s A .=> True

    BS.set s A False
    BS.get s A .=> False

    BS.clear s
    BS.get s D .=> False

testUselessSwapBitSets = do
    say "UselessSwapBitSets"
    s1 <- newUselessSet
    s2 <- newUselessSet
    BS.set s1 A True
    BS.set s2 B True
    BS.set s2 C True
    BS.swapBitSets s1 s2
    BS.toList s1 .-= [B, C]
    BS.toList s2 .-= [A]

testUselessSetList = do
    say "UselessSetList"
    s <- newUselessSet
    BS.fromList [A,B,C,D] s
    BS.get s D .=> True

    BS.set s D False
    BS.get s D .=> False
    BS.get s A .=> True
