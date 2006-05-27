
import qualified Judy.BitSet as BS


main = do
    s <- BS.new
    BS.set s 3 True
    BS.set s 10 True
    BS.set s 30 True
    print s
    xs <- BS.toListIO s
    print xs
    BS.clear s
    xs <- BS.toListIO s
    print xs

