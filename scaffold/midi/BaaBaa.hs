import Midi
import Data.ByteString as BS


main :: IO ()
main = BS.writeFile "baaBaa.mid" (make_music (Longer 4 baaBaa))

intro :: Music
intro  = Sequence [ C, C, G, G ]

run :: Music
run    = Sequence [ A, B, C, A ]

middle :: Music
middle = Sequence [ F, F, E, E ]

run2 :: Music
run2   = Sequence [ D, C, D, E ]

baaBaa :: Music
baaBaa = Sequence [ Longer 2  intro
                  , Higher 12 run
                  , Longer 4  G
                  , Longer 2  middle
                  , run2
                  , Longer 4  C ]
