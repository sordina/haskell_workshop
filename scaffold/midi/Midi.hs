module Midi where

import Codec.Midi
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B

type Note = Int
type Melody = [Note]
type MidiEvent = (Ticks, Message)

data Music
  = C
  | C'
  | D
  | E_
  | E 
  | F
  | F'
  | G
  | G'
  | A
  | B_
  | B
  | Sharp Music
  | Flat Music
  | Higher Integer Music
  | Lower Integer Music
  | Longer Integer Music
  | Sequence [Music]
 deriving(Show)

class Notable a where
  toNotes :: a -> [Note]

instance Notable Music where
    toNotes music = map (+50) (toNotes' music)
        where
        toNotes' C = [0]
        toNotes' C' = [1]
        toNotes' D = [2]
        toNotes' E_ = [3]
        toNotes' E = [4]
        toNotes' F = [5]
        toNotes' F' = [6]
        toNotes' G = [7]
        toNotes' G' = [8]
        toNotes' A = [9]
        toNotes' B_ = [10]
        toNotes' B = [11]
        toNotes' (Sequence ms) = concatMap toNotes' ms
        toNotes' (Sharp music') =  map (+1) (toNotes' music')
        toNotes' (Flat music') =  map (subtract 1) (toNotes' music')
        toNotes' (Higher n music') =  map (+ (fromIntegral n) ) (toNotes' music')
        toNotes' (Lower n music') =  map (subtract ((fromIntegral n ))) (toNotes' music')
        toNotes' (Longer n music') =  concatMap (replicate (fromIntegral n) ) (toNotes' music')

midiSkeleton :: Track Ticks -> Midi
midiSkeleton mel =  Midi {
         fileType = MultiTrack, 
         timeDiv = TicksPerBeat 480, 
         tracks = [
          [
           (0,ChannelPrefix 0),
           (0,TrackName " Grand Piano  "),
           (0,InstrumentName "GM Device  1"),
           (0,TimeSignature 4 2 24 8),
           (0,KeySignature 0 0)
          ]
          ++
          mel
          ++
          [
           (0,TrackEnd)
          ]
         ]
       }  


keydown :: Note -> MidiEvent
keydown k =  (0,NoteOn {channel = 0, key = k, velocity = 80})

keyup :: Note -> MidiEvent
keyup k =  (480,NoteOn {channel = 0, key = k, velocity = 0})

playnote :: Note -> Track Ticks
playnote k = [ keydown k, keyup k ]


createMidi :: Melody -> S.ByteString
createMidi notes =  S.pack $ L.unpack $ B.toLazyByteString $ buildMidi $ midiSkeleton $ concat $ map playnote notes

make_music ::  Music -> S.ByteString
make_music music = createMidi (toNotes music)



