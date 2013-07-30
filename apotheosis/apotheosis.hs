module Apotheosis where

import Euterpea

import Control.Applicative
import Control.Monad.State

import Data.List.Zipper

import Data.Random
import Data.Random.Distribution.Categorical hiding (fromList)
import Data.Random.Source.DevRandom

data Section = Section
    { bassPitch :: Pitch
    , bassProb :: Double
    , pitchClassDist :: RVar PitchClass
    , octaveDist   :: RVar Octave
    , durationDist :: RVar Dur
    , volDist :: RVar Volume
    , restProb :: Double
    , restDurDist :: RVar Dur
    }

data Track = Track
    { music :: Music (Pitch, Volume)
    , progression :: Zipper (Section,Dur)
    , elapsedThisSection :: Dur
    , totalDuration :: Dur
    }

section :: Pitch -> Double -> [(Double,PitchClass)] -> [(Double,Octave)]
           -> [(Double,Dur)] -> [(Double,Volume)] -> Double -> [(Double,Dur)] -> Section
section bp bpb pcd od dd vd rp rd =
    let makeRVar = rvar . fromWeightedList
    in Section bp bpb (makeRVar pcd) (makeRVar od) (makeRVar dd) (makeRVar vd) rp (makeRVar rd)

getRVar :: RVar a -> StateT Track IO a
getRVar v = liftIO $ runRVar v DevURandom

getCurSection :: StateT Track IO Section
getCurSection = (fst . cursor) <$> gets progression

getCurSectionDur :: StateT Track IO Dur
getCurSectionDur = (snd . cursor) <$> gets progression

addRest :: StateT Track IO ()
addRest = do
    s <- getCurSection
    duration <- getRVar $ restDurDist s
    updateMusic $ addVolume 0 $ rest duration

addRanNote :: StateT Track IO ()
addRanNote = do
    s <- getCurSection
    duration <- getRVar $ durationDist s
    pitchclass <- getRVar $ pitchClassDist s
    octave <- getRVar $ octaveDist s
    volume <- getRVar $ volDist s
    updateMusic $ addVolume volume $ note duration (pitchclass,octave)

addBassNote :: StateT Track IO ()
addBassNote = do
    s <- getCurSection
    duration <- getRVar $ durationDist s
    volume <- getRVar $ volDist s
    let bp = bassPitch s
    updateMusic $ addVolume volume $ note duration bp

updateMusic :: Music (Pitch,Volume) -> StateT Track IO ()
updateMusic n = do
    m <- gets music
    elap <- gets elapsedThisSection
    secDur <- getCurSectionDur
    p <- gets progression
    td <- gets totalDuration

    put $ if elap >= secDur
                then Track (m :+: n) (right p) 0 td
                else Track (m :+: n) p (elap + dur n) td

evolve :: StateT Track IO ()
evolve = do
    m <- gets music
    td <- gets totalDuration
    rpr <- restProb <$> getCurSection
    bpr <- bassProb <$> getCurSection

    when (dur m < td) $ do

        r' <- getRVar (uniform 0.0 1.0 :: RVar Double)
        b' <- getRVar (uniform 0.0 1.0 :: RVar Double)

        case () of
          _ | r' < rpr -> addRest
          _ | b' < bpr && bpr /= 0 -> addBassNote
          _ -> addRanNote

        evolve

s1Guitar :: Section
s1Guitar = section (F,3) 0.15
      [(0.6,F),(0.1,B),(0.4,D),(0.6,A),(0.3,C),(1.0,E)]
      [(0.2,3),(0.3,4),(0.1,5)]
      [(0.1,hn),(0.25,qn),(0.5,en),(0.4,sn)]
      [(0.2,127),(1.0,100),(1.0,75),(0.3,50),(0.1,25)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.4,sn)]

s1Flute :: Section
s1Flute = section (F,3) 0
      [(0.6,F),(0.1,B),(0.4,D),(0.6,A),(0.3,C),(1.0,E)]
      [(0.05,4),(0.5,5),(0.1,6)]
      [(0.2,wn),(0.4,hn),(0.6,qn),(0.3,en),(0.2,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s1Bass :: Section
s1Bass = section (F,3) 0
      [(0.6,F),(0.1,B),(0.4,D),(0.6,A),(0.3,C),(1.0,E)]
      [(0.4,2),(0.5,3),(0.1,4)]
      [(0.2,wn),(0.4,hn),(0.6,qn),(0.3,en),(0.2,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s2Guitar :: Section
s2Guitar = section (Fs,3) 0.15
      [(0.6,Fs),(0.5,B),(0.1,D),(0.05,A),(0.4,Cs),(0.6,E)]
      [(0.2,3),(0.3,4),(0.1,5)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s2Flute :: Section
s2Flute = section (Fs,3) 0
      [(0.6,Fs),(0.5,B),(0.1,D),(0.05,A),(0.4,Cs),(0.6,E)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s2Bass :: Section
s2Bass = section (Fs,3) 0
      [(0.6,Fs),(0.5,B),(0.1,D),(0.05,A),(0.4,Cs),(0.6,E)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s3Guitar :: Section
s3Guitar = section (G,3) 0.15
      [(0.6,Gs),(0.5,Bs),(0.1,Cs),(0.05,Es),(0.4,Ds),(0.6,As),(0.3,Fs)]
      [(0.2,3),(0.3,4),(0.1,5)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s3Flute :: Section
s3Flute = section (G,3) 0
      [(0.6,Gs),(0.5,Bs),(0.1,Cs),(0.05,Es),(0.4,Ds),(0.6,As),(0.3,Fs)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s3Bass :: Section
s3Bass = section (G,3) 0
      [(0.6,Gs),(0.5,Bs),(0.1,Cs),(0.05,Es),(0.4,Ds),(0.6,As),(0.3,Fs)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s4Guitar :: Section
s4Guitar = section (Gs,3) 0.15
      [(0.6,Gs),(0.5,C),(0.1,Fs),(0.05,G),(0.4,B),(0.6,E)]
      [(0.2,3),(0.3,4),(0.1,5)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.1
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s4Flute :: Section
s4Flute = section (Gs,3) 0
      [(0.6,Gs),(0.5,C),(0.1,Fs),(0.05,G),(0.4,B),(0.6,E)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

s4Bass :: Section
s4Bass = section (Gs,3) 0
      [(0.6,Gs),(0.5,C),(0.1,Fs),(0.05,G),(0.4,B),(0.6,E)]
      [(0.8,4),(0.4,5),(0.9,6)]
      [(0.1,hn),(0.6,qn),(0.4,en),(0.3,sn)]
      [(1.0,100),(1.0,50)]
      0.2
      [(0.1,hn),(0.25,qn),(0.4,en),(0.3,sn)]

guitarTrack :: Track
guitarTrack = Track (rest 0) (fromList
                    [ (s1Guitar,2*wn)
                    , (s2Guitar,2*wn)
                    , (s1Guitar,2*wn)
                    , (s2Guitar,2*wn)
                    , (s3Guitar,2*wn)
                    , (s4Guitar,2*wn)
                    ]) 0 (12*wn)

fluteTrack :: Track
fluteTrack = Track (rest 0) (fromList
                    [ (s1Flute,2*wn)
                    , (s2Flute,2*wn)
                    , (s1Flute,2*wn)
                    , (s2Flute,2*wn)
                    , (s3Flute,2*wn)
                    , (s4Flute,2*wn)
                    ]) 0 (12*wn)

bassTrack :: Track
bassTrack = Track (rest 0) (fromList
                    [ (s1Bass,2*wn)
                    , (s2Bass,2*wn)
                    , (s1Bass,2*wn)
                    , (s2Bass,2*wn)
                    , (s3Bass,2*wn)
                    , (s4Bass,2*wn)
                    ]) 0 (12*wn)

evolveInstruments :: IO (Music (Pitch, Volume))
evolveInstruments = do
    gt <- instrument AcousticGuitarSteel <$> (music . snd) <$> runStateT evolve guitarTrack
    ft <- instrument Oboe <$> (music . snd) <$> runStateT evolve fluteTrack
    bt <- instrument Bassoon <$> (music . snd) <$> runStateT evolve bassTrack
    return $ tempo (3/5) $ gt :=: ft :=: bt

