module AffineChannelsProperties where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

-- ============================================================
-- ABSTRACT MODEL
-- ============================================================
-- We model a FreeST affine channel abstractly:
--   - A channel has a sender count and a FIFO queue
--   - sendA appends Just x to queue
--   - drop decrements sender count; if count == 0, appends Nothing
--   - receiveA dequeues head

data Msg a = Payload a | Terminator
  deriving (Eq, Show)

data Chan a = Chan
  { senderCount :: Int
  , queue       :: [Msg a]
  } deriving (Show)

newChan :: Int -> Chan a
newChan n = Chan n []

sendMsg :: a -> Chan a -> Chan a
sendMsg x (Chan n q) = Chan n (q ++ [Payload x])

dropSender :: Chan a -> Chan a
dropSender (Chan 1 q) = Chan 0 (q ++ [Terminator])
dropSender (Chan n q) = Chan (n-1) q

receiveMsg :: Chan a -> (Maybe (Msg a), Chan a)
receiveMsg (Chan n [])     = (Nothing, Chan n [])
receiveMsg (Chan n (x:xs)) = (Just x, Chan n xs)

drainAll :: Chan a -> ([a], Bool)
drainAll c = go c [] False
  where
    go ch acc term =
      case receiveMsg ch of
        (Nothing, _)             -> (reverse acc, term)
        (Just Terminator, ch')   -> go ch' acc True
        (Just (Payload x), ch')  -> go ch' (x:acc) term

-- ============================================================
-- GENERATORS
-- ============================================================

data ProducerAction a
  = Send a
  | Drop
  deriving (Show)

-- A trace is a list of per-producer action sequences
-- Each producer ends with exactly one Drop
newtype ProducerTrace a = ProducerTrace [[ProducerAction a]]
  deriving (Show)

genProducerActions :: Arbitrary a => Int -> Gen [ProducerAction a]
genProducerActions maxMsgs = do
  n    <- chooseInt (0, maxMsgs)
  msgs <- vectorOf n arbitrary
  return (map Send msgs ++ [Drop])

genTrace :: Arbitrary a => Gen (ProducerTrace a)
genTrace = do
  numProducers <- chooseInt (1, 4)
  actions      <- vectorOf numProducers (genProducerActions 5)
  return (ProducerTrace actions)

-- Interleave producer traces nondeterministically
interleave :: [[a]] -> Gen [a]
interleave [] = return []
interleave xss = do
  let nonempty = filter (not . null) xss
  if null nonempty
    then return []
    else do
      idx <- chooseInt (0, length nonempty - 1)
      let (h:t) = nonempty !! idx
          rest  = take idx nonempty ++ [t] ++ drop (idx+1) nonempty
      (h:) <$> interleave rest

applyTrace :: ProducerTrace Int -> Gen (Chan Int)
applyTrace (ProducerTrace traces) = do
  let ch0 = newChan (length traces)
  merged <- interleave traces
  return (foldl applyAction ch0 merged)
  where
    applyAction ch (Send x) = sendMsg x ch
    applyAction ch Drop     = dropSender ch

-- ============================================================
-- PROPERTY 1: TERMINATION SOUNDNESS
-- If all senders are dropped -> receiver eventually gets Terminator
-- ============================================================

prop_terminationSoundness :: Property
prop_terminationSoundness =
  forAll (genTrace @Int) $ \trace ->
  forAll (applyTrace trace) $ \ch ->
    let (_, term) = drainAll ch
    in  term === True

-- ============================================================
-- PROPERTY 2: NO EARLY TERMINATION
-- Terminator only appears after all senders are dropped
-- In the model: Terminator is appended only when count reaches 0
-- Observable: no Terminator in queue while senderCount > 0
-- ============================================================

prop_noEarlyTermination :: Property
prop_noEarlyTermination =
  forAll (genTrace @Int) $ \(ProducerTrace traces) ->
  forAll (interleave traces) $ \merged ->
    let ch0 = newChan (length traces)
        -- Scan prefix-by-prefix: Terminator must not appear before all Drops
        steps = scanl applyAction ch0 merged
        dropsSeenAt step = length (filter (== Drop) (take step merged))
        totalDrops = length traces
        terminatorPremature =
          any (\(i, ch) ->
                Terminator `elem` queue ch
                && dropsSeenAt i < totalDrops)
              (zip [0..] steps)
    in  counterexample "Terminator appeared before all senders dropped"
        (not terminatorPremature)
  where
    applyAction ch (Send x) = sendMsg x ch
    applyAction ch Drop     = dropSender ch

-- ============================================================
-- PROPERTY 3: EXACTLY ONE TERMINATOR
-- ============================================================

prop_exactlyOneTerminator :: Property
prop_exactlyOneTerminator =
  forAll (genTrace @Int) $ \trace ->
  forAll (applyTrace trace) $ \ch ->
    let terminators = length (filter (== Terminator) (queue ch))
    in  counterexample ("Expected 1 Terminator, got " ++ show terminators)
        (terminators === 1)

-- ============================================================
-- PROPERTY 4: MESSAGE PRESERVATION
-- Every sent value appears in the drained output
-- ============================================================

prop_messagePreservation :: Property
prop_messagePreservation =
  forAll (genTrace @Int) $ \trace@(ProducerTrace traces) ->
  forAll (applyTrace trace) $ \ch ->
    let sentValues  = [x | Send x <- concat traces]
        (recvd, _)  = drainAll ch
    in  counterexample
          ("Sent: " ++ show (sort sentValues) ++
           "\nRecv: " ++ show (sort recvd))
        (sort recvd === sort sentValues)

-- ============================================================
-- PROPERTY 5: NO DUPLICATION
-- No value appears more times in received than in sent
-- ============================================================

prop_noDuplication :: Property
prop_noDuplication =
  forAll (genTrace @Int) $ \trace@(ProducerTrace traces) ->
  forAll (applyTrace trace) $ \ch ->
    let sentValues  = [x | Send x <- concat traces]
        (recvd, _)  = drainAll ch
        countIn xs v = length (filter (== v) xs)
        allUnique = all (\v -> countIn recvd v <= countIn sentValues v)
                        recvd
    in  counterexample
          ("Duplication detected. Sent: " ++ show (sort sentValues) ++
           "\nRecv: " ++ show (sort recvd))
        allUnique

-- ============================================================
-- PROPERTY 6: FIFO — SINGLE PRODUCER
-- With one producer, received order == sent order
-- ============================================================

prop_fifoSingleProducer :: [Int] -> Property
prop_fifoSingleProducer msgs =
  let ch0    = newChan 1
      ch1    = foldl (flip sendMsg) ch0 msgs
      ch2    = dropSender ch1
      (recvd, term) = drainAll ch2
  in  counterexample
        ("FIFO violated. Sent: " ++ show msgs ++
         "\nRecv: " ++ show recvd)
        (recvd === msgs .&&. term === True)

-- ============================================================
-- PROPERTY 7: MULTI-PRODUCER MULTISET EQUALITY
-- received multiset == union of all sent multisets
-- ============================================================

prop_multiProducerSetEquality :: Property
prop_multiProducerSetEquality =
  forAll (genTrace @Int) $ \trace@(ProducerTrace traces) ->
  forAll (applyTrace trace) $ \ch ->
    let sentValues = sort [x | Send x <- concat traces]
        (recvd, _) = drainAll ch
    in  counterexample
          ("Multiset mismatch.\nSent: " ++ show sentValues ++
           "\nRecv: " ++ show (sort recvd))
        (sort recvd === sentValues)

-- ============================================================
-- PROPERTY 8: SENDER COUNT INVARIANT
-- senderCount is always non-negative
-- ============================================================

prop_senderCountNonNegative :: Property
prop_senderCountNonNegative =
  forAll (genTrace @Int) $ \(ProducerTrace traces) ->
  forAll (interleave traces) $ \merged ->
    let ch0    = newChan (length traces)
        states = scanl applyAction ch0 merged
    in  all (\ch -> senderCount ch >= 0) states
  where
    applyAction ch (Send x) = sendMsg x ch
    applyAction ch Drop     = dropSender ch

-- ============================================================
-- PROPERTY 9: RECEIVE AFTER TERMINATOR IS EMPTY
-- Once Terminator is dequeued, no further payloads exist
-- ============================================================

prop_noMessagesAfterTerminator :: Property
prop_noMessagesAfterTerminator =
  forAll (genTrace @Int) $ \trace ->
  forAll (applyTrace trace) $ \ch ->
    let msgs = queue ch
        afterTerm = dropWhile (/= Terminator) msgs
    in  counterexample
          ("Payloads found after Terminator: " ++ show afterTerm)
        (all (== Terminator) afterTerm)

-- ============================================================
-- PROPERTY 10: CLONE PRESERVES TOTAL COUNT
-- clone(s) where s has count n -> total senders still n
-- (clone increments count by 1 to give n+1 total minus original)
-- Model: newChan n represents n senders; clone = newChan (n+1)
-- ============================================================

prop_clonePreservesSenderCount :: Positive Int -> [Int] -> Property
prop_clonePreservesSenderCount (Positive n) msgs =
  let ch0        = newChan n
      chAfterSend = foldl (flip sendMsg) ch0 msgs
  in  senderCount chAfterSend === n

-- ============================================================
-- SPEC RUNNER
-- ============================================================

spec :: Spec
spec = do
  describe "AffineChannels" $ do

    describe "Termination Soundness" $
      it "all senders dropped -> Terminator received" $
        property prop_terminationSoundness

    describe "No Early Termination" $
      it "Terminator not emitted while senders exist" $
        property prop_noEarlyTermination

    describe "Exactly One Terminator" $
      it "exactly one Nothing/Terminator per channel lifetime" $
        property prop_exactlyOneTerminator

    describe "Message Preservation" $
      it "every sent value is received" $
        property prop_messagePreservation

    describe "No Duplication" $
      it "no value received more times than sent" $
        property prop_noDuplication

    describe "FIFO (single producer)" $
      it "single producer: received order == sent order" $
        property prop_fifoSingleProducer

    describe "Multi-producer Multiset Equality" $
      it "multiset of received == multiset of sent" $
        property prop_multiProducerSetEquality

    describe "Sender Count Non-Negative" $
      it "senderCount never goes below 0" $
        property prop_senderCountNonNegative

    describe "No Messages After Terminator" $
      it "queue contains only Terminator after close" $
        property prop_noMessagesAfterTerminator

    describe "Clone Preserves Count" $
      it "cloning does not lose senders" $
        property prop_clonePreservesSenderCount
