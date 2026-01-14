module Main where -- for ghc compliation

-- Random Number Generator
randomNumGen :: Int -> (Int, Int)  -- takes an Int, returns a tuple, (random number, new seed)
randomNumGen seed = (newSeed, newSeed)  -- The 2nd newSeed is used to generate the next random number
  where
    -- Uses LCG logic
    a = 1103515245
    c = 12345
    m = 2^31
    newSeed = (a * seed + c) `mod` m  -- computes the next seed


-- Get a random number in a range of (low - high)
getRandomNum :: Int -> Int -> Int -> (Int, Int)
getRandomNum low high seed = (randValue, newSeed)
  where
    (r, newSeed) = randomNumGen seed
    range = high - low + 1
    randValue = (r `mod` range) + low


-- The deck, builds a 52 card deck (does not care about suits)
deck :: [Int]
deck = makeDeck 4
  where
    makeDeck :: Int -> [Int]
    makeDeck 0 = [] -- no suits left, we are done
    makeDeck suits = [1..13] ++ makeDeck (suits - 1) -- recursively adds 1-13 for each current suit, until no suits remain and the deck is built


-- Removes the drawn card from the deck at the passed in index
removeCard :: Int -> [Int] -> [Int]
removeCard n xs = kept ++ removed -- xs here represents the deck, not the rest of a list
  where
    kept = take n xs  -- keeps all elements before index n
    removed = drop (n + 1) xs  -- skips the element at index n, & keep the rest


-- Get a random card
pickCard :: [Int] -> Int -> (Int, [Int], Int)
pickCard currentDeck seed = (card, newDeck, seed2)
  where
    deckLen = getLength currentDeck
    (idx, seed2) = getRandomNum 0 (deckLen - 1) seed
    card = getIndex idx currentDeck
    newDeck = removeCard idx currentDeck


-- Get the length 
getLength :: [Int] -> Int
getLength xs = length xs 

-- Get the element at a specific index
getIndex :: Int -> [Int] -> Int
getIndex n xs = xs !! n   


-- Returns and converts the numbers in the deck to their blackjack values
value :: Int -> Int
value 1 = 11 -- Ace
value n
    | n >= 2 && n <= 10 = n
    | otherwise = 10 -- Jack, Queen, and King


-- Returns the total value of the cards in a hand
handTotal :: [Int] -> Int
handTotal cards = adjust total aces
  where
    total = sum [value c | c <- cards] -- sum the Blackjack values of all cards
    aces = length [c | c <- cards, c == 1] -- count how many Aces are in the hand, only count c if the value = 1

    -- Adjusts for Aces if total > 21
    adjust t a
        | t <= 21 = t  -- total is fine
        | a > 0 = adjust (t - 10) (a - 1) -- use Ace as 1 instead of 11, decrement aces, and subtract 10 from the total
        | otherwise = t -- total > 21, no Aces left


-- Converts a card number into its name
showCard :: Int -> String
showCard n
  | n == 1  = "Ace"
  | n == 11 = "Jack"
  | n == 12 = "Queen"
  | n == 13 = "King"
  | otherwise = show n


-- Takes a list of card numbers and returns a readable string of the card names/values
showHand :: [Int] -> String
showHand [] = "" -- An empty hand returns an empty string
showHand (c:cs)
    | null cs = showCard c -- If last card, calls showCard on it
    | otherwise = showCard c ++ ", " ++ showHand cs 


-- Player's turn
playerTurn :: [Int] -> [Int] -> Int -> IO ([Int], [Int], Int)
playerTurn hand deck seed = do
    putStrLn ("Your hand: " ++ showHand hand ++ " (" ++ show total ++ ")")
    putStr "Hit or Stay? (h/s): "
    choice <- getLine -- Reads in the user's choice
    nextAction choice
  where
    total = handTotal hand
    nextAction _
        | total > 21 = putStrLn "Bust: " >> return (hand, deck, seed)
    nextAction "h" = do
        putStrLn ("You drew: " ++ showCard card)
        playerTurn (hand ++ [card]) newDeck newSeed -- calls playerTurn again after you hit
      where
        (card, newDeck, newSeed) = pickCard deck seed
    nextAction "s" = return (hand, deck, seed) -- returns the current hand if you stay


-- Dealer's Turn
dealerTurn :: [Int] -> [Int] -> Int -> IO ([Int], Int)
dealerTurn hand deck seed
    | total >= 17 = do -- dealer cannot hit if they have a hand >= 17
        putStrLn ("Dealer stays at: " ++ showHand hand ++ " (" ++ show total ++ ")")
        return (hand, seed)
    | otherwise = do
        putStrLn ("Dealer draws: " ++ showCard card)
        dealerTurn (hand ++ [card]) newDeck newSeed
  where
    total = handTotal hand
    (card, newDeck, newSeed) = pickCard deck seed


-- Determines the winner
winner :: [Int] -> [Int] -> String
winner player dealer
    | playerTotal > 21 = "Dealer wins!"
    | dealerTotal > 21 = "Player wins!"
    | playerTotal > dealerTotal = "Player wins!"
    | dealerTotal > playerTotal = "Dealer wins!"
    | otherwise = "Tie!"
  where
    playerTotal = handTotal player
    dealerTotal = handTotal dealer
    

-- Tracks the Wins, and returns the updated win count
updateWins :: String -> Int -> Int -> (Int, Int)
updateWins resultStr playerWins dealerWins
    | resultStr == "Player wins!" = (playerWins + 1, dealerWins) 
    | resultStr == "Dealer wins!" = (playerWins, dealerWins + 1)
    | otherwise = (playerWins, dealerWins) --Tie


-- Handles the Dealer's Turn, and bust's from the player
processRound :: [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> IO ()
processRound playerFinal dealerHand deckAfterP seedAfterP playerWins dealerWins
    | handTotal playerFinal > 21 = do -- Player busts, dealer wins
        putStrLn ""
        putStrLn ("Your final:   " ++ showHand playerFinal ++ " (" ++ show (handTotal playerFinal) ++ ")")
        putStrLn ("Dealer final: " ++ showHand dealerHand ++ " (" ++ show (handTotal dealerHand) ++ ")")
        putStrLn "Result: Dealer wins!"

        let (newPlayerWins, newDealerWins) = updateWins "Dealer wins!" playerWins dealerWins
        playRounds newPlayerWins newDealerWins seedAfterP

    | otherwise = do
        -- Dealer plays
        (dealerFinal, _) <- dealerTurn dealerHand deckAfterP seedAfterP

        putStrLn ""
        putStrLn ("Your final:   " ++ showHand playerFinal ++ " (" ++ show (handTotal playerFinal) ++ ")")
        putStrLn ("Dealer final: " ++ showHand dealerFinal ++ " (" ++ show (handTotal dealerFinal) ++ ")")

        let resultStr = winner playerFinal dealerFinal
        putStrLn ("Result: " ++ resultStr)

        let (newPlayerWins, newDealerWins) = updateWins resultStr playerWins dealerWins
        playRounds newPlayerWins newDealerWins seedAfterP


-- Play multiple rounds, and can stop after each round when "exit" is entered
playRounds :: Int -> Int -> Int -> IO ()
playRounds playerWins dealerWins seed = do
    putStrLn $ "\nCurrent Score - Player: " ++ show playerWins ++ ", Dealer: " ++ show dealerWins
    putStr "Press Enter to play a round or type \"exit\" to quit: "
    choice <- getLine

    if choice == "exit"
        then putStrLn $ "Final Score - Player: " ++ show playerWins ++ ", Dealer: " ++ show dealerWins
        else do
            -- Deal initial cards (d1–d4 tracks the deck, not dealer’s hand)
            let (pCard1, d1, s1) = pickCard deck seed
                (pCard2, d2, s2) = pickCard d1 s1
                (dCard1, d3, s3) = pickCard d2 s2
                (dCard2, deckAfterDeal, s4) = pickCard d3 s3

            -- Starting Hand
            let playerHand = [pCard1, pCard2]
                dealerHand = [dCard1, dCard2]

            putStrLn ("Dealer shows: " ++ showCard dCard1)

            -- Player's turn
            (playerFinal, deckAfterP, seedAfterP) <- playerTurn playerHand deckAfterDeal s4

            -- Dealer turn, and continues the game
            processRound playerFinal dealerHand deckAfterP seedAfterP playerWins dealerWins


-- Main function
main :: IO ()
main = do
    putStr "Enter any integer (seed) for the random number generator to begin game: "
    input <- getLine
    let seed = read input
    playRounds 0 0 seed  -- start with zero wins for both
