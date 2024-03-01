import Text.Read (readMaybe)
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

-- :set -package random
-- :set -package array

type Game = Action -> State -> Result

data Result = EndOfGame State Double
            | ContinueGame State
            deriving (Eq, Show)

-- Interal State, Player's Bet, Computer's Bet, Computer's Bet Unit, count, turn (player = 0, comp = 1, dealer = 2), Player stand, comp stand, dealer stand, player bust, computer bust
data State = State InternalState Double Double Double Double Double Bool Bool Bool Bool Bool
            deriving (Eq, Show)

-- Players Cards, Computer Cards, Dealer Cards, Deck
type InternalState = ([Card], [Card], [Card], [Card])

-- (Suit, Number)
type Card = (Char, Int)

data Action = Hit
            | Stand
            deriving (Eq, Show)
-- (Suit, Number)
type Balances = (Double, Double)

-- type PlayerHand = [Card]
-- type DealerHand = [Card]
-- type ComputerHand = [Card]
-- type Deck = [Card]

-- Taken from https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- Hit function that checks whose turn it is gives them the card and switches the turn depending on who has stood
blackjack :: Game
blackjack Hit (State (playerHand, computerHand, dealerHand, firstCard:tailDeck) bet cbet cunit count turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand && dStand = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)
    | turn == 0 && cStand = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand && pStand = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand && cStand = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet cunit (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)

blackjack Stand (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand && dStand = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn True cStand dStand pBust cBust) 0
    | turn == 0 && cStand = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 2 True cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 1 True cStand dStand pBust cBust)
    | turn == 1 && dStand && pStand = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand True dStand pBust cBust) 0
    | turn == 1 && dStand = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 0 pStand True dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 2 pStand True dStand pBust cBust)
    | turn == 2 && pStand && cStand = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand True pBust cBust) 0
    | turn == 2 && pStand = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 1 pStand cStand True pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count 0 pStand cStand True pBust cBust)

-- checks if any of the hands have busted, if both the player and the computer have bust then EndofGame state will be returned, same if Dealer busts.
-- Else continue game and update state to show if the current turn player has busted or not
checkBust :: State -> Result
checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust)
    | getHandValue dealerHand > 21 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) 2
    | getHandValue playerHand > 21 && cBust = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn True cStand dStand True cBust) 0
    | getHandValue computerHand > 21 && pBust = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand True dStand pBust True) 0
    | getHandValue computerHand > 21 && dStand && pStand = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand True dStand pBust True) 0
    | getHandValue playerHand > 21 && dStand && cStand = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn True cStand dStand True cBust) 0
    | getHandValue playerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn True cStand dStand True cBust)
    | getHandValue computerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand True dStand pBust True)
    | otherwise = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust)

-- updates the counter
updateCount :: (Ord a1, Num a1, Fractional a2) => (a3, a1) -> a2 -> a2
updateCount (x, y) cur_count = do
    if y == 10 || y == 1 then
        cur_count - 1
    else if y == 9 then
        cur_count - 0.5
    else if y == 2 || y == 7 then
        cur_count + 0.5
    else if y == 3 || y == 4 || y == 6 then
        cur_count + 1
    else if y == 5 then
        cur_count + 1.5
    else
        cur_count

-- gets the value of a given hand 
getHandValue :: (Ord b, Num b, Foldable t) => t (a, b) -> b
getHandValue hand = do
    let aces = foldr (\ (x,y) z -> if y == 1 then z + 1 else z) 0 hand
    if aces > 0 then do
        getBestValue (getAceValues hand aces)
    else
        foldr (\ (x,y) z -> y + z) 0 hand

-- gets all possible values of a hand containing aces number of aces
getAceValues :: (Ord t1, Num t1, Foldable t2) => t2 (a, t1) -> t1 -> [t1]
getAceValues hand aces
    | aces > 0 = (foldr (\ (x,y) z -> y + z) 0 hand) + (10 * aces) : getAceValues hand (aces - 1)
    | otherwise = [foldr (\ (x,y) z -> y + z) 0 hand]

-- get's the highest number >= to 21
getBestValue :: (Ord t, Num t) => [t] -> t
getBestValue (h:t)
    | length t > 0 = if h > 21 then getBestValue t else h
    | otherwise = h

-- creates a full deck of cards
fullDeck :: [Card]
fullDeck = do
    let oneDeck = [(suit, if value < 11 then value else 10) | suit <- ['s','d','h','c'], value <- [1..13]]
    unsafePerformIO (shuffle (oneDeck ++ oneDeck ++ oneDeck ++ oneDeck ++ oneDeck ++ oneDeck))

-- starts the blackjack game
start :: IO Balances
start = do
    putStrLn "OMG BLACKJACK, how much do you want to deposit?"
    deposit <- getDeposit
    play blackjack (State ([], [], [], fullDeck) 0 0 (deposit / 1000) 0 0 False False False False False) (deposit, deposit)

-- the start of the gameplay loop where the players bet is taken as input and the deck is reshuffled if there are too few remaining cards in the deck
play :: Game -> State -> Balances-> IO Balances
play game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    let newcbet = (1 + max ((count * 52) / fromIntegral (length deck)) (-1)) * cunit
    let (ContinueGame state2) = game Hit state
    let (ContinueGame state3) = game Hit state2
    let (ContinueGame state4) = game Hit state3
    let (ContinueGame state5) = game Hit state4
    let (ContinueGame state6) = game Hit state5
    let (ContinueGame state7) = game Hit state6
    let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state7 in do
        putStrLn "How much you betting?"
        newbet <- getBet x
        if length deck < 125 then do
            personPlay game (ContinueGame (State (playerHand, computerHand, dealerHand, fullDeck) newbet newcbet cunit 0 0 False False False False False)) (x,y)
        else
            personPlay game (ContinueGame (State (playerHand, computerHand, dealerHand, deck) newbet newcbet cunit count 0 False False False False False)) (x,y)
            

-- If the player decides to hit then do Hit Action if stand do Stand Action
personPlay :: Game -> Result -> Balances -> IO Balances
personPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Cards: " ++ show playerHand)
    putStrLn ("Computer's Cards: " ++ show computerHand)
    putStrLn ("Dealer's Cards: " ++ show dealerHand)
    putStrLn "\nChoose whether to [h]it or [s]tand:"
    line <- getLine
    if line == "h" then
        if cStand && dStand then
            personPlay game (game Hit state) balances
        else if cStand then
            dealerPlay game (game Hit state) balances
        else
            computerPlay game (game Hit state) balances
    else if line == "s" then
        if cStand && dStand then
            personPlay game (game Stand state) balances
        else if cStand then
            dealerPlay game (game Stand state) balances
        else
            computerPlay game (game Stand state) balances
    else do
        putStrLn "\nThat is not a valid input."
        personPlay game (ContinueGame state) balances

-- delegates the EndofGame tasks to the dealer
personPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    dealerPlay game (EndOfGame state lost) (x,y)

-- decides whether or not the computer should stand based on other hands
shouldStand :: (Ord a1, Ord a2, Num a3, Num a1, Num a2, Eq a3) => a3 -> a1 -> a2 -> Bool
shouldStand aces handVal upCard = do
    if aces == 0 then do
        if handVal > 16 || (handVal > 12 && upCard < 7 && upCard /= 1) || (handVal == 12 && (upCard == 4 || upCard == 5 || upCard == 6)) then do
            True
        else do
            False
    else do
        if handVal > 18 || (handVal == 18 && upCard /= 9 && upCard /= 10 && upCard /= 1) then do
           True
        else do
            False

-- Decides and plays what action the Computer should take based off of the card counting value
computerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
computerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    let stand = shouldStand (foldr (\ (x,y) z -> if y == 1 then z + 1 else z) 0 computerHand) (getHandValue computerHand) (snd (last dealerHand))

    if stand then do
        putStrLn "\nComputer Stand"
        if pStand && dStand then
            computerPlay game (game Stand state) balances
        else if dStand then
            personPlay game (game Stand state) balances
        else
            dealerPlay game (game Stand state) balances
    else do
        putStrLn "\nComputer Hit"
        if pStand && dStand then
            computerPlay game (game Hit state) balances
        else if dStand then
            personPlay game (game Hit state) balances
        else
            dealerPlay game (game Hit state) balances

-- delegates the EndofGame tasks to the dealer
computerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    dealerPlay game (EndOfGame state lost) (x,y)

-- Decides and plays what action the Dealer should take, (Hit if hand value < 17, Stand if else)
dealerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
dealerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    if getHandValue dealerHand < 17 then do
        putStrLn "\nDealer Hit"
        if pStand && cStand then
            dealerPlay game (game Hit state) balances
        else if pStand then
            computerPlay game (game Hit state) balances
        else
            personPlay game (game Hit state) balances
    else do
        putStrLn "\nDealer Stand"
        if pStand && cStand then
            dealerPlay game (game Stand state) balances
        else if pStand then
            computerPlay game (game Stand state) balances
        else
            personPlay game (game Stand state) balances

-- When Dealer gets EndOfGame state update the appropriate values in Balances depending on who won the round
dealerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("\nYour Cards: " ++ show playerHand)
    putStrLn ("Computer's Cards: " ++ show computerHand)
    putStrLn ("Dealer's Cards: " ++ show dealerHand)
    if lost == 2 then do
        if pBust then do
            putStrLn "\nPlayer Busted"
            putStrLn "Dealer Bust\n"
            if checkIfOutOfMoney (x - bet) then do
                playerOutOfMoneyStatement (x - bet, y + cbet)
            else do
                keepPlayingStatement game state (x - bet, y + cbet)
        else if cBust then do
            putStrLn "\nComputer Busted"
            putStrLn "Dealer Bust\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x + bet, y - cbet)
            else do
                keepPlayingStatement game state (x + bet, y - cbet)
        else do
            putStrLn "\nDealer Bust\n"
            keepPlayingStatement game state (x + bet, y + cbet)
    else if pBust && cBust then do
        putStrLn "\nPlayer and Computer Bust, Dealer Wins"
        dealerBeatBothStatement game state (x,y)
    else if pBust then do
        putStrLn "\nPlayer Bust"
        if getHandValue computerHand > getHandValue dealerHand then do
            putStrLn "Computer beat Dealer\n"
            if checkIfOutOfMoney (x - bet) then do
                playerOutOfMoneyStatement (x - bet, y + cbet)
            else do
                keepPlayingStatement game state (x - bet, y + cbet)
        else if getHandValue computerHand < getHandValue dealerHand then do
            putStrLn "Dealer beat Computer\n"
            dealerBeatBothStatement game state (x,y)
        else do
            putStrLn "Computer tied Dealer\n"
            if checkIfOutOfMoney (x - bet) then do
                playerOutOfMoneyStatement (x - bet, y)
            else do
                keepPlayingStatement game state (x - bet, y)
    else if cBust then do
        putStrLn "\nComputer Bust"
        if getHandValue playerHand > getHandValue dealerHand then do
            putStrLn "Player beat Dealer\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x + bet, y - cbet)
            else do
                keepPlayingStatement game state (x + bet, y - cbet)
        else if getHandValue playerHand < getHandValue dealerHand then do
            putStrLn "Dealer beat Player\n"
            dealerBeatBothStatement game state (x,y)
        else do
            putStrLn "Player tied Dealer\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else do
                keepPlayingStatement game state (x, y - cbet)
    else do
        winnerCheckerIfAllStand game state (x,y)

-- If the player and the computer stood, see who won the round depending on hand values
winnerCheckerIfAllStand :: (Action -> State -> Result) -> State -> Balances -> IO Balances
winnerCheckerIfAllStand game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    let playerValue = getHandValue playerHand
    let dealerValue = getHandValue dealerHand
    if  playerValue > dealerValue then do
        putStrLn "\nPlayer beat Dealer"
        checkComputerWin game state (x + bet,y)
    else if playerValue < dealerValue then do
        putStrLn "\nDealer beat Player"
        checkComputerWin game state (x - bet,y)
    else do
        putStrLn "\nPlayer tied Dealer"
        checkComputerWin game state (x,y)

-- if the computer won check aswell as check if either player or computer is out of money
checkComputerWin :: (Action -> State -> Result) -> State -> Balances -> IO Balances
checkComputerWin game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    let computerValue = getHandValue computerHand
    let dealerValue = getHandValue dealerHand
    if computerValue > dealerValue then do
        putStrLn "Computer beat Dealer\n"
        if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x, y + cbet)
        else do
            keepPlayingStatement game state (x, y + cbet)
    else if computerValue == dealerValue then do
        putStrLn "Computer tied Dealer\n"
        if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x,y)
        else do
            keepPlayingStatement game state (x, y)
    else do
        putStrLn "Dealer beat Computer"
        if checkIfOutOfMoney x && checkIfOutOfMoney (y - cbet) then do
            bothOutOfMoneyStatement (x, y - cbet)
        else if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x, y - cbet)
        else if checkIfOutOfMoney (y - cbet) then do
            computerOutOfMoneyStatement (x, y - cbet)
        else do
            keepPlayingStatement game state (x, y - cbet)

-- check if the balance is 0 or less
checkIfOutOfMoney :: Double -> Bool
checkIfOutOfMoney balance
    | balance == 0 = True
    | otherwise = False

-- output the proper statement if the dealer beat both computer and player
dealerBeatBothStatement :: (Action -> State -> Result) -> State -> Balances -> IO Balances
dealerBeatBothStatement game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    if checkIfOutOfMoney (y - cbet) && checkIfOutOfMoney (x - bet) then do
        bothOutOfMoneyStatement (x - bet, y - cbet)
    else if checkIfOutOfMoney (y - cbet) then do
        computerOutOfMoneyStatement (x - bet, y - cbet)
    else if checkIfOutOfMoney (x - bet) then do
        playerOutOfMoneyStatement (x - bet, y - cbet)
    else do
        keepPlayingStatement game state (x - bet, y - cbet)

-- output the proper statement if the player has ran out of money and restart game
playerOutOfMoneyStatement :: Balances -> IO Balances
playerOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "You're out of money you LOSE\n"
    start

-- output the proper statement if the computer has ran out of money and restart game
computerOutOfMoneyStatement :: Balances -> IO Balances
computerOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "Computer out of money you WIN\n"
    start

-- output the proper statement if the computer and the player has ran out of money and restart game
bothOutOfMoneyStatement :: Balances -> IO Balances
bothOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "Both out of money you TIE\n"
    start

-- checks if the player wants to keep playing after a round 
keepPlayingStatement :: (Action -> State -> Result) -> State -> Balances -> IO Balances
keepPlayingStatement game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet cunit count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer Balance: " ++ show y)
    putStrLn "Do you want to keep playing, [Y]es, [N]o"
    answer <- getLine
    if answer == "y" || answer == "Y" then do
        putStrLn "\n"
        play game (State ([], [], [], deck) 0 0 cunit count 0 False False False False False) (x,y)
    else if answer == "n" || answer == "N" then do
        putStrLn "\nIt was fun playing"
        putStrLn "\nFinal Balances"
        putStrLn ("Your Balance: " ++ show x)
        putStrLn ("Computer Balance: " ++ show y)
        return (x,y)
    else do
        putStrLn "Invalid Answer"
        keepPlayingStatement game state (x,y)

-- get the player input for a number and redo if the input is not a valid number
getNumber :: IO Double
getNumber = do
    number <- getLine
    case readMaybe number :: Maybe Double of
        Nothing -> do
            putStrLn "That is not a number."
            getNumber
        Just x ->
            if x > 0
                then return x
                else do
                    putStrLn "We aren't giving you any money."
                    getNumber

-- get the player's bet and redo if the player bets more than their current balance
getBet :: Double -> IO Double
getBet balance = do
    bet <- getNumber
    if bet > balance then do
        putStrLn "You can't bet what you don't have."
        getBet balance
    else
        return bet

-- get the player's intial deposit and redo if the player deposits less than 1000
getDeposit :: IO Double
getDeposit = do
    balance <- getNumber
    if balance < 1000 then do
        putStrLn "You must bet at least 1000 dollars."
        getDeposit
    else
        return balance

-------------------- Test Cases ---------------------------

-- dealerPlay blackjack (ContinueGame `state`) (100,100)
allTie :: State
allTie = State ([('h',10),('h',10)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

allStandWin :: State
allStandWin = State ([('h',10),('h',10)], [('h',10),('h',10)], [('h',10),('h',9)], [('h',6)]) 10 10 1 0 2 True True False False False

allStandAllLose :: State
allStandAllLose = State ([('h',10),('h',9)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

playerWinCompWin :: State
playerWinCompWin = State ([('h',10),('h',11)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

playerWinCompLose :: State
playerWinCompLose = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

playerWinCompTie :: State
playerWinCompTie = State ([('h',10),('h',11)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

compWinPlayerLose :: State
compWinPlayerLose = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

compWinPlayerTie :: State
compWinPlayerTie = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 1 0 2 True True False False False

dealerBust :: State
dealerBust = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',6)], [('h',6)]) 10 10 1 0 2 True True False False False

compStandNoMoneyPlayerWin :: State
compStandNoMoneyPlayerWin = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False False False

playerStandNoMoneyCompWin :: State
playerStandNoMoneyCompWin = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False False False

compBustNoMoneyPlayerWin :: State
compBustNoMoneyPlayerWin = State ([('h',10),('h',11)], [('h',11),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False False True

compBustNoMoneyPlayerLose :: State
compBustNoMoneyPlayerLose = State ([('h',10),('h',9)], [('h',11),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False False True

playerBustNoMoneyCompWin :: State
playerBustNoMoneyCompWin = State ([('h',11),('h',11)], [('h',11),('h',10)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False True False

playerBustNoMoneyCompLose :: State
playerBustNoMoneyCompLose = State ([('h',11),('h',11)], [('h',9),('h',10)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False True False

bothBustBothNoMoney :: State
bothBustBothNoMoney = State ([('h',11),('h',11)], [('h',11),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 100 1 0 2 True True False True True