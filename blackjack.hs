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

-- Interal State, Player's Bet, Computer's Bet, count, turn (player = 0, comp = 1, dealer = 2), Player stand, comp stand, dealer stand
data State = State InternalState Double Double Double Double Double Double Double Double Double
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
blackjack Hit (State (playerHand, computerHand, dealerHand, firstCard:tailDeck) bet cbet count turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 && dStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 && pStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet cbet (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand == 1 && cStand == 1 = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet (updateCount firstCard count) 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet (updateCount firstCard count) 1 pStand cStand dStand pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet cbet (updateCount firstCard count) 0 pStand cStand dStand pBust cBust)

-- Stand function that sets the state to show that the player for that turn has stood, will return EndofGame state if all players have stood
blackjack Stand (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 && dStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust cBust) 3
    | turn == 0 && cStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 2 1 cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 1 1 cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 && pStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust cBust) 3
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 0 pStand 1 dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 1 pStand 1 dStand pBust cBust)
    | turn == 2 && pStand == 1 && cStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust cBust) 3
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 1 pStand cStand 1 pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count 0 pStand cStand 1 pBust cBust)

-- checks if any of the hands have busted, if both the player and the computer have bust then EndofGame state will be returned, same if Dealer busts.
-- Else continue game and update state to show if the current turn player has busted or not
checkBust :: State -> Result
checkBust (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust)
    | getHandValue dealerHand > 21 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust cBust) 2
    | getHandValue playerHand > 21 && cBust == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand 1 cBust) 4
    | getHandValue computerHand > 21 && pBust == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust 1) 4
    | getHandValue playerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn 1 cStand dStand 1 cBust)
    | getHandValue computerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand 1 dStand pBust 1)
    | otherwise = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust)

-- updates the card counting value
updateCount :: (Ord a1, Num a1, Fractional a2) => (a3, a1) -> a2 -> a2
updateCount (x, y) cur_count = do
    if y > 9 || y == 1 then
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
    deposit <- getNumber
    play blackjack (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0 0) (deposit, deposit)
    
-- starts a round of blackjack
play :: Game -> State -> Balances-> IO Balances
play game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "How much you betting?"
    newbet <- getBet x
    let newcbet = (1 + ((count * 52) / fromIntegral(length deck))) * (y / 1000)
    let (ContinueGame state2) = game Hit state
    let (ContinueGame state3) = game Hit state2
    let (ContinueGame state4) = game Hit state3
    let (ContinueGame state5) = game Hit state4
    let (ContinueGame state6) = game Hit state5
    let (ContinueGame state7) = game Hit state6
    let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state7 in do
        personPlay game (ContinueGame (State (playerHand, computerHand, dealerHand, deck) newbet newcbet count 0 0 0 0 0 0)) (x,y)

-- if player chooses to hit the run Hit action of stand then run Stand action
personPlay :: Game -> Result -> Balances -> IO Balances
personPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Cards: " ++ show playerHand)
    putStrLn ("Computer's Cards: " ++ show computerHand)
    putStrLn ("Dealer's Cards: " ++ show dealerHand)
    putStrLn "\nChoose whether to [h]it or [s]tand:"
    line <- getLine
    if line == "h" then 
        if cStand == 1 && dStand == 1 then
            personPlay game (game Hit state) balances
        else if cStand == 1 then
            dealerPlay game (game Hit state) balances
        else 
            computerPlay game (game Hit state) balances
    else if line == "s" then
        if cStand == 1 && dStand == 1 then
            personPlay game (game Stand state) balances
        else if cStand == 1 then
            dealerPlay game (game Stand state) balances
        else 
            computerPlay game (game Stand state) balances
    else do
        putStrLn "That is not a valid input."
        personPlay game (ContinueGame state) balances

-- If the person playing gets an EndOfGame state then send the Result to the dealer to deal with
personPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    dealerPlay game (EndOfGame state lost) (x,y)


shouldStand :: (Ord a1, Ord a2, Num a3, Num a1, Num a2, Eq a3) => a3 -> a1 -> a2 -> Bool
shouldStand aces handVal upCard = do
    if aces == 0 then do
        if handVal > 16 || (handVal > 12 && upCard > 7) || (handVal == 12 && (upCard == 4 || upCard == 5 || upCard == 6)) then do
            True
        else do
            False
    else do
        if handVal > 18 || (handVal == 18 && (upCard /= 9 || upCard /= 10 || upCard /= 1)) then do
           True
        else do
            False

-- Decides and plays what action the Computer should take based off of the card counting value
computerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
computerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do 
    -- putStrLn ("Your Cards " ++ show playerHand)
    -- putStrLn ("Computer's Cards" ++ show computerHand)
    -- putStrLn ("Dealer's Cards" ++ show dealerHand)

    let stand = shouldStand (foldr (\ (x,y) z -> if y == 1 then z + 1 else z) 0 computerHand) (getHandValue computerHand) (snd (last dealerHand))
    
    if stand then do
        putStrLn "\nComputer Stand"
        if pStand == 1 && dStand == 1 then
            computerPlay game (game Stand state) balances
        else if dStand == 1 then
            personPlay game (game Stand state) balances
        else 
            dealerPlay game (game Stand state) balances
    else do
        putStrLn "\nComputer Hit"
        if pStand == 1 && dStand == 1 then
            computerPlay game (game Hit state) balances
        else if dStand == 1 then
            personPlay game (game Hit state) balances
        else 
            dealerPlay game (game Hit state) balances

-- If the computer playing gets an EndOfGame state then send the Result to the dealer to deal with
computerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    dealerPlay game (EndOfGame state lost) (x,y)

-- Decides and plays what action the Dealer should take, (Hit if hand value < 17, Stand if else)
dealerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
dealerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do 
    -- putStrLn ("Your Cards " ++ show playerHand)
    -- putStrLn ("Computer's Cards" ++ show computerHand)
    -- putStrLn ("Dealer's Cards" ++ show dealerHand)
    if getHandValue dealerHand < 17 then do
        putStrLn "\nDealer Hit"
        if pStand == 1 && cStand == 1 then
            dealerPlay game (game Hit state) balances
        else if pStand == 1 then
            computerPlay game (game Hit state) balances
        else 
            personPlay game (game Hit state) balances
    else do
        putStrLn "\nDealer Stand"
        if pStand == 1 && cStand == 1 then
            dealerPlay game (game Stand state) balances
        else if pStand == 1 then
            computerPlay game (game Stand state) balances
        else 
            personPlay game (game Stand state) balances

-- When Dealer gets EndOfGame state update the appropriate values in Balances depending on who won the round
dealerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("\nYour Cards: " ++ show playerHand)
    putStrLn ("Computer's Cards: " ++ show computerHand)
    putStrLn ("Dealer's Cards: " ++ show dealerHand)
    if lost == 2 then do 
        if pBust == 1 then do
            putStrLn "\nPlayer Busted"
            putStrLn "Dealer Bust\n"
            if checkIfOutOfMoney (x - bet) then do
                playerOutOfMoneyStatement (x - bet, y)
            else do 
                play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x - bet, y + cbet)
        else if cBust == 1 then do
            putStrLn "\nComputer Busted"
            putStrLn "Dealer Bust\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else do 
                play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x + bet, y - cbet)  
        else do 
            putStrLn "\nDealer Bust\n"
            play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x + bet, y + cbet) 
    else if pBust == 1 && cBust == 1 then do
        putStrLn "\nPlayer and Computer Bust, Dealer Wins"
        if checkIfOutOfMoney (y - cbet) && checkIfOutOfMoney (x - bet) then do
            bothOutOfMoneyStatement (x - bet, y - cbet)
        else if checkIfOutOfMoney (y - cbet) then do
            computerOutOfMoneyStatement (x - bet, y - cbet)
        else if checkIfOutOfMoney (x - bet) then do 
            playerOutOfMoneyStatement (x - bet, y - bet)
        else do
            play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x - bet, y - cbet)
    else if pBust == 1 then do
        putStrLn "\nPlayer Bust"
        if getHandValue computerHand > getHandValue dealerHand then do
            putStrLn "Computer beat Dealer\n"
            if checkIfOutOfMoney (x - bet) then do
                playerOutOfMoneyStatement (x - bet, y)
            else do 
                play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x - bet, y + cbet)
        else if getHandValue computerHand < getHandValue dealerHand then do
            putStrLn "Dealer beat Computer\n"
            if checkIfOutOfMoney (y - cbet) && checkIfOutOfMoney (x - bet) then do
                bothOutOfMoneyStatement (x - bet, y - cbet)
            else if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else if checkIfOutOfMoney (x - bet) then do 
                playerOutOfMoneyStatement (x - bet, y)
            else do
                play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x - bet, y - cbet)
            else do
                putStrLn "Computer tied Dealer\n"
                if checkIfOutOfMoney (x - bet) then do
                    playerOutOfMoneyStatement (x - bet, y)
                else do 
                    play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x - bet, y)
    else if cBust == 1 then do 
        putStrLn "\nComputer Bust"
        if getHandValue playerHand > getHandValue dealerHand then do
            putStrLn "Player beat Dealer\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else do 
                play game (State ([], [], [], deck) 0 (x + bet) 0 0 count 0 0 0 0) (x + bet, y - cbet) 
        else if getHandValue playerHand < getHandValue dealerHand then do
            putStrLn "Dealer beat Player\n"
            if checkIfOutOfMoney (y - cbet) && checkIfOutOfMoney (x - bet) then do
                bothOutOfMoneyStatement (x - bet , y - cbet)
            else if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else if checkIfOutOfMoney (x - bet) then do 
                playerOutOfMoneyStatement (x - bet, y)
            else do
                play game (State ([], [], [], deck) 0 (x - bet) 0 0 count 0 0 0 0) (x - bet, y - cbet)
        else do
            putStrLn "Player tied Dealer\n"
            if checkIfOutOfMoney (y - cbet) then do
                computerOutOfMoneyStatement (x, y - cbet)
            else do 
                play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x, y - cbet)
    else do
        winnerCheckerIfAllStand game state (x,y)

-- If the player and the computer stood, see who won the round depending on hand values
winnerCheckerIfAllStand :: (Action -> State -> Result) -> State -> Balances -> IO Balances
winnerCheckerIfAllStand game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    let playerValue = getHandValue playerHand
    let dealerValue = getHandValue dealerHand
    if  playerValue > dealerValue then do
        putStrLn "\nPlayer beat Dealer"
        checkComputerWinPlayerWin game state (x + bet,y)
    else if playerValue < dealerValue then do
        putStrLn "\nDealer beat Player"
        checkComputerWinPlayerWin game state (x - bet,y)
    else do
        putStrLn "\nPlayer tied Dealer"
        checkComputerWinPlayerWin game state (x,y)

-- if the computer won check if the player won as well
checkComputerWinPlayerWin :: (Action -> State -> Result) -> State -> Balances -> IO Balances
checkComputerWinPlayerWin game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet cbet count turn pStand cStand dStand pBust cBust) = state in do
    let computerValue = getHandValue computerHand
    let dealerValue = getHandValue dealerHand
    if computerValue > dealerValue then do 
        putStrLn "Computer beat Dealer\n"
        if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x, y + bet)
    else if computerValue == dealerValue then do 
        putStrLn "Computer tied Dealer\n"
        if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x, y)
    else do
        putStrLn "Dealer beat Computer"
        if checkIfOutOfMoney x then do
            playerOutOfMoneyStatement (x,y)
        else if checkIfOutOfMoney (y - cbet) then do 
            computerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], deck) 0 0 count 0 0 0 0 0 0) (x, y - cbet)

-- check if the balance is 0 or less
checkIfOutOfMoney :: Double -> Bool
checkIfOutOfMoney balance 
    | balance <= 0 = True
    | otherwise = False

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

-- get the player input for a number and redo if the input is not a valid number
getNumber :: IO Double
getNumber = do 
    balance <- getLine
    case readMaybe balance :: Maybe Double of
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


-- Test Cases --

-- dealerPlay blackjack (Continue `state`) (100,100)
allTie :: State
allTie = State ([('h',10),('h',10)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

allLose :: State
allLose = State ([('h',10),('h',9)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

playerWinCompWin :: State
playerWinCompWin = State ([('h',10),('h',11)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

playerWinCompLose :: State
playerWinCompLose = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

playerWinCompTie :: State
playerWinCompTie = State ([('h',10),('h',11)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

compWinPlayerLose :: State
compWinPlayerLose = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

compWinPlayerTie :: State
compWinPlayerTie = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 10 0 2 1 1 0 0 0

dealerBust :: State
dealerBust = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',6)], [('h',6)]) 10 10 0 2 1 1 0 0 0

computerNoMoney :: State
computerNoMoney = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 100 100 0 2 1 1 0 0 0

playerNoMoney :: State
playerNoMoney = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 100 0 2 1 1 0 0 0

-- computerPlay blackjack (Continue `state`) (100,100)
compBustDealerBust :: State
compBustDealerBust = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',7)], [('h',6), ('h',6)]) 10 100 0 1 0 0 0 0 0