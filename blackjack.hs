import Text.Read (readMaybe)

type Game = Action -> State -> Result

data Result = EndOfGame State Double
            | ContinueGame State
            deriving (Eq, Show)

-- Interal State, Players Bet, Players Balance, turn (player = 0, comp = 1, dealer = 2), Player stand, comp stand, dealer stand
data State = State InternalState Double Double Double Double Double Double Double Double
            deriving (Eq, Show)

-- Players Cards, Computer Cards, Dealer Cards, Deck
type InternalState = ([Card], [Card], [Card], [Card])

-- (Suit, Number)
type Card = (Char, Int)

data Action = Hit
            | Stand
            deriving (Eq, Show)

-- type PlayerHand = [Card]
-- type DealerHand = [Card]
-- type ComputerHand = [Card]
-- type Deck = [Card]

blackjack :: Game
blackjack Hit (State (playerHand, computerHand, dealerHand, firstCard:tailDeck) bet balance turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 && dStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 0 pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 2 pStand cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 && pStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 1 pStand cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 0 pStand cStand dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand == 1 && cStand == 1 = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet balance 2 pStand cStand dStand pBust cBust)
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet balance 1 pStand cStand dStand pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet balance 0 pStand cStand dStand pBust cBust)

blackjack Stand (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust)
    | turn == 0 && cStand == 1 && dStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 3
    | turn == 0 && cStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 2 1 cStand dStand pBust cBust)
    | turn == 0 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 1 cStand dStand pBust cBust)
    | turn == 1 && dStand == 1 && pStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 3
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 0 pStand 1 dStand pBust cBust)
    | turn == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 pStand 1 dStand pBust cBust)
    | turn == 2 && pStand == 1 && cStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 3
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 pStand cStand 1 pBust cBust)
    | otherwise = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 0 pStand cStand 1 pBust cBust)
    

checkBust :: State -> Result
checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust)
    | getHandValue dealerHand > 21 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 2
    | getHandValue playerHand > 21 && cBust == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 4
    | getHandValue computerHand > 21 && pBust == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust cBust) 4
    | getHandValue playerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn 1 cStand dStand 1 cBust)
    | getHandValue computerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand pBust 1)
    | otherwise = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust)

getHandValue :: (Ord b, Num b, Foldable t) => t (a, b) -> b
getHandValue hand = do
    let aces = foldr (\ (x,y) z -> if y == 1 then z + 1 else z) 0 hand
    if aces > 0 then do
        getBestValue (getAceValues hand aces)
    else
        foldr (\ (x,y) z -> y + z) 0 hand

getAceValues :: (Ord t1, Num t1, Foldable t2) => t2 (a, t1) -> t1 -> [t1]
getAceValues hand aces
    | aces > 0 = (foldr (\ (x,y) z -> y + z) 0 hand) + (10 * aces) : getAceValues hand (aces - 1)
    | otherwise = [foldr (\ (x,y) z -> y + z) 0 hand]

getBestValue :: (Ord t, Num t) => [t] -> t
getBestValue (h:t)
    | length t > 0 = if h > 21 then getBestValue t else h
    | otherwise = h

-- copy and pasted so yeah
fullDeck :: [Card]
fullDeck = [(suit,value) | suit <- ['s','d','h','c'], value <- [1..13]]

type Balances = (Double, Double)

start :: IO Balances
start = do
    putStrLn "OMG BLACKJACK, how much do you want to deposit?"
    deposit <- getNumber
    play blackjack (State ([], [], [], fullDeck) 0 deposit 0 0 0 0 0 0) (deposit, deposit)
    

play :: Game -> State -> Balances-> IO Balances
play game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "How much you betting?"
    newbet <- (getBet balance)
    let (ContinueGame state2) = game Hit state
    let (ContinueGame state3) = game Hit state2
    let (ContinueGame state4) = game Hit state3
    let (ContinueGame state5) = game Hit state4
    let (ContinueGame state6) = game Hit state5
    let (ContinueGame state7) = game Hit state6
    let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state7 in do
        personPlay game (ContinueGame (State (playerHand, computerHand, dealerHand, deck) newbet balance 0 0 0 0 0 0)) (x,y)


personPlay :: Game -> Result -> Balances -> IO Balances
personPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
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

personPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    -- if lost == 0 then do
    --     putStrLn "\nYou bust"
    --     if (x - bet) == 0 then do
    --         putStrLn ("You lose. The computer had " ++ show y ++ " money remaining.")
    --         start
    --     else
    --         play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x - bet, y)
    -- else if lost == 2 then do
    --     putStrLn "\nDealer bust"
    --     play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x + bet, y + bet)
    -- else
    --     if (y - bet) == 0 then do
    --         putStrLn ("\nYou win! You had " ++ show x ++ " money remaining.")
    --         start
    --     else
    --         play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y - bet)
    dealerPlay game (EndOfGame state lost) (x,y)


computerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
computerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do 
    putStrLn ("Your Cards " ++ show playerHand)
    putStrLn ("Computer's Cards" ++ show computerHand)
    putStrLn ("Dealer's Cards" ++ show dealerHand)
    putStrLn "\nComputer Hit"
    if pStand == 1 && dStand == 1 then
        computerPlay game (game Hit state) balances
    else if dStand == 1 then
        personPlay game (game Hit state) balances
    else 
        dealerPlay game (game Hit state) balances

computerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    -- if lost == 0 then do
    --     putStrLn "\nYou bust"
    --     let (playerBal, compBal) = (x - bet, y)
    --     putStrLn ("\nPlayer Balance: " ++ show playerBal)
    --     putStrLn ("Computer's Balance: " ++ show compBal)

    --     if playerBal == 0 then do
    --         putStrLn "\nYou lose.\n\n"
    --         start
    --     else do
    --         putStrLn "\n"
    --         play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (playerBal, compBal)
    -- else if lost == 2 then do
    --     putStrLn "Dealer bust"
    --     play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x + bet, y + bet)
    -- else 
    --     play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y - bet)
    dealerPlay game (EndOfGame state lost) (x,y)

dealerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
dealerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do 
    putStrLn ("Your Cards " ++ show playerHand)
    putStrLn ("Computer's Cards" ++ show computerHand)
    putStrLn ("Dealer's Cards" ++ show dealerHand)
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
        dog <- getLine
        if pStand == 1 && cStand == 1 then
            dealerPlay game (game Stand state) balances
        else if pStand == 1 then
            computerPlay game (game Stand state) balances
        else 
            personPlay game (game Stand state) balances


dealerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    putStrLn ("Your Cards: " ++ show playerHand)
    putStrLn ("Computer's Cards: " ++ show computerHand)
    putStrLn ("Dealer's Cards: " ++ show dealerHand)
    if lost == 2 then do 
        if (pBust == 1) then do
            putStrLn "\nPlayer Busted"
            putStrLn "Dealer Bust\n"
            if (checkIfOutOfMoney (x - bet)) then do
                playerOutOfMoneyStatement (x - bet,y)
            else do 
                play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y + bet)
        else if (cBust == 1) then do
            putStrLn "\nComputer Busted"
            putStrLn "Dealer Bust\n"
            if (checkIfOutOfMoney (y - bet)) then do
                computerOutOfMoneyStatement (x,y - bet)
            else do 
                play game (State ([], [], [], fullDeck) 0 (x + bet) 0 0 0 0 0 0) (x + bet, y - bet)  
        else do 
            putStrLn "\nDealer Bust\n"
            play game (State ([], [], [], fullDeck) 0 (x + bet) 0 0 0 0 0 0) (x + bet, y + bet) 
    else if pBust == 1 && cBust == 1 then do
        putStrLn "\nPlayer and Computer Bust, Dealer Wins"
        if (checkIfOutOfMoney (y- bet) && checkIfOutOfMoney (x - bet)) then do
            bothOutOfMoneyStatement (x,y - bet)
        else if (checkIfOutOfMoney (y - bet)) then do
            computerOutOfMoneyStatement (x,y - bet)
        else if (checkIfOutOfMoney (x - bet)) then do 
            playerOutOfMoneyStatement (x - bet,y)
        else do
            play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y - bet)
    else if pBust == 1 then do
        putStrLn "\nPlayer Bust"
        if (getHandValue computerHand > getHandValue dealerHand) then do
            putStrLn "Computer beat Dealer\n"
            if (checkIfOutOfMoney (x - bet)) then do
                playerOutOfMoneyStatement (x - bet,y)
            else do 
                play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y + bet)
        else if (getHandValue computerHand < getHandValue dealerHand) then do
            putStrLn "Dealer beat Computer\n"
            if (checkIfOutOfMoney (y- bet) && checkIfOutOfMoney (x - bet)) then do
                bothOutOfMoneyStatement (x - bet, y - bet)
            else if (checkIfOutOfMoney (y - bet)) then do
                computerOutOfMoneyStatement (x,y - bet)
            else if (checkIfOutOfMoney (x - bet)) then do 
                playerOutOfMoneyStatement (x - bet,y)
            else do
                play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y - bet)
            else do
                putStrLn "Computer tied Dealer\n"
                if (checkIfOutOfMoney (x - bet)) then do
                    playerOutOfMoneyStatement (x - bet,y)
                else do 
                    play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y)
    else if cBust == 1 then do 
        putStrLn "\nComputer Bust"
        if (getHandValue playerHand > getHandValue dealerHand) then do
            putStrLn "Player beat Dealer\n"
            if (checkIfOutOfMoney (y - bet)) then do
                computerOutOfMoneyStatement (x,y - bet)
            else do 
                play game (State ([], [], [], fullDeck) 0 (x + bet) 0 0 0 0 0 0) (x + bet, y - bet) 
        else if (getHandValue playerHand < getHandValue dealerHand) then do
            putStrLn "Dealer beat Player\n"
            if (checkIfOutOfMoney (y- bet) && checkIfOutOfMoney (x - bet)) then do
                bothOutOfMoneyStatement (x - bet ,y - bet)
            else if (checkIfOutOfMoney (y - bet)) then do
                computerOutOfMoneyStatement (x,y - bet)
            else if (checkIfOutOfMoney (x - bet)) then do 
                playerOutOfMoneyStatement (x - bet,y)
            else do
                play game (State ([], [], [], fullDeck) 0 (x - bet) 0 0 0 0 0 0) (x - bet, y - bet)
        else do
            putStrLn "Player tied Dealer\n"
            if (checkIfOutOfMoney (y - bet)) then do
                computerOutOfMoneyStatement (x,y - bet)
            else do 
                play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y - bet)
    else do
        winnerChecker game state (x,y)

winnerChecker :: (Action -> State -> Result) -> State -> Balances -> IO Balances
winnerChecker game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    let playerValue = getHandValue playerHand
    let dealerValue = getHandValue dealerHand
    if  (playerValue > dealerValue) then do
        putStrLn ("\nPlayer beat Dealer")
        checkComputerWinPlayerWin game state (x + bet,y)
    else if (playerValue < dealerValue) then do
        putStrLn ("\nDealer beat Player")
        checkComputerWinPlayerWin game state (x - bet,y)
    else do
        putStrLn ("\nPlayer tied Dealer")
        checkComputerWinPlayerWin game state (x,y)

checkComputerWinPlayerWin :: (Action -> State -> Result) -> State -> Balances -> IO Balances
checkComputerWinPlayerWin game state (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand pBust cBust) = state in do
    let computerValue = getHandValue computerHand
    let dealerValue = getHandValue dealerHand
    if (computerValue > dealerValue) then do 
        putStrLn ("Computer beat Dealer\n")
        if (checkIfOutOfMoney x) then do
            playerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y + bet)
    else if (computerValue == dealerValue) then do 
        putStrLn ("Computer tied Dealer\n")
        if (checkIfOutOfMoney x) then do
            playerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y)
    else do
        putStrLn ("Dealer beat Computer")
        if (checkIfOutOfMoney x) then do
            playerOutOfMoneyStatement (x,y)
        else if (checkIfOutOfMoney (y -bet)) then do 
            computerOutOfMoneyStatement (x,y)
        else do 
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0 0 0) (x, y - bet)

checkIfOutOfMoney :: Double  -> Bool
checkIfOutOfMoney balance 
    | balance == 0 = True
    | otherwise = False

playerOutOfMoneyStatement :: Balances -> IO Balances
playerOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "You're out of money you LOSE\n"
    start 

computerOutOfMoneyStatement :: Balances -> IO Balances
computerOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "Computer out of money you WIN\n"
    start 

bothOutOfMoneyStatement :: Balances -> IO Balances
bothOutOfMoneyStatement (x,y) = do
    putStrLn ("Your Balance: " ++ show x)
    putStrLn ("Computer's Balance: " ++ show y)
    putStrLn "Both out of money you TIE\n"
    start 
    
-- this is straight stole from another project
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

getBet :: Double -> IO Double
getBet balance = do
    bet <- getNumber
    if bet > balance then do
        putStrLn "You can't bet what you don't have."
        getBet balance
    else
        return bet
    
yep :: State
yep = State ([('h',5),('h',10)], [], [], [('h',6)]) 10 1000 2 0 0 0 0 0


-- dealerPlay blackjack (Continue `state`) (100,100)
allTie :: State
allTie = State ([('h',10),('h',10)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

allLose :: State
allLose = State ([('h',10),('h',9)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

playerWinCompWin :: State
playerWinCompWin = State ([('h',10),('h',11)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

playerWinCompLose :: State
playerWinCompLose = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

playerWinCompTie :: State
playerWinCompTie = State ([('h',10),('h',11)], [('h',10),('h',10)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

compWinPlayerLose :: State
compWinPlayerLose = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

compWinPlayerTie :: State
compWinPlayerTie = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 10 1000 2 1 1 0 0 0

dealerBust :: State
dealerBust = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',6)], [('h',6)]) 10 1000 2 1 1 0 0 0

computerNoMoney :: State
computerNoMoney = State ([('h',10),('h',11)], [('h',10),('h',9)], [('h',10),('h',10)], [('h',6)]) 100 1000 2 1 1 0 0 0

playerNoMoney :: State
playerNoMoney = State ([('h',10),('h',9)], [('h',10),('h',11)], [('h',10),('h',10)], [('h',6)]) 100 1000 2 1 1 0 0 0

-- computerPlay blackjack (Continue `state`) (100,100)
compBustDealerBust :: State
compBustDealerBust = State ([('h',10),('h',10)], [('h',10),('h',11)], [('h',10),('h',6)], [('h',6), ('h',6)]) 10 1000 1 0 0 0 0 0