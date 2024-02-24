import Text.Read (readMaybe)

type Game = Action -> State -> Result

data Result = EndOfGame State Double
            | ContinueGame State
            deriving (Eq, Show)

-- Interal State, Players Bet, Players Balance, turn (player = 0, comp = 1, dealer = 2), Player stand, comp stand, dealer stand
data State = State InternalState Double Double Double Double Double Double
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
blackjack Hit (State (playerHand, computerHand, dealerHand, firstCard:tailDeck) bet balance turn pStand cStand dStand)
    | turn == 0 && cStand == 1 && dStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 0 pStand cStand dStand)
    | turn == 0 && cStand == 1 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 2 pStand cStand dStand)
    | turn == 0 = checkBust (State (firstCard:playerHand, computerHand, dealerHand, tailDeck) bet balance 1 pStand cStand dStand)
    | turn == 1 && dStand == 1 && pStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 1 pStand cStand dStand)
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 0 pStand cStand dStand)
    | turn == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 2 pStand cStand dStand)
    | turn == 2 && pStand == 1 && cStand == 1 = checkBust (State (playerHand, firstCard:computerHand, dealerHand, tailDeck) bet balance 2 pStand cStand dStand)
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet balance 1 pStand cStand dStand)
    | otherwise = checkBust (State (playerHand, computerHand, firstCard:dealerHand, tailDeck) bet balance 0 pStand cStand dStand)

blackjack Stand (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand)
    | turn == 0 && cStand == 1 && dStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand) 4
    | turn == 0 && cStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 2 1 cStand dStand)
    | turn == 0 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 1 cStand dStand)
    | turn == 1 && dStand == 1 && pStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand) 4
    | turn == 1 && dStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 0 pStand 1 dStand)
    | turn == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 pStand 1 dStand)
    | turn == 2 && pStand == 1 && cStand == 1 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand) 4
    | turn == 2 && pStand == 1 = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 1 pStand cStand 1)
    | otherwise = checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance 0 pStand cStand 1)
    

checkBust :: State -> Result
checkBust (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand)
    | getHandValue playerHand > 21 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand) 0
    -- | getHandValue computerHand > 21 = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand)
    -- | getHandValue dealerHand > 21 = EndOfGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand 1 dStand) 3
    | otherwise = ContinueGame (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand)

getHandValue :: (Foldable t, Num b) => t (a, b) -> b
getHandValue hand = foldr (\ (x,y) z -> y + z) 0 hand

-- copy and pasted so yeah
fullDeck :: [Card]
fullDeck = [(suit,value) | suit <- ['s','d','h','c'], value <- [1..13]]

type Balances = (Double, Double)

start :: IO Balances
start = do
    putStrLn "OMG BLACKJACK, how much do you want to deposit?"
    deposit <- getNumber
    play blackjack (State ([], [], [], fullDeck) 0 deposit 0 0 0 0) (deposit, deposit)
    

play :: Game -> State -> Balances-> IO Balances
play game state balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do
    putStrLn "How much you betting?"
    newbet <- (getBet balance)
    let (ContinueGame state2) = game Hit state
    let (ContinueGame state3) = game Hit state2
    let (ContinueGame state4) = game Hit state3
    let (ContinueGame state5) = game Hit state4
    let (ContinueGame state6) = game Hit state5
    let (ContinueGame state7) = game Hit state6
    let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state7 in do
        personPlay game (ContinueGame (State (playerHand, computerHand, dealerHand, deck) newbet balance 0 0 0 0)) balances


personPlay :: Game -> Result -> Balances -> IO Balances
personPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do
    putStrLn ("\nYour Cards: " ++ show playerHand)
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

personPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do
    if lost == 0 then do
        putStrLn "\nYou bust"
        if (x - bet) == 0 then do
            putStrLn ("You lose. The computer had " ++ show y ++ " money remaining.")
            start
        else
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (x - bet, y)
    else if lost == 2 then do
        putStrLn "\nDealer bust"
        play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (x + bet, y + bet)
    else
        if (y - bet) == 0 then do
            putStrLn ("\nYou win! You had " ++ show x ++ " money remaining.")
            start
        else
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (x, y - bet)


computerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
computerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do 
    -- putStrLn ("\nYour Cards " ++ show playerHand)
    -- putStrLn ("Computer's Cards" ++ show computerHand)
    -- putStrLn ("Dealer's Cards" ++ show dealerHand)
    putStrLn "Computer Hit"
    if pStand == 1 && dStand == 1 then
        computerPlay game (game Hit state) balances
    else if dStand == 1 then
        personPlay game (game Hit state) balances
    else 
        dealerPlay game (game Hit state) balances

computerPlay game (EndOfGame state lost) (x,y) = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do
    if lost == 0 then do
        putStrLn "\nYou bust"
        let (playerBal, compBal) = (x - bet, y)
        putStrLn ("\nPlayer Balance: " ++ show playerBal)
        putStrLn ("Computer's Balance: " ++ show compBal)

        if playerBal == 0 then do
            putStrLn "\nYou lose.\n\n"
            start
        else do
            putStrLn "\n"
            play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (playerBal, compBal)
    else if lost == 2 then do
        putStrLn "Dealer bust"
        play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (x + bet, y + bet)
    else 
        play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (x, y - bet)

dealerPlay :: (Action -> State -> Result) -> Result -> Balances -> IO Balances
dealerPlay game (ContinueGame state) balances = let (State (playerHand, computerHand, dealerHand, deck) bet balance turn pStand cStand dStand) = state in do 
    -- putStrLn ("\nYour Cards " ++ show playerHand)
    -- putStrLn ("Computer's Cards" ++ show computerHand)
    -- putStrLn ("Dealer's Cards" ++ show dealerHand)
    if getHandValue dealerHand < 17 then do
        putStrLn "Dealer Hit"
        if pStand == 1 && cStand == 1 then
            dealerPlay game (game Hit state) balances
        else if pStand == 1 then
            computerPlay game (game Hit state) balances
        else 
            personPlay game (game Hit state) balances
    else do
        putStrLn "Dealer Stand"
        dog <- getLine
        if pStand == 1 && cStand == 1 then
            dealerPlay game (game Stand state) balances
        else if pStand == 1 then
            computerPlay game (game Stand state) balances
        else 
            personPlay game (game Stand state) balances


dealerPlay game (EndOfGame state lost) balances = do
    play game (State ([], [], [], fullDeck) 0 0 0 0 0 0) (0, 0)
    
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
yep = State ([('h',5),('h',10)], [], [], [('h',6)]) 10 1000 2 0 0 0