import  Data.List
import  Data.Ord
import  Data.Char
import  Control.Monad
import  System.IO

data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show,Ord,Eq,Enum)

data Hand = 
    HighCard Rank
  | OnePair Rank Rank
  | TwoPairs Rank Rank Rank
  | ThreeOfAKind Rank
  | Straight Rank
  | Flush Rank
  | FullHouse Rank
  | FourOfAKind Rank
  | StraightFlush Rank
  | FiveOfAKind Rank
  deriving (Show,Ord,Eq)

data Suit = Diamond | Spade | Heart | Club
  deriving (Show,Eq,Enum)

data Card = Card Suit Rank | Joker
  deriving (Show,Eq)

replaceAce []                      = []
replaceAce ((Card suit Ace):cards) = ((Card suit One):cards)
replaceAce (card:cards)            = (card:replaceAce cards) 

replaceJoker []            _       = []
replaceJoker (Joker:cards) newCard = (newCard:cards)
replaceJoker (card:cards)  newCard = (card:(replaceJoker cards newCard))


rankOf (Joker)          = Ace
rankOf (Card suit rank) = rank
suitOf (Joker)          = Diamond
suitOf (Card suit rank) = suit

strongestHand cards
  | hasJoker = case hasFourOfAKind of
      True -> FiveOfAKind topRank
      _    -> maximum [ strongestHand set |
          rank <- [ One .. Ace ],
          suit <- [ Diamond .. Spade ],
          let set = replaceJoker cards (Card suit rank),
          set /= cards
        ]
  | hasAce = maximum [
      currentStrongestHand,
      (strongestHand $ replaceAce cards)
    ]
  | otherwise  = currentStrongestHand
    where
      hasJoker = Joker `elem` cards
      hasAce   = Ace   `elem` (map rankOf cards)
      topRank  = maximum $ map rankOf withoutJoker

      ranks        = sort $ map rankOf cards
      maxKind      = maximum (map length $ group ranks) 

      withoutJoker    = filter (/= Joker) cards
      hasStraight     = and . zipWith ((==) . succ) ranks $ tail ranks
      hasFlush        = null . tail . nub $ map suitOf cards
      hasFourOfAKind  = maxKind == 4
      hasThreeOfAKind = maxKind == 3
      hasFullHouse    = hasThreeOfAKind && length pairs > 0
      nGroupedRanks n = filter ((== n) . length) (group ranks)
      rankThreeOfAKind = head .   concat $ nGroupedRanks 3
      hiCard          = maximum . concat $ nGroupedRanks 1
      pairs           = map head $ nGroupedRanks 2

      currentStrongestHand
        | hasStraight && hasFlush = StraightFlush topRank
        | hasFourOfAKind          = FourOfAKind   topRank
        | hasFullHouse            = FullHouse     rankThreeOfAKind
        | hasFlush                = Flush         topRank
        | hasStraight             = Straight      topRank
        | hasThreeOfAKind         = ThreeOfAKind  rankThreeOfAKind
        | length pairs == 2       = TwoPairs (last pairs) (head pairs) hiCard
        | length pairs == 1       = OnePair  (head pairs) hiCard
        | otherwise               = HighCard hiCard

stringToRank "A" = Ace
stringToRank "J" = Jack
stringToRank "T" = Ten
stringToRank "Q" = Queen
stringToRank "K" = King
stringToRank (s:[]) | isDigit s  = toEnum $ digitToInt s - 1
                    | otherwise  = One
stringToRank _   = One

stringToSuit "D" = Diamond
stringToSuit "H" = Heart
stringToSuit "C" = Club
stringToSuit "S" = Spade
stringToSuit  _  = Spade

stringToCard "JJ"     = Joker
stringToCard (r:s:[]) = Card (stringToSuit (s:[])) (stringToRank (r:[]))
stringToCard _        = Card Diamond One

toPlayerHands string = ((player1,p1s),(player2,p2s))
  where
    cards = words string
    hands = strongestHand . map stringToCard
    (p1s:p2s:[])         = [take 5 cards, drop 5 cards]
    (player1:player2:[]) = map hands (p1s:p2s:[])



doPlay string = ((p1,p1s),cp,(p2,p2s))
  where ((p1,p1s),(p2,p2s)) = toPlayerHands string
        cp | p1 <  p2  = "<"
           | p1 == p2  = "="
           | otherwise = ">"

main = do
  string <- readFile "p054_poker.txt"
  let games = map doPlay (lines string)
  mapM_ (putStrLn . show) games

  putStrLn . show . length . filter (\(_,cp,_)-> cp == ">") $ games

