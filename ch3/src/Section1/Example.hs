{-# LANGUAGE LambdaCase #-}

module Section1.Example where

{-
  Type parameters can be:
    1. added to define polymorphic functions
    2. added to define polymorphic types which can work with values of any type
-}

data Gender = Male | Female | Unknown
  deriving Show

data PersonR = PersonR {
                 fname  :: String,
                 lname  :: String,
                 gender :: Gender
                       } deriving Show

data TimeMachineR = TimeMachineR {
                     manufacturer :: String,
                     t_id :: Int,
                     m_name :: String,
                     time_period :: Period,
                     price :: Double
                                 } deriving Show

data ClientR = GovOrgR { clientNameR :: String }
             | CompanyR {
                          clientNameR     :: String,
                          clientId            :: String,
                          contact         :: PersonR,
                          position        :: String
                        }
             | IndividualR {
                             person          :: PersonR,
                             wantsNewsletter :: Bool
                           }
             deriving Show

data Period = Past | Future deriving Show
data TimeMachine = TimeMachine String Int String Period Double deriving Show

maybeString :: Maybe t -> [Char]
maybeString (Just _) = "Has value"
maybeString (Nothing) = "Has No Value"

swapTriple :: (a,b,c) -> (b,c,a)
swapTriple (a,b,c) = (b,c,a)

duplicate :: a -> (a,a)
duplicate a = (a,a)

nothing :: t -> Maybe t
nothing _ = Nothing

index :: [t] -> [(Int, t)]
index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
                in  (n+1,x):indexed
{-
  1. lambdas can only match one case unless we use
  the GHC LambdaCase extension
-}

nextNumbers :: [Integer] -> [Integer]
nextNumbers = map succ

equalTuples :: [(Integer, Integer)] -> [(Integer, Integer)]
equalTuples = filter (\(x,y) -> x == y)

multiplyByN :: Num a => a -> (a -> a)
multiplyByN n = \x -> n * x

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (\x -> x == 1)

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber n = filter (\x -> x == n)

filterNot :: (t -> Bool) -> [t] -> [t]
filterNot f = filter (\x -> not $ f x)

filterGovOrgs = filter (\case (GovOrgR _) -> True
                              _           -> False
                       )

double :: [Integer] -> [Integer]

{-
   The symbol -> binds to the right
   so the functions a -> b -> c -> d

  a -> (b -> (c -> d)

  Each function that takes multiple  params is a function
  that takes one parameter and returns a partially applied
  function

  Partial application encourages a style of programming where params are not mentioned.
-}

double = map (*2)
