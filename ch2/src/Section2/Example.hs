{-# LANGUAGE ViewPatterns, NamedFieldPuns, RecordWildCards #-}
module Chapter2.Section2.Example where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data ClientsByGender = ClientsByGender Gender Int deriving Show

data Period = Past | Future deriving Show
data TimeMachine = TimeMachine String Int String Period Double deriving Show

{-
  Records on field names must have different types:
    1. each field name is used to define an accessor function
    2. thus, all fields must be unique unless they are using on different value constructors of the same type constructor
      in which case they must be the same type
-}

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

{-
   Practical use of record types for providing options

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data Timeout = NoTimeout | Timeout Integer

data ConnOptions = ConnOptions {
                                 connType      :: ConnType,
                                 connSpeed     :: Integer,
                                 connProxy     :: UseProxy,
                                 connCaching   :: Bool,
                                 connKeepAlive :: Bool,
                                 connTimeout   :: Timeout
                               }


The definiton of Connection is not displayed here

connect :: String -> ConnOptions -> Connection

connect "http://apress.com" connDefault 
connect "http://apress.com" connDefault { connType = UDP }

-}

addOne :: Int -> Int
addOne x = x + 1

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty xs = if null xs then "empty" else head xs

lst1 +++ lst2 = if null lst1
                   then lst2
                   else (head lst1) : (tail lst1 +++ lst2)

[] ++++ lst2 = lst2
(x:xs) ++++ lst2 = x : (xs ++++ lst2)

reverse2 list = if null list
                   then []
                   else reverse2 t +++ [h]
                     where
                       h = head list
                       t = tail list

{- Haskell allows grouping of where blocks seperated within {} with each expression seperated by ; -}
maxmin x = if no_tail x then (h, h) else (max h (max_tail x), min h (min_tail x))
  where
    h = head x
    max_tail = fst . maxmin . tail
    min_tail = snd . maxmin . tail
    no_tail = null . tail


clientName client = case client of
                      GovOrg  name       -> name
                      Company name _ _ _ -> name
                      Individual p _     ->
                        case p of
                          Person fname lname _ -> fname ++ " " ++ lname

clientName' (GovOrg name) = name
clientName' (Company name _ _ _)  = name
clientName' (Individual (Person fname lname _) _) = fname ++ " " ++ lname

incMale ((ClientsByGender _ num):xs) = ClientsByGender Male (num + 1) : xs
incFemale (male:(ClientsByGender _ num):xxs) = male: (ClientsByGender Female (num + 1)) : xxs
incUnknown (male:female:(ClientsByGender _ num):[]) = male : female : (ClientsByGender Unknown (num + 1)):[]

{- This would obviously benefit from using the Map data structure -}
genderByClient :: [ClientsByGender] -> Client -> [ClientsByGender]
genderByClient res client = case client of
                              Individual (Person _ _ Male) _    -> incMale res
                              Individual (Person _ _ Female) _  -> incFemale res
                              Individual (Person _ _ Unknown) _ -> incUnknown res
                              _                                 -> res

clientsByGender :: [Client] -> [ClientsByGender]
clientsByGender clients = foldl genderByClient [ClientsByGender Male 0, ClientsByGender Female 0, ClientsByGender Unknown 0] clients

applyDiscountToItem discount (TimeMachine man id name period price) = TimeMachine man id name period discounted_price
  where discounted_price = price - (price * (discount / 100))

applyDiscount :: Double -> [TimeMachine] -> [TimeMachine]
applyDiscount discount = map $ applyDiscountToItem discount

sorted :: Ord a => [a] -> Bool
sorted []  = True
sorted [_] = True
sorted (x:(r@(y:_))) = x < y && sorted r

ifibonnaci :: Int -> Maybe Int
ifibonnaci n | n < 0 = Nothing
ifibonnaci 0 = Just 0
ifibonnaci 1 = Just 1
ifibonnaci n = Just (f1 + f2)
  where
    Just f1 = ifibonnaci (n - 1)
    Just f2 = ifibonnaci(n - 2)

binom :: Int -> Int -> Int
binom _ 0 = 1
binom n k | n == k = 1
binom n k = binom (n - 1) (k - 1) + binom (n - 1) k

{- Any expression returning a boolean expression can be used as a guard -}

ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _ = False

greet :: ClientR -> String
greet IndividualR { person = PersonR { fname, lname } } = "Welcome " ++ fname ++ " " ++ lname
greet CompanyR { clientNameR } = "Welcome " ++ clientNameR
greet _ = "Greetings"

greet' :: ClientR -> String
greet' IndividualR { person = PersonR { .. } } = "Hi " ++ fname
greet' CompanyR { .. } = "Hello " ++ clientNameR
greet'  _ = "Greetings"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { fname = initial:rest })= p { fname = (toUpper initial):rest }
nameInCapitals p@(PersonR { fname = "" })= p

applyDiscountToItem' :: Double -> TimeMachineR -> TimeMachineR
applyDiscountToItem' discount t@(TimeMachineR { price }) = t { price = discounted_price }
  where discounted_price = price - (price * (discount / 100))

applyDiscount' :: Double -> [TimeMachineR] -> [TimeMachineR]
applyDiscount' discount = map $ applyDiscountToItem' discount

