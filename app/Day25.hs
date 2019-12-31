
module Day25 (adventure,main) where

import Control.Monad (when,ap,liftM)
import Data.Map (Map)
import Data.Maybe (fromMaybe,fromJust)
import Data.Set (Set,(\\),union)
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified IntMachine as IM

adventure :: IO ()
adventure = do
  prog <- IM.loadFile "/home/nic/code/advent/input/day25.input"
  interact (map Char.chr . IM.exec prog . map Char.ord)

main :: IO ()
main = do
  prog <- IM.loadFile "/home/nic/code/advent/input/day25.input"
  password <- runPlan prog thePlan
  putStrLn $ "day25, part1 = " <> show (check password 319815680)

thePlan :: Plan Int
thePlan = do
  --Verbose
  exploreEverywhere
  IO $ putStrLn $ "**everywhere explored"
  --Quiet
  stuff <- collectGoodStuff
  IO $ putStrLn $ "**everything collected: " <> show stuff
  moveToLoc securityCheckpoint
  Execute Inv
  passSecurity stuff
  getPassword

exploreEverywhere :: Plan ()
exploreEverywhere = do
  showUnexplored
  tryPickLocalUnexplored >>= \case
    Just door -> do
      Execute (Move door)
      exploreEverywhere
    Nothing -> do
      showPaths
      planPathToSomewhereWithUnexplored >>= \case
        Just (path,loc) -> do
          travelPath path
          assertLocation loc
          exploreEverywhere
        Nothing -> return ()


planPathToSomewhereWithUnexplored :: Plan (Maybe (Path,Loc))
planPathToSomewhereWithUnexplored = do
  frontiers <- getPaths
  unexplored <- Set.map (\(From (loc,_)) -> loc) <$> findUnexplored <$> GetState
  let notyet fr = null (Set.fromList (Map.keys fr) `Set.intersection` unexplored)
  return $ case dropWhile notyet frontiers of
    frontier:_ ->
      Just $ head [ (path,loc) | (loc,path) <- Map.toList frontier
                                        , loc `Set.member` unexplored ]
    [] -> Nothing


collectGoodStuff :: Plan [Item]
collectGoodStuff = do
  State{things} <- GetState
  let locatedItems =
        [ (item,loc) | (loc,items) <- Map.toList things
                     , item <- Set.toList items
                     , item `notElem` badItems
                     ]
  mapM_ collect locatedItems
  return $ map fst locatedItems
  where
    badItems :: [Item]
    badItems = map Item ["escape pod","infinite loop","photons"
                        ,"giant electromagnet" ,"molten lava"]

collect :: (Item,Loc) -> Plan ()
collect (item,loc) = do
  IO $ putStrLn $ "**collect "<> show (item,loc)
  moveToLoc loc
  Execute (Take item)


showUnexplored :: Plan ()
showUnexplored = do
  unexplored <- findUnexplored_all <$> GetState
  IO $ do
    putStrLn "unexplored:"
    mapM_ (\(From (Loc loc,door)) ->
             putStrLn $ "- " <> loc <> "/" <> show door) unexplored

showPaths :: Plan ()
showPaths = do
  paths <- getPaths
  IO $ do
    putStrLn $ "paths: "
    mapM_ print (zip [0::Int ..] paths)


passSecurity :: [Item] -> Plan ()
passSecurity stuff = do
  dropItems stuff
  tryAlts (subsets stuff)
  where
    tryAlts = \case
      [] -> error "run out of alts"
      items:alts -> do
        takeItems items
        Execute Inv
        Execute (Move E)
        loc <- GetLocation
        if loc /= securityCheckpoint then return () else do
          dropItems items
          tryAlts alts

    subsets :: [a] -> [[a]]
    subsets = \case
      [] -> [[]]
      x:xs -> map (x:) (subsets xs) ++ subsets xs

    dropItems = mapM_ (Execute . Drop)
    takeItems = mapM_ (Execute . Take)


getPassword :: Plan Int
getPassword = do
  State{lastResponse=Response{full}} <- GetState
  return $ read $ head $ drop 1 $ dropWhile (/="typing") (words full)


moveToLoc :: Loc -> Plan ()
moveToLoc loc = do
  IO $ putStrLn $ "**moveToLoc "<> show loc
  path <- pathToLoc loc
  travelPath path
  assertLocation loc

travelPath :: Path -> Plan ()
travelPath path = do
  IO $ putStrLn $ "**travelPath: " <> show path
  mapM_ (Execute . Move) path

assertLocation :: Loc -> Plan ()
assertLocation loc' = do
  State{loc} <- GetState
  if loc == loc' then return () else
    error $ "assertLocation(Actual,Expected): " <> show (loc,loc')

pathToLoc :: Loc -> Plan Path
pathToLoc there = do
  frontiers <- getPaths
  let notyet fr = there `Set.notMember` Set.fromList (Map.keys fr)
  let frontier:_ = dropWhile notyet frontiers
  return $ fromJust $ Map.lookup there frontier


type Path = [Door]
type Frontier = Map Loc Path

getPaths :: Plan [Frontier]
getPaths = do
  loc <- GetLocation
  state <- GetState
  return $ findPaths state loc

findPaths :: State -> Loc -> [Frontier]
findPaths State{links,exits} start =
  bfs (Map.singleton start []) (Set.singleton start)
  where
    bfs :: Frontier -> Set Loc -> [Frontier]
    bfs frontier visited = frontier : do
      let frontier' :: Frontier = Map.fromList
            [ (loc',path++[door])
            | (loc,path) <- Map.toList frontier
            , door <- Set.toList (fromMaybe Set.empty $ Map.lookup loc exits)
            , Just loc' <- return $ Map.lookup (From (loc,door)) links
            , loc' `Set.notMember` visited
            ]
      let new = Set.fromList (Map.keys frontier')
      let visited' = visited `union` new
      if null new then [] else bfs frontier' visited'


tryPickLocalUnexplored :: Plan (Maybe Door)
tryPickLocalUnexplored = do
  loc <- GetLocation
  state <- GetState
  let doors = untakenDoor state loc
  return $ if Set.null doors then Nothing else Just $ Set.findMin doors

untakenDoor :: State -> Loc -> Set Door
untakenDoor state loc =
  Set.fromList [ door | From(loc',door) <- Set.toList $ findUnexplored state, loc==loc' ]

findUnexplored :: State -> Set From
findUnexplored state =
  Set.filter (not . special) $ findUnexplored_all state
  where
    special = \case
      From (loc,E) -> loc == securityCheckpoint
      _ -> False

securityCheckpoint :: Loc
securityCheckpoint = Loc "== Security Checkpoint =="

findUnexplored_all :: State -> Set From
findUnexplored_all State{links,exits} = do
  let all = Set.fromList
        [ From (loc,door)
        | (loc,doors) <- Map.toList exits
        , door <- Set.toList doors ]
  let explored = Set.fromList (Map.keys links)
  all \\ explored


instance Functor Plan where fmap = liftM
instance Applicative Plan where pure = return; (<*>) = ap
instance Monad Plan where return = Ret; (>>=) = Bind

data Plan a where
  Ret :: a -> Plan a
  Bind :: Plan a -> (a -> Plan b) -> Plan b
  GetState :: Plan State
  GetLocation :: Plan Loc
  IO :: IO () -> Plan ()
  Quiet :: Plan ()
  Verbose :: Plan ()
  Execute :: Command -> Plan ()

runPlan :: IM.Prog -> Plan a -> IO a
runPlan prog plan = do
  let ci@(CI resp _) = execCI prog
  state <- showUp False state0 resp
  (_,_,_,_,a) <- loop False 1 ci state plan
  return a
  where
    loop :: Bool -> Int -> CI -> State -> Plan a -> IO (Bool,Int,CI,State,a)
    loop b i ci s = \case
      Ret a -> return (b,i,ci,s,a)
      Bind p f -> do (b,i,ci,s,a) <- loop b i ci s p; loop b i ci s (f a)
      GetState -> return (b,i,ci,s,s)
      GetLocation -> let State{loc} = s in return (b,i,ci,s,loc)
      IO io -> do a <- when b io; return (b,i,ci,s,a)
      Quiet -> return (False,i,ci,s,())
      Verbose -> return (True,i,ci,s,())
      Execute com -> do
        when b $ putStrLn $ "[" <> show i <> "] " <> show com
        let State{loc} = s
        let (CI _ f) = ci
        let ci'@(CI resp' _) = f com
        let s1 = case (com,resp') of
              (Move door,Response{locMaybe=Just loc'}) -> do
                s { links = Map.insert (From (loc,door)) loc' (links s) }
              _ -> s
        s' <- showUp b s1 resp'
        return (b,i+1,ci',s',())

    showUp :: Bool -> State -> Response -> IO State
    showUp b state resp@Response{full=_full} = do
      let state' = updateState state resp
      when b $ putStrLn _full
      return state'


data State = State
  { loc :: Loc
  , lastResponse :: Response
  , links :: Map From Loc
  , exits :: Map Loc (Set Door)
  , things :: Map Loc (Set Item)
  } deriving Show

newtype From = From (Loc,Door) deriving (Eq,Ord,Show)

state0 :: State
state0 = State
  { loc = error "state0,loc"
  , lastResponse = error "state0,lastResponse"
  , links = Map.empty
  , exits = Map.empty
  , things = Map.empty
  }

updateState :: State -> Response -> State
updateState state@State{links,exits,things} resp@Response{locMaybe,doors,items} = do
  case locMaybe of
    Nothing -> state
    Just loc ->
      State
      { loc
      , lastResponse = resp
      , links
      , exits = Map.insert loc (Set.fromList doors) exits
      , things = Map.insert loc (Set.fromList items) things
      }

data CI = CI Response (Command -> CI)

data Command
  = Inv
  | Move Door
  | Take Item
  | Drop Item

instance Show Command where
  show = \case
    Inv -> "inv"
    Move door -> show door
    Take (Item i) -> "take " <> i
    Drop (Item i) -> "drop " <> i

data Response = Response
  { full :: String
  , locMaybe :: Maybe Loc
  , doors :: [Door]
  , items :: [Item] } deriving Show

data Door = N | S | E | W deriving (Eq,Ord)

instance Show Door where
  show = \case N -> "north"; S -> "south"; E -> "east"; W -> "west"

newtype Item = Item String deriving (Eq,Ord,Show)
newtype Loc = Loc String deriving (Eq,Ord,Show)

mkResponse :: String -> Response
mkResponse full = loop resp0 (lines full) where
  resp0 = Response { full, locMaybe = Nothing, doors = [], items = [] }

  loop resp = \case
    [] -> resp
    s@('=':'=':_):xs -> loop (resp { locMaybe = Just $ Loc s }) xs
    "Doors here lead:":xs -> collectDoors resp xs
    "Items here:":xs -> collectItems resp xs
    _:xs ->
      loop resp xs

  collectDoors resp@Response{doors} = \case
    ("- north"):xs -> collectDoors (resp {doors = N : doors }) xs
    ("- south"):xs -> collectDoors (resp {doors = S : doors }) xs
    ("- east"):xs -> collectDoors (resp {doors = E : doors }) xs
    ("- west"):xs -> collectDoors (resp {doors = W : doors }) xs
    xs -> loop resp xs

  collectItems resp@Response{items} = \case
    ('-':' ':item):xs -> collectItems (resp {items = Item item : items }) xs
    xs -> loop resp xs


execCI :: IM.Prog -> CI
execCI prog = loop (IM.execP prog)
  where
    loop :: IM.Process -> CI
    loop = collectOutput ""

    collectOutput :: String -> IM.Process -> CI
    collectOutput acc = \case
      IM.Halt -> CI (mkResponse (reverse acc)) (error "collectOutput, Halt")
      IM.Internal _ p -> collectOutput acc p
      IM.Output int p -> collectOutput (Char.chr int : acc) p
      p@(IM.Input _) ->
        CI (mkResponse (reverse acc)) $ \com -> feedLine (show com<>"\n") p

    feedLine :: String -> IM.Process -> CI
    feedLine = \case
      "" -> loop
      line@(char:rest) -> \case
        IM.Halt -> error "feedLine,halt"
        IM.Output _ _ -> error "output while feedling input line"
        IM.Internal _ p -> feedLine line p
        IM.Input f -> feedLine rest (f (Char.ord char))


check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))
