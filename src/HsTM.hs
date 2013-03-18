{-|
Haskell Turing Machine

Author: Tiago Ferreira
Version: 1.0
|-}

module TuringMachine where

data Movement = L | R | E deriving (Show, Eq)

-- Syntatic Sugar Types
type State           = Int
type InitialState    = Int
type FinalState      = Int
type SetState        = [Int]
type Alphabet        = [Char]
type TapeAlphabet    = [Char]
type Tape            = [Char]
type Config          = (Tape,State,Tape) 

-- delta function types 
type DeltaInput         = (Char,State)
type DeltaOutput        = (Char,State,Movement)
type DeltaFunction      = DeltaInput -> Maybe DeltaOutput

-- Turing Machine type
type TuringMachine = (  SetState,
                        Alphabet,
                        TapeAlphabet,
                        DeltaFunction,
                        InitialState,
                        FinalState)



-- << Functions definitions >>

execTM       :: TuringMachine -> Config -> IO() 
execMovTM    :: Config -> DeltaOutput  -> Maybe Config

execRightMov      :: Config -> State -> Char -> Maybe Config
execLeftMov       :: Config -> State -> Char -> Maybe Config
execEqualMov      :: Config -> State -> Char -> Maybe Config


-- << Functions Body >>


execRightMov (u,_,v:[]) ns nc    = Just (nc:u, ns, ['B'])
execRightMov (u,_,v:vs) ns nc    = Just (nc:u, ns, vs   ) 

execLeftMov  ([],_,_) _ _        = Nothing 
execLeftMov  (u:us,_,v:vs) ns nc = Just (us, ns, u:(nc:vs))

execEqualMov (u,_,v:vs) ns nc    = Just (u,ns,nc:vs)

-- Choose witch movement to perform
execMovTM x (c,s,R) = execRightMov x s c
execMovTM x (c,s,L) = execLeftMov x s c
execMovTM x (c,s,E) = execEqualMov x s c


jAux (Just x) = x

validateMov :: TuringMachine -> Maybe Config -> IO ()
validateMov m Nothing  = putStrLn "Invalid Movement"
validateMov m (Just x) = do
                         putStrLn (show x)
                         execTM m x

validateTrans :: TuringMachine -> Config -> Maybe DeltaOutput -> IO ()
validateTrans m (u,s,v) Nothing    = putStrLn ("No Valid Transitions from: " ++ (show (u,s,v)))
validateTrans m (u,s,v) (Just x)   = do
                                     putStr ((show (head v,s))++"->"++(show x)++" = ")
                                     validateMov m (execMovTM (u,s,v) x)

execTM (q,a,g,delta,i,f) (u,s,v)  | (s==f)       = do
                                                   putStrLn "Execution Ended Successfully"
                                                   putStrLn ("Final Configuration: "++(show (u,s,v)))
                                  | otherwise    = validateTrans (q,a,g,delta,i,f) (u,s,v) (delta (head v,s))


exampleDF :: DeltaFunction
exampleDF x | ('a',1) == x = Just ('b',2,R)
            | ('B',2) == x = Just ('b',1,L)
            | ('b',1) == x = Just ('B',3,E)
            | otherwise = Nothing
                                     
exampleTM = ([1,2,3],"ab","abB",exampleDF,1,3)

exampleInitialConfig :: Config
exampleInitialConfig = ([],1,"a")

execExample :: IO()
execExample = execTM exampleTM ([],1,"a")          
                                            
