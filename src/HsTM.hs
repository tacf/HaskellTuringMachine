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

execTM (q,a,g,delta,i,f) (u,s,v)  = do if (s==f) then  do
                                                        putStrLn "Execution Ended Successfully"
                                                        putStrLn ("Final Configuration: "++(show (u,s,v)))
                                       else do 
                                               let x = (delta (head v, s))
                                                   m = (q,a,g,delta,i,f)
                                               if (x == Nothing) then putStrLn ("No Valid Transitions from: " ++ (show (u,s,v)))
                                               else do
                                                      let y = execMovTM (u,s,v) (jAux x)
                                                      if (y == Nothing) then putStrLn "Invalid Movement"
                                                      else do
                                                           putStrLn ((show (head v, s))++"->"++(show (jAux x))++" = "++(show (jAux y)))
                                                           execTM m (jAux y)


exampleDF :: DeltaFunction
exampleDF x | ('a',1) == x = Just ('b',2,R)
            | ('B',2) == x = Just ('b',1,L)
            | ('b',1) == x = Just ('B',3,E)
            | otherwise = Nothing
                                     
exampleTM = ([1,2,3],"ab","abB",exampleDF,1,3)

exampleInitialConfig :: Config
exampleInitialConfig = ([],1,"a")

execExample = do
              putStrLn ("Initial Configuration " ++ (show exampleInitialConfig))
              execTM exampleTM ([],1,"a")          
                                            
