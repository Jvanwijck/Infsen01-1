----INFSEN01-1, ASSIGNMENT-2, 0837734 Jeffrey van Wijck, 0840620 Scott Hoefnagel

module VStack (VStackWaarde(..), VStackExpr(..), VStack, Programma, vStack) where

-- De waarden die op de stack kunnen staan. Deze waarden worden ook geretourneerd na
-- executie.
--
data IExpr = IEWaarde Int
             | IETelop IExpr IExpr
             | IEVermenigvuldig IExpr IExpr

data BExpr = BEWaarde Bool
             | BEEn IExpr IExpr
             | BEOf IExpr IExpr


data Expr = BE BExpr | IE IExpr



-------------------------------------------
data VStackWaarde = IWaarde Int
                  | BWaarde Bool
                  | Void
                    deriving Show

-- De expressies die de vStack 'begrijpt'
data VStackExpr = IPush Int
                | BPush Bool
                | Telop
                | Vermenigvuldig
                | En
                | Of
                | Gelijk
                | Ongelijk
                | Niet
                | Evalueer
                 deriving Show

type VStack    = [VStackWaarde]
type Programma = [VStackExpr]

-- Executie van een gegevene programma. Retourneert of een foutmelding of de
-- waarde op de top van de stack na executie.
-- 
vStack :: Programma -> Either String VStackWaarde
vStack = execute []

errType :: String -> Either String a
errType opcode = Left $ " '" ++ opcode ++ "' opcode met ill-typed stack"

errUnderflow :: String -> Either String a
errUnderflow opcode = Left $ "Stack underflow met '" ++ opcode ++ "' opcode"

-- Executie van programma met een gegeven stack.
--
execute :: VStack -> Programma -> Either String VStackWaarde
execute [] []                                            = Right Void
execute (s:_) []                                         = Right s

execute s (IPush x : xs)                                 = execute (IWaarde x : s) xs
execute s (BPush x : xs)                                 = execute (BWaarde x : s) xs

execute (IWaarde s1 : IWaarde s2 : ss) (Telop : xs)      = execute (s':ss) xs
    where s' = IWaarde (s1 + s2)
execute (_:_:_) (Telop:_)                                = errType "Telop"
execute _ (Telop:_)                                      = errUnderflow "Telop"

execute (IWaarde s1:IWaarde s2:ss) (Vermenigvuldig : xs) = execute (s':ss) xs
    where s' = IWaarde (s1 * s2)
execute (_:_:_) (Vermenigvuldig:_)                       = errType "Vermenigvuldig"
execute _ (Vermenigvuldig:_)                             = errUnderflow "Vermenigvuldig"

execute (BWaarde s1:BWaarde s2:ss) (En : xs)             = execute (s':ss) xs
    where s' = BWaarde (s1 && s2)
execute (_:_:_) (En:_)                                   = errType "En"
execute _ (En:_)                                         = errUnderflow "En"

execute (BWaarde s1 : BWaarde s2 : ss) (Of : xs)         = execute (s':ss) xs
    where s' = BWaarde (s1 || s2)
execute (_:_:_) (Of:_)                                   = errType "Of"
execute _ (Of:_)                                         = errUnderflow "Of"


----Functie die controleert of de twee waardes gelijk zijn.
execute (IWaarde s1:IWaarde s2:ss) (Gelijk : xs)      = execute (s':ss) xs
    where s' = BWaarde (s1 == s2)
execute (_:_:_:_) (Gelijk:_)                                = errType "Gelijk"
execute _ (Gelijk:_)                                      = errUnderflow "Gelijk"

----Functie die controleert of de twee waardes ongelijk zijn.
execute (IWaarde s1:IWaarde s2:ss) (Ongelijk : xs)      = execute (s':ss) xs
    where s' = BWaarde (s1 /= s2)
execute (_:_:_:_) (Ongelijk:_)                                = errType "Ongelijk"
execute _ (Ongelijk:_)                                      = errUnderflow "Ongelijk"

----Functie die de waardes van Gelijk en Ongelijk omdraait indien meegegeven..
execute (BWaarde s1:ss) (Niet : xs)      = execute (s':ss) xs
    where s' = BWaarde (not s1)
execute (_:_:_:_) (Niet:_)                                = errType "Niet"
execute _ (Niet:_)                                      = errUnderflow "Niet"


execute (IWaarde s1:IWaarde s2:ss) (Evalueer : xs)      = execute (s':ss) xs
    where s' = BWaarde (s1 /= s2)
execute (_:_:_) (Evalueer:_)                                = errType "Evalueer"
execute _ (Evalueer:_)                                      = errUnderflow "Evalueer"




zetOm :: Expr -> [VStackExpr]
zetOm (IE x) = zetIEOm (x)
zetOm (BE x) = zetBEOm (x)

----Maak van een IExpr, een VStackExprdoor 2 waardes te berekenenen voor de IPush, en bepalen of er opgetelt of vermenigvuldigd of zelfs niets gedaan moet worden.
zetIEOm :: IExpr -> [VStackExpr]
zetIEOm (IETelop x y) = [IPush (bereken_Exp_Op x), IPush (bereken_Exp_Op y), Telop]
zetIEOm (IEVermenigvuldig x y) = [IPush (bereken_Exp_Op x), IPush (bereken_Exp_Op y), Vermenigvuldig]
zetIEOm (IEWaarde x) = [IPush x]

----Maak van een BExpr, een VStackExpr door 2 waardes te berekenenen voor de IPush, en bepaald of er een "En, Of"  vergelijking gedaan moet worden.
zetBEOm :: BExpr -> [VStackExpr]
zetBEOm (BEEn x y) = [IPush (bereken_Exp_Op x), IPush (bereken_Exp_Op y), En]
zetBEOm (BEOf x y) = [IPush (bereken_Exp_Op x), IPush (bereken_Exp_Op y), Of]


-------Bereken recursief de iPush waardes. Dit moet in aparte functie omdat de "execute" boven de resultaten in een String teruggeeft.
----Er wordt per IExpr gekeken of de Telop, Vermenigvuldiging of de Waarde is ingevuld.
bereken_Exp_Op :: IExpr -> Int
bereken_Exp_Op (IETelop x y) = bereken_Exp_Op x + bereken_Exp_Op y;
bereken_Exp_Op (IEVermenigvuldig x y) = bereken_Exp_Op x * bereken_Exp_Op y;
bereken_Exp_Op (IEWaarde x) = x;


-------Voorbeeld Data:
-----Opdracht A.3
itest = vStack [IPush 3, IPush 5, Telop]
itest2 = vStack [IPush 3, IPush 3,Gelijk, Niet]
btest = vStack [BPush True, BPush False, Of]

------Opdracht A.1
johnSmith :: IExpr
johnSmith = IEWaarde 1968

bobSmith :: IExpr
bobSmith = IEWaarde 19

chrisSmith :: IExpr
chrisSmith = IETelop johnSmith bobSmith

jeffrey :: IExpr
jeffrey = IEWaarde 19

christinaSmith :: IExpr
christinaSmith = IEVermenigvuldig chrisSmith jeffrey

christinaExpr :: Expr
christinaExpr = IE christinaSmith

scott :: BExpr
scott = BEEn jeffrey bobSmith

scottExpr :: Expr
scottExpr = BE scott
------