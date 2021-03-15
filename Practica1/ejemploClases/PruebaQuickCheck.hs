import Test.QuickCheck
import Data.List

--Referencias principales:
--1: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
--2: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

-- Propiedades básicas de operadores ariméticos
suma :: Int -> Int -> Int
suma x y = x+y

prop_suma_corr x y = suma x y == x+y
prop_suma_conm x y = suma x y == suma y x

division :: Int -> Int -> Int
division x y = div x y

prop_div_corr x y = division x y == div x y

--Esta propiedad difiere ya que damos una precondición
prop_div_corr_s x y = y /= 0 ==> division x y == div x y

-- Propiedades sobre listas
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_qsort_corr_clas xs = classify (length xs <= 1) "length(xs) <= 1" $
                          classify (length xs > 1) "length(xs) > 1" $
                          qsort xs == sort xs

prop_qsort_corr_col xs = collect (length xs <= 1) $
                         collect (length xs) $
                         qsort xs == sort xs

randomList :: Gen [Int]
randomList = sized $ \n ->
    frequency
        [(1,return []), --1/n
         (n,do x <- arbitrary --m/n  m <- [2..n]
               xs <- randomList
               return (sort (x:xs)))]

prop_qsort_corr_fa = forAll randomList $ \xs ->
                       collect (length xs > 2) $
                       collect (length xs) $
                       qsort xs == sort xs

prop_sortl_clase = forAll randomList $ \xs -> not (null xs) ==> head xs == minimum xs

--Listas construidas alrevés
data LSnoc = Lin | Snoc LSnoc Int deriving (Show)

--Una instancia de la clase Arbitrary para generar elementos aleatorios
instance Arbitrary LSnoc where
    arbitrary = do 
        n <- choose (1,2) :: Gen Int --Elegimos aleatoriamente la elección del constructor
        case n of
            1 -> return Lin --El constructor elegido es la lista vacía
            2 -> do x <- arbitrary --Elegimos un número aleatorio
                    xs <- arbitrary --Elegimos una lista snoc aleatoria
                    return (Snoc xs x) --Construimos una lista aleatoria

--Función que genera listas aleatorias
randomLSnoc :: Gen LSnoc
randomLSnoc = sized $ \n ->
    frequency
        [(1,return Lin), --1/n
         (n,do x <- arbitrary --m/n  m <- [2..n]
               xs <- randomLSnoc
               return (Snoc xs x))]

headLS :: LSnoc -> Int
headLS Lin = error "Empty list."
headLS (Snoc Lin x) = x
headLS (Snoc xs _) = headLS xs

nullS :: LSnoc -> Bool
nullS Lin = True
nullS _ = False

toList :: LSnoc -> [Int]
toList Lin = []
toList (Snoc xs x) = (toList xs)++[x]

--Sacar la cabeza de una lista snoc es lo mismo que convertirla a lista
--y aplicar head. Hay que considerar que la lista snoc no sea vacía
prop_headLS xs = not (nullS xs) ==> headLS xs == head (toList xs)