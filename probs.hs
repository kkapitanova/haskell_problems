---- Functional Programming Coursework ----
---- Written by Kristina Kapitanova -----

---- Task 1 -----
-- The function steps takes three positive Int values p m n where p is the height, m is width and n is the number of steps
steps :: Int -> Int -> Int -> String
steps p m n = unlines [concat (replicate (quot i p) mStars) | i <- [p .. (n+1)*p-1]] -- loops from the first to last line of the traingle shape and generates an appropriate string, then concatenatess the strings and puts them in a single one
	where 
		mStars = replicate m '*' -- replicates the asterisk symbol m times

----- Task 2 -----
-- The function flagpattern takes three positive Int values and returns a String that is displayed as a flag pattern with a cross inside of dimension 'size'
flagpattern :: Int -> String
flagpattern size = concat [flag (quot i size) (mod i size) size | i <- [0..size*size-1]] -- calls the flag method for each symbol providing its row and column

flag :: Int -> Int -> Int -> String
flag i j size
	| j == size-1 = "*\n" -- add a line break at the end of the line
	| i == 0 || j == 0 || i == size-1 = "*" -- put an asterisk at the end of the other 3 borders
	| i == j || i == (size-j-1) = "*" -- put an asterics on both diagonals
	| otherwise = " " -- else put an empty space

----- Task 3 -----
-- The function swapwords takes three String values w1 w2 and word and returns word with all occurences of w1 in word replaced by w2
swapwords :: String -> String -> String -> String
swapwords w1 w2 word = init (replaceOn w1 w2 (filter (not . null) (splitOn ' ' [] (word++[' ']))))
			
replaceOn :: String -> String -> [String] -> String
replaceOn _ _ [] = "" -- default case
replaceOn w1 w2 (x:xs) -- loop for each string in the array
	| x == w1 = w2 ++ " " ++ replaceOn w1 w2 xs -- if x is equal to w1, replace it with w2
	| otherwise = x ++ " " ++ replaceOn w1 w2 xs -- else don't replace anything

splitsOn :: Eq a => a -> [a] -> [a] -> [[a]]
splitsOn _ [] [] = [[]] -- default case
splitsOn splitter alreadyRead (x:xs) -- loop for each element of the remaning words to split
	| x == splitter = alreadyRead : splitsOn splitter [] xs -- if x is the splitter than make the alreadyRead array as a result of the function and call the function with the remainder
	| otherwise = splitsOn splitter (alreadyRead++[x]) xs -- if x is not the splitter then add the word to the array of alreadyRead arrays


----- Task 4 -----
-- The function compatibility takes two String values and gives and output that shows the compatibility between them
compatibility :: String -> String -> String
compatibility name1 name2 = genString name1 name2 ++ " and " ++ genString name2 name1 -- apply it to the twice, once in order, once reversed

genString :: String -> String -> String
genString name1 name2
	| rem == 1 = name1 ++ " loves " ++ name2 -- if the remainder is 1 then name1 loves name2
	| rem == 2 = name1 ++ " is physical to " ++ name2 -- if the remainder is 2 then name1 is physical to name2
	| rem == 3 = name1 ++ " hates " ++ name2 -- if the remainder is 1 then name1 hates name2
	| rem == 0 = name1 ++ " is indifferent to " ++ name2 -- if the remainder is 1 then name1 is indiferent name2
	where rem = mod (length (cancelMatching name1 name2)) 4 -- calculate the remainder

cancelMatching :: String -> String -> String
cancelMatching [] _ = "" -- default case
cancelMatching (x:xs) name2 -- loop through each char of the name1
	| elem x name2 = cancelMatching xs (deleteFirst x name2) -- if it part of the second String delete its first occurence
	| otherwise = x : cancelMatching xs name2 -- else continue the loop

deleteFirst :: Char -> [Char] -> [Char]
deleteFirst a (b:bc) | a == b    = bc -- if we have a match return
                     | otherwise = b : deleteFirst a bc -- else continue searching for a matching symbol

----- Task 5 -----
-- The function split takes two arguments of types [a] and a and returns a list that splits the original list where the second argument occurs.
split :: Eq a => [a] -> a -> [[a]]
split [] _ = [] -- default case
split arr splitter = filter (not . null) (splitOn splitter [] (arr++[splitter])) -- adds another spliiter and removes the last empty element of the result of splitOn

splitOn :: Eq a => a -> [a] -> [a] -> [[a]]
splitOn _ [] [] = [[]] -- default case
splitOn splitter alreadyRead (x:xs) -- loop for each element of the remaning words to split
	| x == splitter = alreadyRead : splitOn splitter [] xs -- if x is the splitter than make the alreadyRead array as a result of the function and call the function with the remainder
	| otherwise = splitOn splitter (alreadyRead++[x]) xs -- if x is not the splitter then add the word to the array of alreadyRead arrays
