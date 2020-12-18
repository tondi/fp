actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >>  putChar '\n'
doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn
doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3 :: IO ()
doEcho3 = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ l1 ++ l2


doDialog :: IO ()
doDialog = do
    putStr "What is your happy number? "
    num <- getLine
    if read num == 7
    then do 
        putStrLn "Ah, lucky 7!"
    else if (odd $ read num)
        then putStrLn "Odd number! That's most people's choice..."
        else putStrLn "Hm, even number? Unusual!"


twoQuestions :: IO ()
twoQuestions = putStr "What is your name? "
    >> getLine
    >>= \name -> putStr "How old are you? "
        >> getLine
            >>= \n -> let age = read n :: Int in
            print (name,age)

