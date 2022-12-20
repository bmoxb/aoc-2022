import Data.List.Split (splitOn)
import Data.Char (isAlpha)

main = do
    s <- readFile "input.txt"
    print (evaluate s)

evaluate :: String -> String
evaluate input =
    let finalStacks = applyCommands commands (parseStacks initialString)
    in map last finalStacks
    where
        split = splitOn "\n\n" input
        initialString = split !! 0
        movesString = split !! 1
        commands = map parseCommand (lines movesString)

type Stacks = [String]

parseStacks :: String -> Stacks
parseStacks input =
    parse ls initialStacks
    where
        ls = reverse (lines input)
        stackCount = (length (head ls)) `div` 4 + 1
        initialStacks = replicate stackCount ""
        parse (l:ls) stacks = parse ls (parseStackLine l 1 stacks)
        parse [] stacks = stacks

parseStackLine :: String -> Int -> Stacks -> Stacks
parseStackLine line stackNum stacks
    | stackNum - 1 < length stacks =
        let index = (stackNum - 1) * 4 + 1
            element = line !! index
        in if isAlpha element then
            parseStackLine line (stackNum+1) (addToStack stackNum [element] stacks)
        else
            parseStackLine line (stackNum+1) stacks
    | otherwise = stacks

applyCommands :: [Command] -> Stacks -> Stacks
applyCommands (x:xs) stacks = applyCommands xs (applyCommand x stacks)
applyCommands [] stacks = stacks

applyCommand :: Command -> Stacks -> Stacks
applyCommand c s = moveBetweenStacks (fromStack c) (toStack c) (moveCount c) s

moveBetweenStacks :: Int -> Int -> Int -> Stacks -> Stacks
moveBetweenStacks from to count stacks =
    addToStack to (reverse taken) stacksAfterTake
    where (taken, stacksAfterTake) = (takeFromStack from count stacks)

addToStack :: Int -> String -> Stacks -> Stacks
addToStack to values stacks =
    x ++ newStack : xs
    where
        (x,stack:xs) = splitAt (to - 1) stacks
        newStack = stack ++ values

takeFromStack :: Int -> Int -> Stacks -> (String, Stacks)
takeFromStack from count stacks =
    (drop lenMinusCount stack, x ++ newStack : xs)
    where
        (x,stack:xs) = splitAt (from - 1) stacks
        lenMinusCount = (length stack) - count
        newStack = take lenMinusCount stack

data Command = Command { moveCount :: Int, fromStack :: Int, toStack :: Int }

parseCommand :: String -> Command
parseCommand line =
    Command {moveCount=(read (w !! 1)), fromStack=(read (w !! 3)), toStack=(read (w !! 5))}
    where w = words line
