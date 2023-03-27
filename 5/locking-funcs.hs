import GHC.Core.Opt.CallArity (callArityAnalProgram)
-- Check 5.1: we want to lock the second argument ~x~ of the func, so it's
-- easier to create new funcs

ifEven foo x = if even x
               then foo x
               else x

genIfEvenX x = (\f -> ifEven f x)

-- Now we'll create API url generator function
getRequestUrl host apiKey entity id =
  host ++ "/" ++ entity ++ "/" ++ id ++ "?token=" ++ apiKey

-- How can we simplify it?
-- Well, first of all we don't want to enter the host all the time, so let's lock it:
genHostRequestBuilder host = ( \apiKey entity id ->
                                 getRequestUrl host apiKey entity id )

exampleUrlBuilder = genHostRequestBuilder "https://example.com"

-- But now if we look at ~apiKey~, we don't really want to enter it each time, don't we?
-- We want to use ~exampleUrlBuilder~ as our first argument and lock ~apiKey~
genApiRequestBuilder hostBuilder apiKey =
  ( \entity id -> hostBuilder apiKey entity id )

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337h4sk311"



-- Partial func calling
-- In Haskell if you call a function with fewer arguments than it needs,
-- it will automatically create a new funciton for you!
add4 a b c d = a + b + c + d
mystery = add4 3

-- Now that ~mystery~ function will keep ~3~ as an argument,
-- accepting 3 more arguments from you
anotherMystery = mystery 4

-- Using this logic, we can redefine our url-generator,
-- so it will much easier to read!
simpleUrlBuilder = getRequestUrl "https://example.com"

-- Now this function will only require two arguments: entity & id:
mySimpleUrlBilder = simpleUrlBuilder "1337h4sk311"

-- Partial calling is one of the reasons we woould like to place arguments
-- of the function from more general to more specific
