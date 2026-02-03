cup amount = \msg -> msg amount

getAmount aCup = aCup id

drink aCup amount =
  if amount >= getAmount aCup
    then cup 0
    else cup (getAmount aCup - amount)

main = do
  let myCup = cup 10
  let myCupAfterDrink = drink myCup 3
  print (getAmount myCup) -- Should print 10
  print (getAmount myCupAfterDrink) -- Should print 7