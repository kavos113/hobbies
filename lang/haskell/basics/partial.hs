add4 a b c d = a + 2 * b + 3 * c + 4 * d

add4Partial = add4 1 2

main = do
    print (add4Partial 3 4)  -- 1 + 2*2 + 3*3 + 4*4 = 1 + 4 + 9 + 16 = 30