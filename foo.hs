double x = x+x
quad x = double(double x)
fact n = product [1..n]
average ns = div (sum ns) (length ns)
lasta n = drop ((length n)-1) n
remo1 n = take ((length n)-1) n
remo2 n = reverse (drop 1 (reverse n))
ppp n = "1234567 wodepengyouzainali"
shuffle [] = []  
shuffle n = (tail n) ++ (take 1 n)
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
           where
             ys = [a | a <- xs, a <= x]
             zs = [b | b <- xs, b > x]   
   