Nice and simple to start off

Both parts involved read the string as a `[Int]` and we can then find the sum of the list.

```
getValue :: Char -> Int
getValue '(' = 1
getValue ')' = -1
```

```
λ> sum . map getValue $ "())"
-1
```

For part 2, we can fold over the list and keep track of the postition and the level,
when we hit the basement we can just return the position.
