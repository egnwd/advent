# advent2020

## Stars

| Day | Stars | Code |
|-----:|:-------:|------|
|   1  |  ⭐⭐  | [execs/Day01.hs](https://github.com/egnwd/advent/blob/main/execs/Day01.hs) |
|   2  |  ⭐⭐  | [execs/Day02.hs](https://github.com/egnwd/advent/blob/main/execs/Day02.hs) |
|   3  |  ⭐⭐  | [execs/Day03.hs](https://github.com/egnwd/advent/blob/main/execs/Day03.hs) |
|   4  |  ⭐⭐  | [execs/Day04.hs](https://github.com/egnwd/advent/blob/main/execs/Day04.hs) |
|   5  |  ⭐⭐  | [execs/Day05.hs](https://github.com/egnwd/advent/blob/main/execs/Day05.hs) |
|   6  |  ⭐⭐  | [execs/Day06.hs](https://github.com/egnwd/advent/blob/main/execs/Day06.hs) |
|   7  |  ⭐⭐  | [execs/Day07.hs](https://github.com/egnwd/advent/blob/main/execs/Day07.hs) |

## Requirements

 - [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/): `curl -sSL https://get.haskellstack.org/ | sh` or `brew install haskell-stack`

## Run

### Tests

```sh
λ stack test
```

### Specific Day

```sh
λ stack run day1
```

### Local

For automatic tests on save
```sh
λ ghcid -T=':!stack test'
```

### Attributions

Test framework & some parsing from: [glguy/advent2019](https://github.com/glguy/advent2019)
