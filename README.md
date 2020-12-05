# advent2020

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
