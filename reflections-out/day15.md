Day 15
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day15.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *15* / *[16][day16]* / *[17][day17]* / *[18][day18]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day07]: https://github.com/egnwd/advent/blob/2021/reflections-out/day07.md
[day08]: https://github.com/egnwd/advent/blob/2021/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md
[day10]: https://github.com/egnwd/advent/blob/2021/reflections-out/day10.md
[day11]: https://github.com/egnwd/advent/blob/2021/reflections-out/day11.md
[day12]: https://github.com/egnwd/advent/blob/2021/reflections-out/day12.md
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md
[day14]: https://github.com/egnwd/advent/blob/2021/reflections-out/day14.md
[day16]: https://github.com/egnwd/advent/blob/2021/reflections-out/day16.md
[day17]: https://github.com/egnwd/advent/blob/2021/reflections-out/day17.md
[day18]: https://github.com/egnwd/advent/blob/2021/reflections-out/day18.md

*[Prompt][d15p]* / *[Code][d15g]*

[d15p]: https://adventofcode.com/2021/day/15
[d15g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day15.hs

Today was an execellent [A* problem](https://en.wikipedia.org/wiki/A*_search_algorithm), we start at the origin and aim for the bottom right corner.
The cost to travel is the risk, and the hueristic is the manhattan distance to the corner.


*[Back to all reflections for 2021][reflections]*

## Day 15 Benchmarks

```
>> Day 15a
benchmarking...
time                 141.6 ms   (136.9 ms .. 153.1 ms)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 140.6 ms   (138.0 ms .. 144.1 ms)
std dev              4.223 ms   (2.523 ms .. 6.021 ms)
variance introduced by outliers: 12% (moderately inflated)

* parsing and formatting times excluded

>> Day 15b
benchmarking...
time                 5.281 s    (5.078 s .. 5.542 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.936 s    (4.708 s .. 5.104 s)
std dev              217.7 ms   (4.221 ms .. 266.1 ms)
variance introduced by outliers: 19% (moderately inflated)

* parsing and formatting times excluded
```
