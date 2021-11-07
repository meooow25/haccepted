# haccepted
![Tests badge](https://github.com/meooow25/haccepted/actions/workflows/tests.yml/badge.svg)
![Benchmarks badge](https://img.shields.io/badge/Benchmarks-not%20bad-green)

Data structures and algorithms for competitive programming in Haskell

## About
This library contains data structures and algorithms primarily intended for use in
[competitive programming](https://en.wikipedia.org/wiki/Competitive_programming). This means that,
unlike a typical library, the source code in this library is meant to be looked at, copied, and
modified by anyone using it.

It is expected that the code in this library will be _correct_ and _fast_.
* Property testing with [QuickCheck](https://hackage.haskell.org/package/QuickCheck) is used to
check for correctness.
* Benchmarking with [criterion](https://hackage.haskell.org/package/criterion) is used to ensure
good performance.

**Disclaimer**: I'm new to Haskell. Feedback is welcome! :)

### Haskell and competitive programming
<details>
<summary>Expand</summary>

Competitive programming today is largely dominated by C++, followed by Python and Java, in spite of
many judges offering a large selection of languages to use. When I tried Haskell, I found it usually
leads to short and neat solutions compared to typical imperative implementations. Other benefits of
Haskell, like its ready-to-use abstractions and persistent-by-default data structures, are very nice
to have.

Of course, all this comes at a cost, the most important of which is speed. Simpler problems usually
pose no risk, but it is possible that, contrary to the title of this repo, complex algorithms when
not heavily optimized will be too slow to be accepted by an online judge.

That being said, I still find it enjoyable and worth the effort to solve problems using Haskell, and
I hope you'll agree.

</details>

### Functional style?
<details>
<summary>Expand</summary>

Functional programming is fun, but sometimes mutable state is necessary for an efficient
implementation. Luckily, Haskell allows working with mutable state in the ST monad. So, I'm a little
conflicted about the style that should be used in this library. Should the library as fast as
possible but boring, using ST everywhere? Should it be as functional as possible, even at the cost
of speed?  
I feel the answer is somewhere in the middle, for now. So here are some loose rules I follow.
- Functional style is preferred.
- If functional style worsens the time complexity of an algorithm, ST is used.
- Data structures are usually functional, making them persistent.
- If an algorithm is simpler or a lot faster in the constant factor to implement in ST, ST is used.

</details>

## Browsing
All source files are present in the [`src`](/src) directory. Each file has a header comment, usually
with four parts.
1. An overview of the data structure or algorithm
2. Sources that were referred to when implenting the data structure or algorithm
3. Some notes about the implementation
4. Descriptions and time complexities of the functions intended for external use

When using as a black box, it suffices to only read 1 and 4.

Tests are found in the [`tests`](/tests) directory.  
Benchmarks are in the [`bench`](/bench) directory. A benchmark summary file is at
[`bench-out/bench-out.txt`](/bench-out/bench-out.txt).

## Judges
A non-exhaustive list of online judges that support Haskell submissions, with the available compiler
versions at the time of writing.
* CSES ([GHC 8.10.4](https://cses.fi/howto/))
* Codeforces (GHC 8.10.1) <sub><sup>(Not documented, the version is seen on the Submit page)</sup></sub>
* AtCoder ([GHC 8.8.3](https://atcoder.jp/contests/language-test-202001))
* Kattis ([GHC 8.6.5](https://open.kattis.com/help/haskell))
* Hackerrank ([GHC 8.6.5](https://support.hackerrank.com/hc/en-us/articles/1500002392722--Execution-Environment-and-Samples))
* CodeChef ([GHC 8.0.1](https://www.codechef.com/wiki/list-compilers))
* HackerEarth ([GHC 7.10.3](https://www.hackerearth.com/docs/wiki/developers/judge/))

Most judges offer only the [Haskell Heirarchical Libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/),
some have additional libraries installed. See the respective links for details.

## See also
Other resources for competitive programming in Haskell
- [Brent Yorgey's blog](https://byorgey.wordpress.com/)

## License
MIT. The license text need not be included for competitive programming.
