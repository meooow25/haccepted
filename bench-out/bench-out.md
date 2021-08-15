# Benchmarks

### BinSearch

| Name | 100 | 10000 | 1000000 |
| --- | --- | --- | --- |
| binSearch | 4.303 μs | 1.352 ms | 200.5 ms |
| binSearchA | 10.93 μs | 2.523 ms | 910.4 ms |

### Fenwick

| Name | 100 | 10000 | 1000000 |
| --- | --- | --- | --- |
| buildF | 841.9 ns | 94.36 μs | 6.096 ms |
| updateF | 4.801 μs | 2.721 ms | 1.577 s |
| queryF | 2.019 μs | 832.0 μs | 121.2 ms |