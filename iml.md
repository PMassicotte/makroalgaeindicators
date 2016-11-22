

```r
library(haven)

rm(list = ls())

b_vector <- as.vector(read_sas("../data/parmest_cumcover_in.sas7bdat")$Estimate)
v_b_matrix <- as.matrix(read_sas("../data/covb_cumcover_in.sas7bdat"))

st_depth <- 10
l_vector <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, st_depth, 50, 0)

estimate <- l_vector %*% b_vector

variance <- l_vector %*% v_b_matrix %*% (l_vector)

b_vector
```

```
## [1]  5.4589005179  0.0386988664 -0.0205461398 -0.0065205421 -0.0061530772
## [6]  0.0000000000 -0.1063355660  0.0090905933 -0.0007542095
```

```r
v_b_matrix
```

```
##            col1        col167        col168        col169        col170
##  [1,] 0.1461044  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
##  [2,] 0.0000000  7.597384e-03  1.564059e-03  1.423932e-03  1.451036e-03
##  [3,] 0.0000000  1.564059e-03  2.744152e-03  1.892973e-03  1.767815e-03
##  [4,] 0.0000000  1.423932e-03  1.892973e-03  3.108904e-03  1.897758e-03
##  [5,] 0.0000000  1.451036e-03  1.767815e-03  1.897758e-03  2.303451e-03
##  [6,] 0.0000000  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
##  [7,] 0.0000000 -1.862098e-06  5.783545e-07  1.112108e-06 -1.675621e-06
##  [8,] 0.0000000 -1.369762e-06 -5.199087e-07 -7.564802e-07 -8.224623e-07
##  [9,] 0.0000000  1.680477e-08  1.396619e-07 -3.149767e-07  5.848644e-08
##       col171        col172        col173        col174
##  [1,]      0  0.000000e+00  0.000000e+00  0.000000e+00
##  [2,]      0 -1.862098e-06 -1.369762e-06  1.680477e-08
##  [3,]      0  5.783545e-07 -5.199087e-07  1.396619e-07
##  [4,]      0  1.112108e-06 -7.564802e-07 -3.149767e-07
##  [5,]      0 -1.675621e-06 -8.224623e-07  5.848644e-08
##  [6,]      0  0.000000e+00  0.000000e+00  0.000000e+00
##  [7,]      0  9.192140e-06  3.051272e-07  1.730306e-07
##  [8,]      0  3.051272e-07  2.979274e-07 -1.526810e-07
##  [9,]      0  1.730306e-07 -1.526810e-07  3.804209e-07
```

```r
l_vector
```

```
## [1]  1.0  0.2  0.2  0.2  0.2  0.2 10.0 50.0  0.0
```

```r
estimate
```

```
##         [,1]
## [1,] 4.85117
```

```r
variance
```

```
##           [,1]
## [1,] 0.1494268
```

```r
# boundary_HG <- 74.2
# boundary_GM <- 40.8
# boundary_MP <- 20.4
# boundary_PB <- 13.4

boundary <- c(0, 13.4, 20.4, 40.8, 74.2, 1e6)
labels <- c("bad", "poor", "moderate", "good", "high")

n_iter <- 10000

cumcover <- mat.or.vec(n_iter, 1)

for (i in 1:n_iter) {
  cumcover[i] <- exp(estimate + rnorm(1) * sqrt(variance)) + 1
}

res <- data_frame(
  cumcover = cumcover,
  status = cut(cumcover, breaks = boundary, labels = labels)
)

res %>% 
  ggplot(aes(x = cumcover)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~status)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

