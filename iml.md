

```r
library(haven)

rm(list = ls())

b_vector <- as.vector(read_sas("data/parmest_cumcover_in.sas7bdat")$Estimate)
```

```
## Error: 'data/parmest_cumcover_in.sas7bdat' does not exist in current working directory ('/media/work/projects/makroalgaeIndicators/R').
```

```r
v_b_matrix <- as.matrix(read_sas("data/covb_cumcover_in.sas7bdat"))
```

```
## Error: 'data/covb_cumcover_in.sas7bdat' does not exist in current working directory ('/media/work/projects/makroalgaeIndicators/R').
```

```r
st_depth <- 10
l_vector <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, st_depth, 50, 0)

estimate <- l_vector %*% b_vector
```

```
## Error in eval(expr, envir, enclos): object 'b_vector' not found
```

```r
variance <- l_vector %*% v_b_matrix %*% (l_vector)
```

```
## Error in eval(expr, envir, enclos): object 'v_b_matrix' not found
```

```r
b_vector
```

```
## Error in eval(expr, envir, enclos): object 'b_vector' not found
```

```r
v_b_matrix
```

```
## Error in eval(expr, envir, enclos): object 'v_b_matrix' not found
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
## Error in eval(expr, envir, enclos): object 'estimate' not found
```

```r
variance
```

```
## Error in eval(expr, envir, enclos): object 'variance' not found
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
```

```
## Error: object 'estimate' not found
```

```r
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

