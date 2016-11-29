library(haven)

rm(list = ls())

b_vector <- as.vector(read_sas("data/parmest_cumcover_in.sas7bdat")$Estimate)

## Open Jacob's matrix and replace 1 value at [1,1] with the estimate from the GLMM
v_b_matrix <- as.matrix(read_sas("data/covb_cumcover_in.sas7bdat"))

st_depth <- 10

l_vector <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, st_depth, 50, 0)

estimate <- l_vector %*% b_vector

variance <- l_vector %*% v_b_matrix %*% (l_vector)

b_vector
v_b_matrix
l_vector
estimate
variance

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

ggsave("/home/pmassicotte/Desktop/histo.pdf", height = 4)
