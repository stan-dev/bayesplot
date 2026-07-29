library(bayesplot)
context("SBC")

set.seed(19)
pars <- paste0("beta[", 1:2, "]")
samples_per_prior <- 511
n_replications <- 50
ranks <- list()
for (n in 1:n_replications) {
  r1 <- matrix(0, nrow=samples_per_prior, ncol=length(pars), dimnames=list(NULL, pars))
  for (p1 in 1:length(pars)) {
    r1[sample.int(samples_per_prior, floor(runif(1, 0, samples_per_prior))), p1] <- 1
  }
  ranks[[n]] <- r1
}
set.seed(NULL)

test_that("sbc_hist returns a ggplot object", {
  expect_gg(sbc_hist(ranks, worst = NA))

  g <- sbc_hist(ranks)
  expect_gg(g)
  expect_equal(nlevels(g$data$Parameter), ncol(ranks[[1]]))

  g2 <- sbc_hist(ranks, worst = 1)
  expect_gg(g2)
  expect_equal(nlevels(g2$data$Parameter), 1)
})

test_that("sbc_hist throws correct warnings and errors", {
  expect_warning(sbc_hist(ranks, per_bin = 27),
                 "does not evenly divide the number of samples per prior")
  expect_warning(sbc_hist(ranks, thin = 3),
                 "does not evenly divide the number of samples per prior")

  ranks2 <- ranks
  ranks2[[1]] <- ranks[[1]][, 1, drop=FALSE]
  expect_error(sbc_hist(ranks2), "Not all matrices in 'ranks' have the same dimensions")

  ranks2[[1]] <- ranks[[1]][, 1, drop=TRUE]
  expect_error(sbc_hist(ranks2), "all(sapply(ranks, is.matrix)) is not TRUE", fixed = TRUE)

  expect_error(sbc_hist(ranks[[1]]), "is.list(ranks) is not TRUE", fixed=TRUE)
})
