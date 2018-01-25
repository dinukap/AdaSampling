context("adaS-inputdata")

features.brca <- apply(X = brca[,-10], MARGIN = 2, FUN = as.numeric)
labels.brca <- sapply(X = brca$cla, FUN = function(x) {ifelse(x == "malignant", 1, 0)})

set.seed(1)
train.ind <- sort(sample(nrow(brca), (4*nrow(brca))/5))
test.ind <- seq(1:nrow(brca))[-train.ind]

train.mat <- features.brca[train.ind,]
test.mat <- features.brca[test.ind, ]

train.cls <- labels.brca[train.ind]

#adding noise
pos <- which(train.cls == 1)
neg <- which(train.cls == 0)
train.noisy.cls <- train.cls
train.noisy.cls[sample(pos, floor(length(pos) * 0.4))] <- 0
train.noisy.cls[sample(neg, floor(length(neg) * 0.2))] <- 1


testthat::test_that("length equality of train.mat and cls is enforced", {

  expect_error({
    train.cls1 <- train.cls[-1]
    adaSample(train.mat, test.mat, cls = train.cls1)
  }, "train.mat and cls are of different lengths")

  expect_error({
    train.cls2 <- c(train.cls, 1)
    adaSample(train.mat, test.mat, cls = train.cls2)
  }, "train.mat and cls are of different lengths")

  })

testthat::test_that("adaSample can cope with a data.frame as input", {
  expect_equivalent({
    train.df <- as.data.frame(train.mat)
    adaSample(train.mat = train.df, test.mat, cls = train.noisy.cls)
  },
  {
    adaSample(train.mat = train.df, test.mat, cls = train.noisy.cls)
  })

})

testthat::test_that("input mat rownames don't matter", {
  expect_equivalent({
    train.mat.withrownames <- train.mat
    rownames(train.mat.withrownames) <- paste("rownum", seq(1, nrow(train.mat)), sep = "-")
    adaSample(train.mat = train.mat.withrownames, test.mat, cls = train.noisy.cls)
  },
  {
    adaSample(train.mat, test.mat, cls = train.noisy.cls)
  })
})

testthat::test_that("adaSample doesn't allow train.mat and test.mat to have different numbers of columns ", {
  expect_error({
    test.mat.missingcolumn <- test.mat[,-1]
    adaSample(train.mat, test.mat = test.mat.missingcolumn, cls = train.noisy.cls)
  }, "do not have the same number of columns")
})

testthat::test_that("cls is a vector", {
  expect_error({
    train.noisy.cls.inadataframe <- data.frame(remnantcolumn = train.noisy.cls)
    adaSample(train.mat, test.mat, cls = train.noisy.cls.inadataframe)
  },)
})

testthat::test_that("cls is made of 1s and 0s", {
  expect_error({
    train.noisy.cls.aschar <- sapply(X= train.noisy.cls, FUN = function(x) {ifelse(x==1, "malignant", "benign")})
    adaSample(train.mat, test.mat, cls = train.noisy.cls.aschar)
  }, "cls must only contain 1s and 0s")
})
