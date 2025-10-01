# Test check_named_vctr

test_that("Expected output: NULL, numeric vector", {
  observed <-
    check_named_vctr(x = c(one = 1, two = 2, 3),
                     names = c("one", "two", "three"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})

test_that("Expected output: named vector, numeric vector", {
  observed <-
    check_named_vctr(x = c(one = 1, two = 2, three = 3),
                     names = c("one", "two", "three"),
                     default = NULL)
  
  expected <- c(one = 1, two = 2, three = 3)
  
  expect_equal(observed, expected)
})

test_that("Expected output: NULL, character vector", {
  observed <-
    check_named_vctr(x = c(a = "apple", b = "banana", "cherry"),
                     names = c("c", "b", "a"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})

test_that("Expected output: named vector, character vector", {
  observed <-
    check_named_vctr(x = c(a = "apple", b = "banana", c = "cherry"),
                     names = c("c", "b", "a"),
                     default = NULL)
  
  expected <-
    c(a = "apple", b = "banana", c = "cherry")
  
  expect_equal(observed, expected)
})


test_that("Expected output: NULL, logical vector", {
  observed <-
    check_named_vctr(x = c(this = TRUE, that = FALSE, theOther = TRUE),
                     names = c("notThis", "That", "TheOther"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})


test_that("Expected output: named vector, logical vector", {
  observed <-
    check_named_vctr(x = c(this = TRUE, that = FALSE, theOther = TRUE),
                     names = c("theOther", "this", "that"),
                     default = NULL)
  
  expected <- c(this = TRUE, that = FALSE, theOther = TRUE)
  
  expect_equal(observed, expected)
})


test_that("Expected output: NULL, list", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "trois", "deux", "quatier", "quatre"),
                     default = NULL)
  
  expected <-  NULL
  
  expect_equal(observed, expected)
})


test_that("Expected output: named vector, list", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "trois", "deux", "quatre"),
                     default = NULL)
  
  expected <- list(un = 1, deux = 2, trois = 3, quatre = 4)
  
  expect_equal(observed, expected)
})


test_that("Expected null: list with a logical element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "trois", TRUE, "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})

test_that("Expected null: list with a numeric element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", 1, "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})

test_that("Expected null: list with an integer element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", 1L, "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})


test_that("Expected null: list of character vectors where one vector contains more than one element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", c("three", "trois"), "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})