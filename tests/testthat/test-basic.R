# Basic tests for smartAnno package

test_that("get_api_format works correctly", {
  # Test auto-detection
  expect_equal(get_api_format("claude-3"), "claude")
  expect_equal(get_api_format("gpt-4"), "openai")
  
  # Test manual specification
  expect_equal(get_api_format("any-model", "openai"), "openai")
  expect_equal(get_api_format("any-model", "claude"), "claude")
  
  # Test error handling
  expect_error(get_api_format("any-model", "invalid"))
})

test_that("package loads correctly", {
  expect_true("smartAnno" %in% loadedNamespaces())
})