library(raisqlr)
fixture <- testthat::test_path("fixtures", "RAIS_VINC_PUB_NI.7z")
has_db <- nzchar(Sys.getenv("pwdrais"))
