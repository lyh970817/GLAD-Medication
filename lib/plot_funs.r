test_ci <- function(x, n, d) {
  if (n == 0) {
    return(FALSE)
  }
  diff(binom.test(x = x, n = n, p = 0)$conf.int) < d
}

get_name <- function(colname, n) {
  colname %>%
    str_split("\\.") %>%
    map_chr(n)
}

get_descr <- function(descr_name, n) {
  descr_name %>%
    str_split("- ") %>%
    map(n) %>%
    str_split(" \\(") %>%
    map_chr(1)
}
