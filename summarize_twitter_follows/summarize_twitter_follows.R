library(readr)
library(purrr)
library(dplyr)

follow_metrics <- function(data, user_id) {

  sn = data %>%
         filter(from_id == user_id, follows == 2) %>%
         select(from_sn) %>%
         .[[1]]

  following <- data %>%
                 filter(from_id == user_id, follows == 1) %>%
                 count() %>%
                 .[[1]]

  follows <- data %>%
               filter(to_id == user_id, follows == 1) %>%
               count() %>%
               .[[1]]

  result = tibble(sn = sn,
                   num_following = following,
                   num_follows = follows)

  return (result)
}

summarize_follows <- function(data) {
  user_ids = data %>%
               filter(follows == 2) %>%
               select(from_id)

  result = map_df(user_ids[[1]], ~follow_metrics(data,.x))

  return(result)
}

text = "from_id,     from_sn, to_id,    to_sn, follows
 11111,        alice, 11111,     alice,       2
 22222,          bob, 11111,     alice,       1
 33333,       claire, 11111,     alice,       1
 22222,          bob, 22222,       bob,       2
 33333,       claire, 33333,    claire,       2
"

data <- read_csv(text)

summarize_follows(data)
