install.packages("tidyverse")
library(tidyverse)

df <- readRDS("./ICBe_V1.1_events_agreed.Rds")

assert <- function (df, mask) {
  result <- 
    df %>% 
    filter(!{{ mask }}) %>% 
    select(-starts_with("date_"), -starts_with("rater"))
  
  if (nrow(result) > 0) {
    View(result)
  }
}

na_or_empty <- function (xs) {
  is.na(xs) | xs == ""
}

`=>` <- function(a, b) {
  !a | b
}

# FALSE
df %>% 
  assert(is.na(event_type) | (event_type %in% c("action", "speech", "thought")))

# FALSE
df %>% 
  assert((event_type == "action") %=>% !is.na(do_actor_a))

# FALSE
df %>% 
  assert((event_type == "speech") %=>% !is.na(say_actor_a))

# FALSE
df %>% 
  assert((event_type == "thought") %=>% !is.na(think_actor_a))

df %>% 
  assert(is.na(do_actor_a) %=>% is.na(do_actor_b))

df %>% 
  assert(is.na(do_kind) %=>% is.na(do_actor_a))

# FALSE
df %>% 
  assert(is.na(do_actor_a) %=>% is.na(do_kind))

# Although less strict, still FALSE
df %>% 
  assert(is.na(do_actor_a) %=>% na_or_empty(do_kind))

# FALSE
df %>% 
  assert((event_type != "action") %=>% na_or_empty(do_duration))

# FALSE
df %>%
  assert(na_or_empty(do_interact_kind) | (event_type == "action"))


# --- I do have some preprocessing cleanup code as well, but it's not ready for sharing yet ---
