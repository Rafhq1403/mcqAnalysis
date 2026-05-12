## Build mcq_example dataset for itemAnalysis package.
## A simulated 200-student, 30-item, 4-option multiple-choice test with
## a deliberately mixed mix of item quality so worked examples show
## different patterns of difficulty, discrimination, and distractor
## functioning.

set.seed(42)

n_students <- 200
n_items <- 30
options <- c("A", "B", "C", "D")

# Latent ability for each student (z-score)
ability <- rnorm(n_students)

# Per-item difficulty (b parameter analog) — mix of easy, medium, hard
item_difficulty_param <- c(
  # Easy items (8)
  rep(-1.5, 4), rep(-0.8, 4),
  # Medium items (16)
  rep(-0.2, 6), rep(0.2, 6), rep(0.5, 4),
  # Hard items (4)
  rep(1.2, 2), rep(1.8, 2),
  # Two deliberately bad items (negative discrimination)
  -0.5, 0.3
)
stopifnot(length(item_difficulty_param) == n_items)

# Per-item discrimination (a parameter analog) — most items good,
# a few weak, two negative
item_discrimination_param <- c(
  rep(1.5, 25),       # good items
  rep(0.5, 3),        # weak discriminators
  -0.8, -0.6          # negative-discriminating items (badly written)
)
stopifnot(length(item_discrimination_param) == n_items)

# Random key for each item
key <- sample(options, n_items, replace = TRUE)
names(key) <- paste0("item", sprintf("%02d", seq_len(n_items)))

# Generate responses
responses <- matrix(
  NA_character_,
  nrow = n_students,
  ncol = n_items,
  dimnames = list(
    paste0("student", sprintf("%03d", seq_len(n_students))),
    names(key)
  )
)

for (j in seq_len(n_items)) {
  # Probability of correct response (2PL-style logistic)
  p_correct <- plogis(
    item_discrimination_param[j] * (ability - item_difficulty_param[j])
  )
  correct <- runif(n_students) < p_correct
  # For incorrect responses, sample uniformly among the three distractors
  # except for item 30, which has a "trap" distractor that high-ability
  # students disproportionately choose (to demonstrate a real diagnostic)
  distractors <- setdiff(options, key[j])
  for (i in seq_len(n_students)) {
    if (correct[i]) {
      responses[i, j] <- key[j]
    } else {
      if (j == n_items) {
        # Trap: probability of trap distractor rises with ability
        trap <- distractors[1]
        other <- distractors[-1]
        p_trap <- plogis(0.7 * ability[i])
        responses[i, j] <- if (runif(1) < p_trap) trap else sample(other, 1)
      } else {
        responses[i, j] <- sample(distractors, 1)
      }
    }
  }
}

mcq_example <- list(
  responses = responses,
  key = key
)

usethis::use_data(mcq_example, overwrite = TRUE)
