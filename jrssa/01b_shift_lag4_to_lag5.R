####################################################################
############# Shortcut: build the lag-5 HMM input from the lag-4 files
####################################################################
# Only the lagged attacker positions player1_y, ..., player5_y depend on the
# lag in 01. In the lag-4 files the row of frame t holds the attacker position
# of frame t - 4, so the position of frame t - 5 is in the previous row of the
# same track. Shifting these columns by one row within each track gives lag 5.
# The first row of each track is dropped (its attacker position at t - 5 was
# already removed by 01), i.e. tracks start 0.1 s later than after a full
# rerun of 01 with HMM_LAG = 5. All other outputs of 01 do not depend on the lag.

library(dplyr)
library(vroom)
library(here)
source(here("jrssa", "settings.R"))
stopifnot(HMM_LAG == 5)

att_cols <- paste0("player", 1:5, "_y")

for(w in 1:9){
  cat("week", w, "\n")
  # read every column as text, so no value is changed by type guessing
  # (e.g. event is empty in the first rows and would be guessed as logical)
  d4 <- vroom(here("rawdata", paste0("final_preprocessed_week_", w, "_lag4.csv")),
              col_types = cols(.default = "c"),
              show_col_types = FALSE)
  stopifnot(nrow(problems(d4)) == 0)

  d5 <- d4 |>
    group_by(uniId) |>
    mutate(across(all_of(att_cols), dplyr::lag),
           keep_row = row_number() > 1) |>
    ungroup() |>
    filter(keep_row) |>
    select(-keep_row)

  # check: the new value in each row is the old value of the previous row
  stopifnot(nrow(d5) == nrow(d4) - n_distinct(d4$uniId))

  vroom_write(d5, hmm_file(w), delim = ",")
}
