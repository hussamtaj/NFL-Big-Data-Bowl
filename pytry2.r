
library(tidyverse)
library(reticulate) # Calling python from R
options(scipen = 999) # Don't use scientific notation for big numeric Ids

# We need to override this environment variable to tell reticulate to use Python 3.6
Sys.setenv(RETICULATE_PYTHON = '/opt/conda/envs/py36/bin/python', required = T)

# Import the module
nflrush <- import_from_path('competition','../input/nfl-big-data-bowl-2020/kaggle/competitions/nflrush/')
env <- nflrush$make_env()
iter <- env$iter_test()

py_run_string("import io")
py_run_string("import pandas as pd")

cols_obj <- cols(
  GameClock              = col_character(),
  TimeHandoff            = col_character(),
  TimeSnap               = col_character()
)

# Load train set
train <- read_csv('../input/nfl-big-data-bowl-2020/train.csv', guess_max = 500000, col_types = cols_obj)
nfl <- train


my_dist <- function(y,mu,sigma,theta) {
  return( ( 1 / (sqrt(2 * pi * sigma^2)) ) * exp( (-1/2) * ( (((y - mu) / sigma)^2) + (2 * length(y) * theta))) * theta^(sum(y)) )
}

rushers <- train %>% filter(NflIdRusher == NflId)

rushers_mean <- rushers %>% group_by(NflId) %>% 
  summarise(mean_yards = mean(Yards),
            sd_yards = sd(Yards),
            list_yards = list(Yards)) %>% 
  mutate(sd_yards = if_else(is.na(sd_yards) | sd_yards == 0, 6.51 ,sd_yards),
         mean_yards = if_else(is.na(mean_yards) | mean_yards == 0, 3, mean_yards))

# Test values
yards <- c(-99:99)
# mu <- 10
# sigma <- 10
theta <- .889



#nfl_play_rusher <- nfl %>% select(c("PlayId","NflIdRusher")) %>% distinct()

#ma <- matrix(nrow = nrow(nfl_play_rusher), ncol = 200)
#colnames(ma) <- c("PlayId",glue("Yards{c(-99:99)}"))

my_prediction_CDF <- function(test) {
  Current_ID <- test %>% filter(NflIdRusher == NflId) %>% select("NflId")
  Current_Rusher <- rushers_mean %>% filter(NflId == Current_ID)
  
  if(is.na(Current_Rusher[1,1])) {
    Current_Rusher <- data.frame(NflId = 99999999,
                                 mean_yards = mean(nfl$Yards),
                                 sd_yards = sd(nfl$Yards)
                                 )
                                                
  }

  Current_Dist <- my_dist(y = yards,
                          mu = Current_Rusher[[2]],
                          sigma = Current_Rusher[[3]],
                          theta)
  Current_Dist <- Current_Dist / sum(Current_Dist)
  CDF <- cumsum(Current_Dist)
  return(CDF)
}





t1 <- proc.time()

# Loop through plays
while (TRUE) {

  # Step
  df <- iter_next(iter)
  if (is.null(df)) break # Reached end of the test set
    
  # Unpack list from the iteraror
  # Will convert to CSV, path strings from Python to R, and convert back to data frames; sounds convoluted, but this is way faster than py_to_r()
  test_df = read_csv(df[[1]]$to_csv(index = FALSE), col_types = cols_obj) # test dataframe
  pred_df = read_csv(df[[2]]$to_csv(index = FALSE), col_types = cols(.default = col_double())) # prediction dataframe 
    
  # Make your predictions here
  pred_df[,] <- my_prediction_CDF(test_df)
   
  # Convert pred_df to CSV string; move to Python; build the Pandas data frame on the Python end
  py$pred_df_string <- format_csv(pred_df)
  py_run_string("pred_df = pd.read_csv(io.StringIO(pred_df_string))")
  pred_df_pointer <- py_get_attr(py, "pred_df") # pointer to avoid senseless conversion by env$predict
    
  # Save prediction
  env$predict(pred_df_pointer)
    
}

proc.time() - t1

# Save submission
env$write_submission_file()
