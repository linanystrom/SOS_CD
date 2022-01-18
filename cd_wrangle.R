# Basic setup --------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr", "readxl", "ggplot2", "lme4")

lapply(packages, library, character.only = TRUE)

# Load & Merge data --------------------------------------------------

## Load Qualtrics data

### Exchange for real data when available

MC1A_file   <- read_xlsx()

MC1B_file   <- read_xlsx() 

MC2A_file   <- read_xlsx()

MC2B_file   <- read_xlsx()

MC3A_file   <- read_xlsx()

MC3B_file   <- read_xlsx()

## Load disclosure data

data <- read_xlsx("./data/coding_test.xlsx")

## Merge data

## Rbind dataframes

files <- rbind(MC1A_file, MC1B_file, MC2A_file, MC2B_file, MC3A_file, MC3B_file)

write.csv(
  files,
  "./data/qualtrics_files.csv",
  row.names = FALSE
)

df <- merge(data, files, by = "ResponseId")


# Prepare data for analysis --------------------------------------------------

## Reverse code interview quality, interviewer quality and self-assessment items. Create interview quality, interviewer quality and self-assesment composite. 

###Interview quality

sos <- df %>% 
  mutate(
    interview_adj_awkward_R = case_when(
      interview_adj_awkward == 5 ~ 1,
      interview_adj_awkward == 4 ~ 2,
      interview_adj_awkward == 3 ~ 3,
      interview_adj_awkward == 2 ~ 4,
      interview_adj_awkward == 1 ~ 5
    ),
    interview_adj_difficult_R = case_when(
      interview_adj_difficult == 5 ~ 1,
      interview_adj_difficult == 4 ~ 2,
      interview_adj_difficult == 3 ~ 3,
      interview_adj_difficult == 2 ~ 4,
      interview_adj_difficult == 1 ~ 5
    ),
    
    interview_qual = (interview_adj_smooth + interview_adj_awkward_R + interview_adj_comfortable + interview_adj_difficult_R + interview_adj_open + interview_adj_pleasant)/6,
    
    ###Interviewer quality
    
    interviewer_adj_challenging_R = case_when(
      interviewer_adj_challenging == 5 ~ 1,
      interviewer_adj_challenging == 4 ~ 2,
      interviewer_adj_challenging == 3 ~ 3,
      interviewer_adj_challenging == 2 ~ 4,
      interviewer_adj_challenging == 1 ~ 5
    ),
    interviewer_adj_aggressive_R = case_when(
      interviewer_adj_aggressive == 5 ~ 1,
      interviewer_adj_aggressive == 4 ~ 2,
      interviewer_adj_aggressive == 3 ~ 3,
      interviewer_adj_aggressive == 2 ~ 4,
      interviewer_adj_aggressive == 1 ~ 5
    ),
    interviewer_adj_rough_R = case_when(
      interviewer_adj_rough == 5 ~ 1,
      interviewer_adj_rough == 4 ~ 2,
      interviewer_adj_rough == 3 ~ 3,
      interviewer_adj_rough == 2 ~ 4,
      interviewer_adj_rough == 1 ~ 5
    ),
    interviewer_qual = (interviewer_adj_friendly + interviewer_adj_challenging_R + interviewer_adj_aggressive_R + interviewer_adj_sympathetic + interviewer_adj_interested + interviewer_adj_rough_R)/6,
    
    ### Self-assessment of performance
    
    interview_statements_2 = case_when(
      interview_statements_2 == 5 ~ 1,
      interview_statements_2 == 4 ~ 2,
      interview_statements_2 == 3 ~ 3,
      interview_statements_2 == 2 ~ 4,
      interview_statements_2 == 1 ~ 5
    ),
    interview_statements_3 = case_when(
      interview_statements_3 == 5 ~ 1,
      interview_statements_3 == 4 ~ 2,
      interview_statements_3 == 3 ~ 3,
      interview_statements_3 == 2 ~ 4,
      interview_statements_3 == 1 ~ 5
    ),
    self_assessment = (interview_statements_1 + interview_statements_2 + interview_statements_3 + interview_statements_4)/4,
  )

### Add crime_order column
sos$crime_order <- paste(sos$mock_crime, sos$sequence)

### Organize columns

sos_wrangle <- sos %>%
  select(ID, ResponseId, mock_crime, sequence, crime_order, style, interviewer, stage_1, stage_2, stage_3, stage_4, stage_5, stage_6, st_1_conf, st_2_conf, st_3_conf, st_4_conf, st_1_reinf, st_2_reinf, st_3_reinf, confidence, motivation, interview_qual, interviewer_qual, self_assessment, age, gender, everything())

### Transform data to long format

sos_long <- sos_wrangle %>% pivot_longer(cols = starts_with("stage"), names_to = "time", values_to = "detail")

### Code time variables for interrupted time series regression

sos_long <-sos_long %>% 
  mutate(
    after = case_when(
      time == "stage_1" ~ 0,
      time == "stage_2" ~ 0,
      time == "stage_3" ~ 0,
      time == "stage_4" ~ 1,
      time == "stage_5" ~ 2,
      time == "stage_6" ~ 3
    ),
    
    treatment = case_when(
      sos_long$time == "stage_1" ~ 0,
      sos_long$time == "stage_2" ~ 0,
      sos_long$time == "stage_3" ~ 0,
      sos_long$time == "stage_4" ~ 1,
      sos_long$time == "stage_5" ~ 1,
      sos_long$time == "stage_6" ~ 1
    ),
    
    time = case_when(
      sos_long$time == "stage_1" ~ 1,
      sos_long$time == "stage_2" ~ 2,
      sos_long$time == "stage_3" ~ 3,
      sos_long$time == "stage_4" ~ 4,
      sos_long$time == "stage_5" ~ 5,
      sos_long$time == "stage_6" ~ 6
    ))

### Factor condition

sos_long$style <- factor(sos_long$style, levels = c("standard", "cool_off", "diversion"))


# Export data --------------------------------------------------

write.csv(
  sos_wrangle,
  "./data/sos_wrangle.csv",
  row.names = FALSE
)

write.csv(
  sos_long,
  "./data/sos_long.csv",
  row.names = FALSE
)