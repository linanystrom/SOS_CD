# Basic setup --------------------------------------------------

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr", "readxl", "ggplot2", "lme4", "TOSTER")

lapply(packages, library, character.only = TRUE)

# Import data --------------------------------------------------

sos      <- read.csv("./data/sos_wrangle.csv")

sos_long <- read.csv("./data/sos_long.csv")

# Hypothesis testing - Information disclosure --------------------------------------------------

##Descriptives

info_desc <- sos_long %>% 
  group_by(style, time) %>% 
  summarise(
    Mean = mean(detail, na.rm = TRUE),
    SD = sd(detail, na.rm = TRUE),
    Median = median(detail, na.rm = TRUE),
    SE = SD/sqrt(n()),
    Upper = Mean + (1.96*SE),
    Lower = Mean - (1.96*SE)
  )


## Plot information disclosure 

info_plot <- ggplot(sos_long,
                    aes(
                      x = factor(time),
                      y = detail
                    )) +
  facet_wrap(. ~ style) +
  geom_line(
    aes(
      group = ID
    ),
    size = .05,
    color = "grey",
    position = position_jitter(width = .15, height = .15),
    alpha = .20
  ) +
  geom_line(
    data = info_desc,
    aes(
      x = time,
      y = Mean,
      group = style
    ),
    color = "red",
    size = 1.5
  ) +
  geom_errorbar(
    data = info_desc,
    aes(
      x = time,
      ymax = Upper,
      ymin = Lower,
      group = style
    ),
    inherit.aes = FALSE,
    color = "red",
    width = .25
  ) +
  labs(
    y = "Information disclosure",
    x = "Phase"
  ) +
  scale_x_discrete(
    labels = c("1", "2", "3","4","5","6") ## nope
  ) +
  coord_cartesian(
    ylim = c(0, 5)
  ) +
  theme_classic()

## Interrupted time series linear mixed effects model

info_model_1 <- lmer(detail ~ time  + treatment + after + style + (1|crime_order/ID) + (1|interviewer), data = sos_long, REML = FALSE)

summary(info_model_1)

info_model_int <- lmer(detail ~ time  + treatment + after + style + time*style + treatment*style + after*style + (1|crime_order/ID) + (1|interviewer), data = sos_long, REML = FALSE)

summary(info_model_int)

## Comparing regression models

comp_model_anova <- anova(info_model_1, info_model_int)


# Hypothesis testing - Self-assessment of performance --------------------------------------------------

## Descriptives

perf_desc <- sos %>%
  group_by(style) %>%
  summarise(
    Mean = mean(self_assessment, na.rm = TRUE),
    SD = sd(self_assessment, na.rm = TRUE),
    Median = median(self_assessment, na.rm = TRUE)
  )

## Plots self-assessment

perf_plot <- ggplot(sos,
                    aes(
                      x = self_assessment
                    )) +
  facet_wrap(. ~ style) +
  geom_histogram(
    binwidth = .65,
    color = "black",
    fill = "darkgrey"
  ) +
  theme_classic()

## Comparison of each condition

t_self_DS <- t.test(self_assessment ~ style, data = filter(sos, style == "standard" | style == "cool_off"))

t_self_DR <-t.test(self_assessment ~ style, data = filter(sos, style == "standard" | style == "diversion"))

t_self_RS <-t.test(self_assessment ~ style, data = filter(sos, style == "cool_off" | style == "diversion"))

## Comparison to the midpoint

t_self_mid_D <- t.test(filter(sos, style == "standard")$self_assessment, mu = 3)

t_self_mid_S <- t.test(filter(sos, style == "cool_off")$self_assessment, mu = 3)

t_self_mid_R <- t.test(filter(sos, style == "diversion")$self_assessment, mu = 3)

## TOST (Only performed if initial three t-test are nonsignificant)

self_table <- sos %>%
  filter(complete.cases(self_assessment)) %>% 
  group_by(style) %>%
  summarise(
    Mean = mean(self_assessment, na.rm = TRUE),
    SD = sd(self_assessment, na.rm = TRUE),
    Median = median(self_assessment, na.rm = TRUE),
    n = n()
  ) %>% 
  ungroup()

standard_mean <- filter(self_table, style == "standard")$Mean
cool_off_mean <- filter(self_table, style == "cool_off")$Mean
diversion_mean <- filter(self_table, style == "diversion")$Mean

standard_sd <- filter(self_table, style == "standard")$SD
cool_off_sd <- filter(self_table, style == "cool_off")$SD
diversion_sd <- filter(self_table, style == "diversion")$SD

standard_n <- filter(self_table, style == "standard")$n
cool_off_n <- filter(self_table, style == "cool_off")$n
diversion_n <- filter(self_table, style == "diversion")$n

### Standard vs. Cool Off

tost_self_DS <- TOSTtwo(m1 = standard_mean, m2 = cool_off_mean , sd1 = standard_sd, sd2 =  cool_off_sd, n1 = standard_n, n2 = cool_off_n, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### Standard vs. Diversion

tost_self_DR <-TOSTtwo(m1 = standard_mean, m2 = diversion_mean, sd1 = standard_sd, sd2 = diversion_sd, n1 = standard_n, n2 = diversion_n, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### Cool Off vs. Diversion

tost_self_RS <-TOSTtwo(m1 = cool_off_mean, m2 = diversion_mean, sd1 = cool_off_sd, sd2 = diversion_sd, n1 = cool_off_n, n2 = diversion_n, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)


# Hypothesis testing - Interaction quality --------------------------------------------------

## Descriptives - Interview

interview_desc <- sos %>%
  group_by(style) %>%
  summarise(
    Mean = mean(interview_qual, na.rm = TRUE),
    SD = sd(interview_qual, na.rm = TRUE),
    Median = median(interview_qual, na.rm = TRUE)
  )

## Plot

interview_plot <- ggplot(sos,
                         aes(
                           x = interview_qual
                         )) +
  facet_wrap(. ~ style) +
  geom_histogram(
    binwidth = .65,
    color = "black",
    fill = "darkgrey"
  ) +
  theme_classic()

## Descriptives - Interviewer

interviewer_desc <- sos %>%
  group_by(style) %>%
  summarise(
    Mean = mean(interviewer_qual, na.rm = TRUE),
    SD = sd(interviewer_qual, na.rm = TRUE),
    Median = median(interviewer_qual, na.rm = TRUE)
  )

## Plot

interviewer_plot <- ggplot(sos,
                           aes(
                             x = interviewer_qual
                           )) +
  facet_wrap(. ~ style) +
  geom_histogram(
    binwidth = .65,
    color = "black",
    fill = "darkgrey"
  ) +
  theme_classic()

## Comparison - Interview

t_inteview_DS <- t.test(interview_qual ~ style, data = filter(sos, style == "standard" | style == "cool_off"))

t_inteview_DR <-t.test(interview_qual ~ style, data = filter(sos, style == "standard" | style == "diversion"))

t_inteview_RS <-t.test(interview_qual ~ style, data = filter(sos, style == "diversion" | style == "cool_off"))

## TOST - These will only be used if the t-tests above are nonsignificant

interview_table <- sos %>%
  filter(complete.cases(interview_qual)) %>% 
  group_by(style) %>%
  summarise(
    Mean = mean(interview_qual, na.rm = TRUE),
    SD = sd(interview_qual, na.rm = TRUE),
    Median = median(interview_qual, na.rm = TRUE),
    n = n()
  ) %>% 
  ungroup()

standard_mean_2 <- filter(interview_table, style == "standard")$Mean
diversion_mean_2 <- filter(interview_table, style == "diversion")$Mean
cool_off_mean_2 <- filter(interview_table, style == "cool_off")$Mean

standard_sd_2 <- filter(interview_table, style == "standard")$SD
diversion_sd_2 <- filter(interview_table, style == "diversion")$SD
cool_off_sd_2 <- filter(interview_table, style == "cool_off")$SD

standard_n_2 <- filter(interview_table, style == "standard")$n
diversion_n_2 <- filter(interview_table, style == "diversion")$n
cool_off_n_2 <- filter(interview_table, style == "cool_off")$n

### standard vs. cool_off

tost_interview_DS <- TOSTtwo(m1 = standard_mean_2, m2 =cool_off_mean_2 , sd1 = standard_sd_2, sd2 = cool_off_sd_2, n1 = standard_n_2, n2 =cool_off_n_2, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### standard vs. diversion

tost_interview_DR <-TOSTtwo(m1 = standard_mean_2, m2 = diversion_mean_2, sd1 = standard_sd_2, sd2 = diversion_sd_2, n1 = standard_n_2, n2 = diversion_n_2, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### diversion vs. cool_off

tost_interview_RS <-TOSTtwo(m1 =cool_off_mean_2, m2 = diversion_mean_2, sd1 =cool_off_sd_2, sd2 = diversion_sd_2, n1 =cool_off_n_2, n2 = diversion_n_2, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

## Comparison to negative endpoint

t_interview_neg_D <- t.test(filter(sos, style == "standard")$interview_qual, mu = 1)

t_interview_neg_S <-t.test(filter(sos, style == "cool_off")$interview_qual, mu = 1)

t_interview_neg_R <-t.test(filter(sos, style == "diversion")$interview_qual, mu = 1)

## Comparison - Interviewer

t_interviewer_DS <- t.test(interviewer_qual ~ style, data = filter(sos, style == "standard" | style == "cool_off"))

t_interviewer_DR <-t.test(interviewer_qual ~ style, data = filter(sos, style == "standard" | style == "diversion"))

t_interviewer_RS <-t.test(interviewer_qual ~ style, data = filter(sos, style == "diversion" | style == "cool_off"))

## TOST - These will only be used if the t-tests above are nonsignificant

interviewer_table <- sos %>%
  filter(complete.cases(interview_qual)) %>% 
  group_by(style) %>%
  summarise(
    Mean = mean(interviewer_qual, na.rm = TRUE),
    SD = sd(interviewer_qual, na.rm = TRUE),
    Median = median(interviewer_qual, na.rm = TRUE),
    n = n()
  ) %>% 
  ungroup()

standard_mean_3 <- filter(interviewer_table, style == "standard")$Mean
diversion_mean_3 <- filter(interviewer_table, style == "diversion")$Mean
cool_off_mean_3 <- filter(interviewer_table, style == "cool_off")$Mean

standard_sd_3 <- filter(interviewer_table, style == "standard")$SD
diversion_sd_3 <- filter(interviewer_table, style == "diversion")$SD
cool_off_sd_3 <- filter(interviewer_table, style == "cool_off")$SD

standard_n_3 <- filter(interviewer_table, style == "standard")$n
diversion_n_3 <- filter(interviewer_table, style == "diversion")$n
cool_off_n_3 <- filter(interviewer_table, style == "cool_off")$n

### standard vs. cool_off

tost_interviewer_DS <- TOSTtwo(m1 = standard_mean_3, m2 = cool_off_mean_3 , sd1 = standard_sd_3, sd2 = cool_off_sd_3, n1 = standard_n_3, n2 = cool_off_n_3, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### standard vs. diversion

tost_interviewer_DR <- TOSTtwo(m1 = standard_mean_3, m2 = diversion_mean_3, sd1 = standard_sd_3, sd2 = diversion_sd_3, n1 = standard_n_3, n2 = diversion_n_3, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)

### diversion vs. cool_off

tost_interviewer_RS <- TOSTtwo(m1 = cool_off_mean_3, m2 = diversion_mean_3, sd1 = cool_off_sd_3, sd2 = diversion_sd_3, n1 = cool_off_n_3, n2 = diversion_n_3, low_eqbound_d = -.20, high_eqbound_d = .20, alpha = .05, plot = FALSE, var.equal = FALSE)


## Comparison to the negative endpoint

t_interviewer_neg_D <- t.test(filter(sos, style == "standard")$interviewer_qual, mu = 1)

t_interviewer_neg_S <- t.test(filter(sos, style == "cool_off")$interviewer_qual, mu = 1)

t_interviewer_neg_R <- t.test(filter(sos, style == "diversion")$interviewer_qual, mu = 1)
