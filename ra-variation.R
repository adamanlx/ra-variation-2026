library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)
library(emmeans)

df <- read.csv('transformed_data.csv', header=T) %>%
  mutate(NORTHWEST = ifelse(
    REGION %in% c("Burera", "Musanze", "Rulindo", "Gakenke", "Rubavu"),
    "Northwest", "Elsewhere"
  )) %>%
  mutate(NORTHWEST_DIALECT = ifelse(
    IKIGOYI | IKIRERA,
    "Northwest",
    "Elsewhere"
  )) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  mutate(SCALED_AWARENESS = scale(HAVE_YOU_HEARD_THIS)) %>%
  mutate(FRAME = ifelse(FRAME == "PART", "PTCP", FRAME)) %>%
  mutate(TAM = factor(TAM, levels=c("HAB", "PROG", "FUT"))) %>%
  mutate(FRAME = factor(FRAME, levels=c(
    "INDfinal", "INDDP", "INDko", "INDngo", "NEG", "REL", "PTCP"
  ))) %>%
  mutate(MORPHEME = factor(MORPHEME, levels=c("0", "ra", "p"))) %>%
  # picked ra as reference level since the slope is shallower -> want more about it
  mutate(GENDER = factor(GENDER, levels=c("male", "female")))
  # picked men as reference level bc story is about men

df_avg = df %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME) %>%
  summarize(WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            HAVE_YOU_HEARD_THIS = mean(HAVE_YOU_HEARD_THIS))

# Descriptive stats

df %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME)) %>%
  group_by(TAMMORPHEME, FRAME) %>%
  summarize(SCORE = mean(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=FRAME, values_from=SCORE)

# make this about researcher expectations vs. reality

df_avg %>%
  filter(MORPHEME != "p") %>%
  select(-HAVE_YOU_HEARD_THIS) %>%
  pivot_wider(names_from=MORPHEME, values_from=WOULD_YOU_SAY_THIS) %>%
  ggplot(aes(ra, `0`)) + geom_jitter() + facet_grid(TAM ~ FRAME) +
  geom_hline(yintercept = 3) + geom_vline(xintercept = 3) +
  annotate("rect", xmin=3, xmax=Inf, ymin=3, ymax=Inf, fill="green", alpha=0.1) +
  annotate("rect", xmin=3, xmax=Inf, ymin=3, ymax=-Inf, fill="blue", alpha=0.1) +
  annotate("rect", xmin=3, xmax=-Inf, ymin=3, ymax=Inf, fill="yellow", alpha=0.2)

df_avg %>%
  filter(TAM == "PROG") %>%
  mutate(MORPHEME = ifelse(MORPHEME %in% c("ra", "0"), "affix", "p")) %>%
  group_by(RESPONDENT_ID, MORPHEME, FRAME) %>%
  select(-TAM) %>%
  summarize(MAX_SCORE = max(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=MORPHEME, values_from=MAX_SCORE) %>%
  ggplot(aes(affix, p)) + geom_jitter() + facet_wrap(~ FRAME) +
  geom_hline(yintercept = 2) + geom_vline(xintercept = 2) +
  annotate("rect", xmin=2, xmax=Inf, ymin=2, ymax=Inf, fill="green", alpha=0.1) +
  annotate("rect", xmin=2, xmax=Inf, ymin=2, ymax=-Inf, fill="blue", alpha=0.1) +
  annotate("rect", xmin=2, xmax=-Inf, ymin=2, ymax=Inf, fill="yellow", alpha=0.2) +
  xlab("highest score, either affixal strategy") + ylab("periphrastic")

# awareness: slightly looser version of what you would yourself say
# no pattern to the ones where people said "I've heard this but don't say it"

df %>%
  ggplot(aes(WOULD_YOU_SAY_THIS, HAVE_YOU_HEARD_THIS))+
  geom_jitter() +
  labs(x="scaled acceptance scores", y="scaled awareness scores")

summary(
  lmer(
    WOULD_YOU_SAY_THIS ~ HAVE_YOU_HEARD_THIS + (1 | RESPONDENT_ID),
    data=df
  )
)

df_avg %>%
  filter(HAVE_YOU_HEARD_THIS >= 4, WOULD_YOU_SAY_THIS <= 2) %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME)) %>%
  group_by(TAMMORPHEME, FRAME) %>%
  mutate(WOULD_YOU_SAY_THIS = 1) %>%
  summarize(COUNT = sum(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=FRAME, values_from=COUNT)

# general trend: young men are rating everything higher across the board
# the women do not show a similar age effect
# no effects of region or dialect

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "p")
  )
)
# filter out p so it doesn't correct for an additional thing

# over time,
# men come to like both ra and 0
# women come to distinguish ra and 0 with preference for ra

# with reference levels as gender:male and morpheme:0,
# young men like 0 more
# women like 0 less than men, attenuated by increasing age
# for women, the difference between ra-0 is bigger than it is for men, attenuated with increasing age

# we know that young men like 0, so do they also like ra?
# AGE:MORPHEMEra was not significant means that men were not treating the two morphemes differently over time,
# but when we set the reference level to ra (i.e. not comparing it against 0 just looking at ra on its own),
# there was a marginal effect of age such that young men rated ra- higher

# TO DO: try to replicate numbers from manually setting the reference levels
# in emtrends

emtrends(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "p")
  ),
  pairwise ~ MORPHEME | GENDER,
  var = "AGE"
)

df %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="Age", y="Rating", color="Gender") +
  facet_wrap(~ MORPHEME)

# When we analyzed responses in specific morphosyntactic environments broken out
# by TAM and frame, we only found significant effects of demographic variables in
# PROG/FUT negation.

# women are developing a preference for ra over time
# men are developing a preference for 0 over time

# reference levels: gender=male, morpheme=0
# young men like 0
# women like 0 less than men, attenuated by increasing age
# marginally, men like ra less than they like 0, attenuated by increasing age in sig. interaction
# GENDER:MORPHEME interaction: for women, the ra-0 difference is greater than the difference for men, attenuated by increasing age

# the significant AGE:MORPHEME interaction shows that age effect is greater for 0 than for ra for men 
# this prompts us to ask, if young men like 0, do young men like ra?
# setting the reference level to ra, we find no effect of age. -> change over time young men like 0 more than ra over time

# seeing the difference for women only prompts us to ask, how do they treat each morpheme individually?
# setting the reference level to women, we find no effect of age on either morpheme

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * relevel(GENDER, "female") * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

df %>%
  filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG", TAM %in% c("PROG", "FUT")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="Age", y="Rating", color="Gender") +
  facet_wrap(~ MORPHEME)
