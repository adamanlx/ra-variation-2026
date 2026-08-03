library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)

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
  mutate(MORPHEME = relevel(as_factor(MORPHEME), "ra")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  mutate(SCALED_AWARENESS = scale(HAVE_YOU_HEARD_THIS)) %>%
  mutate(FRAME = ifelse(FRAME == "PART", "PTCP", FRAME)) %>%
  mutate(TAM = factor(TAM, levels=c("HAB", "PROG", "FUT"))) %>%
  mutate(FRAME = factor(FRAME, levels=c(
    "INDfinal", "INDDP", "INDko", "INDngo", "NEG", "REL", "PTCP"
  )))

df_avg = df %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME) %>%
  summarize(WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            HAVE_YOU_HEARD_THIS = mean(HAVE_YOU_HEARD_THIS))

accepts_prog = df_avg %>%
  filter(TAM == "PROG", MORPHEME == "ra", FRAME == "INDfinal",
         WOULD_YOU_SAY_THIS > 1) %>%
  pull(RESPONDENT_ID) %>%
  unique()
accepts_fut = df_avg %>%
  filter(TAM == "FUT", MORPHEME == "ra", FRAME == "INDfinal",
         WOULD_YOU_SAY_THIS > 1) %>%
  pull(RESPONDENT_ID) %>%
  unique()

# Descriptive stats

df %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME)) %>%
  group_by(TAMMORPHEME, FRAME) %>%
  summarize(SCORE = mean(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=FRAME, values_from=SCORE)

df_avg %>%
  filter(MORPHEME != "p") %>%
  filter(
    (TAM == "HAB") |
      (TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) |
      (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)) %>%
  pivot_wider(names_from=MORPHEME, values_from=WOULD_YOU_SAY_THIS) %>%
  ggplot(aes(ra, `0`)) + geom_jitter() + facet_grid(TAM ~ FRAME) +
  geom_hline(yintercept = 2) + geom_vline(xintercept = 2) +
  annotate("rect", xmin=2, xmax=Inf, ymin=2, ymax=Inf, fill="green", alpha=0.1) +
  annotate("rect", xmin=2, xmax=Inf, ymin=2, ymax=-Inf, fill="blue", alpha=0.1) +
  annotate("rect", xmin=2, xmax=-Inf, ymin=2, ymax=Inf, fill="yellow", alpha=0.2)

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

# general trend: young men are rating both morphemes higher across the board
# the women do not show a similar age effect
# no effects of region or dialect

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0"))
  )
)

df %>%
  filter(MORPHEME %in% c("ra", "0")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="Age", y="Rating", color="Gender") +
  facet_wrap(~ MORPHEME)


# when we break it out by frame, negation is the only one that comes
# out as significant, suggesting negation could be driving the broader effect

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG")
  )
)

df %>%
  filter(MORPHEME %in% c("ra", "0"), FRAME == "NEG") %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="Age", y="Rating", color="Gender") +
  facet_wrap(~ MORPHEME)