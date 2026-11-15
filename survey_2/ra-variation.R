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
  mutate(MORPHEME = ifelse(MORPHEME == "0", "CJ", MORPHEME)) %>%
  mutate(MORPHEME = ifelse(MORPHEME == "p", "periphrastic", MORPHEME)) %>%
  mutate(MORPHEME = factor(MORPHEME, levels=c("CJ", "ra", "periphrastic"))) %>%
  mutate(GENDER = factor(GENDER, levels=c("male", "female")))

# averaged within participant and condition
df_avg = df %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME) %>%
  summarize(WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            HAVE_YOU_HEARD_THIS = mean(HAVE_YOU_HEARD_THIS))

### 5.3.1 Respondents

# age, gender
df %>% select(RESPONDENT_ID, AGE) %>% unique() %>% summary()
df %>% filter(GENDER == "female") %>% select(RESPONDENT_ID, AGE) %>% unique() %>% summary()
df %>% filter(GENDER == "male") %>% select(RESPONDENT_ID, AGE) %>% unique() %>% summary()

# region
df %>%
  select(RESPONDENT_ID, REGION) %>%
  unique() %>%
  group_by(REGION) %>%
  summarize(FREQUENCY = n())

# dialect
df %>%
  select(RESPONDENT_ID, IKIRERA, IKIGOYI, IKINYAGISAKA, IKINYAMBO, IGIKIGA) %>%
  unique() %>%
  colSums()

### 5.3.2 Demographic effects across the board

df %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="age", y="acceptance score", color="gender") +
  facet_wrap(~ MORPHEME)

# todo: redo in emtrends?
summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "periphrastic")
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "periphrastic")
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * relevel(GENDER, "female") * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "periphrastic")
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * relevel(GENDER, "female") * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>% filter(MORPHEME != "periphrastic")
  )
)

### 5.3.3 Demographic effects across morphosyntactic environments

# just prog/fut negation
summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "CJ"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "CJ"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * relevel(GENDER, "female") * MORPHEME + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "CJ"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * relevel(GENDER, "female") * relevel(MORPHEME, "ra") + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "CJ"), FRAME == "NEG", TAM %in% c("PROG", "FUT"))
  )
)

df %>%
  filter(MORPHEME %in% c("ra", "CJ"), FRAME == "NEG", TAM %in% c("PROG", "FUT")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(as_factor(GENDER), "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="age", y="rating", color="gender") +
  facet_wrap(~ MORPHEME)

# everywhere that ISN'T prog/fut negation
summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "CJ")) %>%
      filter((FRAME == "NEG" & TAM %in% c("PROG", "FUT")) == FALSE)
  )
)

### 5.3.4 Overall responses across morphosyntactic environments and task effects

df_means <- df_avg %>%
  group_by(TAM, MORPHEME, FRAME) %>%
  summarize(
    WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
    HAVE_YOU_HEARD_THIS = mean(HAVE_YOU_HEARD_THIS)) %>%
  mutate(TYPE = "mean")

# ra- vs. CJ
# (in order to generate the shaded quadrants seen in Figure 4, I
# rendered this multiple times with different quadrants set to green
# and then composited in image-editing software)
rbind(df_avg, df_means) %>%
  filter(MORPHEME != "periphrastic") %>%
  select(-HAVE_YOU_HEARD_THIS) %>%
  pivot_wider(names_from=MORPHEME, values_from=WOULD_YOU_SAY_THIS) %>%
  ggplot(aes(ra, CJ, color=TYPE)) + 
  geom_jitter(width=0.2, height=0.2, aes(shape=TYPE)) +
  facet_grid(TAM ~ FRAME, switch="y") +
  geom_hline(yintercept = 3) + geom_vline(xintercept = 3) +
  annotate("rect", xmin=3, xmax=Inf, ymin=3, ymax=Inf, fill="green", alpha=0) +
  annotate("rect", xmin=3, xmax=Inf, ymin=3, ymax=-Inf, fill="green", alpha=0.1) +
  annotate("rect", xmin=3, xmax=-Inf, ymin=3, ymax=Inf, fill="green", alpha=0) +
  labs(x = "score, ra-", y = "score, CJ") +
  scale_color_manual(
    values = c(
      "mean" = "red"
    ),
    na.value = "black"
  ) +
  scale_shape_manual(
    values = c(
      "mean" = 15
    ),
    na.value = 16
  ) +
  theme(legend.position = "none")

# periphrastic vs. preferred affixal strategy
rbind(df_avg, df_means) %>%
  filter(TAM == "PROG") %>%
  mutate(MORPHEME = ifelse(MORPHEME %in% c("ra", "CJ"), "affix", "periphrastic")) %>%
  group_by(RESPONDENT_ID, MORPHEME, FRAME, TYPE) %>%
  select(-TAM) %>%
  summarize(MAX_SCORE = max(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=MORPHEME, values_from=MAX_SCORE) %>%
  ggplot(aes(affix, periphrastic, color=TYPE)) +
  geom_jitter(width=0.2, height=0.2, aes(shape=TYPE)) + facet_wrap(~ FRAME) +
  geom_hline(yintercept = 3) + geom_vline(xintercept = 3) +
  xlab("highest average score, either affixal strategy") + ylab("score, periphrastic") +
  scale_color_manual(
    values = c(
      "mean" = "red"
    ),
    na.value = "black"
  ) +
  scale_shape_manual(
    values = c(
      "mean" = 15
    ),
    na.value = 16
  ) +
  theme(legend.position = "none")

# some participants seem to not like any strategy for PROG REL
df_avg %>%
  filter(TAM == "PROG", FRAME == "REL") %>%
  select(-c(HAVE_YOU_HEARD_THIS, FRAME)) %>%
  pivot_wider(names_from=MORPHEME, values_from=WOULD_YOU_SAY_THIS) %>%
  filter(CJ < 3, ra < 3, periphrastic < 3) %>%
  nrow()

### 5.3.5 Awareness

df %>%
  ggplot(aes(WOULD_YOU_SAY_THIS, HAVE_YOU_HEARD_THIS))+
  geom_jitter() + geom_smooth(method="lm", se=FALSE) +
  labs(x="acceptance scores", y="awareness scores")

summary(
  lmer(
    WOULD_YOU_SAY_THIS ~ HAVE_YOU_HEARD_THIS + (1 | RESPONDENT_ID),
    data=df
  )
)

# no pattern to the ones where people said "I've heard this but don't say it"
df_avg %>%
  filter(HAVE_YOU_HEARD_THIS >= 4, WOULD_YOU_SAY_THIS <= 2) %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME)) %>%
  group_by(TAMMORPHEME, FRAME) %>%
  mutate(WOULD_YOU_SAY_THIS = 1) %>%
  summarize(COUNT = sum(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=FRAME, values_from=COUNT)