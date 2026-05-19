library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)
library(ggpubr)
library(Hmisc)

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
  mutate(GENDER = relevel(as_factor(GENDER), "male")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  mutate(SCALED_AWARENESS = scale(HAVE_YOU_HEARD_THIS))

df_avg <- df %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME) %>%
  summarize(
    WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
    SCALED_WOULD_YOU_SAY_THIS = mean(SCALED_WOULD_YOU_SAY_THIS)
  )

# tag respondents based on whether they accept prog or fut

accepts_prog = df_avg %>%
  filter(TAM == "PROG", MORPHEME == "ra", FRAME == "INDfinal",
         SCALED_WOULD_YOU_SAY_THIS > 0) %>%
  pull(RESPONDENT_ID) %>%
  unique()
accepts_fut = df_avg %>%
  filter(TAM == "FUT", MORPHEME == "ra", FRAME == "INDfinal",
         SCALED_WOULD_YOU_SAY_THIS > 0) %>%
  pull(RESPONDENT_ID) %>%
  unique()

# SECTION 5.3.2 OVERALL RESPONSES

df %>%
  mutate(TAM = ifelse(TAM == "FUT", "PROG", TAM)) %>%
  group_by(TAM, MORPHEME, FRAME) %>%
  dplyr::summarize(MEAN_SCORE = mean(WOULD_YOU_SAY_THIS),
            SD_SCORE = sd(WOULD_YOU_SAY_THIS),
            MEAN_SCALED_SCORE = mean(SCALED_WOULD_YOU_SAY_THIS),
            SD_SCALED_SCORE = sd(SCALED_WOULD_YOU_SAY_THIS)) %>%
  arrange(SD_SCORE) %>%
  print(n=35)

df %>%
  filter(
    TAM == "HAB" |
      (TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) |
      (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)
  ) %>%
  mutate(TAM = ifelse(TAM == "FUT", "PROG", TAM)) %>%
  group_by(TAM, MORPHEME, FRAME) %>%
  dplyr::summarize(MEAN_SCORE = mean(WOULD_YOU_SAY_THIS),
                   SD_SCORE = sd(WOULD_YOU_SAY_THIS),
                   MEAN_SCALED_SCORE = mean(SCALED_WOULD_YOU_SAY_THIS),
                   SD_SCALED_SCORE = sd(SCALED_WOULD_YOU_SAY_THIS)) %>%
  arrange(MEAN_SCALED_SCORE) %>%
  filter(MORPHEME != "p", !FRAME %in% c("INDngo", "INDko")) %>%
  print(n=35)

df %>%
  filter(MORPHEME != "p") %>%
  filter(
    TAM == "HAB" |
    TAM == "PROG" & RESPONDENT_ID %in% accepts_prog |
    TAM == "FUT" & RESPONDENT_ID %in% accepts_fut
  ) %>%
  mutate(TAM = ifelse(TAM == "HAB", "HAB", "PROG/FUT")) %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME, AGE) %>%
  summarize(AVERAGE_ACCEPTANCE = mean(SCALED_WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=MORPHEME, values_from=AVERAGE_ACCEPTANCE) %>%
  ggplot(aes(ra, `0`)) + geom_jitter(width=0.1, height=0.1) +
  labs(x="ra-", y="unmarked verb") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=0) + geom_hline(yintercept=0) +
  facet_grid(rows = vars(TAM), cols = vars(FRAME))

# awareness

df %>%
  ggplot(aes(SCALED_WOULD_YOU_SAY_THIS, SCALED_AWARENESS))+
  geom_jitter()+geom_smooth(method="lm")+
  labs(x="scaled acceptance scores", y="scaled awareness scores")

summary(
  lmer(
    SCALED_AWARENESS ~ SCALED_WOULD_YOU_SAY_THIS + (1 | RESPONDENT_ID),
    data=df
  )
)

# GENERAL TRENDS

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(MORPHEME %in% c("ra", "0")) %>%
      filter(TAM %in% c("PROG", "FUT")) %>%
      filter(FRAME %in% c("INDngo", "NEG", "REL", "PART"))
  )
)

summary(
  lmer(
    WOULD_YOU_SAY_THIS
    ~ MORPHEME + (1 | TAM) + (1 | FRAME) + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df
  )
)

df %>%
  ggplot(aes(WOULD_YOU_SAY_THIS)) + geom_histogram() +
  labs(x="Thinking specifically about the verb, would you yourself say this?") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Never", "Sometimes", "I don't know", "Usually", "Always")
  )

df %>%
  filter(MORPHEME %in% c("ra", "0")) %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS, color=relevel(GENDER, "female"))) +
  geom_jitter() + geom_smooth(method="lm", se=TRUE) +
  labs(x="Age", y="Rating", color="Gender") +
  facet_wrap(~ MORPHEME)


# 5.3.3 ACCEPTANCE OF TAM READING; INDEPENDENCE OF SYNTACTIC FRAME

accepts_prog %>% unique() %>% length()
accepts_fut %>% unique() %>% length()
intersect(accepts_prog, accepts_fut) %>% unique() %>% length()
union(accepts_prog, accepts_fut) %>% unique() %>% length()

df_avg %>%
  filter(MORPHEME == "ra", FRAME == "INDfinal", TAM %in% c("PROG", "FUT")) %>%
  pivot_wider(id_cols = RESPONDENT_ID, names_from = TAM, values_from = SCALED_WOULD_YOU_SAY_THIS) %>%
  ggplot(aes(PROG, FUT))+geom_jitter(width=0.1, height=0.1)+
  labs(x="present progressive", y="near future")+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)

# are PROG and FUT comparable?

prog_fut_responses <- df_avg %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME, sep='')) %>%
  pivot_wider(id_cols=c(RESPONDENT_ID, FRAME), names_from=TAMMORPHEME, values_from=SCALED_WOULD_YOU_SAY_THIS)

grid.arrange(
  prog_fut_responses %>%
    ggplot(aes(PROGra, FUTra)) + geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm") +
    xlab("PROG, ra-") +
    ylab("FUT, ra-"),
  
  prog_fut_responses %>%
    ggplot(aes(PROG0, FUT0)) + geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm") +
    xlab("PROG, ra-less verb") +
    ylab("FUT, ra-less verb"),
  
  ncol=2
)

summary(lm(PROGra ~ FUTra, prog_fut_responses))
summary(lm(PROG0 ~ FUT0, prog_fut_responses))

df_avg %>%
  filter(
    (TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) |
      (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)
  ) %>%
  group_by(MORPHEME, FRAME) %>%
  summarize(MEAN_WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            MEAN_SCALED_WOULD_YOU_SAY_THIS = mean(SCALED_WOULD_YOU_SAY_THIS))

# SECTION 5.3.5 ACCEPTABILITY OF RA-LESS VERBS BEFORE NGO

summary(lm(WOULD_YOU_SAY_THIS ~ AGE * GENDER * NORTHWEST_DIALECT * MORPHEME,
         data=df %>%
           filter(TAM=="HAB", FRAME=="INDngo")))

df %>%
  filter(MORPHEME != "p", TAM == "HAB", FRAME == "INDngo") %>%
  mutate(TAM = ifelse(TAM == "HAB", "HAB", "PROG/FUT")) %>%
  group_by(RESPONDENT_ID, TAM, MORPHEME, FRAME) %>%
  summarize(AVERAGE_ACCEPTANCE = mean(SCALED_WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=MORPHEME, values_from=AVERAGE_ACCEPTANCE) %>%
  ggplot(aes(ra, `0`)) + geom_jitter(width=0.1, height=0.1) +
  labs(x="ra", y="0") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=0) + geom_hline(yintercept=0)

# SECTION 5.3.6 ACCEPTABILITY OF PROG/FUT ra- across SYNTACTIC FRAMES

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)),
             MORPHEME != "p")
  )
)

df %>%
  filter(FRAME %in% c("NEG"), MORPHEME != "p",
         ((TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut))) %>%
  ggplot(aes(AGE, WOULD_YOU_SAY_THIS)) +
  geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm", se=FALSE) +
  labs(x="Age", y="Scaled acceptance", color="Gender") +
  facet_wrap(~MORPHEME)


summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)),
             MORPHEME != "p")
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)),
             MORPHEME != "p")
  )
)

df %>%
  filter(FRAME %in% c("REL", "PART"), MORPHEME %in% c("ra", "0"),
         ((TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut))) %>%
  ggplot(aes(AGE, SCALED_WOULD_YOU_SAY_THIS, color=relevel(GENDER, "female"))) + geom_jitter(width=0.1, height=0.1) +
  facet_grid(rows = vars(MORPHEME), cols = vars(FRAME)) +
  labs(x="Age", y="Scaled acceptance", color="Gender")

# SECTION 5.3.7 IMPLICATIONAL HIERARCHIES?

neg_rel_part <- df_avg %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL", "PART"), MORPHEME == "ra",
         (TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)) %>%
  pivot_wider(id_cols=c(RESPONDENT_ID, TAM), names_from=FRAME, values_from=SCALED_WOULD_YOU_SAY_THIS)

ggarrange(
  neg_rel_part %>%
    ggplot(
      aes(REL, NEG)
    ) + geom_jitter(width = 0.1, height = 0.1) +
    geom_smooth(method="lm") +
    xlab("relativization") + ylab("negation")+
    xlim(-1.5, 1.5)+ylim(-1.5, 1.5),
  
  neg_rel_part %>%
    ggplot(
      aes(PART, NEG)
    ) + geom_jitter(width = 0.1, height = 0.1) +
    xlab("participial") + ylab("negation")+
    xlim(-1.5, 1.5)+ylim(-1.5, 1.5),
  
  neg_rel_part %>%
    ggplot(
      aes(PART, REL)
    ) + geom_jitter(width = 0.1, height = 0.1) +
    xlab("relativization") + ylab("participial")+
    xlim(-1.5, 1.5)+ylim(-1.5, 1.5),
  
  ncol=3,
  common.legend=TRUE,
  legend="bottom"
)


summary(
  lmer(NEG ~ REL * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part))

summary(
  lmer(NEG ~ PART * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part))

summary(
  lmer(PART ~ REL * TAM + (1 | RESPONDENT_ID),
       data = neg_rel_part))

# SECTION 5.3.8 PERIPHRASTICS

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * PERIPHRASTIC + (1 | RESPONDENT_ID) + (1 | CONDITION_NAME),
    data = df %>%
      filter(TAM == "PROG") %>%
      select(CONDITION_NAME, TAM, MORPHEME, FRAME, SCALED_WOULD_YOU_SAY_THIS,
             RESPONDENT_ID, AGE, GENDER, NORTHWEST, NORTHWEST_DIALECT) %>%
      mutate(PERIPHRASTIC = (MORPHEME == 'p'))
  )
)

df %>%
  filter(TAM == "PROG") %>%
  group_by(RESPONDENT_ID, MORPHEME, FRAME, NORTHWEST) %>%
  summarize(AVERAGE_ACCEPTANCE = mean(SCALED_WOULD_YOU_SAY_THIS)) %>%
  mutate(MORPHEME = ifelse(MORPHEME == "p", "p", "notp")) %>%
  group_by(RESPONDENT_ID, MORPHEME, FRAME, NORTHWEST) %>%
  summarize(MAX_ACCEPTANCE = max(AVERAGE_ACCEPTANCE)) %>%
  pivot_wider(names_from=MORPHEME, values_from=MAX_ACCEPTANCE) %>%
  ggplot(aes(p, notp))+geom_jitter(width=0.1, height=0.1)+
  labs(x="periphrastic", y="highest-accepted affixal strategy")+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  facet_wrap(~FRAME)

# how many pairs are correlated?

pvalues <- df_avg %>%
  filter(MORPHEME != "p") %>%
  mutate(TAM = ifelse(TAM == "FUT", "PROG", TAM)) %>%
  mutate(CONDITION = paste(TAM, MORPHEME, FRAME)) %>%
  group_by(RESPONDENT_ID, CONDITION) %>%
  dplyr::summarize(WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from = CONDITION, values_from = WOULD_YOU_SAY_THIS) %>%
  ungroup() %>%
  select(-RESPONDENT_ID) %>%
  as.matrix() %>%
  rcorr(type = "pearson") %>%
  .$P %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(cols=-var1, names_to="var2", values_to="pvalue") %>%
  na.omit() %>%
  filter(var1 < var2)

# r value = how spread away are the dots from the line? 1 = perfect score
# cor.test will give r value and p value
# need to do multiple comparisons corrections

# are there ra likers and ra haters?

df %>%
  filter(MORPHEME != "p", TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL", "PART")) %>%
  group_by(RESPONDENT_ID, MORPHEME) %>%
  dplyr::summarize(WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS)) %>%
  pivot_wider(names_from=MORPHEME, values_from=WOULD_YOU_SAY_THIS) %>%
  ggplot(aes(ra, `0`)) + geom_jitter() +
  labs(x="average ra- score", y="average unmarked verb score")
