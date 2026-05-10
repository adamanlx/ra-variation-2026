library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)
library(ggpubr)

df <- read.csv('transformed_data.csv', header=T) %>%
  mutate(TAMMORPHEME = paste(TAM, MORPHEME, sep="")) %>%
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
  group_by(TAMMORPHEME, FRAME) %>%
  summarize(SCORE = mean(WOULD_YOU_SAY_THIS),
            SCALED_SCORE = mean(SCALED_WOULD_YOU_SAY_THIS))

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

# NEW SECTION: GENERAL TRENDS

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df %>%
      filter(
        ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
        MORPHEME %in% c("ra", "0"))
  )
)

df %>%
  filter(((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True") | TAM == "HAB"),
         MORPHEME == "ra") %>%
  ggplot(aes(AGE, SCALED_WOULD_YOU_SAY_THIS, color=GENDER, linetype=NORTHWEST)) + geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm", se=FALSE)


# 5.3.3 ACCEPTANCE OF TAM READING; INDEPENDENCE OF SYNTACTIC FRAME

accepts_prog %>% unique() %>% length()
accepts_fut %>% unique() %>% length()
intersect(accepts_prog, accepts_fut) %>% unique() %>% length()
union(accepts_prog, accepts_fut) %>% unique() %>% length()

df_avg %>%
  filter(RESPONDENT_ID %in% accepts_prog, TAM == "PROG") %>%
  group_by(MORPHEME, FRAME) %>%
  summarize(MEAN_WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            MEAN_SCALED_WOULD_YOU_SAY_THIS = mean(SCALED_WOULD_YOU_SAY_THIS))

df_avg %>%
  filter(RESPONDENT_ID %in% accepts_fut, TAM == "FUT") %>%
  group_by(MORPHEME, FRAME) %>%
  summarize(MEAN_WOULD_YOU_SAY_THIS = mean(WOULD_YOU_SAY_THIS),
            MEAN_SCALED_WOULD_YOU_SAY_THIS = mean(SCALED_WOULD_YOU_SAY_THIS))

grid.arrange(
  df_avg %>%
    filter(MORPHEME == "ra", FRAME == "INDfinal", TAM %in% c("PROG", "FUT")) %>%
    pivot_wider(id_cols = RESPONDENT_ID, names_from = TAM, values_from = WOULD_YOU_SAY_THIS) %>%
    ggplot(aes(PROG, FUT))+geom_jitter(width=0.1, height=0.1)+
    labs(title="unscaled", x="PROG", y="FUT")+theme(plot.title = element_text(hjust = 0.5)),
  df_avg %>%
    filter(MORPHEME == "ra", FRAME == "INDfinal", TAM %in% c("PROG", "FUT")) %>%
    pivot_wider(id_cols = RESPONDENT_ID, names_from = TAM, values_from = SCALED_WOULD_YOU_SAY_THIS) %>%
    ggplot(aes(PROG, FUT))+geom_jitter(width=0.1, height=0.1)+
    labs(title="scaled", x="PROG", y="FUT")+theme(plot.title = element_text(hjust = 0.5))+
    geom_vline(xintercept=0)+geom_hline(yintercept=0),
  ncol=2
)

# are PROG and FUT comparable?

prog_fut_resopnses <- df_avg %>%
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

# SECTION 5.3.5 ACCEPTABILITY OF RA-LESS VERBS BEFORE NGO

ngo_responses <- df_avg %>%
  filter(TAM == "HAB", FRAME == "INDngo") %>%
  pivot_wider(id_cols=RESPONDENT_ID, names_from=MORPHEME, values_from=SCALED_WOULD_YOU_SAY_THIS)

ngo_responses %>% filter(ra > 0, `0` > 0) %>% nrow()
ngo_responses %>% filter(ra <= 0, `0` > 0) %>% nrow()
ngo_responses %>% filter(ra > 0, `0` <= 0) %>% nrow()
ngo_responses %>% filter(ra <= 0, `0` <= 0) %>% nrow()

summary(lm(WOULD_YOU_SAY_THIS ~ AGE * GENDER * NORTHWEST_DIALECT * MORPHEME,
         data=df %>%
           filter(TAM=="HAB", FRAME=="INDngo")))

# SECTION 5.3.6 ACCEPTABILITY OF PROG/FUT ra- across SYNTACTIC FRAMES

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

df %>%
  filter(FRAME %in% c("PART"),
         ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
         MORPHEME == "ra") %>%
  ggplot(aes(AGE, SCALED_WOULD_YOU_SAY_THIS, color=GENDER, linetype=NORTHWEST)) + geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm", se=FALSE)

# SECTION 5.3.7 IMPLICATIONAL HIERARCHIES?

neg_rel_part <- df_avg %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL", "PART"), MORPHEME == "ra",
         (TAM == "PROG" & RESPONDENT_ID %in% accepts_prog) | (TAM == "FUT" & RESPONDENT_ID %in% accepts_fut)) %>%
  pivot_wider(id_cols=c(RESPONDENT_ID, TAM), names_from=FRAME, values_from=SCALED_WOULD_YOU_SAY_THIS)

ggarrange(
  neg_rel_part %>%
    ggplot(
      aes(REL, NEG, color=TAM)
    ) + geom_jitter() + 
    geom_smooth(method="lm", se=FALSE) +
    xlab("relativization") + ylab("negation")+
    xlim(-1.5, 1.5)+ylim(-1.5, 1.5),
  
  neg_rel_part %>%
    ggplot(
      aes(PART, NEG, color=TAM)
    ) + geom_jitter() +
    xlab("participial") + ylab("negation")+
    xlim(-1.5, 1.5)+ylim(-1.5, 1.5),
  
  neg_rel_part %>%
    ggplot(
      aes(PART, REL, color=TAM)
    ) + geom_jitter() +
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

# SECTION 6.2.1 NEGATION AS THE SOLE ENVIRONMENT FOR CHANGE: SEGMENTAL DISTINCTIVENESS?

df %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "NEG", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()

df %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "REL", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()

df %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "PART", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()

