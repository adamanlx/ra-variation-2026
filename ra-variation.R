library(tidyverse)
library(factoextra)
library(lmerTest)
library(gridExtra)
library(ggpubr)

df_raw <- read.csv(file.choose(), header=T) # select transformed_data.csv

# add variables
df_raw$TAMMORPHEME <- paste(df_raw$TAM, df_raw$MORPHEME, sep="")

df_raw$NORTHWEST <- "Elsewhere"
df_raw$NORTHWEST[df_raw$REGION%in%c("Burera", "Musanze", "Rulindo", "Gakenke",
                                    "Rubavu")] <- "Northwest"

df_raw$NORTHWEST_DIALECT <- "Elsewhere"
df_raw$NORTHWEST_DIALECT[df_raw$IKIGOYI] <- "Northwest"
df_raw$NORTHWEST_DIALECT[df_raw$IKIRERA] <- "Northwest"
df_raw$NORTHWEST_DIALECT <- as.factor(df_raw$NORTHWEST_DIALECT)

df_raw$AGERANGE <- "20-29"
df_raw$AGERANGE[df_raw$AGE >= 30] <- "30-39"
df_raw$AGERANGE[df_raw$AGE >= 40] <- "40-49"
df_raw$AGERANGE[df_raw$AGE >= 50] <- "50-59"

df_raw$GENDERREGION <- "Elsewhere female"
df_raw$GENDERREGION[df_raw$GENDER == "male" & df_raw$NORTHWEST == "Elsewhere"] <- "Elsewhere male"
df_raw$GENDERREGION[df_raw$GENDER == "female" & df_raw$NORTHWEST == "Northwest"] <- "Northwest female"
df_raw$GENDERREGION[df_raw$GENDER == "male" & df_raw$NORTHWEST == "Northwest"] <- "Northwest male"

# scale responses
df_raw = df_raw %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS)) %>%
  mutate(SCALED_AWARENESS = scale(HAVE_YOU_HEARD_THIS))

widen <- function(df, values_from) {
  return(df %>%
    pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                          "NORTHWEST", "NORTHWEST_DIALECT", "AGERANGE",
                          "GENDERREGION"),
                names_from="CONDITION_NAME",
                values_from=values_from) %>%
    mutate(HAB0INDngo = (HAB0INDngo1 + HAB0INDngo2) / 2) %>%
    mutate(HABraINDngo = (HABraINDngo1 + HABraINDngo2) / 2) %>%
    mutate(PROG0INDDP = (PROG0INDDP1 + PROG0INDDP2) / 2) %>%
    mutate(PROG0INDngo = (PROG0INDngo1 + PROG0INDngo2) / 2) %>%
    mutate(PROG0INDko = (PROG0INDko1 + PROG0INDko2) / 2) %>%
    mutate(PROG0NEG = (PROG0NEG1 + PROG0NEG2) / 2) %>%
    mutate(PROG0REL = (PROG0REL1 + PROG0REL2) / 2) %>%
    mutate(PROG0PART = (PROG0PART1 + PROG0PART2) / 2) %>%
    mutate(PROGraINDDP = (PROGraINDDP1 + PROGraINDDP2) / 2) %>%
    mutate(PROGraINDfinal = (PROGraINDfinal1 + PROGraINDfinal2) / 2) %>%
    mutate(PROGraINDngo = (PROGraINDngo1 + PROGraINDngo2) / 2) %>%
    mutate(PROGraINDko = (PROGraINDko1 + PROGraINDko2) / 2) %>%
    mutate(PROGraNEG = (PROGraNEG1 + PROGraNEG2) / 2) %>%
    mutate(PROGraREL = (PROGraREL1 + PROGraREL2) / 2) %>%
    mutate(PROGraPART = (PROGraPART1 + PROGraPART2) / 2) %>%
    mutate(FUT0INDDP = (FUT0INDDP1 + FUT0INDDP2) / 2) %>%
    mutate(FUT0INDngo = (FUT0INDngo1 + FUT0INDngo2) / 2) %>%
    mutate(FUT0INDko = (FUT0INDko1 + FUT0INDko2) / 2) %>%
    mutate(FUT0NEG = (FUT0NEG1 + FUT0NEG2) / 2) %>%
    mutate(FUT0REL = (FUT0REL1 + FUT0REL2) / 2) %>%
    mutate(FUT0PART = (FUT0PART1 + FUT0PART2) / 2) %>%
    mutate(FUTraINDDP = (FUTraINDDP1 + FUTraINDDP2) / 2) %>%
    mutate(FUTraINDfinal = (FUTraINDfinal1 + FUTraINDfinal2) / 2) %>%
    mutate(FUTraINDngo = (FUTraINDngo1 + FUTraINDngo2) / 2) %>%
    mutate(FUTraINDko = (FUTraINDko1 + FUTraINDko2) / 2) %>%
    mutate(FUTraNEG = (FUTraNEG1 + FUTraNEG2) / 2) %>%
    mutate(FUTraREL = (FUTraREL1 + FUTraREL2) / 2) %>%
    mutate(FUTraPART = (FUTraPART1 + FUTraPART2) / 2) %>%
    select(-contains(c("1", "2"))))
}

# tag respondents based on whether they accept prog or fut

accepts_prog = widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(PROGraINDfinal > 0) %>%
  pull(RESPONDENT_ID) %>%
  unique()
df_raw$ACCEPTS_PROG <- "False"
df_raw$ACCEPTS_PROG[df_raw$RESPONDENT_ID %in% accepts_prog] <- "True"
  
accepts_fut = widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(FUTraINDfinal > 0) %>%
  pull(RESPONDENT_ID) %>%
  unique()
df_raw$ACCEPTS_FUT <- "False"
df_raw$ACCEPTS_FUT[df_raw$RESPONDENT_ID %in% accepts_fut] <- "True"

# SECTION 5.3.2 OVERALL RESPONSES
# overall responses
# TODO redo with tapply

for (tammorpheme in c("HAB0", "HABra", "PROG0", "PROGra", "PROGp", "FUT0", "FUTra")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(tammorpheme, frame, df_raw %>%
              filter(TAMMORPHEME == tammorpheme, FRAME == frame) %>%
              pull(WOULD_YOU_SAY_THIS) %>% mean() %>% round(digits=2)))
  }
}
for (tammorpheme in c("HAB0", "HABra", "PROG0", "PROGra", "PROGp", "FUT0", "FUTra")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(tammorpheme, frame, df_raw %>%
              filter(TAMMORPHEME == tammorpheme, FRAME == frame) %>%
              pull(SCALED_WOULD_YOU_SAY_THIS) %>% mean() %>% round(digits=2)))
  }
}

# awareness

df_raw %>%
  ggplot(aes(SCALED_WOULD_YOU_SAY_THIS, SCALED_AWARENESS))+
  geom_jitter()+geom_smooth(method="lm")+
  labs(x="scaled acceptance scores", y="scaled awareness scores")

summary(
  lmer(
    SCALED_AWARENESS ~ SCALED_WOULD_YOU_SAY_THIS + (1 | RESPONDENT_ID),
    data=df_raw
  )
)

# 5.3.3 ACCEPTANCE OF TAM READING; INDEPENDENCE OF SYNTACTIC FRAME

accepts_prog %>% unique() %>% length()
accepts_fut %>% unique() %>% length()
intersect(accepts_prog, accepts_fut) %>% unique() %>% length()
union(accepts_prog, accepts_fut) %>% unique() %>% length()

# scores
widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_prog) %>%
  select(starts_with("PROG0") | starts_with("PROGra"))  %>%
  summary()

widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_prog) %>%
  select(starts_with("PROG0") | starts_with("PROGra"))  %>%
  summary()

widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_fut) %>%
  select(starts_with("FUT0") | starts_with("FUTra")) %>%
  summary()

widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_fut) %>%
  select(starts_with("FUT0") | starts_with("FUTra")) %>%
  summary()

# how many people accept at all in either TAM?
for (morpheme in c("ra", "0")) {
    for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
      print(c(morpheme, frame,
              widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
                filter(
                  !!sym(paste("PROG", morpheme, frame, sep="")) > 0 |
                  !!sym(paste("FUT", morpheme, frame, sep="")) > 0
                  ) %>%
                select(RESPONDENT_ID) %>% unique() %>% nrow()
              )
            )
    }
}


# how many people accept? PROG accepters, FUT accepters
for (morpheme in c("ra", "0")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(morpheme, frame,
            widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
              filter(
                !!sym(paste("PROG", morpheme, frame, sep="")) > 0
              ) %>%
              filter(RESPONDENT_ID %in% accepts_prog) %>%
              select(RESPONDENT_ID) %>% unique() %>% nrow()
    )
    )
  }
}

for (morpheme in c("ra", "0")) {
  for (frame in c("INDfinal", "INDDP", "INDngo", "INDko", "NEG", "REL", "PART")) {
    print(c(morpheme, frame,
            widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
              filter(
                !!sym(paste("FUT", morpheme, frame, sep="")) > 0
              ) %>%
              filter(RESPONDENT_ID %in% accepts_fut) %>%
              select(RESPONDENT_ID) %>% unique() %>% nrow()
    )
    )
  }
}

# how many people accept across the board?
widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(
    PROGraINDfinal > 0, PROGraINDDP > 0, PROGraINDngo > 0, PROGraINDko > 0,
    PROGraNEG > 0, PROGraREL > 0, PROGraPART > 0
  )

widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(
    FUTraINDfinal > 0, FUTraINDDP > 0, FUTraINDngo > 0, FUTraINDko > 0,
    FUTraNEG > 0, FUTraREL > 0, FUTraPART > 0
  )

widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(
    PROGraINDfinal > 0, PROGraINDDP > 0, PROGraINDngo > 0, PROGraINDko > 0,
    PROGraNEG > 0, PROGraREL > 0, PROGraPART > 0,
    FUTraINDfinal > 0, FUTraINDDP > 0, FUTraINDngo > 0, FUTraINDko > 0,
    FUTraNEG > 0, FUTraREL > 0, FUTraPART > 0
  )
  
# NEW SECTION: GENERAL TRENDS

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data = df_raw %>%
      filter(
        ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
        MORPHEME %in% c("ra", "0"))
  )
)

# SECTION 5.3.4 MEANING OF PROG/FUT RA-

grid.arrange(
  widen(df_raw, "WOULD_YOU_SAY_THIS") %>%
    select(PROGraINDfinal, FUTraINDfinal) %>%
    ggplot(aes(PROGraINDfinal, FUTraINDfinal))+geom_jitter(width=0.1, height=0.1)+
    labs(title="unscaled", x="PROG", y="FUT")+theme(plot.title = element_text(hjust = 0.5)),
  widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
    select(PROGraINDfinal, FUTraINDfinal) %>%
    ggplot(aes(PROGraINDfinal, FUTraINDfinal))+geom_jitter(width=0.1, height=0.1)+
    labs(title="scaled", x="PROG", y="FUT")+theme(plot.title = element_text(hjust = 0.5))+
    geom_vline(xintercept=0)+geom_hline(yintercept=0),
  ncol=2
)

# are PROG and FUT comparable?

prog_fut_responses = widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(RESPONDENT_ID %in% accepts_prog, RESPONDENT_ID %in% accepts_fut) %>%
  select(starts_with("FUT") | starts_with("PROGra") | starts_with("PROG0")) %>%
  pivot_longer(names_to = "CONDITION", values_to = "SCORE", cols = !c("RESPONDENT_ID")) %>%
  mutate(TAMMORPHEME = case_when(
    startsWith(CONDITION, "FUT0") ~ "FUT0",
    startsWith(CONDITION, "FUTra") ~ "FUTra",
    startsWith(CONDITION, "PROG0") ~ "PROG0",
    startsWith(CONDITION, "PROGra") ~ "PROGra"
  )) %>%
  mutate(FRAME = case_when(
    endsWith(CONDITION, "INDfinal") ~ "INDfinal",
    endsWith(CONDITION, "INDDP") ~ "INDDP",
    endsWith(CONDITION, "INDngo") ~ "INDngo",
    endsWith(CONDITION, "INDko") ~ "INDko",
    endsWith(CONDITION, "NEG") ~ "NEG",
    endsWith(CONDITION, "REL") ~ "REL",
    endsWith(CONDITION, "PART") ~ "PART"
  )) %>%
  select(-CONDITION) %>%
  pivot_wider(names_from = TAMMORPHEME, values_from=SCORE)

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

widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(HABraINDngo > 0, HAB0INDngo > 0) %>%
  pull(RESPONDENT_ID) %>% length()
widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(HABraINDngo <= 0, HAB0INDngo > 0) %>%
  pull(RESPONDENT_ID) %>% length()
widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(HABraINDngo > 0, HAB0INDngo <= 0) %>%
  pull(RESPONDENT_ID) %>% length()
widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
  filter(HABraINDngo <= 0, HAB0INDngo <= 0) %>%
  pull(RESPONDENT_ID) %>% length()

summary(lm(WOULD_YOU_SAY_THIS ~ AGE * GENDER * NORTHWEST_DIALECT * MORPHEME,
         data=df_raw %>%
           filter(TAM=="HAB", FRAME=="INDngo")))

# SECTION 5.3.6 ACCEPTABILITY OF PROG/FUT ra- across SYNTACTIC FRAMES

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("NEG"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("REL"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * MORPHEME + (1 | CONDITION_NAME) + (1 | RESPONDENT_ID),
    data=df_raw %>%
      filter(FRAME %in% c("PART"),
             ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
             MORPHEME != "p")
  )
)

df_raw %>%
  filter(FRAME %in% c("PART"),
         ((TAM == "PROG" & ACCEPTS_PROG == "True") | (TAM == "FUT" & ACCEPTS_FUT == "True")),
         MORPHEME == "ra") %>%
  ggplot(aes(AGE, SCALED_WOULD_YOU_SAY_THIS, color=GENDER, linetype=NORTHWEST)) + geom_jitter(width=0.1, height=0.1) + geom_smooth(method="lm", se=FALSE)

# SECTION 5.3.7 IMPLICATIONAL HIERARCHIES?

neg_rel_part = rbind(
  widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
    filter(RESPONDENT_ID %in% accepts_prog) %>%
    select(PROGraNEG, PROGraREL, PROGraPART) %>%
    rename(NEG = PROGraNEG) %>%
    rename(REL = PROGraREL) %>%
    rename(PART = PROGraPART) %>%
    mutate(TAM = "PROG"),
  
  widen(df_raw, "SCALED_WOULD_YOU_SAY_THIS") %>%
    filter(RESPONDENT_ID %in% accepts_fut) %>%
    select(FUTraNEG, FUTraREL, FUTraPART) %>%
    rename(NEG = FUTraNEG) %>%
    rename(REL = FUTraREL) %>%
    rename(PART = FUTraPART) %>%
    mutate(TAM = "FUT")
)

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
      aes(REL, PART, color=TAM)
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

neg_rel_part %>%
  filter(TAM == "PROG", NEG < 0, REL < 0, PART < 0) %>%
  nrow()

for (tam in c("PROG", "FUT")) {
  for (neg in c(TRUE, FALSE)) {
    for (rel in c(TRUE, FALSE)) {
      for (part in c(TRUE, FALSE)) {
        print(c(tam, neg, rel, part,
                neg_rel_part %>%
                  filter(
                    TAM == tam,
                    ifelse(neg, NEG >= 0, NEG < 0),
                    ifelse(rel, REL >= 0, REL < 0),
                    ifelse(part, PART >= 0, PART < 0),
                    ) %>%
                  nrow()))
      }
    }
  }
}

# SECTION 5.3.8 PERIPHRASTICS

summary(
  lmer(
    SCALED_WOULD_YOU_SAY_THIS
    ~ AGE * GENDER * NORTHWEST * PERIPHRASTIC + (1 | RESPONDENT_ID) + (1 | CONDITION_NAME),
    data = df_raw %>%
      filter(TAM == "PROG") %>%
      select(CONDITION_NAME, TAM, MORPHEME, FRAME, SCALED_WOULD_YOU_SAY_THIS,
             RESPONDENT_ID, AGE, GENDER, NORTHWEST, NORTHWEST_DIALECT) %>%
      mutate(PERIPHRASTIC = (MORPHEME == 'p'))
  )
)

# SECTION 6.2.1 NEGATION AS THE SOLE ENVIRONMENT FOR CHANGE: SEGMENTAL DISTINCTIVENESS?

df_raw %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "NEG", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()

df_raw %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "REL", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()

df_raw %>%
  filter(TAM == "PROG" | TAM == "FUT") %>%
  filter(FRAME == "PART", MORPHEME == "ra") %>%
  mutate(SCALED_AWARENESS = as.double(SCALED_AWARENESS)) %>%
  pull(SCALED_AWARENESS) %>%
  na.omit() %>%
  mean()
