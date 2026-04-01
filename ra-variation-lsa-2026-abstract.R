library(tidyverse)
library(factoextra)
library(lmerTest)

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

# scale responses
df_raw = df_raw %>%
  group_by(RESPONDENT_ID) %>%
  mutate(SCALED_WOULD_YOU_SAY_THIS = scale(WOULD_YOU_SAY_THIS))

widen <- function(df, values_from) {
  return(df %>%
           pivot_wider(id_cols=c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                                 "NORTHWEST", "NORTHWEST_DIALECT"),
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

# morphological preference score
mps <- function(df, values_from) {
  result = widen(df, values_from) %>%
    mutate(HABINDDP = HABraINDDP - HAB0INDDP) %>%
    mutate(HABINDfinal = HABraINDfinal - HAB0INDfinal) %>%
    mutate(HABINDngo = HABraINDngo - HAB0INDngo) %>%
    mutate(HABINDko = HABraINDko - HAB0INDko) %>%
    mutate(HABNEG = HABraNEG - HAB0NEG) %>%
    mutate(HABREL = HABraREL - HAB0REL) %>%
    mutate(HABPART = HABraPART - HAB0PART) %>%
    mutate(PROGINDDP = PROGraINDDP - PROG0INDDP) %>%
    mutate(PROGINDfinal = PROGraINDfinal - PROG0INDfinal) %>%
    mutate(PROGINDngo = PROGraINDngo - PROG0INDngo) %>%
    mutate(PROGINDko = PROGraINDko - PROG0INDko) %>%
    mutate(PROGNEG = PROGraNEG - PROG0NEG) %>%
    mutate(PROGREL = PROGraREL - PROG0REL) %>%
    mutate(PROGPART = PROGraPART - PROG0PART) %>% 
    mutate(FUTINDDP = FUTraINDDP - FUT0INDDP) %>%
    mutate(FUTINDfinal = FUTraINDfinal - FUT0INDfinal) %>%
    mutate(FUTINDngo = FUTraINDngo - FUT0INDngo) %>%
    mutate(FUTINDko = FUTraINDko - FUT0INDko) %>%
    mutate(FUTNEG = FUTraNEG - FUT0NEG) %>%
    mutate(FUTREL = FUTraREL - FUT0REL) %>%
    mutate(FUTPART = FUTraPART - FUT0PART) %>%
    mutate(PERIPHRASTICINDDP = PROGpINDDP - pmax(PROGraINDDP, PROG0INDDP)) %>%
    mutate(PERIPHRASTICINDfinal = PROGpINDfinal - pmax(PROGraINDfinal, PROG0INDfinal)) %>%
    mutate(PERIPHRASTICINDngo = PROGpINDngo - pmax(PROGraINDngo, PROG0INDngo)) %>%
    mutate(PERIPHRASTICINDko = PROGpINDko - pmax(PROGraINDko, PROG0INDko)) %>%
    mutate(PERIPHRASTICNEG = PROGpNEG - pmax(PROGraNEG, PROG0NEG)) %>%
    mutate(PERIPHRASTICREL = PROGpREL - pmax(PROGraREL, PROG0REL)) %>%
    mutate(PERIPHRASTICPART = PROGpPART - pmax(PROGraPART, PROG0PART)) %>%
    select(-contains(c("0", "ra", "PROGp"), ignore.case=FALSE)) %>%
    pivot_longer(!all_of(c("RESPONDENT_ID", "AGE", "GENDER", "REGION",
                           "NORTHWEST", "NORTHWEST_DIALECT")),
                 names_to="CONDITION", values_to="IMPROVEMENT")
  
  result$TAM <- "HAB"
  result$TAM[grepl("PROG", result$CONDITION)] <- "PROG"
  result$TAM[grepl("FUT", result$CONDITION)] <- "FUT"
  
  result$FRAME <- "INDDP"
  result$FRAME[grepl("INDfinal", result$CONDITION)] <- "INDfinal"
  result$FRAME[grepl("INDngo", result$CONDITION)] <- "INDngo"
  result$FRAME[grepl("INDko", result$CONDITION)] <- "INDko"
  result$FRAME[grepl("NEG", result$CONDITION)] <- "NEG"
  result$FRAME[grepl("REL", result$CONDITION)] <- "REL"
  result$FRAME[grepl("PART", result$CONDITION)] <- "PART"
  
  return(result)
}

df_mps = mps(df_raw, "WOULD_YOU_SAY_THIS")
df_mps_scaled = mps(df_raw, "SCALED_WOULD_YOU_SAY_THIS")

df_mps_scaled$GENDERREGION <- "Other"
df_mps_scaled$GENDERREGION[df_mps_scaled$GENDER == "male" & df_mps_scaled$NORTHWEST == "Northwest"] <- "Northwest male"

# negation, relativization, participials

df_mps_scaled %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("NEG", "REL")) %>%
  ggplot(aes(AGE, IMPROVEMENT, color=GENDER))+facet_wrap(~factor(FRAME, levels=c("NEG", "REL", "PART")), labeller=as_labeller(c(`NEG`="negated", `REL`="relativized")))+geom_jitter()+geom_smooth(method="lm", se=FALSE)+ylab("Preference for ra-")+xlab("Age")+labs(color="Gender")


df_mps_scaled %>%
  filter(TAM %in% c("PROG", "FUT"), FRAME %in% c("PART")) %>%
  ggplot(aes(AGE, IMPROVEMENT, color=GENDERREGION))+facet_wrap(~factor(FRAME, levels=c("NEG", "REL", "PART")), labeller=as_labeller(c(`PART`="participial", `REL`="relativized")))+geom_jitter()+geom_smooth(method="lm", se=FALSE)+ylab("Preference for ra-")+xlab("Age")+labs(color="Gender and region")

# ngo

df_mps_scaled %>%
  filter(TAM %in% c("HAB"), FRAME %in% c("INDngo")) %>%
  ggplot(aes(AGE, IMPROVEMENT, color=GENDER))+geom_jitter()+geom_smooth(method="lm", se=FALSE)+ylab("Preference for ra-")+xlab("Age")+labs(color="Gender and region")
