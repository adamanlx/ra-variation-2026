# Variation and Change in Kinyarwanda TAM-marking *ra-*

Adam An and Jean Paul Ngoboka

## Survey 1

`survey_1_design.md` contains survey questions as presented to participants and English translations.

`survey_1_responses.csv` contains data as exported from Google Forms with identifying information removed.

## Survey 2

### Survey design and `raw_anonymized_data.csv`

`survey_2_design.md` contains survey questions as presented to participants and English translations. `survey_2_stimuli.md` contains stimuli used in Survey 2. `raw_anonymized_data.csv` contains data as exported from Qualtrics with identifying information removed. Note that in these files `CJ` experimental conditions are labeled with `0`. Other experimental condition names are consistent with the paper.

### `annotated_data.csv`

This file is almost the same as `raw_anonymized_data.csv` but with some of the Qualtrics auto-generated fields removed and with a new column, `CODED_REGION`. This column holds the ways that we manually coded each participant for region, doing our best to prioritize their location between the ages of 5 and 17.

### `transform_data.py`

This Python script turns `annotated_data.csv` into `transformed_data.csv`.

### `transformed_data.csv`

This file contains participant responses in "long format."

`WOULD_YOU_SAY_THIS` values are:

1. Nta na rimwe nayivuga gutyo. (I would never say the sentence like that.)
2. Hari ubwo nayivuga gutyo. (I would occasionally say the sentence like that.)
3. Simbizi. (I don't know.)
4. Akenshi ni ko nayivuga. (I would often say the sentence like that.)
5. Buri gihe ni ko nayivuga. (I would always say the sentence like that.)

`HAVE_YOU_HEARD_THIS` values are:

1. Byantangaza cyane. Nta muntu n'umwe ndumva abivuga atyo. (I would be extremely surprised. I have never heard someone say it like that.)
2. Byantangaza biringaniye. Ndumva bicocamye. (I would be quite surprised. It sounds weird to me.)
3. Sinzi neza niba byantangaza cyangwa bitantangaza. (I am unsure whether it would surprise me or not.)
4. Ntabwo byantangaza pe. Nkunze kumva abantu babivuga gutyo. (I wouldn't really be surprised. I have heard people say things in the same way.)
5. Ntabwo byantangaza na gato. Kuri njye ndumva ari ibisanzwe. (I would not be at all surprised. It sounds normal to me.)