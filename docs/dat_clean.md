# Data Cleaning and Processing

Here I documented thoroughly the data cleaning process through which the
variables for analyses were generated. The corresponding code can be found in
the 'munge' directory. Two separate cleaning scripts were used, one for the
survey response dataset `dat` and the other for a table regarding antidepressant
receptor binding profile `med_mat`.

## Survey Response Dataset

* List of raw datasets: **DEM**, **GAD**, **PHQ**, **SASPD**, **CIDID**,
  **CIDIA**, **CIDIP**, **PAD**, **MHD**, **MED**.

* ALl raw data files are `feathe` format.

* Those who are NA in the last mandatory question (number of best aspects) are
  discarded from **MED**. Because this means they did not complete the whole
  questionnaire.

* All data files were left joined with the ID column in **MED**.  Ethnicities in **DEM** were examined for the resulting participants and most participants are white, so we will not include ethnicity as a variable for analyses.
* `bmi`: derived (with the `GLAD_derive` function) metric BMI extracted from
  **DEM**. All values were recoded to absolute values in which those higher than
  60 or lower than 13 were set to NA._

* `score__gad`: derived (with the `GLAD_derive` function) total score for
  **GAD** was extracted. In addition, Derived total score for **GAD** for all
  participants who completed **GAD** (`gad_all_`) were extrated for comparison
  with the scores of those in **MED**.

* `score__phq`: derived (with the `GLAD_derive` function) total score for
  **PHQ** was extracted. In addition, Derived total score for **PHQ** for all
  participants who completed **PHQ** (`phq_all`) were extrated for comparison
  with the scores of those in **MED**.

* `score_cidid`: derived (with the `GLAD_derive` function) total score for
  **CIDID** was extracted.

* `score_cidia`: derived (with the `GLAD_derive` function) total score for
  **CIDIA** was extracted.

* `score_cidip`: derived (with the `GLAD_derive` function) total score for
  **CIDIP** was extracted.

* `score_saspd`: derived (with the `GLAD_derive` function) total score for
  **SASPD** was extracted.

* `score_pad`: derived (with the `GLAD_derive` function) total score for **PAD**
  was extracted.

* `score_mhd`: derived (with the `GLAD_derive` function) total score for **MHD**
  was extracted.

* `fam`: the number of relateives with diagnostic history of psychiatric
  disorders, computed by summing

        "FAM.depression_number", "FAM.anxiety_number",
        "FAM.panicattacks_number", "FAM.antepostnataldepression_number",
        "FAM.socialanxiety_number", "FAM.specificphobia_number",
        "FAM.mania_number", "FAM.autism_number", "FAM.ptsd_number",
        "FAM.ocd_number", "FAM.psychosisother_number",
        "FAM.otherobsessivecompulsive_number", "FAM.personalitydisorder_number",
        "FAM.overeating_number", "FAM.mentalhealthother_number",
        "FAM.agoraphobia_number", "FAM.anorexia_number",
        "FAM.schizophrenia_number", "FAM.bulimia_number"

  in **FAM** with the `rowSums(na.rm = T)` function but participants with NAs in
  all variables were set to NA. -77, -88 and -99 were set to NA before the
  summation and values after summation larger than 30 were discarded.

The following variables are from **MED**.

Note that **1)** Variables involving multiple medications did not include the
option "Other antidepressants". **2)** Binning was applied to some variables
computed by averaging ordinal variables taking only few levels, since as
expected such averaging produced variables concentrated at certain values but
sparse elsewhere. The number of bins were determined by inspection of the
stem-and-leaf plots and histograms and were selected such that the cut-offs are
most natural and with a sufficient number of bins to minimize loss of
information yet retaining an appropriate number of observations in each bin.

An important distinction which relates to the diagnostics is whether `na.rm = T`
is used for `rowMeans` and `rowSums`. Whereas `rowSums` would work fine
regardless we have zeros or NAs for a non-confirmatory response, `rowMeans`
divides the same sum by different numbers when zeros are used (the number of
zeroes plus the number of confirmatory responses) and NAs are used (the number
of just the confirmatory responses because NAs have been removed).

* `mean_eff`: computed by using `rowMeans(na.rm = T)` for all the effectiveness
  variables (one for each antidepressant). This variable was cut into six bins.

* `mean_sef_`: computed by using `rowSums(na.rm = T)` for all the side effect
  variables then divide by the number of medications one takes `n_med` (this
  variable is in turn computed by `rowSums(na.rm = T)` across all medication
  columns but with observations that are NA in all columns set to NA).

* `start`: values large than 100 or smaller than 8 in the treatment start
  variables were first discarded. `min(na.rm = T)` appled to each observation
  was then used to compute this variable.

* `dur`: values large than 62 in the duration of treatment variables were first
  discarded. Since the first antidepressant was approved in 1958
  [@LMM2JXED#Hillhouse_Porter_2015_Brief]. `rowSums` across all medication
  columns but with observations that are NA in all columns set to NA was used to
  compute this variable.

* `remission`: computed by using `rowMeans(na.rm = T)` for all the remission
  variables (one for each antidepressant). This variable was cut into three
  bins.

* `intolerance`: computed by using `rowMeans(na.rm = T)` for all the intolerance
  variables (one for each antidepressant). This variable was cut into three
  bins.

* `mean_imprvdur_`: computed by using `rowMeans(na.rm = T)` for all the
  intolerance variables (one for each antidepressant).

* `ben`: the overall benefit variable.

* `sef`: the overall side effect variable.

* `n_best`: computed by summing all the best aspects columns except for "Other".

All the above variables were computed indiviualy as vectors then put together
into a dataframe with all resulting non-finite values (NA, NaN, Inf) from the
previous computations set to NA.

## Table of Medication Receptor Binding Profile

* The raw table is from this [web
  page](https://en.wikipedia.org/wiki/Pharmacology_of_antidepressants).

* The ">" signs were discarded and all NA values were recoded to 0s.

# References
