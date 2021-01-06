## Medication clustering

As we are not interested in the absolute magnitude of the binding constants,
rather we are interested in the "direction" in which an antidepressant binds to
various receptors. For example, if an antidepressant binds equally strongly to
receptors A and B, but not to others, and another antidepressant binds equally
weakly to receptor A and B, but not to others, we would like to see these two
antidepressants as similar and hence falling into the same cluster. Therefore,
we use cosine distance or L2 normalized Euclidean distance as the distance
measures. As clustering algorithms require users to decide the desired number of
clusters, we use a combination of three techniques to aid our decision. We apply
hierarchical clustering with cosine distance and L2 normalized Euclidean
distance and draw a scree plot of within group sums of squares with K-means
clustering with L2 normalized Euclidean distance.  We also use multidimensional
scaling for visualisation of clustering results. 

## Regression analysis

### Continuous dependent variable

As the side effect severity index is positively skewed with clumping at zero, we
use two-part regression models when using side effect severity as the dependent
variable. A two-part regression model assumes two different data generating
process. The first process is the dichotomous event for whether the outcome is
positive. The second process determines the exact value of the outcome,
conditioned on it being positive. It can be shown that such models can be
estimated by separately maximizing the likelihood functions of the two different
classes of models. We fit a logistic regression model for whether the outcome is
positive, and a gamma regression with log link for outcome larger than zero, as
after removal of the clumping zeroes, the side effect severity index still
appears positively skewed.

### Ordinal dependent variable

We use partial proportional odds model to model other, ordinal dependent
variables. Partial proportional odds model can be formulated as follows.

$$logit(P(Y_{i} \leq j)) = \sum{x_i \beta} + \sum{z_i \beta_j} + \theta_j$$

Suppose that $Y_i = j (j = 1, 2, 3, ...J)$, this amounts to fitting $J - 1$
logistic regression models ($logit(P_i \leq 1)$, $logit(P_i \leq 2)$, ...,
$logit(P_i \leq J - 1)$. $x$s are a set of predictors with constant effect
$\beta$s in all the logistic models and $z$s a set of predictors with different
effects $\beta_j$s across the models (i.e. across different levels of the
response variable). $\theta_j$ is the intercept for each model which also
differs by level.
The $\beta$s that are constant across levels are termed ordinal effects.
$\beta_j$s, which are free to vary depending on the level of $Y$ are termed
nominal effects.

We first fit the data with all effects entered as ordinal. We then run a series
likelihood ratio test by entering each effect as nominal. The significant
effects (suggesting a better fit when entered with nominal) are re-entered into
the final model as nominal effects.

### Correlates of side effects severity and treatment intolerance

The fully adjusted model with mean side effects as the dependent variable
suggests that time on antidepressants and the number of psychiatric diagnoses
are both associated with a higher likelihood of having one or more side effects,
whereas number of panic disorder symptoms, number of relatives with psychiatric
diagnoses and age at earliest treatment are associated with having more side
effects conditioned on having one or more side effects.

In the fully adjusted model with overall side effect rating as the dependent
variable, only time on antidepressants shows significant associations, with a
positive association with giving a rating higher than 1 versus a rating equal to
1, but a negative association with giving a rating higher than 4 versus a rating
less than equal to 4. In the fully adjusted model with likelihood of intolerance
as the dependent variable, only number of psychiatric diagnoses shows a positive
association with being higher than level 1 versus being in level 1.

As BMI was measured in the GLAD sign-up questionnaire which was completed at a
different time point to the medication questionnaire, we remove BMI and refit
the models and find the results unchanged. We also replace the number of
psychiatric diagnoses with individual psychiatric diagnoses. The results are
largely unchanged except with the number of panic disorder symptoms showing a
negative association with overall side effect rating.

We reconducct the analyses on the sub-sample that are currently depressed and on
antidepressants and find similar results in the model with mean side effects,
except with current depression conditionally associating with more side effects.
In the model with overall side effect rating, only BMI is associated with higher
ratings. No significant association is found in the model with likelihood of
intolerance. The results did not change afer removal of BMI or replacing the
number of psychiatric diagnoses with individual diagnoses.

### Correlates of effectiveness

#### Mean number of side effects

In the fully adjusted model with mean effectiveness being the dependent
variaible and mean number of side effects as the side effect severity measure ,
age at earliest treatment is significant but only for levels higher than 4. Mean
side effects is associated with higher mean effectiveness whereas the number of
psychiatric diagnoses is associated with lower mean effectiveness. The results
did not change after removal of BMI and the number of psychiatric diagnoses.

In the fully adjusted model with likelihood of remission, mean side effects
shows a negative association, whereas time on antidepressants shows a positive
association but only for likelihood of being higher than level 1 versus being
equal to level 1. The results did not change after removal of BMI and the number of psychiatric diagnoses.  

BMI, the number of psychiatric diagnoses and mean side effects are positively
associated with overall benefits at all levels. Age at earliest treatment and
time on antidepressants also show positive associations but only up to level 2.
However, after removal of BMI, age at earliest treatment is no longer
significant, whereas time on antidepressants show positive associations across
all levels.

BMI, the number of panic disorder symptoms and the number of relatives with
psychiatric diagnoses are negatively associated with the number of best aspects,
whereas the number of psychiatric diagnoses, mean number of side effects and
time on antidepressants show positive associations. After removal of BMI and the
number of psychiatric diagnoses, mean side effects no longer show an
association, and the number of panic disorder symptoms show positive
associations for levels higher than 4.

#### Likelihood of intolerance

Likelihood of intolerance shows a positive association with mean effectiveness.
Number of psychiatric diagnoses and number of relatives with psychiatric
diagnoses show positive associations only at higher than 2 versus lower than or
equal to 2. Lifetime depression shows a negative association at higher than 2
versus lower than or equal to 2 and higher than 5 versus lower than or equal to
5. Age at earliest treatmente shows a positive association at levels higher than
3. After removal of BMI and number of psychiatric diagnoses, number of relatives
shows an additional association at levevl 3.

Number of panic disorder symptoms shows a negative association with likelihood
of remission, whereas time on antidepressants also show a positive association
but only for the likelihood of being higher than level 1 versus being equal to
level 1. The results did not change afer removal of BMI and psychiatric
diagnoses.

Age at earliest treatment shows a positive association with overall benefits at
higher than level 1 versus being equal to level 1. Number of psychiatric
diagnoses,  time on antidepressants and BMI show positive associations, whereas
likelihood of intolerance shows a negative association. The results did not
change afer removal of BMI and psychiatric diagnoses.

Likelihood of intolerance shows a negative association with number of best
aspects up to level 4. Number of panic disorder symptoms shows and number of
relatives with psychiatric diagnoses show negative associations, whereas time on
antidepressants shows a positive association. The results did not
change afer removal of BMI and psychiatric diagnoses.

#### Overall side effects

Age at earliest treatment shows positive association for levels higher than 4.
BMI shows a positive association at level 1. Overall side effects shows a
negative association.The results did not change afer removal of BMI and
psychiatric diagnoses.

Overall side effects and number of panic disorder symptoms show negative
associations. Time on antidepressants shows a positive association at level 1.
Number of panic disorder symptoms are no longer significant after removal of BMI
and psychiatric diagnoses.

BMI, number of psychiatric diagnoses and time on antidepressants show positive
asassociations. Overall side effects show a negative association.The results did
not change afer removal of BMI and psychiatric diagnoses.

Lifetime depression, number of panic disorder symptoms and number of relatives
with psychiatric diagnoses show negative associations, whereas time on
antidepressants shows a positive association. After removal of BMI and
psychiatric diagnoses, number of panic disorder symptoms are only significant
after level 3.

# Discussion

## Side effect severity and treatment intolerance

mean side effects 

age-- earliest treatment.+ panic disorder symptoms.+
mhd+.
relatives.+ time on antidepressant+.

indiv mhd 
relatives..

current
age.-
current deprpession .+
relatives.. time on antidepressant..

current indiv mhd bmi


overall side effects 

age...+
time on antidepressant +...-

indiv mhd 
bipolar...+

no bmi
age+..+

current
bmi+
current depression+

likelihood of intolerance
age-
psychiatric diagnoses+.

indiv mhd NULL

current
age..
current depression+.



In the current study, we used two different measures for side effect severity,
1) the average side effect count across medications and 2) a five-point Likert
scale rating item for overall side effect severity. The former is arguably a
more "objective" measure of side effect severity and surprisingly does not
correlate well with the latter measure (r = 0.35). An additional variable that
should be related to side effect severity is treatment intolerance. Only number
of psychiatric diagnoses shows a significant positive association with treatment
intolerance and correlates identified in regression models differ significantly
for the two side effect severity measures. 

The model using mean number of side effects as the dependent variable show
current depression, age at earliest treatment, time on antidepressants, number
of panic disorder symptoms and number of relatives with psychiatric diagnoses to
be positive correlates. This is in mostly line with previous findings by
@942N3AV8#Bet_Etal_2013_Side, where the side effect severity measure is defined
as the number of side effects experienced for a single antidepressant in a
single depressive episode and are found to be positively associated with
psychiatric commorbidities and depression severity. @942N3AV8#Bet_Etal_2013_Side, however, found duration of
use, a similar measure to time on antidepressants in the current study to be
negatively associated with side effect severity. Note that we in fact use a
rather different definition to @942N3AV8#Bet_Etal_2013_Side.  Whereas
@942N3AV8#Bet_Etal_2013_Side measures duration of use in a two-year window
prospectively with most participants taking a single antidepressant in a single
episode, we measure time on antidepressants by summing duration of use of all
antidepressants used in all retrospective episodes. 

In the model using overall side effect rating, only time on antidepressants
emerges as a significant correlate, with the result suggesting that participants
with higher time on antidepressants are more likely to give ratings higher than
the worst, but in contrary less likely to give very high ratings. Such
discrepant findings might suggest that the validity of a subjective report of an
"overall" side effect severity is in question. On the other hand, the impact of
side effects on patients' experience might not have a strong positive
association with the number of the types of side effects experienced.  The
frequency at which these side effects are experienced, for instance, might play
a more important role.

# Effectiveness

Using four different effectiveness measures, mean effectiveness, overall
benefit, likelihood of remission and number of best aspects, we find BMI, age at
earliest treatment, time on antidepressants, number of psychiatric diagnoses and
number of panic disorder symptoms , lifetime and current depression and the side
effect and intolerance measures to be associated with effectiveness. Most of
these results are in line with previous findings that highlight the roles of
psychiatric commorbidities, especially those concerning anxiety and more
severe level of depression in poorer treatment outcomes. 
[@WCAUCZJ2#Howland_Etal_2008_Factors; @MP64S4G2#Chekroud_Etal_2016_Crosstrial;
@KTZ8XBKT#Rajpurkar_Etal_2020_Evaluation].

Interestingly, we consistently find age at earliest treatment and time on
antidepressants, which we use as indicators for lifetime medication history, to
be associated with better treatment outcomes, even when different effectiveness
and side effect/intolerance measures are used in the model.
@WCAUCZJ2#Howland_Etal_2008_Factors found that more persistent depression,
defined by a length of current depression episode negatively correlates with
treatment effectiveness. In contrary, our results suggest that patients with
longer lifetime medication history who can be seen as having more persistent
lifetime deprpession respond better to treatments, although it is more likely
that more active treatment seeking and adherence leads to the better treatment
response. 

We have also found mean number of side effects consistently show positive
associations with the effectiveness measures except for likelihood of remission,
despite the fact that overall side effect rating and treatment intolerance both
show negative associations. 




