# Description 

The main focus of this paper is to determine the factors associated with the development of Successful Aging (SE) and Healthy Aging (SE) in population over 45 years of age, through a systematic review and meta-analysis of epidemiological cohort studies.  

<p>
    <h1 align = "center"><b> Associated factors with successful and healthy aging: Systematic review and meta-analysis of longitudinal studies </h1>
</p>
<br />

## Review question 
+ What are the factors associated with the development of Successful and healthy aging in population older than 45 years?

+ What are the risk factors associated with Successful and healthy aging in population older than 45 years?

+ What are the theoretical models, operational definitions, and components most used to defined Successful and Healthy Aging? 

+ What are the most used domains and components to assess successful and healthy aging?

+ What are the frequencies of each component used in the definitions of Successful and heathy aging? 

## Search
Condition or Domain being Studied
Successful aging, healthy aging. 
Population 
Community dwelling older adults (>45 years of age).     
<br />
__Exposure__    
Multi-dimensional factors associated with the development of Successful aging (SA) and Healthy aging (HA). These factors can be grouped by 5 main domains, each of which expands in other areas.     
+ Health factors: Physical function (e.g., ADL, IADLS, Disability), Cognitive function (e.g., cognitive preserved), Physiological health (e.g., metabolic health, presence of chronic disease, Blood pressure, BMI).
+ Psychological Factors: Mental health (e.g., presence of depression symptoms), Psychological well-being (e.g., quality of life, goal pursuit, personal resources).
Subjective health status (e.g., Self-rated health, subjective feeling regarding Successful aging).  
+ Behavioral factors: Health behaviors (e.g., physical activity, smoking status, alcohol consumption, diet). 
+ Social Factors: Social well-being (e.g., social participation, marital status), Socio demographic (e.g., socioeconomic status, education level, economic security). Environmental factors (e.g., access to public spaces, environmental security). 
+ Other factors: Health indices or short form surveys. 
+ General health status: mortality, health indices or short form surveys.

__Comparator/Control__   
None

__Type of study to be included__     
Longitudinal, cohort, and follow-up studies.    

__Context__  
+ Articles with a clear operationalization of successful or Healthy aging.
+ Articles with a clear operationalization of the variables associated with the development of Successful or healthy aging.
+ Articles whose main text has been translated into English or Spanish.
+ Articles from anywhere in the world.
+ Articles that report population over 45 years of age.

__Main Outcome__     
Successful or healthy aging clearly operationalized with any number of components and domains.  
+ Measures of effect__     
+ Odds ratio and Risk Ratio   
<br />

## Data extraction 
After the identification and the deduplication of the records retrieved through all the data bases. Inclusion and exclusion criteria will be applied to the title and abstract of all registries using a screening and selection tool. Criteria will follow PIcOS methodology:

- Population = community dwelling older adults (>45 years of age),
- Exposure = multi-dimensional factors associated with the development of Successful aging (SA) and Healthy aging (HA),
- Outcome = Successful aging or Healthy aging,
- Study Design = Longitudinal, cohort, and follow-up studies.   

Eligible articles will be retrieved, and the full text will be reviewed for eligibility. A sample of articles will be reviewed by two reviewers in the identification of title and abstract and screening phases, any discrepancies will be resolved by discussion of all reviewers. Kappa statistic will be assessed in the identification and screening phases.   
<br />
All data will be recorded in an excel spread sheet and processed with R. A repository with all the changes is kept locally. Information retrieved for each article will be: 1) Title, 2) Authors, 3) Publication year, 4) Digital object identifier, 5) Journal, 6) Country, 7) Sample size, 8) Mean age after follow-up, 9) Age range after follow-up, 10) years of follow-up, 11) Number of waves of follow-up, 12) SA or HA theorical frame work, 13) Percentage of SA or HA at the end of the study, 14) Components used to defined SA or HA, 15) measure of effect (OR or RR), 16) factors associated with SA or HA, 17) measure of effect for each factor, 18) Confidence interval for each factor.   
<br />
## __Risk of bias (Quiality assessment)__ 
The risk of bias will be assessed using the Newcastle-Ottawa Scale (NOS) for Assessing the Quality of Nonrandomized Studies in Meta-Analysis for Cohort Studies. Three broad perspectives will be assessed: Selection of the study group, the comparability of the groups, and the ascertainment of outcome of interest for cohort studies.

## __Strategy for data synthesis__
Our main objective of the meta-analysis is to establish the effects of different domains on the development of SA and HA. We will generate a table with the information of all study’s characteristics. 

We will use R and RStudio software to conduct the meta-analysis. We will first pool ratio measures of effect OR and RR using random effect models, the results will be presented with a forest plot. Heterogeneity will be measured using I2 Statistic. We will consider 25%, 50%, and 75% as low, moderate, and high heterogeneity. If a study reports different results for women and men, both results will be included in the analysis, except in case where a result for the mixed population is provided. Publication bias will be assess using Egger test.

### __Subgroup analysis__
In order to examine the effect of different variables on the association between each domain with SA and HA, we will consider the effects of all subgroups of each domain. Secondary analysis will be performed to examine the association of all domains with different opalizations of SA and HA, this analysis will consider which domains were used to define SA and HA (health, psychological, behavioral, social, or an SA or HA index).
