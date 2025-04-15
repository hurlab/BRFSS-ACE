
This project includes the scripts related to the following manuscript. 
### Title: Adverse childhood experiences and chronic health outcomes: Evidence from 33 US states in the Behavioral Risk Factor Surveillance System, 2019-2023
### Authors  
Christianah Jemiyo<sup>\*</sup>, Brett A. McGregor<sup>\*</sup>, Hasin Rehana, Junguk Hur<sup>\$</sup>

### Affiliation  
Department of Biomedical Sciences, University of North Dakota School of Medicine and Health Sciences, Grand Forks, ND 58202, USA

**<sup>\*</sup>These authors contributed equally.**  
**<sup>\$</sup>Corresponding author:** junguk.hur@med.und.edu (J.H.); Telephone: +1-701-777-6814 (J.H.)  
**Contact address:** Department of Biomedical Sciences, University of North Dakota School of Medicine and Health Sciences, Grand Forks, ND 58202, USA

### Abstract
**Background:** Recent evidence suggests a significant association between adverse childhood experience (ACE) and chronic health outcomes among U.S. adults. However, there remains a critical need to explore these associations specifically with respect to racial disparities. Early adversity and preexisting health vulnerabilities may interact, compounding the risk of chronic diseases in historically marginalized populations. This study further explored the relationship between ACE and chronic disease, recognizing that ACEs may exert a more pronounced effect in racial and ethnic groups already at elevated risk. To investigate this relationship, subgroup analysis was conducted to explore variations by race and ethnicity.  
**Methods:** We analyzed data from the Behavioral Risk Factor Surveillance System (BRFSS) collected from 33 states between 2019 and 2023. ACE scores were categorized as none, low (1–2), or high (3+). Log-binomial regression assessed the relationship between ACE scores and 17 health outcomes. Subgroup analyses examined variation by race/ethnicity, and geographic patterns were summarized by state. All analyses accounted for age, sex, race/ethnicity, income, and education.  
**Results:** Of 359,507 participants, 24.4% reported high ACE exposure. Emotional abuse, parental separation, and household substance abuse were the most reported ACEs. Individuals with high ACE exposure had higher risks of depression, smoking, coronary heart disease, and other conditions. Racial disparities were evident in the subgroup analysis. While white respondents with high ACE were significantly associated with many health outcomes, other races/ethnicities often demonstrated higher risk ratios when significant. Particularly, AIAN respondents showed the highest national-level risk for conditions such as heart attack, coronary heart disease, and stroke. Geographically, ACE prevalence and health-related outcomes varied by state, with Oregon and Nevada exhibiting the highest mean ACE scores.  
**Discussion:** High ACE scores are associated with chronic disease and mental health issues. These findings highlight significant racial and geographic disparities in ACE exposure and its health impacts. Addressing ACEs holistically by considering state-related factors and predisposed health risks among racial/ethnic groups is an emerging need. State-level policies focused on trauma prevention, particularly for vulnerable racial groups and high-risk geographic areas, may help implement interventions tailored to address the unique associations between ACEs and health outcomes in diverse populations.

## Directory Description
| File/Folder Name                           | Description                                                                                                 |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------------|
| `race_analysis_results_new`               | Contains the results of subgroup regression analyses stratified by race across all states.                 |
| `results`                                 | Summarizes ACE counts and prevalence, sociodemographic characteristics, and multivariable regression results, organized by state. |
| `RR_by_states`                            | Includes state-level maps visualizing risk ratios for health outcomes by ACE exposure.                     |
| `state_race_analysis`                     | Holds the log-binomial regression outputs stratified by race for North Dakota and South Dakota.            |
| `state_race_analysis_results`            | Stores summary counts and ACE category distributions by race (e.g., AIAN) and state (e.g., ND and SD).     |
| `state_race_response_by_prop`            | Contains proportional distributions of race responses per state.                                           |
| `ACE_and_17_health_outcomes_BRFSS_2019_2023_data_analysis.R` | Main R script performing data processing, ACE score computation, and analysis of associations with 17 health outcomes using BRFSS 2019–2023 data. |
| `state_based_data_availability_map`      | Visualizes which U.S. states included ACE questions in BRFSS across the 2019–2023 survey years.            |
| `state_level_ACE_distribution_map`       | Displays the distribution of mean ACE scores at the state level across all available BRFSS data.           |

### Citation
Jemiyo C<sup>\*</sup>, McGregor BA<sup>\*</sup>, Rehana H, and Hur J. Adverse childhood experiences and chronic health outcomes: Evidence from 33 US states in the Behavioral Risk Factor Surveillance System, 2019-2023. <b><i>BMC Public Health</i></b>. (In press)<br><br>
