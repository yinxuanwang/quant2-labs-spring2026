In groups, choose the most appropriate strategy for analysing the given dataset and research question. Prepare a short presentation (no slides, you can just talk through code and figures) discussing why you chose the given strategy. Compare your results to another strategy that is less appropriate. Discuss the differences in the analysis


# Reunification

Country-year panel of gdp
Treatment variable is reunification of Germany post 1990s

Outcome: gdp 
Treatment: treat
Interesting covariates: "trade","infrate","industry", "schooling", "invest80"

# Collective

Effects of states implementing mandatory collective bargaining agreements for public sector unions

Year, State: The state and year of the measurement
YearCBrequired: The year that the state adopted mandatory collective bargaining
lnppexpend: Log per pupil expenditures in 2010 dollars 

# Turnout

State-level voter turnout data
policy_edr = election-day registration policy

# Kansas

Economic indicators for US states from 1990-2016

Format
A dataframe with 5250 rows and 32 variables:

fips
FIPS code for each state

year
Year of measurement

qtr
Quarter (1-4) of measurement

state
Name of State

gdp
Gross State Product (millions of $) Values before 2005 are linearly interpolated between years

revenuepop
State and local revenue per capita

rev_state_total
State total general revenue (millions of $)

rev_local_total
Local total general revenue (millions of $)

popestimate
Population estimate

qtrly_estabs_count
Count of establishments for a given quarter

month1_emplvl, month2_emplvl, month3_emplvl
Employment level for first, second, and third months of a given quarter

total_qtrly_wages
Total wages for a givne quarter

taxable_qtrly_wage
Taxable wages for a given quarter

avg_wkly_wage
Average weekly wage for a given quarter

year_qtr
Year and quarter combined into one continuous variable

treated
Whether the state passed tax cuts before the given year and quareter

lngdpcapita
Natural log of GDP per capita

emplvlcapita
Average employment level per capita

Xcapita
Per capita value of X

abb
State abbreviation