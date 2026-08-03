Notes PHS





HBT: Health Board of Treatment

Department Type names have changed and Type 1 replaces ED and Type 3 replaces MIU/Other.

* ED: Type 1. A core, consultant-led Emergency Department operating 24 hours a day, seven days a week
* MIU/Other: Type 3. A non-core service, usually a Minor Injuries Unit or similar facility, often nurse- or GP-led and potentially operating limited hours







DATA SETS NOTES:



**MONTH DEMOGRAPHICS:**



what does QF mean? and why is it empty or ":"? should we ingore this columns?



* Month --> 100 months

  * check if consecutive and which period
* Country --> 1 country (scotland)
* HBT --> health board of treatment (location)
* Department Type

  * ED: Type 1. A core, consultant-led Emergency Department operating 24 hours a day, seven days a week
  * MIU/Other: Type 3. A non-core service, usually a Minor Injuries Unit or similar facility, often nurse- or GP-led and potentially operating limited hour.
* Age: 7 blocks

  * check age blocks and distribution
* Sex:





download the main simd2020 data set and use it to extract the deprivation level of the area, rather the specific for each person?







Data sets columns:

* month demo: month (from 2018), HB, type 1 and 3, age, sex, deprovation, # attendances
* month activity: month (from 2007), HB, trt location, type 1 and 3, category(planned unplanned), time waiting blocks, # attendances
* trt location: HB, Trt location code and name
* month referrals: month (from 2018), HB, trt location, type 1 and 3, age, referral type, # attendances
* month discahrges: month (from 2018), HB, trt location, type 1 and 3, age, discharge type, # attendances
* month mutiple att: month (last 12 months), HB, type 1 and 3, age, sex, deprivation, # attendances
* month when: month (2018), HB, trt location, type 1 and 3, day in the week, time in the day, in/out  of hours,  # attendances



We also have on the side the population data, i have a few data sets but i think the most useful would be

* table 1: HB, Age, sex, population
* table 4: HB, area, pop density.



And in addition i have the SIMD data sets:

* the main one with the Data zone, HB and the quintiles (the ones we are using in this project)
* the SIMd with the specific indicators: Data zone, HB, total populatio, working age population, income, employiment, alcohol abuse, etc
* the SIMD lookup code: Postcode, datazone and quintile







**Main idea:**



* step 1: attendance risk --> Given Scotland's population, who is most likely to show up at A\&E?

  * identifies the demographic and socioeconomic factors (age, sex, deprivation, area-level indicators) that predict whether someone from a given population group attends A\&E at all.
  * The outcome is a population adjusted rate
  * **PREVENTION**
* step 2: admission --> Given that someone has already arrived at A\&E, what factors predict whether they get admitted to hospital?

  * the population at risk is no longer "everyone in Scotland," it's "everyone who actually attended A\&E."
  * The outcome shifts changes from a rate against external population to a proportion within the data itself (admitted vs. not admitted, among attenders only).
  * The predictors shift too: from demographic/deprivation factors to operational/clinical ones (referral source, time of day, department type, etc).
  * **OPERATIONAL PLANNING**









**Academic references for cyclic cubic spline**



The primary reference is Wood (2017) — the textbook, which is the definitive methodological source for everything in your GAM:



Wood, S.N. (2017). Generalized Additive Models: An Introduction with R (2nd edition). Chapman and Hall/CRC.



Section 4.1.2 specifically discusses cyclic spline bases and when to use them — the guidance is direct: cyclic smooths are appropriate whenever the covariate is periodic/circular (time of day, day of year, month of year, angle, etc.) and the function value at the start and end of the period should match by construction.



For a more applied health-services reference that uses this exact specification (cyclic smooth on month-of-year in an A\&E/emergency-department context), you could cite:



Bhaskaran, K., Gasparrini, A., Hajat, S., Smeeth, L., \& Armstrong, B. (2013). Time series regression studies in environmental epidemiology. International Journal of Epidemiology, 42(4), 1187–1195.



This paper discusses the use of cyclic temporal terms for seasonal adjustment in health outcome time series — not A\&E specifically, but the same methodological principle applied to the same class of problem (health event counts with seasonal periodicity).



































**Mama, muchísimas felicidades! Me da mucha pena no poder estar contigo en el día de tu cumple** 

**Estos últimos meses me he acordado mucho de un paseo que dimos por la playa en benicassim afínales de un verano, creo que iba a comenzar la eso. Yo te contaba lo nerviosa que estaba y al avez emocionada de comenzar esa nueva fase. lo recuerdo como una conversación de tu a tu, no tanto de madre a hija (que tambien) pero de amiga que sabia perfertamente como me sentía y que decía las cosas correctas para que yo viese que esas sensaciones eran completamente normales e incluso buenas, porque de eso se trata la vida, de sentir. Aunque esa conversación fuera algo muy pequeño, a mi me marco mucho y me ha guiado desde entonces**

