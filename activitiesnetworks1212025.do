clear all
use "C:\Users\ccchi\OneDrive\Desktop\Dropbox\02 This Week\001 Research\PoliticsSocSciData\grabbagdata111925.dta"

*DROP DUPLICATES
duplicates tag v8, gen(dup)
tab dup
bysort v8: keep if _n == 1

***DROP SPEEDERS
summarize duration, detail
scalar med_duration = r(p50)
scalar speed_threshold = med_duration / 3
drop if duration < speed_threshold

**ACTIVITIES 

factor museum art_gallery symphony_orchestra_opera gardening carnival_fair_amusement_park music_concert_festival play_or_musical library fancy_restaurant fast_food go_for_walk exercise_or_yoga dance_performance movie_theater read_novel_poem_or_play attend_sports home_auto_repair hiking_camping_boating historic_site go_to_festival, pcf

**THREE FACTORS
screeplot, yline(1)
**.9584
estat kmo

factor museum art_gallery symphony_orchestra_opera gardening carnival_fair_amusement_park music_concert_festival play_or_musical library fancy_restaurant fast_food go_for_walk exercise_or_yoga dance_performance movie_theater read_novel_poem_or_play attend_sports home_auto_repair hiking_camping_boating historic_site go_to_festival, pcf factors(3)
rotate, varimax

predict high_cc_arts independent_improvement low_cc_practical
label var high_cc_arts "High Cultural Capital Arts"
label var independent_improvement "Independent Self Improvement"
label var low_cc_practical "Low Cultural Capital DIY Repair"


****FIRST PASS AT MODELS

regress high_cc_arts strong_rotate_1_minoritygroups strong_rotate_1_conservative weak_rotate_1_majoritygroups weak_rotate_1_minoritygroups educ child_arts  income age2 i.gender i.race3 poli, vce(robust)

regress independent_improvement strong_rotate_1_minoritygroups strong_rotate_1_conservative weak_rotate_1_majoritygroups weak_rotate_1_minoritygroups educ child_arts  income age2 i.gender i.race3 poli, vce(robust)

regress low_cc_practical strong_rotate_1_minoritygroups strong_rotate_1_conservative weak_rotate_1_majoritygroups weak_rotate_1_minoritygroups educ child_arts  income age2 i.gender i.race3 poli, vce(robust)

