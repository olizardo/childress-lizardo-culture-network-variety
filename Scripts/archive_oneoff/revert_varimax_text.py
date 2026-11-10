#!/usr/bin/env python3
"""
Scripts/revert_varimax_text.py

One-time text revision pass reverting the "Practice and Networks" Google Doc
from the September 2026 oblimin specification back to varimax (the standing
rotation choice as of the local pipeline revert). All old/new text below was
extracted verbatim from a fresh drive_download() of the live document
(FULL_PARAGRAPH_EDITS) or copied from the same fresh download for the three
Discussion-section substrings (SUBSTRING_EDITS), and all replacement numbers
were freshly recomputed from cache/01_prepared_data.rds / cache/02_models.rds
under the reverted (varimax) pipeline -- NOT copied verbatim from the
original pre-oblimin memory record, since package-version drift means the
current varimax pipeline's exact numbers differ slightly from the very first
varimax run (see AGENTS.md Section 5).

Two of the fifteen paragraphs (Table 1 composition discussion; Table 2
composition discussion; Table A3 Wald test) reuse the ORIGINAL pre-oblimin
narrative verbatim because the current reverted numbers were checked and
found to still support that exact narrative. All others were rewritten with
freshly computed figures.

Usage: revert_varimax_text.py <in.docx> <out.docx>
"""

import sys
sys.path.insert(0, "Scripts")
from docx_text_edits import revise_docx, RevisionError

FULL_PARAGRAPH_EDITS = [
    # 1. Activities rotation method
    (
        "We construct our primary outcome variables for cultural consumption and "
        "participation from these items. To that end, we used principal component "
        "factor analysis and retained a three-factor solution (eigenvalues > 1; "
        "KMO = .958), which together explained 58.7% of the variance. We rotate "
        "this solution obliquely (direct oblimin) rather than orthogonally "
        "(varimax), since there is no substantive reason to assume that public "
        "arts participation, everyday solitary leisure, and residual practical "
        "leisure are uncorrelated with one another; oblimin lets the data, rather "
        "than the rotation method, determine the association among these "
        "dimensions. We generated cultural consumption and participation "
        "variables for each respondent from the predicted factor scores for each "
        "observation, standardized to have a mean of 0 and a standard deviation "
        "of 1. ",
        "We construct our primary outcome variables for cultural consumption and "
        "participation from these items. To that end, we used principal component "
        "factor analysis with varimax rotation and retained a three-factor "
        "solution (eigenvalues > 1; KMO = .958), which together explained 58.7% "
        "of the variance. We generated cultural consumption and participation "
        "variables for each respondent from the predicted factor scores for each "
        "observation, standardized to have a mean of 0 and a standard deviation "
        "of 1. ",
    ),
    # 2. Activities Factor 1 (arts)
    (
        "The first factor accounted for 42.8% of the variance.  As seen in Figure "
        "1, Factor 1 is characterized by high levels of public arts "
        "participation, including symphony and opera attendance (loading = 0.88), "
        "dance performances (0.88), theater or musical performances (0.84), art "
        "galleries (0.83), live music concerts (0.82), museum visits (0.80), "
        "festivals (0.77), historic site visits (0.71), and movie theater "
        "attendance (0.69). ",
        "The first factor accounted for 39.9% of the variance.  As seen in Figure "
        "1, Factor 1 is characterized by high levels of public arts "
        "participation, including symphony and opera attendance (loading = 0.84), "
        "dance performances (0.84), theater or musical performances (0.82), art "
        "galleries (0.81), amusement park or carnival attendance (0.81), live "
        "music concerts (0.81), museum visits (0.77), festival attendance (0.75), "
        "and sports attendance (0.74). ",
    ),
    # 3. Activities Factor 2/3 (leisure/residual)
    (
        "The second factor accounted for 9.1% of the variance and is "
        "characterized by recreational leisure activities including \u201cgoing "
        "for walks\u201d (loading = 0.85), exercise or athletic activities (0.49), "
        "reading novels (0.43), gardening (0.33), going to the library (0.33), "
        "and hiking (0.29). We interpret this factor as everyday solitary "
        "leisure. The third factor accounts for 6.8% of the variance, and we "
        "interpret it as essentially a residual category, with loadings on "
        "gardening (0.54), home auto repair (0.41), hiking or camping (0.23), "
        "and a strong negative loading for fast food consumption (\u20130.69). "
        "Because we rotate obliquely rather than orthogonally, we also note "
        "that all three activity factors are only weakly correlated with one "
        "another (|r| \u2264 .25), consistent with treating them as substantively "
        "distinct outcomes.",
        "The second factor accounted for 11.7% of the variance and is "
        "characterized by recreational leisure activities including \u201cgoing "
        "for walks\u201d (loading = 0.81), exercise or athletic activities (0.56), "
        "reading novels (0.48), going to the library (0.42), hiking (0.39), and "
        "gardening (0.37). We interpret this factor as everyday solitary "
        "leisure. The third factor accounts for 7.1% of the variance, and we "
        "interpret it as essentially a residual category, with loadings on "
        "gardening (0.57), home auto repair (0.46), hiking or camping (0.28), "
        "and a strong negative loading for fast food consumption (\u20130.68).",
    ),
    # 4. Network ties rotation method
    (
        "For each group, respondents answered on the following five-point, "
        "ordered scale with the following options: None/0 people; 1 person; 2 "
        "to 5 people; 6 to 10 people; more than 10 people.  For both weak and "
        "strong ties, we similarly rely on an exploratory principal component "
        "factor analysis to classify items into broader classes, again "
        "rotating the solution obliquely (direct oblimin) rather than "
        "orthogonally (varimax): there is no theoretical reason to expect that "
        "knowing more ideologically liberal alters and knowing more "
        "ideologically conservative alters are uncorrelated, since both may "
        "simply reflect a respondent\u2019s overall network size or sociability "
        "rather than opposite poles of a single ideological spectrum. In both "
        "cases, we retain three-factor solutions (eigenvalues > 1; KMOs = .938 "
        "for weak ties and .935 for strong ties), with each solution explaining "
        "approximately 61% of the total variance. As we did for the cultural "
        "participation and leisure activities items, we calculated personal "
        "network variety and composition variables for each respondent from "
        "the predicted factor scores. These scores are standardized to have a "
        "mean of zero and a standard deviation of 1. ",
        "For each group, respondents answered on the following five-point, "
        "ordered scale with the following options: None/0 people; 1 person; 2 "
        "to 5 people; 6 to 10 people; more than 10 people.  For both weak and "
        "strong ties, we similarly rely on an exploratory principal component "
        "factor analysis with varimax rotation to classify items into broader "
        "classes. In both cases, we retain three-factor solutions (eigenvalues "
        "> 1; KMOs = .938 for weak ties and .935 for strong ties), with each "
        "solution explaining approximately 61% of the total variance. As we "
        "did for the cultural participation and leisure activities items, we "
        "calculated personal network variety and composition variables for "
        "each respondent from the predicted factor scores. These scores are "
        "standardized to have a mean of zero and a standard deviation of 1. ",
    ),
    # 5. Network Factor 1 (variety)
    (
        "Factor 1 explains 20.1% of the variance in the weak-tie model and "
        "25.4% in the strong-tie model. Across both weak- and strong-tie "
        "networks, this factor loads positively across a wide range of social "
        "groups rather than clustering around a single category. In the weak "
        "tie model, Factor 1 is anchored by high loadings on knowing Native "
        "Hawaiian or Pacific Islander (.85), American Indian (.80), MENA (.76), "
        "and Asian (.59) individuals, with additional loadings on class-coded "
        "ties (second-home owners; .56) and immigrant-origin contacts (knowing "
        "someone born outside the U.S.; .45); the strong tie model shows a "
        "highly similar pattern, including strong loadings on Pacific Islander "
        "(.87), MENA (.84), American Indian (.81), Asian (.67), "
        "immigrant-origin (.62), and second-home owner (.60) ties. We interpret "
        "Factor 1 as capturing network variety rather than ties to any "
        "particular group or orientation, as we show in the supplementary "
        "analysis in the appendix.",
        "Factor 1 explains 20.1% of the variance in the weak-tie model and "
        "23.8% in the strong-tie model. Across both weak- and strong-tie "
        "networks, this factor loads positively across a wide range of social "
        "groups rather than clustering around a single category. In the weak "
        "tie model, Factor 1 is anchored by high loadings on knowing Native "
        "Hawaiian or Pacific Islander (.83), American Indian (.79), MENA (.77), "
        "and Asian (.63) individuals, with additional loadings on class-coded "
        "ties (second-home owners; .59) and immigrant-origin contacts (knowing "
        "someone born outside the U.S.; .51); the strong tie model shows a "
        "highly similar pattern, including strong loadings on Pacific Islander "
        "(.81), MENA (.80), American Indian (.78), Asian (.66), "
        "immigrant-origin (.63), and second-home owner (.63) ties. We interpret "
        "Factor 1 as capturing network variety rather than ties to any "
        "particular group or orientation, as we show in the supplementary "
        "analysis in the appendix.",
    ),
    # 6. Network Factor 2/3 (ideology)
    (
        " In both weak and strong tie networks, Factor 2, which explains  "
        "23.9% of the variance for weak ties and 14.0% for strong ties, loads "
        "most strongly on left-leaning ideology and aligned groups. For weak "
        "ties, Factor 2 is anchored by knowing very liberal (.78) and LGBTQ "
        "(.76) individuals, those with low church attendance (.62) and "
        "Hispanic/Latine contacts (.62), with a secondary loading on urban "
        "ties (.56). The strong tie model shows a similar concentration on "
        "very liberal (.76) and LGBTQ (.69) ties, alongside low church "
        "attendance (.49) and urban (.45) contacts. In turn, Factor 3, which "
        "explains 17.3% of the variance for weak ties and 21.8% for strong "
        "ties, loads most strongly on conservative and religious social "
        "environments. In the weak tie model, this factor is anchored by "
        "knowing very conservative individuals (.84) and frequent churchgoers "
        "(.70), with secondary loadings on rural ties (.68) and second-home "
        "owners (.38); the strong tie model is centered on similarly strong "
        "loadings for knowing white (.80), very conservative (.79), women "
        "(.70), and frequent churchgoing (.70) contacts, alongside rural ties "
        "(.69), indicating that for strong ties this factor also picks up a "
        "broader conventional/mainstream social profile alongside political "
        "conservatism. ",
        " In both weak and strong tie networks, Factor 2, which explains "
        "22.9% of the variance for weak ties and 17.9% for strong ties, loads "
        "most strongly on left-leaning ideology and aligned groups. For weak "
        "ties, Factor 2 is anchored by knowing very liberal (.75) and LGBTQ "
        "(.73) individuals, Hispanic/Latine contacts (.64), and men (.63), "
        "with a secondary loading on low church attendance (.61). The strong "
        "tie model shows a similar concentration on very liberal (.81) and "
        "LGBTQ (.72) ties, alongside low church attendance (.59) and men "
        "(.56). In turn, Factor 3, which explains 18.2% of the variance for "
        "weak ties and 19.6% for strong ties, loads most strongly on "
        "conservative and religious social environments. In the weak tie "
        "model, this factor is anchored by knowing very conservative "
        "individuals (.80) and frequent churchgoers (.71), with secondary "
        "loadings on knowing white (.68) and rural (.67) contacts; the strong "
        "tie model is centered on similarly strong loadings for knowing very "
        "conservative (.78), white (.73), and frequent churchgoing (.71) "
        "contacts, alongside rural ties (.65). ",
    ),
    # 7. Factor 3 naming / correlation sentence
    (
        "We interpret Factor 3 as capturing right-leaning ideology and aligned "
        "groups, which remains conceptually distinct from the variety-oriented "
        "networks captured by Factor 1 and the liberal-aligned groups of Factor "
        "2, though under the oblique rotation we use here these three dimensions "
        "are allowed to (and do) correlate rather than being forced to "
        "independence: variety correlates modestly with both liberal (r = .35 "
        "weak, .26 strong) and conservative (r = .25 weak, .44 strong) "
        "composition, and liberal and conservative composition are themselves "
        "modestly positively correlated with one another (r = .39 weak, .28 "
        "strong) rather than negatively related, indicating that knowing more "
        "ideologically liberal and more ideologically conservative alters are not "
        "opposite poles of a single spectrum but partly co-occur, plausibly "
        "reflecting respondents\u2019 overall network breadth. We thus refer to Factor "
        "1 as weak and strong-tie variety, and to Factors 2 and 3 as weak and "
        "strong-tie liberal and conservative composition. ",
        "We interpret Factor 3 as capturing right-leaning ideology and aligned "
        "groups, which is clearly distinct from the variety-oriented networks "
        "captured by Factor 1 and the liberal-aligned groups of Factor 2; "
        "because we retain the orthogonal (varimax) rotation used for the "
        "activities battery, these three dimensions are forced to be "
        "uncorrelated by construction, so no inter-factor correlations are "
        "reported for this battery. We thus refer to Factor 1 as weak and "
        "strong-tie variety, and to Factors 2 and 3 as weak and strong-tie "
        "liberal and conservative composition. ",
    ),
    # 8. Arts Wald test / Figure 3 paragraph
    (
        "Across all models, and consistent with the network variety model, "
        "both weak- and strong-tie variety predict increasing frequencies of "
        "public arts participation. The coefficient estimate for the "
        "strong-tie network variable is more than twice that for the "
        "weak-tie variable\u2014a statistically significant difference (F(1, "
        "1213) = 19.41, p < .001)\u2014indicating that a diverse range of strong "
        "ties has a greater effect on driving public arts participation.  "
        "This difference is illustrated in Figure 3, which plots the "
        "predicted score in the public arts participation factor on the "
        "y-axis as a function of the respondent\u2019s score in the strong-tie "
        "(solid line) and weak-tie (dashed line) variety factors on the "
        "x-axis. The Figure shows that a one-unit increase in the strong tie "
        "variety score corresponds to roughly a half-standard deviation "
        "increase in arts participation, after adjusting for other "
        "socio-demographic factors. Note also that the effect of weak-tie "
        "variety is more strongly mediated by the inclusion of the "
        "socio-demographic factors in Model 2, and is reduced by about half "
        "compared to Model 1. The effect of strong tie variety on public arts "
        "participation, on the other hand, is almost entirely direct, with "
        "the estimate being only slightly lower in Model 2 than in Model 1. ",
        "Across all models, and consistent with the network variety model, "
        "both weak- and strong-tie variety predict increasing frequencies of "
        "public arts participation. The coefficient estimate for the "
        "strong-tie network variable is more than twice that for the "
        "weak-tie variable\u2014a statistically significant difference (F(1, "
        "1211) = 19.20, p < .001)\u2014indicating that a diverse range of strong "
        "ties has a greater effect on driving public arts participation.  "
        "This difference is illustrated in Figure 3, which plots the "
        "predicted score in the public arts participation factor on the "
        "y-axis as a function of the respondent\u2019s score in the strong-tie "
        "(solid line) and weak-tie (dashed line) variety factors on the "
        "x-axis. The Figure shows that a one-unit increase in the strong tie "
        "variety score corresponds to roughly a half-standard deviation "
        "increase in arts participation, after adjusting for other "
        "socio-demographic factors. Note also that the effect of weak-tie "
        "variety is more strongly mediated by the inclusion of the "
        "socio-demographic factors in Model 2, and is reduced by about half "
        "compared to Model 1. The effect of strong tie variety on public arts "
        "participation, on the other hand, is almost entirely direct, with "
        "the estimate being only slightly lower in Model 2 than in Model 1. ",
    ),
    # 9. Race / political ideology sociodemographic paragraph (Table 1, Model 2)
    (
        "The statistically significant sociodemographic effects in Model 2 are "
        "consistent with what we would expect from previous work. Education and "
        "childhood exposure to the arts increase the contemporaneous frequency of "
        "public arts consumption. In contrast, participation in the public arts "
        "declines with age and is higher among self-identified white respondents "
        "than among AAPI (p < .001) or multiracial respondents (p = .005); the "
        "corresponding contrast with Hispanic/Latine respondents is in the same "
        "direction but only marginally significant (p = .07). Interestingly, we "
        "find that political ideology has a substantively small but statistically "
        "significant net effect: more conservative respondents report a higher "
        "frequency of participation in public arts than liberals. This contrasts "
        "with previous work that has examined the impact of political ideology on "
        "self-reported taste for cultural items, which generally finds that "
        "liberals like a broader range of cultural items (Rawlings and Childress "
        "2023), suggesting a disjuncture between the effects of political "
        "ideology on self-reported taste and participation.",
        "The statistically significant sociodemographic effects in Model 2 are "
        "consistent with what we would expect from previous work. Education and "
        "childhood exposure to the arts increase the contemporaneous frequency of "
        "public arts consumption. In contrast, participation in the public arts "
        "declines with age and is higher among self-identified white respondents "
        "than among AAPI (p < .001) or multiracial respondents (p < .001); the "
        "corresponding contrast with Hispanic/Latine respondents is in the same "
        "direction but only marginally significant (p = .05). Interestingly, we "
        "find that political ideology has a substantively small but statistically "
        "significant net effect: more conservative respondents report a higher "
        "frequency of participation in public arts than liberals. This contrasts "
        "with previous work that has examined the impact of political ideology on "
        "self-reported taste for cultural items, which generally finds that "
        "liberals like a broader range of cultural items (Rawlings and Childress "
        "2023), suggesting a disjuncture between the effects of political "
        "ideology on self-reported taste and participation.",
    ),
    # 10. Table 1 composition discussion (arts)
    (
        "As shown in Models 3 and 4, adding ideological network composition "
        "does little to attenuate the effects of network variety for either "
        "weak or strong ties, suggesting that the factor analyses capture "
        "distinct sources of variance in the personal networks of U.S. "
        "respondents. After adjusting for relevant sociodemographic "
        "covariates in Model 4, we find that weak ties to people in liberal "
        "positions are associated with lower levels of arts participation "
        "(b = -.13, p = .001); this is the only one of the four composition "
        "coefficients that reaches conventional significance in this model, "
        "as strong-tie liberal composition (b = -.04, p = .27), weak-tie "
        "conservative composition (b = .01, p = .73), and strong-tie "
        "conservative composition (b = .03, p = .40) are not statistically "
        "distinguishable from zero. Note that the direct positive effect of "
        "conservative political ideology on arts participation in Model 2 "
        "(b = .03, p = .03) is nonetheless fully mediated by personal "
        "network composition: when ideological network composition is "
        "included in Model 4, the coefficient estimate is now close to zero "
        "and no longer statistically significant (b = -.01, p = .46), "
        "suggesting that the greater likelihood of those who identify as "
        "conservative reporting higher levels of arts participation is "
        "explained by their broader pattern of network composition rather "
        "than any single ideological tie category, consistent with a "
        "homophily effect (Mark 1998; McPherson et al. 2001). ",
        "As shown in Models 3 and 4, adding ideological network composition "
        "does little to attenuate the effects of network variety for either "
        "weak or strong ties, suggesting that the factor analyses capture "
        "distinct sources of variance in the personal networks of U.S. "
        "respondents. Interestingly, the two principal axes of network "
        "composition pull arts participation in different directions "
        "depending on tie strength. After adjusting for relevant "
        "sociodemographic covariates in Model 4, we find that weak ties to "
        "people in liberal positions are associated with lower levels of "
        "arts participation. In contrast, strong ties to people in "
        "conservative positions are associated with higher levels. Note that "
        "the direct positive effect of conservative political ideology on "
        "arts participation in Model 2 is fully mediated by personal network "
        "composition. When ideological network composition is included in "
        "Model 4, the coefficient estimate is now close to zero and no "
        "longer statistically significant, suggesting that the greater "
        "likelihood of those who identify as conservative reporting higher "
        "levels of arts participation is explained by their stronger ties "
        "to other persons in more \u201cconservative\u201d social positions, "
        "consistent with a homophily effect (Mark 1998; McPherson et al. "
        "2001). ",
    ),
    # 11. Table 2 intro / variety range (solitary leisure)
    (
        "Table 2 examines the network predictors of everyday solitary leisure, "
        "again focusing on weak- and strong-tie network variety in Models 1 "
        "and 2, with measures of ideological network composition added in "
        "Models 3 and 4. In contrast to public-arts models, the variety of "
        "weak- and strong-tie personal networks exerts countervailing "
        "influences. Across all specifications, weak-tie variety is "
        "positively associated with everyday solitary leisure, whereas "
        "strong-tie variety is negatively associated, net of other "
        "predictors. Substantively, a one-unit increase in the predicted "
        "factor score for weak-tie variety corresponds to an increase of "
        "approximately 0.13 to 0.21 standard deviations in solitary leisure "
        "across the four model specifications. In contrast, greater "
        "strong-tie variety is associated with a roughly 0.08 to 0.17 "
        "standard deviation decrease in the frequency of engagement in "
        "everyday leisure activities. As before, we observe a somewhat "
        "stronger mediation of the weak-tie variety effect by "
        "socio-demographic variables than for the strong-tie variety, "
        "although not to the same extent as in the public arts participation "
        "models. Overall, it seems that, given the strong positive effect of "
        "variety in strong ties on more active forms of arts participation, "
        "they function here as a mechanism to funnel people into those more "
        "public pursuits, thereby depressing participation in competing "
        "solitary leisure activities. Weak ties, in contrast, given their "
        "less demanding nature, facilitate involvement across the entire "
        "range of leisure and cultural consumption activities. ",
        "Table 2 examines the network predictors of everyday solitary leisure, "
        "again focusing on weak- and strong-tie network variety in Models 1 "
        "and 2, with measures of ideological network composition added in "
        "Models 3 and 4. In contrast to public-arts models, the variety of "
        "weak- and strong-tie personal networks exerts countervailing "
        "influences. Across all specifications, weak-tie variety is "
        "positively associated with everyday solitary leisure, whereas "
        "strong-tie variety is negatively associated, net of other "
        "predictors. Substantively, a one-unit increase in the predicted "
        "factor score for weak-tie variety corresponds to an increase of "
        "approximately 0.14 to 0.23 standard deviations in solitary leisure "
        "across the four model specifications. In contrast, greater "
        "strong-tie variety is associated with a roughly 0.13 to 0.18 "
        "standard deviation decrease in the frequency of engagement in "
        "everyday leisure activities. As before, we observe a somewhat "
        "stronger mediation of the weak-tie variety effect by "
        "socio-demographic variables than for the strong-tie variety, "
        "although not to the same extent as in the public arts participation "
        "models. Overall, it seems that, given the strong positive effect of "
        "variety in strong ties on more active forms of arts participation, "
        "they function here as a mechanism to funnel people into those more "
        "public pursuits, thereby depressing participation in competing "
        "solitary leisure activities. Weak ties, in contrast, given their "
        "less demanding nature, facilitate involvement across the entire "
        "range of leisure and cultural consumption activities. ",
    ),
    # 12. Childhood arts / other controls (solitary leisure, Model 2)
    (
        "Looking at Model 2, we see that the effects of other socio-demographic "
        "variables are similar to those observed in the public arts participation "
        "models, though there are also notable contrasts. Education increases the "
        "frequency of engagement in more solitary leisure activities, as does "
        "exposure to childhood arts, whose effect attenuates somewhat but remains "
        "statistically significant once network composition variables are "
        "introduced in Model 4 (b = .04, p = .02). Age does not affect solitary "
        "leisure, which makes sense given the less demanding nature of these "
        "activities compared to participation in the public arts. Higher "
        "household income increases solitary leisure activities (in contrast to "
        "its null effect on public arts participation), as does "
        "self-identification as a woman, consistent with previously observed "
        "gendering of such activities, such as gardening, yoga, and, "
        "particularly, fiction reading (Long 2003). Finally, we also observe a "
        "net effect of political ideology on solitary leisure activities, with "
        "liberals more likely to engage in them than conservatives.",
        "Looking at Model 2, we see that the effects of other socio-demographic "
        "variables are similar to those observed in the public arts participation "
        "models, though there are also notable contrasts. Education increases the "
        "frequency of engagement in more solitary leisure activities, as does "
        "exposure to childhood arts, whose effect attenuates somewhat but remains "
        "statistically significant once network composition variables are "
        "introduced in Model 4 (b = .04, p = .04). Age does not affect solitary "
        "leisure, which makes sense given the less demanding nature of these "
        "activities compared to participation in the public arts. Higher "
        "household income increases solitary leisure activities (in contrast to "
        "its null effect on public arts participation), as does "
        "self-identification as a woman, consistent with previously observed "
        "gendering of such activities, such as gardening, yoga, and, "
        "particularly, fiction reading (Long 2003). Finally, we also observe a "
        "net effect of political ideology on solitary leisure activities, with "
        "liberals more likely to engage in them than conservatives.",
    ),
    # 13. Table 2 composition discussion (solitary leisure)
    (
        "As before, adding ideological network composition does little to "
        "attenuate the effects of network variety. In Models 3 and 4, strong ties "
        "to persons in \u201cliberal\u201d positions are positively associated with "
        "everyday solitary leisure (b = .11, p = .02), and weak-tie liberal "
        "composition shows a positive association of similar magnitude that falls "
        "just short of conventional significance (b = .09, p = .06). Ties to "
        "conservative positions show, at most, a comparably weak positive "
        "association for strong ties (b = .09, p = .09) and none for weak ties (b "
        "= .02, p = .62), so we read this pattern cautiously as suggesting that "
        "liberal-leaning ties are the more consistent compositional correlate of "
        "solitary leisure, rather than as evidence of a sharp "
        "liberal/conservative asymmetry. Furthermore, as observed for public arts "
        "participation, the association between political ideological "
        "self-placement and solitary leisure in Model 2 (b = -.04, p = .01) is "
        "fully mediated by network composition: the Model 4 estimate falls to "
        "near zero and loses significance (b = -.01, p = .66), suggesting that "
        "the higher likelihood of liberals reporting engagement in everyday "
        "leisure is explained by their broader pattern of network composition "
        "rather than any single ideological tie category, also consistent with a "
        "homophily effect (Mark 1998; McPherson et al. 2001). ",
        "As before, adding ideological network composition does little to "
        "attenuate the effects of network variety. However, ideological "
        "composition plays a more consistent role in this domain. In Models "
        "3 and 4, both weak and strong ties to persons in \u201cliberal\u201d "
        "positions are positively associated with everyday solitary "
        "leisure, while ties to conservatives have no discernible impact. "
        "Furthermore, as observed for public arts participation, the "
        "association between political ideological self-placement and "
        "solitary leisure in Model 2 is fully mediated by network "
        "composition, suggesting that the higher likelihood of liberals "
        "reporting engagement in everyday leisure is explained by their "
        "higher propensity to have personal networks rich in strong and "
        "weak ties to people in \u201cliberal\u201d positions, also consistent "
        "with a homophily effect (Mark 1998; McPherson et al. 2001). ",
    ),
    # 14. Table A3 (residual leisure) Wald test
    (
        "Consistent with this expectation, none of the four composition "
        "coefficients approaches statistical significance in Models 3 or 4 of "
        "Table A3, and formal Wald tests confirm that the composition block adds "
        "no explanatory power over models with variety alone (\u03c7\u00b2(4) = 4.05, p = "
        ".399) or over models that already include the full set of "
        "socio-demographic controls (\u03c7\u00b2(4) = 7.07, p = .132). This null pattern, "
        "set alongside the associations we observe for public arts participation "
        "and solitary leisure, supports treating our composition measures as "
        "tapping domain-specific cultural and leisure engagement rather than an "
        "artifact correlated with any activity measure in the battery.",
        "Consistent with this expectation, none of the four composition "
        "coefficients approaches statistical significance in Models 3 or 4 of "
        "Table A3, and formal Wald tests confirm that the composition block adds "
        "no explanatory power over models with variety alone (\u03c7\u00b2(4) = 5.99, p = "
        ".200) or over models that already include the full set of "
        "socio-demographic controls (\u03c7\u00b2(4) = 5.61, p = .231). This null pattern, "
        "set alongside the associations we observe for public arts participation "
        "and solitary leisure, supports treating our composition measures as "
        "tapping domain-specific cultural and leisure engagement rather than an "
        "artifact correlated with any activity measure in the battery.",
    ),
    # 15. Table A2 variety-count validation
    (
        "To further assess the interpretation of Factor 1 as capturing "
        "network variety, we examined its association with independently "
        "constructed variety count measures of the number of distinct "
        "social groups respondents reported knowing. Variety counts were "
        "calculated as the number of groups for which respondents reported "
        "knowing at least one person, and in Table A2, associations are "
        "reported as Pearson correlations. For strong ties, the network "
        "variety factor remains the most strongly correlated of the three "
        "factors with these observed counts (r = .79, versus .54 and .73 "
        "for liberal and conservative composition, respectively). For weak "
        "ties, this discriminant pattern is less clean under the oblique "
        "rotation: the liberal-composition factor correlates about as "
        "strongly with the raw group count (r = .76) as does the variety "
        "factor itself (r = .70), and conservative composition is not far "
        "behind (r = .62). This is an expected consequence of allowing the "
        "three weak-tie factors to correlate rather than forcing them to "
        "independence: respondents with broader weak-tie networks tend to "
        "know more people across all eighteen groups, including "
        "ideologically-coded ones, so the ideological composition factors "
        "partly track sheer network breadth as well as ideological "
        "content. Results are substantively similar when using a stricter "
        "threshold that requires respondents to report knowing two or more "
        "people in a given group. We therefore read Factor 1 as capturing "
        "cross-group breadth most cleanly for strong ties, while for weak "
        "ties this validation check is suggestive rather than conclusive, "
        "and we interpret the weak-tie composition estimates in Tables 1\u20132 "
        "and A3 accordingly.",
        "To further assess the interpretation of Factor 1 as capturing "
        "network variety, we examined its association with independently "
        "constructed variety count measures of the number of distinct "
        "social groups respondents reported knowing. Variety counts were "
        "calculated as the number of groups for which respondents reported "
        "knowing at least one person, and in Table A2, associations are "
        "reported as Pearson correlations. For strong ties, the network "
        "variety factor is the most strongly correlated of the three "
        "factors with these observed counts (r = .61, versus .50 and .50 "
        "for liberal and conservative composition, respectively). For weak "
        "ties, this discriminant pattern is less clean: liberal composition "
        "correlates about as strongly with the raw group count (r = .59) as "
        "does the variety factor itself (r = .56), and conservative "
        "composition is not far behind (r = .46). This likely reflects the "
        "fact that respondents with broader weak-tie networks tend to know "
        "more people across all eighteen groups, including "
        "ideologically-coded ones, so the ideological composition factors "
        "partly track sheer network breadth as well as ideological "
        "content. Results are substantively similar when using a stricter "
        "threshold that requires respondents to report knowing two or more "
        "people in a given group. We therefore read Factor 1 as capturing "
        "cross-group breadth most cleanly for strong ties, while for weak "
        "ties this validation check is suggestive rather than conclusive, "
        "and we interpret the weak-tie composition estimates in Tables 1\u20132 "
        "and A3 accordingly.",
    ),
]

SUBSTRING_EDITS = [
    (
        "network variety in weak and strong ties operates in opposing "
        "directions, with",
        "network variety in weak and strong ties operates orthogonally to "
        "each other, with",
    ),
    (
        "with those with liberal-leaning weak ties engaging in less "
        "conventional arts participation, which those with more liberal "
        "strong ties, and to a lesser extent liberal weak ties, seem to be "
        "making up for with more everyday solitary leisure.",
        "with those with liberal leaning weak ties engaging in less "
        "conventional arts participation and those with conservative "
        "strong ties engaging in more, which those with more liberal weak "
        "and strong ties seem to be making up for with more everyday "
        "solitary leisure.",
    ),
    (
        "Our finding that weak ties to liberals are associated with lower "
        "levels of conventional arts participation likely reflects",
        "Our finding that weak ties to liberals are associated with lower "
        "levels of conventional arts participation, while strong ties to "
        "conservatives are associated with higher levels, likely reflects",
    ),
]

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: revert_varimax_text.py <in.docx> <out.docx>")
        sys.exit(1)
    try:
        n_full, n_sub = revise_docx(
            sys.argv[1], sys.argv[2],
            full_paragraph_edits=FULL_PARAGRAPH_EDITS,
            substring_edits=SUBSTRING_EDITS,
        )
    except RevisionError as e:
        print(f"[ERROR] {e}")
        sys.exit(1)
    print(f"Applied {n_full} full-paragraph edits, {n_sub} substring edits. Wrote {sys.argv[2]}.")
