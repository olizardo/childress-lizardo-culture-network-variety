# Brainstorm: Approaches to Reviewer Methodological Comments

**Manuscript:** SSR-D-26-01726, *Beyond Network Variety in Arts Attendance: Network Composition, Ideological Sorting, and Everyday Leisure*

This document collects nine candidate approaches to the methodological concerns raised across the four reviews. Items are grouped by concern. Cheap fixes come first where that helps sequencing, and the suggested order appears at the end.

---

## 1. Measurement Model: PCA as a Starting Point, Not the Final Scale

**Reviewers:** 1, 4

Reviewers 1 and 4 both argue that PCA scores weight every item equally, so the constructs are not clearly defined.

- **Keep PCA/EFA for exploration and justify it.** Report parallel analysis to support the number of factors. Compute polychoric correlation matrices, since the network items are 5-point and the activity items are 7-point ordinal scales.
- **Reliability.** Report McDonald's omega for each factor, with Cronbach's alpha as a secondary statistic. Alpha assumes tau-equivalence, which is doubtful for these heterogeneous item sets, so omega is the more defensible headline number.
- **Ordinal CFA.** Fit a CFA on the polychoric matrix using WLSMV in `lavaan`, with the factor structure supported by theory and EFA. Report CFI, RMSEA, and SRMR. Reviewer 1 asks for this explicitly, and Reviewer 4 names it as a defensible option.
- **IRT as an alternative.** A graded response model (`mirt`) for the network items and a partial credit model for the activity items would address Reviewer 4's suggestion directly. This is more work and may be unnecessary if the CFA performs well.
- **Scale-score robustness.** Refit the key regressions using (a) the PCA scores, (b) the CFA scores, and (c) simple mean scores of items with clear loadings. Show whether the substantive conclusions hold across all three. This is likely the single most persuasive addition because it answers "does the result depend on the method?" directly.

---

## 2. Rotation: Report Both and Choose the Default Deliberately

**Reviewers:** 1, 4

Reviewer 4 argues that varimax imposes orthogonality without justification, and Reviewer 1 asks for oblique rotation explicitly.

- Report varimax and direct oblimin side by side, including the inter-factor correlations.
- Show that the regression results are stable across the two specifications.
- The project notes say an oblimin variant was run and then reverted, so the code and numbers should exist in git history.
- Whether oblique should become the default is a judgment call. If the factors correlate weakly under oblimin, varimax can be defended as a robustness-tested simplification.

---

## 3. Meaning of Factor 1 and the Variety-Versus-Size Question

**Reviewers:** 2, 4

This is the most conceptually loaded methodological concern. Reviewer 4 suggests Factor 1 may reflect network volume or expansiveness rather than range.

- Correlate Factor 1 with the total number of alters nominated and with the number of groups endorsed.
- Re-estimate the models controlling for total network size.
- Construct a proportion-based variety measure (groups endorsed divided by total alters, or a Simpson/Blau index over group shares).
- If the proportion-based measure yields similar results, the claim about range is stronger. If it does not, the paper should state that Factor 1 captures breadth of contact in absolute terms.
- This also responds to Reviewer 2's request to explain the counts-versus-proportions issue in the main text.

---

## 4. Ideology × Composition Interactions

**Reviewer:** 4

- Add centered respondent-ideology × network-composition interaction terms.
- Compare nested models with a likelihood ratio or Wald test.
- Report predicted values at homophilic and heterophilic network profiles, since interaction coefficients are hard to interpret alone.
- Check VIFs after adding interactions, and clarify how centering changes the interpretation of the main effects.

---

## 5. Separating Weak- and Strong-Tie Coefficients

**Reviewer:** 2

- Present the weak-only, strong-only, and joint models in a single table.
- Report the correlation between weak and strong variety, and the VIFs.
- The key question is whether the negative strong-tie coefficient in the solitary leisure model survives when strong variety enters alone. If it appears only in the joint model, that is a suppression pattern, and the paper should describe it as one.

---

## 6. Causal Language and Reverse Causality

**Reviewers:** 3, 4

This is mainly a framing and writing fix rather than a new analysis.

- Replace causal verbs ("drives," "effect," "funnels") in the Results with associational language. Reserve causal framing for a clearly labeled Discussion.
- Add a reverse-causality discussion grounded in social foci theory (Feld), which Reviewer 4 effectively invites.
- Run an E-value sensitivity analysis for the key coefficients to show how strong an unmeasured confounder would need to be to explain them away. This is inexpensive and speaks directly to the overselling concern.
- If retrospective items exist (for example, childhood arts exposure), use them as a partial temporal ordering check. This is not a causal identification strategy and should be described that way.

---

## 7. Sample Representativeness and Weights

**Reviewers:** 1, 4

- **Weights.** Rake the sample to ACS or CPS margins for age, sex, race/ethnicity, education, and region. Political affiliation requires an external benchmark, such as Pew or ANES marginals, and the paper must name the source. Report weighted and unweighted results side by side.
- **Quality checks.** Report attention-check failures, completion time, and straightlining. Show results with and without flagged cases.
- **Prolific claims.** Reframe the quality claims to say that Prolific has been compared with other nonprobability panels. Reviewer 4 is right that the current citations do not support the stronger claim of correspondence with probability samples.

---

## 8. Activity Battery and Item Selection

**Reviewers:** 1, 3

- **Instrumental items.** Run a sensitivity analysis that drops arguably routine or instrumental items (fast food, auto repair, possibly walking), recomputes the factors, and reports whether Factor 2 changes.
- **Yoga.** Drop yoga and check whether the liberal-tie association with solitary leisure disappears, as Reviewer 1 suspects.
- **Provenance.** If the items come from an existing inventory (for example, Peterson-style omnivore items or the Survey of Public Participation in the Arts), cite that lineage explicitly.
- **Scope.** Acknowledge the omitted domains Reviewer 3 lists (collecting, travel, shopping, hosting, group memberships) as a scope condition. Change "solitary leisure" to "the study's solitary leisure items" where the sentence implies a real-world category.

---

## 9. Coding Clarifications

**Reviewer:** 2

- **Gender.** Check the reference category and sign in Table 2. If men are the reference group, the text must describe the direction correctly.
- **Education.** State the actual coding. If Table A1 shows seven levels but the regression uses one linear term, say so explicitly in the table notes. A robustness model with categorical education would also address the concern.

---

## Suggested Sequencing

1. **Start with cheap items:** 6 (framing), 8 (sensitivity analyses), and 9 (coding).
2. **Then the high-leverage robustness work:** 1 (scale-score comparison) and 7 (weighted models), since these address the most reviewers at once.
3. **Then the more involved modeling:** CFA/IRT in 1, interactions in 4, and the rotation decision in 2 (which depends on what the CFA shows).

---

## Open Decisions

- Does the dataset include the weighting variables (age, sex, race, education, region), or must weights be constructed from scratch?
- Are there retrospective or temporal measures that could support a partial ordering check?
- Should oblique rotation become the new default, or should varimax remain the default with oblimin as a reported robustness check?
