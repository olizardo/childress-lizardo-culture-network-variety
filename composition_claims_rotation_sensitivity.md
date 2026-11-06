# Composition-Term Discussion Review: Table 1 and Table 2

**Purpose:** Review the Table 1 (public arts participation) and Table 2 (solitary leisure) discussion paragraphs against the m4 coefficients under varimax (canonical) and oblimin (rotation sensitivity), and list sentences that would need rewording. No manuscript text was changed.

**Sources:**
- Manuscript text: `cache/_manuscript_text_cache.txt` (approx. lines 600 to 700)
- Coefficients: `cache/rotation_sensitivity/canonical_models_{varimax,oblimin}.rds`
- Significance thresholds: HC1-robust OLS, α = .05

**Caveat:** The sentence-level judgments are the reviewer's, not the audit script's. `Scripts/audit_manuscript_stats.R` does not cover the rows marked "Breaks," the mediation sentences, the Table 2 conservative-tie sentence, or the gender sentence.

---

## Table 1: Public Arts Participation (m4)

| Sentence (paraphrased) | Varimax | Oblimin | Status |
|---|---|---|---|
| Weak liberal ties are associated with lower arts participation | −0.118*** | −0.127*** | Holds |
| Strong conservative ties are associated with higher arts participation | 0.098** (p = .005) | 0.032 (p = .40) | Breaks under oblimin |
| The two composition axes "pull arts participation in different directions" | Both significant | Only weak liberal significant | Breaks under oblimin; depends on the row above |
| Composition does little to attenuate variety effects (m2 to m4) | Weak .088* to .091*; strong .408 to .408 | Weak .085* to .138***; strong .395 to .382 | Holds |
| Conservative ideology effect in Model 2 is "fully mediated," near zero in Model 4 | .030* to −.009 (p = .61) | .028* to −.012 (p = .46) | Holds on significance; "fully mediated" implies a mediation test that was not run |

## Table 2: Solitary Leisure (m4 unless noted)

| Sentence (paraphrased) | Varimax | Oblimin | Status |
|---|---|---|---|
| Strong-tie variety is negative "across all specifications" | Significant in all four models | Model 1: −0.081 (p = .10) | Breaks under oblimin in Model 1 |
| Weak and strong liberal ties are positive in Models 3 and 4 | Weak .133** (m3), .120** (m4); strong .135** (m3), .092* (m4) | Weak .088 (p = .07) (m3), .091 (p = .06) (m4); strong .150*** (m3), .106* (m4) | Weak liberal breaks under oblimin |
| Ties to conservatives have "no discernible impact" in Models 3 and 4 | Weak conservative m3 = .091* (p = .035) | Not significant | Already contradicted under varimax (Model 3) |
| Self-identification as a woman increases solitary leisure (Model 2) | Man coefficient −0.109* (m2); −0.065 (p = .24, m4) | −0.114* (m2); −0.067 (m4) | Depends on Model 2 only; reference category is Woman |
| Political self-placement effect is "fully mediated" by composition | −0.040* (m2) to −0.007 (p = .72, m4) | −0.041* (m2) to −0.009 (p = .66, m4) | Holds on significance; same mediation-wording issue |

## Ranges Already Flagged by the Audit

| Sentence | Varimax | Oblimin | Status |
|---|---|---|---|
| Weak-tie variety, 0.14 to 0.23 SD across four models | 0.14 to 0.23 | 0.13 to 0.21 | Flagged by the audit |
| Strong-tie variety magnitude, 0.13 to 0.18 SD | 0.13 to 0.18 | 0.08 to 0.17 | Flagged by the audit |

---

## Summary of Rewording Needs

1. **Table 1, strong conservative ties.** Under oblimin the coefficient is not significant, so the "higher levels" claim does not hold. Under varimax it holds.
2. **Table 1, "pull in different directions."** Holds only under varimax. Under oblimin only the weak liberal axis is significant.
3. **Table 1, "fully mediated."** Holds on significance under both rotations, but the wording implies a formal mediation test that was not run. Consider reporting an indirect-effect test or softening the phrasing.
4. **Table 2, strong-tie negativity "across all specifications."** Under oblimin Model 1 is not significant.
5. **Table 2, weak liberal ties in Models 3 and 4.** Not significant under oblimin.
6. **Table 2, conservative ties with "no discernible impact."** Weak conservative ties are significant in Model 3 under varimax (p = .035). This is wrong under the canonical specification, not only under oblimin.
7. **Table 2, gender.** The coefficient is for men relative to women. The claim about women rests on Model 2 alone; Model 4 is not significant under either rotation. Reviewer 2 raised a related concern. This sentence sits outside the composition block but in the same paragraph.
8. **Table 2, political self-placement "fully mediated."** Same mediation-wording issue as item 3.
9. **Table 2 ranges.** Already flagged by the audit (see the table above).

The Table 2 mechanism sentence ("funnel people into those more public pursuits") is untested and reads as a hypothesis rather than a result. It does not depend on a coefficient, so it is not listed as a rewording item, but it may need hedging.
