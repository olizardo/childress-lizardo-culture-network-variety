#!/usr/bin/env python3
"""
Scripts/apply_oblimin_text_revisions.py

One-time (idempotent-ish, but not designed for repeat runs) surgical text
revision pass for the "Practice and Networks" Google Doc, applied when the
primary factor-analytic method for both the activities battery and the
weak-/strong-tie network item batteries was switched from varimax
(orthogonal) to oblimin (oblique) rotation.

Two edit modes are used:
  1. Whole-paragraph replacement (FULL_PARA_EDITS): for paragraphs that are
     self-contained methods/results statements, we match the paragraph's
     full concatenated text exactly and put the entire new text in the
     paragraph's first run, blanking the rest (same approach as the
     pre-existing Table A4 -> Figure A4 CAPTION_EDIT in sync_manuscript.py).
     This is safe here because these paragraphs carry uniform run
     formatting (plain body text, no embedded italics/bold).
  2. Run-preserving substring splice (SUBSTRING_EDITS): the Discussion
     section is authored as one very long paragraph. Rather than collapsing
     it into a single run (which would risk clobbering any inline
     formatting elsewhere in that paragraph), we locate the exact
     characters to change and splice the replacement into just the run(s)
     spanning that substring, leaving every other run in the paragraph
     byte-for-byte untouched.

Usage: apply_oblimin_text_revisions.py <in.docx> <out.docx>
"""

import sys
import zipfile
import xml.etree.ElementTree as ET

NS = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}

# Re-registering the document's namespace prefixes before serialization
# keeps ET.tostring() from renaming them to auto-generated ns0:/ns1:/...
# prefixes, which some strict OOXML readers (pandoc's docx reader among
# them) fail to parse even though the result is technically valid XML.
for _prefix, _uri in [
    ("w", "http://schemas.openxmlformats.org/wordprocessingml/2006/main"),
    ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
    ("r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
    ("wp", "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"),
    ("a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
    ("pic", "http://schemas.openxmlformats.org/drawingml/2006/picture"),
]:
    ET.register_namespace(_prefix, _uri)

# ---------------------------------------------------------------------------
# 1. Whole-paragraph replacements (exact full-text match required)
# ---------------------------------------------------------------------------
FULL_PARA_EDITS = [
    (
        "We construct our primary outcome variables for cultural consumption and "
        "participation from these items. To that end, we used principal component "
        "factor analysis with varimax rotation and retained a three-factor "
        "solution (eigenvalues > 1; KMO = .958), which together explained 58.7% "
        "of the variance. We generated cultural consumption and participation "
        "variables for each respondent from the predicted factor scores for each "
        "observation, standardized to have a mean of 0 and a standard deviation "
        "of 1. ",
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
    ),
    (
        "The first factor accounted for 41.9% of the variance.  As seen in Figure "
        "1 and in Table A2 in the Appendix, Factor 1 is characterized by high "
        "levels of public arts participation, including symphony and opera "
        "attendance (loading = 0.85), dance performances (0.84), art galleries "
        "(0.83), theater or musical performances (0.83), live music concerts "
        "(0.82), museum visits (0.80), festivals (0.77), historic site visits "
        "(0.71), and movie theater attendance (0.69). ",
        "The first factor accounted for 42.8% of the variance.  As seen in Figure "
        "1 and in Table A2 in the Appendix, Factor 1 is characterized by high "
        "levels of public arts participation, including symphony and opera "
        "attendance (loading = 0.88), dance performances (0.88), theater or "
        "musical performances (0.84), art galleries (0.83), live music concerts "
        "(0.82), museum visits (0.80), festivals (0.77), historic site visits "
        "(0.71), and movie theater attendance (0.69). ",
    ),
    (
        "The second factor accounted for 10.0% of the variance and is "
        "characterized by recreational leisure activities including \u201cgoing "
        "for walks\u201d (loading = 0.82), exercise or athletic activities (0.53), "
        "reading novels (0.46), going to the library (0.39), gardening (0.37), "
        "and hiking (0.36). We interpret this factor as everyday solitary "
        "leisure. The third factor accounts for only 6.8% of the variance, and "
        "we interpret it as essentially a residual category, with loadings on "
        "gardening (0.54), home auto repair (0.42), hiking or camping (0.24), "
        "and a strong negative loading for fast food consumption (\u20130.69). We "
        "include the full loadings on all three factors in Appendix Table A2.",
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
        "distinct outcomes. We include the full loadings on all three factors "
        "in Appendix Table A2.",
    ),
    (
        "For each group, respondents answered on the following five-point, "
        "ordered scale with the following options: None/0 people; 1 person; 2 "
        "to 5 people; 6 to 10 people; more than 10 people.  For both weak and "
        "strong ties, we similarly rely on an exploratory principal component "
        "factor analysis with varimax rotation to classify items into broader "
        "classes. In both cases, we retain three-factor solutions (eigenvalues "
        "> 1; KMOs = .926 for weak ties and .922 for strong ties), with each "
        "solution explaining approximately 61% of the total variance. As we did "
        "for the cultural participation and leisure activities items, we "
        "calculated personal network variety and composition variables for "
        "each respondent from the predicted factor scores. These scores are "
        "standardized to have a mean of zero and a standard deviation of 1. ",
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
        "cases, we retain three-factor solutions (eigenvalues > 1; KMOs = .926 "
        "for weak ties and .922 for strong ties), with each solution explaining "
        "approximately 61% of the total variance. As we did for the cultural "
        "participation and leisure activities items, we calculated personal "
        "network variety and composition variables for each respondent from "
        "the predicted factor scores. These scores are standardized to have a "
        "mean of zero and a standard deviation of 1. ",
    ),
    (
        "Factor 1 explains 22.9% of the variance in the weak-tie model and "
        "26.3% in the strong-tie model. Across both weak- and strong-tie "
        "networks, this factor loads positively across a wide range of social "
        "groups rather than clustering around a single category. In the weak "
        "tie model, Factor 1 is anchored by high loadings on knowing Native "
        "Hawaiian or Pacific Islander (.83), American Indian (.79), MENA (.77), "
        "and Asian (.65) individuals, with additional loadings on "
        "immigrant-origin contacts (knowing someone born outside the U.S.; .53) "
        "and class-coded ties (second-home owners; .58); the strong tie model "
        "shows a highly similar pattern, including strong loadings on Pacific "
        "Islander (.80), MENA (.79), American Indian (.77), Asian (.68), "
        "immigrant-origin (.65), and second-home owner (.61) ties. We interpret "
        "Factor 1 as capturing network variety rather than ties to any "
        "particular group or orientation, as we show in the supplementary "
        "analysis in the appendix.",
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
    ),
    (
        " In both weak and strong tie networks, Factor 2, which explains  "
        "21.1% of the variance for weak ties and 17.5% for strong ties, loads "
        "most strongly on left-leaning ideology and aligned groups. For weak "
        "ties, Factor 2 is anchored by knowing very liberal (.76) and LGBTQ "
        "(.76) individuals, those with low church attendance (.64), and with "
        "secondary loadings on urban (.56) and immigrant-adjacent (.50) ties. "
        "The strong tie model shows an even stronger concentration on very "
        "liberal (.82) and LGBTQ (.73) ties, alongside urban (.54) and "
        "Hispanic/Latine (.45) contacts. In turn, Factor 3, which explains 17% "
        "of the variance for weak ties and 17.2% for strong ties, loads most "
        "strongly on conservative and religious social environments. In the "
        "weak tie model, this factor is anchored by knowing very conservative "
        "individuals (.82) and frequent churchgoers (.72), with secondary "
        "loadings on rural ties (.71) and second-home owners (.43); the strong "
        "tie model shows a similar pattern, centered on very conservative "
        "(.80) and religious (.71) ties, alongside rural contacts (.68). We "
        "interpret Factor 3 as capturing right-leaning ideology and aligned "
        "groups, which is clearly distinct from the variety-oriented networks "
        "captured by Factor 1, and the liberal-aligend groups of Factor 2. We "
        "thus refer to Factor 1 as weak and strong-tie variety, and to "
        "Factors 2 and 3 as weak and strong-tie liberal and conservative "
        "composition. ",
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
        "conservatism. We interpret Factor 3 as capturing right-leaning "
        "ideology and aligned groups, which remains conceptually distinct from "
        "the variety-oriented networks captured by Factor 1 and the "
        "liberal-aligned groups of Factor 2, though under the oblique rotation "
        "we use here these three dimensions are allowed to (and do) correlate "
        "rather than being forced to independence: variety correlates "
        "modestly with both liberal (r = .35 weak, .26 strong) and "
        "conservative (r = .25 weak, .44 strong) composition, and liberal and "
        "conservative composition are themselves modestly positively "
        "correlated with one another (r = .39 weak, .28 strong) rather than "
        "negatively related, indicating that knowing more ideologically "
        "liberal and more ideologically conservative alters are not opposite "
        "poles of a single spectrum but partly co-occur, plausibly reflecting "
        "respondents\u2019 overall network breadth. We thus refer to Factor 1 as "
        "weak and strong-tie variety, and to Factors 2 and 3 as weak and "
        "strong-tie liberal and conservative composition. ",
    ),
    (
        "Across all models, and consistent with the network variety model, "
        "both weak- and strong-tie variety predict increasing frequencies of "
        "public arts participation. The coefficient estimate for the "
        "strong-tie network variable is more than twice that for the "
        "weak-tie variable\u2014a statistically significant difference (F(1, "
        "1213) = 18.22, p < .001)\u2014indicating that a diverse range of strong "
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
    ),
    (
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
    ),
    (
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
        "As before, adding ideological network composition does little to "
        "attenuate the effects of network variety. In Models 3 and 4, "
        "strong ties to persons in \u201cliberal\u201d positions are positively "
        "associated with everyday solitary leisure (b = .11, p = .02), and "
        "weak-tie liberal composition shows a positive association of "
        "similar magnitude that falls just short of conventional "
        "significance (b = .09, p = .06). Ties to conservative positions "
        "show, at most, a comparably weak positive association for strong "
        "ties (b = .09, p = .09) and none for weak ties (b = .02, p = .62), "
        "so we read this pattern cautiously as suggesting that liberal-"
        "leaning ties are the more consistent compositional correlate of "
        "solitary leisure, rather than as evidence of a sharp liberal/"
        "conservative asymmetry. Furthermore, as observed for public arts "
        "participation, the association between political ideological "
        "self-placement and solitary leisure in Model 2 (b = -.04, p = "
        ".01) is fully mediated by network composition: the Model 4 "
        "estimate falls to near zero and loses significance (b = -.01, p "
        "= .66), suggesting that the higher likelihood of liberals "
        "reporting engagement in everyday leisure is explained by their "
        "broader pattern of network composition rather than any single "
        "ideological tie category, also consistent with a homophily "
        "effect (Mark 1998; McPherson et al. 2001). ",
    ),
    (
        "Consistent with this expectation, none of the four composition "
        "coefficients approaches statistical significance in Models 3 or 4 "
        "of Table 3, and formal Wald tests confirm that the composition "
        "block adds no explanatory power over models with variety alone "
        "(\u03c7\u00b2(4) = 5.99, p = .200) or over models that already include the "
        "full set of socio-demographic controls (\u03c7\u00b2(4) = 5.61, p = .231). "
        "This null pattern, set alongside the associations we observe for "
        "public arts participation and solitary leisure, supports treating "
        "our composition measures as tapping domain-specific cultural and "
        "leisure engagement rather than an artifact correlated with any "
        "activity measure in the battery.",
        "Consistent with this expectation, none of the four composition "
        "coefficients approaches statistical significance in Models 3 or 4 "
        "of Table 3, and formal Wald tests confirm that the composition "
        "block adds no explanatory power over models with variety alone "
        "(\u03c7\u00b2(4) = 4.05, p = .399) or over models that already include the "
        "full set of socio-demographic controls (\u03c7\u00b2(4) = 7.07, p = .132). "
        "This null pattern, set alongside the associations we observe for "
        "public arts participation and solitary leisure, supports treating "
        "our composition measures as tapping domain-specific cultural and "
        "leisure engagement rather than an artifact correlated with any "
        "activity measure in the battery.",
    ),
    (
        "To further assess the interpretation of Factor 1 as capturing "
        "network variety, we examined its association with independently "
        "constructed variety count measures of the number of distinct "
        "social groups respondents reported knowing. Variety counts were "
        "calculated as the number of groups for which respondents reported "
        "knowing at least one person, and in Table A5, associations are "
        "reported as Pearson correlations. For both weak and strong ties, "
        "the network variety factor was positively correlated with these "
        "observed counts, with correlations exceeding those observed for "
        "the ideological network factors. Results are substantively "
        "similar when using a stricter threshold that requires respondents "
        "to report knowing two or more people in a given group, supporting "
        "the interpretation that Factor 1 reflects the breadth of "
        "cross-group social exposure rather than ideological composition.",
        "To further assess the interpretation of Factor 1 as capturing "
        "network variety, we examined its association with independently "
        "constructed variety count measures of the number of distinct "
        "social groups respondents reported knowing. Variety counts were "
        "calculated as the number of groups for which respondents reported "
        "knowing at least one person, and in Table A5, associations are "
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
        "and we interpret the weak-tie composition estimates in Tables 1\u20133 "
        "accordingly.",
    ),
]

# ---------------------------------------------------------------------------
# 2. Run-preserving substring splices (for the single long Discussion
#    paragraph, so unrelated runs/formatting elsewhere in it are untouched)
# ---------------------------------------------------------------------------
SUBSTRING_EDITS = [
    (
        "network variety in weak and strong ties operates orthogonally to "
        "each other, with",
        "network variety in weak and strong ties operates in opposing "
        "directions, with",
    ),
    (
        "with those with liberal leaning weak ties engaging in less "
        "conventional arts participation and those with conservative "
        "strong ties engaging in more, which those with more liberal weak "
        "and strong ties seem to be making up for with more everyday "
        "solitary leisure.",
        "with those with liberal-leaning weak ties engaging in less "
        "conventional arts participation, which those with more liberal "
        "strong ties, and to a lesser extent liberal weak ties, seem to be "
        "making up for with more everyday solitary leisure.",
    ),
    (
        "Our finding that weak ties to liberals are associated with lower "
        "levels of conventional arts participation, while strong ties to "
        "conservatives are associated with higher levels, likely reflects",
        "Our finding that weak ties to liberals are associated with lower "
        "levels of conventional arts participation likely reflects",
    ),
]


def para_text(p):
    return "".join(t.text or "" for t in p.findall(".//w:t", NS))


def apply_full_para_edits(body, edits, verbose=True):
    applied = 0
    for p in body.findall(".//w:p", NS):
        text = para_text(p)
        for old, new in edits:
            if text == old:
                # NOTE: a single <w:r> run can contain more than one <w:t>
                # (e.g. split around a <w:tab/>), so we must collect every
                # <w:t> descendant of the paragraph directly, in document
                # order -- NOT one-per-run via r.find(), which silently
                # grabs only the first <w:t> of any multi-<w:t> run and
                # leaves the rest (still holding the old text) untouched.
                t_elems = p.findall(".//w:t", NS)
                if not t_elems:
                    continue
                t_elems[0].text = new
                for extra in t_elems[1:]:
                    extra.text = ""
                applied += 1
                if verbose:
                    print(f"  [full-paragraph] applied edit ({len(new)} chars)")
                break
    return applied


def apply_substring_edit(body, old_substr, new_substr, verbose=True):
    """Splice new_substr in place of old_substr, touching only the run(s)
    whose text actually overlaps the match; every other run in the
    paragraph (and every other paragraph) is left completely alone."""
    for p in body.findall(".//w:p", NS):
        text = para_text(p)
        idx = text.find(old_substr)
        if idx == -1:
            continue
        end = idx + len(old_substr)

        t_elems = [t for t in p.findall(".//w:t", NS)]
        cursor = 0
        spans = []  # (t_elem, start, end) in the full-paragraph-text coordinate space
        for t in t_elems:
            s = t.text or ""
            spans.append((t, cursor, cursor + len(s)))
            cursor += len(s)

        touched = [(t, s, e) for (t, s, e) in spans if e > idx and s < end]
        if not touched:
            continue

        first_t, first_s, first_e = touched[0]
        last_t, last_s, last_e = touched[-1]
        prefix = (first_t.text or "")[: idx - first_s]
        suffix = (last_t.text or "")[end - last_s :]

        if first_t is last_t:
            first_t.text = prefix + new_substr + suffix
        else:
            first_t.text = prefix + new_substr
            last_t.text = suffix
            for t, _, _ in touched[1:-1]:
                t.text = ""

        if verbose:
            print(f"  [substring splice] '{old_substr[:40]}...' -> "
                  f"'{new_substr[:40]}...' ({len(touched)} run(s) touched)")
        return True
    if verbose:
        print(f"  [WARN] substring not found: '{old_substr[:60]}...'")
    return False


def revise(in_docx, out_docx, verbose=True):
    with zipfile.ZipFile(in_docx, "r") as zin:
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}

    doc_tree = ET.fromstring(all_files["word/document.xml"])
    body = doc_tree.find("w:body", NS)

    n_full = apply_full_para_edits(body, FULL_PARA_EDITS, verbose=verbose)
    if verbose:
        print(f"Applied {n_full} of {len(FULL_PARA_EDITS)} full-paragraph edits.")

    n_sub = 0
    for old, new in SUBSTRING_EDITS:
        if apply_substring_edit(body, old, new, verbose=verbose):
            n_sub += 1
    if verbose:
        print(f"Applied {n_sub} of {len(SUBSTRING_EDITS)} substring splices.")

    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)

    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

    return n_full, n_sub


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: apply_oblimin_text_revisions.py <in.docx> <out.docx>")
        sys.exit(1)
    n_full, n_sub = revise(sys.argv[1], sys.argv[2])
    total_expected = len(FULL_PARA_EDITS) + len(SUBSTRING_EDITS)
    total_applied = n_full + n_sub
    if total_applied != total_expected:
        print(f"[ERROR] Expected {total_expected} edits, applied {total_applied}. "
              "Aborting -- inspect before uploading.")
        sys.exit(1)
