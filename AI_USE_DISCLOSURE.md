# AI Use Disclosure

## Tools and Scope of Use

Gemini 3.1 Pro was used as a collaborative co-pilot for code generation, debugging, and narrative drafting.

For the replication of Card and Krueger (1994), AI was used to translate the paper's methodological data-cleaning rules into R code for data manipulation and table formatting. For the independent extension, AI was used to refactor scripts from my undergraduate honors thesis and streamline a redundant R script into a functionalized workflow for generating population-weighted gap plots.

## Verification and Validation

All AI-generated code and analytical outputs were independently checked against the original source materials. In the replication phase, the generated outputs were cross-referenced with the published tables in Card and Krueger (1994). Target metrics—including regression coefficients, standard errors, and sample sizes—were checked to confirm alignment with the paper.

For the extension, the refactored Stata and R code was executed locally. The resulting data matrices and visualization plots were compared with the verified outputs from the original honors thesis.

## Errors Identified and Corrected

Several AI-generated errors and misalignments were identified and corrected during the workflow:

- **Sample inclusion criteria:** The initial replication code did not apply the paper's exact sample inclusion criteria, inflating the sample size and affecting the estimated coefficients. The code was corrected by enforcing stricter data filters based on the original manuscript's notes.
- **R syntax and dependencies:** Some generated code contained character-escaping errors and package-dependency issues that prevented execution. These outputs were rejected, and the statistical extraction functions and plotting commands were rewritten using more stable alternatives.
- **Narrative drafting:** Initial drafts of the narrative evaluation were sometimes overly verbose. They were iteratively edited to preserve a direct, professional academic tone.

## Responsibility

AI was used as an assistive tool, not as a substitute for independent verification or judgment. The final code, analytical results, and written discussion were reviewed and validated by the author.
