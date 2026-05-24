# Agent Instructions

## Weather Questions

When a weather-related question is asked in this repo, use the question-driven
workflow in `analysis_questions/README.md` by default.

- Create a notebook from `analysis_questions/question_template.Rmd`.
- Store the exact user prompt in the notebook `trigger_prompt` field.
- Run the analysis and write the notebook `Verdict`.
- Publish only if the result clears the insightfulness gate.
- If publishing, end the blog post with an `Original question` section containing
  the exact prompt that triggered the workflow.
- Update `docs/blog/index.html`, `docs/blog/feed.xml`, and
  `analysis_questions/analyses.yml`, then run
  `Rscript analysis_questions/update_site_analyses.R`.

Only skip this workflow when the user explicitly asks for a quick inline answer.
