# Agent Instructions

## Weather Questions

When a weather-related question is asked in this repo, use the question-driven
workflow in `analysis_questions/README.md` by default.

When available, use the weather/data-story skills in this order:

- `karthik-analysis-planner` to turn the question into explicit metrics,
  denominators, comparisons, caveats, and falsification checks.
- `dataviz-orchestrator` for loose exploratory questions where the work needs
  the full loop from analysis contract to rendered visual story.
- `weather-question-analysis` for the repo-local notebook, verdict, and
  publish/no-publish workflow.
- `dataviz-selector` before choosing any chart form.
- `karthik-data-visualization` before finalizing chart code or exported charts.
- `dataviz-critique` after rendering charts, before treating a chart as
  publication-ready.
- `karthik-writing-style` when drafting or polishing a public blog post.

- Use ERA5 data only for weather analysis. Do not use GFS-backed data for
  analysis posts because GFS rows can be overwritten and invalidate results.
- Create a notebook from `analysis_questions/question_template.Rmd`.
- Store the exact user prompt in the notebook `trigger_prompt` field.
- Run the analysis and write the notebook `Verdict`.
- Publish only if the result clears the insightfulness gate.
- If publishing, end the blog post with an `Original question` section containing
  the exact prompt that triggered the workflow.
- Update `docs/blog/index.html`, `docs/blog/feed.xml`, and
  `analysis_questions/analyses.yml`, then run
  `Rscript analysis_questions/update_site_analyses.R`.

For Hermes-triggered questions (`/weather_question` or `/weather-question`), stop
after the notebook, verdict, and gist. Ask for `/weather_publish <slug>` before
editing public blog/site files, committing, or pushing.

For Hermes approved publish commands (`/weather_publish` or `/weather-publish`),
publish only the named analysis after confirming the notebook verdict is
`Insightful: yes`, then commit and push.

Only skip this workflow when the user explicitly asks for a quick inline answer.
