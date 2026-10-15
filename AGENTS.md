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

- Select the source using `analysis_questions/data_sources.md`: WeatherUnion for
  local station observations and neighbourhood comparisons; Oikolab/ERA5 for
  long-term history, records, and seasonal baselines. Check actual coverage first.
  Keep the two sources distinct when a question needs both. Within Oikolab use
  ERA5 only; do not use GFS-backed data for analysis posts because those rows can
  be overwritten and invalidate results.
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

## Substack publishing

For Hermes-approved weather publishing, follow `publishing/README.md` after finalizing
and verifying the website article. Use only `python3 publishing/substack.py` with the
pinned hardened fork. For every Substack post, follow `publishing/subtitle-style.md`
and save a reviewed subtitle in `publishing/post-metadata.json` before export. Inspect
the title/subtitle pair, exported Markdown and preview before creating the draft.
Substack is draft-only until the user explicitly changes the automation policy.
Even `/weather_publish` publishes only the website and creates or updates a Substack
draft. Return its editor URL for manual review and publication. Do not publish,
schedule, email subscribers, or update live Substack posts. Do not bypass the runner's
draft-only guard or change its mode as part of an ordinary article request.
Do not create remote Substack drafts in Hermes question/draft mode. Never ask for a
session cookie in chat; the user runs terminal setup. Complete authorized website
publishing if Substack is unavailable and report the Substack result separately.
