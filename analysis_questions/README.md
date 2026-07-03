# Question Analysis Workflow

Use this workflow when a weather question comes in and we want to answer it from the
local Bangalore weather data.

For agent-assisted work in this repo, this workflow is the default path for weather
questions asked in chat. A quick inline answer is fine only when the user explicitly
asks for one. Otherwise, create the notebook, run the checks, and make the
publish/no-publish decision in the repo.

## Skill Use

When available, use these skills as part of the workflow:

1. `karthik-analysis-planner` for operational definitions, denominators,
   comparison frame, caveats, and falsification checks.
2. `dataviz-orchestrator` for loose exploratory questions where the work needs
   the full loop from analysis contract to rendered visual story.
3. `weather-question-analysis` for the repo-local notebook/verdict/publishing
   workflow.
4. `dataviz-selector` before choosing chart forms.
5. `karthik-data-visualization` before finalizing chart code or exported charts.
6. `dataviz-critique` after rendering charts, before treating a chart as
   publication-ready.
7. `karthik-writing-style` for public blog-post prose.

## Hermes Draft Mode

When a question arrives through Hermes using `/weather_question` or
`/weather-question`, run only the draft half of the workflow:

1. Create and run the notebook.
2. Write the `Verdict`.
3. Create analysis charts if needed.
4. Reply in Hermes with the gist, the insightfulness verdict, and the files created.
5. If the result is publishable, ask for explicit approval with
   `/weather_publish <slug>`.

In Hermes draft mode, do not create or edit `docs/blog/`, `docs/index.html`,
`docs/blog/feed.xml`, or `analysis_questions/analyses.yml`. Do not commit or push.

## Hermes Approved Publish Mode

When the user replies with `/weather_publish <slug>` or `/weather-publish <slug>`,
publish only the named completed analysis:

1. Confirm the notebook exists and its `Verdict` says `Insightful: yes`.
2. Create or finalize the blog post in `docs/blog/`.
3. Update `analysis_questions/analyses.yml`, `docs/blog/index.html`, and
   `docs/blog/feed.xml`.
4. Run `Rscript analysis_questions/update_site_analyses.R`.
5. Verify the generated files.
6. Commit the relevant changes and push.
7. Report the published URL and commit hash back to Hermes.

## Process

1. Copy `question_template.Rmd` to a new notebook in this directory.
   Use a short slug, for example `october-rain-later.Rmd`.
2. Fill in the YAML fields at the top:
   - `question`
   - `slug`
   - `date`
   - `analyst`
3. Run the notebook. It should load the local `.RData` files, build a tidy analysis
   table, make at least one chart if the answer is visual, and write public charts to
   `docs/assets/analysis/`.
4. Decide whether the result is worth keeping around. Be strict.
5. Only if the result is genuinely insightful, write a blog post in `docs/blog/`.
   Keep it around 400 words, include a few relevant charts/images, and put
   `This post is AI-written.` at the top of the article body.
   End the post with an `Original question` section containing the exact user prompt
   that triggered the workflow.
6. Add the post to the top of `docs/blog/index.html` and `docs/blog/feed.xml`.
   The blog is additive and reverse chronological.
7. Add an entry to the top of `analyses.yml` with `insightful: true`,
   `publish: true`, and `blog_url: "blog/<slug>.html"`.
8. Run:

```bash
Rscript analysis_questions/update_site_analyses.R
```

This rewrites the homepage blog cards in `docs/index.html` from the first three
published entries in `analyses.yml`.
Published, insightful entries must have a `blog_url`; the updater fails if the blog
post is missing, lacks the AI-written disclosure, or is absent from `docs/blog/feed.xml`.

## Insightfulness Gate

Publish only when the notebook clears all of these:

- The answer is not obvious from a single daily chart.
- The result changes, sharpens, or falsifies a plausible prior.
- The effect is large enough to explain in one short paragraph.
- The chart can stand alone without caveats doing all the work.
- The analysis uses enough historical data for the claim being made.

Do not publish when the result is weak, mostly null, too sensitive to arbitrary
thresholds, or only interesting because the question was interesting.

## Notebook Standard

Each notebook should end with a `Verdict` section containing:

- `Insightful: yes/no`
- the answer in 2-4 sentences
- the reason it should or should not be added to the site
- the chart filenames, if any
- if published, the blog post path and RSS entry status

The public site gets the polished chart, short homepage summary, and blog post. The
notebook keeps the messy reasoning, checks, thresholds, and false starts.
