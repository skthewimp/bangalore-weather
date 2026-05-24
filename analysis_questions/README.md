# Question Analysis Workflow

Use this workflow when a weather question comes in and we want to answer it from the
local Bangalore weather data.

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
5. Only if the result is genuinely insightful, add an entry to `analyses.yml` with
   `insightful: true` and `publish: true`.
6. Run:

```bash
Rscript analysis_questions/update_site_analyses.R
```

This rewrites the homepage blog cards in `docs/index.html` from `analyses.yml`.
If an entry has a `blog_url`, the card points readers to the post instead of directly
to the chart.

Published blog posts live in `docs/blog/`. Keep the visible disclosure
`This post is AI-written.` at the top of every post, and add each new post to
`docs/blog/feed.xml`.

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

The public site gets only the polished chart and short summary. The notebook keeps the
messy reasoning, checks, thresholds, and false starts.
