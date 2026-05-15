# Few-Shot Annotation Workbench

This folder is for building a better few-shot prompt for the AI-generated Bangalore weather annotations.

The daily production script currently asks Claude to summarize recent weather signals into three bullets. The goal here is to build a curated training set of real historical examples so the model learns better editorial judgment:

- when the real story is a rain burst after dryness
- when a longer wet stretch matters more than one heavy day
- when warm nights matter more than daytime highs
- when the right frame is 7 days, 14 days, or 30 days

## Files

- `build_weather_situations.R`: mines historical windows, selects representative situations, and renders review cards
- `fewshot_examples.csv`: curated reviewed examples that are now used for few-shot prompting in `bangalore_weather_update.R`
- `situation_cards.md`: generated markdown with visual cards and placeholders for user feedback
- `data/selected_situations.csv`: selected examples and their metrics
- `data/candidate_windows.csv`: the larger scored window set
- `charts/`: generated review charts for the selected examples

## Workflow

Run:

```bash
Rscript fewshot_annotations/build_weather_situations.R
```

Then review `fewshot_annotations/situation_cards.md`, fill in the preferred headline for each example, and note what the model should learn from that framing.

## Production use

`bangalore_weather_update.R` now reads `fewshot_annotations/fewshot_examples.csv` and turns those reviewed examples into actual few-shot prompt messages for Claude.

The examples intentionally contain a single reviewed lead bullet each. The live system prompt tells Claude to learn framing from those examples, then produce the usual 3 bullets for the current report.

To keep token cost down, the production script samples 4 examples at random from this file on each run rather than sending the full bank every day.
