# AI Agent Notes

## Oikolab API Guidelines
*   **Monthly Limit**: The free tier provides exactly **1,500 data units** per month.
*   **Request Limit**: The API enforces a strict maximum of **500 units per single API call**. If you need to pull >40 years of data, you must fetch it in chunks (e.g., 20-year intervals).
*   **Unit Calculation**: 1 data unit = 1 parameter (e.g., `wind_speed`) for 1 location for 1 month of time-series data.
*   **Current Usage**: The `bangalore_weather_update.R` script fetches delta updates (a few days at a time), which consumes only ~2 units per month per parameter.

## Terminal Execution Guidelines (Important!)
*   **Non-Interactive Execution**: When executing terminal commands automatically, the system crashes if it encounters an interactive pager or prompt (throwing `unexpected user interaction type: not permission`).
*   **Git**: Always bypass pagers. Use `git --no-pager status`, `git --no-pager diff`, and `git --no-pager log`.
*   **File Deletion**: The system profile aliases `rm` to `rm -i`. When deleting files via command line, always use `\rm` to force a non-interactive execution. 
*   **Commits**: Use `caveman-commit` skill conventions for commit formatting.

## Project Structure
*   **Historical Wind Scripts**: `fetch_historical_wind.R` and `fetch_historical_wind_direction.R` are one-off scripts used to pull 45-year chunks (1981-present). 
*   **Updates**: Both `wind_speed` and `wind_direction` have now been integrated into `bangalore_weather_update.R` and `bangaloreWind.RData`.

## Weather Question Workflow
*   When a weather-related question is asked in this project, do not answer only from an ad hoc terminal summary unless the user explicitly asks for a quick answer only.
*   Trigger the question-driven workflow in `analysis_questions/README.md`: create a notebook from `analysis_questions/question_template.Rmd`, run the analysis, write the `Verdict`, and decide whether it is insightful enough to publish.
*   If the result is published as a blog post, the post must end with a short `Original question` section containing the exact prompt that triggered the workflow.
*   After publishing, update `docs/blog/index.html`, `docs/blog/feed.xml`, `analysis_questions/analyses.yml`, and run `Rscript analysis_questions/update_site_analyses.R`.
