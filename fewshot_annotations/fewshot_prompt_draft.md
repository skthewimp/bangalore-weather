# Few-Shot Prompt Draft For `bangalore_weather_update.R`

This draft is designed to teach editorial judgment from the reviewed examples while still producing the live output format of exactly 3 bullets.

## Suggested system-prompt addition

Add this after the existing instructions about the input fields and before the final output-format constraints.

```text
Use the examples below as editorial guidance, not as rigid templates. They show what kinds of signals deserve emphasis.

Editorial priorities learned from past reviewed examples:
- If most rainfall in the window came from one short burst, lead with that burst rather than calling the whole window wet.
- If rain caused a clear cooling break, mention the temperature change, not just the rainfall amount.
- If temperature departures are large and sustained, they usually matter more than modest rainfall anomalies.
- Warm nights and cool nights are distinct stories; do not blur them into a generic temperature summary.
- When a dry spell is the main story, pair the length of the spell with the rainfall shortfall versus normal.
- When record-breaking days occur on most days in the window, treat that as the lead signal.
- If rain fell on nearly every day, say that directly; persistence matters.
- If a 30-day period is dominated by a 1-2 day deluge, do not describe it as a steadily accumulating wet month.

The examples provide one reviewed "lead framing" line for each case. In the live task, use the same judgment to choose the 3 most interesting bullets.
```

## Few-shot example block

These are cleaned versions of the reviewed examples. They preserve the user's framing choices while smoothing repetition and typos.

```text
Example 1
Input signal summary:
- 14-day window in late April
- 81.1mm rain vs 24.8mm normal
- Wettest 3-day stretch 51.7mm
- Temperatures turned cooler than normal
Reviewed lead framing:
Over 20mm rain on Apr 24 and Apr 25 led to a sharp temperature drop.
What this teaches:
Do not force a generic dry-spell frame when the dominant visible story is the heavy rain burst and subsequent cooling.

Example 2
Input signal summary:
- 30-day October window
- 361.0mm rain vs 132.6mm normal
- Rain on every day of the window
- A late-month burst intensified the cooling
Reviewed lead framing:
Consistently heavy rain through October, with no dry days and a sharp cool-down after the late-month burst.
What this teaches:
When persistence is real, say it directly. "No dry days" is stronger than a vague wet-period summary.

Example 3
Input signal summary:
- 7-day May window
- 110.7mm rain vs 24.5mm normal
- 3 rainy days
- Nearly all rain concentrated at the end of the week
Reviewed lead framing:
Extreme rain on May 20 followed a hot, dry start to the week.
What this teaches:
If one day dominates the rainfall, lead with that day rather than describing the whole week as broadly wet.

Example 4
Input signal summary:
- 14-day October window
- 1.1mm rain vs 57.7mm normal
- Almost no rain during a normally wet time
Reviewed lead framing:
Dry spell from Oct 13 to Oct 25, with just 1mm of rain versus 58mm expected.
What this teaches:
For dry windows, combine duration and rainfall deficit in the same line.

Example 5
Input signal summary:
- 14-day late-April / early-May window
- Avg highs +4.4°C above normal
- Avg lows +3.6°C above normal
- 11 record days
Reviewed lead framing:
Sustained heat wave for two weeks, with both highs and lows more than 3°C above normal.
What this teaches:
When heat is overwhelming, keep the frame on heat even if there is also a dryness story.

Example 6
Input signal summary:
- 14-day December window
- Avg lows +3.7°C above normal
- Rain occurred, but nights stayed warm
Reviewed lead framing:
A sustained warm spell this winter kept minimum temperatures about 4°C above normal, with rain failing to break it.
What this teaches:
If rain normally cools Bangalore but did not do so, that contrast is worth surfacing.

Example 7
Input signal summary:
- 14-day late-March window
- 129.5mm rain vs 8.6mm normal
- Avg highs -4.5°C below normal
- Rain on every day
Reviewed lead framing:
Unseasonal heavy rain made for a notably cool March, with highs about 5°C below normal.
What this teaches:
Even when rain is extraordinary, a resulting temperature anomaly can still be the main story.

Example 8
Input signal summary:
- 14-day late-October / early-November window
- Avg lows -3.6°C below normal
- Very little rain
Reviewed lead framing:
Cold nights and almost no rain after Oct 27 left lows about 3.5°C below expected.
What this teaches:
When both cold and dryness are present, keep the more interesting one first and let the other support it.

Example 9
Input signal summary:
- 14-day late-November window
- 14 record days
- Avg highs +3.8°C above normal
- Almost no rain
Reviewed lead framing:
Record high temperatures persisted through the full fortnight, making for a hot and dry November patch.
What this teaches:
If record days cover almost the whole window, records become the lead signal.

Example 10
Input signal summary:
- 30-day November window
- 371.5mm rain vs 56.6mm normal
- 25 rainy days
- 169.8mm fell in the wettest 3-day stretch
Reviewed lead framing:
An exceptionally wet November was driven mainly by a deluge on Nov 15 and 16.
What this teaches:
Do not force a cumulative frame when a short deluge explains most of the monthly anomaly.

Example 11
Input signal summary:
- 30-day October window
- 27.2mm rain vs 138.0mm normal
- Strong monthly rainfall deficit
Reviewed lead framing:
Only 27mm of rain fell in the last month, against the usual 136mm, making for a notably dry October.
What this teaches:
When the signal is genuinely cumulative, monthly totals can be the right lead.

Example 12
Input signal summary:
- 14-day early-March window
- Rain arrived after a long hot, dry stretch
- Temperatures dropped after Mar 10 and especially after Mar 12
Reviewed lead framing:
Rain after Mar 10 brought a sharp temperature drop following a record-hot start.
What this teaches:
In whiplash windows, tie the event date to the temperature break and avoid overcomplicating the framing.
```

## Recommended use

- Keep all 12 examples for now; they cover useful edge cases.
- Treat examples 1 and 10 as especially important, because they correct a common model failure: over-reading aggregate rainfall and under-reading concentration.
- The final prompt should prefer these examples for judgment, but not imitate their sentence structure too literally.
