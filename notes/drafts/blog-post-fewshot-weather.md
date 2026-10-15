# Few-shotting A Weather Chart

A couple of weeks ago, the Bangalore weather chart did something that annoyed me more than it should have.

The subtitle said something like "trace rainfall only". Which was technically true if you looked only at the window aggregate. But it was also obviously wrong if you had lived through the week. There had been a long dry spell, then one proper burst of rain, and a noticeable drop in temperature. That was the story. The chart had looked at a real weather event and described it like an accountant.

My first instinct was the obvious bad one - patch the prompt for that case. Tell Claude to notice when one day contributes most of the rain. Tell it to look for a dry spell before the first rainy day. Tell it not to say "trace rainfall" so casually. This would have fixed that one report, and then broken in some other way the moment the pattern changed. Two consecutive wet days, say. Or a run of warm nights. Or a weird cool spell in March. Overfitting a prompt to yesterday's annoyance is still overfitting, even if you do it in complete sincerity.

So I ended up doing the more laborious thing.

I built a little workbench inside the repo to mine historical windows and turn them into review cards. Not just one fixed 14-day window either. Some stories live in a week. Some need a fortnight. Some need a full month. A dry October, for example, is not really a "what happened on one day?" story. A pre-monsoon burst absolutely is. So the examples had to vary in timescale as well as in weather pattern.

The fun part was making the review process visual. I realised fairly early that there was no point giving myself twelve dense text blobs and pretending I would respond well to them. So each candidate example became a little card - a compact chart showing actual highs and lows against normal, and daily rain against normal, plus a few facts underneath. The job then was simple: for each card, write the one headline I would have wanted. Not three bullets. Not perfect prose. Just the lead framing.

That distinction turned out to matter.

The useful signal in those responses was not the wording. In fact some of the wording was noisy because by the time I got to the later examples I was plainly tired and repetitive. The useful signal was what I chose to emphasise. Heavy rain on one day rather than the total. Temperature break rather than rain amount. Record streak rather than dry spell. Dryness plus deficit rather than "below normal rainfall". In one case I had labelled a month as a sustained wet build-up, and then immediately overruled myself on review because the real story was just two absurdly wet days in the middle.

In other words, the few-shot examples were not there to teach style. They were there to teach editorial judgment.

That meant the production setup had to change as well. The old daily script just assembled a recent weather block and sent it off. The new one has a reusable function that can build the same stats block for any historical window. So when Claude sees a reviewed example now, it is not seeing a toy summary. It is seeing the same kind of structured input it will get for today's weather - daily actual-vs-normal detail, streaks, wettest rolling stretches, antecedent dry or wet runs, record days, all of that - followed by one reviewed lead line.

I briefly tried stuffing all twelve reviewed examples into every call. This was not outrageously expensive - about a cent a run - but it felt like the wrong tradeoff. The bigger risk was not money. It was prompt rot. Too many examples and the model starts averaging them, or latching onto their surface quirks instead of actually reading today's case. So I left the full bank on disk, but the production script now samples four examples at random on each run. Same pool, smaller prompt, some variety.

I also did one full end-to-end run for today's chart, partly because there is no substitute for actually running the thing and partly because prompt work has a nasty habit of "working in principle" until the first real call. The Anthropic request went through, the chart rendered, and the subtitle was sensible enough to keep:

- Early May heat with highs a couple of degrees above normal
- Minimal rain despite several rainy days
- A recent heat record

That's not some profound triumph. It is just better than where I started, which was a model flattening a real weather week into a lazy aggregate.

The main thing I learnt from this little exercise is that prompt engineering is often a data curation problem in disguise. If you already have the right features, the hard part is not telling the model "be smarter". The hard part is showing it a few cases where a human made a judgment call about what the actual story was.

And yes, there is now an entire `fewshot_annotations/` folder in the repo for this. As is my wont, a small irritation has grown into a proper subsystem.
