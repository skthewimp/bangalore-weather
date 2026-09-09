# Writing Substack subtitles

Use this process for every weather post prepared for Substack. Read the installed
`karthik-writing-style` guide as well. The subtitle is an editorial decision made from
the finished article and its title; the publishing script stores and checks it.

## What the source blog suggests

Source: [Pertinent Observations](https://noenthuda.substack.com/), Karthik's main blog.
Reviewed 23 recent title/subtitle pairs returned by its public archive endpoint on
9 September 2026, covering posts dated 16 May through 5 September 2026. This is a
recent sample, not a claim about every post or a fixed house-style formula.

The recurring pattern is a compact explanation of the actual argument. The subtitle
can reveal the answer, name a concrete annoyance, explain a mechanism, or add a
complication. It often runs to one or two ordinary sentences. First-person language
appears when the post is about a real experience; it is not a required voice marker.

Examples, quoted from the author's own subtitles:

- [Totes not adorbs](https://noenthuda.substack.com/p/totes-not-adorbs):
  “I found India-made tote bags impossible to use, since their handles are way too
  short. No such issues with foreign totes”
  The title is playful; the subtitle supplies the specific problem and comparison.
- [How LLMs help me think better](https://noenthuda.substack.com/p/how-llms-help-me-think-better):
  “They remind me of old (documented) thoughts, and can sometimes develop surprisingly
  good connections between them”
  The subtitle explains how and retains the qualification “sometimes”.
- [Claude helped me buy coffee](https://noenthuda.substack.com/p/claude-helped-me-buy-coffee):
  “In all my experiments I kept buying suboptimal coffee, so got Claude to build a
  recommendation system to help me shop better”
  The subtitle gives the trigger and the action taken.
- [Abundance and attention](https://noenthuda.substack.com/p/abundance-and-attention):
  “With AI, it is easy to be abundant in your production, but the user's attention
  is still limited. So the creator's job is to curate, and direct the user on where
  to pay attention”
  The abstract title gets a concrete argument and implication.

## Process for each post

1. Read the final title, article and notebook verdict. Identify what the title already
   tells the reader. Write down one useful thing the subtitle can add: the comparison,
   mechanism, concrete trigger, qualification, or consequence supported by the article.
2. Draft three genuinely different candidates internally. Try different additions to
   the title; do not make three cosmetic paraphrases of one line. Select only the one
   that helps this particular title. A question can work when the answer remains open.
3. Write one or two conversational sentences. Around 20–35 words is a useful starting
   range, not a limit or a scoring rule. Use plain verbs and the author's actual terms.
   Use first person only for an experience or judgment already present in the article.
4. Check every claim against the article. Preserve the unit, denominator and comparison.
   A claim about rain per rainy day must not become a claim about monthly rainfall.
   Descriptive evidence must not turn into a causal explanation. Retain modest effects
   and uncertainty. Include a number only if it helps the reader understand the point.
5. Read the title and subtitle aloud together. Remove repeated claims, generic topic
   descriptions, inflated surprise, vague promises, forced jokes, and unnecessary
   jargon. Do not force every subtitle into a contrast, question, or punchline.
6. Save the selected line under the article slug in `publishing/post-metadata.json`.
   Review the pair again if the title or article changes. Export reads this exact line;
   it must not silently invent a subtitle or reuse a website lede without review.
7. Inspect the local preview, then verify `draft_subtitle` after the remote write.
   Changing metadata on a draft does not authorize publishing it or sending email.

## Persistence and updates

Each entry in `post-metadata.json` contains a `subtitle` string. Keep it on one line.
This file is the durable editorial source; the local Substack Markdown is an export.
Export includes the subtitle in frontmatter and refuses missing or blank entries.
Draft/publish/update also refuse an exported subtitle that differs from the stored line.

For an existing draft, `python3 publishing/substack.py subtitle <slug>` applies the
stored line, preserves the article body, and verifies the remote result. For a live
post, use the existing reviewed export/audit/update workflow instead. The subtitle
command deliberately handles drafts only.
