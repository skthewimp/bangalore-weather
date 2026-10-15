# Bangalore's Two Monsoons, And The Winds That Bring Them

I've been pulling Bangalore weather data from ERA5 for a while now, mostly for temperature stories. A few weeks ago I added wind speed and direction to the pile, and then stared at it for several days without quite knowing what to do with it. The obvious thing - a wind rose for every month - turned out to be almost useless. Twelve tiny polar plots, eyes darting around, no clear story.

So I asked a simpler question. Through the year, where does Bangalore's wind come from? And then a slightly more interesting one - when it actually rains, where is the wind coming from?

[CHART 1: bangalore_wind_river.png]

The wind story is cleaner than I expected. I weighted each hour by wind speed - so a strong gust counts more than a faint breeze - which felt like the right thing to do, since a calm easterly and a 6 m/s westerly are not really the same kind of "wind from that direction". From December through March, easterlies dominate - that orange band at the bottom. By late May, the southwest monsoon has flipped the wind almost completely, and June through September is overwhelmingly westerly (the teal band swallows the chart). October is the messy transition month, and by November the easterlies are back. It's the kind of pattern you sort of know from school geography but rarely see laid out across an axis.

Now the rain.

[CHART 2: bangalore_rain_river.png]

Two peaks. One in late May / early June, just over 5mm/day on average. Another in October, also around 5mm/day. A dip in July-August - it still rains in those months, but individual rainy days dump less, on average, than the pre-monsoon thunderstorms or the October retreats. This is something most Bangaloreans intuit without articulating - that October feels like the wettest month. The data agrees.

What I found more interesting is which winds bring which rain.

[CHART 3: bangalore_rain_by_season_and_wind.png]

Pre-monsoon storms (April-May) are the messiest in origin. Westerlies bring the largest single share at 24%, but it's a wide spread - about a quarter of pre-monsoon rain comes from the eastern half of the compass. These are local convective storms, the kind that build up in a hot afternoon and unleash for forty minutes, and the synoptic wind direction matters less because the rain is being driven by ground heating rather than a continental airmass.

The southwest monsoon is the cleanest signal in the entire dataset. 69% of June-September rain falls when the wind is blowing from the West. Add the SW and NW sectors and you're at 90%. This is a single-engine season.

The October-November rain is the surprise. Most of us call it the "northeast monsoon" and assume the rain comes from the east. The easterlies (E + NE + N) do account for about 53% of the rain - so the name isn't wrong - but westerlies still bring 15%. The SW monsoon doesn't quit cleanly. It dribbles out.

One last thing the data threw up that I didn't expect:

[CHART 4: bangalore_rain_diurnal_by_season.png]

Pre-monsoon rain is almost entirely an evening phenomenon. From midnight through 2pm, it barely rains. Then a sharp climb to a 17:00 / 18:00 spike, tapering off by midnight. The SW monsoon has a similar afternoon peak but rains noticeably through the night too. Different physics - the pre-monsoon stuff is convective storms that need afternoon heating to fire, while the SW monsoon is sustained synoptic rain that doesn't care what time it is.

Anyway. That's where I've got to. There's more to pull out of this - I haven't looked at how individual years differ from this average yet, or whether the monsoon is shifting decade-on-decade. Both feel like the next thing to dig into.
