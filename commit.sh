#!/bin/bash
git add .
git commit -m "feat(data): add wind direction and speed fetching

- Fetch 45-year historical wind direction data in chunks
- Append wind direction to bangaloreWind.RData
- Update daily script to pull both wind metrics"
