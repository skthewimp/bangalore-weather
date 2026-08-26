# Bangalore Weather Deployment Runbook

This project now has two related things:

1. The static website in `docs/`.
2. The daily data/chart pipeline that refreshes `docs/latest.json`, `docs/assets/latest.png`,
   and `docs/archive/...`.

The site is intentionally static. There is no backend, no live LLM endpoint, and no
interactive charting layer. The homepage reads `latest.json` in the browser and otherwise
serves plain HTML/CSS/PNG assets.

## Current hosting decision

`weather.karthiks.co` is intended to point to this server, not GitHub Pages.

The canonical Caddy config for this repo is:

```text
deploy/Caddyfile
```

It serves:

```text
weather.karthiks.co -> /home/karthik/apps/bangalore-weather/docs
```

and preserves the existing placeholder/reverse-proxy hosts:

```text
claw.karthiks.co    -> static placeholder
hermes.karthiks.co  -> static placeholder
demo.karthiks.co    -> localhost:5000
```

## Important current state

As of 22 May 2026:

- Caddy is running as a system service.
- The runtime Caddy config was successfully reloaded once from `deploy/Caddyfile`.
- `/etc/caddy/Caddyfile` was still the old default config when last checked from Codex.
- Therefore the site may work until Caddy restarts, but the config must be persisted for
  this to survive reboot/reload.

Persist it with:

```bash
cd /home/karthik/apps/bangalore-weather
sudo cp deploy/Caddyfile /etc/caddy/Caddyfile
sudo caddy validate --config /etc/caddy/Caddyfile
sudo systemctl reload caddy
```

Because Caddy runs as the `caddy` user and the repo lives under `/home/karthik`, make sure
the home directory is traversable:

```bash
chmod o+x /home/karthik
```

That does not make `/home/karthik` listable or readable; it only lets Caddy traverse the
path to the world-readable `docs/` files.

## DNS

DreamHost should point:

```text
weather.karthiks.co  A  64.227.150.189
```

If using AAAA, it must point to a real IPv6 address on this machine. Otherwise omit AAAA.

Useful checks:

```bash
getent hosts weather.karthiks.co
curl -I http://weather.karthiks.co
curl -I https://weather.karthiks.co
```

Expected once DNS and Caddy are both correct:

- HTTP returns a Caddy `308` redirect to HTTPS.
- HTTPS returns `200`.
- The page title is `Bangalore Weather`.

If this server cannot resolve the hostname yet but external browsers can, use a forced
local check:

```bash
curl -k -I --resolve weather.karthiks.co:443:127.0.0.1 https://weather.karthiks.co
```

That should return `200` if Caddy is configured correctly.

## Daily publishing

Run:

```bash
cd /home/karthik/apps/bangalore-weather
./run_daily_weather.sh
```

This script:

1. Loads `.Renviron` from the project root if present.
2. Fetches recent Oikolab data.
3. Repairs older GFS rows to ERA5 when allowance permits.
4. Generates the current chart in `charts/`.
5. Copies the latest chart into:

   ```text
   docs/assets/latest.png
   docs/archive/bangalore_weather_YYYYMMDD.png
   ```

6. Writes:

   ```text
   docs/latest.json
   docs/feed.xml
   docs/sitemap.xml
   docs/data/bangalore_daily_weather.csv
   ```

The homepage consumes `docs/latest.json` and `docs/assets/latest.png`.

Daily commentary is intentionally LLM-generated. The policy is:

- Use Haiku only: `claude-haiku-4-5-20251001`.
- Do not fall back to local/template prose if Claude fails. Weak fallback copy is worse than
  a failed run because it makes the site look fresher than the analysis really is.
- Retry Claude a few times for transient API or wording-validation failures.
- Validate bullets independently and keep valid unique bullets across retries, so one bad
  bullet does not discard two usable ones.
- Reject ambiguous recent-window wording. Rainy-day counts must say `last 14 days` or an
  exact date range, not bare phrasing such as `12 of 14 days`.
- If Haiku cannot produce acceptable commentary, let the job fail loudly and inspect it.

The daily update also regenerates the sitemap so `lastmod` does not drift from the actual
published data.

### Automatic daily run

The intended production scheduler is a user-level systemd timer:

```text
deploy/bangalore-weather-daily.service
deploy/bangalore-weather-daily.timer
```

It runs `./run_daily_weather.sh` every day at 02:30 UTC, which is 08:00 IST.
`Persistent=true` means that if the machine is asleep or rebooting at 08:00 IST, systemd
will run the missed update when the user manager comes back.

Install or refresh the timer with:

```bash
mkdir -p ~/.config/systemd/user
cp deploy/bangalore-weather-daily.service ~/.config/systemd/user/
cp deploy/bangalore-weather-daily.timer ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now bangalore-weather-daily.timer
systemctl --user list-timers --all | grep bangalore-weather
```

For this to survive logout and reboot, linger should be enabled once:

```bash
sudo loginctl enable-linger karthik
```

Useful checks:

```bash
systemctl --user status bangalore-weather-daily.timer --no-pager
journalctl --user -u bangalore-weather-daily.service --since "2 days ago" --no-pager
```

Manual end-to-end test:

```bash
systemctl --user start bangalore-weather-daily.service
systemctl --user status bangalore-weather-daily.service --no-pager
journalctl --user -u bangalore-weather-daily.service --since "10 minutes ago" --no-pager
```

Expected result:

- `status=0/SUCCESS` in systemd.
- A log line like `Claude commentary accepted on attempt 1`.
- Fresh mtimes on `docs/latest.json`, `docs/assets/latest.png`, `docs/feed.xml`, and
  `docs/sitemap.xml`.

Live local Caddy checks:

```bash
curl -k -I --connect-to weather.karthiks.co:443:127.0.0.1:443 https://weather.karthiks.co/
curl -k -I --connect-to weather.karthiks.co:443:127.0.0.1:443 https://weather.karthiks.co/latest.json
curl -k -I --connect-to weather.karthiks.co:443:127.0.0.1:443 https://weather.karthiks.co/assets/latest.png
curl -k -I --connect-to weather.karthiks.co:443:127.0.0.1:443 https://weather.karthiks.co/feed.xml
curl -k -I --connect-to weather.karthiks.co:443:127.0.0.1:443 https://weather.karthiks.co/sitemap.xml
```

The year archive grid is driven by:

```text
docs/archive_years.json
```

The daily updater checks the current data year. When the data rolls into a new year, it treats
the previous year as complete, generates the completed-year chart if needed, copies it to
`docs/assets/analysis/`, and prepends that year to `archive_years.json`.

### Claude failures

If the daily run fails around the Claude step, first decide whether this is our key/quota or
Anthropic service health.

Check recent service logs:

```bash
journalctl --user -u bangalore-weather-daily.service --since "2 days ago" --no-pager
```

Check Anthropic public status:

```bash
curl -L -sS https://status.anthropic.com/api/v2/status.json
curl -L -sS https://status.anthropic.com/api/v2/incidents/unresolved.json
curl -L -sS https://status.anthropic.com/api/v2/incidents.json
```

As of 22 May 2026, Haiku 4.5 had recent upstream incidents:

- 20 May 2026: `Elevated errors on Claude Haiku 4.5`.
- 22 May 2026: `Elevated error rate on multiple models`, with updates mentioning remaining
  Haiku 4.5 errors.

So a sudden burst of `429`, `529`, or transient API failures is not automatically a project
bug. It can be Anthropic-side instability.

To check the current project key without exposing it, run a tiny Haiku probe and inspect
headers:

```bash
Rscript -e 'library(httr2); if (file.exists(".Renviron")) readRenviron(".Renviron"); req <- request("https://api.anthropic.com/v1/messages") |> req_headers(`x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"), `anthropic-version` = "2023-06-01", `content-type` = "application/json") |> req_body_json(list(model = "claude-haiku-4-5-20251001", max_tokens = 1, messages = list(list(role = "user", content = "ping")))) |> req_error(is_error = function(resp) FALSE); resp <- req_perform(req); cat("status=", resp_status(resp), "\n", sep = ""); h <- resp_headers(resp); wanted <- names(h)[grepl("ratelimit|retry-after|request-id", names(h), ignore.case = TRUE)]; for (n in wanted) cat(n, ": ", h[[n]], "\n", sep = "")'
```

Interpretation:

- `200` with plenty of `anthropic-ratelimit-*-remaining` means the key is fine.
- `429` with low remaining counts or a `retry-after` means wait; do not hammer manual reruns.
- `529` usually means provider overload. Wait and rerun later.

Do not switch the daily job to Sonnet just for reliability. It is too expensive for this
small daily text task. Keep Haiku-only, retry patiently, fail loudly, and try again after the
incident clears.

## Secrets

The project-local `.Renviron` contains API keys and must never be committed.

`.gitignore` includes:

```text
.Renviron
```

Keep `.Renviron.example` committed as the template.

## Request box

The homepage has a capture-only request box. It does not answer questions and does not call
an LLM.

When the Google Form exists, edit this line near the bottom of `docs/index.html`:

```js
const requestFormUrl = "";
```

Set it to the public Google Form URL. The button will become active automatically.

## If the site shows the Caddy welcome page

That means the request is reaching this server, but Caddy is serving its default `:80`
site from `/usr/share/caddy` instead of the weather site.

Fix:

```bash
cd /home/karthik/apps/bangalore-weather
sudo cp deploy/Caddyfile /etc/caddy/Caddyfile
sudo caddy validate --config /etc/caddy/Caddyfile
sudo systemctl reload caddy
```

Then check:

```bash
curl -I https://weather.karthiks.co
```

## If HTTPS fails

Check Caddy logs:

```bash
sudo journalctl -u caddy --since "30 minutes ago" --no-pager
```

Common causes:

- DNS has not propagated.
- `weather.karthiks.co` points to the wrong IP.
- An AAAA record points somewhere else.
- Port 80 or 443 is blocked.
- Caddy cannot traverse `/home/karthik`.

## If the homepage loads but the chart is missing

Check:

```bash
ls -l docs/assets/latest.png docs/latest.json
curl -I https://weather.karthiks.co/assets/latest.png
curl https://weather.karthiks.co/latest.json
```

If missing, rerun:

```bash
./run_daily_weather.sh
```

## GitHub Pages note

`docs/CNAME` still contains:

```text
weather.karthiks.co
```

That file only matters if GitHub Pages is used again. For the current server-hosted setup,
Caddy/DreamHost DNS are the source of truth.
