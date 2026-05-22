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
   ```

The homepage consumes `docs/latest.json` and `docs/assets/latest.png`.

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
