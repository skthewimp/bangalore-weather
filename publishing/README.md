# Substack publishing from Hermes

The runtime is pinned in `substack-runtime.json` to our hardened fork:
https://github.com/skthewimp/substack-cli

The publication is https://bangaloreweather.substack.com. The website remains the
article source. This integration does not publish anything during installation.

## Local setup

This Linux runner needs Python 3.10+, Git, Pandoc and bubblewrap. They are available on
this machine. It uses the Python standard library at runtime; Markdown tables must be
exported as approved chart images before publishing.

```bash
python3 publishing/substack.py install
python3 publishing/substack.py setup
```

Run setup yourself in an interactive terminal. While logged in as the publication's
author, open browser developer tools, select Application/Storage > Cookies > the
publication domain, and find `connect.sid` or `substack.sid`. Use the name that is
actually present. For `substack.sid`, run:

```bash
python3 publishing/substack.py setup --cookie-name substack.sid
```

Copy that cookie's value exactly as shown. Paste it at the hidden terminal prompt,
never in Hermes/chat. Setup stores an owner-only, gitignored credential file and checks
it with `doctor`. Repeat setup when the session expires.

```bash
python3 publishing/substack.py doctor
```

## Approved publish workflow

`/weather_question` remains notebook/verdict/gist only. Do not create a remote draft
or publish a Substack post during that workflow.

For `/weather_publish <slug>`, finish and verify the approved website article, then:

```bash
python3 publishing/substack.py export <slug>
# Inspect .substack-state/posts/<slug>/post.md and preview.json.
python3 publishing/substack.py publish <slug>
```

Export requires a literal `Insightful: yes` under the notebook's Verdict. If a notebook
uses inline R to render its verdict, inspect its executed result and record the final
literal verdict first. The HTML must contain one article, a title, the AI disclosure,
and the Original question section. The export copies only local docs/assets images.
It preserves article text, links, captions and the original question through Pandoc.
Substack controls the resulting typography and layout.

Publish is web-only. Add `--send-email` only when subscriber delivery was explicitly
requested. To stage a remote draft for an approved article without publishing it:

```bash
python3 publishing/substack.py draft <slug>
```

IDs are saved in the local Markdown. Existing live posts refuse a second publish.
Report the website and Substack URLs separately. If authentication or Substack fails,
finish the authorized website work and report that the Substack step remains pending;
do not claim both destinations succeeded.

## Updating an existing post

```bash
python3 publishing/substack.py export <slug>
python3 publishing/substack.py audit <slug>
```

Inspect the current live article, exported Markdown, removed text/image references,
and the revision fingerprint in the audit. An audit with differences exits nonzero;
that is a review requirement, not a reason to bypass it. Within the user's approved
edit scope, use the reviewed live fingerprint for an intentional replacement:

```bash
python3 publishing/substack.py update <slug> --accept-live-sha256 <reviewed-live-sha256>
```

Never mechanically forward the audit fingerprint. The CLI independently audits,
backs up the live record, and rechecks the revision before writing. Local Markdown
versions and remote JSON backups live in `.substack-state/backups/`. Retain this
private state: it includes draft IDs and recovery material. Git does not back it up.

An interrupted/failed write leaves `pending.json` and blocks another write. Inspect
the remote publication before retrying. If the post exists, reconnect it explicitly:

```bash
python3 publishing/substack.py reconcile <slug> --id <existing-substack-id>
```

Reconcile verifies the remote slug and clears the marker; it does not publish. If no
remote post exists, retain the diagnostic marker until you have verified that no write
occurred, then remove that selected post's marker manually before retrying.

## Execution and maintenance boundaries

The runner verifies the exact runtime commit and refuses local modifications. The CLI
runs under bubblewrap with a clean environment, an isolated process namespace, and only
the selected post directory, backups, standard runtime files and pinned package exposed.
Offline rendering has no network or credentials. Authenticated runs receive one cookie
through an anonymous pipe-backed file; the host checks the exact publication origin.
The CLI refuses redirects. No Hermes home directory or repository checkout is mounted.

Authenticated runs share network access: this is filesystem/process isolation, not an
OS-enforced domain firewall. Destination restriction also relies on the pinned reviewed
code and configuration. A compromised host user can alter the runner or its pin; this
does not protect against that. Cookie scope and Substack's undocumented API remain
limitations. Avoid concurrent browser edits while updating: the API write is not atomic
with the revision check. There is no automatic backup restoration command.

Do not auto-update the fork or run its upstream `agent install`. Review changes in the
fork, run its security tests, then deliberately change the full commit pin. Recreate the
ignored runtime checkout at that pin. No upstream CI workflow or installation script is
executed by this runner.
