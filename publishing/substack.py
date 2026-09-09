"""Export weather articles and run the pinned Substack CLI in a filesystem sandbox."""
import argparse
import fcntl
import getpass
import hashlib
import html
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urljoin, urlsplit

ROOT = Path(__file__).resolve().parents[1]
STATE = ROOT / '.substack-state'
RUNTIME = ROOT / '.substack-runtime'
SPEC = json.loads((ROOT / 'publishing/substack-runtime.json').read_text())
SLUG = re.compile(r'[a-z0-9]+(?:-[a-z0-9]+)*\Z')


def fail(message):
    raise RuntimeError(message)


def execute(argv, **kwargs):
    return subprocess.run(argv, check=True, text=True, **kwargs)


def private_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    fd, temp = tempfile.mkstemp(dir=path.parent)
    try:
        with os.fdopen(fd, 'w') as out:
            json.dump(value, out, indent=2)
            out.write('\n')
        os.replace(temp, path)
    finally:
        if os.path.exists(temp):
            os.unlink(temp)


def inside(path, root):
    path, root = Path(path).resolve(), Path(root).resolve()
    if not path.is_relative_to(root):
        fail('Path escapes its approved directory')
    return path


def install():
    if not RUNTIME.exists():
        execute(['git', 'clone', '--no-checkout', SPEC['repository'], str(RUNTIME)])
        execute(['git', '-C', str(RUNTIME), 'checkout', '--detach', SPEC['commit']])
    verify_runtime()
    print('Pinned runtime ready:', SPEC['commit'])


def verify_runtime():
    if not RUNTIME.is_dir():
        fail('Run python3 publishing/substack.py install first')
    commit = execute(['git', '-C', str(RUNTIME), 'rev-parse', 'HEAD'], capture_output=True).stdout.strip()
    if commit != SPEC['commit']:
        fail('Runtime commit differs from the reviewed pin')
    dirty = execute(['git', '-C', str(RUNTIME), 'status', '--porcelain', '--untracked-files=all'],
                    capture_output=True).stdout
    if dirty:
        fail('Runtime has local modifications; refusing execution')


def credentials():
    credential = STATE / 'credentials.json'
    if not credential.is_file():
        fail('Substack is not authenticated. Run python3 publishing/substack.py setup in a terminal')
    if credential.is_symlink() or credential.stat().st_mode & 0o077:
        fail('Credential file must be a regular owner-only file')
    config = json.loads(credential.read_text())
    if config.get('publication_url') != SPEC['publication_url']:
        fail('Credential destination does not match the approved publication')
    # Reject stale cached identities and optional templates from another publication.
    cookie_name = config.get('session_cookie_name', 'connect.sid')
    if cookie_name not in ('connect.sid', 'substack.sid'):
        fail('Session cookie name must be connect.sid or substack.sid')
    config = {key: config[key] for key in ('publication_url', 'session_token')}
    config['session_cookie_name'] = cookie_name
    if not isinstance(config['session_token'], str) or not 1 <= len(config['session_token']) <= 4096:
        fail('Session cookie is missing or too large')
    return config


def check_draft_mode(command):
    mode = SPEC.get('mode', 'draft-only')
    if mode != 'draft-only':
        fail('Unrecognized publishing mode; changing automation policy requires a reviewed code change')
    allowed = {'doctor', 'list', 'get', 'templates', 'render', 'audit', 'push', 'set', '--help', '--version'}
    if not command or command[0] not in allowed:
        fail('Substack is draft-only. Publish manually in Substack after reviewing the draft')
    if command[0] == 'set' and (len(command) != 4 or command[2] != '--subtitle'):
        fail('Draft-only metadata edits are limited to the reviewed subtitle command')


def sandbox(work, command, authenticated=False, capture=False):
    check_draft_mode(command)
    verify_runtime()
    if not shutil.which('bwrap'):
        fail('bubblewrap is required; there is no unsandboxed fallback')
    work.mkdir(parents=True, exist_ok=True, mode=0o700)
    backup = STATE / 'backups' / work.name
    backup.mkdir(parents=True, exist_ok=True, mode=0o700)
    argv = ['bwrap', '--unshare-all', '--die-with-parent', '--new-session',
            '--cap-drop', 'ALL', '--clearenv', '--ro-bind', '/usr', '/usr',
            '--symlink', 'usr/bin', '/bin', '--symlink', 'usr/lib', '/lib',
            '--symlink', 'usr/lib64', '/lib64', '--proc', '/proc', '--dev', '/dev',
            '--tmpfs', '/tmp', '--dir', '/home/publisher', '--dir', '/app',
            '--ro-bind', str(RUNTIME / 'substack_cli'), '/app/substack_cli',
            '--bind', str(work), '/work', '--bind', str(backup), '/state',
            '--setenv', 'HOME', '/home/publisher', '--setenv', 'PATH', '/usr/bin:/bin',
            '--setenv', 'SUBSTACK_ASSET_ROOT', '/work',
            '--setenv', 'SUBSTACK_BACKUP_DIR', '/state', '--setenv', 'XDG_CACHE_HOME', '/work/.cache',
            '--chdir', '/work']
    if authenticated:
        config = credentials()
        # Pipe credentials into the sandbox; no environment or argv contains the cookie.
        credential_read, credential_write = os.pipe()
        os.write(credential_write, json.dumps(config).encode())
        os.close(credential_write)
        argv += ['--share-net', '--dir', '/run', '--ro-bind-data', str(credential_read), '/run/credentials.json']
        for path in ('/etc/ssl', '/etc/resolv.conf', '/etc/hosts', '/etc/nsswitch.conf'):
            if Path(path).exists():
                argv += ['--ro-bind', path, path]
        prefix = ['--config', '/run/credentials.json']
    else:
        credential_read = None
        prefix = []
    # Avoid persisting discovered IDs back to the read-only injected config.
    program = ('import sys; sys.path.insert(0,"/app"); '
               'from substack_cli import config; original=config.load; '
               'config.load=lambda path: config.Config(original(path).values,None); '
               'from substack_cli.cli import main; raise SystemExit(main())')
    argv += ['/usr/bin/python3', '-I', '-B', '-c', program, *prefix, *command]
    try:
        return execute(argv, capture_output=capture,
                       pass_fds=(credential_read,) if credential_read is not None else ())
    finally:
        if credential_read is not None:
            os.close(credential_read)


class Article(HTMLParser):
    """Keep the article body and heading; copy only local published chart assets."""
    def __init__(self, source, work):
        super().__init__(convert_charrefs=True)
        self.source, self.work = source, work
        self.depth = 0
        self.heading = False
        self.title = []
        self.parts = []
        self.article_count = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == 'h1' and not self.depth:
            self.heading = True
        if tag == 'article':
            self.article_count += 1
            self.depth += 1
            return
        if not self.depth:
            return
        if tag in ('script', 'style', 'iframe', 'object', 'embed'):
            fail('Unsupported active content in blog article')
        if tag == 'img':
            reference = urlsplit(attrs.get('src', ''))
            if reference.scheme or reference.netloc or reference.query:
                fail('Export requires local chart assets')
            asset = inside(self.source.parent / unquote(reference.path), ROOT / 'docs/assets')
            raw = asset.read_bytes()
            name = hashlib.sha256(raw).hexdigest()[:20] + asset.suffix.lower()
            target = self.work / 'assets' / name
            target.parent.mkdir(exist_ok=True)
            target.write_bytes(raw)
            attrs = {'src': 'assets/' + name, 'alt': attrs.get('alt', '')}
        elif tag == 'a':
            href = urljoin('https://bangaloreweather.karthiks.co/blog/' + self.source.name,
                           attrs.get('href', ''))
            if urlsplit(href).scheme not in ('http', 'https', 'mailto'):
                fail('Unsupported link scheme')
            attrs = {'href': href}
        else:
            attrs = {}
        self.parts.append('<' + tag + ''.join(' ' + k + '="' + html.escape(v, quote=True) + '"'
                                             for k, v in attrs.items()) + '>')

    def handle_endtag(self, tag):
        if tag == 'h1':
            self.heading = False
        if tag == 'article':
            self.depth -= 1
        elif self.depth:
            self.parts.append('</' + tag + '>')

    def handle_data(self, data):
        if self.heading:
            self.title.append(data)
        if self.depth:
            self.parts.append(html.escape(data))


def check_verdict(slug):
    notebook = inside(ROOT / 'analysis_questions' / (slug + '.Rmd'), ROOT / 'analysis_questions')
    text = notebook.read_text()
    section = re.search(r'^#{1,3} Verdict\s*$([\s\S]*)', text, re.MULTILINE)
    if not section or not re.search(r'^\s*(?:- )?Insightful:\s*yes\s*$', section[1], re.MULTILINE):
        fail('Notebook must contain a literal Insightful: yes in its Verdict before publishing')


def post_id(work):
    path = work / 'post.md'
    if not path.exists():
        return None
    match = re.search(r'^id:\s*(\d+)\s*$', path.read_text().split('---', 2)[1], re.MULTILINE)
    return match[1] if match else None


def subtitle_for(slug):
    path = ROOT / 'publishing/post-metadata.json'
    entries = json.loads(path.read_text()) if path.exists() else {}
    value = entries.get(slug, {}).get('subtitle')
    if not isinstance(value, str) or not value.strip():
        fail('Write and review a subtitle in publishing/post-metadata.json using publishing/subtitle-style.md')
    if len(value.splitlines()) != 1 or any(ord(c) < 32 for c in value):
        fail('Subtitle must be a single line without control characters')
    return value.strip()


def article_parts(work):
    text = (work / 'post.md').read_text()
    match = re.fullmatch(r'---\n(.*?)\n---\n([\s\S]*)', text, re.DOTALL)
    if not match:
        fail('Exported post needs valid frontmatter')
    return match[1].splitlines(), match[2]


def require_subtitle(slug, work):
    lines, _ = article_parts(work)
    values = [line[len('subtitle:'):].strip() for line in lines if line.startswith('subtitle:')]
    if values != [subtitle_for(slug)]:
        fail('Exported subtitle is missing or stale. Export again or use subtitle for an existing draft')


def sync_subtitle(slug, work):
    subtitle = subtitle_for(slug)
    identifier = post_id(work)
    if not identifier:
        fail('Create a draft first')
    before = remote(work, identifier)
    if before.get('slug') != slug or before.get('is_published'):
        fail('Subtitle command requires a matching unpublished draft')
    lines, body = article_parts(work)
    path = work / 'post.md'
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    private_json(STATE / 'backups' / slug / (digest + '-local.json'),
                 {'markdown': path.read_text()})
    lines = [line for line in lines if not line.startswith('subtitle:')]
    lines.append('subtitle: ' + subtitle)
    path.write_text('---\n' + '\n'.join(lines) + '\n---\n' + body)
    mutate(work, ['set', identifier, '--subtitle', subtitle])
    after = remote(work, identifier)
    if after.get('draft_subtitle') != subtitle:
        fail('Remote subtitle verification failed; inspect the draft')
    for key in ('draft_body', 'draft_title', 'slug', 'is_published'):
        if after.get(key) != before.get(key):
            fail('Other draft content changed during the subtitle update; inspect before continuing')
    print('Verified draft subtitle:', subtitle)


def export(slug, work):
    check_verdict(slug)
    subtitle = subtitle_for(slug)
    source = inside(ROOT / 'docs/blog' / (slug + '.html'), ROOT / 'docs/blog')
    identifier = post_id(work)
    parser = Article(source, work)
    parser.feed(source.read_text())
    if parser.article_count != 1 or parser.depth:
        fail('Expected exactly one complete article')
    title = ''.join(parser.title).strip()
    body = ''.join(parser.parts)
    if not title or 'This post is AI-written.' not in body or 'Original question' not in body:
        fail('Article needs a title, AI disclosure and Original question section')
    converted = execute(['pandoc', '-f', 'html', '-t', 'gfm-raw_html', '--wrap=none'],
                        input=body, capture_output=True).stdout
    # Upstream frontmatter is a small parser; JSON quoting is not interpreted as YAML.
    if '\n' in title or '\r' in title:
        fail('Title must be a single line')
    header = f'---\ntitle: {title}\nsubtitle: {subtitle}\nslug: {slug}\n'
    if identifier:
        header += f'id: {identifier}\n'
    target = work / 'post.md'
    if target.exists():
        digest = hashlib.sha256(target.read_bytes()).hexdigest()
        private_json(STATE / 'backups' / slug / (digest + '-local.json'),
                     {'markdown': target.read_text()})
    target.write_text(header + '---\n\n' + converted)
    print('Exported:', target)
    sandbox(work, ['render', 'post.md', '-o', 'preview.json'])


def remote(work, identifier):
    result = sandbox(work, ['get', str(identifier), '--json'], authenticated=True, capture=True)
    return json.loads(result.stdout)


def mutate(work, command):
    check_draft_mode(command)
    pending = work / 'pending.json'
    if pending.exists():
        fail('An earlier write is unresolved; inspect Substack and run reconcile before another write')
    credentials()  # Missing setup must not leave an ambiguous-write marker.
    private_json(pending, {'command': command, 'note': 'Inspect remote state before retrying'})
    sandbox(work, command, authenticated=True)
    pending.unlink()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['install', 'setup', 'doctor', 'export', 'render',
                                         'draft', 'publish', 'audit', 'update', 'reconcile', 'subtitle'])
    parser.add_argument('slug', nargs='?')
    parser.add_argument('--accept-live-sha256')
    parser.add_argument('--send-email', action='store_true')
    parser.add_argument('--cookie-name', choices=['connect.sid', 'substack.sid'],
                        default='connect.sid', help='cookie name shown in your browser (setup only)')
    parser.add_argument('--id', type=int, help='existing remote post ID for reconciliation')
    args = parser.parse_args()
    if args.action in ('publish', 'update') or args.send_email:
        fail('Substack is draft-only. Use draft, then review and publish manually in Substack')
    STATE.mkdir(exist_ok=True, mode=0o700)
    # Serialize all runner actions to protect local IDs and in-flight write markers.
    with (STATE / 'runner.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if args.action == 'install':
            return install()
        if args.action == 'setup':
            if not sys.stdin.isatty():
                fail('Run setup yourself in an interactive terminal; never send a cookie in chat')
            token = getpass.getpass(f'Substack {args.cookie_name} (hidden): ').strip()
            if not 1 <= len(token) <= 4096 or any(c in token for c in '\r\n;'):
                fail('Invalid session cookie')
            private_json(STATE / 'credentials.json',
                         {'publication_url': SPEC['publication_url'], 'session_token': token,
                          'session_cookie_name': args.cookie_name})
            sandbox(STATE / 'doctor', ['doctor'], authenticated=True)
            return
        if args.action == 'doctor':
            return sandbox(STATE / 'doctor', ['doctor'], authenticated=True)
        if not args.slug or not SLUG.fullmatch(args.slug):
            fail('A lowercase article slug is required')
        work = STATE / 'posts' / args.slug
        work.mkdir(parents=True, exist_ok=True, mode=0o700)
        if args.action == 'export':
            return export(args.slug, work)
        if args.action == 'render':
            return sandbox(work, ['render', 'post.md', '-o', 'preview.json'])
        if args.action == 'reconcile':
            if not args.id:
                fail('Inspect the remote publication, then provide its existing post --id')
            record = remote(work, args.id)
            if record.get('slug') != args.slug:
                fail('Remote post slug does not match')
            path = work / 'post.md'
            text = re.sub(r'^id:\s*\d+\s*\n', '', path.read_text(), flags=re.MULTILINE)
            path.write_text(text.replace('---\n', f'---\nid: {args.id}\n', 1))
            (work / 'pending.json').unlink(missing_ok=True)
            print('Linked existing post; inspect its content before publishing or updating')
            return
        check_verdict(args.slug)
        if args.action == 'subtitle':
            return sync_subtitle(args.slug, work)
        if args.action in ('draft', 'publish', 'update'):
            require_subtitle(args.slug, work)
        if args.action in ('audit', 'update'):
            identifier = post_id(work)
            if not identifier:
                fail('No linked post ID; create a draft or reconcile an existing post first')
            record = remote(work, identifier)
            if record.get('slug') != args.slug or not record.get('is_published'):
                fail('Linked post must be live and match the approved slug')
        if args.action == 'audit':
            return sandbox(work, ['audit', 'post.md', '--json'], authenticated=True)
        if args.action == 'update':
            command = ['update', 'post.md', '--yes']
            if args.accept_live_sha256:
                command += ['--accept-live-sha256', args.accept_live_sha256]
            return mutate(work, command)
        if (work / 'pending.json').exists():
            fail('An earlier write is unresolved; inspect remote state and reconcile first')
        identifier = post_id(work)
        if identifier:
            record = remote(work, identifier)
            if record.get('slug') != args.slug:
                fail('Linked post does not match the approved slug')
            if record.get('is_published'):
                fail('This post is already live. Automated live edits are disabled in draft-only mode')
        sandbox(work, ['render', 'post.md', '-o', 'preview.json'])
        mutate(work, ['push', 'post.md'])
        print(SPEC['publication_url'] + '/publish/post/' + post_id(work))
        if args.action == 'publish':
            identifier = post_id(work)
            if not identifier:
                fail('Draft ID missing; inspect remote state before retrying')
            command = ['publish', identifier, '--yes',
                       '--send-email' if args.send_email else '--no-email']
            mutate(work, command)
            record = remote(work, identifier)
            if not record.get('is_published') or record.get('slug') != args.slug:
                fail('Publication verification failed; inspect Substack before retrying')
            print(SPEC['publication_url'] + '/p/' + args.slug)


if __name__ == '__main__':
    try:
        main()
    except (RuntimeError, OSError, subprocess.CalledProcessError, ValueError, KeyError) as exc:
        print('Error:', exc, file=sys.stderr)
        sys.exit(1)
