"""Offline safety checks for the weather integration and its real sandbox."""
import importlib.util
import json
import subprocess
from pathlib import Path

import pytest

SPEC = importlib.util.spec_from_file_location('weather_substack', Path(__file__).with_name('substack.py'))
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


def test_actual_sandbox_hides_home_environment_and_host_processes(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'STATE', tmp_path / 'state')
    original = runner.execute
    monkeypatch.setenv('HERMES_SECURITY_CHECK', 'must-not-inherit')

    def execute(argv, **kwargs):
        if argv[0] == 'bwrap':
            argv = list(argv)
            argv[argv.index('-c') + 1] = (
                'import os; from pathlib import Path; '
                'assert not Path("/home/karthik").exists(); '
                'assert not Path("/proc/1/root/home/karthik").exists(); '
                'assert "HERMES_SECURITY_CHECK" not in os.environ; '
                'assert not Path("/run/credentials.json").exists(); '
                'assert Path("/app/substack_cli/cli.py").is_file(); '
                'print("isolated")')
        return original(argv, **kwargs)
    monkeypatch.setattr(runner, 'execute', execute)
    assert 'isolated' in runner.sandbox(tmp_path / 'post', ['--help'], capture=True).stdout


def test_wrong_publication_refused_before_authenticated_execution(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'STATE', tmp_path)
    runner.private_json(tmp_path / 'credentials.json', {
        'publication_url': 'https://attacker.example', 'session_token': 'fake'})
    with pytest.raises(RuntimeError, match='does not match'):
        runner.sandbox(tmp_path / 'post', ['doctor'], authenticated=True)


def test_pending_write_blocks_retry(tmp_path, monkeypatch):
    (tmp_path / 'pending.json').write_text('{}')
    monkeypatch.setattr(runner, 'sandbox', lambda *a, **k: pytest.fail('must not execute'))
    with pytest.raises(RuntimeError, match='unresolved'):
        runner.mutate(tmp_path, ['push', 'post.md'])


def test_failed_write_retains_marker(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'credentials', dict)
    def failure(*a, **k):
        raise subprocess.CalledProcessError(1, 'simulated write')
    monkeypatch.setattr(runner, 'sandbox', failure)
    with pytest.raises(subprocess.CalledProcessError):
        runner.mutate(tmp_path, ['publish', '123', '--yes'])
    assert json.loads((tmp_path / 'pending.json').read_text())['command'][0] == 'publish'


def test_export_rejects_asset_traversal(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'ROOT', tmp_path)
    source = tmp_path / 'docs/blog/post.html'
    source.parent.mkdir(parents=True)
    secret = tmp_path / 'secret.png'
    secret.write_text('private')
    article = runner.Article(source, tmp_path / 'work')
    with pytest.raises(RuntimeError, match='escapes'):
        article.feed('<article><img src="../../secret.png"></article>')


def test_verdict_gate_does_not_accept_unrun_inline_code(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'ROOT', tmp_path)
    folder = tmp_path / 'analysis_questions'
    folder.mkdir()
    notebook = folder / 'post.Rmd'
    notebook.write_text('## Verdict\nInsightful: `r "yes"`\n')
    with pytest.raises(RuntimeError, match='literal'):
        runner.check_verdict('post')
    notebook.write_text('## Verdict\nInsightful: yes\n')
    runner.check_verdict('post')


def test_actual_authenticated_sandbox_receives_only_selected_credential(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'STATE', tmp_path)
    runner.private_json(tmp_path / 'credentials.json', {
        'publication_url': runner.SPEC['publication_url'], 'session_token': 'fake-offline-cookie'})
    original = runner.execute

    def execute(argv, **kwargs):
        if argv[0] == 'bwrap':
            argv = list(argv)
            argv[argv.index('-c') + 1] = (
                'import json; from pathlib import Path; '
                'v=json.loads(Path("/run/credentials.json").read_text()); '
                'assert v["session_token"] == "fake-offline-cookie"; '
                'assert not Path("/home/karthik").exists(); '
                'print("credential-isolated")')
        return original(argv, **kwargs)
    monkeypatch.setattr(runner, 'execute', execute)
    result = runner.sandbox(tmp_path / 'post', ['doctor'], authenticated=True, capture=True)
    assert result.stdout.strip() == 'credential-isolated'


@pytest.mark.parametrize('name', ['connect.sid', 'substack.sid'])
def test_credentials_preserve_the_selected_cookie_name(tmp_path, monkeypatch, name):
    monkeypatch.setattr(runner, 'STATE', tmp_path)
    runner.private_json(tmp_path / 'credentials.json', {
        'publication_url': runner.SPEC['publication_url'], 'session_token': 'fake',
        'session_cookie_name': name})
    assert runner.credentials()['session_cookie_name'] == name


def test_credentials_reject_unrecognized_cookie_name(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'STATE', tmp_path)
    runner.private_json(tmp_path / 'credentials.json', {
        'publication_url': runner.SPEC['publication_url'], 'session_token': 'fake',
        'session_cookie_name': 'anything-else'})
    with pytest.raises(RuntimeError, match='cookie name'):
        runner.credentials()
