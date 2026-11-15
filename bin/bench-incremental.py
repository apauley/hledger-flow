#!/usr/bin/env python3
"""Benchmark plain and rerun-scripts imports on reproducible, rules-only ledgers.

Run with Python 3, a built hledger-flow, a supported native hledger and a new
results directory. Pass --rounds N or --shapes archive original one many to
select measurements. samples.csv contains CLI wall times in milliseconds.
"""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import statistics
import subprocess
import sys
import time


GENERATOR = Path(__file__).resolve().with_name('generate-workload.py')
SHAPES = {'archive': (3, 1, 31), 'original': (4, 0, 31),
          'one': (1, 0, 1000), 'many': (100, 0, 10)}


def seed(root, shape):
    if shape == 'original':
        # Exact archive CSVs, filenames and nested rules from the layout experiment.
        bank = root / 'import/owner/bank'
        for account in range(4):
            for year, count in [(2024, 24), (2025, 6), (2026, 1)]:
                inbox = bank / f'account{account}/1-in/{year}'
                inbox.mkdir(parents=True)
                rows = ''.join(f'{year}-01-{d:02d},Item {d},-5\n' for d in range(1, 11))
                for i in range(count):
                    (inbox / f'statement{i:02}.csv').write_bytes(rows.encode())
        (bank.parent.parent / 'bank.rules').write_bytes(
            b'fields date, description, amount\naccount1 assets:bank:checking\naccount2 expenses:bank-fees\n')
        override = bank / 'account0'
        (override / 'bank-account0.rules').write_bytes(b'include ../../../bank.rules\ninclude rules/group.rules\n')
        (override / 'rules').mkdir()
        (override / 'rules/group.rules').write_bytes(b'include merchant.rules\n')
        (override / 'rules/merchant.rules').write_bytes(b'if Item 1\n  account2 expenses:old\n')
    else:
        plain, advanced, statements = SHAPES[shape]
        subprocess.run([sys.executable, GENERATOR, 'mixed', root, '--plain', str(plain),
                        '--advanced', str(advanced), '--statements', str(statements),
                        '--transactions', '10', '--seed', '42'], check=True)
        if shape == 'archive':
            for inbox in sorted(root.glob('import/*/*/*/1-in')):
                for year, numbers in [(2025, range(25, 31)), (2026, [31])]:
                    target = inbox / str(year)
                    target.mkdir()
                    for number in numbers:
                        statement = inbox / '2024' / f'statement-{number:04}.csv'
                        statement.rename(target / statement.name)


def addition(root, shape):
    inbox = sorted(root.glob('import/*/*/*/1-in'))[0]
    latest = sorted(inbox.iterdir())[-1]
    rows = (b'2026-02-01,New fee,-17\n' if shape == 'original' else
            b'date,reference,description,amount,category\n2026-02-01,extra,New fee,-17,unknown\n')
    (latest / 'added.csv').write_bytes(rows)


def journals(root):
    return {p.relative_to(root): p.read_bytes() for p in root.rglob('*.journal')}


def records(root):
    cache = root / '.hledger-flow/cache/import'
    result = {}
    for p in cache.rglob('*'):
        if p.is_file():
            stat = p.stat()
            result[p] = stat.st_ino, stat.st_mtime_ns
    return result


def invoke(command, root, env, incremental, newest, selected, regenerated):
    args = command + (['--incremental', 'rerun-scripts'] if incremental else []) + (['--start-year', '2026'] if newest else [])
    start = time.perf_counter_ns()
    run = subprocess.run(args, cwd=root, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=900)
    elapsed = (time.perf_counter_ns() - start) / 1e6
    log = run.stdout.decode('utf-8', 'replace')
    summary = re.search(r'Imported (\d+)(?: of (\d+))? statements? for ', log)
    if (run.returncode or 'Incremental reuse unavailable' in log or not summary or
            (int(summary[1]), int(summary[2] or summary[1])) != (regenerated, selected)):
        raise RuntimeError(f'Expected {regenerated}/{selected} with status 0: {args}\n{log[-4000:]}')
    return elapsed


def benchmark(shape, args, cmd, env, writer, stream):
    root = args.out / shape / 'ledger'
    baseline = root.parent / 'baseline'
    seed(root, shape)
    plain, advanced, statements = SHAPES[shape]
    total = (plain + advanced) * statements
    invoke(cmd, root, env, False, False, total, total)
    expected = {'unchanged': journals(root)}
    invoke(cmd, root, env, True, False, total, total)
    if journals(root) != expected['unchanged'] or len(records(root)) != total:
        raise RuntimeError(f'{shape}: initial cache or journals differ')
    shutil.copytree(root, baseline)
    addition(root, shape)
    invoke(cmd, root, env, False, False, total + 1, total + 1)
    expected['added'] = journals(root)
    if expected['added'] == expected['unchanged']:
        raise RuntimeError(f'{shape}: added statement did not change journals')
    samples = {}
    for round_ in range(1, args.rounds + 1):
        for newest in ([False, True] if shape in ('archive', 'original') else [False]):
            for case in ('first', 'unchanged', 'added'):
                for incremental in ([False, True] if round_ % 2 else [True, False]):
                    shutil.rmtree(root)
                    shutil.copytree(baseline, root)
                    if case == 'first':
                        shutil.rmtree(root / '.hledger-flow')
                    if case == 'added':
                        addition(root, shape)
                    before = records(root)
                    selected = (4 if newest else total) + (case == 'added')
                    regenerated = selected if not incremental or case == 'first' else int(case == 'added')
                    ms = invoke(cmd, root, env, incremental, newest, selected, regenerated)
                    after = records(root)
                    writes = sum(before.get(p) != stamp for p, stamp in after.items())
                    if writes != (regenerated if incremental else 0) or before.keys() - after.keys():
                        raise RuntimeError(f'{shape}/{case}: unexpected cache changes ({writes} replacements)')
                    if journals(root) != expected['added' if case == 'added' else 'unchanged']:
                        raise RuntimeError(f'{shape}/{case}: journal bytes differ from full import')
                    key = (shape, '2026' if newest else 'all', case, 'rerun-scripts' if incremental else 'full')
                    samples.setdefault(key, []).append(ms)
                    writer.writerow([*key, round_, selected, regenerated, writes, f'{ms:.3f}'])
                    stream.flush()
        print(f'{shape}: verified round {round_}', flush=True)
    for key, times in samples.items():
        print(*key, f'{statistics.median(times):.1f} ms', flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('flow', type=Path)
    parser.add_argument('hledger', type=Path)
    parser.add_argument('out', type=Path)
    parser.add_argument('--rounds', type=int, default=3)
    parser.add_argument('--shapes', nargs='+', choices=SHAPES, default=list(SHAPES))
    args = parser.parse_args()
    if args.rounds < 1:
        parser.error('--rounds must be positive')
    args.out = args.out.resolve()
    args.out.mkdir(parents=True, exist_ok=False)
    conf = args.out / 'empty.conf'
    conf.touch()
    flow, engine = args.flow.resolve(strict=True), args.hledger.resolve(strict=True)
    cmd = [flow, '-H', engine, '--hledger-conf', conf, 'import']
    env = dict(os.environ, HLEDGER_FLOW_NO_UPDATE_CHECK='1', XDG_CONFIG_HOME=str(args.out / 'config'), TZ='UTC')
    for key in ('GHCRTS', 'LEDGER_FILE', 'HLEDGER_CONF'):
        env.pop(key, None)
    meta = {'platform': platform.platform(), 'logical_cpus': os.cpu_count(), 'rounds': args.rounds,
            'hledger': subprocess.check_output([engine, '--version'], text=True).strip(),
            'hledger_bytes': engine.stat().st_size}
    for name, binary in [('flow', flow), ('hledger', engine)]:
        meta[name + '_sha256'] = hashlib.sha256(binary.read_bytes()).hexdigest()
    (args.out / 'metadata.json').write_text(json.dumps(meta, indent=2) + '\n', encoding='utf-8')
    with (args.out / 'samples.csv').open('w', newline='', encoding='utf-8') as stream:
        writer = csv.writer(stream)
        writer.writerow(['shape', 'scope', 'case', 'mode', 'round', 'selected', 'converted', 'records_replaced', 'wall_ms'])
        for shape in args.shapes:
            benchmark(shape, args, cmd, env, writer, stream)


if __name__ == '__main__':
    main()
