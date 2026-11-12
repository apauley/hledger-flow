// Pages whose source lives elsewhere in the repository, because they also
// ship outside the site. QUICKSTART.md is packed into every release archive.
// The generated copies are git-ignored: edit the source, not the copy.
import { readFileSync, writeFileSync } from 'node:fs';

const pages = [
  {
    from: '../QUICKSTART.md',
    to: 'src/content/docs/quickstart.md',
    title: 'Quick start',
    description: 'Install hledger-flow and hledger, then import your first statement.',
  },
];

for (const page of pages) {
  const source = readFileSync(new URL(`../${page.from}`, import.meta.url), 'utf8');
  const body = source
    .replace(/^# .*\n/, '')
    .replaceAll('](https://hledger-flow.github.io/', '](/');
  const editUrl = `https://github.com/apauley/hledger-flow/edit/master/${page.from.replace('../', '')}`;
  const frontmatter = `---\ntitle: ${page.title}\ndescription: ${page.description}\neditUrl: ${editUrl}\n---\n`;
  writeFileSync(new URL(`../${page.to}`, import.meta.url), frontmatter + body);
}
