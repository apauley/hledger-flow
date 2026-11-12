// Fails when a permanent /go/ link printed by the CLI no longer lands on an
// existing page and section of the built site.
import { existsSync, readFileSync } from 'node:fs';

const links = JSON.parse(readFileSync(new URL('../go-links.json', import.meta.url), 'utf8'));
const broken = Object.entries(links).flatMap(([slug, target]) => {
  const [path, anchor] = target.split('#');
  const file = new URL(`../dist${path}index.html`, import.meta.url);
  if (!path.startsWith('/') || !path.endsWith('/') || !existsSync(file)) return [`/go/${slug}/ -> ${target}: no such page`];
  if (anchor && !readFileSync(file, 'utf8').includes(`id="${anchor}"`)) return [`/go/${slug}/ -> ${target}: no #${anchor} on that page`];
  return [];
});
if (broken.length) {
  console.error(`Broken /go/ links. Fix the targets in site/go-links.json:\n${broken.join('\n')}`);
  process.exit(1);
}
console.log(`All ${Object.keys(links).length} /go/ links resolve.`);
