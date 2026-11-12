import { readFileSync } from 'node:fs';
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightLinksValidator from 'starlight-links-validator';

// Permanent links printed by the CLI (see src/Hledger/Flow/DocHelpers.hs).
const goLinks = JSON.parse(readFileSync(new URL('./go-links.json', import.meta.url), 'utf8'));
const redirects = Object.fromEntries(
  Object.entries(goLinks).map(([slug, target]) => [`/go/${slug}`, target]),
);

export default defineConfig({
  site: 'https://hledger-flow.github.io',
  trailingSlash: 'always',
  compressHTML: false,
  redirects,
  integrations: [
    starlight({
      title: 'hledger-flow',
      description:
        'Organise bank statements by owner, bank, account and year, and rebuild your hledger journals from them whenever your rules change.',
      social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/apauley/hledger-flow' }],
      editLink: { baseUrl: 'https://github.com/apauley/hledger-flow/edit/master/site/' },
      customCss: ['@fontsource-variable/recursive/mono.css', './src/styles/tokens.css', './src/styles/docs.css'],
      expressiveCode: { shiki: { langAlias: { hledger: 'txt', rules: 'txt' } } },
      plugins: [starlightLinksValidator({ errorOnLocalLinks: true })],
      sidebar: [
        { label: 'Quick start', link: '/quickstart/' },
        {
          label: 'Tutorial',
          items: ['tutorial', 'tutorial/part-1', 'tutorial/part-2', 'tutorial/part-3', 'tutorial/part-4', 'tutorial/part-5'],
        },
        {
          label: 'Reference',
          items: [
            'reference/status',
            'reference/input-files',
            'reference/rules-files',
            'reference/preprocess',
            'reference/construct',
            'reference/windows-hooks',
            'reference/manual-journals',
            'reference/balances',
            'reference/price-files',
            'reference/directives',
            'reference/release-notices',
          ],
        },
        { label: 'Guides', items: ['guides/transfers', 'guides/ledger'] },
      ],
    }),
  ],
});
