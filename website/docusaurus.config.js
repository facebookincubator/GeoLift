/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// With JSDoc @type annotations, IDEs can provide config autocompletion
/** @type {import('@docusaurus/types').DocusaurusConfig} */
(module.exports = {
  title: 'GeoLift',
  tagline: 'The Open Source solution from Meta Open Source to calculate Lift at a geo-level.',
  url: 'https://facebookincubator.github.io/',
  baseUrl: '/GeoLift/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'facebookincubator', // Usually your GitHub org/user name.
  projectName: 'GeoLift', // Usually your repo name.

  presets: [
    [
      '@docusaurus/preset-classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl: 'https://github.com/facebookincubator/GeoLift/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl:
            'https://github.com/facebookincubator/GeoLift/',
            blogSidebarTitle: 'All posts',
            blogSidebarCount: 'ALL',
            blogTitle: 'GeoLift Blog',
            blogDescription: 'A blog for all things GeoLift!',

        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'GeoLift',
        logo: {
          alt: 'GeoLift Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Documentation',
          },
          {
            type: 'doc',
            docId: 'GettingStarted/Walkthrough',
            position: 'left',
            label: 'Walkthroughs',
            },
          {
            to: 'Successes',
            position: 'left',
            label: 'Success Cases',
          },
          {
            type: 'doc',
            docId: 'About',
            position: 'left',
            label: 'About',
          },
          {to: 'blog', label: 'Blog', position: 'left'},
          // Please keep GitHub link to the right for consistency.
          {
            href: 'https://github.com/facebookincubator/GeoLift/',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Learn',
            items: [
              {
                label: 'Documentation',
                to: 'docs/intro',
              },
              {
                label: 'Blueprint Course',
                href: 'https://www.facebookblueprint.com/student/path/253063-geolift-marketing-measurement?sid=cd9a46bc-9685-4af4-8ac3-4482e858fba9&sid_i=5',
              },
              {
                label: 'Success Cases',
                to: 'Successes',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Facebook',
                href: 'https://www.facebook.com/groups/fbgeolift',
              },
              {
                label: 'YouTube',
                href: 'https://www.youtube.com/channel/UCzkuhQFBCHz9TxzUXlNUINg',
              }
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Blog',
                to: 'blog',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/facebookincubator/GeoLift/',
              },
            ],
          },
          {
            title: 'Legal',
            // Please do not remove the privacy and terms, it's a legal requirement.
            items: [
              {
                label: 'Privacy',
                href: 'https://opensource.facebook.com/legal/privacy/',
              },
              {
                label: 'Terms',
                href: 'https://opensource.facebook.com/legal/terms/',
              },
              {
                label: 'Data Policy',
                href: 'https://opensource.facebook.com/legal/data-policy/',
              },
              {
                label: 'Cookie Policy',
                href: 'https://opensource.facebook.com/legal/cookie-policy/',
              },
            ],
          },
        ],
        logo: {
          alt: 'Facebook Open Source Logo',
          src: 'img/oss_logo.png',
          href: 'https://opensource.facebook.com',
        },
        // Please do not remove the credits, help to publicize Docusaurus :)
        copyright: `Copyright © ${new Date().getFullYear()} Facebook, Inc. Built with Docusaurus.`,
      },
    }),
});
